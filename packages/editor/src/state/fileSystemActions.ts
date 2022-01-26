import {
  constructLayout,
  FilePointer,
  ICachedWorkspacePointer,
  IExamples,
  IFileSystemState,
  IWorkspace,
  IWorkspacePointer,
  IWorkspaceState,
  ProgramFile,
  SavedFile,
  WorkspaceFile,
} from "../types/FileSystem";
import { toast } from "react-toastify";
import { Model } from "flexlayout-react";
import { FileDispatcher } from "./fileReducer";
import { compileDomain } from "@penrose/core";

async function fetchRegistry(): Promise<any> {
  const res = await fetch(
    "https://raw.githubusercontent.com/penrose/penrose/main/examples/registry.json"
  );
  if (res.ok) {
    return await res.json();
  }
  return null;
}

export async function fetchExamples(): Promise<IExamples | null> {
  const registry = await fetchRegistry();
  if (registry) {
    const examples: IExamples = {
      substances: {},
      styles: {},
      domains: {},
      trios: {},
    };
    Object.entries(registry.domains).forEach(([id, domain]: [string, any]) => {
      const fileName = `${id}.dsl`;
      examples.domains[fileName] = {
        type: "domain",
        name: domain.name,
        id: fileName,
        location: {
          type: "example",
          path: registry.root + domain.URI,
        },
      };
    });
    Object.entries(registry.styles).forEach(([id, style]: [string, any]) => {
      const fileName = `${id}.sty`;
      examples.styles[fileName] = {
        type: "style",
        name: style.name,
        domain: style.domain,
        id: fileName,
        location: {
          type: "example",
          path: registry.root + style.URI,
        },
      };
    });
    Object.entries(registry.substances).forEach(
      ([id, substance]: [string, any]) => {
        const fileName = `${id}.sub`;
        examples.substances[fileName] = {
          type: "substance",
          name: substance.name,
          domain: substance.domain,
          id: fileName,
          location: {
            type: "example",
            path: registry.root + substance.URI,
          },
        };
      }
    );
    examples.trios = registry.trios.map(
      (trio: any, idx: number): ICachedWorkspacePointer => {
        const substance = `${trio.substance}.sub`;
        const style = `${trio.style}.sty`;
        const domain = `${trio.domain}.dsl`;
        return {
          type: "cached_workspace",
          files: {
            [substance]: examples.substances[substance],
            [style]: examples.styles[style],
            [domain]: examples.domains[domain],
          },
          name: `${registry.substances[trio.substance].name} - ${
            registry.styles[trio.style].name
          }`,
          id: `${trio.substance},${trio.style},${trio.domain}`,
          location: {
            type: "example",
            path: idx.toString(),
          },
        };
      }
    );
    return examples;
  } else {
    toast.error("Failed to fetch examples");
  }
  return null;
}

/**
 * For pointers and their loaded contents from memory
 * @param pointer
 * @param contents
 * @returns The typesafe parsed in-memory version of the pointer
 */
function processPointerContents(
  pointer: FilePointer,
  contents: string
): SavedFile {
  switch (pointer.type) {
    case "substance":
    case "style":
    case "domain":
      return { contents, id: pointer.id, type: "program_file" };
    case "diagram_state":
      return {
        type: "state_file",
        contents: JSON.parse(contents),
        id: pointer.id,
      };
    case "workspace":
    case "cached_workspace":
      const parsedWorkspace = JSON.parse(contents);
      const loadedLayout = {
        ...parsedWorkspace,
        layout: Model.fromJson(parsedWorkspace.layout),
      };
      return {
        type: "workspace_file",
        contents: loadedLayout,
        id: pointer.id,
      };
  }
}

function buildExampleWorkspace(
  pointer: ICachedWorkspacePointer
): SavedFile | null {
  const sub = Object.values(pointer.files).find((f) => f.type === "substance");
  const sty = Object.values(pointer.files).find((f) => f.type === "style");
  const dsl = Object.values(pointer.files).find((f) => f.type === "domain");
  if (!sub || !sty || !dsl) {
    toast.error("Could not retrieve sub or sty or dsl for trio");
    return null;
  }
  const workspace: WorkspaceFile = {
    type: "workspace_file",
    id: pointer.id,
    contents: {
      openFiles: {
        [sub.id]: sub,
        [sty.id]: sty,
        [dsl.id]: dsl,
      },
      compileTrioSetting: {
        substance: sub.id,
        style: sty.id,
        domain: dsl.id,
      },
      creator: "penrose",
      forkedFrom: null,
      name: pointer.name,
      id: pointer.id,
      domainCache: null,
      layout: constructLayout([
        {
          type: "tabset",
          weight: 100,
          children: [
            {
              type: "tab",
              name: sub.name,
              component: "file",
              id: sub.id,
            },
            {
              type: "tab",
              name: sty.name,
              component: "file",
              id: sty.id,
            },
            {
              type: "tab",
              name: dsl.name,
              component: "file",
              id: dsl.id,
            },
          ],
        },
      ]),
    },
  };
  return workspace;
}

async function retrieveFileFromPointer(
  pointer: FilePointer
): Promise<SavedFile | null> {
  switch (pointer.location.type) {
    case "local":
      const stored = localStorage.getItem(pointer.location.localStorageKey);
      if (stored) {
        return processPointerContents(pointer, stored);
      }
      break;
    case "example":
      if (pointer.type === "cached_workspace") {
        return buildExampleWorkspace(pointer);
      } else {
        const res = await fetch(pointer.location.path);
        if (res.ok) {
          const text = await res.text();
          return processPointerContents(pointer, text);
        }
      }
      break;
    case "gist":
      /**
       * pull different files from gist - use gist api
       */
      break;
    default:
      break;
  }
  return null;
}

export async function loadWorkspace(
  dispatch: FileDispatcher,
  workspace: IWorkspacePointer
): Promise<void> {
  const loadedWorkspace = await retrieveFileFromPointer(workspace);
  if (loadedWorkspace !== null && loadedWorkspace.type === "workspace_file") {
    const workspace: IWorkspaceState = {
      fileContents: {},
      openWorkspace: loadedWorkspace.contents,
    };
    for (let [id, ptr] of Object.entries(loadedWorkspace.contents.openFiles)) {
      const retrieved = await retrieveFileFromPointer(ptr);
      if (retrieved !== null) {
        workspace.fileContents[id] = retrieved;
      } else {
        toast.error(`Failed to load file ${id}`);
      }
    }
    if (
      loadedWorkspace.contents.domainCache === null &&
      loadedWorkspace.contents.compileTrioSetting.domain !== null
    ) {
      const res = compileDomain(
        workspace.fileContents[
          loadedWorkspace.contents.compileTrioSetting.domain
        ].contents as string
      );
      if (res.isOk()) {
        workspace.openWorkspace.domainCache = res.value || null;
      }
    }
    dispatch({ type: "SET_WORKSPACE", workspaceState: workspace });
  } else {
    toast.error(`Failed to load workspace ${workspace.id}`);
  }
}

/*
function writeFileToDisk(
  pointer: FilePointer,
  contents: string,
  workspaceId: string
): IWorkspaceState {
  if (pointer.location.type === "local") {
  } else {
    //  generate new local pointer with new Id
    //   change return type
    // override workspace author, if there is one
  }
  return { fileContents: {}, workspace: "" };
}
*/
