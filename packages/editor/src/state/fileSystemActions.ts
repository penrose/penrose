import {
  constructLayout,
  DiagramFilePointer,
  DomainFilePointer,
  FilePointer,
  ICachedWorkspacePointer,
  IExamples,
  ILocalFileSystem,
  ILocalLocation,
  IWorkspace,
  IWorkspacePointer,
  SavedFile,
  DiagramFile,
  StyleFilePointer,
  SubstanceFilePointer,
  WorkspaceFile,
} from "../types/FileSystem";
import { toast } from "react-toastify";
import { Model } from "flexlayout-react";
import { compileDomain } from "@penrose/core";
import { v4 } from "uuid";

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
        domain: examples.domains[`${style.domain}.dsl`],
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
          domain: examples.domains[`${substance.domain}.dsl`],
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
 * Turns fetched file contents into valid SavedFile
 * @param pointer
 * @param value
 */
function processExamplePointerContents(
  pointer: FilePointer,
  value: string
): SavedFile {
  switch (pointer.type) {
    case "substance":
    case "style":
      return { id: pointer.id, type: `${pointer.type}_file`, contents: value };
    case "domain":
      const compiledDomain = compileDomain(value);
      const cache = compiledDomain.isOk() ? compiledDomain.value : null;
      return { id: pointer.id, type: "domain_file", contents: value, cache };
    case "workspace":
    case "cached_workspace":
      const parsed = JSON.parse(value);
      return {
        ...parsed,
        contents: {
          ...parsed.contents,
          layout: Model.fromJson(parsed.contents.layout),
        },
      };
    default:
      console.error("Can't handle example type", pointer.type);
      return { id: pointer.id, contents: "", type: "substance_file" };
  }
}

/**
 * For pointers and their loaded contents from memory
 * @param pointer
 * @param contents
 * @returns The typesafe parsed in-memory version of the pointer
 */
function processLocalPointerContents(
  pointer: FilePointer,
  value: string
): SavedFile {
  const parsed = JSON.parse(value);
  switch (pointer.type) {
    case "substance":
    case "style":
    case "domain":
    case "diagram_state":
      return parsed;
    case "workspace":
    case "cached_workspace":
      return {
        ...parsed,
        contents: {
          ...parsed.contents,
          layout: Model.fromJson(parsed.contents.layout),
        },
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
  const diagramId = v4();
  const workspace: WorkspaceFile = {
    type: "workspace_file",
    id: pointer.id,
    contents: {
      openFiles: {
        [sub.id]: sub,
        [sty.id]: sty,
        [dsl.id]: dsl,
      },
      location: pointer.location,
      creator: "penrose",
      forkedFrom: null,
      name: pointer.name,
      id: pointer.id,
      layout: constructLayout([
        {
          type: "tabset",
          weight: 50,
          id: "main",
          children: [
            {
              type: "tab",
              name: sub.name,
              component: "file",
              id: v4(),
              config: {
                id: sub.id,
              },
            },
            {
              type: "tab",
              name: sty.name,
              component: "file",
              id: v4(),
              config: {
                id: sty.id,
              },
            },
            {
              type: "tab",
              name: dsl.name,
              component: "file",
              id: v4(),
              config: {
                id: dsl.id,
              },
            },
          ],
        },
        {
          type: "tabset",
          weight: 50,
          id: "preview",
          children: [
            {
              type: "tab",
              name: "New Diagram",
              component: "diagram_initializer",
              id: diagramId,
              config: {
                id: diagramId,
              },
            },
          ],
        },
      ]).toJson(),
    },
  };
  return workspace;
}

export async function retrieveFileFromPointer(
  pointer: FilePointer
): Promise<SavedFile | null> {
  switch (pointer.location.type) {
    case "local":
      const stored = localStorage.getItem(pointer.location.localStorageKey);
      if (stored) {
        return processLocalPointerContents(pointer, stored);
      }
      break;
    case "example":
      if (pointer.type === "cached_workspace") {
        return buildExampleWorkspace(pointer);
      } else {
        const res = await fetch(pointer.location.path);
        if (res.ok) {
          const text = await res.text();
          return processExamplePointerContents(pointer, text);
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
