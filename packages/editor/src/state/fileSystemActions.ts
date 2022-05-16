import { compileDomain } from "@penrose/core";
import { Model } from "flexlayout-react";
import localforage from "localforage";
import { toast } from "react-toastify";
import { v4 } from "uuid";
import {
  constructLayout,
  DiagramFile,
  DiagramFilePointer,
  DomainFilePointer,
  FilePointer,
  ICachedWorkspacePointer,
  IExamples,
  SavedFile,
  StyleFilePointer,
  SubstanceFilePointer,
  WorkspaceFile,
} from "../types/FileSystem";

async function fetchRegistry(): Promise<any> {
  const res = await fetch(
    "https://raw.githubusercontent.com/penrose/penrose/main/packages/examples/src/registry.json"
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
    const idMap: any = {
      domains: {},
      substances: {},
      styles: {},
    };
    Object.entries(registry.domains).forEach(
      ([name, domain]: [string, any]) => {
        const id = v4();
        idMap.domains[name] = id;
        examples.domains[id] = {
          type: "domain",
          name: domain.name,
          id,
          location: {
            type: "example",
            path: registry.root + domain.URI,
          },
        };
      }
    );
    Object.entries(registry.styles).forEach(([name, style]: [string, any]) => {
      const id = v4();
      idMap.styles[name] = id;
      examples.styles[id] = {
        type: "style",
        name: style.name,
        domain: examples.domains[idMap.domains[style.domain]],
        id,
        location: {
          type: "example",
          path: registry.root + style.URI,
        },
      };
    });
    Object.entries(registry.substances).forEach(
      ([name, substance]: [string, any]) => {
        const id = v4();
        idMap.substances[name] = id;
        examples.substances[id] = {
          type: "substance",
          name: substance.name,
          domain: examples.domains[idMap.domains[substance.domain]],
          id,
          location: {
            type: "example",
            path: registry.root + substance.URI,
          },
        };
      }
    );
    examples.trios = registry.trios.map(
      (trio: any, idx: number): ICachedWorkspacePointer => {
        const substance = idMap.substances[trio.substance];
        const style = idMap.styles[trio.style];
        const domain = idMap.domains[trio.domain];
        return {
          type: "workspace",
          files: {
            [substance]: examples.substances[substance],
            [style]: examples.styles[style],
            [domain]: examples.domains[domain],
          },
          name: `${registry.substances[trio.substance].name} - ${
            registry.styles[trio.style].name
          }`,
          id: v4(),
          location: {
            type: "example",
            // dummy value
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
  value: any
): SavedFile {
  switch (pointer.type) {
    case "substance":
    case "style":
    case "diagram_state":
      return value;
    case "domain":
      const compiledDomain = compileDomain(value.contents);
      const cache = compiledDomain.isOk() ? compiledDomain.value : null;
      return { ...value, cache };
    case "workspace":
      return {
        ...value,
        contents: {
          ...value.contents,
          layout: Model.fromJson(value.contents.layout),
        },
      };
  }
}

function buildExampleWorkspace(
  pointer: ICachedWorkspacePointer
): SavedFile | null {
  const sub = Object.values(pointer.files).find(
    (f) => f.type === "substance"
  ) as SubstanceFilePointer;
  const sty = Object.values(pointer.files).find(
    (f) => f.type === "style"
  ) as StyleFilePointer;
  const dsl = Object.values(pointer.files).find(
    (f) => f.type === "domain"
  ) as DomainFilePointer;
  if (!sub || !sty || !dsl) {
    toast.error("Could not retrieve sub or sty or dsl for trio");
    return null;
  }
  const diagramId = v4();

  const diagramPointer: DiagramFilePointer = {
    name: "Diagram",
    type: "diagram_state",
    substance: sub,
    style: sty,
    domain: dsl,
    id: diagramId,
    location: {
      type: "example",
      path: "",
    },
  };
  const workspace: WorkspaceFile = {
    type: "workspace_file",
    id: pointer.id,
    contents: {
      openFiles: {
        [sub.id]: sub,
        [sty.id]: sty,
        [dsl.id]: dsl,
        [diagramPointer.id]: diagramPointer,
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
              name: "Diagram",
              component: "file",
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

// TODO: separate the workspace example retrieval, it's jank to put it here. Use processExamplePointerContents
export async function retrieveFileFromPointer(
  pointer: FilePointer
): Promise<SavedFile | null> {
  switch (pointer.location.type) {
    case "local":
      const stored = await localforage.getItem(
        pointer.location.localStorageKey
      );
      console.log(stored);
      if (stored !== null) {
        console.log(stored);
        // return processLocalPointerContents(pointer, stored);
      }
      break;
    case "example":
      if (pointer.type === "workspace" && "files" in pointer) {
        return buildExampleWorkspace(pointer);
      } else if (pointer.type === "diagram_state") {
        // Make dummy diagram
        const diagram: DiagramFile = {
          id: pointer.id,
          type: "diagram_file",
          contents: null,
          metadata: {
            error: null,
            autostep: true,
            variation: "",
          },
        };
        return diagram;
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
  console.error(`Retrieval error for`, pointer);
  return null;
}
