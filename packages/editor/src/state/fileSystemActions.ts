import {
  FilePointer,
  IExamples,
  IExampleTrio,
  IFileSystem,
  IWorkspaceState,
  ProgramFile,
} from "../types/FileSystem";
import { FileDispatcher } from "./fileReducer";
import { toast } from "react-toastify";

export async function fetchExamples(dispatch: FileDispatcher): Promise<void> {
  const res = await fetch(
    "https://raw.githubusercontent.com/penrose/penrose/main/examples/registry.json"
  );
  if (res.ok) {
    const registry = await res.json();
    const examples: IExamples = {
      substances: {},
      styles: {},
      domains: {},
      trios: [],
    };
    Object.entries(registry.domains).forEach(([id, domain]: [string, any]) => {
      examples.domains[id] = {
        type: "domain",
        name: domain.name,
        id,
        location: {
          type: "example",
          path: registry.root + domain.URI,
        },
      };
    });
    Object.entries(registry.styles).forEach(([id, style]: [string, any]) => {
      examples.styles[id] = {
        type: "style",
        name: style.name,
        domain: style.domain,
        id,
        location: {
          type: "example",
          path: registry.root + style.URI,
        },
      };
    });
    Object.entries(registry.substances).forEach(
      ([id, substance]: [string, any]) => {
        examples.substances[id] = {
          type: "substance",
          name: substance.name,
          domain: substance.domain,
          id,
          location: {
            type: "example",
            path: registry.root + substance.URI,
          },
        };
      }
    );
    examples.trios = registry.trios.map(
      (trio: any): IExampleTrio => ({
        substance: registry.substances[trio.substance],
        style: registry.styles[trio.style],
        domain: registry.domains[trio.domain],
      })
    );
    dispatch({ type: "SET_EXAMPLES", examples });
  } else {
    toast.error("Failed to fetch examples");
  }
}

async function hydrateFileSystem(): Promise<IFileSystem> {
  //
  return {
    local: {
      workspaces: {},
      substances: {},
      styles: {},
      domains: {},
      diagrams: {},
    },
    examples: {
      substances: {},
      styles: {},
      domains: {},
      trios: [],
    },
  };
}

async function retrieveFileFromPointer(
  pointer: FilePointer
): Promise<ProgramFile | null> {
  switch (pointer.location.type) {
    case "local":
      break;
    case "example":
      break;
    case "gist":
      break;
    default:
      break;
  }
  return null;
}

function writeFile(
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
