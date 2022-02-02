import {
  constructLayout,
  DiagramFilePointer,
  DomainFilePointer,
  FilePointer,
  ICachedWorkspacePointer,
  IExamples,
  IFileSystemState,
  IWorkspace,
  IWorkspacePointer,
  IWorkspaceState,
  ProgramFile,
  SavedFile,
  StateFile,
  StyleFilePointer,
  SubstanceFilePointer,
  WorkspaceFile,
} from "../types/FileSystem";
import { toast } from "react-toastify";
import {
  Actions,
  DockLocation,
  Model,
  TabNode,
  TabSetNode,
} from "flexlayout-react";
import { FileDispatcher } from "./fileReducer";
import {
  compileDomain,
  compileTrio,
  prepareState,
  stepUntilConvergence,
} from "@penrose/core";
import { v4 } from "uuid";
import { useCallback } from "react";
import { initial } from "lodash";
import { TrioSelection } from "../components/DiagramInitializer";

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
    case "domain":
      return { id: pointer.id, type: "program_file", contents: value };
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
      return { id: pointer.id, contents: "", type: "program_file" };
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
  const workspace: WorkspaceFile = {
    type: "workspace_file",
    id: pointer.id,
    contents: {
      openFiles: {
        [sub.id]: sub,
        [sty.id]: sty,
        [dsl.id]: dsl,
      },
      creator: "penrose",
      forkedFrom: null,
      name: pointer.name,
      id: pointer.id,
      domainCache: null,
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
        {
          type: "tabset",
          weight: 50,
          id: "preview",
          children: [
            {
              type: "tab",
              name: "New Diagram",
              component: "diagram_initializer",
              id: v4(),
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

export const useLoadWorkspace = (dispatch: FileDispatcher) =>
  useCallback(
    (workspacePointer: IWorkspacePointer) => {
      (async () => {
        const loadedWorkspace = await retrieveFileFromPointer(workspacePointer);
        if (
          loadedWorkspace !== null &&
          loadedWorkspace.type === "workspace_file"
        ) {
          const workspace: IWorkspaceState = {
            fileContents: {},
            openWorkspace: loadedWorkspace.contents,
          };
          for (let [id, ptr] of Object.entries(
            loadedWorkspace.contents.openFiles
          )) {
            const retrieved = await retrieveFileFromPointer(ptr);
            if (retrieved !== null) {
              workspace.fileContents[id] = retrieved;
            } else {
              toast.error(`Failed to load file ${id}`);
            }
          }
          const openDomains = Object.values(
            loadedWorkspace.contents.openFiles
          ).filter(({ type }) => type === "domain");
          if (
            loadedWorkspace.contents.domainCache === null &&
            openDomains.length > 0
          ) {
            const res = compileDomain(
              workspace.fileContents[openDomains[0].id].contents as string
            );
            if (res.isOk()) {
              workspace.openWorkspace.domainCache = res.value || null;
            } else {
              toast.warn("Couldn't compile domain for autocompletion");
            }
          }
          dispatch({ type: "SET_WORKSPACE", workspaceState: workspace });
        } else {
          toast.error(`Failed to load workspace ${workspacePointer.id}`);
        }
      })();
    },
    [dispatch]
  );

export const useOpenFileInWorkspace = (
  dispatch: FileDispatcher,
  workspace: IWorkspace
) =>
  useCallback(
    (pointer: FilePointer) => {
      (async () => {
        if (pointer.id in workspace.openFiles) {
          // If already open, jump there
          workspace.layout.doAction(Actions.selectTab(pointer.id));
          return;
        }
        const loadedFile = await retrieveFileFromPointer(pointer);
        if (loadedFile !== null) {
          dispatch({ type: "OPEN_FILE", file: loadedFile, pointer });
          if (!workspace.layout.getActiveTabset()) {
            workspace.layout.doAction(
              Actions.setActiveTabset(
                workspace.layout.getMaximizedTabset()?.getId() ?? "main"
              )
            );
          }
          workspace.layout.doAction(
            Actions.addNode(
              {
                type: "tab",
                component: "file",
                name: pointer.name,
                id: pointer.id,
              },
              // HACK: the fallback is fallible
              workspace.layout.getActiveTabset()?.getId() || "main",
              DockLocation.CENTER,
              -1,
              true
            )
          );
        } else {
          toast.error(`Failed to load file ${pointer.name}`);
        }
      })();
    },
    [dispatch, workspace]
  );

export const useUpdateNodeToDiagramCreator = (workspace: IWorkspace) =>
  useCallback(
    (node: TabNode) => {
      workspace.layout.doAction(
        Actions.updateNodeAttributes(node.getId(), {
          component: "diagram_initializer",
          name: "New Diagram",
        })
      );
    },
    [workspace]
  );

export function newFileCreatorTab(workspace: IWorkspace, node: TabSetNode) {
  workspace.layout.doAction(
    Actions.addNode(
      {
        type: "tab",
        component: "new_tab",
        name: "New Tab",
        id: v4(),
      },
      node.getId(),
      DockLocation.CENTER,
      -1,
      true
    )
  );
}

async function _compileDiagram(
  dispatch: FileDispatcher,
  workspaceState: IWorkspaceState,
  diagramPointer: DiagramFilePointer,
  autostep: boolean
) {
  const diagramFile: StateFile = {
    type: "state_file",
    id: diagramPointer.id,
    contents: null,
    metadata: {
      error: null,
      autostep,
    },
  };
  const substance = (
    workspaceState.fileContents[diagramPointer.substance.id] as ProgramFile
  ).contents;
  const style = (
    workspaceState.fileContents[diagramPointer.style.id] as ProgramFile
  ).contents;
  const domain = (
    workspaceState.fileContents[diagramPointer.domain.id] as ProgramFile
  ).contents;
  const compiledDomain = compileDomain(domain);
  if (compiledDomain.isOk()) {
    dispatch({
      type: "SET_DOMAIN_CACHE",
      domainCache: compiledDomain.value,
    });
  } else {
    toast.error("Couldn't compile domain");
  }
  const compileResult = compileTrio(domain, substance, style);
  if (compileResult.isOk()) {
    const initialState = await prepareState(compileResult.value);
    diagramFile.contents = initialState;
    dispatch({ type: "UPDATE_OPEN_FILE", file: diagramFile });
    if (autostep) {
      const stepResult = stepUntilConvergence(initialState);
      if (stepResult.isOk()) {
        const convergedState = stepResult.value;
        dispatch({
          type: "UPDATE_OPEN_FILE",
          file: { ...diagramFile, contents: convergedState },
        });
      } else {
        diagramFile.metadata.error = stepResult.error;
        dispatch({ type: "UPDATE_OPEN_FILE", file: diagramFile });
      }
    }
  } else {
    diagramFile.metadata.error = compileResult.error;
    dispatch({ type: "UPDATE_OPEN_FILE", file: diagramFile });
  }
}

export const useUpdateNodeToNewDiagram = (
  dispatch: FileDispatcher,
  workspaceState: IWorkspaceState
) =>
  useCallback(
    (node: TabNode, trioSelection: TrioSelection, autostep: boolean) => {
      const id = v4();
      const diagramPointer: DiagramFilePointer = {
        type: "diagram_state",
        id: id,
        substance: trioSelection.substance as SubstanceFilePointer,
        style: trioSelection.style as StyleFilePointer,
        domain: trioSelection.domain as DomainFilePointer,
        name: "Diagram",
        location: {
          type: "local",
          localStorageKey: id,
        },
      };
      const diagramFile: StateFile = {
        type: "state_file",
        id: diagramPointer.id,
        contents: null,
        metadata: {
          error: null,
          autostep: true,
        },
      };
      dispatch({
        type: "OPEN_FILE",
        file: diagramFile,
        pointer: diagramPointer,
      });
      workspaceState.openWorkspace.layout.doAction(
        Actions.updateNodeAttributes(node.getId(), {
          component: "file",
          name: "Diagram",
          id,
        })
      );
      _compileDiagram(dispatch, workspaceState, diagramPointer, autostep);
    },
    [dispatch, workspaceState]
  );

export const useCompileDiagram = (
  dispatch: FileDispatcher,
  workspaceState: IWorkspaceState
) =>
  useCallback(
    (diagramPointer: DiagramFilePointer) => {
      _compileDiagram(dispatch, workspaceState, diagramPointer, true);
    },
    [dispatch, workspaceState]
  );

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
