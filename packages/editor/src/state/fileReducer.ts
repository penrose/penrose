import { Env, stepState } from "@penrose/core";
import { Actions, DockLocation, Model } from "flexlayout-react";
import { Dispatch } from "react";
import { v4 } from "uuid";
import {
  constructLayout,
  FilePointer,
  IExamples,
  IFileSystemState,
  IWorkspace,
  IWorkspaceState,
  SavedFile,
  TrioType,
} from "../types/FileSystem";

export type FileAction =
  | { type: "UPDATE_OPEN_FILE"; file: SavedFile }
  | { type: "UPDATE_LAYOUT"; layout: Model }
  | { type: "SET_WORKSPACE"; workspaceState: IWorkspaceState }
  | { type: "OPEN_FILE"; file: SavedFile; pointer: FilePointer }
  | { type: "CLOSE_FILE"; id: string }
  | { type: "SET_DOMAIN_CACHE"; domainCache: Env | null };

export type FileDispatcher = Dispatch<FileAction>;
export function initialFilesState(): IFileSystemState {
  const date = new Date();
  const workspaceId = v4();
  return {
    fileSystem: {
      workspaces: {},
      substances: {},
      styles: {},
      domains: {},
      diagrams: {},
    },
    workspace: {
      fileContents: {},
      openWorkspace: {
        domainCache: null,
        openFiles: {},
        name: `${date.getMonth() + 1}/${date.getDate()}/${date.getFullYear()}`,
        id: workspaceId,
        creator: null,
        forkedFrom: null,
        layout: constructLayout([
          {
            type: "tabset",
            weight: 100,
            children: [
              { type: "tab", name: "examples", component: "examples" },
            ],
          },
        ]),
      },
    },
  };
}

// Adapted from https://stackoverflow.com/questions/34401098/remove-a-property-in-an-object-immutably#comment83640640_47227198
const deleteProperty = (obj: any, key: string) => {
  const { [key]: _, ...newObj } = obj;
  return newObj;
};

export default function FileReducer(
  state: IFileSystemState,
  action: FileAction
): IFileSystemState {
  switch (action.type) {
    case "SET_DOMAIN_CACHE":
      return {
        ...state,
        workspace: {
          ...state.workspace,
          openWorkspace: {
            ...state.workspace.openWorkspace,
            domainCache: action.domainCache,
          },
        },
      };
    case "UPDATE_OPEN_FILE":
      // TODO: if example/gist, create new!
      return {
        ...state,
        workspace: {
          ...state.workspace,
          fileContents: {
            ...state.workspace.fileContents,
            [action.file.id]: action.file,
          },
        },
      };
    case "OPEN_FILE":
      // TODO: save
      return {
        ...state,
        workspace: {
          ...state.workspace,
          fileContents: {
            ...state.workspace.fileContents,
            [action.file.id]: action.file,
          },
          openWorkspace: {
            ...state.workspace.openWorkspace,
            openFiles: {
              ...state.workspace.openWorkspace.openFiles,
              [action.pointer.id]: action.pointer,
            },
          },
        },
      };
    case "CLOSE_FILE":
      // TODO: save
      return {
        ...state,
        workspace: {
          ...state.workspace,
          fileContents: deleteProperty(state.workspace.fileContents, action.id),
          openWorkspace: {
            ...state.workspace.openWorkspace,
            openFiles: deleteProperty(
              state.workspace.openWorkspace.openFiles,
              action.id
            ),
          },
        },
      };
    case "UPDATE_LAYOUT":
      return {
        ...state,
        workspace: {
          ...state.workspace,
          openWorkspace: {
            ...state.workspace.openWorkspace,
            layout: action.layout,
          },
        },
      };
    case "SET_WORKSPACE":
      return {
        ...state,
        workspace: action.workspaceState,
      };
  }
}
