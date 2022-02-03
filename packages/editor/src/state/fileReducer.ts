import { Env } from "@penrose/core";
import { Model } from "flexlayout-react";
import { Dispatch } from "react";
import { v4 } from "uuid";
import {
  constructLayout,
  FilePointer,
  IFileSystemState,
  IWorkspaceState,
  SavedFile,
} from "../types/FileSystem";

export type FileAction =
  | { type: "UPDATE_OPEN_FILE"; file: SavedFile }
  | { type: "UPDATE_LAYOUT"; layout: Model }
  | { type: "SET_WORKSPACE"; workspaceState: IWorkspaceState }
  | { type: "SET_DOMAIN_CACHE"; domainCache: Env | null };

export type FileDispatcher = Dispatch<FileAction>;
export function initialFilesState(): IFileSystemState {
  const date = new Date();
  const name = `${date.getMonth() + 1}/${date.getDate()}/${date.getFullYear()}`;
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
      workspacePointer: {
        type: "workspace",
        name,
        id: workspaceId,
        location: {
          localStorageKey: `workspace-${workspaceId}`,
          type: "local",
        },
      },
      fileContents: {},
      openWorkspace: {
        domainCache: null,
        openFiles: {},
        name,
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
export const deleteProperty = (obj: any, key: string) => {
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
