import { Model } from "flexlayout-react";
import { Dispatch } from "react";
import { v4 } from "uuid";
import {
  constructLayout,
  IExamples,
  IFileSystemState,
  IWorkspace,
  IWorkspaceState,
  SavedFile,
} from "../types/FileSystem";

export type FileAction =
  | { type: "UPDATE_OPEN_FILE"; file: SavedFile }
  | { type: "UPDATE_LAYOUT"; layout: Model }
  | { type: "SET_WORKSPACE"; workspaceState: IWorkspaceState };

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
        compileTrioSetting: {
          substance: null,
          style: null,
          domain: null,
        },
        name: `${date.getMonth()}/${date.getDate()}/${date.getFullYear()}`,
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

export default function FileReducer(
  state: IFileSystemState,
  action: FileAction
): IFileSystemState {
  switch (action.type) {
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
