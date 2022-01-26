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
  | {
      type: "SET_TRIO_MEMBER";
      id: string;
      kind: "substance" | "style" | "domain";
    };

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
      if (
        action.pointer.type in
          state.workspace.openWorkspace.compileTrioSetting &&
        state.workspace.openWorkspace.compileTrioSetting[
          action.pointer.type as TrioType
        ] === null
      ) {
        state.workspace.openWorkspace.compileTrioSetting[
          action.pointer.type as TrioType
        ] = action.pointer.id;
      }
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
      // This is hideous. Removes from trio dropdowns if it's in there
      const trioSetting = Object.entries(
        state.workspace.openWorkspace.compileTrioSetting
      ).filter(([t, id]) => id === action.id);
      if (trioSetting.length > 0) {
        state.workspace.openWorkspace.compileTrioSetting[
          trioSetting[0][0] as TrioType
        ] =
          Object.values(state.workspace.openWorkspace.openFiles).find(
            ({ type, id }) => type === trioSetting[0][0] && id !== action.id
          )?.id ?? null;
      }
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
    case "SET_TRIO_MEMBER":
      // TODO: save
      return {
        ...state,
        workspace: {
          ...state.workspace,
          openWorkspace: {
            ...state.workspace.openWorkspace,
            compileTrioSetting: {
              ...state.workspace.openWorkspace.compileTrioSetting,
              [action.kind]: action.id,
            },
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
