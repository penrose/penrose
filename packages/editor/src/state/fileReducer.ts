import { Dispatch } from "react";
import { IExamples, IFileSystemState } from "../types/FileSystem";

export type FileAction =
  | { type: "UPDATE_FILE" }
  | { type: "SET_EXAMPLES"; examples: IExamples };

export type FileDispatcher = Dispatch<FileAction>;
export function initialState(): IFileSystemState {
  return {
    fileSystem: {
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
    },
    workspace: {
      fileContents: {},
      workspace: "",
    },
  };
}

export default function reducer(
  state: IFileSystemState,
  action: FileAction
): IFileSystemState {
  switch (action.type) {
    case "UPDATE_FILE":
      return initialState();
    case "SET_EXAMPLES":
      return {
        ...state,
        fileSystem: { ...state.fileSystem, examples: action.examples },
      };
  }
}
