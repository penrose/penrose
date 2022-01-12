import { PenroseState } from "@penrose/core";
import { IJsonRowNode } from "flexlayout-react";

export interface ProgramFile {
  contents: string | PenroseState;
  id: string;
}

interface IExampleLocation {
  location: "example";
  path: string;
}

interface IGistLocation {
  location: "gist";
  gistURL: string;
}

interface ILocalLocation {
  location: "local";
  localStorageKey: string;
}

export type FileLocation = IExampleLocation | IGistLocation | ILocalLocation;

interface _FilePointer {
  id: string;
  name: string;
  location: FileLocation;
}

interface DomainFilePointer extends _FilePointer {
  type: "domain";
}

interface StyleFilePointer extends _FilePointer {
  type: "style";
  domain: DomainFilePointer;
  //   ...eventually, we could have
  //   dependencies: FilePointer[]
}

interface SubstanceFilePointer extends _FilePointer {
  type: "substance";
  domain: DomainFilePointer;
}

interface DiagramFilePointer extends _FilePointer {
  type: "diagram_state";
  substance: SubstanceFilePointer;
  style: StyleFilePointer;
  domain: DomainFilePointer;
}

export type FilePointer =
  | DomainFilePointer
  | StyleFilePointer
  | SubstanceFilePointer
  | DiagramFilePointer;

export interface IExampleTrio {
  substance: SubstanceFilePointer;
  style: StyleFilePointer;
  domain: DomainFilePointer;
}

export interface IOpenFile {
  file: ProgramFile;
  pointer: FilePointer;
}

/* points to IDs in openFiles */
export interface ITrioSetting {
  substance: string;
  style: string;
  domain: string;
}

export interface IWorkspace {
  openFiles: { [id: string]: FilePointer };
  /* Which files to compile into diagram by default */
  compileTrioSetting: ITrioSetting;
  /* defaults to today's date */
  name: string;
  id: string;
  creator: null;
  /* id key in tabs points to openFiles */
  layout: IJsonRowNode;
  /* TODO: change tab appearance based on if it's in compileTrioSetting
    https://rawgit.com/caplin/FlexLayout/demos/demos/v0.6/typedoc/interfaces/IJsonTabNode.html#icon 
  */
}

export interface ILocal {
  workspaces: { [id: string]: IWorkspace };
  substances: { [id: string]: SubstanceFilePointer };
  styles: { [id: string]: StyleFilePointer };
  domains: { [id: string]: DomainFilePointer };
  diagrams: { [id: string]: DiagramFilePointer };
}

export interface IExamples {
  substances: { [id: string]: SubstanceFilePointer };
  styles: { [id: string]: StyleFilePointer };
  domains: { [id: string]: DomainFilePointer };
  trios: IExampleTrio[];
}

export interface IFileSystem {
  local: ILocal;
  examples: IExamples;
}

/* The workspace's files state, in memory
  When gist/example is open, fileContents is pre-hydrated
*/
export interface IWorkspaceState {
  fileContents: { [id: string]: IOpenFile };
  workspace: string;
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
  switch (pointer.location.location) {
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
  if (pointer.location.location === "local") {
  } else {
    //  generate new local pointer with new Id
    //   change return type
    // override workspace author, if there is one
  }
  return { fileContents: {}, workspace: "" };
}
