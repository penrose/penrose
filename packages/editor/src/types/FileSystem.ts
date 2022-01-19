import { PenroseState } from "@penrose/core";
import { IJsonRowNode } from "flexlayout-react";

export interface ProgramFile {
  contents: string | PenroseState;
  id: string;
}

interface IExampleLocation {
  type: "example";
  path: string;
}

interface IGistLocation {
  type: "gist";
  gistURL: string;
}

interface ILocalLocation {
  type: "local";
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
  /**
   * GitHub Username
   */
  creator: string | null;
  /**
   * Gist ID. If I'm the creator, provide option to update.
   */
  forkedFrom: string | null;
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

export interface IFileSystemState {
  fileSystem: IFileSystem;
  workspace: IWorkspaceState;
}
