import { PenroseState } from "@penrose/core";
import { IJsonRowNode } from "flexlayout-react";

export interface ProgramFile {
  contents: string | PenroseState;
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
export interface ITrioPointer {
  substance: string;
  style: string;
  domain: string;
}

export interface IWorkspace {
  openFiles: { [id: string]: IOpenFile };
  /* Which files to compile into diagram by default */
  compileTrioSetting: ITrioPointer;
  /* defaults to today's date */
  name: string;
  creator: null;
  /* True if gist or example trio */
  immutable: boolean;
  /* id key in tabs points to openFiles */
  layout: IJsonRowNode;
}

export interface ILocal {
  workspaces: IWorkspace[];
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
  currentWorkspace: IWorkspace;
  local: ILocal;
  examples: IExamples;
}

// if example/gist, then "fork" immediately on attempt to save
// ie make new pointer
