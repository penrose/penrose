import { Env, PenroseState } from "@penrose/core";
import { IJsonTabSetNode, Model } from "flexlayout-react";

export const constructLayout = (children: IJsonTabSetNode[]): Model =>
  Model.fromJson({
    global: {},
    borders: [
      {
        type: "border",
        location: "left",
        show: true,
        children: [
          {
            type: "tab",
            name: "files",
            enableClose: false,
          },
          {
            type: "tab",
            name: "examples",
            enableClose: false,
            component: "examples",
          },
          {
            type: "tab",
            name: "settings",
            component: "settings",
            enableClose: false,
          },
        ],
      },
    ],
    layout: {
      type: "row",
      weight: 100,
      children,
    },
  });

export interface ProgramFile {
  type: "program_file";
  contents: string;
  id: string;
}
export interface StateFile {
  type: "state_file";
  contents: PenroseState;
  id: string;
}
export interface WorkspaceFile {
  type: "workspace_file";
  contents: IWorkspace;
  id: string;
}
export type SavedFile = ProgramFile | StateFile | WorkspaceFile;

interface IExampleLocation {
  type: "example";
  path: string;
}

interface IGistLocation {
  type: "gist";
  gistURI: string;
}

interface ILocalLocation {
  type: "local";
  /**
   * Defaults to pointer's id
   */
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

export interface IWorkspacePointer extends _FilePointer {
  type: "workspace";
}

/**
 * Workaround for example workspaces
 */
export interface ICachedWorkspacePointer extends _FilePointer {
  type: "cached_workspace";
  files: { [id: string]: FilePointer };
}

export type FilePointer =
  | DomainFilePointer
  | StyleFilePointer
  | SubstanceFilePointer
  | DiagramFilePointer
  | IWorkspacePointer
  | ICachedWorkspacePointer;

export interface ITrioSetting {
  substance: string | null;
  style: string | null;
  domain: string | null;
}

export type TrioType = "substance" | "style" | "domain";

export type FilePointerMap = { [id: string]: FilePointer };
export interface IWorkspace {
  /**
   * Cached file contents in @IWorkspaceState
   */
  openFiles: FilePointerMap;
  /* Which files to compile into diagram by default */
  compileTrioSetting: ITrioSetting;
  domainCache: Env | null;
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
  layout: Model;
  /* TODO: change tab appearance based on if it's in compileTrioSetting
    https://rawgit.com/caplin/FlexLayout/demos/demos/v0.6/typedoc/interfaces/IJsonTabNode.html#icon 
  */
}

/**
 * We separate the filePointers so they're easier to enumerate in the UI
 */
export interface ILocalFileSystem {
  workspaces: { [id: string]: IWorkspacePointer };
  substances: { [id: string]: SubstanceFilePointer };
  styles: { [id: string]: StyleFilePointer };
  domains: { [id: string]: DomainFilePointer };
  diagrams: { [id: string]: DiagramFilePointer };
}

export interface IExamples {
  substances: { [id: string]: SubstanceFilePointer };
  styles: { [id: string]: StyleFilePointer };
  domains: { [id: string]: DomainFilePointer };
  trios: { [id: string]: IWorkspacePointer };
}

/* 
  When gist/example is open, fileContents is pre-hydrated
*/
export interface IWorkspaceState {
  /**
   * The in-memory loaded files for the workspace
   */
  fileContents: { [id: string]: SavedFile };
  openWorkspace: IWorkspace;
}

export interface IFileSystemState {
  fileSystem: ILocalFileSystem;
  workspace: IWorkspaceState;
}
