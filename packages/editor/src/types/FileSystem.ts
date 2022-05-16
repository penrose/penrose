import { Env, PenroseError, PenroseState } from "@penrose/core";
import { IJsonModel, IJsonTabSetNode, Model } from "flexlayout-react";

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
            component: "files",
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

export interface SubstanceFile {
  type: "substance_file";
  contents: string;
  id: string;
}

export interface StyleFile {
  type: "style_file";
  contents: string;
  id: string;
}

export interface DomainFile {
  type: "domain_file";
  contents: string;
  id: string;
  cache: Env | null;
}

export interface DiagramMetadata {
  error: PenroseError | null;
  autostep: boolean;
  variation: string;
  // maybe state enum: unoptimized, broken, etc
}
export interface DiagramFile {
  type: "diagram_file";
  contents: PenroseState | null;
  metadata: DiagramMetadata;
  id: string;
}
export interface WorkspaceFile {
  type: "workspace_file";
  contents: IWorkspaceJSON;
  id: string;
}

/**
 * Ids should never change
 */
export type SavedFile =
  | DomainFile
  | StyleFile
  | SubstanceFile
  | DiagramFile
  | WorkspaceFile;

export interface IExampleLocation {
  type: "example";
  path: string;
}

export interface IGistLocation {
  type: "gist";
  gistURI: string;
}

export interface ILocalLocation {
  type: "local";
  /**
   * Defaults to pointer's id
   */
  localStorageKey: string;
}

export type FileLocation = IExampleLocation | IGistLocation | ILocalLocation;

interface _FilePointer {
  /**
   * This Id should never change
   */
  id: string;
  name: string;
  location: FileLocation;
}

export interface DomainFilePointer extends _FilePointer {
  type: "domain";
}

export interface StyleFilePointer extends _FilePointer {
  type: "style";
  domain: DomainFilePointer;
  //   ...eventually, we could have
  //   dependencies: StyleFilePointer[] | ImagePointer[]
}

export interface SubstanceFilePointer extends _FilePointer {
  type: "substance";
  domain: DomainFilePointer;
}

/**
 * A rendered (or to-be-rendered) diagram
 */
export interface DiagramFilePointer extends _FilePointer {
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
  type: "workspace";
  files: { [id: string]: FilePointer };
}

export type FilePointer =
  | DomainFilePointer
  | StyleFilePointer
  | SubstanceFilePointer
  | DiagramFilePointer
  | IWorkspacePointer
  | ICachedWorkspacePointer;

export type TrioType = "substance" | "style" | "domain";

export type WorkspacePointer = IWorkspacePointer | ICachedWorkspacePointer;

export type FilePointerMap = { [id: string]: FilePointer };

export type FileContents = { [id: string]: SavedFile };

export interface IWorkspace {
  /**
   * Pointers to cached file contents in @FileContents
   */
  openFiles: FilePointerMap;
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
  layout: IJsonModel;
  location: FileLocation;
  /* TODO: change tab appearance based on if it's in compileTrioSetting
    https://rawgit.com/caplin/FlexLayout/demos/demos/v0.6/typedoc/interfaces/IJsonTabNode.html#icon 
  */
}

/**
 * JSON-encodable version of @IWorkspace
 */
export interface IWorkspaceJSON extends Omit<IWorkspace, "layout"> {
  layout: IJsonModel;
}

/**
 * We separate the filePointers so they're easier to enumerate in the UI
 * These are stored in localStorage/indexedDB
 */
export interface ILocalFileSystem {
  workspace: { [id: string]: IWorkspacePointer };
  substance: { [id: string]: SubstanceFilePointer };
  style: { [id: string]: StyleFilePointer };
  domain: { [id: string]: DomainFilePointer };
  diagram_state: { [id: string]: DiagramFilePointer };
}

export interface IExamples {
  substances: { [id: string]: SubstanceFilePointer };
  styles: { [id: string]: StyleFilePointer };
  domains: { [id: string]: DomainFilePointer };
  trios: { [id: string]: ICachedWorkspacePointer };
}
