import { Env } from "@penrose/core";
import { atom } from "recoil";
import { v4 as uuid } from "uuid";

export type WorkspaceLocation =
  | {
      kind: "local";
    }
  | { kind: "gist"; id: string; author: string };

export type WorkspaceMetadata = {
  name: string;
  lastModified: string;
  id: string;
  location: WorkspaceLocation;
};

export const workspaceMetadataState = atom<WorkspaceMetadata>({
  key: "workspaceMetadata",
  default: {
    name: "Untitled Diagram",
    id: uuid(),
    lastModified: new Date().toISOString(),
    location: { kind: "local" },
  },
});

export type ProgramFile = {
  contents: string;
  name: string;
};

export const substanceFileState = atom<ProgramFile>({
  key: "substanceFile",
  default: {
    contents: "",
    name: ".sub",
  },
});

export const styleFileState = atom<ProgramFile>({
  key: "styleFile",
  default: {
    contents: "",
    name: ".sty",
  },
});

export const domainFileState = atom<ProgramFile>({
  key: "domainFile",
  default: {
    contents: "",
    name: ".dsl",
  },
});

export const domainCacheState = atom<Env | null>({
  key: "domainCache",
  default: null,
});

// rendered diagram / error state / seed
