import {
  compileDomain,
  Env,
  PenroseError,
  PenroseState,
  readRegistry,
  Trio,
} from "@penrose/core";
import { Actions, BorderNode, TabNode } from "flexlayout-react";
import localforage from "localforage";
import { debounce } from "lodash";
import toast from "react-hot-toast";
import {
  atom,
  AtomEffect,
  DefaultValue,
  selector,
  selectorFamily,
} from "recoil";
import { v4 as uuid } from "uuid";
import { layoutModel } from "../App";
import { generateVariation } from "./variation";

export type ProgramType = "substance" | "style" | "domain";

export type GistLocation = {
  kind: "gist";
  id: string;
  author: string;
  avatar: string;
};

export type WorkspaceLocation =
  | {
      kind: "local";
      /**
       * True if file is explicitly saved to localStorage
       */
      saved: boolean;
    }
  | GistLocation
  | {
      kind: "example";
      root: string; // URL to the parent folder of the Style file
    }
  | {
      kind: "roger";
      style?: string; // path to the Style file
      substance?: string; // path to the Substance file
      domain?: string; // path to the Domain file
    };

export type WorkspaceMetadata = {
  name: string;
  lastModified: string;
  id: string;
  forkedFromGist: string | null;
  editorVersion: number;
  location: WorkspaceLocation;
};

export type ProgramFile = {
  contents: string;
  name: string;
};

export type Workspace = {
  metadata: WorkspaceMetadata;
  files: {
    substance: ProgramFile;
    style: ProgramFile;
    domain: ProgramFile;
  };
};

export type LocalWorkspaces = {
  [id: string]: WorkspaceMetadata;
};

export type RogerState =
  | {
      kind: "disconnected";
    }
  | {
      kind: "connected";
      ws: WebSocket;
      substance: string[];
      style: string[];
      domain: string[];
    };

const localFilesEffect: AtomEffect<LocalWorkspaces> = ({ setSelf, onSet }) => {
  setSelf(
    localforage
      .getItem("local_files")
      .then(
        (savedValue) =>
          (savedValue != null ? savedValue : {}) as LocalWorkspaces
      )
  );

  onSet((newValue, _, isReset) => {
    isReset
      ? localforage.removeItem("local_files")
      : localforage.setItem("local_files", newValue);
  });
};

export const localFilesState = atom<LocalWorkspaces>({
  key: "localFiles",
  default: {},
  effects: [localFilesEffect],
});

/**
 * On any state change to the workspace, if it's being saved, autosave it (debounced)
 */
const saveWorkspaceEffect: AtomEffect<Workspace> = ({ onSet, setSelf }) => {
  onSet(
    // HACK: this isn't typesafe
    debounce(async (newValue: Workspace, oldValue, isReset) => {
      // If edit is made on something that isnt already local
      if (
        newValue.metadata.id === oldValue.metadata.id &&
        newValue.metadata.location.kind !== "local" &&
        newValue.metadata.location.kind !== "roger"
      ) {
        setSelf((workspace) => ({
          ...(workspace as Workspace),
          metadata: {
            ...(workspace as Workspace).metadata,
            location: { kind: "local", saved: false },
            forkedFromGist:
              newValue.metadata.location.kind === "gist"
                ? newValue.metadata.location.id
                : null,
          } as WorkspaceMetadata,
        }));
      }
      // If the workspace is already in localStorage
      if (
        newValue.metadata.location.kind === "local" &&
        newValue.metadata.location.saved
      ) {
        await localforage.setItem(newValue.metadata.id, newValue);
      }
    }, 500)
  );
};

/**
 * When workspace is loaded in, sync the fileNames with the layout
 */
const syncFilenamesEffect: AtomEffect<Workspace> = ({ onSet }) => {
  onSet((newValue: Workspace, oldValue: Workspace | DefaultValue) => {
    if (oldValue instanceof DefaultValue) {
      return;
    }
    if (newValue.metadata.id !== oldValue.metadata.id) {
      layoutModel.visitNodes((node) => {
        if (node.getType() === "tab" && (node as TabNode).getConfig()) {
          const kind = (node as TabNode).getConfig().kind as ProgramType;
          layoutModel.doAction(
            Actions.renameTab(node.getId(), newValue.files[kind].name)
          );
        }
      });
    }
  });
};

export const currentWorkspaceState = atom<Workspace>({
  key: "currentWorkspace",
  default: {
    metadata: {
      name: "Untitled Diagram",
      id: uuid(),
      lastModified: new Date().toISOString(),
      editorVersion: 0.1,
      location: { kind: "local", saved: false },
      forkedFromGist: null,
    },
    files: {
      substance: {
        name: ".sub",
        contents: "",
      },
      style: {
        name: ".sty",
        contents: "",
      },
      domain: {
        name: ".dsl",
        contents: "",
      },
    },
  },
  effects: [saveWorkspaceEffect, syncFilenamesEffect],
});

export const currentRogerState = atom<RogerState>({
  key: "currentRogerState",
  default: { kind: "disconnected" },
});

/**
 * Access workspace's files granularly
 */
export const fileContentsSelector = selectorFamily<ProgramFile, ProgramType>({
  key: "fileContents",
  get: (programType: ProgramType) => ({ get }) => {
    return get(currentWorkspaceState).files[programType];
  },
  set: (programType: ProgramType) => ({ set }, newValue) => {
    set(currentWorkspaceState, (state) => ({
      ...state,
      files: { ...state.files, [programType]: newValue },
    }));
  },
});

/**
 * Access just the workspace's metadata.
 * Auto updates the localStorage via effects.
 */
export const workspaceMetadataSelector = selector<WorkspaceMetadata>({
  key: "workspaceMetadata",
  get: ({ get }) => {
    return get(currentWorkspaceState).metadata;
  },
  set: ({ set }, newValue) => {
    const newMetadata = newValue as WorkspaceMetadata;
    set(currentWorkspaceState, (state) => ({
      ...state,
      metadata: newMetadata,
    }));
    // If local & saved, add it to the localFiles
    if (newMetadata.location.kind === "local" && newMetadata.location.saved) {
      set(localFilesState, (state) => ({
        ...state,
        [(newValue as WorkspaceMetadata).id]: newValue as WorkspaceMetadata,
      }));
    }
  },
});

export const domainCacheState = selector<Env | null>({
  key: "domainCache",
  get: ({ get }) => {
    const domainProgram = get(fileContentsSelector("domain")).contents;
    const compiledDomain = compileDomain(domainProgram);
    if (compiledDomain.isOk()) {
      return compiledDomain.value;
    }
    return null;
  },
});

export type DiagramMetadata = {
  variation: string;
  stepSize: number;
  autostep: boolean;
};

export type Diagram = {
  state: PenroseState | null;
  error: PenroseError | null;
  metadata: DiagramMetadata;
};

export const diagramState = atom<Diagram>({
  key: "diagramState",
  default: {
    state: null,
    error: null,
    metadata: {
      variation: generateVariation(),
      stepSize: 10000,
      autostep: true,
    },
  },

  //   necessary due to diagram extension
  dangerouslyAllowMutability: true,
});

/**
 * Pulls out just the metadata for efficiency
 */
export const diagramMetadataSelector = selector<DiagramMetadata>({
  key: "diagramMetadata",
  get: ({ get }) => {
    return get(diagramState).metadata;
  },
  set: ({ set }, newValue) => {
    set(diagramState, (state) => ({
      ...state,
      metadata: newValue as DiagramMetadata,
    }));
  },
});

export const exampleTriosState = atom<Trio[]>({
  key: "exampleTrios",
  default: selector({
    key: "exampleTrios/default",
    get: async () => {
      try {
        const res = await fetch(
          "https://raw.githubusercontent.com/penrose/penrose/main/packages/examples/src/registry.json"
        );
        if (!res.ok) {
          toast.error(`Could not retrieve examples: ${res.statusText}`);
          return [];
        }
        const registry = await res.json();
        const trios = readRegistry(registry)
          .filter((e) => "showInIDE" in e && e.showInIDE)
          .map((trio: Trio) => ({
            ...trio,
            substanceURI: registry.root + trio.substanceURI,
            styleURI: registry.root + trio.styleURI,
            domainURI: registry.root + trio.domainURI,
          }));
        return trios;
      } catch (err) {
        toast.error(`Could not retrieve examples: ${err}`);
        return [];
      }
    },
  }),
});

export type LocalGithubUser = {
  username: string;
  avatar: string;
  accessToken: string;
};

export type Settings = {
  github: LocalGithubUser | null;
  vimMode: boolean;
  debugMode: boolean;
};

const settingsEffect: AtomEffect<Settings> = ({ setSelf, onSet }) => {
  setSelf(
    localforage
      .getItem("settings")
      .then(
        (savedValue) =>
          (savedValue != null ? savedValue : new DefaultValue()) as Settings
      )
  );

  onSet((newValue, _, isReset) => {
    isReset
      ? localforage.removeItem("settings")
      : localforage.setItem("settings", newValue);
  });
};
const debugModeEffect: AtomEffect<Settings> = ({ onSet }) => {
  onSet((newValue, _, isReset) => {
    layoutModel.visitNodes((node) => {
      if (
        node.getType() === "border" &&
        (node as BorderNode).getClassName() === "debugBorder"
      ) {
        layoutModel.doAction(
          Actions.updateNodeAttributes(node.getId(), {
            show: newValue.debugMode,
          })
        );
      }
    });
  });
};

export const settingsState = atom<Settings>({
  key: "settings",
  default: {
    github: null,
    vimMode: false,
    // debug mode is on by default in local dev mode
    debugMode: process.env.NODE_ENV === "development",
  },
  effects: [settingsEffect, debugModeEffect],
});
