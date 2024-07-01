import {
  DomainCache,
  SubstanceCache,
  getDomainCache,
  getSubstanceCache,
} from "@penrose/components";
import { PenroseError, PenroseWarning } from "@penrose/core";
import { PathResolver, Trio, TrioMeta } from "@penrose/examples/dist/index.js";
import registry from "@penrose/examples/dist/registry.js";
import { Actions, BorderNode, TabNode } from "flexlayout-react";
import localforage from "localforage";
import { debounce, range } from "lodash";
import { RefObject } from "react";
import toast from "react-hot-toast";
import {
  AtomEffect,
  DefaultValue,
  atom,
  selector,
  selectorFamily,
} from "recoil";
import { v4 as uuid } from "uuid";
import { layoutModel } from "../App.js";

import {
  DiagramID,
  LayoutStats,
  RenderState,
  StepSequenceID,
} from "../optimizer/common.js";
import Optimizer from "../optimizer/optimizer.js";

import { generateVariation } from "./variation.js";

export const optimizer = await Optimizer.create();

export const EDITOR_VERSION = 0.1;

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
      resolver?: PathResolver;
    }
  | GistLocation
  | {
      kind: "example";
      resolver: PathResolver;
    }
  | {
      kind: "roger";
      style?: string; // path to the Style file
      substance?: string; // path to the Substance file
      domain?: string; // path to the Domain file
    };

export type WorkspaceMetadata = {
  name: string;
  // ISO String of date
  lastModified: string;
  id: string;
  // Gist ID
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
      trio: string[];
    };

const localFilesEffect: AtomEffect<LocalWorkspaces> = ({ setSelf, onSet }) => {
  setSelf(
    localforage
      .getItem("local_files")
      .then(
        (savedValue) =>
          (savedValue != null ? savedValue : {}) as LocalWorkspaces,
      ),
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
 * TODO: changes that happen within the 500ms window will not be collected, this is an
 *       issue with things that are not directly related to editing trios i.e. duplicating
 *       workspaces, saving a new workspace, etc., see issue #1695
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
        setSelf((workspaceOrDefault) => {
          const workspace = workspaceOrDefault as Workspace;
          let resolver: PathResolver | undefined = undefined;
          if (workspace.metadata.location.kind === "example")
            resolver = workspace.metadata.location.resolver;
          return {
            ...workspace,
            metadata: {
              ...workspace.metadata,
              location: { kind: "local", saved: false, resolver },
              forkedFromGist:
                newValue.metadata.location.kind === "gist"
                  ? newValue.metadata.location.id
                  : null,
            } as WorkspaceMetadata,
          };
        });
      }
      // If the workspace is already in localStorage
      if (
        newValue.metadata.location.kind === "local" &&
        newValue.metadata.location.saved
      ) {
        await localforage.setItem(newValue.metadata.id, newValue);
      }
    }, 500),
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
            Actions.renameTab(node.getId(), newValue.files[kind].name),
          );
        }
      });
    }
  });
};

export const defaultWorkspaceState = (): Workspace => ({
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
      name: ".substance",
      contents: "",
    },
    style: {
      name: ".style",
      contents: `canvas {
  width = 400
  height = 400
}`,
    },
    domain: {
      name: ".domain",
      contents: "",
    },
  },
});

export const currentWorkspaceState = atom<Workspace>({
  key: "currentWorkspace",
  default: defaultWorkspaceState(),
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
  get:
    (programType: ProgramType) =>
    ({ get }) => {
      return get(currentWorkspaceState).files[programType];
    },
  set:
    (programType: ProgramType) =>
    ({ set }, newValue) => {
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

export const domainCacheState = selector<DomainCache>({
  key: "domainCache",
  get: ({ get }) => {
    const domainProgram = get(fileContentsSelector("domain")).contents;
    const domainCache = getDomainCache(domainProgram);
    return domainCache;
  },
});

export const substanceCacheState = selector<SubstanceCache>({
  key: "substanceCache",
  get: ({ get }) => {
    const substanceProgram = get(fileContentsSelector("substance")).contents;
    const substanceCache = getSubstanceCache(substanceProgram);
    return substanceCache;
  },
});

export type DiagramMetadata = {
  variation: string;
  stepSize: number;
  autostep: boolean;
  interactive: boolean;
  excludeWarnings: string[];
  source: {
    domain: string;
    substance: string;
    style: string;
  };
};

export type Diagram = {
  state: RenderState | null;
  error: PenroseError | null;
  warnings: PenroseWarning[];
  layoutStats: LayoutStats;
  diagramId: DiagramID | null;
  stepSequenceId: StepSequenceID | null;
  metadata: DiagramMetadata;
};

export type Canvas = {
  ref: RefObject<HTMLDivElement> | null;
};

export const canvasState = atom<Canvas>({
  key: "canvasState",
  default: {
    ref: null,
  },
});

export const diagramState = atom<Diagram>({
  key: "diagramState",
  default: {
    state: null,
    error: null,
    warnings: [],
    layoutStats: [],
    diagramId: null,
    stepSequenceId: null,
    metadata: {
      variation: generateVariation(),
      stepSize: 10000,
      autostep: true,
      interactive: false,
      excludeWarnings: [],
      source: {
        substance: "",
        style: "",
        domain: "",
      },
    },
  },

  //   necessary due to diagram extension
  dangerouslyAllowMutability: true,
});

export type LayoutTimeline = number[][];

export const layoutTimelineState = atom<LayoutTimeline>({
  key: "layoutTimelineState",
  default: [],
});

export const diagramWorkerState = atom<{
  compiling: boolean;
  optimizing: boolean;
}>({
  key: "diagramWorkerState",
  default: {
    compiling: false,
    optimizing: false,
  },
});

export const showCompileErrsState = atom<boolean>({
  key: "showCompileErrsState",
  default: false,
});

export type DiagramGrid = {
  variations: string[];
  gridSize: number;
};

const gridSizeEffect: AtomEffect<DiagramGrid> = ({ onSet, setSelf }) => {
  onSet((newValue, oldValue) => {
    const old = oldValue as DiagramGrid;
    if (newValue.gridSize > old.gridSize) {
      setSelf({
        ...newValue,
        variations: [
          ...old.variations,
          ...range(newValue.gridSize - old.gridSize).map(() =>
            generateVariation(),
          ),
        ],
      });
    }
  });
};

export const diagramGridState = atom<DiagramGrid>({
  key: "diagramGridState",
  default: {
    variations: range(10).map((i) => generateVariation()),
    gridSize: 10,
  },
  effects: [gridSizeEffect],
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
    set(diagramGridState, ({ gridSize }) => ({
      variations: range(gridSize).map((i) =>
        i === 0 ? (newValue as DiagramMetadata).variation : generateVariation(),
      ),
      gridSize,
    }));
  },
});

export interface TrioWithPreview {
  id: string;
  get: () => Promise<Trio>;
  name?: string;
  preview?: string;
}

export const exampleTriosState = atom<TrioWithPreview[]>({
  key: "exampleTrios",
  default: selector({
    key: "exampleTrios/default",
    get: async () => {
      try {
        const trios: [string, TrioMeta][] = [];
        for (const [id, meta] of registry.entries()) {
          if (meta.trio && meta.gallery) {
            trios.push([id, meta]);
          }
        }
        return Promise.all(
          trios.map(async ([id, { get, name }]) => {
            const svg = await fetch(
              encodeURI(
                `https://raw.githubusercontent.com/penrose/penrose/ci/refs/heads/main/${id}.svg`,
              ),
            );
            const trio: TrioWithPreview = { id, get, name };
            if (!svg.ok) {
              console.error(`could not fetch preview for ${id}`);
              return {
                ...trio,
                preview: `<svg><rect fill="#cbcbcb" width="50" height="50"/></svg>`,
              };
            }
            return { ...trio, preview: await svg.text() };
          }),
        );
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

export type GistMetadata = {
  name: string;
  editorVersion: number;
  forkedFromGist: string | null;
  fileNames: {
    substance: string;
    style: string;
    domain: string;
  };
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
          (savedValue != null ? savedValue : new DefaultValue()) as Settings,
      ),
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
          }),
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

export const codemirrorHistory = atom<boolean>({
  key: "codemirrorHistory",
  default: true,
});
