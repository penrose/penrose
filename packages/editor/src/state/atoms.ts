import {
  compileDomain,
  DomainEnv,
  PenroseError,
  PenroseWarning,
} from "@penrose/core";
import { PathResolver, Trio, TrioMeta } from "@penrose/examples/dist/index.js";
import registry from "@penrose/examples/dist/registry.js";
import { User as FirebaseUser } from "firebase/auth";
import { Actions, BorderNode, TabNode } from "flexlayout-react";
import localforage from "localforage";
import { debounce, range } from "lodash";
import { RefObject } from "react";
import toast from "react-hot-toast";
import {
  atom,
  AtomEffect,
  DefaultValue,
  selector,
  selectorFamily,
} from "recoil";
import { v4 as uuid } from "uuid";
import { layoutModel } from "../App.js";
import { RenderState } from "../worker/common.js";
import OptimizerWorker from "../worker/OptimizerWorker.js";
import { generateVariation } from "./variation.js";

export const optimizer = new OptimizerWorker();

export const EDITOR_VERSION = 0.1;

export type ProgramType = "substance" | "style" | "domain";

export type GistLocation = {
  kind: "gist";
  id: string;
  author: string;
  avatar: string;
};

export type WorkspaceLocation =
  /**
   * "local" used for unstored non-example workspaces, to distinguish for
   * save button functionality
   */
  | { kind: "local" }
  | {
      /**
       * If file is explicitly saved to cloud storage
       */
      kind: "stored";
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
  lastModified: number;
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

export type SavedWorkspaces = {
  [id: string]: Workspace;
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

export type AuthModalState = {
  loginIsOpen: boolean;
  registerIsOpen: boolean;
};

export type AppUser = FirebaseUser | null;

export type AutosaveTimer = NodeJS.Timeout | null;

export const savedFilesState = atom<SavedWorkspaces>({
  key: "savedFiles",
  default: {},
  effects: [],
});

export const autosaveTimerState = atom<AutosaveTimer>({
  key: "autosaveTimerState",
  default: null,
});

/**
 * On any state change to a stored workspace, mark it as unsaved (debounced)
 */
const markWorkspaceUnsavedEffect: AtomEffect<Workspace> = ({
  onSet,
  setSelf,
}) => {
  onSet(
    // HACK: this isn't typesafe (comment from old saveWorkspaceEffect)
    debounce(async (newValue: Workspace, oldValue) => {
      // Check equal ids to prevent state change when swapping active diagram
      // Check equal saved values to prevent this effect from self-triggering
      if (
        newValue.metadata.location.kind == "stored" &&
        newValue.metadata.location.saved &&
        newValue.metadata.id == oldValue.metadata.id &&
        newValue.metadata.location.saved == oldValue.metadata.location.saved
      ) {
        setSelf((workspaceOrDefault) => {
          const workspace = workspaceOrDefault as Workspace;
          let resolver: PathResolver | undefined = undefined;
          return {
            ...workspace,
            metadata: {
              ...workspace.metadata,
              location: { kind: "stored", saved: false, resolver },
              forkedFromGist:
                newValue.metadata.location.kind === "gist"
                  ? newValue.metadata.location.id
                  : null,
            } as WorkspaceMetadata,
          };
        });
        console.log("hit mark unsaved");
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
    lastModified: Date.parse(new Date().toISOString()),
    editorVersion: 0.1,
    location: { kind: "local" },
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
  effects: [markWorkspaceUnsavedEffect, syncFilenamesEffect],
});

export const currentRogerState = atom<RogerState>({
  key: "currentRogerState",
  default: { kind: "disconnected" },
});

export const currentAuthModalState = atom<AuthModalState>({
  key: "currentLoginModalState",
  default: { loginIsOpen: false, registerIsOpen: false },
});

export const currentAppUser = atom<AppUser>({
  key: "currentAppUser",
  default: null,
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
  },
});

export const domainCacheState = selector<DomainEnv | null>({
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
  id: string;
  init: boolean;
  optimizing: boolean;
}>({
  key: "diagramWorkerState",
  default: {
    id: "",
    init: false,
    optimizing: false,
  },
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
  githubAccessToken: string | null;
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
    githubAccessToken: null,
    vimMode: false,
    // debug mode is on by default in local dev mode
    debugMode: process.env.NODE_ENV === "development",
  },
  effects: [settingsEffect, debugModeEffect],
});
