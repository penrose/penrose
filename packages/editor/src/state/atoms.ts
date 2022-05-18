import { compileDomain, Env, PenroseError, PenroseState } from "@penrose/core";
import localforage from "localforage";
import { debounce } from "lodash";
import { atom, AtomEffect, selector, selectorFamily } from "recoil";
import { v4 as uuid } from "uuid";

export type ProgramType = "substance" | "style" | "domain";

export type WorkspaceLocation =
  | {
      kind: "local";
      /**
       * True if file is explicitly saved to localStorage
       */
      saved: boolean;
    }
  | { kind: "gist"; id: string; author: string };

export type WorkspaceMetadata = {
  name: string;
  lastModified: string;
  id: string;
  forked_from_id?: string;
  editor_version: number;
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
  /*default: selector<LocalWorkspaces>({
    key: "localFiles/default",
    get: async () => {
      const locals = await localforage.getItem("local_files");
      return (locals !== null ? locals : {}) as LocalWorkspaces;
    },
  }),*/
  effects: [localFilesEffect],
});

/**
 * On any state change to the workspace, if it's being saved, autosave it (debounced)
 */
const saveWorkspaceEffect: AtomEffect<Workspace> = ({ onSet }) => {
  onSet(
    // HACK: this isn't typesafe
    debounce(async (newValue: Workspace, oldValue, isReset) => {
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

export const currentWorkspaceState = atom<Workspace>({
  key: "currentWorkspace",
  default: {
    metadata: {
      name: "Untitled Diagram",
      id: uuid(),
      lastModified: new Date().toISOString(),
      editor_version: 0.1,
      location: { kind: "local", saved: false },
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
  effects: [saveWorkspaceEffect],
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
  autostep: boolean;
};

export type Diagram = {
  state: PenroseState | null;
  error: PenroseError | null;
  metadata: DiagramMetadata;
};

// TODO we COULD prepopulate the default with a compilation of the source, assuming it doesnt error
export const diagramState = atom<Diagram>({
  key: "diagramState",
  default: {
    state: null,
    error: null,
    metadata: {
      variation: uuid(),
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
