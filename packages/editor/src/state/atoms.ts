/**
 * This is where our state lives!
 */

import {
  compileDomain,
  compileTrio,
  prepareState,
  resample,
  stepUntilConvergence,
  variationSeeds,
} from "@penrose/core";
import {
  Actions,
  DockLocation,
  Model,
  TabNode,
  TabSetNode,
} from "flexlayout-react";
import localForage from "localforage";
import { debounce, isEmpty, memoize } from "lodash";
import { toast } from "react-toastify";
import { atom, AtomEffect, CallbackInterface, useRecoilCallback } from "recoil";
import { v4 } from "uuid";
import { TrioSelection } from "../components/DiagramInitializer";
import {
  constructLayout,
  DiagramFile,
  DiagramFilePointer,
  DomainFile,
  DomainFilePointer,
  FileContents,
  FileLocation,
  FilePointer,
  ILocalFileSystem,
  ILocalLocation,
  IWorkspace,
  IWorkspacePointer,
  SavedFile,
  StyleFile,
  StyleFilePointer,
  SubstanceFile,
  SubstanceFilePointer,
  WorkspaceFile,
  WorkspacePointer,
} from "../types/FileSystem";
import { retrieveFileFromPointer } from "./fileSystemActions";
// maybe TODO? derived state with file contents and pointer both

// Adapted from https://stackoverflow.com/questions/34401098/remove-a-property-in-an-object-immutably#comment83640640_47227198
export const deleteProperty = (obj: any, key: string) => {
  const { [key]: _, ...newObj } = obj;
  return newObj;
};

// Defaults
const _today = new Date();
const todayName = `${
  _today.getMonth() + 1
}/${_today.getDate()}/${_today.getFullYear()}`;
const firstId = v4();

// Memoized so that we dont over-debounce unrelated saves
const debouncedSave = memoize((key: string) =>
  debounce(async (file: SavedFile) => {
    const saveFile = { ...file };
    if (saveFile.type === "diagram_file") {
      saveFile.contents = null;
    } else if (saveFile.type === "domain_file") {
      saveFile.cache = null;
    }
    await localForage.setItem(key, saveFile);
  }, 500)
);

const defaultModel = constructLayout([
  {
    type: "tabset",
    weight: 100,
    children: [{ type: "tab", name: "examples", component: "examples" }],
  },
]);

export const layoutState = atom<Model>({
  key: "layoutState",
  default: defaultModel,
  //   necessary due to react flex layout mutating stuff all the time
  dangerouslyAllowMutability: true,
});

// When user changes a workspace property, conduct side effects
const _editWorkspace = async (set: any, workspace: IWorkspace) => {
  if (workspace.location.type !== "local") {
    // If updating a workspace that isn't "local" yet, save it
    toast.info("Forking new workspace...");
    const name = `fork of ${workspace.name}`;
    const location: FileLocation = {
      type: "local",
      localStorageKey: `workspace-${workspace.id}`,
    };
    const saved = { ...workspace, name, location };
    set(workspaceState, saved);

    const saveFile: WorkspaceFile = {
      type: "workspace_file",
      contents: saved,
      id: saved.id,
    };
    await debouncedSave(saved.location.localStorageKey)(saveFile);
    const pointer: IWorkspacePointer = {
      type: "workspace",
      id: saved.id,
      location,
      name,
    };
    _addLocalFileToFileSystem(set, pointer);
  } else if (
    workspace.location.type === "local" &&
    !isEmpty(workspace.openFiles)
  ) {
    //   If updating an already-local workspace, save it
    const saveFile: WorkspaceFile = {
      type: "workspace_file",
      contents: workspace,
      id: workspace.id,
    };
    await debouncedSave(saveFile.id)(saveFile);
  }
};

export const useSetLayout = () =>
  useRecoilCallback(({ set, snapshot }) => (model: Model) => {
    set(layoutState, model);
    const workspace = snapshot.getLoadable(workspaceState).contents!;
    _editWorkspace(set, { ...workspace, layout: model.toJson() });
  });

export const workspaceState = atom<IWorkspace>({
  key: "workspaceState",
  default: {
    openFiles: {},
    location: {
      type: "local",
      localStorageKey: `workspace-${firstId}`,
    },
    name: todayName,
    id: firstId,
    creator: null,
    forkedFrom: null,
    layout: defaultModel.toJson(),
  },
  effects: [],
});

export const fileContentsState = atom<FileContents>({
  key: "fileContents",
  default: {},
  //   necessary due to diagram extension
  dangerouslyAllowMutability: true,
});

const saveLocalFileSystemEffect: AtomEffect<ILocalFileSystem> = ({
  setSelf,
  onSet,
  trigger,
}) => {
  const loadFileSystem = async () => {
    const fileSystem = await localForage.getItem("localFileSystem");
    if (fileSystem !== null) {
      setSelf(fileSystem as ILocalFileSystem);
    }
  };
  if (trigger === "get") {
    loadFileSystem();
  }
  //  TODO: do this init in a useEffect,sorry
  onSet((newFileSystem) => {
    localForage.setItem("localFileSystem", newFileSystem);
  });
};

export const localFileSystem = atom<ILocalFileSystem>({
  key: "localFileSystem",
  default: {
    workspace: {},
    substance: {},
    style: {},
    domain: {},
    diagram_state: {},
  },
  effects: [saveLocalFileSystemEffect],
});

const _addLocalFileToFileSystem = (set: any, pointer: FilePointer) => {
  if (pointer.location.type !== "local") {
    console.error("cannot add non-local file to file system", pointer);
    return;
  }
  set(localFileSystem, (state: ILocalFileSystem) => ({
    ...state,
    [pointer.type]: {
      ...state[pointer.type],
      [pointer.id]: pointer,
    },
  }));
};

/**
 * Hook to load a specified workspace
 */
export const useLoadWorkspace = () =>
  // TODO: useRecoilTransaction when it's stable
  useRecoilCallback(
    ({ set }: CallbackInterface) => async (
      workspacePointer: WorkspacePointer
    ) => {
      const loadedWorkspace = await retrieveFileFromPointer(workspacePointer);
      if (
        loadedWorkspace !== null &&
        loadedWorkspace.type === "workspace_file"
      ) {
        const fileContents: FileContents = {};
        for (let [id, ptr] of Object.entries(
          loadedWorkspace.contents.openFiles
        )) {
          const retrieved = await retrieveFileFromPointer(ptr);
          if (retrieved !== null) {
            fileContents[id] = retrieved;
          } else {
            toast.error(`Failed to load file ${id}`);
          }
        }
        //   Empty layout to prevent NPE's
        set(layoutState, constructLayout([]));

        set(fileContentsState, fileContents);
        set(workspaceState, loadedWorkspace.contents);
        set(layoutState, Model.fromJson(loadedWorkspace.contents.layout));
      } else {
        toast.error(`Failed to load workspace ${workspacePointer.id}`);
      }
    }
  );

// TODO: if loading a substance or diagram that depends on other files, load those in first!!
export const useOpenFileInWorkspace = () =>
  useRecoilCallback(({ set, snapshot }) => async (pointer: FilePointer) => {
    const workspace: IWorkspace = snapshot.getLoadable(workspaceState).contents;
    const layout = snapshot.getLoadable(layoutState).contents;
    // If file already open in workspace, jump to it (focus it)
    if (pointer.id in workspace.openFiles) {
      layout.visitNodes((node: any) => {
        layout.visitNodes((node: any) => {
          if (
            node.getType() === "tab" &&
            (node as TabNode).getConfig() &&
            (node as TabNode).getConfig().id === pointer.id
          ) {
            layout.doAction(Actions.selectTab(node.getId()));
          }
        });
      });
    } else {
      const loadedFile = await retrieveFileFromPointer(pointer);
      if (loadedFile !== null) {
        set(fileContentsState, (state) => ({
          ...state,
          [pointer.id]: loadedFile,
        }));
        _editWorkspace(set, {
          ...workspace,
          openFiles: { ...workspace.openFiles, [pointer.id]: pointer },
        });
        if (!layout.getActiveTabset()) {
          layout.doAction(
            Actions.setActiveTabset(
              layout.getMaximizedTabset()?.getId() ?? "main"
            )
          );
        }
        layout.doAction(
          Actions.addNode(
            {
              type: "tab",
              component: "file",
              name: pointer.name,
              id: pointer.id,
              config: {
                id: pointer.id,
              },
            },
            // HACK: the fallback is fallible
            layout.getActiveTabset()?.getId() || "main",
            DockLocation.CENTER,
            -1,
            true
          )
        );
      } else {
        toast.error(`Failed to load file ${pointer.name}`);
      }
    }
  });

export const useNewFileCreatorTab = () =>
  useRecoilCallback(
    ({ snapshot }) => (node: TabSetNode) => {
      const layout: Model = snapshot.getLoadable(layoutState).contents;
      layout.doAction(
        Actions.addNode(
          {
            type: "tab",
            component: "new_tab",
            name: "New Tab",
            id: v4(),
            config: {
              id: v4(),
            },
          },
          node.getId(),
          DockLocation.CENTER,
          -1,
          true
        )
      );
    },
    [layoutState]
  );

export const useCloseWorkspaceFile = () =>
  useRecoilCallback(({ set, snapshot }) => (id: string) => {
    // TODO: check if file has dependents in the workspace, and prevent closing if so
    const workspace = snapshot.getLoadable(workspaceState).contents;
    _editWorkspace(set, {
      ...workspace,
      openFiles: deleteProperty(workspace.openFiles, id),
    });
    set(fileContentsState, (fileContents: FileContents) =>
      deleteProperty(fileContents, id)
    );
  });

export const useUpdateFile = () =>
  useRecoilCallback(
    ({ set, snapshot }) => async (id: string, contents: string) => {
      let pointer: FilePointer = snapshot.getLoadable(workspaceState).contents
        .openFiles[id];
      const file = snapshot.getLoadable(fileContentsState).contents[id];
      const updated = { ...file, contents };
      if (pointer.location.type !== "local") {
        // Makes it local!
        pointer = {
          ...pointer,
          name: `fork of ${pointer.name}`,
          location: {
            type: "local",
            localStorageKey: `${pointer.type}-${pointer.id}`,
          },
        };
        const workspace = snapshot.getLoadable(workspaceState).contents;
        _editWorkspace(set, {
          ...workspace,
          openFiles: { ...workspace.openFiles, [pointer.id]: pointer },
        });

        _addLocalFileToFileSystem(set, pointer);
      }
      set(fileContentsState, (state) => ({
        ...state,
        [file.id]: updated,
      }));
      await debouncedSave((pointer.location as ILocalLocation).localStorageKey)(
        updated
      );
    }
  );

const _autostepToConverge = async (prevDiagramFile: DiagramFile, set: any) => {
  const diagramFile = { ...prevDiagramFile };
  const stepResult = stepUntilConvergence(diagramFile.contents!);
  if (stepResult.isOk()) {
    const convergedState = stepResult.value;
    diagramFile.contents = convergedState;
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [diagramFile.id]: diagramFile,
    }));
  } else {
    diagramFile.metadata.error = stepResult.error;
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [diagramFile.id]: diagramFile,
    }));
  }
};

const _compileDiagram = async (
  oldDiagramFile: DiagramFile,
  substanceFile: SubstanceFile,
  styleFile: StyleFile,
  domainFile: DomainFile,
  set: any
) => {
  const diagramFile = { ...oldDiagramFile };
  const compiledDomain = compileDomain(domainFile.contents);
  if (compiledDomain.isOk()) {
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [domainFile.id]: {
        ...state[domainFile.id],
        cache: compiledDomain.value,
      },
    }));
  } else {
    diagramFile.metadata.error = compiledDomain.error;
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [diagramFile.id]: diagramFile,
    }));
    return;
  }
  const compileResult = compileTrio({
    domain: domainFile.contents,
    substance: substanceFile.contents,
    style: styleFile.contents,
    variation: diagramFile.metadata.variation,
  });
  if (compileResult.isOk()) {
    const initialState = await prepareState(compileResult.value);
    diagramFile.contents = initialState;
    diagramFile.metadata.error = null;
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [diagramFile.id]: diagramFile,
    }));
    if (diagramFile.metadata.autostep) {
      _autostepToConverge(diagramFile, set);
    }
  } else {
    diagramFile.metadata.error = compileResult.error;
    set(fileContentsState, (state: FileContents) => ({
      ...state,
      [diagramFile.id]: diagramFile,
    }));
  }
};

export const useUpdateNodeToNewDiagram = () =>
  useRecoilCallback(
    ({ set, snapshot }) => async (
      node: TabNode,
      trioSelection: TrioSelection,
      autostep: boolean
    ) => {
      const id = v4();
      const diagramPointer: DiagramFilePointer = {
        type: "diagram_state",
        id,
        substance: trioSelection.substance as SubstanceFilePointer,
        style: trioSelection.style as StyleFilePointer,
        domain: trioSelection.domain as DomainFilePointer,
        name: "Diagram",
        location: {
          type: "local",
          localStorageKey: `diagram-${id}`,
        },
      };
      const diagramFile: DiagramFile = {
        type: "diagram_file",
        id: diagramPointer.id,
        contents: null,
        metadata: {
          error: null,
          autostep: true,
          variation: v4(),
        },
      };
      await debouncedSave(diagramFile.id)(diagramFile);
      set(fileContentsState, (state) => ({
        ...state,
        [diagramFile.id]: diagramFile,
      }));
      const workspace = snapshot.getLoadable(workspaceState).contents;
      _editWorkspace(set, {
        ...workspace,
        openFiles: {
          ...workspace.openFiles,
          [diagramPointer.id]: diagramPointer,
        },
      });
      _addLocalFileToFileSystem(set, diagramPointer);
      const layout: Model = snapshot.getLoadable(layoutState).contents;
      layout.doAction(
        Actions.updateNodeAttributes(node.getId(), {
          component: "file",
          name: "Diagram",
          config: {
            id,
          },
        })
      );
      const fileContents = snapshot.getLoadable(fileContentsState).contents;
      const substanceFile = fileContents[diagramPointer.substance.id];
      const styleFile = fileContents[diagramPointer.style.id];
      const domainFile = fileContents[diagramPointer.domain.id];
      await _compileDiagram(
        diagramFile,
        substanceFile,
        styleFile,
        domainFile,
        set
      );
    }
  );

export const useRecompileDiagram = () =>
  useRecoilCallback(
    ({ set, snapshot }) => async (diagramPointer: DiagramFilePointer) => {
      const fileContents = snapshot.getLoadable(fileContentsState).contents;
      const diagramFile = fileContents[diagramPointer.id];
      const substanceFile = fileContents[diagramPointer.substance.id];
      const styleFile = fileContents[diagramPointer.style.id];
      const domainFile = fileContents[diagramPointer.domain.id];

      await _compileDiagram(
        diagramFile,
        substanceFile,
        styleFile,
        domainFile,
        set
      );
    }
  );

export const useResampleDiagram = () =>
  useRecoilCallback(
    ({ set, snapshot }) => async (diagramPointer: DiagramFilePointer) => {
      const fileContents = snapshot.getLoadable(fileContentsState).contents;
      const diagramFile = { ...fileContents[diagramPointer.id] } as DiagramFile;
      diagramFile.metadata.variation = v4();
      diagramFile.contents!.seeds = variationSeeds(
        diagramFile.metadata.variation
      ).seeds;
      diagramFile.contents = resample(diagramFile.contents!);
      _autostepToConverge(diagramFile, set);
    }
  );
