import { runtimeError } from "@penrose/core";
import { Style } from "@penrose/examples/dist/index.js";
import registry from "@penrose/examples/dist/registry.js";
import { deleteDoc, doc, getDoc, setDoc, updateDoc } from "firebase/firestore";
import { debounce } from "lodash";
import queryString from "query-string";
import { useCallback, useEffect, useRef } from "react";
import toast from "react-hot-toast";
import {
  RecoilState,
  useRecoilCallback,
  useRecoilState,
  useRecoilValue,
} from "recoil";
import { v4 as uuid } from "uuid";
import { isErr, showOptimizerError } from "../optimizer/common.js";
import {
  DownloadPNG,
  DownloadSVG,
  pathResolver,
  zipTrio,
} from "../utils/downloadUtils.js";
import {
  authObject,
  createWorkspaceObject,
  db,
  getDiagram,
} from "../utils/firebaseUtils.js";
import { stateToSVG } from "../utils/renderUtils.js";
import {
  AutosaveTimer,
  Canvas,
  Diagram,
  DiagramMetadata,
  EDITOR_VERSION,
  GistMetadata,
  ProgramFile,
  RogerState,
  SavedWorkspaces,
  Settings,
  TrioWithPreview,
  Workspace,
  WorkspaceLocation,
  WorkspaceMetadata,
  autosaveTimerState,
  canvasState,
  codemirrorHistory,
  currentRogerState,
  currentWorkspaceState,
  defaultWorkspaceState,
  diagramMetadataSelector,
  diagramState,
  diagramWorkerState,
  fileContentsSelector,
  optimizer,
  savedFilesState,
  settingsState,
  showCompileErrsState,
  workspaceMetadataSelector,
} from "./atoms.js";
import { generateVariation } from "./variation.js";

const _compileDiagram = async (
  substance: string,
  style: string,
  domain: string,
  variation: string,
  excludeWarnings: string[],
  set: <T>(state: RecoilState<T>, update: (t: T) => T) => void,
) => {
  // indicate that buttons should gray out for now
  set(diagramWorkerState, (state) => ({
    optimizing: false,
    resampling: false,
    compiling: true,
  }));

  const compiling = toast.loading("Compiling...");
  const compileResult = await optimizer.compile(
    domain,
    style,
    substance,
    variation,
  );
  toast.dismiss(compiling);

  // un-gray buttons
  set(diagramWorkerState, () => ({
    optimizing: false,
    resampling: false,
    compiling: false,
  }));

  if (isErr(compileResult)) {
    // display error
    set(diagramState, (diagram) => ({
      ...diagram,
      error: compileResult.error,
    }));
    return;
  }

  // get currently available step sequence id and history (should be exactly one
  // id, and no history, currently)
  const pollResult = await optimizer.poll(compileResult.value.diagramId);
  if (isErr(pollResult)) {
    set(diagramState, (diagram) => ({
      ...diagram,
      error: runtimeError(showOptimizerError(pollResult.error)),
    }));
    return;
  }

  // we've succesfully compiled, so we can update the diagram metadata, warnings, etc
  set(diagramState, (diagram) => ({
    ...diagram,
    warnings: compileResult.value.warnings,
    error: null,
    historyInfo: pollResult.value,
    diagramId: compileResult.value.diagramId,
    // uses our assumption that there will only be one step sequence for a newly
    // compiled diagram
    historyLoc: {
      sequenceId: pollResult.value.keys().next().value,
      frame: 0,
    },
    metadata: {
      ...diagram.metadata,
      variation,
      excludeWarnings,
      source: {
        substance,
        style,
        domain,
      },
    },
  }));

  set(diagramWorkerState, () => ({
    compiling: false,
    resampling: false,
    optimizing: true,
  }));
};

export const useCompileDiagram = () => {
  const workspace = useRecoilValue(currentWorkspaceState);
  const metadata = useRecoilValue(diagramMetadataSelector);
  return useRecoilCallback(
    ({ snapshot, set }) =>
      async () => {
        set(showCompileErrsState, true);
        const domainFile = workspace.files.domain.contents;
        const substanceFile = workspace.files.substance.contents;
        const styleFile = workspace.files.style.contents;
        await _compileDiagram(
          substanceFile,
          styleFile,
          domainFile,
          metadata.variation,
          metadata.excludeWarnings,
          set,
        );
      },
    [workspace, metadata],
  );
};

export const useIsUnsaved = () =>
  useRecoilCallback(({ snapshot, set }) => () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    return !isCleanWorkspace(workspace);
  });

export const useClearAutosave = (set: any, snapshot: any) => {
  const autosaveTimer: AutosaveTimer = snapshot.getLoadable(autosaveTimerState)
    .contents as AutosaveTimer;

  if (autosaveTimer != null) {
    clearTimeout(autosaveTimer);
    set(autosaveTimerState, null);
  }
};
/*
 * See: https://github.com/uiwjs/react-codemirror/issues/405
 * Summary: Utilizing React Codemirror provides useful abstractions that
 * would be annoying to implement manually (namely onChange hook)
 * Docs recommend clearing history by resetting State, but this would
 * remove the React Codemirror state. This is a hacky workaround
 */
export const useResetEditorHistory = (set: any) => {
  set(codemirrorHistory, false);
  setTimeout(() => set(codemirrorHistory, true), 1);
};

export const useResampleDiagram = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram: Diagram = snapshot.getLoadable(diagramState)
      .contents as Diagram;
    if (diagram.diagramId === null) {
      toast.error("Cannot resample uncompiled diagram");
      return;
    }

    const variation = generateVariation();
    const resamplingLoading = toast.loading("Resampling...");

    const resampleResult = await optimizer.resample(
      diagram.diagramId,
      variation,
    );
    toast.dismiss(resamplingLoading);

    if (isErr(resampleResult)) {
      set(diagramState, (diagram) => ({
        ...diagram,
        error: runtimeError(showOptimizerError(resampleResult.error)),
      }));
      return;
    }

    // resampling succeeded, so we know we're now optimizing
    set(diagramWorkerState, () => ({
      compiling: false,
      optimizing: true,
      resampling: false,
    }));
    set(diagramState, (diagram) => ({
      ...diagram,
      historyLoc: {
        sequenceId: resampleResult.value,
        frame: 0,
      },
      // on resample, only clear runtime errors
      error:
        diagram.error !== null && diagram.error.errorType !== "RuntimeError"
          ? diagram.error
          : null,
      metadata: {
        ...diagram.metadata,
        variation,
      },
    }));
  });

export const useDownloadTrio = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const metadata = snapshot.getLoadable(workspaceMetadataSelector)
      .contents as WorkspaceMetadata;
    const fileTitle = metadata.name.replaceAll(" ", "_").toLowerCase();
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const dsl = workspace.files.domain;
    const sub = workspace.files.substance;
    const sty = workspace.files.style;

    // save trio
    if (
      dsl.contents.length > 0 &&
      sub.contents.length > 0 &&
      sty.contents.length > 0
    ) {
      zipTrio([dsl, sub, sty], fileTitle);
    } else {
      toast.error(
        "Could not export: no Penrose diagram detected. Compile a Penrose trio and try again.",
      );
    }
  });

export const useCopyToClipboard = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const sub = workspace.files.substance.contents;
    const sty = workspace.files.style.contents;
    const dsl = workspace.files.domain.contents;
    const concatenated = `-- .substance\n${sub}\n-- .style\n${sty}\n-- .domain\n${dsl}\n`;

    navigator.clipboard
      .writeText(concatenated)
      .then(() => {
        toast.success("Copied trio to clipboard!");
      })
      .catch(() => {
        toast.error("Error could not copy");
      });
  });

export const useDownloadSvg = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram = snapshot.getLoadable(diagramMetadataSelector)
      .contents as DiagramMetadata;
    const canvas = snapshot.getLoadable(canvasState).contents as Canvas;
    if (canvas.ref && canvas.ref.current !== null) {
      const svg = canvas.ref.current.firstElementChild as SVGSVGElement;
      if (svg !== null) {
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        const domain = snapshot.getLoadable(fileContentsSelector("domain"))
          .contents as ProgramFile;
        const substance = snapshot.getLoadable(
          fileContentsSelector("substance"),
        ).contents as ProgramFile;
        const style = snapshot.getLoadable(fileContentsSelector("style"))
          .contents as ProgramFile;
        DownloadSVG(
          svg,
          metadata.name,
          domain.contents,
          substance.contents,
          style.contents,
          metadata.editorVersion.toString(),
          diagram.variation,
        );
      } else {
        toast.error(
          "Could not export: no Penrose diagram detected. Compile a Penrose trio and try again.",
        );
      }
    }
  });

// download an svg with raw TeX labels
export const useDownloadSvgTex = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram = snapshot.getLoadable(diagramMetadataSelector)
      .contents as DiagramMetadata;
    const metadata = snapshot.getLoadable(workspaceMetadataSelector)
      .contents as WorkspaceMetadata;
    const rogerState = snapshot.getLoadable(currentRogerState)
      .contents as RogerState;
    const canvas = snapshot.getLoadable(canvasState).contents as Canvas;
    if (canvas.ref && canvas.ref.current !== null) {
      const { state } = snapshot.getLoadable(diagramState).contents as Diagram;
      if (state !== null) {
        const rendered = await stateToSVG(state, {
          pathResolver: (path: string) =>
            pathResolver(path, rogerState, metadata),
          width: state.canvas.width.toString(),
          height: state.canvas.height.toString(),
          texLabels: true,
        });
        const domain = snapshot.getLoadable(fileContentsSelector("domain"))
          .contents as ProgramFile;
        const substance = snapshot.getLoadable(
          fileContentsSelector("substance"),
        ).contents as ProgramFile;
        const style = snapshot.getLoadable(fileContentsSelector("style"))
          .contents as ProgramFile;
        DownloadSVG(
          rendered,
          metadata.name,
          domain.contents,
          substance.contents,
          style.contents,
          metadata.editorVersion.toString(),
          diagram.variation,
        );
      } else {
        toast.error(
          "Could not export: no Penrose diagram detected. Compile a Penrose trio and try again.",
        );
      }
    }
  });

export const useDownloadPng = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram = snapshot.getLoadable(diagramState).contents as Diagram;
    const canvas = snapshot.getLoadable(canvasState).contents as Canvas;
    if (canvas.ref && canvas.ref.current !== null) {
      const svg = canvas.ref.current.firstElementChild as SVGSVGElement;
      if (svg !== null) {
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        const filename = `${metadata.name}.png`;
        if (diagram.state) {
          const { canvas: canvasDims } = diagram.state;
          const { width, height } = canvasDims;
          DownloadPNG(svg, filename, width, height, 1);
        }
      } else {
        toast.error(
          "Could not export: no Penrose diagram detected. Compile a Penrose trio and try again.",
        );
      }
    }
  });

export const useDownloadPdf = () =>
  useRecoilCallback(({ snapshot }) => async () => {
    const diagram = snapshot.getLoadable(diagramState).contents as Diagram;
    const { state } = diagram;
    const canvas = snapshot.getLoadable(canvasState).contents as Canvas;
    if (canvas.ref && canvas.ref.current !== null) {
      const svg = canvas.ref.current.firstElementChild as SVGSVGElement;
      if (svg !== null && state) {
        // clear all of the <penrose> metadata if it is present
        // this metadata is added to SVGs for saving/loading but it will break PDF
        const metadataQuery = svg.querySelector("penrose");
        if (metadataQuery !== null) {
          metadataQuery.innerHTML = "";
        }
        const metadata = snapshot.getLoadable(workspaceMetadataSelector)
          .contents as WorkspaceMetadata;
        const openedWindow = window.open(
          "",
          "PRINT",
          `height=${state.canvas.height},width=${state.canvas.width}`,
        );
        if (openedWindow === null) {
          toast.error("Couldn't open popup to print");
          return;
        }
        openedWindow.document.write(
          `<!DOCTYPE html><head><title>${metadata.name}</title></head><body>`,
        );
        openedWindow.document.write(svg.outerHTML);
        openedWindow.document.write("</body></html>");
        openedWindow.document.close();
        openedWindow.focus();
        openedWindow.print();
      } else {
        toast.error(
          "Could not export: no Penrose diagram detected. Compile a Penrose trio and try again.",
        );
      }
    }
  });

// returns true if there are no unsaved changes
export const isCleanWorkspace = (workspace: Workspace): boolean => {
  if (
    (workspace.metadata.location.kind === "stored" &&
      !workspace.metadata.location.saved) ||
    (workspace.metadata.location.kind == "local" &&
      workspace.metadata.location.changesMade)
  ) {
    return false;
  } else {
    return true;
  }
};

export const useLoadLocalWorkspace = () =>
  useRecoilCallback(({ set, snapshot }) => async (id: string) => {
    const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const currentSavedFilesState = snapshot.getLoadable(savedFilesState)
      .contents as SavedWorkspaces;
    if (
      !isCleanWorkspace(currentWorkspace) &&
      !confirm(
        "You have unsaved changes. Are you sure you want to load a new workspace?",
      )
    ) {
      return;
    }

    // Clear autosave to avoid race
    useClearAutosave(set, snapshot);

    let loadedWorkspace: Workspace | null;
    if (id in currentSavedFilesState) {
      loadedWorkspace = currentSavedFilesState[id];
    } else {
      /**
       * Workspace missing from local state, search database instead.
       * A fallback in case of some unforseen bug, I do not expect this
       * branch to be reached
       */
      loadedWorkspace = await getDiagram(id);
    }

    if (loadedWorkspace != null) {
      set(currentWorkspaceState, loadedWorkspace as Workspace);
      await _compileDiagram(
        loadedWorkspace.files.substance.contents,
        loadedWorkspace.files.style.contents,
        loadedWorkspace.files.domain.contents,
        uuid(),
        [],
        set,
      );
      useResetEditorHistory(set);
    }
  });

export const useLoadExampleWorkspace = () =>
  useRecoilCallback(
    ({ set, reset, snapshot }) =>
      async (meta: TrioWithPreview) => {
        const currentWorkspace = snapshot.getLoadable(
          currentWorkspaceState,
        ).contents;
        if (
          !isCleanWorkspace(currentWorkspace) &&
          !confirm(
            "You have unsaved changes. Are you sure you want to load a gallery example?",
          )
        ) {
          return;
        }

        // Clear autosave to avoid race
        useClearAutosave(set, snapshot);

        const id = toast.loading("Loading example...");
        const { domain, style, substance, variation, excludeWarnings } =
          await meta.get();
        toast.dismiss(id);
        const styleJoined = style
          .map(({ contents }: Style) => contents)
          .join("\n");
        // HACK: we should really use each Style's individual `resolver`
        const { resolver } = style[0];
        set(currentWorkspaceState, {
          metadata: {
            id: uuid(),
            name: meta.name!,
            lastModified: Date.parse(new Date().toISOString()),
            editorVersion: EDITOR_VERSION,
            location: {
              kind: "example",
              resolver,
            },
            forkedFromGist: null,
          },
          files: {
            domain: {
              contents: domain,
              name: `.domain`,
            },
            style: {
              contents: styleJoined,
              name: `.style`,
            },
            substance: {
              contents: substance,
              name: `.substance`,
            },
          },
        });
        reset(diagramState);
        await _compileDiagram(
          substance,
          styleJoined,
          domain,
          variation,
          excludeWarnings,
          set,
        );

        useResetEditorHistory(set);
      },
  );

export const useNewWorkspace = () =>
  useRecoilCallback(({ reset, set, snapshot }) => () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState).contents;
    if (
      !isCleanWorkspace(workspace) &&
      !confirm(`You have unsaved changes. Are you sure you want to create
      a new workspace?`)
    ) {
      return;
    }
    // set rather than reset to generate new id to avoid id conflicts
    set(currentWorkspaceState, () => defaultWorkspaceState());
    reset(diagramState);
    useResetEditorHistory(set);
  });

export const useCheckURL = () =>
  useRecoilCallback(({ set, snapshot, reset }) => async () => {
    const parsed = queryString.parse(window.location.search);
    if ("gist" in parsed) {
      // Loading a gist
      // Show loading notification only if not redirected from share
      var id!: string;
      if (!("pub" in parsed)) {
        id = toast.loading("Loading gist...");
      }
      const res = await fetch(
        `https://api.github.com/gists/${parsed["gist"]}`,
        {
          headers: {
            accept: "application/vnd.github.v3+json",
          },
        },
      );
      if (!("pub" in parsed)) {
        toast.dismiss(id);
      }
      if (res.status !== 200) {
        console.error(res);
        toast.error(`Could not load gist: ${res.statusText}`);
        return;
      }
      const json = await res.json();
      const gistFiles = json.files;
      const gistMetadata = JSON.parse(
        gistFiles["metadata.json"].content,
      ) as GistMetadata;
      const metadata: WorkspaceMetadata = {
        name: gistMetadata.name,
        id: uuid(),
        lastModified: json.created_at,
        editorVersion: gistMetadata.editorVersion,
        forkedFromGist: gistMetadata.forkedFromGist,
        location: {
          kind: "gist",
          id: json.id,
          author: json.owner.login,
          avatar: json.owner.avatar_url,
        },
      };
      const files = {
        domain: {
          contents: gistFiles["domain"].content,
          name: gistMetadata.fileNames.domain,
        },
        style: {
          contents: gistFiles["style"].content,
          name: gistMetadata.fileNames.style,
        },
        substance: {
          contents: gistFiles["substance"].content,
          name: gistMetadata.fileNames.substance,
        },
      };
      const workspace: Workspace = {
        metadata,
        files,
      };
      set(currentWorkspaceState, workspace);

      // Notification + save to clipboard if redirected from clicking share
      if ("pub" in parsed) {
        const gistParameter = queryString.stringify({ gist: parsed["gist"] });
        const shareableURL = `${window.location.origin}${window.location.pathname}?${gistParameter}`;
        navigator.clipboard.writeText(shareableURL).then(() => {
          toast.success("Copied shareable link to clipboard");
        });
        // Hide pub query parameter from displayed URL
        window.history.replaceState({}, document.title, shareableURL);
      }
    } else if ("examples" in parsed) {
      const t = toast.loading("Loading example...");
      const id = parsed["examples"];
      if (typeof id !== "string") return;
      const ex = registry.get(id);
      if (ex === undefined || !ex.trio) return;
      const { domain, style, substance, variation, excludeWarnings } =
        await ex.get();
      toast.dismiss(t);
      const styleJoined = style.map(({ contents }: any) => contents).join("\n");
      // HACK: we should really use each Style's individual `resolver`
      const { resolver } = style[0];
      set(currentWorkspaceState, {
        metadata: {
          id: uuid(),
          name: ex.name!,
          lastModified: Date.parse(new Date().toISOString()),
          editorVersion: EDITOR_VERSION,
          location: {
            kind: "example",
            resolver,
          },
          forkedFromGist: null,
        },
        files: {
          domain: {
            contents: domain,
            name: `.domain`,
          },
          style: {
            contents: styleJoined,
            name: `.style`,
          },
          substance: {
            contents: substance,
            name: `.substance`,
          },
        },
      });
      reset(diagramState);
      await _compileDiagram(
        substance,
        styleJoined,
        domain,
        variation,
        excludeWarnings,
        set,
      );
      toast.dismiss(t);
    }
    // TODO: implementing loading individual registry examples by URL
  });

export const usePublishGist = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const settings = snapshot.getLoadable(settingsState).contents as Settings;
    if (settings.githubAccessToken === null) {
      console.error(`Not authorized with GitHub`);
      toast.error(`Not authorized with GitHub`);
      return;
    }
    const gistMetadata: GistMetadata = {
      name: workspace.metadata.name,
      editorVersion: workspace.metadata.editorVersion,
      forkedFromGist: workspace.metadata.forkedFromGist,
      fileNames: {
        domain: workspace.files.domain.name,
        style: workspace.files.style.name,
        substance: workspace.files.substance.name,
      },
    };
    const res = await fetch("https://api.github.com/gists", {
      method: "POST",
      headers: {
        accept: "application/vnd.github.v3+json",
        Authorization: `token ${settings.githubAccessToken}`,
      },
      body: JSON.stringify({
        description: workspace.metadata.name,
        files: {
          // https://stackoverflow.com/a/20949455/10833799
          ["_" + workspace.metadata.name]: {
            content: "a Penrose gist",
          },
          "metadata.json": {
            content: JSON.stringify(gistMetadata),
          },
          domain: {
            content: workspace.files.domain.contents,
          },
          style: {
            content: workspace.files.style.contents,
          },
          substance: {
            content: workspace.files.substance.contents,
          },
        },
        public: false,
      }),
    });
    const json = await res.json();
    if (res.status !== 201) {
      console.error(`Could not publish gist: ${res.statusText}`);
      toast.error(`Could not publish gist: ${res.statusText} ${json.message}`);
      return;
    }
    // Use query string (pub) to pass state to display notification on next page
    const gistParameter = queryString.stringify({ gist: json.id, pub: true });
    toast.success("Redirecting to gist...");
    window.location.search = gistParameter;
  });

export const useDeleteWorkspace = () =>
  useRecoilCallback(
    ({ set, snapshot, reset }) =>
      async (workspaceMetadata: WorkspaceMetadata) => {
        const { id, name } = workspaceMetadata;
        const shouldDelete = confirm(`Delete ${name}?`);
        if (!shouldDelete) {
          return;
        }

        // Delete from cloud storage
        if (authObject.currentUser != null) {
          const notif = toast.loading(`Deleting ${name}...`);
          await deleteDoc(doc(db, authObject.currentUser.uid, id))
            .catch((error) => {
              toast.error(`Error deleting diagram: ${name}`);
              console.log(
                `Error code: ${error.code}, Error message: ${error.message}`,
              );
              toast.dismiss(notif);
              return;
            })
            .then(() => {
              // If successful, remove from local state
              const currentWorkspace = snapshot.getLoadable(
                currentWorkspaceState,
              ).contents;
              set(savedFilesState, (savedFiles) => {
                const { [id]: removedFile, ...newFiles } = savedFiles;
                return newFiles;
              });

              // Clear autosave to avoid race
              useClearAutosave(set, snapshot);

              if (currentWorkspace.metadata.id === id) {
                // set rather than reset to generate new id to avoid id conflicts
                set(currentWorkspaceState, () => defaultWorkspaceState());
                reset(diagramState);
              }
              toast.dismiss(notif);
              toast.success(`Deleted ${name}`);
            });
        }
      },
  );

/**
 * Used when a "local" workspace is saved for the first time
 * Also used for duplicate diagram, diagramId passed in as fresh uuid
 */
export const useSaveNewWorkspace = () =>
  useRecoilCallback(({ snapshot, set }) => async (diagramId: string) => {
    if (
      authObject.currentUser != null &&
      authObject.currentUser.uid != undefined
    ) {
      const currentWorkspace = snapshot.getLoadable(
        currentWorkspaceState,
      ).contents;

      const modificationTime = Date.parse(new Date().toISOString());
      // Save to cloud
      const notif = toast.loading("saving...");
      await setDoc(doc(db, authObject.currentUser.uid, diagramId), {
        diagramId: diagramId,
        name: currentWorkspace.metadata.name,
        lastModified: modificationTime,
        editorVersion: currentWorkspace.metadata.editorVersion,
        substance: currentWorkspace.files.substance.contents,
        style: currentWorkspace.files.style.contents,
        domain: currentWorkspace.files.domain.contents,
      })
        .catch((error) => {
          console.log(
            `Error code: ${error.code}, Error message: ${error.message}`,
          );
          toast.error("Encountered an error");
        })
        // Update local state
        .then(() => {
          set(savedFilesState, (prevState) => ({
            ...prevState,
            [diagramId]: createWorkspaceObject(
              currentWorkspace.metadata.name,
              modificationTime,
              diagramId,
              currentWorkspace.metadata.editorVersion,
              true,
              currentWorkspace.files.substance.contents,
              currentWorkspace.files.style.contents,
              currentWorkspace.files.domain.contents,
            ),
          }));
          set(currentWorkspaceState, (prevState) => ({
            ...prevState,
            metadata: {
              ...prevState.metadata,
              id: diagramId,
              lastModified: modificationTime,
              location: { kind: "stored", saved: true } as WorkspaceLocation,
            },
          }));
          toast.dismiss(notif);
        });
    } else {
      toast.error("Could not save workspace, please check login credentials");
    }
  });

export const useSaveWorkspace = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const currentWorkspace: Workspace = snapshot.getLoadable(
      currentWorkspaceState,
    ).contents as Workspace;

    /**
     * Check exists because of autosave. In autosave, a metadata update race
     * makes it so that checking if the workspace is not saved before calling
     * this function infeasible. As such, we move the check into this
     * function.
     */
    if (
      currentWorkspace.metadata.location.kind == "stored" &&
      currentWorkspace.metadata.location.saved
    ) {
      return;
    }

    if (
      authObject.currentUser != null &&
      authObject.currentUser.uid != undefined
    ) {
      // Check for overwriting conflicts by checking lastModified string
      const storedDiagram = await getDoc(
        doc(db, authObject.currentUser.uid, currentWorkspace.metadata.id),
      );
      if (storedDiagram.exists()) {
        const storedDiagramData = storedDiagram.data();
        if (
          storedDiagramData.lastModified !=
            currentWorkspace.metadata.lastModified &&
          !confirm(
            `Merge conflict detected. Are you sure you want to override saved 
            changes?`,
          )
        ) {
          return;
        }
      } else {
        toast.error("Error saving workspace, this workspace does not exist");
        return;
      }
      const notif = toast.loading("saving...");
      const modificationTime = Date.parse(new Date().toISOString());
      // Save to cloud
      await updateDoc(
        doc(db, authObject.currentUser.uid, currentWorkspace.metadata.id),
        {
          name: currentWorkspace.metadata.name,
          lastModified: modificationTime,
          editorVersion: currentWorkspace.metadata.editorVersion,
          substance: currentWorkspace.files.substance.contents,
          style: currentWorkspace.files.style.contents,
          domain: currentWorkspace.files.domain.contents,
        },
      )
        .catch((error) => {
          console.log(
            `Error code: ${error.code}, Error message: ${error.message}`,
          );
          toast.error("Encountered an error");
        })
        // Update local state
        .then(() => {
          set(savedFilesState, (prevState) => ({
            ...prevState,
            [currentWorkspace.metadata.id]: createWorkspaceObject(
              currentWorkspace.metadata.name,
              modificationTime,
              currentWorkspace.metadata.id,
              currentWorkspace.metadata.editorVersion,
              true,
              currentWorkspace.files.substance.contents,
              currentWorkspace.files.style.contents,
              currentWorkspace.files.domain.contents,
            ),
          }));
          set(currentWorkspaceState, (prevState) => ({
            ...prevState,
            metadata: {
              ...prevState.metadata,
              lastModified: modificationTime,
              location: { kind: "stored", saved: true } as WorkspaceLocation,
            },
          }));
          toast.dismiss(notif);
        });
    } else {
      toast.error("Could not save workspace, please check login credentials");
    }
  });

/**
 * Allows user to save workspace with ctrl+s or cmd+s
 * If user in an "local" workspace (new workspace): Adds to saved workspaces
 * If user in a "stored" workspace that's unsaved: Saves
 * If user in an example, gist, or roger workspace: Does nothing
 */
export const saveShortcutHook = () => {
  const saveWorkspace = useSaveWorkspace();
  const saveNewWorkspace = useSaveNewWorkspace();

  // Need useRecoilCallback for snapshot, otherwise currentWorkspace outdated
  const handleShortcut = useRecoilCallback(
    ({ snapshot }) =>
      async (event: KeyboardEvent) => {
        const currentWorkspace = snapshot.getLoadable(
          currentWorkspaceState,
        ).contents;
        if (event.repeat) return;
        // Cmd+s or Ctrl+s
        if ((event.metaKey || event.ctrlKey) && event.key === "s") {
          if (
            currentWorkspace.metadata.location.kind == "stored" &&
            !currentWorkspace.metadata.location.saved
          ) {
            event.preventDefault();
            saveWorkspace();
          } else if (currentWorkspace.metadata.location.kind == "local") {
            event.preventDefault();
            saveNewWorkspace(uuid());
          }
        }
      },
  );

  useEffect(() => {
    document.addEventListener("keydown", handleShortcut, {
      passive: false,
    });

    // Cleanup
    return () => document.removeEventListener("keydown", handleShortcut);
  }, []);
};

/**
 * Autosaves every 4 seconds after a user has finished editing.
 */
export const autosaveHook = () => {
  const currentWorkspace = useRecoilValue(currentWorkspaceState);
  const isInitialRender = useRef(true);
  const saveWorkspace = useSaveWorkspace();
  const [autosaveTimerValue, autosaveTimerSetter] =
    useRecoilState(autosaveTimerState);

  /**
   * useCallback necessary for debounce to work. Without debounce, every
   * character entered will trigger the function.
   */
  const autosaveLogic = useCallback(
    debounce(async () => {
      // Reset autosave timer
      if (autosaveTimerValue != null) {
        clearTimeout(autosaveTimerValue);
      }
      // Set new timer, after 5 seconds have elapsed without edit
      const newTimeoutId = setTimeout(() => {
        if (currentWorkspace.metadata.location.kind == "stored") {
          saveWorkspace();
        }
      }, 4000);
      autosaveTimerSetter(newTimeoutId);
    }, 500),
    // So that updates to these values won't be reflected in execution
    [autosaveTimerValue, currentWorkspace.metadata],
  );

  useEffect(() => {
    if (isInitialRender.current) {
      isInitialRender.current = false;
      return;
    }
    autosaveLogic();
  }, [
    currentWorkspace.files.substance.contents,
    currentWorkspace.files.style.contents,
    currentWorkspace.files.domain.contents,
  ]);
};
