import { Style } from "@penrose/examples/dist/index.js";
import registry from "@penrose/examples/dist/registry.js";
import { deleteDoc, doc, getDoc, setDoc, updateDoc } from "firebase/firestore";
import { range } from "lodash";
import queryString from "query-string";
import toast from "react-hot-toast";
import { useRecoilCallback } from "recoil";
import { v4 as uuid } from "uuid";
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
import { LayoutStats, RenderState } from "../worker/message.js";
import {
  Canvas,
  Diagram,
  DiagramGrid,
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
  canvasState,
  currentRogerState,
  currentWorkspaceState,
  defaultWorkspaceState,
  diagramGridState,
  diagramMetadataSelector,
  diagramState,
  diagramWorkerState,
  fileContentsSelector,
  optimizer,
  savedFilesState,
  settingsState,
  workspaceMetadataSelector,
} from "./atoms.js";
import { generateVariation } from "./variation.js";

const _compileDiagram = async (
  substance: string,
  style: string,
  domain: string,
  variation: string,
  excludeWarnings: string[],
  set: any,
) => {
  const compiling = toast.loading("Compiling...");
  const onUpdate = (updatedState: RenderState, stats: LayoutStats) => {
    set(diagramState, (state: Diagram): Diagram => {
      return {
        ...state,
        error: null,
        // TODO: warnings
        // warnings: initialState.warnings,
        metadata: {
          ...state.metadata,
          variation,
          excludeWarnings,
          source: {
            domain,
            substance,
            style,
          },
        },
        state: updatedState,
      };
    });

    // TODO: update grid state too
    set(diagramGridState, ({ gridSize }: DiagramGrid) => ({
      variations: range(gridSize).map((i) =>
        i === 0 ? variation : generateVariation(),
      ),
      gridSize,
    }));
  };

  const id = optimizer.run({
    domain,
    style,
    substance,
    variation,
    onUpdate,
    onError: (error) => {
      toast.dismiss(compiling);
      set(diagramState, (state: Diagram) => ({ ...state, error }));
      set(diagramWorkerState, {
        ...diagramWorkerState,
        running: false,
      });
    },
    onComplete: () => {
      toast.dismiss(compiling);
      set(diagramWorkerState, {
        ...diagramWorkerState,
        running: false,
      });
    },
  });

  set(diagramWorkerState, {
    ...diagramWorkerState,
    id,
    running: true,
  });

  // TODO: update grid state too
  // set(diagramGridState, ({ gridSize }: DiagramGrid) => ({
  //   variations: range(gridSize).map((i) =>
  //     i === 0 ? variation : generateVariation(),
  //   ),
  //   gridSize,
  // }));
};

export const useCompileDiagram = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const domainFile = workspace.files.domain.contents;
    const substanceFile = workspace.files.substance.contents;
    const styleFile = workspace.files.style.contents;
    const diagram = snapshot.getLoadable(diagramState).contents as Diagram;

    await _compileDiagram(
      substanceFile,
      styleFile,
      domainFile,
      diagram.metadata.variation,
      diagram.metadata.excludeWarnings,
      set,
    );
  });

export const useIsUnsaved = () =>
  useRecoilCallback(({ snapshot, set }) => () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    return !isCleanWorkspace(workspace);
  });

export const useResampleDiagram = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram: Diagram = snapshot.getLoadable(diagramState)
      .contents as Diagram;
    const id: string = snapshot.getLoadable(diagramWorkerState)
      .contents as string;
    if (diagram.state === null) {
      toast.error("Cannot resample uncompiled diagram");
      return;
    }
    const variation = generateVariation();
    const resamplingLoading = toast.loading("Resampling...");
    optimizer.resample(
      id,
      variation,
      (resampled) => {
        set(diagramState, (state) => ({
          ...state,
          metadata: { ...state.metadata, variation },
          state: resampled,
        }));
        // update grid state too
        set(diagramGridState, ({ gridSize }) => ({
          variations: range(gridSize).map((i) =>
            i === 0 ? variation : generateVariation(),
          ),
          gridSize,
        }));
      },
      () => {
        toast.dismiss(resamplingLoading);
      },
    );
  });

// const _saveLocally = (set: any) => {
//   const id = toast.loading("saving...");
//   set(workspaceMetadataSelector, (state: WorkspaceMetadata) => ({
//     ...state,
//     location: { kind: "stored", saved: true } as WorkspaceLocation,
//   }));
//   toast.dismiss(id);
// };

// export const useSaveDiagram = () =>
//   useRecoilCallback(({ set }) => () => {
//     const notif = toast.loading("saving...")
//     set()
//     // _saveLocally(set);
//   });

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

export const useDuplicate = () =>
  useRecoilCallback(({ set }) => () => {
    set(workspaceMetadataSelector, (state: WorkspaceMetadata) => ({
      ...state,
      location: { kind: "stored", saved: true } as WorkspaceLocation,
      id: uuid(),
    }));
  });

// returns true if there are no unsaved changes
export const isCleanWorkspace = (workspace: Workspace): boolean => {
  if (
    workspace.metadata.location.kind === "stored" &&
    !workspace.metadata.location.saved
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

    let loadedWorkspace: Workspace | null;
    if (id in currentSavedFilesState) {
      loadedWorkspace = currentSavedFilesState[id];
    } else {
      // Workspace missing from local state, search database instead
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
            lastModified: new Date().toISOString(),
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
  });

export const useCheckURL = () =>
  useRecoilCallback(({ set, snapshot, reset }) => async () => {
    const parsed = queryString.parse(window.location.search);
    // if (
    //   "access_token" in parsed &&
    //   "profile[login]" in parsed &&
    //   "profile[avatar_url]" in parsed
    // ) {
    //   // Signed into GitHub
    //   set(settingsState, (state) => ({
    //     ...state,
    //     github: {
    //       username: parsed["profile[login]"],
    //       accessToken: parsed["access_token"],
    //       avatar: parsed["profile[avatar_url]"],
    //     } as LocalGithubUser,
    //   }));
    // }
    if ("gist" in parsed) {
      // Loading a gist
      const id = toast.loading("Loading gist...");
      const res = await fetch(
        `https://api.github.com/gists/${parsed["gist"]}`,
        {
          headers: {
            accept: "application/vnd.github.v3+json",
          },
        },
      );
      toast.dismiss(id);
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
          lastModified: new Date().toISOString(),
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

// this probably needs to be reworked
export const usePublishGist = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const settings = snapshot.getLoadable(settingsState).contents as Settings;
    if (settings.githubAccessToken === null) {
      // check auth token instead in local storage (in case user cleared cache)
      console.error(`Not authorized with GitHub`);
      toast.error(`Not authorized with GitHub`);
      return;
    }
    // save draft to a new workspace before redirecting to gist url
    if (
      workspace.metadata.location.kind === "stored" &&
      !workspace.metadata.location.saved
    ) {
      // call saveDiagram
      // console.log("bruh");
      // await _saveLocally(set);
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
    toast.success(`Published gist, redirecting...`);
    window.location.search = queryString.stringify({ gist: json.id });
  });

const REDIRECT_URL =
  process.env.NODE_ENV === "development"
    ? "https://penrose-gh-auth-zeta.vercel.app/connect/github"
    : "https://penrose-gh-auth.vercel.app/connect/github";
export const useSignIn = () =>
  useRecoilCallback(({ set, snapshot }) => () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState).contents;
    if (
      !isCleanWorkspace(workspace) &&
      !confirm("You have unsaved changes. Please save before continuing.")
    ) {
      return;
    }
    window.location.replace(REDIRECT_URL);
  });

export const useDeleteLocalFile = () =>
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

export const useSaveNewWorkspace = () =>
  useRecoilCallback(({ snapshot, set }) => async (diagramId: string) => {
    if (
      authObject.currentUser != null &&
      authObject.currentUser.uid != undefined
    ) {
      const currentWorkspace = snapshot.getLoadable(
        currentWorkspaceState,
      ).contents;

      const modificationTime = new Date().toISOString();
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
        .catch((error) => toast.error("Encountered an error"))
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
    const currentWorkspace = snapshot.getLoadable(
      currentWorkspaceState,
    ).contents;
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
        toast.error("Error saving workspace");
        return;
      }
      const notif = toast.loading("saving...");
      const modificationTime = new Date().toISOString();
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
        .catch((error) => toast.error("Encountered an error"))
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
