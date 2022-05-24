import {
  compileDomain,
  compileTrio,
  PenroseState,
  prepareState,
  resample,
  stateConverged,
  stepState,
  stepStateSafe,
  Trio,
  variationSeeds,
} from "@penrose/core";
import localforage from "localforage";
import queryString from "query-string";
import toast from "react-hot-toast";
import { useRecoilCallback } from "recoil";
import { v4 as uuid } from "uuid";
import {
  currentWorkspaceState,
  Diagram,
  diagramState,
  localFilesState,
  LocalGithubUser,
  Settings,
  settingsState,
  Workspace,
  WorkspaceLocation,
  WorkspaceMetadata,
  workspaceMetadataSelector,
} from "./atoms";
import { generateVariation } from "./variation";

const _compileDiagram = async (
  substance: string,
  style: string,
  domain: string,
  variation: string,
  autostep: boolean,
  stepSize: number,
  set: any
) => {
  const compiledDomain = compileDomain(domain);
  if (compiledDomain.isErr()) {
    set(diagramState, (state: Diagram) => ({
      ...state,
      error: compiledDomain.error,
    }));
    return;
  }
  const compileResult = compileTrio({
    domain,
    substance,
    style,
    variation,
  });
  if (compileResult.isErr()) {
    set(diagramState, (state: Diagram) => ({
      ...state,
      error: compileResult.error,
    }));
    return;
  }
  const initialState = resample(await prepareState(compileResult.value));
  set(
    diagramState,
    (state: Diagram): Diagram => ({
      ...state,
      error: null,
      metadata: { ...state.metadata, variation },
      state: initialState,
    })
  );
  _stepDiagram(autostep, stepSize, initialState, set);
};

export const _stepDiagram = async (
  autostep: boolean,
  stepSize: number,
  initialState: PenroseState,
  set: any
) => {
  if (autostep) {
    const steppingLoading = toast.loading("Stepping...");
    let currentState = initialState;
    while (!stateConverged(currentState)) {
      const stepResult = stepStateSafe(currentState, stepSize);
      if (stepResult.isErr()) {
        set(diagramState, (state: Diagram) => ({
          ...state,
          error: stepResult.error,
        }));
        return;
      } else {
        await new Promise((r) => setTimeout(r, 1));
        set(diagramState, (state: Diagram) => ({
          ...state,
          error: null,
          state: stepResult.value,
        }));
        currentState = stepResult.value;
      }
    }
    toast.dismiss(steppingLoading);
  }
};

export const useStepDiagram = () =>
  useRecoilCallback(({ set }) => () =>
    set(diagramState, (diagram: Diagram) => {
      if (diagram.state === null) {
        toast.error(`No diagram`);
        return diagram;
      }
      return {
        ...diagram,
        state: stepState(diagram.state, diagram.metadata.stepSize),
      };
    })
  );

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
      diagram.metadata.autostep,
      diagram.metadata.stepSize,
      set
    );
  });

export const useResampleDiagram = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram: Diagram = snapshot.getLoadable(diagramState)
      .contents as Diagram;
    if (diagram.state === null) {
      toast.error("Cannot resample uncompiled diagram");
      return;
    }
    const variation = generateVariation();
    const resamplingLoading = toast.loading("Resampling...");
    const seeds = variationSeeds(variation).seeds;
    const resampled = resample({ ...diagram.state, seeds });
    set(diagramState, (state) => ({
      ...state,
      metadata: { ...state.metadata, variation },
      state: resampled,
    }));
    toast.dismiss(resamplingLoading);
    const { autostep, stepSize } = diagram.metadata;
    _stepDiagram(autostep, stepSize, resampled, set);
  });

const _saveLocally = (set: any) => {
  console.info("saving locally...");
  set(workspaceMetadataSelector, (state: WorkspaceMetadata) => ({
    ...state,
    id: uuid(),
    location: { kind: "local", saved: true } as WorkspaceLocation,
  }));
};

export const useSaveLocally = () =>
  useRecoilCallback(({ set }) => () => {
    _saveLocally(set);
  });

const _confirmDirtyWorkspace = (workspace: Workspace): boolean => {
  if (
    workspace.metadata.location.kind === "local" &&
    !workspace.metadata.location.saved &&
    !(
      workspace.files.domain.contents === "" &&
      workspace.files.style.contents === "" &&
      workspace.files.substance.contents === ""
    )
  ) {
    return confirm("Your current workspace is unsaved. Overwrite it?");
  }
  return true;
};

export const useLoadLocalWorkspace = () =>
  useRecoilCallback(({ set, snapshot }) => async (id: string) => {
    const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    if (!_confirmDirtyWorkspace(currentWorkspace)) {
      return;
    }
    const loadedWorkspace = (await localforage.getItem(id)) as Workspace;
    if (loadedWorkspace === null) {
      console.error("Could not retrieve workspace", id);
      toast.error(`Could not retrieve workspace ${id}`);
      return;
    }
    set(currentWorkspaceState, loadedWorkspace as Workspace);
    await _compileDiagram(
      loadedWorkspace.files.substance.contents,
      loadedWorkspace.files.style.contents,
      loadedWorkspace.files.domain.contents,
      uuid(),
      true,
      10000, // COMBAK: figure out the right default
      set
    );
  });

export const useLoadExampleWorkspace = () =>
  useRecoilCallback(({ set, reset, snapshot }) => async (trio: Trio) => {
    const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
      .contents;
    if (!_confirmDirtyWorkspace(currentWorkspace)) {
      return;
    }
    const id = toast.loading("Loading example...");
    const domainReq = await fetch(trio.domainURI);
    const styleReq = await fetch(trio.styleURI);
    const substanceReq = await fetch(trio.substanceURI);
    toast.dismiss(id);
    const domain = await domainReq.text();
    const style = await styleReq.text();
    const substance = await substanceReq.text();
    set(currentWorkspaceState, {
      metadata: {
        id: uuid(),
        name: trio.name,
        lastModified: new Date().toISOString(),
        editorVersion: 0.1,
        location: {
          kind: "example",
        },
        forkedFromGist: null,
      },
      files: {
        domain: {
          contents: domain,
          name: `${trio.domainName}.dsl`,
        },
        style: {
          contents: style,
          name: `${trio.styleName}.sty`,
        },
        substance: {
          contents: substance,
          name: `${trio.substanceName}.sub`,
        },
      },
    });
    reset(diagramState);
    await _compileDiagram(
      substance,
      style,
      domain,
      trio.variation,
      true,
      10000, // COMBAK: figure out the right default
      set
    );
  });

export const useCheckURL = () =>
  useRecoilCallback(({ set }) => async () => {
    const parsed = queryString.parse(window.location.search);
    if (
      "access_token" in parsed &&
      "profile[login]" in parsed &&
      "profile[avatar_url]" in parsed
    ) {
      // Signed into GitHub
      set(settingsState, (state) => ({
        ...state,
        github: {
          username: parsed["profile[login]"],
          accessToken: parsed["access_token"],
          avatar: parsed["profile[avatar_url]"],
        } as LocalGithubUser,
      }));
    } else if ("gist" in parsed) {
      // Loading a gist
      const id = toast.loading("Loading gist...");
      const res = await fetch(
        `https://api.github.com/gists/${parsed["gist"]}`,
        {
          headers: {
            accept: "application/vnd.github.v3+json",
          },
        }
      );
      toast.dismiss(id);
      if (res.status !== 200) {
        console.error(res);
        toast.error(`Could not load gist: ${res.statusText}`);
        return;
      }
      const json = await res.json();
      const gistFiles = json.files;
      const gistMetadata = JSON.parse(gistFiles["metadata.json"].content);
      const metadata: WorkspaceMetadata = {
        name: gistMetadata.name,
        id: uuid(),
        lastModified: json.created_at,
        editorVersion: gistMetadata.editorVersion,
        forkedFromGist: null,
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
    }
  });

export const usePublishGist = () =>
  useRecoilCallback(({ snapshot }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const settings = snapshot.getLoadable(settingsState).contents as Settings;
    if (settings.github === null) {
      console.error(`Not authorized with GitHub`);
      toast.error(`Not authorized with GitHub`);
      return;
    }
    const res = await fetch("https://api.github.com/gists", {
      method: "POST",
      headers: {
        accept: "application/vnd.github.v3+json",
        Authorization: `token ${settings.github.accessToken}`,
      },
      body: JSON.stringify({
        description: workspace.metadata.name,
        files: {
          // https://stackoverflow.com/a/20949455/10833799
          ["_" + workspace.metadata.name]: {
            content: "a Penrose gist",
          },
          "metadata.json": {
            content: JSON.stringify({
              name: workspace.metadata.name,
              editorVersion: workspace.metadata.editorVersion,
              fileNames: {
                domain: workspace.files.domain.name,
                style: workspace.files.style.name,
                substance: workspace.files.substance.name,
              },
            }),
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
    if (!_confirmDirtyWorkspace(workspace)) {
      return;
    }
    window.location.replace(REDIRECT_URL);
  });

export const useDeleteLocalFile = () =>
  useRecoilCallback(
    ({ set, snapshot, reset }) => async (
      workspaceMetadata: WorkspaceMetadata
    ) => {
      const { id, name } = workspaceMetadata;
      const shouldDelete = confirm(`Delete ${name}?`);
      if (!shouldDelete) {
        return;
      }
      const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
        .contents;
      // removes from index
      set(localFilesState, (localFiles) => {
        const { [id]: removedFile, ...newFiles } = localFiles;
        return newFiles;
      });
      await localforage.removeItem(id);
      if (currentWorkspace.metadata.id === id) {
        reset(currentWorkspaceState);
      }
      toast.success(`Removed ${name}`);
    }
  );
