import {
  compileDomain,
  compileTrio,
  prepareState,
  stepUntilConvergence,
  Trio,
} from "@penrose/core";
import localforage from "localforage";
import { useRecoilCallback } from "recoil";
import { v4 as uuid } from "uuid";
import {
  currentWorkspaceState,
  Diagram,
  diagramState,
  Workspace,
  WorkspaceLocation,
  WorkspaceMetadata,
  workspaceMetadataSelector,
} from "./atoms";

const _compileDiagram = async (
  substance: string,
  style: string,
  domain: string,
  variation: string,
  autostep: boolean,
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
  const initialState = await prepareState(compileResult.value);
  set(diagramState, (state: Diagram) => ({
    ...state,
    error: null,
    state: initialState,
  }));
  if (autostep) {
    const stepResult = stepUntilConvergence(initialState);
    if (stepResult.isErr()) {
      set(diagramState, (state: Diagram) => ({
        ...state,
        error: stepResult.error,
      }));
      return;
    }
    set(diagramState, (state: Diagram) => ({
      ...state,
      error: null,
      state: stepResult.value,
    }));
  }
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
      diagram.metadata.autostep,
      set
    );
  });

const _saveLocally = (set: any) => {
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

const _confirmDirtyWorkspace = (workspace: Workspace, set: any) => {
  if (
    workspace.metadata.location.kind === "local" &&
    !workspace.metadata.location.saved &&
    !(
      workspace.files.domain.contents === "" &&
      workspace.files.style.contents === "" &&
      workspace.files.substance.contents === ""
    )
  ) {
    const confirmation = confirm("Your current workspace is unsaved. Save it?");
    if (confirmation) {
      _saveLocally(set);
      // TODO: does not complete save side effects
    }
  }
};

export const useLoadLocalWorkspace = () =>
  useRecoilCallback(({ set, reset, snapshot }) => async (id: string) => {
    const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    _confirmDirtyWorkspace(currentWorkspace, set);
    const loadedWorkspace = (await localforage.getItem(id)) as Workspace;
    if (loadedWorkspace === null) {
      console.error("Could not retrieve workspace", id);
      return;
    }
    set(currentWorkspaceState, loadedWorkspace as Workspace);
    await _compileDiagram(
      loadedWorkspace.files.substance.contents,
      loadedWorkspace.files.style.contents,
      loadedWorkspace.files.domain.contents,
      uuid(),
      true,
      set
    );
  });

export const useLoadExampleWorkspace = () =>
  useRecoilCallback(({ set, snapshot }) => async (trio: Trio) => {
    const currentWorkspace = snapshot.getLoadable(currentWorkspaceState)
      .contents;
    _confirmDirtyWorkspace(currentWorkspace, set);
    const domainReq = await fetch(trio.domainURI);
    const styleReq = await fetch(trio.styleURI);
    const substanceReq = await fetch(trio.substanceURI);
    const domain = await domainReq.text();
    const style = await styleReq.text();
    const substance = await substanceReq.text();
    set(currentWorkspaceState, {
      metadata: {
        id: uuid(),
        name: trio.name,
        lastModified: new Date().toISOString(),
        editor_version: 0.1,
        location: {
          kind: "example",
        },
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
    await _compileDiagram(substance, style, domain, trio.variation, true, set);
  });
