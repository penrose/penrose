import {
  compileDomain,
  compileTrio,
  prepareState,
  stepUntilConvergence,
} from "@penrose/core";
import localforage from "localforage";
import { useRecoilCallback } from "recoil";
import { currentWorkspaceState, diagramState, Workspace } from "./atoms";

export const useCompileDiagram = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const workspace = snapshot.getLoadable(currentWorkspaceState)
      .contents as Workspace;
    const domainFile = workspace.files.domain.contents;
    const substanceFile = workspace.files.substance.contents;
    const styleFile = workspace.files.style.contents;
    const diagram = snapshot.getLoadable(diagramState).contents;
    const compiledDomain = compileDomain(domainFile);
    if (compiledDomain.isErr()) {
      set(diagramState, (state) => ({ ...state, error: compiledDomain.error }));
      return;
    }
    const compileResult = compileTrio({
      domain: domainFile,
      substance: substanceFile,
      style: styleFile,
      variation: diagram.variation,
    });
    if (compileResult.isErr()) {
      set(diagramState, (state) => ({ ...state, error: compileResult.error }));
      return;
    }
    const initialState = await prepareState(compileResult.value);
    set(diagramState, (state) => ({
      ...state,
      error: null,
      state: initialState,
    }));
    if (diagram.autostep) {
      const stepResult = stepUntilConvergence(initialState);
      if (stepResult.isErr()) {
        set(diagramState, (state) => ({ ...state, error: stepResult.error }));
        return;
      }
      set(diagramState, (state) => ({
        ...state,
        error: null,
        state: stepResult.value,
      }));
    }
  });

export const useLoadLocalWorkspace = () =>
  useRecoilCallback(({ set }) => async (id: string) => {
    const workspace = await localforage.getItem(id);
    if (workspace === null) {
      console.error("Could not retrieve workspace", id);
      return;
    }
    set(currentWorkspaceState, workspace as Workspace);
  });
