import { compileDomain, compileTrio, prepareState } from "@penrose/core";
import { useRecoilCallback } from "recoil";
import {
  diagramState,
  domainFileState,
  styleFileState,
  substanceFileState,
} from "./atoms";

export const useCompileDiagram = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const domainFile = snapshot.getLoadable(domainFileState).contents;
    const substanceFile = snapshot.getLoadable(substanceFileState).contents;
    const styleFile = snapshot.getLoadable(styleFileState).contents;
    const diagram = snapshot.getLoadable(diagramState).contents;
    const compiledDomain = compileDomain(domainFile.contents);
    if (compiledDomain.isErr()) {
      set(diagramState, (state) => ({ ...state, error: compiledDomain.error }));
      return;
    }
    const compileResult = compileTrio({
      domain: domainFile.contents,
      substance: substanceFile.contents,
      style: styleFile.contents,
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
    /* if (diagramFile.metadata.autostep) {
        _autostepToConverge(diagramFile, set);
      } */
  });
