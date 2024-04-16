import { compile, compileDomain, resample } from "@penrose/core";
import toast from "react-hot-toast";
import { CallbackInterface, useRecoilCallback } from "recoil";
import { loadFromSnapshot } from "../utils/Utils";
import {
  Diagram,
  currentDiagramState,
  currentDomainProgramState,
  currentStyleProgramState,
  currentSubstanceProgramState,
} from "./atoms";
import { generateVariation } from "./variation";

const _compileDiagram = async (
  substance: string,
  style: string,
  domain: string,
  variation: string,
  excludeWarnings: string[],
  set: CallbackInterface["set"],
) => {
  const compiledDomain = compileDomain(domain);
  if (compiledDomain.isErr()) {
    set(currentDiagramState, (state: Diagram) => ({
      ...state,
      error: compiledDomain.error,
    }));
    return;
  }
  const compileResult = await compile({
    domain,
    substance,
    style,
    variation,
    excludeWarnings,
  });
  if (compileResult.isErr()) {
    set(currentDiagramState, (state: Diagram) => ({
      ...state,
      error: compileResult.error,
    }));
    return;
  }
  const initialState = compileResult.value;
  set(
    currentDiagramState,
    (state: Diagram): Diagram => ({
      ...state,
      error: null,
      warnings: initialState.warnings,
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
      state: initialState,
    }),
  );
};

export const useCompileDiagram = () =>
  useRecoilCallback(({ snapshot, set }) => async () => {
    const domainProgram = loadFromSnapshot(snapshot, currentDomainProgramState);
    const substanceProgram = loadFromSnapshot(
      snapshot,
      currentSubstanceProgramState,
    );
    const styleProgram = loadFromSnapshot(snapshot, currentStyleProgramState);
    const diagram = loadFromSnapshot(snapshot, currentDiagramState);
    const variation = diagram.metadata.variation;

    await _compileDiagram(
      substanceProgram,
      styleProgram,
      domainProgram,
      variation,
      [],
      set,
    );
  });

export const useResampleDiagram = () =>
  useRecoilCallback(({ set, snapshot }) => async () => {
    const diagram: Diagram = loadFromSnapshot(snapshot, currentDiagramState);
    if (diagram.state === null) {
      toast.error("Cannot resample a diagram that does not exist");
      return;
    }
    const variation = generateVariation();
    const resamplingLoading = toast.loading("Resampling...");
    const resampled = resample({ ...diagram.state, variation });
    set(currentDiagramState, (state) => ({
      ...state,
      metadata: { ...state.metadata, variation },
      state: resampled,
    }));
    toast.dismiss(resamplingLoading);
  });
