import { makeTranslationDifferentiable } from "engine/EngineUtils";
import { decodeState, evalShapes } from "engine/Evaluator";
import { step } from "engine/Optimizer";
import {
  compileSubstance,
  checkSubstance,
  parseSubstance,
} from "compiler/Substance";
import { compileDomain, checkDomain, parseDomain } from "compiler/Domain";
import { insertPending } from "engine/PropagateUpdate";
import React from "react";
import ReactDOM from "react-dom";
import { resampleBest } from "shapes/ShapeDef";
import Canvas from "ui/Canvas";
import Embed from "ui/Embed";
import { collectLabels } from "utils/CollectLabels";

export const resample = (state: State, numSamples: number): State => {
  return resampleBest(state, numSamples);
};

export const stepState = (state: State): State => {
  const numSteps = 1;
  return step(state, numSteps);
};

/**
 * Decode
 * NOTE: this function is only used for compilerOutput now. Will deprecate as soon as style compiler is in the frontend
 */
export const prepareState = async (data: any): Promise<State> => {
  const state: State = decodeState(data);

  // Make sure that the state decoded from backend conforms to the types in types.d.ts, otherwise the typescript checking is just not valid for e.g. Tensors
  // convert all TagExprs (tagged Done or Pending) in the translation to Tensors (autodiff types)

  const stateAD: State = {
    ...state,
    originalTranslation: state.originalTranslation,
    translation: makeTranslationDifferentiable(state.translation),
  };

  // After the pending values load, they only use the evaluated shapes (all in terms of numbers)
  // The results of the pending values are then stored back in the translation as autodiff types
  const stateEvaled: State = evalShapes(stateAD);
  const labelCache: LabelCache = await collectLabels(stateEvaled.shapes);

  // const labeledShapesWithImgs: any = await loadImages(labeledShapes);
  return insertPending({
    ...stateEvaled,
    labelCache,
  });
};

export const diagram = async (
  data: State,
  node: HTMLElement
): Promise<void> => {
  data.labelCache = await collectLabels(data.shapes);
  ReactDOM.render(<Embed data={data} />, node);
};

export {
  compileDomain,
  compileSubstance,
  checkDomain,
  checkSubstance,
  parseSubstance,
  parseDomain,
};
