import { checkDomain, compileDomain, Env, parseDomain } from "compiler/Domain";
import { compileStyle } from "compiler/Style";
import {
  checkSubstance,
  compileSubstance,
  parseSubstance,
  SubstanceEnv,
} from "compiler/Substance";
import { evalShapes } from "engine/Evaluator";
import { step } from "engine/Optimizer";
import { insertPending } from "engine/PropagateUpdate";
import React from "react";
import ReactDOM from "react-dom";
import { notEmptyLabel, resampleBest, sortShapes } from "shapes/ShapeDef";
import Embed from "ui/Embed";
import { collectLabels } from "utils/CollectLabels";
import { andThen, Result } from "utils/Error";
import { loadImages } from "utils/Util";

export const resample = (state: State, numSamples: number): State => {
  return resampleBest(state, numSamples);
};

export const stepState = (state: State): State => {
  const numSteps = 1;
  return step(state, numSteps);
};

export const diagram = async (
  data: State,
  node: HTMLElement
): Promise<void> => {
  data.labelCache = await collectLabels(data.shapes);
  ReactDOM.render(<Embed data={data} />, node);
};

export const compileTrio = (
  domainProg: string,
  subProg: string,
  styProg: string
): Result<State, PenroseError> => {
  const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);

  const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
    (env) => compileSubstance(subProg, env),
    domainRes
  );

  const styRes: Result<State, PenroseError> = andThen(
    (res) => compileStyle(styProg, ...res),
    subRes
  );

  return styRes;
};

// TODO:L errors
export const prepareState = async (state: State): Promise<State> => {
  const stateAD = {
    ...state,
    originalTranslation: state.originalTranslation,
  };

  // After the pending values load, they only use the evaluated shapes (all in terms of numbers)
  // The results of the pending values are then stored back in the translation as autodiff types
  const stateEvaled: State = evalShapes(stateAD);

  const labeledShapesWithImgs: any = await loadImages(stateEvaled.shapes);

  const labelCache: LabelCache = await collectLabels(stateEvaled.shapes);

  const sortedShapes: Shape[] = sortShapes(
    labeledShapesWithImgs,
    stateEvaled.shapeOrdering
  );

  const nonEmpties = sortedShapes.filter(notEmptyLabel);

  const stateWithPendingProperties = insertPending({
    ...stateEvaled,
    labelCache,
    shapes: nonEmpties,
  });

  return stateWithPendingProperties;
};

export {
  compileDomain,
  compileSubstance,
  checkDomain,
  checkSubstance,
  parseSubstance,
  parseDomain,
};
