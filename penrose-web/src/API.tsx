import { step } from "engine/Optimizer";
import {
  compileSubstance,
  checkSubstance,
  parseSubstance,
} from "compiler/Substance";
import { compileDomain, checkDomain, parseDomain } from "compiler/Domain";
import { resampleBest } from "shapes/ShapeDef";
import { collectLabels } from "utils/CollectLabels";

export const resample = async (
  state: State,
  numSamples: number
): Promise<State> => {
  const newState = resampleBest(state, numSamples);
  const labeledShapes: any = await collectLabels(newState.shapes);
  return { ...newState, shapes: labeledShapes };
};

export const stepState = async (state: State): Promise<State> => {
  const numSteps = 1;
  const newState = step(state!, numSteps);
  const labeledShapes: any = await collectLabels(newState.shapes);
  return { ...newState, shapes: labeledShapes };
};

export {
  compileDomain,
  compileSubstance,
  checkDomain,
  checkSubstance,
  parseSubstance,
  parseDomain,
};
