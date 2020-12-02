import { step } from "engine/Optimizer";
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
