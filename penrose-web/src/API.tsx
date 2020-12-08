import { makeTranslationDifferentiable } from "engine/EngineUtils";
import { decodeState, evalShapes } from "engine/Evaluator";
import { step } from "engine/Optimizer";
import { insertPending } from "engine/PropagateUpdate";
import { resampleBest } from "shapes/ShapeDef";
import { collectLabels } from "utils/CollectLabels";

export const resample = async (
  state: State,
  numSamples: number
): Promise<State> => {
  const newState = resampleBest(state, numSamples);
  const labelCache: LabelCache = await collectLabels(newState.shapes);
  return { ...newState, labelCache };
};

export const stepState = async (state: State): Promise<State> => {
  const numSteps = 1;
  const newState = step(state, numSteps);
  const labelCache: LabelCache = await collectLabels(newState.shapes);
  return { ...newState, labelCache };
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
  console.log(labelCache);

  // const labeledShapesWithImgs: any = await loadImages(labeledShapes);
  return insertPending({
    ...stateEvaled,
    labelCache,
  });
};
