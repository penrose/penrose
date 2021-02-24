import { checkDomain, compileDomain, Env, parseDomain } from "compiler/Domain";
import { compileStyle } from "compiler/Style";
import {
  checkSubstance,
  compileSubstance,
  parseSubstance,
  SubstanceEnv,
} from "compiler/Substance";
import { evalShapes } from "engine/Evaluator";
import { initializeMat, step } from "engine/Optimizer";
import { insertPending } from "engine/PropagateUpdate";
import RenderStatic, {
  RenderInteractive,
  RenderShape,
} from "renderer/Renderer";
import { notEmptyLabel, resampleBest, sortShapes } from "renderer/ShapeDef";
import { PenroseError } from "types/errors";
import * as ShapeTypes from "types/shapeTypes";
import { Shape } from "types/shapeTypes";
import { collectLabels } from "utils/CollectLabels";
import { andThen, Result, showError } from "utils/Error";
import { bBoxDims, toHex } from "utils/Util";

/**
 * Resample all shapes in the state by generating a number of samples (`numSamples`) and picking the sample with the lowest initial energy value.
 * @param state current state
 * @param numSamples number of samples to choose from
 */
export const resample = (state: State, numSamples: number): State => {
  return resampleBest(state, numSamples);
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 1)
 */
export const stepState = (state: State, numSteps = 1): State => {
  return step(state, numSteps);
};

/**
 * Repeatedly take one step in the optimizer given the current state until convergence.
 * @param state current state
 */
export const stepUntilConvergence = (state: State): State => {
  const numSteps = 1;
  let currentState = state;
  while (!stateConverged(currentState))
    currentState = step(currentState, numSteps);
  return currentState;
};

/**
 * Embed a static Penrose diagram in a DOM node.
 *
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 * @param node a node in the DOM tree
 */
export const diagram = async (
  domainProg: string,
  subProg: string,
  styProg: string,
  node: HTMLElement
): Promise<void> => {
  const res = compileTrio(domainProg, subProg, styProg);
  if (res.isOk()) {
    const state: State = await prepareState(res.value);
    const optimized = stepUntilConvergence(state);
    node.appendChild(RenderStatic(optimized));
  } else
    throw Error(
      `Error when generating Penrose diagram: ${showError(res.error)}`
    );
};

/**
 * Embed an interactive Penrose diagram in a DOM node.
 *
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 * @param node a node in the DOM tree
 */
export const interactiveDiagram = async (
  domainProg: string,
  subProg: string,
  styProg: string,
  node: HTMLElement
): Promise<void> => {
  const updateData = (state: State) => {
    const stepped = stepUntilConvergence(state);
    node.replaceChild(
      RenderInteractive(stepped, updateData),
      node.firstChild as Node
    );
  };
  const res = compileTrio(domainProg, subProg, styProg);
  if (res.isOk()) {
    const state: State = await prepareState(res.value);
    const optimized = stepUntilConvergence(state);
    node.appendChild(RenderInteractive(optimized, updateData));
  } else
    throw Error(
      `Error when generating Penrose diagram: ${showError(res.error)}`
    );
};

/**
 * Given a trio of Domain, Substance, and Style programs, compile them into an initial `State`. Note that this function does _not_ evaluate the shapes. Generation of shapes is handled in `prepareState`.
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 */
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

/**
 * Generate all shapes and collect labels and images (if applicable) given an initial `State`.
 * @param state an initial diagram state
 */
export const prepareState = async (state: State): Promise<State> => {
  await initializeMat();
  // TODO:L errors
  const stateAD = {
    ...state,
    originalTranslation: state.originalTranslation,
  };

  // After the pending values load, they only use the evaluated shapes (all in terms of numbers)
  // The results of the pending values are then stored back in the translation as autodiff types
  const stateEvaled: State = evalShapes(stateAD);

  const labelCache: LabelCache = await collectLabels(stateEvaled.shapes);

  const sortedShapes: Shape[] = sortShapes(
    stateEvaled.shapes,
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

/**
 * Returns true if state is converged
 * @param state current state
 */
export const stateConverged = (state: State): boolean =>
  state.params.optStatus.tag === "EPConverged";

/**
 * Returns true if state is the initial frame
 * @param state current state
 */
export const stateInitial = (state: State): boolean =>
  state.params.optStatus.tag === "NewIter";

export type PenroseState = State;

export type { PenroseError } from "./types/errors";

export {
  compileDomain,
  compileSubstance,
  checkDomain,
  checkSubstance,
  parseSubstance,
  parseDomain,
  RenderStatic,
  RenderShape,
  RenderInteractive,
  ShapeTypes,
  bBoxDims,
  toHex,
  showError,
};
