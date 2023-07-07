import { start, stepUntil } from "@penrose/optimizer";
import seedrandom from "seedrandom";
import { compileDomain } from "./compiler/Domain.js";
import { compileStyle } from "./compiler/Style.js";
import { compileSubstance } from "./compiler/Substance.js";
import { PathResolver, toInteractiveSVG, toSVG } from "./renderer/Renderer.js";
import * as ad from "./types/ad.js";
import { Env } from "./types/domain.js";
import { PenroseError } from "./types/errors.js";
import { Fn, LabelCache, State } from "./types/state.js";
import { SubstanceEnv } from "./types/substance.js";
import {
  collectLabels,
  insertPending,
  mathjaxInit,
} from "./utils/CollectLabels.js";
import {
  Result,
  andThen,
  err,
  nanError,
  ok,
  showError,
} from "./utils/Error.js";
import { safe } from "./utils/Util.js";

/**
 * Use the current resample seed to sample all shapes in the State.
 * @param state current state
 */
export const resample = (state: State): State => {
  const rng = seedrandom(state.variation);
  return insertPending({
    ...state,
    varyingValues: state.inputs.map(({ meta }) =>
      meta.init.tag === "Sampled" ? meta.init.sampler(rng) : meta.init.pending
    ),
    currentStageIndex: 0,
    params: start(state.varyingValues.length),
  });
};

const step = (
  state: State,
  options: {
    until: () => boolean;
  }
): Result<State, PenroseError> => {
  const { constraintSets, optStages, currentStageIndex } = state;
  const stage = optStages[currentStageIndex];
  const masks = safe(constraintSets.get(stage), "missing stage");
  const xs = new Float64Array(state.varyingValues);
  const params = stepUntil(
    (x: Float64Array, weight: number, grad: Float64Array): number =>
      state.gradient(masks, x, weight, grad).phi,
    xs,
    state.params,
    options.until
  );
  // if there is an optimizer error, wrap it around a `PenroseError`
  if (params.optStatus === "Error") {
    return err({
      errorType: "RuntimeError",
      ...nanError("", state),
    });
  } else {
    return ok({ ...state, varyingValues: Array.from(xs), params });
  }
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 10000)
 */
export const stepTimes = (
  state: State,
  numSteps = 10000
): Result<State, PenroseError> => {
  let i = 0;
  const steppedState = step(state, { until: (): boolean => i++ >= numSteps });
  if (steppedState.isErr()) {
    return steppedState;
  } else {
    const state = steppedState.value;
    if (isOptimized(state) && !finalStage(state)) {
      const nextInitState = nextStage(state);
      return ok(nextInitState);
    } else {
      return steppedState;
    }
  }
};

/**
 * Move the current state to the next layout stage. If the current state is already at the final stage, return the current state.
 * @param state current state
 */
export const nextStage = (state: State): State => {
  if (finalStage(state)) {
    return state;
  } else {
    return {
      ...state,
      currentStageIndex: state.currentStageIndex + 1,
      params: start(state.varyingValues.length),
    };
  }
};

/**
 * Run the optimizer on the current state until the current layout stage converges.
 * @param state current state
 */
export const stepNextStage = (state: State): Result<State, PenroseError> => {
  let currentState = state;
  while (!isOptimized(currentState)) {
    // step until convergence of the current stage.
    const res = step(currentState, { until: () => false });
    if (res.isOk()) {
      currentState = res.value;
    } else {
      return res;
    }
  }
  return ok(nextStage(currentState));
};

/**
 * Run the optimizer on the current state until it converges.
 * @param state current state
 */
export const optimize = (state: State): Result<State, PenroseError> => {
  let currentState = state;
  while (!isOptimized(currentState) || !finalStage(currentState)) {
    if (isOptimized(currentState)) {
      currentState = nextStage(currentState);
    }
    const res = step(currentState, { until: () => false });
    if (res.isOk()) {
      currentState = res.value;
    } else {
      return res;
    }
  }
  return ok(currentState);
};

const optimizeOrThrow = (state: State): State => {
  const result = optimize(state);
  if (result.isErr()) {
    throw Error(showError(result.error));
  } else {
    return result.value;
  }
};

/**
 * Embed a static Penrose diagram in a DOM node.
 *
 * @param prog a Penrose trio and variation
 * @param node a node in the DOM tree
 * @param pathResolver a resolver function for fetching Style imports
 * @param name the name of the diagram
 */
export const diagram = async (
  prog: {
    substance: string;
    style: string;
    domain: string;
    variation: string;
    excludeWarnings: string[];
  },
  node: HTMLElement,
  pathResolver: PathResolver,
  name?: string
): Promise<void> => {
  const res = await compile(prog);
  if (res.isOk()) {
    const state: State = res.value;
    const optimized = optimizeOrThrow(state);
    const rendered = await toSVG(optimized, pathResolver, name ?? "");
    node.appendChild(rendered);
  } else {
    throw Error(
      `Error when generating Penrose diagram: ${showError(res.error)}`
    );
  }
};

/**
 * Embed an interactive Penrose diagram in a DOM node.
 *
 * @param prog a Penrose trio and variation
 * @param pathResolver a resolver function for fetching Style imports
 * @param node a node in the DOM tree
 * @param name the name of the diagram
 */
export const interactiveDiagram = async (
  prog: {
    substance: string;
    style: string;
    domain: string;
    variation: string;
    excludeWarnings: string[];
  },
  node: HTMLElement,
  pathResolver: PathResolver,
  name?: string
): Promise<void> => {
  const updateData = async (state: State) => {
    const stepped = optimizeOrThrow(state);
    const rendering = await toInteractiveSVG(
      stepped,
      updateData,
      pathResolver,
      name ?? ""
    );
    node.replaceChild(rendering, node.firstChild!);
  };
  const res = await compile(prog);
  if (res.isOk()) {
    const state: State = res.value;
    const optimized = optimizeOrThrow(state);
    const rendering = await toInteractiveSVG(
      optimized,
      updateData,
      pathResolver,
      name ?? ""
    );
    node.appendChild(rendering);
  } else {
    throw Error(
      `Error when generating Penrose diagram: ${showError(res.error)}`
    );
  }
};

/**
 * Given a trio of Domain, Substance, and Style programs, compile them into an initial `State`.
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 */
export const compile = async (prog: {
  substance: string;
  style: string;
  domain: string;
  variation: string;
  excludeWarnings?: string[];
}): Promise<Result<State, PenroseError>> => {
  const domainRes: Result<Env, PenroseError> = compileDomain(prog.domain);

  const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
    (env) => compileSubstance(prog.substance, env),
    domainRes
  );

  const styRes: Result<State, PenroseError> = subRes.isErr()
    ? err(subRes.error)
    : await compileStyle(
        prog.variation,
        prog.style,
        prog.excludeWarnings ?? [],
        ...subRes.value
      );

  if (styRes.isErr()) {
    return styRes;
  } else {
    const state = styRes.value;
    // collect labels and return state
    const convert = mathjaxInit();
    const labelCache: Result<LabelCache, PenroseError> = await collectLabels(
      state.shapes,
      convert
    );

    if (labelCache.isErr()) {
      return err(labelCache.error);
    }
    return ok(insertPending({ ...state, labelCache: labelCache.value }));
  }
};

/**
 * Returns true if state is optimized
 * @param state current state
 */
export const isOptimized = (state: State): boolean =>
  state.params.optStatus === "EPConverged";

/**
 * Returns true if state results in an error
 * @param state current state
 */
export const isError = (state: State): boolean =>
  state.params.optStatus === "Error";

/**
 * Returns true if the diagram state is on the last layout stage in the layout pipeline
 * @param state current state
 */
export const finalStage = (state: State): boolean =>
  state.currentStageIndex === state.optStages.length - 1;

/**
 * Returns true if state is the initial frame
 * @param state current state
 */
export const isInitial = (state: State): boolean =>
  state.params.optStatus === "NewIter";

const evalGrad = (s: State): ad.OptOutputs => {
  const { constraintSets, optStages, currentStageIndex } = s;
  const stage = optStages[currentStageIndex];
  const masks = safe(constraintSets.get(stage), "missing stage");
  const x = new Float64Array(s.varyingValues);
  // we constructed `x` to throw away, so it's OK to update it in-place with the
  // gradient after computing the energy
  return s.gradient(masks, x, s.params.weight, x);
};

/**
 * Evaluate the overall energy of a `State`.
 * @param s a state
 * @returns a scalar value of the current energy
 */
export const evalEnergy = (s: State): number => evalGrad(s).phi;
// TODO: maybe don't also compute the gradient, just to throw it away

/**
 * Evaluate a list of constraints/objectives.
 * @param s a state
 * @returns a list of the energies of the requested functions, evaluated at the `varyingValues` in the `State`
 */
export const evalFns = (
  s: State
): {
  constrEngs: number[];
  objEngs: number[];
} => {
  // Evaluate the energy of each requested function (of the given type) on the varying values in the state
  const outputs = evalGrad(s);
  // TODO: maybe don't also compute the gradient, just to throw it away
  return { constrEngs: outputs.constraints, objEngs: outputs.objectives };
};

export type PenroseState = State;
export type PenroseFn = Fn;

export * from "./api.js";
export { checkDomain, compileDomain, parseDomain } from "./compiler/Domain.js";
export {
  checkSubstance,
  compileSubstance,
  parseSubstance,
  prettySubstance,
} from "./compiler/Substance.js";
export { constrDict } from "./contrib/Constraints.js";
export { compDict } from "./contrib/Functions.js";
export { objDict } from "./contrib/Objectives.js";
export { toInteractiveSVG, toSVG } from "./renderer/Renderer.js";
export type { PathResolver } from "./renderer/Renderer.js";
export { makeCanvas, simpleContext } from "./shapes/Samplers.js";
export type { Canvas } from "./shapes/Samplers.js";
export { sampleShape, shapeTypes } from "./shapes/Shapes.js";
export type { ShapeType } from "./shapes/Shapes.js";
export type { Env } from "./types/domain.js";
export type {
  PenroseError,
  Warning as PenroseWarning,
} from "./types/errors.js";
export type { CompFunc } from "./types/functions.js";
export type { SubProg } from "./types/substance.js";
export * as Value from "./types/value.js";
export { errLocs, showError } from "./utils/Error.js";
export type { Result } from "./utils/Error.js";
export {
  allWarnings,
  describeType,
  hexToRgba,
  prettyPrintExpr,
  prettyPrintFn,
  prettyPrintPath,
  rgbaToHex,
  zip2,
} from "./utils/Util.js";
