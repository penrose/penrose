import { initConstraintWeight } from "@penrose/optimizer";
import seedrandom from "seedrandom";
import { checkDomain, compileDomain, parseDomain } from "./compiler/Domain";
import { compileStyle } from "./compiler/Style";
import {
  checkSubstance,
  compileSubstance,
  parseSubstance,
  prettySubstance,
} from "./compiler/Substance";
import {
  PathResolver,
  RenderInteractive,
  RenderShape,
  RenderStatic,
} from "./renderer/Renderer";
import { Canvas } from "./shapes/Samplers";
import { showMutations } from "./synthesis/Mutation";
import { Synthesizer } from "./synthesis/Synthesizer";
import { Env } from "./types/domain";
import { PenroseError } from "./types/errors";
import { Registry, Trio } from "./types/io";
import { Fn, LabelCache, State } from "./types/state";
import { SubProg, SubstanceEnv } from "./types/substance";
import { collectLabels, insertPending } from "./utils/CollectLabels";
import { andThen, err, nanError, ok, Result, showError } from "./utils/Error";
import {
  bBoxDims,
  normList,
  prettyPrintExpr,
  prettyPrintFn,
  prettyPrintPath,
  toSvgPaintProperty,
  zip2,
} from "./utils/Util";

/**
 * Use the current resample seed to sample all shapes in the State.
 * @param state current state
 */
export const resample = (state: State): State => {
  const rng = seedrandom(state.variation);
  return {
    ...state,
    varyingValues: state.inputs.map((meta, i) =>
      "sampler" in meta ? meta.sampler(rng) : state.varyingValues[i]
    ),
    params: {
      ...state.params,
      weight: initConstraintWeight,
      optStatus: "NewIter",
    },
    frozenValues: new Set(),
  };
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 10000)
 */
export const stepState = (state: State, numSteps = 10000): State => {
  return { ...state, ...step(state, numSteps) };
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 10000)
 */
export const stepStateSafe = (
  state: State,
  numSteps = 10000
): Result<State, PenroseError> => {
  const res = stepState(state, numSteps);
  if (res.params.optStatus === "Error") {
    return err({
      errorType: "RuntimeError",
      ...nanError("", res),
    });
  }
  return ok(res);
};

/**
 * Repeatedly take one step in the optimizer given the current state until convergence.
 * @param state current state
 */
export const stepUntilConvergence = (
  state: State,
  numSteps = 10000
): Result<State, PenroseError> => {
  let currentState = state;
  while (
    !(currentState.params.optStatus === "Error") &&
    !stateConverged(currentState)
  ) {
    currentState = stepState(currentState, numSteps);
  }
  if (currentState.params.optStatus === "Error") {
    return err({
      errorType: "RuntimeError",
      ...nanError("", currentState),
    });
  }
  return ok(currentState);
};

const stepUntilConvergenceOrThrow = (state: State): State => {
  const result = stepUntilConvergence(state);
  if (result.isErr()) {
    throw Error(showError(result.error));
  } else {
    return result.value;
  }
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
  prog: {
    substance: string;
    style: string;
    domain: string;
    variation: string;
  },
  node: HTMLElement,
  pathResolver: PathResolver
): Promise<void> => {
  const res = compileTrio(prog);
  if (res.isOk()) {
    const state: State = await prepareState(res.value);
    const optimized = stepUntilConvergenceOrThrow(state);
    const rendered = await RenderStatic(optimized, pathResolver);
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
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 * @param node a node in the DOM tree
 */
export const interactiveDiagram = async (
  prog: {
    substance: string;
    style: string;
    domain: string;
    variation: string;
  },
  node: HTMLElement,
  pathResolver: PathResolver
): Promise<void> => {
  const updateData = async (state: State) => {
    const stepped = stepUntilConvergenceOrThrow(state);
    const rendering = await RenderInteractive(
      stepped,
      updateData,
      pathResolver
    );
    node.replaceChild(rendering, node.firstChild!);
  };
  const res = compileTrio(prog);
  if (res.isOk()) {
    const state: State = await prepareState(res.value);
    const optimized = stepUntilConvergenceOrThrow(state);
    const rendering = await RenderInteractive(
      optimized,
      updateData,
      pathResolver
    );
    node.appendChild(rendering);
  } else {
    throw Error(
      `Error when generating Penrose diagram: ${showError(res.error)}`
    );
  }
};

/**
 * Given a trio of Domain, Substance, and Style programs, compile them into an initial `State`. Note that this function does _not_ evaluate the shapes. Generation of shapes is handled in `prepareState`.
 * @param domainProg a Domain program string
 * @param subProg a Substance program string
 * @param styProg a Style program string
 */
export const compileTrio = (prog: {
  substance: string;
  style: string;
  domain: string;
  variation: string;
}): Result<State, PenroseError> => {
  const domainRes: Result<Env, PenroseError> = compileDomain(prog.domain);

  const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
    (env) => compileSubstance(prog.substance, env),
    domainRes
  );

  const styRes: Result<State, PenroseError> = andThen(
    (res) => compileStyle(prog.variation, prog.style, ...res),
    subRes
  );

  return styRes;
};

/**
 * Collect labels and images (if applicable).
 * @param state an initial diagram state
 */
export const prepareState = async (state: State): Promise<State> => {
  const labelCache: Result<LabelCache, PenroseError> = await collectLabels(
    state.shapes
  );

  if (labelCache.isErr()) {
    throw Error(showError(labelCache.error));
  }

  return insertPending({ ...state, labelCache: labelCache.value });
};

/**
 * Returns true if state is converged
 * @param state current state
 */
export const stateConverged = (state: State): boolean =>
  state.params.optStatus === "EPConverged";

/**
 * Returns true if state is the initial frame
 * @param state current state
 */
export const stateInitial = (state: State): boolean =>
  state.params.optStatus === "NewIter";

/**
 * Read and flatten the registry file for Penrose examples into a list of program trios.
 *
 * @param registry JSON file of the registry
 */
export const readRegistry = (registry: Registry): Trio[] => {
  const { substances, styles, domains, trios } = registry;
  const res = [];
  for (const {
    domain: dslID,
    style: styID,
    substance: subID,
    variation,
  } of trios) {
    const domain = domains[dslID];
    const substance = substances[subID];
    const style = styles[styID];
    const trio = {
      substanceURI: substance.URI,
      styleURI: style.URI,
      domainURI: domain.URI,
      substanceName: substance.name,
      styleName: style.name,
      domainName: domain.name,
      variation,
      name: `${subID}-${styID}`,
    };
    res.push(trio);
  }
  return res;
};

/**
 * Evaluate the overall energy of a `State`. If the `State` does not have an optimization problem initialized (i.e. it doesn't have a defined `objectiveAndGradient` field), this function will call `genOptProblem` to initialize it. Otherwise, it will evaluate the cached objective function.
 * @param s a state with or without an optimization problem initialized
 * @returns a scalar value of the current energy
 */
export const evalEnergy = (s: State): number => {
  const { objectiveAndGradient, weight } = s.params;
  // TODO: maybe don't also compute the gradient, just to throw it away
  return objectiveAndGradient(weight)(s.varyingValues).f;
};

/**
 * Evaluate a list of constraints/objectives: this will be useful if a user want to apply a subset of constrs/objs on a `State`. This function assumes that the state already has the objectives and constraints compiled.
 * @param fns a list of constraints/objectives
 * @param s a state with its opt functions cached
 * @returns a list of the energies of the requested functions, evaluated at the `varyingValues` in the `State`
 */
export const evalFns = (
  s: State
): { constrEngs: Map<string, number>; objEngs: Map<string, number> } => {
  // Evaluate the energy of each requested function (of the given type) on the varying values in the state
  let { lastConstrEnergies, lastObjEnergies } = s.params;
  if (!lastConstrEnergies || !lastObjEnergies) {
    const { objEngs, constrEngs } = s.params.objectiveAndGradient(
      s.params.weight
    )(s.varyingValues);
    lastConstrEnergies = constrEngs;
    lastObjEnergies = objEngs;
  }
  return {
    constrEngs: new Map(
      zip2(s.constrFns.map(prettyPrintFn), lastConstrEnergies)
    ),
    objEngs: new Map(zip2(s.objFns.map(prettyPrintFn), lastObjEnergies)),
  };
};

export type PenroseState = State;
export type PenroseFn = Fn;

export { constrDict } from "./contrib/Constraints";
export { compDict } from "./contrib/Functions";
export { objDict } from "./contrib/Objectives";
export { secondaryGraph } from "./engine/Autodiff";
export type { PathResolver } from "./renderer/Renderer";
export { makeCanvas, simpleContext } from "./shapes/Samplers";
export { shapedefs } from "./shapes/Shapes";
export type {
  SynthesizedSubstance,
  SynthesizerSetting,
} from "./synthesis/Synthesizer";
export type { PenroseError } from "./types/errors";
export type { Shape } from "./types/shape";
export * as Value from "./types/value";
export type { Result } from "./utils/Error";
export { hexToRgba, rgbaToHex } from "./utils/Util";
export {
  compileDomain,
  compileSubstance,
  checkDomain,
  checkSubstance,
  parseSubstance,
  parseDomain,
  Synthesizer,
  showMutations,
  RenderShape,
  RenderInteractive,
  RenderStatic,
  bBoxDims,
  prettySubstance,
  showError,
  prettyPrintFn,
  prettyPrintPath,
  prettyPrintExpr,
  normList,
  toSvgPaintProperty,
};
export type { Registry, Trio };
export type { Env };
export type { SubProg };
export type { Canvas };
