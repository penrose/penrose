import { genOptProblem } from "@penrose/optimizer";
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
  safe,
  toSvgPaintProperty,
} from "./utils/Util";

/**
 * Use the current resample seed to sample all shapes in the State.
 * @param state current state
 */
export const resample = (state: State): State => {
  const rng = seedrandom(state.variation);
  const { constraintSets, optStages } = state;
  const { inputMask, objMask, constrMask } = safe(
    constraintSets.get(optStages[0]),
    "missing first stage"
  );
  return insertPending({
    ...state,
    varyingValues: state.inputs.map((meta) =>
      meta.init.tag === "Sampled" ? meta.init.sampler(rng) : meta.init.pending
    ),
    currentStageIndex: 0,
    params: genOptProblem(inputMask, objMask, constrMask),
  });
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 10000)
 */
export const stepState = (state: State, numSteps = 10000): State => {
  const steppedState: State = {
    ...state,
    ...state.gradient.step(state, numSteps),
  };
  if (stateConverged(steppedState) && !finalStage(steppedState)) {
    const nextInitState = nextStage(steppedState);
    return nextInitState;
  } else {
    return steppedState;
  }
};

export const nextStage = (state: State): State => {
  if (finalStage(state)) {
    return state;
  } else {
    const { constraintSets, optStages, currentStageIndex } = state;
    const nextStage = optStages[currentStageIndex + 1];
    const { inputMask, objMask, constrMask } = safe(
      constraintSets.get(nextStage),
      "missing next stage"
    );
    return {
      ...state,
      currentStageIndex: currentStageIndex + 1,
      params: genOptProblem(inputMask, objMask, constrMask),
    };
  }
};

export const stepNextStage = (state: State, numSteps = 10000): State => {
  let currentState = state;
  while (
    !(currentState.params.optStatus === "Error") &&
    !stateConverged(currentState)
  ) {
    currentState = {
      ...currentState,
      ...currentState.gradient.step(currentState, numSteps),
    };
  }
  return nextStage(currentState);
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
    (!stateConverged(currentState) || !finalStage(currentState))
  ) {
    if (stateConverged(currentState)) {
      currentState = nextStage(currentState);
    }
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
  const res = await compileTrio(prog);
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
  const res = await compileTrio(prog);
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
export const compileTrio = async (prog: {
  substance: string;
  style: string;
  domain: string;
  variation: string;
}): Promise<Result<State, PenroseError>> => {
  const domainRes: Result<Env, PenroseError> = compileDomain(prog.domain);

  const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
    (env) => compileSubstance(prog.substance, env),
    domainRes
  );

  const styRes: Result<State, PenroseError> = subRes.isErr()
    ? err(subRes.error)
    : await compileStyle(prog.variation, prog.style, ...subRes.value);

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
 * Returns true if the diagram state is on the last layout stage in the layout pipeline
 * @param state current state
 */
export const finalStage = (state: State): boolean =>
  state.currentStageIndex === state.optStages.length - 1;

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
 * @param galleryOnly Only return trios where `gallery === true`
 */
export const readRegistry = (
  registry: Registry,
  galleryOnly: boolean
): Trio[] => {
  const { substances, styles, domains, trios } = registry;
  const res = [];
  for (const trioEntry of trios) {
    const {
      domain: dslID,
      style: styID,
      substance: subID,
      variation,
      gallery,
      name,
    } = trioEntry;
    const domain = domains[dslID];
    const substance = substances[subID];
    const style = styles[styID];
    const trio: Trio = {
      substanceURI: registry.root + substance.URI,
      styleURI: registry.root + style.URI,
      domainURI: registry.root + domain.URI,
      substanceID: subID,
      domainID: dslID,
      styleID: styID,
      variation,
      name: name ?? `${subID}-${styID}`,
      id: `${subID}-${styID}`,
      gallery: gallery ?? false,
    };
    if (!galleryOnly || trioEntry.gallery) {
      res.push(trio);
    }
  }
  return res;
};

/**
 * Evaluate the overall energy of a `State`. If the `State` does not have an optimization problem initialized (i.e. it doesn't have a defined `objectiveAndGradient` field), this function will call `genOptProblem` to initialize it. Otherwise, it will evaluate the cached objective function.
 * @param s a state with or without an optimization problem initialized
 * @returns a scalar value of the current energy
 */
export const evalEnergy = (s: State): number => {
  // TODO: maybe don't also compute the gradient, just to throw it away
  return s.gradient.call([...s.varyingValues, s.params.weight]).primary;
};

/**
 * Evaluate a list of constraints/objectives: this will be useful if a user want to apply a subset of constrs/objs on a `State`. This function assumes that the state already has the objectives and constraints compiled.
 * @param fns a list of constraints/objectives
 * @param s a state with its opt functions cached
 * @returns a list of the energies of the requested functions, evaluated at the `varyingValues` in the `State`
 */
export const evalFns = (
  s: State
): {
  constrEngs: number[];
  objEngs: number[];
} => {
  const { constrFns, objFns } = s;
  // Evaluate the energy of each requested function (of the given type) on the varying values in the state
  let { lastObjEnergies, lastConstrEnergies } = s.params;
  if (lastObjEnergies === null || lastConstrEnergies === null) {
    const { secondary } = s.gradient.call([
      ...s.varyingValues,
      s.params.weight,
    ]);
    lastObjEnergies = secondary.slice(0, s.params.objMask.length);
    lastConstrEnergies = secondary.slice(s.params.objMask.length);
  }
  return {
    constrEngs: lastConstrEnergies,
    objEngs: lastObjEnergies,
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
export { hexToRgba, rgbaToHex, zip2 } from "./utils/Util";
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
