import { compileCompGraph } from "engine/EngineUtils";
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
import { Fn, FnEvaled, LabelCache, State } from "./types/state";
import { SubProg, SubstanceEnv } from "./types/substance";
import { collectLabels, insertPending } from "./utils/CollectLabels";
import { andThen, ok, Result, showError } from "./utils/Error";
import {
  bBoxDims,
  normList,
  prettyPrintExpr,
  prettyPrintFn,
  prettyPrintPath,
  toSvgPaintProperty,
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
  };
};

/**
 * Take n steps in the optimizer given the current state.
 * @param state current state
 * @param numSteps number of steps to take (default: 10000)
 */
export const stepState = (state: State, numSteps = 10000): State => {
  return stepUntilConvergence(state).unwrapOr(state);
};

/**
 * Repeatedly take one step in the optimizer given the current state until convergence.
 * @param state current state
 */
export const stepUntilConvergence = (
  state: State,
  numSteps = 10000
): Result<State, PenroseError> => {
  return ok({
    ...state,
    varyingValues: state.optimizer!.converge({
      inputs: state.inputs.map((meta) =>
        "pending" in meta ? "pending" : "sampler"
      ),
      numObjEngs: state.objFns.length,
      numConstrEngs: state.constrFns.length,
      varyingValues: state.varyingValues,
    }),
  });
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

  const withPending = insertPending({ ...state, labelCache: labelCache.value });

  const { optimizer, computeShapes } = await compileCompGraph(
    withPending.optProblem,
    withPending.shapes
  );

  return { ...withPending, optimizer, computeShapes };
};

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
export type { FnEvaled };
export type { Registry, Trio };
export type { Env };
export type { SubProg };
export type { Canvas };
