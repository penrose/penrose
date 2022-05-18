import { Matrix } from "ml-matrix";
import { Canvas } from "shapes/Samplers";
import * as ad from "types/ad";
import { A } from "./ast";
import { Shape } from "./shape";
import { Expr, Path } from "./style";
import { Translation } from "./translation";
import { ArgVal, FloatV } from "./value";

export type ShapeFn = (xs: number[]) => Shape[];

/**
 * The diagram state modeling the original Haskell types
 */
export interface State {
  seeds: Seeds;
  varyingInitInfo: { [pathStr: string]: number }; // These are the values the style writer set initially
  varyingPaths: Path<A>[];
  uninitializedPaths: Path<A>[];
  pendingPaths: Path<A>[];
  objFns: Fn[];
  constrFns: Fn[];
  varyingValues: number[];
  translation: Translation;
  shapeOrdering: string[];
  labelCache: LabelCache;
  shapes: Shape[];
  canvas: Canvas;
  computeShapes: ShapeFn;
  params: Params;
}

// Some compDict functions (currently only sampleColor) need a prng, so we need
// to keep a seed around in the state to allow us to recreate it at every step
// in order for those functions to give deterministic results as the
// optimization progresses. However, our code is currently not very
// well-structured, so there are a lot of different places that independently
// evaluate expressions which can include calls to these nondeterministic
// compDict functions. Thus, as a temporary solution, we keep around a different
// seed for each of those places (only the ones in index.ts), so that even
// though they won't be able to agree with each other, at least each one will
// agree with itself across different steps of the optimization. Note, to be
// clear: the fact that the different places using different seeds disagree with
// each other is actually a problem, but for now that problem doesn't have a
// negative impact because we don't currently use color in the optimization
// itself. An ideal solution would be to not keep around any prng or seed in the
// state at all, and instead generate an explicit representation of all
// necessary randomness during an initial sampling that happens right after
// compilation.
export interface Seeds {
  evalEnergy: string;
  evalFns: string;
  prepare: string;
  resample: string;
  step: string;
}

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;
export interface EquationData {
  tag: "EquationData";
  width: FloatV<number>;
  height: FloatV<number>;
  rendered: HTMLElement;
}

export interface TextData {
  tag: "TextData";
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
}

export type LabelCache = [string, LabelData][];

export type VaryMap<T = ad.Num> = Map<string, T>;

export interface FnDone<T> {
  name: string;
  args: ArgVal<T>[];
  optType: OptType;
}

/**
 * Generic export interface for constraint or objective functions
 */
export interface Fn {
  fname: string;
  fargs: Expr<A>[];
  optType: OptType;
}
export type OptType = "ObjFn" | "ConstrFn";

export type OptStatus =
  | "NewIter"
  | "UnconstrainedRunning"
  | "UnconstrainedConverged"
  | "EPConverged"
  | "Error";

// `n` is the size of the varying state
export interface LbfgsParams {
  lastState: Matrix | undefined; // nx1 (col vec)
  lastGrad: Matrix | undefined; // nx1 (col vec)
  s_list: Matrix[]; // list of nx1 col vecs
  y_list: Matrix[]; // list of nx1 col vecs
  numUnconstrSteps: number;
  memSize: number;
}

export interface FnEvaled {
  f: number;
  gradf: number[];
  objEngs: number[];
  constrEngs: number[];
}

export interface Params {
  optStatus: OptStatus;
  /** Constraint weight for exterior point method **/
  weight: number;
  /** Info for unconstrained optimization **/
  UOround: number;
  lastUOstate: number[];
  lastUOenergy: number;
  lastObjEnergies: number[];
  lastConstrEnergies: number[];

  /** Info for exterior point method **/
  EPround: number;
  lastEPstate: number[];
  lastEPenergy: number;

  lastGradient: number[]; // Value of gradient evaluated at the last state
  lastGradientPreconditioned: number[]; // Value of gradient evaluated at the last state, preconditioned by LBFGS
  // ^ Those two are stored to make them available to Style later

  // For L-BFGS
  lbfgsInfo: LbfgsParams;

  xsVars: ad.Num[]; // Computational graph (leaf vars), from the bottom up

  // For energy/gradient compilation
  graphs: ad.GradGraphs;

  functionsCompiled: boolean;

  // Higher-order functions (not yet applied with hyperparameters, in this case, just the EP weight)
  objectiveAndGradient: (epWeight: number) => (xs: number[]) => FnEvaled;

  // Applied with weight (or hyperparameters in general) -- may change with the EP round
  currObjectiveAndGradient(xs: number[]): FnEvaled;

  // `xsVars` are all the leaves of the energy graph
  energyGraph: ad.Num; // This is the top of the energy graph (parent node)
  epWeightNode: ad.Num; // Handle to node for EP weight (so it can be set as the weight changes)
}

// Just the compiled function and its grad, with no weights for EP/constraints/penalties, etc.
export type FnCached = (xs: number[]) => FnEvaled;

export interface WeightInfo {
  epWeightNode: ad.Input;
  epWeight: number;
}
