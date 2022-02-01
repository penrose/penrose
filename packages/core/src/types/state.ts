import { Canvas } from "shapes/Samplers";
import { VarAD, GradGraphs } from "./ad";
import { A } from "./ast";
import { Shape } from "./shape";
import { Expr, Path } from "./style";
import { ArgVal, IFloatV, Translation, Value } from "./value";

/**
 * The diagram state modeling the original Haskell types
 */
export interface IState {
  seeds: Seeds;
  varyingPaths: Path<A>[];
  varyingInitInfo: { [pathStr: string]: number }; // These are the values the style writer set initially
  shapePaths: Path<A>[];
  shapeProperties: any; // TODO: types
  uninitializedPaths: Path<A>[];
  params: Params;
  objFns: Fn[];
  constrFns: Fn[];
  policyParams: any; // TODO: types
  oConfig: any; // TODO: types
  pendingPaths: Path<A>[];
  varyingValues: number[];
  translation: Translation;
  originalTranslation: Translation;
  shapeOrdering: string[];
  labelCache: LabelCache;
  shapes: Shape[];
  varyingMap: VaryMap;
  canvas: Canvas;
}
export type State = IState;

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
  width: IFloatV<number>;
  height: IFloatV<number>;
  rendered: HTMLElement;
}

export interface TextData {
  tag: "TextData";
  width: IFloatV<number>;
  height: IFloatV<number>;
  descent: IFloatV<number>;
  ascent: IFloatV<number>;
}

export type LabelCache = [string, LabelData][];

export type VaryMap<T = VarAD> = Map<string, T>;

export type FnDone<T> = IFnDone<T>;
export interface IFnDone<T> {
  name: string;
  args: ArgVal<T>[];
  optType: OptType;
}

export type Fn = IFn;

/**
 * Generic export interface for constraint or objective functions
 */
export interface IFn {
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

export type LbfgsParams = ILbfgsParams;

// `n` is the size of the varying state
export interface ILbfgsParams {
  // TODO: Store as matrix types
  lastState: any | undefined; // nx1 (col vec)
  lastGrad: any | undefined; // nx1 (col vec)
  s_list: any[]; // list of nx1 col vecs
  y_list: any[]; // list of nx1 col vecs
  numUnconstrSteps: number;
  memSize: number;
}

export type Params = IParams;

export interface IParams {
  optStatus: OptStatus;
  /** Constraint weight for exterior point method **/
  weight: number;
  /** Info for unconstrained optimization **/
  UOround: number;
  lastUOstate: number[];
  lastUOenergy: number;
  /** Info for exterior point method **/
  EPround: number;
  lastEPstate: number[];
  lastEPenergy: number;

  lastGradient: number[]; // Value of gradient evaluated at the last state
  lastGradientPreconditioned: number[]; // Value of gradient evaluated at the last state, preconditioned by LBFGS
  // ^ Those two are stored to make them available to Style later

  // For L-BFGS
  lbfgsInfo: LbfgsParams;

  xsVars: VarAD[]; // Computational graph (leaf vars), from the bottom up

  // For energy/gradient compilation
  graphs: GradGraphs;

  functionsCompiled: boolean;

  // Higher-order functions (not yet applied with hyperparameters, in this case, just the EP weight)
  objective: (epWeight: number) => (xs: number[]) => number;
  gradient: (epWeight: number) => (xs: number[]) => number[];

  // Applied with weight (or hyperparameters in general) -- may change with the EP round
  currObjective(xs: number[]): number;
  currGradient(xs: number[]): number[];

  // `xsVars` are all the leaves of the energy graph
  energyGraph: VarAD; // This is the top of the energy graph (parent node)
  constrWeightNode: VarAD; // Handle to node for constraint weight (so it can be set as the weight changes)
  epWeightNode: VarAD; // similar to constrWeightNode

  // Cached versions of compiling each objective and constraint into a function and gradient
  objFnCache: { [k: string]: FnCached }; // Key is the serialized function name, e.g. `contains(A.shape, B.shape)`
  constrFnCache: { [k: string]: FnCached }; // This is kept separate from objfns because objs/constrs may have the same names (=> clashing keys if in same dict)
}

export type FnCached = IFnCached;

// Just the compiled function and its grad, with no weights for EP/constraints/penalties, etc.
export interface IFnCached {
  f(xs: number[]): number;
  gradf(xs: number[]): number[];
}

export type WeightInfo = IWeightInfo;

export interface IWeightInfo {
  constrWeightNode: VarAD; // Constant
  epWeightNode: VarAD; // Changes (input in optimization, but we do NOT need the gradient WRT it)
  constrWeight: number;
  epWeight: number;
}
