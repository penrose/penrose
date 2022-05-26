import { Matrix } from "ml-matrix";
import { Canvas } from "shapes/Samplers";
import * as ad from "types/ad";
import { A } from "./ast";
import { Shape } from "./shape";
import { Expr } from "./style";
import { Translation } from "./styleSemantics";
import { FloatV } from "./value";

export type ShapeFn = (xs: number[]) => Shape[];

/**
 * The diagram state
 */
export interface State {
  objFns: Fn[];
  constrFns: Fn[];
  varyingValues: number[];
  translation: Translation;
  labelCache: LabelCache;
  canvas: Canvas;
  computeShapes: ShapeFn;
  params: Params;
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
