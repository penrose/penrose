import { Matrix } from "ml-matrix";
import { Canvas, InputMeta } from "shapes/Samplers";
import * as ad from "types/ad";
import { A } from "./ast";
import { StyleWarning } from "./errors";
import { Shape, ShapeAD } from "./shape";
import { ConstrFn, ObjFn } from "./style";
import { WithContext } from "./styleSemantics";
import { FloatV } from "./value";

export type ShapeFn = (xs: number[]) => Shape[];

/**
 * The diagram state
 */
export interface State {
  warnings: StyleWarning[];
  variation: string;
  objFns: Fn[];
  constrFns: Fn[];
  varyingValues: number[];
  inputs: InputMeta[]; // same length as `varyingValues`
  labelCache: LabelCache;
  shapes: ShapeAD[];
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

export type LabelCache = Map<string, LabelData>;

/**
 * Generic export interface for constraint or objective functions
 */
export interface Fn {
  ast: WithContext<ObjFn<A> | ConstrFn<A>>;
  output: ad.Num;
}

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
  lastUOstate?: number[];
  lastUOenergy?: number;
  lastObjEnergies?: number[];
  lastConstrEnergies?: number[];

  /** Info for exterior point method **/
  EPround: number;
  lastEPstate?: number[];
  lastEPenergy?: number;

  lastGradient: number[]; // Value of gradient evaluated at the last state
  lastGradientPreconditioned: number[]; // Value of gradient evaluated at the last state, preconditioned by LBFGS
  // ^ Those two are stored to make them available to Style later

  // For L-BFGS
  lbfgsInfo: LbfgsParams;

  // Higher-order functions (not yet applied with hyperparameters, in this case, just the EP weight)
  objectiveAndGradient: (epWeight: number) => (xs: number[]) => FnEvaled;

  // Applied with weight (or hyperparameters in general) -- may change with the EP round
  currObjectiveAndGradient(xs: number[]): FnEvaled;

  energyGraph: ad.Num; // This is the top of the energy graph (parent node)
}

// Just the compiled function and its grad, with no weights for EP/constraints/penalties, etc.
export type FnCached = (xs: number[]) => FnEvaled;
