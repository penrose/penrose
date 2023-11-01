import * as tf from "@tensorflow/tfjs";

export type Expr = Bool | Num;

export type Bool = tf.Scalar;

export type Num = tf.Scalar;

export type Var = tf.Scalar & tf.Variable;

//#region Types for compiled autodiff graph

/**
 * A structure used to collect the various outputs of a `Gradient` function.
 * This is generic in the concrete number type, because it can also be useful in
 * situations where the elements are, for instance, computation graph nodes.
 */
export interface Outputs<T> {
  /** Derivatives of primary output with respect to inputs. */
  gradient: Map<Var, T>;
  /** Primary output. */
  primary: T;
  /** Secondary outputs. */
  secondary: T[];
}

export type Compiled = (
  inputs: (x: Var) => number,
  mask?: boolean[],
) => Outputs<number>;

export interface OptOutputs {
  phi: number; // see `Fn` from `engine/Optimizer`
  objectives: number[];
  constraints: number[];
}

export interface Masks {
  inputMask: boolean[];
  objMask: boolean[];
  constrMask: boolean[];
}

// you can think of the `Fn` type from `engine/Optimizer` as this type
// partially applied with `masks` and projecting out the `phi` field
export type Gradient = (
  masks: Masks,
  inputs: Float64Array,
  weight: number,
  grad: Float64Array,
) => OptOutputs;

export interface Description {
  /** zero by default */
  objective?: Num;
  /** empty by default */
  constraints?: Num[];
}

export interface Options {
  /** always false by default */
  until?(): boolean;
}

export interface Run {
  converged: boolean;
  /** doesn't include frozen */
  vals: Map<Var, number>;
  /** returns a new `Run`, leaving this one unchanged */
  run(opts: Options): Run;
}

export interface Config {
  /** uses `val` field by default */
  vals?(x: Var): number;
  /** always false by default */
  freeze?(x: Var): boolean;
}

export interface Problem {
  start(config: Config): Run;
}

//#endregion

//#region Types for generalizing our system autodiff

export type Pt2 = [Num, Num];

export const isPt2 = (vec: Num[]): vec is Pt2 => vec.length === 2;

//#endregion
