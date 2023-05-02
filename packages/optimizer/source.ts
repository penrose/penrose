import { LbfgsParams } from "./bindings/LbfgsParams";
import { OptState } from "./bindings/OptState";
import { OptStatus } from "./bindings/OptStatus";
import { Params } from "./bindings/Params";
import {
  penrose_gen_opt_problem,
  penrose_init,
  penrose_poly_roots,
  penrose_step,
} from "./build/penrose_optimizer";
import "./instance";

penrose_init();

/**
 * Replaces the contents of the input vector with the roots of the monic
 * polynomial whose degree is the length of the vector and whose coefficient
 * with a given degree is the element of the vector at that index. Any root with
 * a nonzero imaginary component is replaced with `NaN`.
 */
export const polyRoots = penrose_poly_roots;

const bools = (a: boolean[]) => new Int32Array(a.map((x) => (x ? 1 : 0)));

/**
 * A structure used to collect the various outputs of a `Gradient` function.
 * This is generic in the concrete number type, because it can also be useful in
 * situations where the elements are, for instance, computation graph nodes.
 */
export interface Outputs<T> {
  /** Derivatives of primary output with respect to inputs. */
  gradient: T[];
  /** Primary output. */
  primary: T;
  /** Secondary outputs. */
  secondary: T[];
}

/**
 * Computes the sum of several functions on `inputs`, where the corresponding
 * element of `mask` determines whether that function contributes to the sum.
 */
export type GradFn = (
  inputs: Float64Array,
  mask: Int32Array,
  gradient: Float64Array,
  secondary: Float64Array
) => number;

export class Gradient {
  f: GradFn;
  numAddends: number;
  numSecondary: number;

  constructor(f: GradFn, numAddends: number, numSecondary: number) {
    this.f = f;
    this.numAddends = numAddends;
    this.numSecondary = numSecondary;
  }

  /**
   * @param inputs to the function
   * @param mask which addends to include
   * @returns the `primary` output, its `gradient`, and any `secondary` outputs
   */
  call(inputs: number[], mask?: boolean[]): Outputs<number> {
    const maskNums = new Int32Array(this.numAddends);
    for (let i = 0; i < this.numAddends; i++)
      maskNums[i] = mask !== undefined && i in mask && !mask[i] ? 0 : 1;
    const gradient = new Float64Array(inputs.length);
    const secondary = new Float64Array(this.numSecondary);
    const primary = this.f(
      new Float64Array(inputs),
      maskNums,
      gradient,
      secondary
    );
    return {
      gradient: Array.from(gradient),
      primary,
      secondary: Array.from(secondary),
    };
  }
}

/**
 * @param state current state
 * @param steps how many to take
 * @returns updated state
 */
export const step = (state: OptState, f: GradFn, steps: number): OptState =>
  penrose_step(state, f, steps);

/**
 * @param gradMask whether each varying value index should be optimized
 * @param objMask whether each objective should be optimized
 * @param constrMask whether each constraint should be optimized
 * @returns initial optimization parameters
 */
export const genOptProblem = (
  gradMask: boolean[],
  objMask: boolean[],
  constrMask: boolean[]
): Params =>
  penrose_gen_opt_problem(bools(gradMask), bools(objMask), bools(constrMask));

export type { LbfgsParams, OptState, OptStatus, Params };
