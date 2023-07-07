import { LbfgsParams } from "./bindings/LbfgsParams.js";
import { OptStatus } from "./bindings/OptStatus.js";
import { Params } from "./bindings/Params.js";
import {
  penrose_init,
  penrose_poly_roots,
  penrose_start,
  penrose_step_until,
} from "./build/penrose_optimizer.js";
import "./instance";

penrose_init();

/**
 * Replaces the contents of `v` with the roots of the monic polynomial whose
 * degree is the length of the vector and whose coefficient with a given degree
 * is the element of the vector at that index. Any root with a nonzero imaginary
 * component is replaced with `NaN`.
 */
export const polyRoots: (v: Float64Array) => void = penrose_poly_roots;

// see page 443 of Engineering Optimization, Fourth Edition
/**
 * Given an objective function `f` and some constraint functions `g_j`, where we
 * want to minimize `f` subject to `g_j(x) \leq 0` for all `j`, this function
 * computes the following, where `\lambda` is `weight`:
 *
 * `\phi(x, \lambda) = f(x) + \lambda * \sum_j \max(g_j(x), 0)^2`
 *
 * The gradient with respect to `x` is stored in `grad`.
 *
 * @param x input vector
 * @param weight `\lambda`, the weight of the constraint term
 * @param grad mutable array to store gradient in
 * @returns the augmented objective value `\phi(x, \lambda)`
 */
export type Fn = (
  x: Float64Array,
  weight: number,
  grad: Float64Array
) => number;

/**
 * @returns an initial state for optimizing in `n` dimensions
 */
export const start = (n: number): Params => penrose_start(n);

/**
 * Steps the optimizer until either convergence or `stop` returns `true`.
 * @param f to compute objective, constraints, and gradient
 * @param x mutable array to update at each step
 * @param state initial optimizer state
 * @param stop early stopping criterion
 * @returns optimizer state after stepping
 */
export const stepUntil = (
  f: Fn,
  x: Float64Array,
  state: Params,
  stop: () => boolean
): Params => penrose_step_until(f, x, state, stop);

export type { LbfgsParams, OptStatus, Params };
