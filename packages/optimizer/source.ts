import { LbfgsParams } from "./bindings/LbfgsParams";
import { OptState } from "./bindings/OptState";
import { OptStatus } from "./bindings/OptStatus";
import { Outputs } from "./bindings/Outputs";
import { Params } from "./bindings/Params";
import {
  penrose_gen_opt_problem,
  penrose_init,
  penrose_step,
} from "./build/penrose_optimizer";

penrose_init();

const bools = (a: boolean[]) => new Int32Array(a.map((x) => (x ? 1 : 0)));

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

/**
 * Computes the sum of several functions on `inputs`, where the corresponding
 * element of `mask` determines whether that function contributes to the sum.
 */
export type Gradient = (inputs: number[], mask: boolean[]) => Outputs<number>;

/**
 * @param state current state
 * @param steps how many to take
 * @returns updated state
 */
export const step = (state: OptState, f: Gradient, steps: number): OptState =>
  penrose_step(state, f, steps);

export type { LbfgsParams, OptState, OptStatus, Outputs, Params };
