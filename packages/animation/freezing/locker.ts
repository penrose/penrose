import { State } from "@penrose/core/dist/types/state";
import * as ad from "@penrose/core/dist/types/ad";
import { constSampler } from "@penrose/core/dist/shapes/Samplers";

export type Locker = (from: ad.Var, to: ad.Var) => void;

export const makeLocker = (fromState: State, toState: State): Locker => {
  const toVarIndexMap = new Map(
    toState.inputs.map((input, i) => [input.handle, i]),
  );
  const fromVarIndexMap = new Map(
    fromState.inputs.map((input, i) => [input.handle, i]),
  );
  const handled: Map<ad.Var, number> = new Map();

  return (from: ad.Var, to: ad.Var) => {
    const toIndex = toVarIndexMap.get(to);
    if (toIndex === undefined) {
      throw new Error("Cannot find Var in toState.inputs");
    }

    const fromIndex = fromVarIndexMap.get(from);
    if (fromIndex === undefined) {
      throw new Error("Cannot find Var in fromState.inputs");
    }

    const val = fromState.varyingValues[fromIndex];

    const previousVal = handled.get(to);

    if (previousVal !== undefined) {
      // this ad.Var has already been handled. As a sanity check, make sure the values remain the same.
      if (previousVal !== val) {
        throw new Error(
          `Conflicting value for variable: new = ${val}, old = ${previousVal}`,
        );
      }
    } else {
      // set initialized value to be fixed, constant sampler
      toState.inputs[toIndex].meta.init = {
        tag: "Sampled",
        sampler: constSampler(val),
      };
      // never optimize
      toState.inputs[toIndex].meta.stages = new Set();
      toState.varyingValues[toIndex] = val;
      to.val = val;

      // change the input mask to never optimize the variable
      toState.constraintSets.forEach(
        (masks) => (masks.inputMask[toIndex] = false),
      );

      // mark this ad.Var as handled
      handled.set(to, val);
    }
  };
};
