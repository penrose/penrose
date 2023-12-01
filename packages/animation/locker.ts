import { State } from "@penrose/core/dist/types/state";
import * as ad from "@penrose/core/dist/types/ad";
import { constSampler } from "@penrose/core/dist/shapes/Samplers";

export type Locker = (from: ad.Var, to: ad.Var) => void;

export const makeLockerInState = (fromState: State, toState: State): Locker => {
  const toVarIndexMap = new Map(
    toState.inputs.map((input, i) => [input.handle, i]),
  );
  const fromVarIndexMap = new Map(
    fromState.inputs.map((input, i) => [input.handle, i]),
  );
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
  };
};
