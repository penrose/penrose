import { PenroseState } from "@penrose/core";

export const removeStaging = (
  state: PenroseState
): PenroseState => {
  // "or" together all masks, and replace optStages with only one ("default")
  const combinedMasks = Array.from(state.constraintSets.values()).reduce(
    (acc, mask) => ({
      inputMask: acc.inputMask.map((val, idx) => val || mask.inputMask[idx]),
      objMask: acc.objMask.map((val, idx) => val || mask.objMask[idx]),
      constrMask: acc.constrMask.map((val, idx) => val || mask.constrMask[idx]),
    }),
    {
      inputMask: new Array(state.inputs.length).fill(false),
      objMask: new Array(state.objFns.length).fill(false),
      constrMask: new Array(state.constrFns.length).fill(false),
    }
  );

  state.constraintSets = new Map([["default", combinedMasks]]);
  state.optStages = ["default"];
  state.currentStageIndex = 0;

  return state;
};