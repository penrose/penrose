// import memoize from "fast-memoize";

import { encodeState } from "./Evaluator";
// const memoized = (p: any) =>
// memoize((...args: any) => JSON.stringify(p(...args)));
export const Step = (steps: number, data: any) => ({
  tag: "Step",
  contents: [steps, encodeState(data)],
});

export const StepUntilConvergence = (data: any) => ({
  tag: "StepUntilConvergence",
  contents: encodeState(data),
});

export const EnergyValues = (data: any) => ({
  tag: "EnergyValues",
  contents: encodeState(data),
});

export const Resample = (samples: number, data: any) => ({
  tag: "Resample",
  contents: [samples, encodeState(data)],
});

export const CompileTrio = (
  substance: string,
  style: string,
  element: string
) => ({
  tag: "CompileTrio",
  contents: [substance, style, element],
});

export const ReconcileNext = (
  substance: string,
  style: string,
  element: string,
  state: any
) => ({
  tag: "ReconcileNext",
  contents: [encodeState(state), substance, style, element],
});

export const GetVersion = () => ({ tag: "GetVersion" });

export const GetEnv = (substance: string, element: string) => ({
  tag: "GetEnv",
  contents: [substance, element],
});

export const converged = (state: State) =>
  state.paramsr.optStatus.tag === "EPConverged";
export const initial = (state: State) =>
  state.paramsr.optStatus.tag === "NewIter";

export const running = (state: State) =>
  state.paramsr.optStatus.tag === "UnconstrainedRunning" ||
  state.paramsr.optStatus.tag === "UnconstrainedConverged";
