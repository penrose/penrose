// import memoize from "fast-memoize";

import { pickBy } from "lodash";
// const memoized = (p: any) =>
// memoize((...args: any) => JSON.stringify(p(...args)));
export const Step = (steps: number, data: any) => ({
  tag: "Step",
  contents: [steps, transformValidJSON(data)]
});

export const Resample = (samples: number, data: any) => ({
  tag: "Resample",
  contents: [samples, transformValidJSON(data)]
});

export const CompileTrio = (
  substance: string,
  style: string,
  element: string
) => ({
  tag: "CompileTrio",
  contents: [substance, style, element]
});

export const ReconcileNext = (
  substance: string,
  style: string,
  element: string,
  state: any
) => ({
  tag: "ReconcileNext",
  contents: [transformValidJSON(state), substance, style, element]
});

export const GetVersion = () => ({ tag: "GetVersion" });

export const GetEnv = (substance: string, element: string) => ({
  tag: "GetEnv",
  contents: [substance, element]
});

export const transformValidJSON = (data: any) => ({
  ...data,
  shapesr: data.shapesr.map(([name, shape]: [string, any]) => [
    name,

    pickBy(shape, (k: any) => !k.omit)
  ])
});
export const converged = (state: any) =>
  state.paramsr && state.paramsr.optStatus.tag === "EPConverged";
export const initial = (state: any) =>
  state.paramsr && state.paramsr.optStatus.tag === "NewIter";

export const running = (state: any) =>
  state.paramsr &&
  (state.paramsr.optStatus.tag === "UnconstrainedConverged" ||
    state.paramsr.optStatus.tag === "UnconstrainedRunning");
