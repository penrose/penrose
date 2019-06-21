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

export const transformValidJSON = (data: any) => ({
  ...data,
  shapesr: data.shapesr.map(([name, shape]: [string, any]) => [
    name,

    pickBy(shape, (k: any) => !k.omit)
  ])
});
