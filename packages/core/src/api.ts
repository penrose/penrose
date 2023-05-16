import { start, stepUntil } from "@penrose/optimizer";
import { constrDict } from "./contrib/Constraints";
import { sdfRect } from "./contrib/Functions";
import {
  fns,
  genBytes,
  getExport,
  input,
  makeImports,
  makeMeta,
  ops,
  primaryGraph,
} from "./engine/Autodiff";
import { add, div, mul, neg, sub } from "./engine/AutodiffFunctions";
import * as ad from "./types/ad";
import { measureText, TextMeasurement } from "./utils/CollectLabels";

//#region Variable-level API

export interface Problem {
  minimize: () => void;
}

export const problem = async (
  objective: ad.Num,
  constraints: ad.Num[]
): Promise<Problem> => {
  // `inputs` keep track of all the inputs across all constraints and objective, and the weight
  const inputs = new Map<ad.Input, number>();
  // add in the weight
  const lambda = input(42);
  // make the comp graphs for obj and constrs
  const getKey = (x: ad.Input): number => {
    if (x === lambda) return 0;
    else if (inputs.has(x)) return inputs.get(x)!;
    else {
      const idx = inputs.size + 1;
      inputs.set(x, idx);
      return idx;
    }
  };
  const obj = primaryGraph(objective, getKey);
  const constrs = constraints.map((x) =>
    primaryGraph(mul(lambda, fns.toPenalty(x)), getKey)
  );
  const graphs = [obj, ...constrs];
  const meta = makeMeta(graphs);
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(genBytes(graphs)),
    makeImports(meta.memory)
  );
  const f = getExport(meta, instance);
  const n = inputs.size;
  const params = start(n);
  return {
    minimize: () => {
      // allocate a new array to store inputs
      const xs = new Float64Array(n);
      // populate inputs with initial values from `val`
      for (const [input, index] of inputs) {
        // skip the weight input
        xs[index - 1] = input.val;
      }
      // call the optimizer
      stepUntil(
        (
          inputs: Float64Array /*read-only*/,
          weight: number,
          grad: Float64Array /*write-only*/
        ): number => {
          if (inputs.length !== n)
            throw Error(`expected ${n} inputs, got ${inputs.length}`);
          if (grad.length !== n)
            throw Error(
              `expected ${n} inputs, got gradient with length ${grad.length}`
            );
          meta.arrInputs.set(inputs.subarray(0, n), 1);
          // the last input is the weight
          meta.arrInputs[0] = weight;
          // we don't use masks, so they are set to 1
          meta.arrMask.fill(1);
          meta.arrGrad.fill(0);
          meta.arrSecondary.fill(0);
          const phi = f();
          grad.set(meta.arrGrad.subarray(1, meta.numInputs));
          return phi;
        },
        xs,
        params,
        (): boolean => false
      );
      // put the optimized values back to the inputs
      for (const [input, index] of inputs) {
        input.val = xs[index - 1];
      }
    },
  };
};

//#endregion

interface Rect {
  center: ad.Num[];
  width: ad.Num;
  height: ad.Num;
}

export const textBBox = (
  measure: TextMeasurement,
  x: ad.Num,
  y: ad.Num
): Rect => {
  const { height, width, actualDescent } = measure;
  y = add(y, sub(div(height, 2), actualDescent));
  return {
    center: [x, y],
    width,
    height,
  };
};

export const eq = (a: ad.Num, b: ad.Num): ad.Num => sub(a, b);
export const dist = (a: ad.Num[], b: ad.Num[]): ad.Num =>
  ops.vnorm(ops.vsub(a, b));

export const onCanvasRect = (
  [canvasWidth, canvasHeight]: [ad.Num, ad.Num],
  { center: [x, y], width, height }: Rect
): ad.Num => {
  const canvasXRange: [ad.Num, ad.Num] = [0, canvasWidth];
  const canvasYRange: [ad.Num, ad.Num] = [0, canvasHeight];
  const hw = div(width, 2);
  const hh = div(height, 2);
  return add(
    constrDict.contains1D.body(canvasXRange, [sub(x, hw), add(x, hw)]),
    constrDict.contains1D.body(canvasYRange, [sub(y, hh), add(y, hh)])
  );
};

export const onCanvasPoint = ([x, y]: ad.Num[], canvas: [ad.Num, ad.Num]) => {
  const [hcw, hch] = [div(canvas[0], 2), div(canvas[1], 2)];
  const xInRange = add(sub(x, neg(hcw)), sub(hcw, x));
  const yInRange = add(sub(y, neg(hch)), sub(hch, y));
  return add(xInRange, yInRange);
};

export { measureText, input as scalar, sdfRect };
