import { constrDict } from "./contrib/Constraints.js";
import {
  elasticEnergy,
  equivalued,
  perimeter,
} from "./contrib/CurveConstraints.js";
import { sdfRect } from "./contrib/Functions.js";
import { consecutiveTuples } from "./contrib/Utils.js";
import { input, ops } from "./engine/Autodiff.js";
import { absVal, add, div, neg, pow, sub } from "./engine/AutodiffFunctions.js";
import * as ad from "./types/ad.js";
import { TextMeasurement, measureText } from "./utils/CollectLabels.js";

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

export const eq = (a: ad.Num, b: ad.Num): ad.Num => absVal(sub(a, b));
export const dist = (a: ad.Num[], b: ad.Num[]): ad.Num => ops.vdist(a, b);

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

export const onCanvasPoint = (
  [x, y]: ad.Num[],
  canvas: [ad.Num, ad.Num]
): ad.Num => {
  const [hcw, hch] = [div(canvas[0], 2), div(canvas[1], 2)];
  const xInRange = add(sub(x, neg(hcw)), sub(hcw, x));
  const yInRange = add(sub(y, neg(hch)), sub(hch, y));
  return add(xInRange, yInRange);
};

export const lessThan = (x: ad.Num, y: ad.Num): ad.Num => sub(x, y);

export { numOf, numsOf } from "./contrib/Utils.js";
export { ops, problem } from "./engine/Autodiff.js";
export * from "./engine/AutodiffFunctions.js";
export { Input, Num, Problem } from "./types/ad.js";
export {
  measureText,
  input as scalar,
  sdfRect,
  perimeter,
  consecutiveTuples,
  equivalued,
  elasticEnergy,
  sub,
  pow,
};
