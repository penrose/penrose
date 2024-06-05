import * as ad from "../types/ad.js";
import { Center, Fill, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { FloatV } from "../types/value.js";
import { boolV, floatV, noPaint, strV } from "../utils/Util.js";
import {
  Canvas,
  Context,
  sampleColor,
  sampleVector,
  sampleWidth,
} from "./Samplers.js";

export interface CircleProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Center<T> {
  r: FloatV<T>;
}

export const sampleCircle = (
  context: Context,
  canvas: Canvas,
): CircleProps<ad.Num> => {
  return {
    name: strV("defaultCircle"),
    strokeWidth: floatV(0),
    strokeStyle: strV("solid"),
    strokeColor: noPaint(),
    strokeDasharray: strV(""),
    fillColor: sampleColor(context),
    center: sampleVector(context, canvas),
    r: sampleWidth(context, canvas),
    ensureOnCanvas: boolV(true),
  };
};

export type Circle<T> = ShapeCommon<T> & {
  shapeType: "Circle";
} & CircleProps<T>;

export const makeCircle = (
  context: Context,
  canvas: Canvas,
  properties: Partial<CircleProps<ad.Num>>,
): Circle<ad.Num> => ({
  ...sampleCircle(context, canvas),
  ...properties,
  shapeType: "Circle",
  passthrough: new Map(),
});
