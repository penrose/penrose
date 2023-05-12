import * as ad from "../types/ad";
import { Center, Fill, Named, ShapeCommon, Stroke } from "../types/shapes";
import { FloatV } from "../types/value";
import { boolV, floatV, noPaint, strV } from "../utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface CircleProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Center<T> {
  r: FloatV<T>;
}

export const sampleCircle = (
  context: Context,
  canvas: Canvas
): CircleProps<ad.Num> => {
  return {
    name: strV("defaultCircle"),
    style: strV(""),
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
  properties: Partial<CircleProps<ad.Num>>
): Circle<ad.Num> => ({
  ...sampleCircle(context, canvas),
  ...properties,
  shapeType: "Circle",
  passthrough: new Map(),
});
