import * as ad from "../types/ad.js";
import { Center, Fill, Rotate, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { FloatV } from "../types/value.js";
import { boolV, fakePath, floatV, noPaint, strV } from "../utils/Util.js";
import {
  Canvas,
  Context,
  sampleColor,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers.js";

export interface EllipseProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Rotate<T>,
    Center<T> {
  rx: FloatV<T>;
  ry: FloatV<T>;
}

export const sampleEllipse = (
  context: Context,
  canvas: Canvas,
): EllipseProps<ad.Num> => ({
  name: strV("defaultEllipse"),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(context),
  rotation: floatV(0),
  center: sampleVector(context, canvas),
  rx: sampleWidth(context, canvas),
  ry: sampleHeight(context, canvas),
  ensureOnCanvas: boolV(true),
});

export type Ellipse<T> = ShapeCommon<T> & {
  shapeType: "Ellipse";
} & EllipseProps<T>;

export const makeEllipse = (
  context: Context,
  canvas: Canvas,
  properties: Partial<EllipseProps<ad.Num>>,
): Ellipse<ad.Num> => ({
  ...sampleEllipse(context, canvas),
  ...properties,
  shapeType: "Ellipse",
  passthrough: new Map(),
  path: fakePath("defaultEllipse"),
});
