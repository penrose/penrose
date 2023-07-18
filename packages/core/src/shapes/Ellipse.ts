import * as ad from "../types/ad.js";
import { Center, Fill, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { FloatV } from "../types/value.js";
import { boolV, floatV, noPaint, strV } from "../utils/Util.js";
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
});
