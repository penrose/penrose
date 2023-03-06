import * as ad from "../types/ad";
import { Center, Fill, Named, Shape, Stroke } from "../types/shapes";
import { FloatV } from "../types/value";
import { boolV, floatV, noPaint, strV } from "../utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers";

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
  canvas: Canvas
): EllipseProps<ad.Num> => ({
  name: strV("defaultEllipse"),
  style: strV(""),
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

export type Ellipse<T> = Shape<T> & { shapeType: "Ellipse" } & EllipseProps<T>;

export const makeEllipse = (
  context: Context,
  canvas: Canvas,
  properties: Partial<EllipseProps<ad.Num>>
): Ellipse<ad.Num> => ({
  ...sampleEllipse(context, canvas),
  ...properties,
  shapeType: "Ellipse",
});
