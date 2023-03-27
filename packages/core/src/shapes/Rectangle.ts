import * as ad from "../types/ad";
import {
  Center,
  Corner,
  Fill,
  Named,
  Rect,
  ShapeCommon,
  Stroke,
  Transform,
} from "../types/shapes";
import { boolV, floatV, id3x3, noPaint, strV } from "../utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface RectangleProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Center<T>,
    Rect<T>,
    Corner<T>,
    Transform<T> {}

export const sampleRectangle = (
  context: Context,
  canvas: Canvas
): RectangleProps<ad.Num> => ({
  name: strV("defaultRectangle"),
  style: strV(""),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(context),
  center: sampleVector(context, canvas),
  width: sampleWidth(context, canvas),
  height: sampleHeight(context, canvas),
  cornerRadius: floatV(0),
  transform: id3x3(),
  ensureOnCanvas: boolV(true),
});

export type Rectangle<T> = ShapeCommon<T> & {
  shapeType: "Rectangle";
} & RectangleProps<T>;

export const makeRectangle = (
  context: Context,
  canvas: Canvas,
  properties: Partial<RectangleProps<ad.Num>>
): Rectangle<ad.Num> => ({
  ...sampleRectangle(context, canvas),
  ...properties,
  shapeType: "Rectangle",
  passthrough: new Map(),
});
