import * as ad from "../types/ad";
import {
  Center,
  Corner,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  Stroke,
} from "../types/shapes";
import { boolV, floatV, noPaint, strV } from "../utils/Util";
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
    Rotate<T>,
    Rect<T>,
    Corner<T> {}

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
  rotation: floatV(0),
  ensureOnCanvas: boolV(true),
});

export type Rectangle<T> = Shape<T> & {
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
});
