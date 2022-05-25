import {
  Center,
  Corner,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  Stroke,
} from "types/shapes";
import { boolV, floatV, noPaint, strV } from "utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface RectangleProps
  extends Named,
    Stroke,
    Fill,
    Center,
    Rotate,
    Rect,
    Corner {}

export const sampleRectangle = (
  context: Context,
  canvas: Canvas
): RectangleProps => ({
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

export type Rectangle = Shape & { shapeType: "Rectangle" } & RectangleProps;

export const makeRectangle = (
  context: Context,
  canvas: Canvas,
  properties: Partial<RectangleProps>
): Rectangle => ({
  ...sampleRectangle(context, canvas),
  ...properties,
  shapeType: "Rectangle",
});
