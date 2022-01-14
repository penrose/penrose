import {
  INamed,
  IStroke,
  IFill,
  ICenter,
  IRect,
  ICorner,
  IShape,
  IRotate,
} from "types/shapes";
import {
  Canvas,
  sampleColor,
  sampleHeight,
  sampleNoPaint,
  sampleStroke,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
} from "./Samplers";

// not to be confused with IRect... need to rename maybe?
export interface IRectangle
  extends INamed,
    IStroke,
    IFill,
    ICenter,
    IRotate,
    IRect,
    ICorner {}

export const sampleRectangle = (canvas: Canvas): IRectangle => ({
  name: StrV("defaultRectangle"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  width: sampleWidth(canvas),
  height: sampleHeight(canvas),
  cornerRadius: sampleZero(),
  rotation: sampleZero(),
});

export type Rectangle = IShape & { shapeType: "Rectangle" } & IRectangle;

export const makeRectangle = (
  canvas: Canvas,
  properties: Partial<IRectangle>
): Rectangle => ({
  ...sampleRectangle(canvas),
  ...properties,
  shapeType: "Rectangle",
});
