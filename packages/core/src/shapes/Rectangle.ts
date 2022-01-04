import { bboxFromRect } from "engine/BBox";
import {
  ICenter,
  ICorner,
  IFill,
  INamed,
  IRect,
  IShape,
  IStroke,
  ShapeDef,
} from "./Shapes";
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
    IRect,
    ICorner {}

export const sampleRectangle = (canvas: Canvas): IRectangle => ({
  name: StrV("defaultRectangle"),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  width: sampleWidth(canvas),
  height: sampleHeight(canvas),
  cornerRadius: sampleZero(),
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

export const Rectangle = ShapeDef({
  sampler: sampleRectangle,
  constr: makeRectangle,

  bbox: bboxFromRect,
  isRectlike: true,
});
