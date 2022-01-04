import { bboxFromRect } from "engine/BBox";
import {
  ICenter,
  ICorner,
  IFill,
  INamed,
  IRect,
  IShape,
  IStroke,
  weaken,
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

export type Rectangle = IShape & IRectangle;

export const Rectangle = {
  sampler: weaken(sampleRectangle),
  constr: (canvas: Canvas, properties: Partial<IRectangle>): Rectangle => ({
    ...sampleRectangle(canvas),
    ...properties,
    shapeType: "Rectangle",
    bbox: function () {
      return bboxFromRect(this);
    },
  }),
};
