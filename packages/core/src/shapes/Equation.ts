import { bboxFromRectlike } from "engine/BBox";
import { ICenter, INamed, IRect, IRotate, IShape, IString } from "./Shapes";
import { Canvas, sampleVector, sampleZero, StrV } from "./Samplers";

export interface IEquation extends INamed, ICenter, IRect, IRotate, IString {}

export const sampleEquation = (canvas: Canvas): IEquation => ({
  name: StrV("defaultEquation"),
  center: sampleVector(canvas),
  width: sampleZero(),
  height: sampleZero(),
  rotation: sampleZero(),
  string: StrV("defaultLabelText"),
});

export type Equation = IShape & IEquation;

export const Equation = (
  canvas: Canvas,
  properties: Partial<IEquation>
): Equation => ({
  ...sampleEquation(canvas),
  ...properties,
  shapeType: "Equation",
  bbox: function () {
    return bboxFromRectlike(this); // assumes w and h correspond to string
  },
});
