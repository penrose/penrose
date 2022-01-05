import { INamed, ICenter, IRect, IRotate, IString, IShape } from "types/shapes";
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

export type Equation = IShape & { shapeType: "Equation" } & IEquation;

export const makeEquation = (
  canvas: Canvas,
  properties: Partial<IEquation>
): Equation => ({
  ...sampleEquation(canvas),
  ...properties,
  shapeType: "Equation",
});
