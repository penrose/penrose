import {
  INamed,
  ICenter,
  IRect,
  IRotate,
  IString,
  IShape,
  IFill,
} from "types/shapes";
import {
  Canvas,
  sampleBlack,
  sampleVector,
  sampleZero,
  StrV,
} from "./Samplers";

export interface IEquation
  extends INamed,
    IFill,
    ICenter,
    IRect,
    IRotate,
    IString {}

export const sampleEquation = (canvas: Canvas): IEquation => ({
  name: StrV("defaultEquation"),
  fillColor: sampleBlack(),
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
