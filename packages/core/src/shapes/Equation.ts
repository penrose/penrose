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
  BoolV,
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
  style: StrV(""),
  fillColor: sampleBlack(),
  center: sampleVector(canvas),
  width: sampleZero(),
  height: sampleZero(),
  rotation: sampleZero(),
  string: StrV("defaultLabelText"),
  fontSize: StrV("12pt"),
  ensureOnCanvas: BoolV(true),
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
