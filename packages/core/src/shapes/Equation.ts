import {
  ICenter,
  IFill,
  INamed,
  IRect,
  IRotate,
  IShape,
  IString,
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

export const sampleEquation = (
  rng: seedrandom.prng,
  canvas: Canvas
): IEquation => ({
  name: StrV("defaultEquation"),
  style: StrV(""),
  fillColor: sampleBlack(),
  center: sampleVector(rng, canvas),
  width: sampleZero(),
  height: sampleZero(),
  rotation: sampleZero(),
  string: StrV("defaultLabelText"),
  fontSize: StrV("12pt"),
  ensureOnCanvas: BoolV(true),
});

export type Equation = IShape & { shapeType: "Equation" } & IEquation;

export const makeEquation = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IEquation>
): Equation => ({
  ...sampleEquation(rng, canvas),
  ...properties,
  shapeType: "Equation",
});
