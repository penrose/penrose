import {
  Center,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  String,
  Stroke,
} from "types/shapes";
import {
  boolV,
  Canvas,
  sampleBlack,
  sampleNoPaint,
  sampleVector,
  sampleZero,
  strV,
} from "./Samplers";

export interface EquationProps
  extends Named,
    Fill,
    Stroke,
    Center,
    Rect,
    Rotate,
    String {}

export const sampleEquation = (
  rng: seedrandom.prng,
  canvas: Canvas
): EquationProps => ({
  name: strV("defaultEquation"),
  style: strV(""),
  fillColor: sampleBlack(),
  center: sampleVector(rng, canvas),
  width: sampleZero(),
  height: sampleZero(),
  rotation: sampleZero(),
  strokeColor: sampleNoPaint(),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeDasharray: strV(""),
  string: strV("defaultLabelText"),
  fontSize: strV("12pt"),
  ensureOnCanvas: boolV(true),
});

export type Equation = Shape & { shapeType: "Equation" } & EquationProps;

export const makeEquation = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<EquationProps>
): Equation => ({
  ...sampleEquation(rng, canvas),
  ...properties,
  shapeType: "Equation",
});
