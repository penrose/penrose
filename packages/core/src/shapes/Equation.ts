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
import { black, boolV, floatV, noPaint, strV } from "utils/Util";
import { Canvas, Context, sampleVector } from "./Samplers";

export interface EquationProps
  extends Named,
    Fill,
    Stroke,
    Center,
    Rect,
    Rotate,
    String {}

export const sampleEquation = (
  context: Context,
  canvas: Canvas
): EquationProps => ({
  name: strV("defaultEquation"),
  style: strV(""),
  fillColor: black(),
  center: sampleVector(context, canvas),
  width: floatV(context.makeInput({ pending: 0 })),
  height: floatV(context.makeInput({ pending: 0 })),
  rotation: floatV(0),
  string: strV("defaultLabelText"),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fontSize: strV("12pt"),
  ensureOnCanvas: boolV(true),
});

export type Equation = Shape & { shapeType: "Equation" } & EquationProps;

export const makeEquation = (
  context: Context,
  canvas: Canvas,
  properties: Partial<EquationProps>
): Equation => ({
  ...sampleEquation(context, canvas),
  ...properties,
  shapeType: "Equation",
});
