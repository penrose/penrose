import {
  Center,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  String,
} from "../types/shapes";
import { black, boolV, floatV, strV, vectorV } from "../utils/Util";
import { Canvas, Context, uniform } from "./Samplers";

export interface EquationProps
  extends Named,
    Fill,
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
  center: vectorV([
    context.makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.xRange) },
      stages: "All",
    }),
    context.makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.yRange) },
      stages: "All",
    }),
  ]),
  width: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    })
  ),
  height: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    })
  ),
  rotation: floatV(0),
  string: strV("defaultLabelText"),
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
