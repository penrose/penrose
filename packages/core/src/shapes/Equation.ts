import * as ad from "../types/ad";
import {
  Center,
  Fill,
  Named,
  Rect,
  Rotate,
  ShapeCommon,
  String,
} from "../types/shapes";
import { black, boolV, floatV, strV, vectorV } from "../utils/Util";
import { Canvas, Context, uniform } from "./Samplers";

export interface EquationProps<T>
  extends Named<T>,
    Fill<T>,
    Center<T>,
    Rect<T>,
    Rotate<T>,
    String<T> {}

export const sampleEquation = (
  context: Context,
  canvas: Canvas
): EquationProps<ad.Num> => ({
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

export type Equation<T> = ShapeCommon<T> & {
  shapeType: "Equation";
} & EquationProps<T>;

export const makeEquation = (
  context: Context,
  canvas: Canvas,
  properties: Partial<EquationProps<ad.Num>>
): Equation<ad.Num> => ({
  ...sampleEquation(context, canvas),
  ...properties,
  shapeType: "Equation",
  passthrough: new Map(),
});
