import * as ad from "../types/ad.js";
import { Arrow, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { StrV, VectorV } from "../types/value.js";
import { boolV, floatV, strV } from "../utils/Util.js";
import { Canvas, Context, sampleColor, sampleVector } from "./Samplers.js";

export interface LineProps<T> extends Named<T>, Stroke<T>, Arrow<T> {
  start: VectorV<T>;
  end: VectorV<T>;
  strokeLinecap: StrV;
}

export const sampleLine = (
  context: Context,
  canvas: Canvas
): LineProps<ad.Num> => ({
  name: strV("defaultLine"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: sampleColor(context),
  strokeDasharray: strV(""),
  startArrowheadSize: floatV(1),
  startArrowhead: strV("none"),
  flipStartArrowhead: boolV(false),
  endArrowheadSize: floatV(1),
  endArrowhead: strV("none"),
  start: sampleVector(context, canvas),
  end: sampleVector(context, canvas),
  strokeLinecap: strV(""),
  ensureOnCanvas: boolV(true),
});

export type Line<T> = ShapeCommon<T> & { shapeType: "Line" } & LineProps<T>;

export const makeLine = (
  context: Context,
  canvas: Canvas,
  properties: Partial<LineProps<ad.Num>>
): Line<ad.Num> => ({
  ...sampleLine(context, canvas),
  ...properties,
  shapeType: "Line",
  passthrough: new Map(),
});
