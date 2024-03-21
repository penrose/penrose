import * as ad from "../types/ad.js";
import { Arrow, Fill, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { StrV, VectorV } from "../types/value.js";
import { black, boolV, constStrV, floatV, noPaint } from "../utils/Util.js";
import { Canvas, Context, sampleVector } from "./Samplers.js";

export interface LineProps<T> extends Named<T>, Stroke<T>, Arrow<T>, Fill<T> {
  start: VectorV<T>;
  end: VectorV<T>;
  // `stroke-linecap` only takes effect on <altGlyph>, <path>, <polyline>, <line>, <text>, <textPath>, <tref>, and <tspan>.
  // https://www.w3docs.com/learn-css/stroke-linecap.html
  strokeLinecap: StrV<T>;
}

export const sampleLine = (
  context: Context,
  canvas: Canvas,
): LineProps<ad.Num> => ({
  name: constStrV("defaultLine"),
  strokeWidth: floatV(1),
  strokeStyle: constStrV("solid"),
  strokeColor: black(),
  strokeDasharray: constStrV(""),
  startArrowheadSize: floatV(1),
  startArrowhead: constStrV("none"),
  flipStartArrowhead: boolV(false),
  endArrowheadSize: floatV(1),
  endArrowhead: constStrV("none"),
  start: sampleVector(context, canvas),
  end: sampleVector(context, canvas),
  strokeLinecap: constStrV(""),
  ensureOnCanvas: boolV(true),
  fillColor: noPaint(),
});

export type Line<T> = ShapeCommon<T> & { shapeType: "Line" } & LineProps<T>;

export const makeLine = (
  context: Context,
  canvas: Canvas,
  properties: Partial<LineProps<ad.Num>>,
): Line<ad.Num> => ({
  ...sampleLine(context, canvas),
  ...properties,
  shapeType: "Line",
  passthrough: new Map(),
});
