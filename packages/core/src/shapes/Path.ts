import * as ad from "../types/ad.js";
import { Arrow, Fill, Named, ShapeCommon, Stroke } from "../types/shapes.js";
import { PathDataV, StrV } from "../types/value.js";
import {
  black,
  boolV,
  floatV,
  noPaint,
  pathDataV,
  strV,
} from "../utils/Util.js";
import { Canvas, Context } from "./Samplers.js";

export interface PathProps<T> extends Named<T>, Stroke<T>, Fill<T>, Arrow<T> {
  d: PathDataV<T>;
  // `stroke-linecap` only takes effect on <altGlyph>, <path>, <polyline>, <line>, <text>, <textPath>, <tref>, and <tspan>.
  // https://www.w3docs.com/learn-css/stroke-linecap.html
  strokeLinecap: StrV;
}

export const samplePath = (
  context: Context,
  _canvas: Canvas
): PathProps<ad.Num> => ({
  name: strV("defaultPath"),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: black(),
  strokeDasharray: strV(""),
  strokeLinecap: strV("butt"),
  fillColor: noPaint(),
  startArrowheadSize: floatV(1),
  startArrowhead: strV("none"),
  flipStartArrowhead: boolV(false),
  endArrowheadSize: floatV(1),
  endArrowhead: strV("none"),
  d: pathDataV([]),
  ensureOnCanvas: boolV(true),
});

export type Path<T> = ShapeCommon<T> & { shapeType: "Path" } & PathProps<T>;

export const makePath = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PathProps<ad.Num>>
): Path<ad.Num> => ({
  ...samplePath(context, canvas),
  ...properties,
  shapeType: "Path",
  passthrough: new Map(),
});
