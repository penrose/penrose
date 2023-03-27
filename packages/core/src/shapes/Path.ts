import * as ad from "../types/ad";
import { Arrow, Fill, Named, ShapeCommon, Stroke } from "../types/shapes";
import { PathDataV } from "../types/value";
import { boolV, floatV, noPaint, pathDataV, strV } from "../utils/Util";
import { Canvas, Context, sampleColor } from "./Samplers";

export interface PathProps<T> extends Named<T>, Stroke<T>, Fill<T>, Arrow<T> {
  d: PathDataV<T>;
}

export const samplePath = (
  context: Context,
  _canvas: Canvas
): PathProps<ad.Num> => ({
  name: strV("defaultPath"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: sampleColor(context),
  strokeDasharray: strV(""),
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
