import * as ad from "../types/ad.js";
import {
  Fill,
  Named,
  Poly,
  Scale,
  ShapeCommon,
  Stroke,
} from "../types/shapes.js";
import { boolV, floatV, noPaint, ptListV, strV } from "../utils/Util.js";
import { Canvas, Context, sampleColor } from "./Samplers.js";

export interface PolygonProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Scale<T>,
    Poly<T> {}

export const samplePolygon = (
  context: Context,
  _canvas: Canvas
): PolygonProps<ad.Num> => ({
  name: strV("defaultPolygon"),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(context),
  scale: floatV(1),
  points: ptListV([
    [0, 0],
    [0, 10],
    [10, 0],
  ]),
  ensureOnCanvas: boolV(true),
});

export type Polygon<T> = ShapeCommon<T> & {
  shapeType: "Polygon";
} & PolygonProps<T>;

export const makePolygon = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolygonProps<ad.Num>>
): Polygon<ad.Num> => ({
  ...samplePolygon(context, canvas),
  ...properties,
  shapeType: "Polygon",
  passthrough: new Map(),
});
