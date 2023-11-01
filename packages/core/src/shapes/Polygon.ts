import { scalar } from "@tensorflow/tfjs";
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
  _canvas: Canvas,
): PolygonProps<ad.Num> => ({
  name: strV("defaultPolygon"),
  strokeWidth: floatV(scalar(0)),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(context),
  scale: floatV(scalar(1)),
  points: ptListV([
    [scalar(0), scalar(0)],
    [scalar(0), scalar(10)],
    [scalar(10), scalar(0)],
  ]),
  ensureOnCanvas: boolV(true),
});

export type Polygon<T> = ShapeCommon<T> & {
  shapeType: "Polygon";
} & PolygonProps<T>;

export const makePolygon = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolygonProps<ad.Num>>,
): Polygon<ad.Num> => ({
  ...samplePolygon(context, canvas),
  ...properties,
  shapeType: "Polygon",
  passthrough: new Map(),
});
