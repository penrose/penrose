import * as ad from "../types/ad";
import {
  Fill,
  Named,
  Poly,
  ShapeCommon,
  Stroke,
  Transform,
} from "../types/shapes";
import {
  black,
  boolV,
  floatV,
  id3x3,
  noPaint,
  ptListV,
  strV,
} from "../utils/Util";
import { Canvas, Context } from "./Samplers";

export interface PolylineProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Poly<T>,
    Transform<T> {}

export const samplePolyline = (
  _context: Context,
  _canvas: Canvas
): PolylineProps<ad.Num> => ({
  name: strV("defaultPolyline"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: black(),
  strokeDasharray: strV(""),
  fillColor: noPaint(),
  points: ptListV([
    [0, 0],
    [0, 10],
    [10, 0],
  ]),
  transform: id3x3(),
  ensureOnCanvas: boolV(true),
});

export type Polyline<T> = ShapeCommon<T> & {
  shapeType: "Polyline";
} & PolylineProps<T>;

export const makePolyline = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolylineProps<ad.Num>>
): Polyline<ad.Num> => ({
  ...samplePolyline(context, canvas),
  ...properties,
  shapeType: "Polyline",
  passthrough: new Map(),
});
