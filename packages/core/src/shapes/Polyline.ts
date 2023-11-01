import { scalar } from "@tensorflow/tfjs";
import { StrV } from "src/types/value.js";
import * as ad from "../types/ad.js";
import {
  Fill,
  Named,
  Poly,
  Scale,
  ShapeCommon,
  Stroke,
} from "../types/shapes.js";
import { black, boolV, floatV, noPaint, ptListV, strV } from "../utils/Util.js";
import { Canvas, Context } from "./Samplers.js";

export interface PolylineProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Scale<T>,
    Poly<T> {
  // `stroke-linecap` only takes effect on <altGlyph>, <path>, <polyline>, <line>, <text>, <textPath>, <tref>, and <tspan>.
  // https://www.w3docs.com/learn-css/stroke-linecap.html
  strokeLinecap: StrV;
}

export const samplePolyline = (
  _context: Context,
  _canvas: Canvas,
): PolylineProps<ad.Num> => ({
  name: strV("defaultPolyline"),
  strokeWidth: floatV(scalar(1)),
  strokeStyle: strV("solid"),
  strokeColor: black(),
  strokeDasharray: strV(""),
  strokeLinecap: strV("butt"),
  fillColor: noPaint(),
  scale: floatV(scalar(1)),
  points: ptListV([
    [scalar(0), scalar(0)],
    [scalar(0), scalar(10)],
    [scalar(10), scalar(0)],
  ]),
  ensureOnCanvas: boolV(true),
});

export type Polyline<T> = ShapeCommon<T> & {
  shapeType: "Polyline";
} & PolylineProps<T>;

export const makePolyline = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolylineProps<ad.Num>>,
): Polyline<ad.Num> => ({
  ...samplePolyline(context, canvas),
  ...properties,
  shapeType: "Polyline",
  passthrough: new Map(),
});
