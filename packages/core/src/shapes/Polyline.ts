import { Fill, Named, Poly, Scale, Shape, Stroke } from "types/shapes";
import { black, boolV, floatV, noPaint, ptListV, strV } from "utils/Util";
import { Canvas, Context } from "./Samplers";

export interface PolylineProps extends Named, Stroke, Fill, Scale, Poly {}

export const samplePolyline = (
  _context: Context,
  _canvas: Canvas
): PolylineProps => ({
  name: strV("defaultPolyline"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: black(),
  strokeDasharray: strV(""),
  fillColor: noPaint(),
  scale: floatV(1),
  points: ptListV([
    [0, 0],
    [0, 10],
    [10, 0],
  ]),
  ensureOnCanvas: boolV(true),
});

export type Polyline = Shape & { shapeType: "Polyline" } & PolylineProps;

export const makePolyline = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolylineProps>
): Polyline => ({
  ...samplePolyline(context, canvas),
  ...properties,
  shapeType: "Polyline",
});
