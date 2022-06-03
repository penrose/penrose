import { Fill, Named, Poly, Scale, Shape, Stroke } from "types/shapes";
import { boolV, floatV, noPaint, ptListV, strV } from "utils/Util";
import { Canvas, Context, sampleColor } from "./Samplers";

export interface PolygonProps extends Named, Stroke, Fill, Scale, Poly {}

export const samplePolygon = (
  context: Context,
  _canvas: Canvas
): PolygonProps => ({
  name: strV("defaultPolygon"),
  style: strV(""),
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

export type Polygon = Shape & { shapeType: "Polygon" } & PolygonProps;

export const makePolygon = (
  context: Context,
  canvas: Canvas,
  properties: Partial<PolygonProps>
): Polygon => ({
  ...samplePolygon(context, canvas),
  ...properties,
  shapeType: "Polygon",
});
