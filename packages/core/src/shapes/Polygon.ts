import { Fill, Named, Poly, Scale, Shape, Stroke } from "types/shapes";
import { boolV, floatV, ptListV, strV } from "utils/Util";
import { Canvas, sampleColor, sampleNoPaint, sampleZero } from "./Samplers";

export interface PolygonProps extends Named, Stroke, Fill, Scale, Poly {}

export const samplePolygon = (
  rng: seedrandom.prng,
  _canvas: Canvas
): PolygonProps => ({
  name: strV("defaultPolygon"),
  style: strV(""),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(rng),
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
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<PolygonProps>
): Polygon => ({
  ...samplePolygon(rng, canvas),
  ...properties,
  shapeType: "Polygon",
});
