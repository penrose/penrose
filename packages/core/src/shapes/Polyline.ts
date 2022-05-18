import { Fill, Named, Poly, Scale, Shape, Stroke } from "types/shapes";
import {
  boolV,
  Canvas,
  floatV,
  ptListV,
  sampleBlack,
  sampleNoPaint,
  strV,
} from "./Samplers";

export interface PolylineProps extends Named, Stroke, Fill, Scale, Poly {}

export const samplePolyline = (
  _rng: seedrandom.prng,
  _canvas: Canvas
): PolylineProps => ({
  name: strV("defaultPolyline"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: sampleBlack(),
  strokeDasharray: strV(""),
  fillColor: sampleNoPaint(),
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
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<PolylineProps>
): Polyline => ({
  ...samplePolyline(rng, canvas),
  ...properties,
  shapeType: "Polyline",
});
