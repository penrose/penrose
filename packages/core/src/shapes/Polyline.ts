import { IFill, INamed, IPoly, IScale, IShape, IStroke } from "types/shapes";
import {
  BoolV,
  Canvas,
  FloatV,
  PtListV,
  sampleBlack,
  sampleNoPaint,
  StrV,
} from "./Samplers";

export interface IPolyline extends INamed, IStroke, IFill, IScale, IPoly {}

export const samplePolyline = (
  _rng: seedrandom.prng,
  _canvas: Canvas
): IPolyline => ({
  name: StrV("defaultPolyline"),
  style: StrV(""),
  strokeWidth: FloatV(1),
  strokeStyle: StrV("solid"),
  strokeColor: sampleBlack(),
  strokeDasharray: StrV(""),
  fillColor: sampleNoPaint(),
  scale: FloatV(1),
  points: PtListV([
    [0, 0],
    [0, 10],
    [10, 0],
  ]),
  ensureOnCanvas: BoolV(true),
});

export type Polyline = IShape & { shapeType: "Polyline" } & IPolyline;

export const makePolyline = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IPolyline>
): Polyline => ({
  ...samplePolyline(rng, canvas),
  ...properties,
  shapeType: "Polyline",
});
