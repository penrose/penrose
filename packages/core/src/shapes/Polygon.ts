import { IFill, INamed, IPoly, IScale, IShape, IStroke } from "types/shapes";
import {
  BoolV,
  Canvas,
  FloatV,
  PtListV,
  sampleColor,
  sampleNoPaint,
  sampleZero,
  StrV,
} from "./Samplers";

export interface IPolygon extends INamed, IStroke, IFill, IScale, IPoly {}

export const samplePolygon = (
  rng: seedrandom.prng,
  _canvas: Canvas
): IPolygon => ({
  name: StrV("defaultPolygon"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(rng),
  scale: FloatV(1),
  points: PtListV([
    [0, 0],
    [0, 10],
    [10, 0],
  ]),
  ensureOnCanvas: BoolV(true),
});

export type Polygon = IShape & { shapeType: "Polygon" } & IPolygon;

export const makePolygon = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IPolygon>
): Polygon => ({
  ...samplePolygon(rng, canvas),
  ...properties,
  shapeType: "Polygon",
});
