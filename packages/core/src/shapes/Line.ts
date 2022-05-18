import * as ad from "types/ad";
import { Arrow, Named, Shape, Stroke } from "types/shapes";
import { StrV, VectorV } from "types/value";
import {
  boolV,
  Canvas,
  floatV,
  sampleColor,
  sampleVector,
  strV,
} from "./Samplers";

export interface LineProps extends Named, Stroke, Arrow {
  start: VectorV<ad.Num>;
  end: VectorV<ad.Num>;
  strokeLinecap: StrV;
}

export const sampleLine = (
  rng: seedrandom.prng,
  canvas: Canvas
): LineProps => ({
  name: strV("defaultLine"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: sampleColor(rng),
  strokeDasharray: strV(""),
  arrowheadSize: floatV(1),
  arrowheadStyle: strV("arrowhead-2"),
  startArrowhead: boolV(false),
  endArrowhead: boolV(false),
  start: sampleVector(rng, canvas),
  end: sampleVector(rng, canvas),
  strokeLinecap: strV(""),
  ensureOnCanvas: boolV(true),
});

export type Line = Shape & { shapeType: "Line" } & LineProps;

export const makeLine = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<LineProps>
): Line => ({
  ...sampleLine(rng, canvas),
  ...properties,
  shapeType: "Line",
});
