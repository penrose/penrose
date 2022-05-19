import * as ad from "types/ad";
import { Center, Fill, Named, Shape, Stroke } from "types/shapes";
import { FloatV } from "types/value";
import { boolV, strV } from "utils/Util";
import {
  Canvas,
  sampleColor,
  sampleNoPaint,
  sampleVector,
  sampleWidth,
  sampleZero,
} from "./Samplers";

export interface CircleProps extends Named, Stroke, Fill, Center {
  r: FloatV<ad.Num>;
}

export const sampleCircle = (
  rng: seedrandom.prng,
  canvas: Canvas
): CircleProps => ({
  name: strV("defaultCircle"),
  style: strV(""),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  r: sampleWidth(rng, canvas),
  ensureOnCanvas: boolV(true),
});

export type Circle = Shape & { shapeType: "Circle" } & CircleProps;

export const makeCircle = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<CircleProps>
): Circle => ({
  ...sampleCircle(rng, canvas),
  ...properties,
  shapeType: "Circle",
});
