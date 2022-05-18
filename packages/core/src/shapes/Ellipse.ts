import * as ad from "types/ad";
import { Center, Fill, Named, Shape, Stroke } from "types/shapes";
import { FloatV } from "types/value";
import {
  boolV,
  Canvas,
  sampleColor,
  sampleHeight,
  sampleNoPaint,
  sampleVector,
  sampleWidth,
  sampleZero,
  strV,
} from "./Samplers";

export interface EllipseProps extends Named, Stroke, Fill, Center {
  rx: FloatV<ad.Num>;
  ry: FloatV<ad.Num>;
}

export const sampleEllipse = (
  rng: seedrandom.prng,
  canvas: Canvas
): EllipseProps => ({
  name: strV("defaultEllipse"),
  style: strV(""),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  rx: sampleWidth(rng, canvas),
  ry: sampleHeight(rng, canvas),
  ensureOnCanvas: boolV(true),
});

export type Ellipse = Shape & { shapeType: "Ellipse" } & EllipseProps;

export const makeEllipse = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<EllipseProps>
): Ellipse => ({
  ...sampleEllipse(rng, canvas),
  ...properties,
  shapeType: "Ellipse",
});
