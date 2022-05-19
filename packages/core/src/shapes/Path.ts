import * as ad from "types/ad";
import { Arrow, Fill, Named, Shape, Stroke } from "types/shapes";
import { PathDataV } from "types/value";
import { boolV, floatV, pathDataV, strV } from "utils/Util";
import { Canvas, sampleColor, sampleNoPaint } from "./Samplers";

export interface PathProps extends Named, Stroke, Fill, Arrow {
  d: PathDataV<ad.Num>;
}

export const samplePath = (
  rng: seedrandom.prng,
  _canvas: Canvas
): PathProps => ({
  name: strV("defaultPath"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: sampleColor(rng),
  strokeDasharray: strV(""),
  fillColor: sampleNoPaint(),
  arrowheadSize: floatV(1),
  arrowheadStyle: strV("arrowhead-2"),
  startArrowhead: boolV(false),
  endArrowhead: boolV(false),
  d: pathDataV([]),
  ensureOnCanvas: boolV(true),
});

export type Path = Shape & { shapeType: "Path" } & PathProps;

export const makePath = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<PathProps>
): Path => ({
  ...samplePath(rng, canvas),
  ...properties,
  shapeType: "Path",
});
