import {
  Center,
  Corner,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  Stroke,
} from "types/shapes";
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

export interface RectangleProps
  extends Named,
    Stroke,
    Fill,
    Center,
    Rotate,
    Rect,
    Corner {}

export const sampleRectangle = (
  rng: seedrandom.prng,
  canvas: Canvas
): RectangleProps => ({
  name: strV("defaultRectangle"),
  style: strV(""),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  width: sampleWidth(rng, canvas),
  height: sampleHeight(rng, canvas),
  cornerRadius: sampleZero(),
  rotation: sampleZero(),
  ensureOnCanvas: boolV(true),
});

export type Rectangle = Shape & { shapeType: "Rectangle" } & RectangleProps;

export const makeRectangle = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<RectangleProps>
): Rectangle => ({
  ...sampleRectangle(rng, canvas),
  ...properties,
  shapeType: "Rectangle",
});
