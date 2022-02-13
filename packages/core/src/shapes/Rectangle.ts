import {
  ICenter,
  ICorner,
  IFill,
  INamed,
  IRect,
  IRotate,
  IShape,
  IStroke,
} from "types/shapes";
import {
  BoolV,
  Canvas,
  sampleColor,
  sampleHeight,
  sampleNoPaint,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
} from "./Samplers";

// not to be confused with IRect... need to rename maybe?
export interface IRectangle
  extends INamed,
    IStroke,
    IFill,
    ICenter,
    IRotate,
    IRect,
    ICorner {}

export const sampleRectangle = (
  rng: seedrandom.prng,
  canvas: Canvas
): IRectangle => ({
  name: StrV("defaultRectangle"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  width: sampleWidth(rng, canvas),
  height: sampleHeight(rng, canvas),
  cornerRadius: sampleZero(),
  rotation: sampleZero(),
  ensureOnCanvas: BoolV(true),
});

export type Rectangle = IShape & { shapeType: "Rectangle" } & IRectangle;

export const makeRectangle = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IRectangle>
): Rectangle => ({
  ...sampleRectangle(rng, canvas),
  ...properties,
  shapeType: "Rectangle",
});
