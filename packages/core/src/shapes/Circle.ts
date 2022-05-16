import * as ad from "types/ad";
import { ICenter, IFill, INamed, IShape, IStroke } from "types/shapes";
import { IFloatV } from "types/value";
import {
  BoolV,
  Canvas,
  sampleColor,
  sampleNoPaint,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
} from "./Samplers";

export interface ICircle extends INamed, IStroke, IFill, ICenter {
  r: IFloatV<ad.Num>;
}

export const sampleCircle = (
  rng: seedrandom.prng,
  canvas: Canvas
): ICircle => ({
  name: StrV("defaultCircle"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  r: sampleWidth(rng, canvas),
  ensureOnCanvas: BoolV(true),
});

export type Circle = IShape & { shapeType: "Circle" } & ICircle;

export const makeCircle = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<ICircle>
): Circle => ({
  ...sampleCircle(rng, canvas),
  ...properties,
  shapeType: "Circle",
});
