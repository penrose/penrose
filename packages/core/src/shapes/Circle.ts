import { VarAD } from "types/ad";
import { INamed, IStroke, IFill, ICenter, IShape } from "types/shapes";
import { IFloatV } from "types/value";
import {
  Canvas,
  sampleNoPaint,
  sampleColor,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
  BoolV,
} from "./Samplers";

export interface ICircle extends INamed, IStroke, IFill, ICenter {
  r: IFloatV<VarAD>;
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
