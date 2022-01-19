import { VarAD } from "types/ad";
import { INamed, IStroke, IFill, ICenter, IShape } from "types/shapes";
import { IFloatV } from "types/value";
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

export interface IEllipse extends INamed, IStroke, IFill, ICenter {
  rx: IFloatV<VarAD>;
  ry: IFloatV<VarAD>;
}

export const sampleEllipse = (
  rng: seedrandom.prng,
  canvas: Canvas
): IEllipse => ({
  name: StrV("defaultEllipse"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  rx: sampleWidth(rng, canvas),
  ry: sampleHeight(rng, canvas),
  ensureOnCanvas: BoolV(true),
});

export type Ellipse = IShape & { shapeType: "Ellipse" } & IEllipse;

export const makeEllipse = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IEllipse>
): Ellipse => ({
  ...sampleEllipse(rng, canvas),
  ...properties,
  shapeType: "Ellipse",
});
