import * as ad from "types/ad";
import { IArrow, INamed, IShape, IStroke } from "types/shapes";
import { IStrV, IVectorV } from "types/value";
import {
  BoolV,
  Canvas,
  FloatV,
  sampleColor,
  sampleVector,
  StrV,
} from "./Samplers";

export interface ILine extends INamed, IStroke, IArrow {
  start: IVectorV<ad.Num>;
  end: IVectorV<ad.Num>;
  strokeLinecap: IStrV;
}

export const sampleLine = (rng: seedrandom.prng, canvas: Canvas): ILine => ({
  name: StrV("defaultLine"),
  style: StrV(""),
  strokeWidth: FloatV(1),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(rng),
  strokeDasharray: StrV(""),
  arrowheadSize: FloatV(1),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  start: sampleVector(rng, canvas),
  end: sampleVector(rng, canvas),
  strokeLinecap: StrV(""),
  ensureOnCanvas: BoolV(true),
});

export type Line = IShape & { shapeType: "Line" } & ILine;

export const makeLine = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<ILine>
): Line => ({
  ...sampleLine(rng, canvas),
  ...properties,
  shapeType: "Line",
});
