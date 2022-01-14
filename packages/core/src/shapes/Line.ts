import { constOf } from "engine/Autodiff";
import { VarAD } from "types/ad";
import { INamed, IStroke, IArrow, IShape } from "types/shapes";
import { IStrV, IVectorV } from "types/value";
import {
  BoolV,
  Canvas,
  FloatV,
  sampleColor,
  sampleFloatIn,
  sampleVector,
  StrV,
} from "./Samplers";

export interface ILine extends INamed, IStroke, IArrow {
  start: IVectorV<VarAD>;
  end: IVectorV<VarAD>;
  strokeLinecap: IStrV;
}

export const sampleLine = (canvas: Canvas): ILine => ({
  name: StrV("defaultLine"),
  style: StrV(""),
  strokeWidth: FloatV(constOf(1)),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(),
  strokeDasharray: StrV(""),
  arrowheadSize: FloatV(constOf(1)),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  start: sampleVector(canvas),
  end: sampleVector(canvas),
  strokeLinecap: StrV(""),
  ensureOnCanvas: BoolV(true),
});

export type Line = IShape & { shapeType: "Line" } & ILine;

export const makeLine = (canvas: Canvas, properties: Partial<ILine>): Line => ({
  ...sampleLine(canvas),
  ...properties,
  shapeType: "Line",
});
