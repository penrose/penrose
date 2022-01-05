import { constOf } from "engine/Autodiff";
import { VarAD } from "types/ad";
import { INamed, IStroke, IArrow, IShape } from "types/shapes";
import { IVectorV } from "types/value";
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
}

export const sampleLine = (canvas: Canvas): ILine => ({
  name: StrV("defaultLine"),
  strokeWidth: sampleFloatIn(5, 15),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(),
  strokeDashArray: StrV(""),
  arrowheadSize: FloatV(constOf(1)),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  start: sampleVector(canvas),
  end: sampleVector(canvas),
});

export type Line = IShape & { shapeType: "Line" } & ILine;

export const makeLine = (canvas: Canvas, properties: Partial<ILine>): Line => ({
  ...sampleLine(canvas),
  ...properties,
  shapeType: "Line",
});
