import { VarAD } from "types/ad";
import { INamed, IStroke, IFill, ICenter, IShape } from "types/shapes";
import { IFloatV } from "types/value";
import {
  Canvas,
  sampleNoPaint,
  sampleColor,
  sampleStroke,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
  BoolV,
} from "./Samplers";

export interface ICircle extends INamed, IStroke, IFill, ICenter {
  r: IFloatV<VarAD>;
}

export const sampleCircle = (canvas: Canvas): ICircle => ({
  name: StrV("defaultCircle"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  r: sampleWidth(canvas),
  ensureOnCanvas: BoolV(true),
});

export type Circle = IShape & { shapeType: "Circle" } & ICircle;

export const makeCircle = (
  canvas: Canvas,
  properties: Partial<ICircle>
): Circle => ({
  ...sampleCircle(canvas),
  ...properties,
  shapeType: "Circle",
});
