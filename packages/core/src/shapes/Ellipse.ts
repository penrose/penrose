import { VarAD } from "types/ad";
import { INamed, IStroke, IFill, ICenter, IShape } from "types/shapes";
import { IFloatV } from "types/value";
import {
  Canvas,
  sampleColor,
  sampleHeight,
  sampleNoPaint,
  sampleStroke,
  sampleVector,
  sampleWidth,
  StrV,
} from "./Samplers";

export interface IEllipse extends INamed, IStroke, IFill, ICenter {
  rx: IFloatV<VarAD>;
  ry: IFloatV<VarAD>;
}

export const sampleEllipse = (canvas: Canvas): IEllipse => ({
  name: StrV("defaultEllipse"),
  style: StrV(""),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  rx: sampleWidth(canvas),
  ry: sampleHeight(canvas),
});

export type Ellipse = IShape & { shapeType: "Ellipse" } & IEllipse;

export const makeEllipse = (
  canvas: Canvas,
  properties: Partial<IEllipse>
): Ellipse => ({
  ...sampleEllipse(canvas),
  ...properties,
  shapeType: "Ellipse",
});
