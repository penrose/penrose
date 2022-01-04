import * as BBox from "engine/BBox";
import { VarAD } from "types/ad";
import { IFloatV } from "types/value";
import { ICenter, IFill, INamed, IShape, IStroke } from "./Shapes";
import {
  Canvas,
  sampleNoPaint,
  sampleColor,
  sampleStroke,
  sampleVector,
  sampleWidth,
  StrV,
} from "./Samplers";

export interface ICircle extends INamed, IStroke, IFill, ICenter {
  r: IFloatV<VarAD>;
}

export const sampleCircle = (canvas: Canvas): ICircle => ({
  name: StrV("defaultCircle"),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  r: sampleWidth(canvas),
});

export type Circle = IShape & ICircle;

export const Circle = (
  canvas: Canvas,
  properties: Partial<ICircle>
): Circle => ({
  ...sampleCircle(canvas),
  ...properties,
  shapeType: "Circle",
  bbox: function () {
    return BBox.bboxFromCircle(this);
  },
});
