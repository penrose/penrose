import * as BBox from "engine/BBox";
import { VarAD } from "types/ad";
import { IFloatV } from "types/value";
import { ICenter, IFill, INamed, IShape, IStroke } from "./Interfaces";

export interface ICircle extends INamed, IStroke, IFill, ICenter {
  r: IFloatV<VarAD>;
}

export type Circle = IShape & ICircle;

export const Circle = (properties: ICircle): Circle => ({
  ...properties,
  shapeType: "Circle",
  bbox: function () {
    return BBox.bboxFromCircle(this);
  },
});
