import { bboxFromEllipse } from "engine/BBox";
import { VarAD } from "types/ad";
import { IFloatV } from "types/value";
import { ICenter, IFill, INamed, IShape, IStroke } from "./Interfaces";

export interface IEllipse extends INamed, IStroke, IFill, ICenter {
  rx: IFloatV<VarAD>;
  ry: IFloatV<VarAD>;
}

export type Ellipse = IShape & IEllipse;

export const Ellipse = (properties: IEllipse): Ellipse => ({
  ...properties,
  shapeType: "Ellipse",
  bbox: function () {
    return bboxFromEllipse(this);
  },
});
