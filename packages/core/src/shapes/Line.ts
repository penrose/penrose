import { bboxFromLinelike } from "engine/BBox";
import { VarAD } from "types/ad";
import { IVectorV } from "types/value";
import { IArrow, INamed, IShape, IStroke } from "./Interfaces";

export interface ILine extends INamed, IStroke, IArrow {
  start: IVectorV<VarAD>;
  end: IVectorV<VarAD>;
}

export type Line = IShape & ILine;

export const Line = (properties: ILine): Line => ({
  ...properties,
  shapeType: "Line",
  bbox: function () {
    return bboxFromLinelike(this);
  },
});
