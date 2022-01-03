import { bboxFromPath } from "engine/BBox";
import { VarAD } from "types/ad";
import { IPathDataV } from "types/value";
import { IArrow, IFill, INamed, IShape, IStroke } from "./Interfaces";

export interface IPath extends INamed, IStroke, IFill, IArrow {
  d: IPathDataV<VarAD>;
}

export type Path = IShape & IPath;

export const Path = (properties: IPath): Path => ({
  ...properties,
  shapeType: "Path",
  bbox: function () {
    return bboxFromPath(this);
  },
});
