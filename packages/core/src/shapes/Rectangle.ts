import { bboxFromRect } from "engine/BBox";
import {
  ICenter,
  ICorner,
  IFill,
  INamed,
  IRect,
  IShape,
  IStroke,
} from "./Interfaces";

// not to be confused with IRect... need to rename maybe?
export interface IRectangle
  extends INamed,
    IStroke,
    IFill,
    ICenter,
    IRect,
    ICorner {}

export type Rectangle = IShape & IRectangle;

export const Rectangle = (properties: IRectangle): Rectangle => ({
  ...properties,
  shapeType: "Rectangle",
  bbox: function () {
    return bboxFromRect(this);
  },
});
