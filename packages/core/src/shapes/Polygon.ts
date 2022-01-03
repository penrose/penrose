import { bboxFromPolygon } from "engine/BBox";
import { IFill, INamed, IPoly, IScale, IShape, IStroke } from "./Interfaces";

export interface IPolygon extends INamed, IStroke, IFill, IScale, IPoly {}

export type Polygon = IShape & IPolygon;

export const Polygon = (properties: IPolygon): Polygon => ({
  ...properties,
  shapeType: "Polygon",
  bbox: function () {
    // https://github.com/penrose/penrose/issues/709
    return bboxFromPolygon(this);
  },
});
