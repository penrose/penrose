import { bboxFromPolygon } from "engine/BBox";
import { IFill, INamed, IPoly, IScale, IShape, IStroke } from "./Interfaces";

export interface IPolyline extends INamed, IStroke, IFill, IScale, IPoly {}

export type Polyline = IShape & IPolyline;

export const Polyline = (properties: IPolyline): Polyline => ({
  ...properties,
  shapeType: "Polyline",
  bbox: function () {
    // https://github.com/penrose/penrose/issues/709
    return bboxFromPolygon(this);
  },
});
