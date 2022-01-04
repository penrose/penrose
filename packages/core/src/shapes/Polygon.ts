import { constOf } from "engine/Autodiff";
import { bboxFromPolygon } from "engine/BBox";
import { IFill, INamed, IPoly, IScale, IShape, IStroke } from "./Interfaces";
import {
  Canvas,
  FloatV,
  PtListV,
  sampleColor,
  sampleNoPaint,
  sampleStroke,
  StrV,
} from "./Samplers";

export interface IPolygon extends INamed, IStroke, IFill, IScale, IPoly {}

export const samplePolygon = (_canvas: Canvas): IPolygon => ({
  name: StrV("defaultPolygon"),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  scale: FloatV(constOf(1)),
  points: PtListV(
    [
      [0, 0],
      [0, 10],
      [10, 0],
    ].map((p) => p.map(constOf))
  ),
});

export type Polygon = IShape & IPolygon;

export const Polygon = (
  canvas: Canvas,
  properties: Partial<IPolygon>
): Polygon => ({
  ...samplePolygon(canvas),
  ...properties,
  shapeType: "Polygon",
  bbox: function () {
    // https://github.com/penrose/penrose/issues/709
    return bboxFromPolygon(this);
  },
});
