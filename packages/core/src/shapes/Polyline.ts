import { constOf } from "engine/Autodiff";
import { INamed, IStroke, IFill, IScale, IPoly, IShape } from "types/shapes";
import {
  Canvas,
  FloatV,
  PtListV,
  sampleColor,
  sampleNoPaint,
  sampleStroke,
  StrV,
} from "./Samplers";

export interface IPolyline extends INamed, IStroke, IFill, IScale, IPoly {}

export const samplePolyline = (_canvas: Canvas): IPolyline => ({
  name: StrV("defaultPolyline"),
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

export type Polyline = IShape & { shapeType: "Polyline" } & IPolyline;

export const makePolyline = (
  canvas: Canvas,
  properties: Partial<IPolyline>
): Polyline => ({
  ...samplePolyline(canvas),
  ...properties,
  shapeType: "Polyline",
});
