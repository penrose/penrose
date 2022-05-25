import * as ad from "types/ad";
import { Center, Fill, Named, Shape, Stroke } from "types/shapes";
import { FloatV } from "types/value";
import { boolV, floatV, noPaint, strV } from "utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface EllipseProps extends Named, Stroke, Fill, Center {
  rx: FloatV<ad.Num>;
  ry: FloatV<ad.Num>;
}

export const sampleEllipse = (
  context: Context,
  canvas: Canvas
): EllipseProps => ({
  name: strV("defaultEllipse"),
  style: strV(""),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(context),
  center: sampleVector(context, canvas),
  rx: sampleWidth(context, canvas),
  ry: sampleHeight(context, canvas),
  ensureOnCanvas: boolV(true),
});

export type Ellipse = Shape & { shapeType: "Ellipse" } & EllipseProps;

export const makeEllipse = (
  context: Context,
  canvas: Canvas,
  properties: Partial<EllipseProps>
): Ellipse => ({
  ...sampleEllipse(context, canvas),
  ...properties,
  shapeType: "Ellipse",
});
