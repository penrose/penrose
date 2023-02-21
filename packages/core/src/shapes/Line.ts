import * as ad from "../types/ad";
import { Arrow, Named, Shape, Stroke } from "../types/shapes";
import { StrV, VectorV } from "../types/value";
import { black, boolV, floatV, strV } from "../utils/Util";
import { Canvas, Context, sampleVector } from "./Samplers";

export interface LineProps extends Named, Stroke, Arrow {
  start: VectorV<ad.Num>;
  end: VectorV<ad.Num>;
  strokeLinecap: StrV;
}

export const sampleLine = (context: Context, canvas: Canvas): LineProps => ({
  name: strV("defaultLine"),
  style: strV(""),
  strokeWidth: floatV(1),
  strokeStyle: strV("solid"),
  strokeColor: black(),
  strokeDasharray: strV(""),
  startArrowheadSize: floatV(1),
  startArrowhead: strV("none"),
  flipStartArrowhead: boolV(false),
  endArrowheadSize: floatV(1),
  endArrowhead: strV("none"),
  start: sampleVector(context, canvas),
  end: sampleVector(context, canvas),
  strokeLinecap: strV(""),
  ensureOnCanvas: boolV(true),
});

export type Line = Shape & { shapeType: "Line" } & LineProps;

export const makeLine = (
  context: Context,
  canvas: Canvas,
  properties: Partial<LineProps>
): Line => ({
  ...sampleLine(context, canvas),
  ...properties,
  shapeType: "Line",
});
