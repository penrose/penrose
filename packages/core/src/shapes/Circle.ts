import * as ad from "types/ad";
import { Center, Fill, Named, Shape, Stroke } from "types/shapes";
import { FloatV } from "types/value";
import { boolV, floatV, noPaint, strV } from "utils/Util";
import {
  Canvas,
  Context,
  sampleColor,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface CircleProps extends Named, Stroke, Fill, Center {
  r: FloatV<ad.Num>;
}

export const sampleCircle = (context: Context, canvas: Canvas): CircleProps => {
  return {
    name: strV("defaultCircle"),
    style: strV(""),
    strokeWidth: floatV(0),
    strokeStyle: strV("solid"),
    strokeColor: noPaint(),
    strokeDasharray: strV(""),
    fillColor: sampleColor(context),
    center: sampleVector(context, canvas),
    r: sampleWidth(context, canvas),
    ensureOnCanvas: boolV(true),
  };
};

export type Circle = Shape & { shapeType: "Circle" } & CircleProps;

export const makeCircle = (
  context: Context,
  canvas: Canvas,
  properties: Partial<CircleProps>
): Circle => ({
  ...sampleCircle(context, canvas),
  ...properties,
  shapeType: "Circle",
});
