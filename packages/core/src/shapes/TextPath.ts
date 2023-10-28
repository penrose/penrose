import * as ad from "../types/ad.js";
import { Fill, Named, ShapeCommon, String, Stroke } from "../types/shapes.js";
import { FloatV, PathDataV, StrV } from "../types/value.js";
import {
  black,
  boolV,
  floatV,
  noPaint,
  pathDataV,
  strV,
} from "../utils/Util.js";
import { Canvas, Context } from "./Samplers.js";

export interface TextPathProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    String<T> {
  startOffset: FloatV<T>;
  method: StrV;
  spacing: StrV;
  path: PathDataV<T>;
}

// Default TextPath properties
export const sampleTextPath = (
  context: Context,
  canvas: Canvas,
): TextPathProps<ad.Num> => ({
  name: strV("defaultTextPath"),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  fillColor: black(),
  string: strV("defaultTextPath"),
  startOffset: floatV(0),
  method: strV("align"),
  spacing: strV("auto"),
  strokeDasharray: strV(""),
  fontSize: strV("12px"),
  path: pathDataV([]),
  ensureOnCanvas: boolV(true),
});

// TextPath type
export type TextPath<T> = ShapeCommon<T> & {
  shapeType: "TextPath";
} & TextPathProps<T>;

// Factory function to create a TextPath object
export const makeTextPath = (
  context: Context,
  canvas: Canvas,
  properties: Partial<TextPathProps<ad.Num>>,
): TextPath<ad.Num> => ({
  ...sampleTextPath(context, canvas),
  ...properties,
  shapeType: "TextPath",
  passthrough: new Map(),
});
