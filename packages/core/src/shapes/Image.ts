import * as ad from "../types/ad.js";
import { Center, Named, Rect, Rotate, ShapeCommon } from "../types/shapes.js";
import { StrV } from "../types/value.js";
import { boolV, floatV, strV } from "../utils/Util.js";
import {
  Canvas,
  Context,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers.js";

export interface ImageProps<T> extends Named<T>, Center<T>, Rect<T>, Rotate<T> {
  href: StrV;
  // note, SVG also has these two attributes:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/crossorigin
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio
}

export const sampleImage = (
  context: Context,
  canvas: Canvas
): ImageProps<ad.Num> => ({
  name: strV("defaultImage"),
  style: strV(""),
  center: sampleVector(context, canvas),
  width: sampleWidth(context, canvas),
  height: sampleHeight(context, canvas),
  rotation: floatV(0),
  href: strV("defaultImage"),
  ensureOnCanvas: boolV(true),
});

export type Image<T> = ShapeCommon<T> & { shapeType: "Image" } & ImageProps<T>;

export const makeImage = (
  context: Context,
  canvas: Canvas,
  properties: Partial<ImageProps<ad.Num>>
): Image<ad.Num> => ({
  ...sampleImage(context, canvas),
  ...properties,
  shapeType: "Image",
  passthrough: new Map(),
});
