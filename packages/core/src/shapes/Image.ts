import * as ad from "../types/ad";
import { Center, Named, Rect, ShapeCommon, Transform } from "../types/shapes";
import { StrV } from "../types/value";
import { boolV, id3x3, strV } from "../utils/Util";
import {
  Canvas,
  Context,
  sampleHeight,
  sampleVector,
  sampleWidth,
} from "./Samplers";

export interface ImageProps<T>
  extends Named<T>,
    Center<T>,
    Rect<T>,
    Transform<T> {
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
  href: strV("defaultImage"),
  transform: id3x3(),
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
