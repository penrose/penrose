import { Center, Named, Rect, Rotate, Shape } from "types/shapes";
import { StrV } from "types/value";
import {
  boolV,
  Canvas,
  sampleHeight,
  sampleVector,
  sampleWidth,
  sampleZero,
  strV,
} from "./Samplers";

export interface ImageProps extends Named, Center, Rect, Rotate {
  href: StrV;
  // note, SVG also has these two attributes:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/crossorigin
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio
}

export const sampleImage = (
  rng: seedrandom.prng,
  canvas: Canvas
): ImageProps => ({
  name: strV("defaultImage"),
  style: strV(""),
  center: sampleVector(rng, canvas),
  width: sampleWidth(rng, canvas),
  height: sampleHeight(rng, canvas),
  rotation: sampleZero(),
  href: strV("defaultImage"),
  ensureOnCanvas: boolV(true),
});

export type Image = Shape & { shapeType: "Image" } & ImageProps;

export const makeImage = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<ImageProps>
): Image => ({
  ...sampleImage(rng, canvas),
  ...properties,
  shapeType: "Image",
});
