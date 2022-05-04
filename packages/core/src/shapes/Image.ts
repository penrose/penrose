import { ICenter, INamed, IRect, IRotate, IShape } from "types/shapes";
import { IStrV } from "types/value";
import {
  BoolV,
  Canvas,
  sampleHeight,
  sampleVector,
  sampleWidth,
  sampleZero,
  StrV,
} from "./Samplers";

export interface IImage extends INamed, ICenter, IRect, IRotate {
  href: IStrV;
  // note, SVG also has these two attributes:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/crossorigin
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio
}

export const sampleImage = (rng: seedrandom.prng, canvas: Canvas): IImage => ({
  name: StrV("defaultImage"),
  style: StrV(""),
  center: sampleVector(rng, canvas),
  width: sampleWidth(rng, canvas),
  height: sampleHeight(rng, canvas),
  rotation: sampleZero(),
  href: StrV("defaultImage"),
  ensureOnCanvas: BoolV(true),
});

export type Image = IShape & { shapeType: "Image" } & IImage;

export const makeImage = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IImage>
): Image => ({
  ...sampleImage(rng, canvas),
  ...properties,
  shapeType: "Image",
});
