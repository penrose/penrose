import { bboxFromRectlike } from "engine/BBox";
import { IStrV } from "types/value";
import { ICenter, INamed, IRect, IRotate, IShape, ShapeDef } from "./Shapes";
import {
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

export const sampleImage = (canvas: Canvas): IImage => ({
  name: StrV("defaultImage"),
  center: sampleVector(canvas),
  width: sampleWidth(canvas),
  height: sampleHeight(canvas),
  rotation: sampleZero(),
  href: StrV("defaultImage"),
});

export type Image = IShape & { shapeType: "Image" } & IImage;

export const makeImage = (
  canvas: Canvas,
  properties: Partial<IImage>
): Image => ({
  ...sampleImage(canvas),
  ...properties,
  shapeType: "Image",
});

export const Image = ShapeDef({
  sampler: sampleImage,
  constr: makeImage,

  bbox: bboxFromRectlike, // https://github.com/penrose/penrose/issues/712
  isRectlike: true,
});
