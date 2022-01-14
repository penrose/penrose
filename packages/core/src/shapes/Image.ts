import { INamed, ICenter, IRect, IRotate, IShape } from "types/shapes";
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

export const sampleImage = (canvas: Canvas): IImage => ({
  name: StrV("defaultImage"),
  style: StrV(""),
  center: sampleVector(canvas),
  width: sampleWidth(canvas),
  height: sampleHeight(canvas),
  rotation: sampleZero(),
  href: StrV("defaultImage"),
  ensureOnCanvas: BoolV(true),
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
