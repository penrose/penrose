import { bboxFromRectlike } from "engine/BBox";
import { IStrV } from "types/value";
import { ICenter, INamed, IRect, IRotate, IShape } from "./Interfaces";
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

export type Image = IShape & IImage;

export const Image = (canvas: Canvas, properties: Partial<IImage>): Image => ({
  ...sampleImage(canvas),
  ...properties,
  shapeType: "Image",
  bbox: function () {
    // https://github.com/penrose/penrose/issues/712
    return bboxFromRectlike(this);
  },
});
