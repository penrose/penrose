import { bboxFromRectlike } from "engine/BBox";
import { IStrV } from "types/value";
import { ICenter, INamed, IRect, IRotate, IShape } from "./Interfaces";

// not to be confused with IRect... need to rename maybe?
export interface IImage extends INamed, ICenter, IRect, IRotate {
  href: IStrV;
  // note, SVG also has these two attributes:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/crossorigin
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio
}

export type Image = IShape & IImage;

export const Image = (properties: IImage): Image => ({
  ...properties,
  shapeType: "Image",
  bbox: function () {
    // https://github.com/penrose/penrose/issues/712
    return bboxFromRectlike(this);
  },
});
