import { add } from "engine/Autodiff";
import * as BBox from "engine/BBox";
import { isPt2 } from "types/ad";
import {
  ICenter,
  ICorner,
  IFill,
  INamed,
  IRect,
  IShape,
  IStroke,
  ITransform,
} from "./Interfaces";

// not to be confused with IRect... need to rename maybe?
export interface IRectangle
  extends INamed,
    IStroke,
    IFill,
    ICenter,
    IRect,
    ICorner,
    ITransform {}

const bboxFromRect = ({
  width,
  height,
  center,
  strokeWidth,
}: IRectangle): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromRect expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  // rx just rounds the corners, doesn't change the bbox
  return BBox.bbox(
    add(width.contents, strokeWidth.contents),
    add(height.contents, strokeWidth.contents),
    center.contents
  );
};

export type Rectangle = IShape & IRectangle;

export const Rectangle = (properties: IRectangle): Rectangle => ({
  ...properties,
  shapeType: "Rectangle",
  bbox: function () {
    return bboxFromRect(this);
  },
});
