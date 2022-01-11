import { constOf } from "engine/Autodiff";
import { sqrt, mul } from "engine/AutodiffFunctions";
import { Pt2, VarAD } from "types/ad";
import * as BBox from "engine/BBox";
import { shapedefs } from "shapes/Shapes";

/**
 * Return bounding box from any provided shape.
 */
export const bboxFromShape = ([t, s]: [string, any]): BBox.BBox => {
  return shapedefs[t].bbox(s);
};

/**
 * Return center of the shape `shape`.
 * For shapes without the property `center`, the center of their bounding box is returned.
 */
export const shapeCenter = ([t, s]: [string, any]): Pt2 => {
  if ("center" in s) {
    return s.center.contents;
  } else {
    // Return center of bounding box
    const bbox = bboxFromShape([t, s]);
    return bbox.center;
  }
};

/**
 * Return size of the shape `shape`.
 * - `radius` for circles.
 * - `sqrt( w * h )`, where `w` and `h` are the width and height of the bounding box, for all other shapes.
 */
 export const shapeSize = ([t, s]: [string, any]): VarAD => {
  if (t == "Circle") {
    return mul(constOf(2.0), s.r.contents);
  } else {
    const bbox = bboxFromShape([t, s]);
    return sqrt(mul(bbox.width, bbox.height));
  }
};
