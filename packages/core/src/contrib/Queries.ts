import {
  constOf,
  squared,
  sub,
  varOf,
  max,
  ops,
  sqrt,
  mul,
} from "engine/Autodiff";
import { Pt2, VarAD } from "types/ad";
import * as BBox from "engine/BBox";
import { findDef } from "renderer/ShapeDef";

/**
 * Preconditions:
 *   (shape-specific)
 * Input: A shape.
 * Output: A new BBox
*/
export const bboxFromShape = (t: string, s: any): BBox.BBox => {
  return findDef(t).bbox(s);
};

/**
 * Return center of the shape `shape`.
 * For shapes without the property `center`, the center of their bounding box is returned.
 */
export const shapeCenter = ([t, s]: [string, any]): Pt2 => {
  if ('center' in s) {
    return s.center.contents;
  }
  else {
    // Return center of bounding box
    const bbox = bboxFromShape(t, s);
    return bbox.center;
  }
};

/**
 * Return size of the shape `shape`.
 * - `radius` for circles.
 * - `side` for squares.
 * - `sqrt( w * h )`, where `w` and `h` are the width and heoght of the bounding box, for all other shapes.
 */
 export const shapeSize = ([t, s]: [string, any]): VarAD => {
  switch (t) {
    case "Circle":
      return mul(constOf(2.0), s.r.contents);
    case "Square":
      return s.side.contents;
    default: {
      const bbox = bboxFromShape(t, s);
      return sqrt(mul(bbox.w, bbox.h));
    }
  }
};

/**
 * Encourage the center of the shape `shape` to be in the direction `direction` with respect to shape `shapeRef`.
 * For shapes without the property `center`, the center of their bounding box is used.
 */
export const inDirection = (
  [, shape]: [string, any],
  [, shapeRef]: [string, any],
  direction: Pt2,
  offset: number,
  normalizeDirection: boolean,
): VarAD => {
  const center = shapeCenter(shape);
  const centerRef = shapeCenter(shapeRef);
  var dotProduct;
  if (normalizeDirection) {
    dotProduct = ops.vdot(ops.vsub(center, centerRef), ops.vnormalize(direction));
  }
  else {
    dotProduct = ops.vdot(ops.vsub(center, centerRef), direction);
  }
  return squared(max(constOf(0.0), sub(dotProduct, varOf(offset))));
};
