import { ops } from "engine/Autodiff";
import { mul, neg, sqrt } from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import { shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import { msign } from "./Functions";

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
export const shapeCenter = ([t, s]: [string, any]): ad.Pt2 => {
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
export const shapeSize = ([t, s]: [string, any]): ad.Num => {
  if (t === "Circle") {
    return mul(2, s.r.contents);
  } else {
    const bbox = bboxFromShape([t, s]);
    return sqrt(mul(bbox.width, bbox.height));
  }
};

/**
 * Return vertices of polygon-like shapes.
 */
export const polygonLikePoints = ([t, s]: [string, any]): ad.Pt2[] => {
  if (t === "Polygon") return s.points.contents;
  else if (shapedefs[t].isLinelike) return [s.start.contents, s.end.contents];
  else if (shapedefs[t].isRectlike) {
    // TODO: add support for rotated rectangles
    const bbox = bboxFromShape([t, s]);
    const corners = BBox.corners(bbox);
    return [
      corners.topRight,
      corners.topLeft,
      corners.bottomLeft,
      corners.bottomRight,
    ];
  } else {
    throw new Error(`${t} not supported for polygonLikePoints.`);
  }
};

/**
 * Return outward unit normal vector to `lineSegment` with respect to `insidePoint`.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 */
export const outwardUnitNormal = (
  lineSegment: ad.Num[][],
  insidePoint: ad.Num[]
): ad.Num[] => {
  const normal = ops.vnormalize(
    ops.rot90(ops.vsub(lineSegment[1], lineSegment[0]))
  );
  const insideValue = ops.vdot(ops.vsub(insidePoint, lineSegment[0]), normal);
  return ops.vmul(neg(msign(insideValue)), normal);
};
