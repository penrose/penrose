import { overlappingPolygonPoints, rectangleDifference } from "contrib/Minkowski";
import { ops, constOf } from "engine/Autodiff";
import { shapeCenter, bboxFromShape, polygonLikePoints } from "contrib/Queries";
import { Pt2, VarAD } from "types/ad";
import * as BBox from "engine/BBox";
import {
  neg,
  sub,
  mul,
  div,
  min,
  max,
  absVal,
  add,
  addN,
  ifCond,
  lt,
  sqrt,
  squared,
} from "engine/AutodiffFunctions";
import { Circle } from "shapes/Circle";
import { Polygon } from "shapes/Polygon";
import { Rectangle } from "shapes/Rectangle";
import { Text } from "shapes/Text";
import { Equation } from "shapes/Equation";
import { Image } from "shapes/Image";
import { Line } from "shapes/Line";

// -------- Ovelapping helpers

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingCircles = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Circle],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const o = [s1.r.contents, s2.r.contents, padding];
  return sub(d, addN(o));
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingPolygons = (
  [t1, s1]: [string, Polygon | Rectangle | Text | Equation | Image | Line],
  [t2, s2]: [string, Polygon | Rectangle | Text | Equation | Image | Line],
  padding: VarAD = constOf(0.0)
): VarAD => {
  return overlappingPolygonPoints(
    polygonLikePoints([t1, s1]),
    polygonLikePoints([t2, s2]),
    padding
  );
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingCircleLine = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Line],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // collect constants
  const c = s1.center.contents;
  const r = s1.r.contents;
  const a = s2.start.contents;
  const b = s2.end.contents;
  const o = padding;
   
  // Return the distance between the circle center c and the
  // segment ab, minus the circle radius r and offset o.  This
  // quantity will be negative of the circular disk intersects
  // a thickened "capsule" associated with the line (of radius o).
  // The expression for the point-segment distance d comes from
  // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
  // (see "Segment - exact").
  const u = ops.vsub(c,a); // u = c-a
  const v = ops.vsub(b,a); // v - b-a
  // h = clamp( <u,v>/<v,v>, 0, 1 )
  const h = max( constOf(0.), min( constOf(1.), div( ops.vdot(u,v), ops.vdot(v,v) ) ));
  // d = | u - h*v |
  const d = ops.vnorm( ops.vsub( u, ops.vmul(h,v) ) );
  // return d - (r+o)
  return sub( d, add( r, o ));
};

/**
 * Returns the signed distance from a rectangle at the origin.
 */
 export const rectangleSignedDistance = (
  bottomLeft: Pt2,
  topRight: Pt2
): VarAD => {
  // Calculate relative coordinates for rectangle signed distance
  const [xp, yp] = ops.vmul(constOf(0.5), ops.vadd(bottomLeft, topRight)).map((x) => absVal(x));
  const [xr, yr] = ops.vmul(constOf(0.5), ops.vsub(topRight, bottomLeft))
  const [xq, yq] = ops.vsub([xp, yp], [xr, yr]);
  // Positive distance (nonzero when the rectangle does not contain the origin)
  const e1 = sqrt(add(constOf(10e-15),add(
    squared(max(sub(xp, xr), constOf(0.0))),
    squared(max(sub(yp, yr), constOf(0.0)))
  )));
  // Negative distance (nonzero when the rectangle does contain the origin)
  const ne2 = min(max(xq, yq), constOf(0.0));
  // Return the signed distance
  return add(e1, ne2);
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  // Get the Minkowski difference rectangle
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, padding);
  // Return the signed distance
  return rectangleSignedDistance(bottomLeft, topRight);
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingRectangleCircle = (
  [t1, s1]: [string, Rectangle],
  [t2, s2]: [string, Circle],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  // Get the Minkowski difference of inner rectangle
  const innerPadding = sub(padding, box2.width);
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, innerPadding);
  // Return the signed distance
  const innerSDF = rectangleSignedDistance(bottomLeft, topRight);
  return sub(innerSDF, box2.width);
};

// -------- Contains helpers

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsCircles = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Circle],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const o = padding
    ? sub(sub(s1.r.contents, s2.r.contents), padding)
    : sub(s1.r.contents, s2.r.contents);
  const res = sub(d, o);
  return res;
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsCircleRectlike = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Rectangle | Text | Equation | Image],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties
  const s2BBox = bboxFromShape([t2, s2]);
  const d = ops.vdist(shapeCenter([t1, s1]), s2BBox.center);
  const textR = max(s2BBox.width, s2BBox.height);
  return add(sub(d, s1.r.contents), textR);
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsRectlikeCircle = (
  [, s1]: [string, Rectangle | Text | Equation | Image],
  [, s2]: [string, Circle],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties

  // collect constants
  const halfW = mul(constOf(0.5), s1.width.contents); // half rectangle width
  const halfH = mul(constOf(0.5), s1.height.contents); // half rectangle height
  const [rx, ry] = s1.center.contents; // rectangle center
  const r = s2.r.contents; // circle radius
  const [cx, cy] = s2.center.contents; // circle center

  // Return maximum violation in either the x- or y-direction.
  // In each direction, the distance from the circle center (cx,cy) to
  // the rectangle center (rx,ry) must be no greater than the size of
  // the rectangle (w/h), minus the radius of the circle (r) and the
  // padding (o).  We can compute this violation via the function
  //    max( |cx-rx| - (w/2-r-o),
  //         |cy-ry| - (h/2-r-o) )
  return max(
    sub(absVal(sub(cx, rx)), sub(sub(halfW, r), padding)),
    sub(absVal(sub(cy, ry)), sub(sub(halfH, r), padding))
  );
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  const [[xl1, xr1], [xl2, xr2]] = [BBox.xRange(box1), BBox.xRange(box2)];
  const [[yl1, yr1], [yl2, yr2]] = [BBox.yRange(box1), BBox.yRange(box2)];
  return addN([
    ifCond(lt(xl1, xl2), constOf(0), squared(sub(xl1, xl2))),
    ifCond(lt(xr2, xr1), constOf(0), squared(sub(xr2, xr1))),
    ifCond(lt(yl1, yl2), constOf(0), squared(sub(yl1, yl2))),
    ifCond(lt(yr2, yr1), constOf(0), squared(sub(yr2, yr1))),
  ]);
};
