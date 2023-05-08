import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  lt,
  max,
  maxN,
  mul,
  neg,
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Circle } from "../shapes/Circle";
import { Ellipse } from "../shapes/Ellipse";
import { Polygon } from "../shapes/Polygon";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { circleToImplicitEllipse, ellipseToImplicit } from "./ImplicitShapes";
import {
  containsConvexPolygonPoints,
  overlappingImplicitEllipses,
} from "./Minkowski";
import { bboxFromShape, shapeCenter } from "./Queries";
import {
  atDistOutside,
  isLinelike,
  noIntersectCircles,
  pointInBox,
  Rectlike,
} from "./Utils";

// -------- Ovelapping helpers

/**
 * Require that ellipse `s1` overlaps ellipse `s2` with some overlap `overlap`.
 */
export const overlappingEllipse = (
  s1: Ellipse<ad.Num>,
  s2: Ellipse<ad.Num>,
  overlap: ad.Num
): ad.Num => {
  // HACK: An arbitrary factor `Math.PI / 3` has been added
  // to minimize the probability of obtaining a lower degree
  // polynomial in the Minkowski penalty for implicit shapes.
  const d = ops.vdist(s1.center.contents, s2.center.contents);
  const factor = div(1, add(1, d));
  const ei1 = ellipseToImplicit(s1, neg(overlap), mul(Math.PI / 3, factor));
  const ei2 = ellipseToImplicit(s2, 0, factor);
  return overlappingImplicitEllipses(ei1, ei2);
};

/**
 * Require that circle `s1` overlaps ellipse `s2` with some overlap `overlap`.
 */
export const overlappingCircleEllipse = (
  s1: Circle<ad.Num>,
  s2: Ellipse<ad.Num>,
  overlap: ad.Num = 0
): ad.Num => {
  // HACK: An arbitrary factor `Math.PI / 3` has been added
  // to minimize the probability of obtaining a lower degree
  // polynomial in the Minkowski penalty for implicit shapes.
  const d = ops.vdist(s1.center.contents, s2.center.contents);
  const factor = div(1, add(1, d));
  const ei1 = circleToImplicitEllipse(
    s1,
    neg(overlap),
    mul(Math.PI / 3, factor)
  );
  const ei2 = ellipseToImplicit(s2, 0, factor);
  return overlappingImplicitEllipses(ei1, ei2);
};

/**
 * Require that shape `s1` is at a distance of `distance` from shape `s2`.
 */
export const atDistLabel = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  distance: ad.Num
): ad.Num => {
  let pt;
  if (isLinelike(s1)) {
    // Position label close to the arrow's end
    pt = s1.end.contents;
  } else {
    // Only assume shape1 has a center
    pt = shapeCenter(s1);
  }

  // Get bounding box
  const rect = bboxFromShape(s2);

  return ifCond(
    pointInBox([pt[0], pt[1]], rect),
    // If the point is inside the box, push it outside w/ `noIntersect`
    noIntersectCircles(rect.center, rect.width, pt, 2),
    // If the point is outside the box, try to get the distance from the point to equal the desired distance
    atDistOutside([pt[0], pt[1]], rect, distance)
  );
};

// -------- Contains helpers

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsCircles = (
  s1: Circle<ad.Num>,
  s2: Circle<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  const d = ops.vdist(shapeCenter(s1), shapeCenter(s2));
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
  s1: Circle<ad.Num>,
  s2: Rectlike<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  // TODO: Remake using Minkowski penalties
  const s2BBox = bboxFromShape(s2);
  const d = ops.vdist(shapeCenter(s1), s2BBox.center);
  const textR = max(s2BBox.width, s2BBox.height);
  return add(sub(d, s1.r.contents), textR);
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsRectlikeCircle = (
  s1: Rectlike<ad.Num>,
  s2: Circle<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  // TODO: Remake using Minkowski penalties

  // collect constants
  const halfW = mul(0.5, s1.width.contents); // half rectangle width
  const halfH = mul(0.5, s1.height.contents); // half rectangle height
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
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  // TODO: Remake using Minkowski penalties
  const box1 = bboxFromShape(s1);
  const box2 = bboxFromShape(s2);
  const [[xl1, xr1], [xl2, xr2]] = [BBox.xRange(box1), BBox.xRange(box2)];
  const [[yl1, yr1], [yl2, yr2]] = [BBox.yRange(box1), BBox.yRange(box2)];
  return addN([
    ifCond(lt(xl1, xl2), 0, squared(sub(xl1, xl2))),
    ifCond(lt(xr2, xr1), 0, squared(sub(xr2, xr1))),
    ifCond(lt(yl1, yl2), 0, squared(sub(yl1, yl2))),
    ifCond(lt(yr2, yr1), 0, squared(sub(yr2, yr1))),
  ]);
};

/**
 * Require that a polygon `s1` contains another polygon `s2`.
 */
export const containsPolygonPolygon = (
  s1: Polygon<ad.Num>,
  s2: Polygon<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  return maxN(
    s2.points.contents.map((x) =>
      containsConvexPolygonPoints(s1.points.contents, x, padding)
    )
  );
};

/**
 * Require that a polygon `s1` contains circle `s2`.
 */
export const containsPolygonCircle = (
  s1: Polygon<ad.Num>,
  s2: Circle<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  return containsConvexPolygonPoints(
    s1.points.contents,
    s2.center.contents,
    add(padding, s2.r.contents)
  );
};

/**
 * Require that a circle `s1` contains polygon `s2`.
 */
export const containsCirclePolygon = (
  s1: Circle<ad.Num>,
  s2: Polygon<ad.Num>,
  padding: ad.Num = 0
): ad.Num => {
  return maxN(
    s2.points.contents.map((x) =>
      sub(add(ops.vdist(x, s1.center.contents), padding), s1.r.contents)
    )
  );
};
