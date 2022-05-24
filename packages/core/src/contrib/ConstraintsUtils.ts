import {
  containsPolygonPoints,
  convexPartitions,
  overlappingImplicitEllipses,
  overlappingPolygonPoints,
  overlappingPolygonPointsEllipse,
  rectangleDifference,
  rectangleSignedDistance,
} from "contrib/Minkowski";
import { bboxFromShape, polygonLikePoints, shapeCenter } from "contrib/Queries";
import { atDistOutside, noIntersectCircles, pointInBox } from "contrib/Utils";
import { ops } from "engine/Autodiff";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  lt,
  max,
  maxN,
  min,
  minN,
  mul,
  squared,
  sub,
} from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import { Circle } from "shapes/Circle";
import { Ellipse } from "shapes/Ellipse";
import { Equation } from "shapes/Equation";
import { Image } from "shapes/Image";
import { Line } from "shapes/Line";
import { Polygon } from "shapes/Polygon";
import { Rectangle } from "shapes/Rectangle";
import { shapedefs } from "shapes/Shapes";
import { Text } from "shapes/Text";
import * as ad from "types/ad";
import { circleToImplicitEllipse, ellipseToImplicit } from "./ImplicitShapes";

// -------- Ovelapping helpers

/**
 * Require that circle `s1` overlaps circle `s2` with some padding `padding`.
 */
export const overlappingCircles = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Circle],
  padding: ad.Num = 0
): ad.Num => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const o = [s1.r.contents, s2.r.contents, padding];
  return sub(d, addN(o));
};

/**
 * Require that polygon `s1` overlaps polygon `s2` with some padding `padding`.
 */
export const overlappingPolygons = (
  [t1, s1]: [string, Polygon | Rectangle | Text | Equation | Image | Line],
  [t2, s2]: [string, Polygon | Rectangle | Text | Equation | Image | Line],
  padding: ad.Num = 0
): ad.Num => {
  return overlappingPolygonPoints(
    polygonLikePoints([t1, s1]),
    polygonLikePoints([t2, s2]),
    padding
  );
};

/**
 * Require that bounding box of `s1` overlaps bounding box of `s2` with some padding `padding`.
 */
export const overlappingAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: ad.Num = 0
): ad.Num => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  // Get the Minkowski difference rectangle
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, padding);
  // Return the signed distance
  return rectangleSignedDistance(bottomLeft, topRight);
};

/**
 * Require that ellipse `s1` overlaps ellipse `s2` with some padding `padding`.
 */
export const overlappingEllipse = (
  [, s1]: [string, Ellipse],
  [, s2]: [string, Ellipse],
  padding: ad.Num
): ad.Num => {
  // HACK: An arbitrary factor `Math.PI / 3` has been added
  // to minimize the probability of obtaining a lower degree
  // polynomial in the Minkowski penalty for implicit shapes.
  const d = ops.vdist(s1.center.contents, s2.center.contents);
  const ei1 = ellipseToImplicit(s1, padding, div(Math.PI / 3, d));
  const ei2 = ellipseToImplicit(s2, 0, div(1, d));
  return overlappingImplicitEllipses(ei1, ei2);
};

/**
 * Require that rectangle `s1` overlaps circle `s2` with some padding `padding`.
 */
export const overlappingRectlikeCircle = (
  [t1, s1]: [string, Rectangle | Text | Equation | Image],
  [t2, s2]: [string, Circle],
  padding: ad.Num = 0
): ad.Num => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  // Get the Minkowski difference of inner rectangle
  const innerPadding = sub(padding, s2.r.contents);
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, innerPadding);
  // Return the signed distance
  const innerSDF = rectangleSignedDistance(bottomLeft, topRight);
  return sub(innerSDF, s2.r.contents);
};

/**
 * Require that polygon `s1` overlaps ellipse `s2` with some padding `padding`.
 */
export const overlappingPolygonEllipse = (
  [t1, s1]: [string, Polygon | Rectangle | Text | Equation | Image | Line],
  [, s2]: [string, Ellipse],
  padding: ad.Num = 0
): ad.Num => {
  const points = polygonLikePoints([t1, s1]);
  const cp = convexPartitions(points);
  return minN(cp.map((p) => overlappingPolygonPointsEllipse(p, s2, padding)));
};

/**
 * Require that circle `s1` overlaps ellipse `s2` with some padding `padding`.
 */
export const overlappingCircleEllipse = (
  [, s1]: [string, Circle],
  [, s2]: [string, Ellipse],
  padding: ad.Num = 0
): ad.Num => {
  // HACK: An arbitrary factor `Math.PI / 3` has been added
  // to minimize the probability of obtaining a lower degree
  // polynomial in the Minkowski penalty for implicit shapes.
  const d = ops.vdist(s1.center.contents, s2.center.contents);
  const ei1 = circleToImplicitEllipse(s1, padding, div(Math.PI / 3, d));
  const ei2 = ellipseToImplicit(s2, 0, div(1, d));
  return overlappingImplicitEllipses(ei1, ei2);
};

/**
 * Require that circle `s1` overlaps line `s2` with some padding `padding`.
 */
export const overlappingCircleLine = (
  [, s1]: [string, Circle],
  [, s2]: [string, Line],
  padding: ad.Num = 0
): ad.Num => {
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
  const u = ops.vsub(c, a); // u = c-a
  const v = ops.vsub(b, a); // v - b-a
  // h = clamp( <u,v>/<v,v>, 0, 1 )
  const h = max(0, min(1, div(ops.vdot(u, v), ops.vdot(v, v))));
  // d = | u - h*v |
  const d = ops.vnorm(ops.vsub(u, ops.vmul(h, v)));
  // return d - (r+o)
  return sub(d, add(r, o));
};

/**
 * Require that shape `s1` is at a distance of `distance` from shape `s2`.
 */
export const atDistLabel = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  distance: ad.Num
): ad.Num => {
  let pt;
  if (shapedefs[t1].isLinelike) {
    // Position label close to the arrow's end
    pt = s1.end.contents;
  } else {
    // Only assume shape1 has a center
    pt = shapeCenter([t1, s1]);
  }

  // Get bounding box
  const rect = bboxFromShape([t2, s2]);

  return ifCond(
    pointInBox(pt, rect),
    // If the point is inside the box, push it outside w/ `noIntersect`
    noIntersectCircles(rect.center, rect.width, pt, 2),
    // If the point is outside the box, try to get the distance from the point to equal the desired distance
    atDistOutside(pt, rect, distance)
  );
};

// -------- Contains helpers

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsCircles = (
  [t1, s1]: [string, Circle],
  [t2, s2]: [string, Circle],
  padding: ad.Num = 0
): ad.Num => {
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
  padding: ad.Num = 0
): ad.Num => {
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
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: ad.Num = 0
): ad.Num => {
  // TODO: Remake using Minkowski penalties
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
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
  [, s1]: [string, Polygon],
  [, s2]: [string, Polygon],
  padding: ad.Num = 0
): ad.Num => {
  return maxN(
    s2.points.contents.map((x) =>
      containsPolygonPoints(s1.points.contents, x, padding)
    )
  );
};

/**
 * Require that a polygon `s1` contains circle `s2`.
 */
export const containsPolygonCircle = (
  [, s1]: [string, Polygon],
  [, s2]: [string, Circle],
  padding: ad.Num = 0
): ad.Num => {
  return containsPolygonPoints(
    s1.points.contents,
    s2.center.contents,
    sub(padding, s2.r.contents)
  );
};

/**
 * Require that a circle `s1` contains polygon `s2`.
 */
export const containsCirclePolygon = (
  [, s1]: [string, Circle],
  [, s2]: [string, Polygon],
  padding: ad.Num = 0
): ad.Num => {
  return maxN(
    s2.points.contents.map((x) =>
      sub(add(ops.vdist(x, s1.center.contents), padding), s1.r.contents)
    )
  );
};
