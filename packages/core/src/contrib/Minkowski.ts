import { convexPartition, isClockwise } from "poly-partition";
import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  div,
  eq,
  ifCond,
  max,
  maxN,
  min,
  minN,
  mul,
  neg,
  polyRoots,
  sqrt,
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Ellipse } from "../shapes/Ellipse";
import * as ad from "../types/ad";
import { safe } from "../utils/Util";
import {
  ellipsePolynomial,
  ellipseToImplicit,
  halfPlaneToImplicit,
  ImplicitEllipse,
  implicitEllipseFunc,
  ImplicitHalfPlane,
  implicitHalfPlaneFunc,
  implicitIntersectionOfEllipsesFunc,
} from "./ImplicitShapes";
import { outwardUnitNormal } from "./Queries";
import { numsOf } from "./Utils";

/**
 * Compute coordinates of Minkowski sum of AABBs representing the first rectangle `box1` and the negative of the second rectangle `box2`.
 * Returns coordinates of the bottom left and top right corners.
 * Note: This is not the Minkowski difference in the classical sense, rather just a Minkowski sum of A and -B.
 * @param box1 First bounding box.
 * @param box2 Second bounding box.
 * @param padding Additional padding added to one of the boxes.
 */
export const rectangleDifference = (
  box1: BBox.BBox,
  box2: BBox.BBox,
  padding: ad.Num
): [ad.Pt2, ad.Pt2] => {
  // Prepare coordinates
  const [xa1, xa2, ya1, ya2] = [
    BBox.minX(box1),
    BBox.maxX(box1),
    BBox.minY(box1),
    BBox.maxY(box1),
  ];
  const [xb1, xb2, yb1, yb2] = [
    BBox.minX(box2),
    BBox.maxX(box2),
    BBox.minY(box2),
    BBox.maxY(box2),
  ];
  // Compute coordinates of the new rectangle
  const xs = [sub(xa1, xb1), sub(xa2, xb2), sub(xa1, xb2), sub(xa2, xb1)];
  const ys = [sub(ya1, yb1), sub(ya2, yb2), sub(ya1, yb2), sub(ya2, yb1)];
  // Return corners
  return [
    [sub(minN(xs), padding), sub(minN(ys), padding)], // Bottom left corner
    [add(maxN(xs), padding), add(maxN(ys), padding)], // Top right corner
  ];
};

/**
 * Return value of the Signed Distance Function (SDF) of a half-plane evaluated at the origin.
 * @param lineSegment Two points defining a side of the first polygon.
 * @param otherPoints All vertices of the second polygon.
 * @param insidePoint Point inside of the half-plane.
 * @param padding Padding added to the half-plane.
 */
export const halfPlaneSDF = (
  lineSegment: ad.Num[][],
  otherPoints: ad.Num[][],
  insidePoint: ad.Num[],
  padding: ad.Num
): ad.Num => {
  const normal = outwardUnitNormal(lineSegment, insidePoint);
  const alpha = ops.vdot(normal, lineSegment[0]);
  const alphaOther = maxN(otherPoints.map((p) => ops.vdot(normal, p)));
  return add(neg(add(alpha, alphaOther)), padding);
};

/**
 * Return value of one-sided Signed Distance Function (SDF) of the Minkowski sum of two polygons `p1` and `p2` evaluated at the origin.
 * Only half-planes related to sides of the first polygon `p1` are considered.
 * @param p1 Sequence of points defining the first polygon.
 * @param p2 Sequence of points defining the second polygon.
 * @param padding Padding around the Minkowski sum.
 */
const convexPolygonMinkowskiSDFOneSided = (
  p1: ad.Num[][],
  p2: ad.Num[][],
  padding: ad.Num
): ad.Num => {
  const center = ops.vdiv(p1.reduce(ops.vadd), p1.length);
  // Create a list of all sides given by two subsequent vertices
  const sides = Array.from({ length: p1.length }, (_, key) => key).map((i) => [
    p1[i],
    p1[i > 0 ? i - 1 : p1.length - 1],
  ]);
  const sdfs = sides.map((s: ad.Num[][]) =>
    halfPlaneSDF(s, p2, center, padding)
  );
  return maxN(sdfs);
};

/**
 * Return value of the Signed Distance Function (SDF) of the Minkowski sum of two polygons `p1` and `p2` evaluated at the origin.
 * @param p1 Sequence of points defining the first polygon.
 * @param p2 Sequence of points defining the second polygon.
 * @param padding Padding around the Minkowski sum.
 */
export const convexPolygonMinkowskiSDF = (
  p1: ad.Num[][],
  p2: ad.Num[][],
  padding: ad.Num
): ad.Num => {
  return max(
    convexPolygonMinkowskiSDFOneSided(p1, p2, padding),
    convexPolygonMinkowskiSDFOneSided(p2, p1, padding)
  );
};

/**
 * Returns list of convex polygons comprising the original polygon. Assumes that
 * the polygon shape remains fixed after this function is called; that is, some
 * transformations can be applied, but vertices cannot change independently.
 * @param p Sequence of points defining a simple polygon.
 */
export const convexPartitions = (p: ad.Num[][]): ad.Num[][][] => {
  if (p.length <= 3) {
    return [p];
  }

  // HACK (see also the note in types/ad): our autodiff engine isn't powerful
  // enough to let us run a convex partitioning algorithm inside the optimizer
  // loop, so instead, here we compile enough of the computation graph to
  // compute the initial positions of the polygon vertices, evaluate that using
  // the input values embedded in the children of the `ad.Num`s we were passed,
  // run a convex partitioning algorithm on those vertex positions, and cross
  // our fingers that this remains a valid convex partition as we optimize
  const coords = numsOf(p.flat());

  // map each point back to its original VecAD object; note, this depends on the
  // fact that two points with the same contents are considered different as
  // keys in a JavaScript Map, very scary!
  const pointMap = new Map(
    p.map((point, i) => {
      const j = 2 * i;
      return [{ x: coords[j], y: coords[j + 1] }, point];
    })
  );

  const contour = [...pointMap.keys()];
  if (isClockwise(contour)) {
    contour.reverse();
  }
  const convexPolygons = convexPartition(contour);

  return convexPolygons.map((poly) =>
    poly.map((point) =>
      safe(
        pointMap.get(point),
        "polygon decomposition unexpectedly created a new point"
      )
    )
  );
};

/**
 * Overlapping constraint function for polygon points with overlap `overlap`.
 * @param polygonPoints1 Sequence of points defining the first polygon.
 * @param polygonPoints2 Sequence of points defining the second polygon.
 * @param overlap Overlap applied to one of the polygons.
 */
export const overlappingPolygonPoints = (
  polygonPoints1: ad.Num[][],
  polygonPoints2: ad.Num[][],
  overlap: ad.Num = 0
): ad.Num => {
  const cp1 = convexPartitions(polygonPoints1);
  const cp2 = convexPartitions(
    polygonPoints2.map((p: ad.Num[]) => ops.vneg(p))
  );
  return maxN(
    cp1.map((p1) =>
      minN(cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2, neg(overlap))))
    )
  );
};

/**
 * Returns the signed distance from a rectangle at the origin.
 */
export const rectangleSignedDistance = (
  bottomLeft: ad.Pt2,
  topRight: ad.Pt2
): ad.Num => {
  // Calculate relative coordinates for rectangle signed distance
  const [xp, yp] = ops
    .vmul(0.5, ops.vadd(bottomLeft, topRight))
    .map((x) => absVal(x));
  const [xr, yr] = ops.vmul(0.5, ops.vsub(topRight, bottomLeft));
  const [xq, yq] = ops.vsub([xp, yp], [xr, yr]);
  // Positive distance (nonzero when the rectangle does not contain the origin)
  const e1 = sqrt(
    add(squared(max(sub(xp, xr), 0)), squared(max(sub(yp, yr), 0)))
  );
  // Negative distance (nonzero when the rectangle does contain the origin)
  const ne2 = min(max(xq, yq), 0);
  // Return the signed distance
  return add(e1, ne2);
};

/**
 * Constraint checking whether `point` is inside a convex polygon with vertices `polygonPoints`.
 * @param polygonPoints Sequence of points defining a convex polygon.
 * @param point Testing point.
 * @param padding Padding applied to the polygon.
 */
export const containsConvexPolygonPoints = (
  p1: ad.Num[][],
  p2: ad.Num[],
  padding: ad.Num
): ad.Num => {
  const center = ops.vdiv(p1.reduce(ops.vadd), p1.length);
  // Create a list of all sides given by two subsequent vertices
  const sides = Array.from({ length: p1.length }, (_, key) => key).map((i) => [
    p1[i],
    p1[i > 0 ? i - 1 : p1.length - 1],
  ]);
  const sdfs = sides.map((s: ad.Num[][]) =>
    halfPlaneSDF(s, [ops.vneg(p2)], center, neg(padding))
  );
  return maxN(sdfs);
};

/**
 * Return candidates for extremal points of the implicit functions.
 * @param ei Implicit ellipse parameters.
 * @param hpi Implicit half-plane parameters.
 * @param lambda Solution to the quadratic formula.
 */
const pointCandidates = (
  ei: ImplicitEllipse,
  hpi: ImplicitHalfPlane,
  lambda: ad.Num
): [ad.Num, ad.Num] => {
  const c = div(lambda, mul(2, sub(lambda, 1)));
  return [
    add(ei.x, mul(c, div(hpi.a, ei.a))),
    add(ei.y, mul(c, div(hpi.b, ei.b))),
  ];
};

/**
 * Helper for Signed Distance Function (SDF) of a polygon and ellipse.
 * @param lineSegment Two points defining the line segment.
 * @param ellipse Ellipse shape.
 * @param insidePoint Any point inside of the half-plane.
 * @param padding Padding around the Minkowski sum.
 */
export const halfPlaneEllipseSDF = (
  lineSegment: ad.Num[][],
  ellipse: Ellipse<ad.Num>,
  insidePoint: ad.Num[],
  padding: ad.Num
): ad.Num => {
  const hpi = halfPlaneToImplicit(lineSegment, insidePoint, 0);
  const ei = ellipseToImplicit(ellipse, padding);
  const e = div(
    add(mul(ei.b, squared(hpi.a)), mul(ei.a, squared(hpi.b))),
    mul(
      mul(ei.a, ei.b),
      mul(4, add(add(mul(ei.x, hpi.a), mul(ei.y, hpi.b)), sub(ei.c, hpi.c)))
    )
  );
  const ed = sqrt(div(e, add(1, e)));
  const point1 = pointCandidates(ei, hpi, add(1, ed));
  const point2 = pointCandidates(ei, hpi, sub(1, ed));
  const m1 = min(
    implicitHalfPlaneFunc(hpi, point1[0], point1[1]),
    implicitHalfPlaneFunc(hpi, point2[0], point2[1])
  );
  const m2 = max(
    implicitEllipseFunc(ei, ei.x, ei.y),
    implicitHalfPlaneFunc(hpi, ei.x, ei.y)
  );
  return min(m1, m2);
};

/**
 * Overlapping constraint function for of a polygon and ellipse.
 * @param polygonPoints Sequence of points defining a polygon.
 * @param ellipse Ellipse shape.
 * @param padding Padding applied to the ellipse.
 */
export const overlappingPolygonPointsEllipse = (
  polygonPoints: ad.Num[][],
  ellipse: Ellipse<ad.Num>,
  padding: ad.Num
): ad.Num => {
  const center = ops.vdiv(polygonPoints.reduce(ops.vadd), polygonPoints.length);
  // Create a list of all sides given by two subsequent vertices
  const sides = Array.from(
    { length: polygonPoints.length },
    (_, key) => key
  ).map((i) => [
    polygonPoints[i],
    polygonPoints[i > 0 ? i - 1 : polygonPoints.length - 1],
  ]);
  const sdfs = sides.map((s: ad.Num[][]) =>
    halfPlaneEllipseSDF(s, ellipse, center, padding)
  );
  return maxN(sdfs);
};

/**
 * Return candidates for extremal points of the implicit functions for ellipse-ellipse case.
 * @param ei1 Implicit ellipse parameters.
 * @param ei2 Implicit ellipse parameters.
 * @param lambda Solution to the quadratic formula.
 */
export const pointCandidatesEllipse = (
  ei1: ImplicitEllipse,
  ei2: ImplicitEllipse,
  lambda: ad.Num
): [ad.Num, ad.Num] => {
  const x = div(
    add(
      mul(ei1.x, ei1.a),
      mul(lambda, sub(mul(ei2.x, ei2.a), mul(ei1.x, ei1.a)))
    ),
    add(ei1.a, mul(lambda, sub(ei2.a, ei1.a)))
  );
  const y = div(
    add(
      mul(ei1.y, ei1.b),
      mul(lambda, sub(mul(ei2.y, ei2.b), mul(ei1.y, ei1.b)))
    ),
    add(ei1.b, mul(lambda, sub(ei2.b, ei1.b)))
  );
  return [x, y];
};

/**
 * Overlapping constraint function for two implicit ellipses.
 * @param ei1 First implicit ellipse.
 * @param ei2 Second implicit ellipse.
 */
export const overlappingImplicitEllipses = (
  ei1: ImplicitEllipse,
  ei2: ImplicitEllipse
): ad.Num => {
  const poly = ellipsePolynomial(ei1, ei2);
  const roots = polyRoots(poly).map((r) => ifCond(eq(r, r), r, 0));
  const points = roots.map((root: ad.Num) =>
    pointCandidatesEllipse(ei1, ei2, root)
  );
  const m1 = minN(
    points.map(([x, y]: [ad.Num, ad.Num]) =>
      implicitIntersectionOfEllipsesFunc(ei1, ei2, x, y)
    )
  );
  const m2 = min(
    implicitIntersectionOfEllipsesFunc(ei1, ei2, ei1.x, ei1.y),
    implicitIntersectionOfEllipsesFunc(ei1, ei2, ei2.x, ei2.y)
  );
  return min(m1, m2);
};
