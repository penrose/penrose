import { genCode, ops, secondaryGraph } from "engine/Autodiff";
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
  neg,
  sqrt,
  squared,
  sub,
} from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import { convexPartition, isClockwise } from "poly-partition";
import { Ellipse } from "shapes/Ellipse";
import * as ad from "types/ad";
import { safe } from "utils/Util";

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
 * Return -1.0 for negative number, +1.0 otherwise.
 */
const signOf = (x: ad.Num): ad.Num => {
  const negative = lt(x, 0);
  return ifCond(negative, -1, 1);
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
  return ops.vmul(neg(signOf(insideValue)), normal);
};

/**
 * Return value of the Signed Distance Function (SFD) of a half-plane evaluated at the origin.
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
  return neg(addN([alpha, alphaOther, padding]));
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
  const g = secondaryGraph(p.flat());
  const inputs = [];
  for (const v of g.nodes.keys()) {
    if (typeof v !== "number" && v.tag === "Input") {
      inputs[v.key] = v.val;
    }
  }
  const coords = genCode(g)(inputs).secondary;

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
 * Overlapping constraint function for polygon points with padding `padding`.
 * @param polygonPoints1 Sequence of points defining the first polygon.
 * @param polygonPoints2 Sequence of points defining the second polygon.
 * @param padding Padding applied to one of the polygons.
 */
export const overlappingPolygonPoints = (
  polygonPoints1: ad.Num[][],
  polygonPoints2: ad.Num[][],
  padding: ad.Num = 0
): ad.Num => {
  const cp1 = convexPartitions(polygonPoints1);
  const cp2 = convexPartitions(
    polygonPoints2.map((p: ad.Num[]) => ops.vneg(p))
  );
  return maxN(
    cp1.map((p1) =>
      minN(cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2, padding)))
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
const containsConvexPolygonPoints = (
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
    halfPlaneSDF(s, [ops.vneg(p2)], center, padding)
  );
  return maxN(sdfs);
};

/**
 * Constraint checking whether `point` is inside a polygon with vertices `polygonPoints`.
 * @param polygonPoints Sequence of points defining a polygon.
 * @param point Testing point.
 * @param padding Padding applied to the polygon.
 */
export const containsPolygonPoints = (
  polygonPoints: ad.Num[][],
  point: ad.Num[],
  padding: ad.Num = 0
): ad.Num => {
  const cp1 = convexPartitions(polygonPoints);
  return maxN(cp1.map((p1) => containsConvexPolygonPoints(p1, point, padding)));
};

/**
 * Parameters of implicitly defined ellipse:
 * `a * (X - x)^2 + b * (Y - y)^2 = c`
 */
interface ImplicitEllipse {
  a: ad.Num;
  b: ad.Num;
  c: ad.Num;
  x: ad.Num;
  y: ad.Num;
}

/**
 * Parameters of implicitly defined half-plane:
 * `a * X + b * Y <= c`
 */
interface ImplicitHalfPlane {
  a: ad.Num;
  b: ad.Num;
  c: ad.Num;
}

/**
 * Evaluate the implicit function for an ellipse at point with coordinates `x` and `y`.
 * @param ei Implicit ellipse parameters.
 * @param x X-coordinate.
 * @param y Y-coordinate.
 */
const implicitEllipseFunc = (
  ei: ImplicitEllipse,
  x: ad.Num,
  y: ad.Num
): ad.Num => {
  return sub(
    add(mul(ei.a, squared(sub(x, ei.x))), mul(ei.b, squared(sub(y, ei.y)))),
    ei.c
  );
};

/**
 * Evaluate the implicit function for an half-plane at point with coordinates `x` and `y`.
 * @param hpi Implicit half-plane parameters.
 * @param x X-coordinate.
 * @param y Y-coordinate.
 */
const implicitHalfPlaneFunc = (
  hpi: ImplicitHalfPlane,
  x: ad.Num,
  y: ad.Num
): ad.Num => {
  return sub(add(mul(hpi.a, x), mul(hpi.b, y)), hpi.c);
};

/**
 * Return implicit half-plane parameters given a line and a point inside the half-plane.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 * @param padding Padding around the Half-plane.
 */
const halfPlaneToImplicit = (
  lineSegment: ad.Num[][],
  insidePoint: ad.Num[],
  padding: ad.Num
): ImplicitHalfPlane => {
  const normal = outwardUnitNormal(lineSegment, insidePoint);
  return {
    a: normal[0],
    b: normal[1],
    c: sub(ops.vdot(normal, lineSegment[0]), padding),
  };
};

/**
 * Return implicit ellipse parameters from an explicit representation.
 * @param ellipse Explicit ellipse shape.
 */
const ellipseToImplicit = (ellipse: Ellipse): ImplicitEllipse => {
  const rx = ellipse.rx.contents;
  const ry = ellipse.ry.contents;
  return {
    a: div(ry, rx),
    b: div(rx, ry),
    c: mul(rx, ry),
    x: ellipse.center.contents[0],
    y: ellipse.center.contents[1],
  };
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
    sub(ei.x, mul(c, div(hpi.a, ei.a))),
    sub(ei.y, mul(c, div(hpi.b, ei.b))),
  ];
};

/**
 * Helper for Signed Distance Function (SFD) of a polygon and ellipse.
 * @param lineSegment Two points defining the line segment.
 * @param ellipse Ellipse shape.
 * @param insidePoint Any point inside of the half-plane.
 * @param padding Padding around the Minkowski sum.
 */
export const halfPlaneEllipseSDF = (
  lineSegment: ad.Num[][],
  ellipse: Ellipse,
  insidePoint: ad.Num[],
  padding: ad.Num
): ad.Num => {
  const hpi = halfPlaneToImplicit(lineSegment, insidePoint, padding);
  const ei = ellipseToImplicit(ellipse);
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
 * Overlapping constraint function for polygon points and ellipse with padding `padding`.
 * @param polygonPoints Sequence of points defining a polygon.
 * @param ellipse Ellipse shape.
 * @param padding Padding around the Minkowski sum.
 */
export const overlappingPolygonPointsEllipse = (
  polygonPoints: ad.Num[][],
  ellipse: Ellipse,
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
