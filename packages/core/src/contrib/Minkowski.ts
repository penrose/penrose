import {
  add,
  addN,
  constOf,
  neg,
  ops,
  sub,
  varOf,
  ifCond,
  lt,
  max,
  minN,
  maxN,
} from "engine/Autodiff";
import * as _ from "lodash";
import { VarAD } from "types/ad";
import * as BBox from "engine/BBox";

/**
 * Compute coordinates of Minkowski sum of AABBs representing the first rectangle `box1` and the negative of the second rectangle `box2`.
 * Note: This is not the Minkowski difference in the classical sense, rather just a Minkowski sum of A and -B.
 * @param box1 First bounding box.
 * @param box2 Second bounding box.
 * @param padding Additional padding added to one of the boxes.
 */
export const rectangleDifference = (
  box1: BBox.BBox, 
  box2: BBox.BBox,
  padding: VarAD
): VarAD[][] => {
  // Prepare coordinates
  const [xa1, xa2, ya1, ya2] = [
    sub(BBox.minX(box1), padding),
    add(BBox.maxX(box1), padding),
    sub(BBox.minY(box1), padding),
    add(BBox.maxY(box1), padding),
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
    [minN(xs), minN(ys)],
    [maxN(xs), maxN(ys)],
  ];
};

/**
 * Return -1.0 for negative number, +1.0 otherwise.
 */
const signOf = (x: VarAD): VarAD => {
  const negative = lt(x, constOf(0.0));
  return ifCond(negative, constOf(-1.0), constOf(1.0));
};

/**
 * Return outward unit normal vector to `lineSegment` with respect to `insidePoint`.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 */
const outwardUnitNormal = (
  lineSegment: VarAD[][],
  insidePoint: VarAD[]
): VarAD[] => {
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
const halfPlaneSDF = (
  lineSegment: VarAD[][],
  otherPoints: VarAD[][],
  insidePoint: VarAD[],
  padding: VarAD
): VarAD => {
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
  p1: VarAD[][],
  p2: VarAD[][],
  padding: VarAD
): VarAD => {
  const center = ops.vdiv(p1.reduce(ops.vadd), varOf(p1.length));
  const sides = Array.from({ length: p1.length }, (_, i) => i).map((i) => [
    p1[i],
    p1[i > 0 ? i - 1 : p1.length - 1],
  ]);
  const sdfs = sides.map((s: VarAD[][]) => halfPlaneSDF(s, p2, center, padding));
  return maxN(sdfs);
};

/**
 * Return value of the Signed Distance Function (SDF) of the Minkowski sum of two polygons `p1` and `p2` evaluated at the origin.
 * @param p1 Sequence of points defining the first polygon.
 * @param p2 Sequence of points defining the second polygon.
 * @param padding Padding around the Minkowski sum.
 */
export const convexPolygonMinkowskiSDF = (
  p1: VarAD[][], 
  p2: VarAD[][],
  padding: VarAD
): VarAD => {
  return max(
    convexPolygonMinkowskiSDFOneSided(p1, p2, padding),
    convexPolygonMinkowskiSDFOneSided(p2, p1, padding)
  );
};

/**
 * Returns list of convex polygons comprising the original polygon.
 * @param p Sequence of points defining a polygon.
 */
export const convexPartitions = (p: VarAD[][]): VarAD[][][] => {
  // TODO: Add convex partitioning algorithm for polygons.
  // See e.g.: https://doc.cgal.org/Manual/3.2/doc_html/cgal_manual/Partition_2/Chapter_main.html#Section_9.3
  return [p];
};
