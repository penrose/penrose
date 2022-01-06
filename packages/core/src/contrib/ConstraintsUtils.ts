import { convexPolygonMinkowskiSDF, convexPartitions, rectangleDifference } from "contrib/Minkowski";
import {
  ifCond, 
  lt,
  neg,
  ops,
  sub,
  mul,
  div,
  min,
  max,
  minN,
  maxN,
  absVal,
  add,
  addN,
  sqrt,
  squared,
  constOf,
} from "engine/Autodiff";
import { shapeCenter, bboxFromShape } from "contrib/Queries";
import { VarAD } from "types/ad";
import * as BBox from "engine/BBox";
import { linePts } from "utils/OtherUtils";

// -------- Ovelapping helpers

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingLines = (
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const oneSimplex1 = { points: { contents: [s1.start.contents, s1.end.contents] } };
  const oneSimplex2 = { points: { contents: [s2.start.contents, s2.end.contents] } };
  return overlappingPolygons(['1-simplex', oneSimplex1], ['1-simplex', oneSimplex2], padding);
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
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
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const cp1 = convexPartitions(s1.points.contents);
  const cp2 = convexPartitions(s2.points.contents.map((p: VarAD[]) => ops.vneg(p)));
  return maxN(
    cp1.map((p1) => minN(
      cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2, padding))
    ))
  );
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
 export const overlappingRectlikeCircle = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const s1BBox = bboxFromShape([t1, s1]);
  const textR = max(s1BBox.w, s1BBox.h);
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  return sub(d, add(add(s2.r.contents, textR), padding));
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  const box1 = bboxFromShape([t1, s1]);
  const box2 = bboxFromShape([t2, s2]);
  const [pc1, pc2] = rectangleDifference(box1, box2, padding);
  const [xp, yp] = ops.vmul(constOf(0.5), ops.vadd(pc1, pc2));
  const [xr, yr] = ops.vmul(constOf(0.5), ops.vsub(pc2, pc1));
  const [xq, yq] = ops.vsub([absVal(xp), absVal(yp)], [xr, yr]);
  const e1 = sqrt(add(constOf(10e-15), add(
    squared(max(sub(xp, xr), constOf(0.0))),
    squared(max(sub(yp, yr), constOf(0.0)))
  )));
  const e2 = neg(min(max(xq, yq), constOf(0.0)));
  return sub(e1, e2);
};

// -------- Contains helpers

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
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
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties
  const s2BBox = bboxFromShape([t2, s2]);
  const d = ops.vdist(shapeCenter([t1, s1]), s2BBox.center);
  const textR = max(s2BBox.w, s2BBox.h);
  return add(sub(d, s1.r.contents), textR);
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsRectlikeCircle = (
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties

  // collect constants
  const halfW = mul(constOf(0.5), s1.w.contents); // half rectangle width
  const halfH = mul(constOf(0.5), s1.h.contents); // half rectangle height
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
export const containsSquareCircle = (
  [, s1]: [string, any],
  [t2, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties

  // dist (outerx, outery) (innerx, innery) - (0.5 * outer.side - inner.radius)
  const sq = s1.center.contents;
  const d = ops.vdist(sq, shapeCenter([t2, s2]));
  return sub(d, sub(mul(constOf(0.5), s1.side.contents), s2.r.contents));
};

/**
 * Require that a shape `s1` contains another shape `s2`.
 */
export const containsSquareLinelike = (
  [t1, s1]: [string, any],
  [, s2]: [string, any],
  padding: VarAD = constOf(0.0)
): VarAD => {
  // TODO: Remake using Minkowski penalties
  const [[startX, startY], [endX, endY]] = linePts(s2);
  const [x, y] = shapeCenter([t1, s1]);
  const r = div(s1.side.contents, constOf(2.0));
  const [lx, ly] = [mul(sub(x, r), padding), mul(sub(y, r), padding)];
  const [rx, ry] = [mul(add(x, r), padding), mul(add(y, r), padding)];
  return addN([
    mul(sub(startX, lx), sub(startX, rx)),
    mul(sub(startY, ly), sub(startY, ry)),
    mul(sub(endX, lx), sub(endX, rx)),
    mul(sub(endY, ly), sub(endY, ry))
  ]);
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
    ifCond(lt(yr2, yr1), constOf(0), squared(sub(yr2, yr1)))
  ]);
};
