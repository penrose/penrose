import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  lte,
  mul,
  sign,
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import { Path } from "../shapes/Path";
import { Polygon } from "../shapes/Polygon";
import { Polyline } from "../shapes/Polyline";
import * as ad from "../types/ad";
import {
  consecutiveTriples,
  consecutiveTuples,
  extractPoints,
  isClosed,
} from "./Utils";

/**
 * All values in the list should be equal
 */
export const equivalued = (x: ad.Num[]): ad.Num => {
  const mean = div(addN(x), x.length);
  return addN(x.map((y: ad.Num) => squared(sub(y, mean))));
};

/**
 * Returns discrete curvature approximation given three consecutive points
 */
export const curvature = (
  p1: [ad.Num, ad.Num],
  p2: [ad.Num, ad.Num],
  p3: [ad.Num, ad.Num]
): ad.Num => {
  const v1 = ops.vsub(p2, p1);
  const v2 = ops.vsub(p3, p2);
  const angle = ops.angleFrom(v1, v2);
  return angle;
  // Alternative discrete curvature definition
  // return mul(2, sin(div(angle, 2)));
  // return mul(2, tan(div(angle, 2)));
};

/**
 * Returns the total length of polygonal chain given its nodes
 */
export const perimeter = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const sides = consecutiveTuples(points, closed);
  return addN(sides.map(([p1, p2]: [ad.Num, ad.Num][]) => ops.vdist(p1, p2)));
};

/**
 * Returns the signed area enclosed by a polygonal chain given its nodes
 */
export const signedArea = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const sides = consecutiveTuples(points, closed);
  return mul(
    0.5,
    addN(
      sides.map(([p1, p2]: [ad.Num, ad.Num][]) =>
        sub(mul(p1[0], p2[1]), mul(p1[1], p2[0]))
      )
    )
  );
};

/**
 * Returns the turning number of polygonal chain given its nodes
 */
export const turningNumber = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  return div(totalCurvature(points, closed), 2 * Math.PI);
};

/**
 * Returns the isoperimetric ratio (perimeter squared divided by enclosed area)
 */
export const isoperimetricRatio = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  return div(squared(perimeter(points, closed)), signedArea(points, closed));
};

/**
 * Returns integral of curvature along the curve
 */
export const totalCurvature = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) => curvature(p1, p2, p3))
  );
};

/**
 * Returns integral of curvature squared along the curve
 */
export const elasticEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      mul(
        squared(curvature(p1, p2, p3)),
        mul(0.5, mul(ops.vdist(p1, p2), ops.vdist(p2, p3)))
      )
    )
  );
};

export const constrDictCurves = {
  /**
   * The shape should be locally convex (all angles between consecutive edges would have the same sign)
   */
  isLocallyConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const triples = consecutiveTriples(points, isClosed([t, s]));
    const angles = triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
    );
    const meanSign = sign(addN(angles));
    return addN(
      angles.map((angle: ad.Num) =>
        ifCond(lte(mul(meanSign, angle), 0), squared(angle), 0)
      )
    );
  },

  /**
   * The enclosed area should be convex
   * Implemented using local convexity penalty (`localPenalty`) and global turning number penalty (`globalPenalty`)
   */
  isConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const localPenalty = constrDictCurves.isLocallyConvex([t, s]);
    const points = extractPoints([t, s]);
    const tn = turningNumber(points, isClosed([t, s]));
    const globalPenalty = squared(sub(absVal(tn), 1));
    return add(localPenalty, globalPenalty);
  },

  /**
   * All edges should have the same length
   */
  isEquilateral: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveTuples(points, isClosed([t, s]));
    return equivalued(hs.map(([p1, p2]: ad.Num[][]) => ops.vdist(p1, p2)));
  },

  /**
   * All angles between consecutive edges should be equal
   */
  isEquiangular: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveTriples(points, isClosed([t, s]));
    return equivalued(
      hs.map(([p1, p2, p3]: ad.Num[][]) =>
        ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
      )
    );
  },
};
