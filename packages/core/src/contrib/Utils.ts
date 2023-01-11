import { EPS_DENOM, genCodeSync, ops, secondaryGraph } from "engine/Autodiff";
import {
  absVal,
  add,
  and,
  div,
  gt,
  ifCond,
  lt,
  max,
  min,
  mul,
  or,
  squared,
  sub,
} from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import _ from "lodash";
import { Path } from "shapes/Path";
import { Polygon } from "shapes/Polygon";
import { Polyline } from "shapes/Polyline";
import * as ad from "types/ad";

/**
 * Require that a shape at `center1` with radius `r1` not intersect a shape at `center2` with radius `r2` with optional padding `padding`. (For a non-circle shape, its radius should be half of the shape's general "width")
 */
export const noIntersectCircles = (
  center1: ad.Num[],
  r1: ad.Num,
  center2: ad.Num[],
  r2: ad.Num,
  padding = 10
): ad.Num => {
  // noIntersect [[x1, y1, s1], [x2, y2, s2]] = - dist (x1, y1) (x2, y2) + (s1 + s2 + 10)
  const res = add(add(r1, r2), padding);
  return sub(res, ops.vdist(center1, center2));
};

// ------- Polygon-related helpers

/**
 * Return true iff `p` is in rect `b`.
 */
export const pointInBox = (p: ad.Pt2, rect: BBox.BBox): ad.Bool => {
  return and(
    and(lt(BBox.minX(rect), p[0]), lt(p[0], BBox.maxX(rect))),
    and(lt(BBox.minY(rect), p[1]), lt(p[1], BBox.maxY(rect)))
  );
};

/**
 * Helper function for atDist constraint.
 * If the point is outside the box, try to get the distance from the point to equal the desired distance.
 */
export const atDistOutside = (
  pt: ad.Pt2,
  rect: BBox.BBox,
  offset: ad.Num
): ad.Num => {
  const dsqRes = dsqBP(pt, rect);
  const WEIGHT = 1;
  return mul(WEIGHT, absVal(sub(dsqRes, squared(offset))));
};

/**
 * Assuming `rect` is an axis-aligned bounding box (AABB),
 * compute the positive distance squared from point `p` to box `rect` (not the signed distance).
 * https://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
 */
const dsqBP = (p: ad.Pt2, rect: BBox.BBox): ad.Num => {
  const dx = max(
    max(sub(BBox.minX(rect), p[0]), 0),
    sub(p[0], BBox.maxX(rect))
  );
  const dy = max(
    max(sub(BBox.minY(rect), p[1]), 0),
    sub(p[1], BBox.maxY(rect))
  );
  return add(squared(dx), squared(dy));
};

// ------- 1D

/**
 * Return the amount of overlap between two intervals in R. (0 if none)
 */
export const overlap1D = (
  [l1, r1]: [ad.Num, ad.Num],
  [l2, r2]: [ad.Num, ad.Num]
): ad.Num => {
  const d = (x: ad.Num, y: ad.Num) => absVal(sub(x, y)); // Distance between two reals
  // const d = (x: ad.Num, y: ad.Num) => squared(sub(x, y)); // Distance squared, if just the asymptotic behavior matters
  return ifCond(
    or(lt(r1, l2), lt(r2, l1)), // disjoint intervals => overlap is 0
    0,
    min(d(l2, r1), d(l1, r2))
  );
};

/**
 * Return numerically-encoded boolean indicating whether `x \in [l, r]`.
 */
export const inRange = (x: ad.Num, l: ad.Num, r: ad.Num): ad.Bool => {
  return and(gt(x, l), lt(x, r));
};

/**
 * Linearly interpolate between left `l` and right `r` endpoints, at fraction `k` of interpolation.
 */
const lerp = (l: ad.Num, r: ad.Num, k: ad.Num): ad.Num => {
  // TODO: Rewrite the lerp code to be more concise
  return add(mul(l, sub(1, k)), mul(r, k));
};

/**
 * Linearly interpolate between vector `l` and vector `r` endpoints, at fraction `k` of interpolation.
 */
const lerp2 = (l: ad.Num[], r: ad.Num[], k: ad.Num): [ad.Num, ad.Num] => {
  return [lerp(l[0], r[0], k), lerp(l[1], r[1], k)];
};

/**
 * Sample a line `line` at `NUM_SAMPLES` points uniformly.
 */
export const sampleSeg = (line: ad.Num[][]): ad.Pt2[] => {
  const NUM_SAMPLES = 15;
  const NUM_SAMPLES2 = 1 + NUM_SAMPLES;
  // TODO: Check that this covers the whole line, i.e. no off-by-one error
  const samples = _.range(1 + NUM_SAMPLES).map((i) => {
    const k = div(i, NUM_SAMPLES2);
    return lerp2(line[0], line[1], k);
  });

  return samples;
};

/**
 * Repel a vector `a` from a vector `b` with weight `c`.
 */
export const repelPoint = (c: ad.Num, a: ad.Num[], b: ad.Num[]): ad.Num =>
  div(c, add(ops.vdistsq(a, b), EPS_DENOM));

/**
 * Clamp `x` in range `[l, r]`.
 */
export const clamp = ([l, r]: [number, number], x: ad.Num): ad.Num => {
  return max(l, min(r, x));
};

/**
 * Return the closest point on segment `[start, end]` to point `pt`.
 */
export const closestPt_PtSeg = (
  pt: ad.Num[],
  [start, end]: ad.Num[][]
): ad.Num[] => {
  const EPS0 = 10e-3;
  const lensq = max(ops.vdistsq(start, end), EPS0); // Avoid a divide-by-0 if the line is too small

  // If line seg looks like a point, the calculation just returns (something close to) `v`
  const dir = ops.vsub(end, start);
  // t = ((p -: v) `dotv` dir) / lensq -- project vector onto line seg and normalize
  const t = div(ops.vdot(ops.vsub(pt, start), dir), add(lensq, EPS_DENOM));
  const t1 = clamp([0.0, 1.0], t);

  // v +: (t' *: dir) -- walk along vector of line seg
  return ops.vadd(start, ops.vmul(t1, dir));
};

/**
 * Get numerical values of nodes in the computation graph. This function calls `secondaryGraph` to construct a partial computation graph and runs `genCode` to generate code to evaluate the values.
 *
 * @param xs nodes in the computation graph
 * @returns a list of `number`s corresponding to nodes in `xs`
 */
export const numsOf = (xs: ad.Num[]): number[] => {
  const g = secondaryGraph(xs);
  const inputs = [];
  for (const v of g.nodes.keys()) {
    if (typeof v !== "number" && v.tag === "Input") {
      inputs[v.key] = v.val;
    }
  }
  const f = genCodeSync(g);
  return f.call(inputs).secondary;
};

export const numOf = (x: ad.Num): number => {
  return numsOf([x])[0];
};

/**
 * Return list of tuples of consecutive points
 */
export const consecutiveTuples = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): [ad.Num, ad.Num][][] => {
  return Array.from({ length: points.length }, (_, key) => key).map((i) => [
    points[i > 0 ? i - 1 : points.length - 1],
    points[i],
  ]);
};

/**
 * Return list of triples of consecutive points
 */
export const consecutiveTriples = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): [ad.Num, ad.Num][][] => {
  return Array.from({ length: points.length }, (_, key) => key).map((i) => [
    points[i > 1 ? i - 2 : points.length - 2],
    points[i > 0 ? i - 1 : points.length - 1],
    points[i],
  ]);
};

/**
 * Return indicator of closed Polyline, Polygon or Path shape
 */
export const isClosed = ([t, s]: [
  string,
  Polyline | Polygon | Path
]): boolean => {
  throw new Error("Function not implemented."); // TODO
};

/**
 * Return list of points from Polyline, Polygon or Path shape
 */
export const extractPoints = ([t, s]: [string, any]): [ad.Num, ad.Num][] => {
  if (t === "Polyline" || t === "Polygon") return s.points.contents;
  else if (t === "Path") return s.d.contents;
  else throw new Error(`Point extraction not defined for shape ${t}.`);
};
