import { constOf, constOfIf, EPS_DENOM, ops } from "engine/Autodiff";
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
import * as _ from "lodash";
import { Pt2, VarAD } from "types/ad";

/**
 * Require that a shape at `center1` with radius `r1` not intersect a shape at `center2` with radius `r2` with optional padding `padding`. (For a non-circle shape, its radius should be half of the shape's general "width")
 */
export const noIntersectCircles = (
  center1: VarAD[],
  r1: VarAD,
  center2: VarAD[],
  r2: VarAD,
  padding = 10
): VarAD => {
  // noIntersect [[x1, y1, s1], [x2, y2, s2]] = - dist (x1, y1) (x2, y2) + (s1 + s2 + 10)
  const res = add(add(r1, r2), constOfIf(padding));
  return sub(res, ops.vdist(center1, center2));
};

// ------- Polygon-related helpers

/**
 * Return true iff `p` is in rect `b`.
 */
export const pointInBox = (p: Pt2, rect: BBox.BBox): VarAD => {
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
  pt: Pt2,
  rect: BBox.BBox,
  offset: VarAD
): VarAD => {
  const dsqRes = dsqBP(pt, rect);
  const WEIGHT = 1;
  return mul(constOf(WEIGHT), absVal(sub(dsqRes, squared(offset))));
};

/**
 * Assuming `rect` is an axis-aligned bounding box (AABB),
 * compute the positive distance squared from point `p` to box `rect` (not the signed distance).
 * https://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
 */
const dsqBP = (p: Pt2, rect: BBox.BBox): VarAD => {
  const dx = max(
    max(sub(BBox.minX(rect), p[0]), constOf(0.0)),
    sub(p[0], BBox.maxX(rect))
  );
  const dy = max(
    max(sub(BBox.minY(rect), p[1]), constOf(0.0)),
    sub(p[1], BBox.maxY(rect))
  );
  return add(squared(dx), squared(dy));
};

// ------- 1D

/**
 * Return the amount of overlap between two intervals in R. (0 if none)
 */
export const overlap1D = (
  [l1, r1]: [VarAD, VarAD],
  [l2, r2]: [VarAD, VarAD]
): VarAD => {
  const d = (x: VarAD, y: VarAD) => absVal(sub(x, y)); // Distance between two reals
  // const d = (x: VarAD, y: VarAD) => squared(sub(x, y)); // Distance squared, if just the asymptotic behavior matters
  return ifCond(
    or(lt(r1, l2), lt(r2, l1)), // disjoint intervals => overlap is 0
    constOf(0),
    min(d(l2, r1), d(l1, r2))
  );
};

/**
 * Return numerically-encoded boolean indicating whether `x \in [l, r]`.
 */
export const inRange = (x: VarAD, l: VarAD, r: VarAD): VarAD => {
  if (l.val > r.val) throw Error("invalid range"); // TODO do this range check better
  const fals = constOf(0);
  const tru = constOf(1);
  return ifCond(and(gt(x, l), lt(x, r)), tru, fals);
};

/**
 * Linearly interpolate between left `l` and right `r` endpoints, at fraction `k` of interpolation.
 */
const lerp = (l: VarAD, r: VarAD, k: VarAD): VarAD => {
  // TODO: Rewrite the lerp code to be more concise
  return add(mul(l, sub(constOf(1.0), k)), mul(r, k));
};

/**
 * Linearly interpolate between vector `l` and vector `r` endpoints, at fraction `k` of interpolation.
 */
const lerp2 = (l: VarAD[], r: VarAD[], k: VarAD): [VarAD, VarAD] => {
  return [lerp(l[0], r[0], k), lerp(l[1], r[1], k)];
};

/**
 * Sample a line `line` at `NUM_SAMPLES` points uniformly.
 */
export const sampleSeg = (line: VarAD[][]) => {
  const NUM_SAMPLES = 15;
  const NUM_SAMPLES2 = constOf(1 + NUM_SAMPLES);
  // TODO: Check that this covers the whole line, i.e. no off-by-one error
  const samples = _.range(1 + NUM_SAMPLES).map((i) => {
    const k = div(constOf(i), NUM_SAMPLES2);
    return lerp2(line[0], line[1], k);
  });

  return samples;
};

/**
 * Repel a vector `a` from a vector `b` with weight `c`.
 */
export const repelPoint = (c: VarAD, a: VarAD[], b: VarAD[]) =>
  div(c, add(ops.vdistsq(a, b), constOf(EPS_DENOM)));

/**
 * Encourage that an arrow `arr` be centered between two shapes with centers `center1` and `center2`, and text size (?) `[o1, o2]`.
 */
export const centerArrow2 = (
  arr: any,
  center1: VarAD[],
  center2: VarAD[],
  [o1, o2]: VarAD[]
): VarAD => {
  const vec = ops.vsub(center2, center1); // direction the arrow should point to
  const dir = ops.vnormalize(vec);

  let start = center1;
  let end = center2;

  // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality

  // TODO: add abs
  if (gt(ops.vnorm(vec), add(o1, absVal(o2)))) {
    start = ops.vadd(center1, ops.vmul(o1, dir));
    end = ops.vadd(center2, ops.vmul(o2, dir));
  }

  const fromPt = arr.start.contents;
  const toPt = arr.end.contents;

  return add(ops.vdistsq(fromPt, start), ops.vdistsq(toPt, end));
};

/**
 * Clamp `x` in range `[l, r]`.
 */
const clamp = ([l, r]: number[], x: VarAD): VarAD => {
  return max(constOf(l), min(constOf(r), x));
};

/**
 * Return the closest point on segment `[start, end]` to point `pt`.
 */
export const closestPt_PtSeg = (
  pt: VarAD[],
  [start, end]: VarAD[][]
): VarAD[] => {
  const EPS0 = constOf(10e-3);
  const lensq = max(ops.vdistsq(start, end), EPS0); // Avoid a divide-by-0 if the line is too small

  // If line seg looks like a point, the calculation just returns (something close to) `v`
  const dir = ops.vsub(end, start);
  // t = ((p -: v) `dotv` dir) / lensq -- project vector onto line seg and normalize
  const t = div(
    ops.vdot(ops.vsub(pt, start), dir),
    add(lensq, constOf(EPS_DENOM))
  );
  const t1 = clamp([0.0, 1.0], t);

  // v +: (t' *: dir) -- walk along vector of line seg
  return ops.vadd(start, ops.vmul(t1, dir));
};
