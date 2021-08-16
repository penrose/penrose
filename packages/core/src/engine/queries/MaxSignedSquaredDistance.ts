import { VarAD } from "types/ad";
import { mul, constOf, squared, max, ops, div, add, EPS_DENOM, min, sub, sqrt, absVal, and, ifCond, lt, neg } from 'engine/Autodiff';
import { overboxFromShape, underboxFromShape } from 'engine/BBox';
import * as BBox from 'engine/BBox';

/**
 * For input shapes A and B, computes max_{a in ∂A} ssd(a, B) where ssd is the signed squared
 * distance computed using B's SDF.
 * 
 * Properties:
 * 
 * TODO: FIX THE DESCRIPTIONS HERE
 * maxSSD(A, B) > 0 <-> A is not a subset of B. In this case it is equal to the Hausdorff
 * distance from A to B. This value is _not_ symmetric.
 * 
 * maxSSD(A, B) < 0 <-> A is a subset of B. In this case it is equal to the negative shortest distance
 * from A to B. I don't think this is symmetric. The other direction is positive (I think).
 * 
 * To symmetrize this query, compute max(maxSSD(A, B), maxSSD(B, A))
 */

const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (t1: string, t2: string) => {
  return isRectLike(t1) && isRectLike(t2);
}

/**
 * Clamp `x` in range `[l, r]`.
 */
 const clamp = ([l, r]: number[], x: VarAD): VarAD => {
  return max(constOf(l), min(constOf(r), x));
};

/**
 * Return the distance between point `pt` and segment `[start, end]`.
 */
 const pointSegmentDist = (pt: VarAD[], [start, end]: VarAD[][]): VarAD => {
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
  const pointOnLine = ops.vadd(start, ops.vmul(t1, dir));
  return ops.vdist(pt, pointOnLine);
};

/**
 * Return the distance between segments `s1` and `s2`.
 */
 const segmentSegmentDist = (s1: VarAD[][], s2: VarAD[][]): VarAD => {
  return min(
    min(
      pointSegmentDist(s1[0], s2),
      pointSegmentDist(s1[1], s2),
    ),
    min(
      pointSegmentDist(s2[0], s1),
      pointSegmentDist(s2[1], s1),
    ),
  )
};

const AABBHausdorffSquaredDistance = (
  box1: BBox.BBox,
  box2: BBox.BBox,
) => {
  const rx = div(box2.w, constOf(2));
  const ry = div(box2.h, constOf(2));

  const x0 = sub(BBox.minX(box1), box2.center[0]);
  const x1 = sub(BBox.maxX(box1), box2.center[0]);
  const y0 = sub(BBox.minY(box1), box2.center[1]);
  const y1 = sub(BBox.maxY(box1), box2.center[1]);

  return add(
    squared(max(constOf(0),
    max(
      sub(absVal(x0), rx),
      sub(absVal(x1), rx),
    )
    )),
    squared(max(constOf(0),
    max(
      sub(absVal(y0), ry),
      sub(absVal(y1), ry),
    )
    )),
  )
}

const AABBNegativeDistance = (
  box1: BBox.BBox,
  box2: BBox.BBox,
) => {
  // distance to sides of the box
  // TODO: this could also probably be written in the above style with absVal and vice versa. which
  // style is better?

  const rx = div(box2.w, constOf(2));
  const ry = div(box2.h, constOf(2));

  const x0 = sub(BBox.minX(box1), box2.center[0]);
  const x1 = sub(BBox.maxX(box1), box2.center[0]);
  const y0 = sub(BBox.minY(box1), box2.center[1]);
  const y1 = sub(BBox.maxY(box1), box2.center[1]);

  return min(constOf(0), max(
    sub(max(absVal(x0), absVal(x1)), rx),
    sub(max(absVal(y0), absVal(y1)), ry),
  ));
}

const AABBImplementation = (
  box1: BBox.BBox,
  box2: BBox.BBox,
) => {
  const nonNegSqDist = AABBHausdorffSquaredDistance(box1, box2);
  const nonPosSqDist = neg(squared(AABBNegativeDistance(box1, box2)));
  // const nonPosSqDist = constOf(0);

  return add(nonNegSqDist, nonPosSqDist);
}

export const lowerBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    // TODO: figure out which bounds are correct here
    const box1 = overboxFromShape(t1, s1);
    const box2 = overboxFromShape(t2, s2);

    return AABBImplementation(box1, box2);
};

export const exact = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t1, t2)) {
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    }

    if (isRectLike(t1) && isRectLike(t2)) {
      const box1 = overboxFromShape(t1, s1);
      const box2 = overboxFromShape(t2, s2);

      return AABBImplementation(box1, box2);
    } else {
      throw Error(`expected an exact closest-distance query implementation for ${t1} and ${t2}, but did not find one`);
    }
};

export const upperBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    // TODO: figure out which bounds are correct here
    const box1 = underboxFromShape(t1, s1);
    const box2 = underboxFromShape(t2, s2);

    return AABBImplementation(box1, box2);
};

export const exactOrLowerBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  lowerBound([t1, s1], [t2, s2]);

export const exactOrUpperBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  upperBound([t1, s1], [t2, s2]);
