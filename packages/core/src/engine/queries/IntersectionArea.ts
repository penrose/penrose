import * as _ from "lodash";
import { VarAD, Pt2 } from "types/ad";
import { ops, add, min, max, mul, constOf, absVal, sub, div, ifCond, or, lt, atan2, sqrt, squared, neg, addN, debug } from 'engine/Autodiff';
import { overboxFromShape, underboxFromShape } from 'engine/BBox';
import * as BBox from 'engine/BBox';
import { EPS_DENOM } from '../Autodiff';

/**
 * Computes the area of A ∩ B.
 */

// TODO: there aren't really measure-zero things in a diagram engine since everything takes up space...
const hasMeasureZero = (t: string) => ["Line", "Arrow", "Polyline"].includes(t);
const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (t1: string, t2: string) => {
  return (
    // hasMeasureZero(t1) ||
    // hasMeasureZero(t2) ||
    (isRectLike(t1) && isRectLike(t2)) ||
    (t1 === "Circle" && t2 === "Circle")
  )
};

// TODO: move this into BBox
// TODO: consolidate Box and BBox
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
    // or(lt(debug(r1, "r1"), debug(l2, "l2")), lt(debug(r2, "r2"), debug(l1, "l1"))), // disjoint
    // intervals => overlap is 0
    or(lt(r1, l2), lt(r2, l1)),
    constOf(0),
    min(d(l2, r1), d(l1, r2))
  );
};

const AABBOverlap = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ) => {
    if (!isRectLike(t1) || !isRectLike(t2)) {
      throw Error(`AABBOverlap expected rect-like shapes, but got ${t1} and ${t2}.`);
    }

    // adapted from distance code found here: https://gamedev.stackexchange.com/a/154040
    const box1 = overboxFromShape(t1, s1);
    const box2 = overboxFromShape(t2, s2);

    const outerWidth = sub(
      max(BBox.maxX(box1), BBox.maxX(box2)),
      min(BBox.minX(box1), BBox.minX(box2))
    );
    const outerHeight = sub(
      max(BBox.maxY(box1), BBox.maxY(box2)),
      min(BBox.minY(box1), BBox.minY(box2))
    );

    const innerWidth = max(constOf(0), sub(add(box1.w, box2.w), outerWidth));
    const innerHeight = max(constOf(0), sub(add(box1.h, box2.h), outerHeight));

    return mul(innerWidth, innerHeight);
}

// TODO: add acos to autodiff directly
// const acos = (x: VarAD): VarAD => atan2(sqrt(sub(constOf(1), squared(x))), x);
// TODO: is this approximation good? is it necessary?
// https://stackoverflow.com/a/3380723
const acos = (x: VarAD): VarAD => add(mul(sub(mul(constOf(-0.69813170079773212), squared(x)), constOf(0.87266462599716477)), x), constOf(1.5707963267948966))

export const lowerBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (t1 === "Circle" && t2 === "Circle") {
      // TODO: the quadrilateral inside the intersection may serve as a nice upper bound
    }

    const box1 = underboxFromShape(t1, s1);
    const box2 = underboxFromShape(t2, s2);

    const overlapX = overlap1D(BBox.xRange(box1), BBox.xRange(box2));
    const overlapY = overlap1D(BBox.yRange(box1), BBox.yRange(box2));

    return mul(overlapX, overlapY);
};

export const exact = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t1, t2)) {
      throw Error(`exact intersection area query not supported for ${t1} and ${t2}`)
    }

    // if (hasMeasureZero(t1) || hasMeasureZero(t2)) {
    //   return constOf(0);
    // } else
    if (isRectLike(t1) && isRectLike(t2)) {
      // const box1 = overboxFromShape(t1, s1);
      // const box2 = overboxFromShape(t2, s2);

      // const overlapX = overlap1D(BBox.xRange(box1), BBox.xRange(box2));
      // const overlapY = overlap1D(BBox.yRange(box1), BBox.yRange(box2));

      // // return add(mul(debug(overlapX, "overlapX"), debug(overlapY, "overlapY")), mul(constOf(0),
      // // add(debug(box2.w, "2w"), debug(box2.h, "2h"))));
      // return mul(debug(overlapX, "overlapX"), debug(overlapY, "overlapY"));
      return AABBOverlap([t1, s1], [t2, s2]);
    } else if (t1 === "Circle" && t2 === "Circle") {
      // https://diego.assencio.com/?index=8d6ca3d82151bad815f78addf9b5c1c6
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      const d = add(r1, r2);
      const d1 = div(addN([squared(r1), neg(squared(r2)), squared(d)]), mul(constOf(2), d));
      const d2 = sub(d, d1);
      // TODO: can I remove the epsilons here?
      return add(
        sub(mul(squared(r1), acos(div(d1, r1))), mul(d1, sqrt(add(sub(squared(r1), squared(d1)), constOf(EPS_DENOM))))),
        sub(mul(squared(r2), acos(div(d2, r2))), mul(d2, sqrt(add(sub(squared(r2), squared(d2)), constOf(EPS_DENOM))))),
      )
    } else if ((t1 === "Circle" && t2 === "Ellipse") || (t1 === "Ellipse" && t2 === "Circle")) {
      // There may be an exact way to compute this.
      throw Error(`exact intersection area query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Ellipse") {
      // Reducible to ellipse-circle case.
      throw Error(`exact intersection area query not supported for ${t1} and ${t2}`)
    } else {
      throw Error(`expected an exact intersection area query implementation for ${t1} and ${t2}, but did not find one`)
    }
};

export const upperBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (t1 === "Circle" && t2 === "Circle") {
      // TODO: the quadrilateral outside the intersection may serve as a nice upper bound
    }

    const box1 = overboxFromShape(t1, s1);
    const box2 = overboxFromShape(t2, s2);

    const overlapX = overlap1D(BBox.xRange(box1), BBox.xRange(box2));
    const overlapY = overlap1D(BBox.yRange(box1), BBox.yRange(box2));

    return mul(overlapX, overlapY);
};

export const exactOrLowerBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  lowerBound([t1, s1], [t2, s2]);

export const exactOrUpperBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  upperBound([t1, s1], [t2, s2]);
