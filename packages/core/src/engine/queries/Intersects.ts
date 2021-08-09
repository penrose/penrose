import * as _ from "lodash";
import { VarAD, Pt2 } from "types/ad";
import { ops, add, min, max, mul, constOf, absVal, sub, div, ifCond, or, lt, atan2, sqrt, squared, neg, addN, debug, and } from 'engine/Autodiff';
import { overboxFromShape, underboxFromShape } from 'engine/BBox';
import * as BBox from 'engine/BBox';
import { EPS_DENOM } from '../Autodiff';

/**
 * Checks whether A and B have intersection points.
 */

// TODO: there aren't really measure-zero things in a diagram engine since everything takes up space...
const hasMeasureZero = (t: string) => ["Line", "Arrow", "Polyline"].includes(t);
const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (t1: string, t2: string) => {
  return true;
};

export const lowerBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    return constOf(0);
};

export const exact = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t1, t2)) {
      throw Error(`exact intersection query not supported for ${t1} and ${t2}`)
    }

    if (t1 === "InfiniteLine" && t2 === "InfiniteLine") {
      // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
      const [x1, y1] = s1.start.contents;
      const [x2, y2] = s1.end.contents;
      const [x3, y3] = s2.start.contents;
      const [x4, y4] = s2.end.contents;

      const D = sub(
        mul(sub(x1, x2), sub(y3, y4)),
        mul(sub(y1, y2), sub(x3, x4)),
      );

      const EPS0 = constOf(1e-3);
      return lt(EPS0, absVal(D));
  } else if (t1 === "Line" && t2 === "Line") {
      // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
      const [x1, y1] = s1.start.contents;
      const [x2, y2] = s1.end.contents;
      const [x3, y3] = s2.start.contents;
      const [x4, y4] = s2.end.contents;

      const D = sub(
        mul(sub(x1, x2), sub(y3, y4)),
        mul(sub(y1, y2), sub(x3, x4)),
      );

      const T = sub(
        mul(sub(x1, x3), sub(y3, y4)),
        mul(sub(y1, y3), sub(x3, x4)),
      );

      const U = sub(
        mul(sub(x2, x1), sub(y1, y3)),
        mul(sub(y2, y1), sub(x1, x3)),
      );

      const EPS0 = constOf(1e-3);

      return and(
        lt(EPS0, absVal(D)),
        and(
          ifCond(
          lt(constOf(0), D),
          and(lt(constOf(0), T), lt(T, D)),
          and(lt(D, T), lt(T, constOf(0)))
          ),
          ifCond(
          lt(constOf(0), D),
          and(lt(constOf(0), U), lt(U, D)),
          and(lt(D, U), lt(U, constOf(0)))
          ),
        )
      );
    } else {
      throw Error(`expected an exact intersection query implementation for ${t1} and ${t2}, but did not find one`)
    }
};

export const upperBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    return constOf(1);
};

export const exactOrLowerBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  lowerBound([t1, s1], [t2, s2]);

export const exactOrUpperBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  upperBound([t1, s1], [t2, s2]);
