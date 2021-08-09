import { VarAD } from "types/ad";
import { sub, debug } from 'engine/Autodiff';
import * as Area from 'engine/queries/Area';
import * as IntersectionArea from 'engine/queries/IntersectionArea';

/**
 * Computes the area of A - B.
 * TODO: consider these cases more carefully! for now using area(A) - intersectionArea(A, B)
 */

export const hasExactImpl = (t1: string, t2: string) => {
  return Area.hasExactImpl(t1) && IntersectionArea.hasExactImpl(t1, t2);
};

export const lowerBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    // TODO: this code assumes Area can/should always be computed exactly
    const areaA = Area.exact([t1, s1]);
    const areaAB = IntersectionArea.upperBound([t1, s1], [t2, s2]);
    return sub(areaA, areaAB);
};

export const exact = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t1, t2)) {
      throw Error(`exact difference area query not supported for ${t1} and ${t2}`)
    }

    // const areaA = debug(Area.exact([t1, s1]), "area in diff area");
    // const areaAB = debug(IntersectionArea.exact([t1, s1], [t2, s2]), "inter-area in diff area");
    const areaA = Area.exact([t1, s1]);
    const areaAB = IntersectionArea.exact([t1, s1], [t2, s2]);
    return sub(areaA, areaAB);
};

export const upperBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    // TODO: this code assumes Area can/should always be computed exactly
    // const areaA = debug(Area.exact([t1, s1]), "area");
    // const areaAB = debug(IntersectionArea.lowerBound([t1, s1], [t2, s2]), "intersection");
    const areaA = Area.exact([t1, s1]);
    const areaAB = IntersectionArea.lowerBound([t1, s1], [t2, s2]);
    return sub(areaA, areaAB);
};

export const exactOrLowerBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  lowerBound([t1, s1], [t2, s2]);

export const exactOrUpperBound = ([t1, s1]: [string, any], [t2, s2]: [string, any],): VarAD =>
  hasExactImpl(t1, t2) ?
  exact([t1, s1], [t2, s2]) :
  upperBound([t1, s1], [t2, s2]);
