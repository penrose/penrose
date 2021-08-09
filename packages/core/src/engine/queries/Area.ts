import { VarAD } from "types/ad";
import { mul, constOf, squared, debug } from 'engine/Autodiff';
import { overboxFromShape, underboxFromShape } from 'engine/BBox';

/**
 * Computes the area of A.
 */

// TODO: there aren't really measure-zero things in a diagram engine since everything takes up space...
const hasMeasureZero = (t: string) => ["Line", "Arrow", "Polyline"].includes(t);
const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (_t: string) => true;

export const lowerBound = (
  [t, s]: [string, any],
  ): VarAD => {
    const box = underboxFromShape(t, s);

    return mul(box.w, box.h);
};

export const exact = (
  [t, s]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t)) {
      throw Error(`exact area query not supported for ${t}`)
    }

    if (hasMeasureZero(t)) {
      return constOf(0);
    } else if (isRectLike(t)) {
      const box = overboxFromShape(t, s);
      // return debug(mul(debug(box.w, "areaW"), debug(box.h, "areaH")), "area");
      return mul(box.w, box.h);
    } else if (t === "Circle") {
      return mul(constOf(Math.PI), squared(s.r.contents));
    } else if (t === "Ellipse") {
      return mul(constOf(Math.PI), mul(s.rx.contents, s.ry.contents));
    } else {
      throw Error(`expected an exact area query implementation for ${t}, but did not find one`);
    }
};

export const upperBound = (
  [t, s]: [string, any],
  ): VarAD => {
    const box = overboxFromShape(t, s);

    return mul(box.w, box.h);
};

export const exactOrLowerBound = ([t, s]: [string, any]): VarAD =>
  hasExactImpl(t) ?
  exact([t, s]) :
  lowerBound([t, s]);

export const exactOrUpperBound = ([t, s]: [string, any]): VarAD =>
  hasExactImpl(t) ?
  exact([t, s]) :
  upperBound([t, s]);
