import { VarAD } from "types/ad";
import { overboxFromShape } from 'engine/BBox';

/**
 * Computes the centroid of A.
 */

const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (_t: string) => true;

export const exact = (
  [t, s]: [string, any],
  ): VarAD[] => {
    if (!hasExactImpl(t)) {
      throw Error(`exact centroid query not supported for ${t}`)
    }

    if (isRectLike(t)) {
      const box = overboxFromShape(t, s);
      return box.center;
    } else if (t === "Circle") {
      return s.center.contents;
    } else if (t === "Ellipse") {
      return s.center.contents;
    } else {
      throw Error(`expected an exact area query implementation for ${t}, but did not find one`);
    }
};
