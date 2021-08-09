import { VarAD, Pt2 } from 'types/ad';
import { mul, constOf, squared, debug, ifCond, lt, ops } from 'engine/Autodiff';
import { BBox, maxX, maxY, minX, minY, overboxFromShape, underboxFromShape } from 'engine/BBox';

/**
 * Computes the bounding line of A in direction v.
 * Think of it as taking a line at infinity perpendicular to direction v, then bringing it closer to
 * the shape in the opposite direction of v until it hits the shape.
 */

// TODO: VarAD[]'s here are really Pt2, but lose type safety through AD functions

// TODO: there aren't really measure-zero things in a diagram engine since everything takes up space...
const hasMeasureZero = (t: string) => ["Line", "Arrow", "Polyline"].includes(t);
const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

const furthestPointAABB = (bbox: BBox, dir: VarAD[]): VarAD[] => {
  const EPS0 = constOf(1e-3);

  return [
    ifCond(
      // x == 0
      lt(squared(dir[0]), EPS0),
      bbox.center[0],
      ifCond(
        // x > 0
        lt(constOf(0), dir[0]),
        maxX(bbox),
        minX(bbox),
      ),
    ),
    ifCond(
      // y == 0
      lt(squared(dir[1]), EPS0),
      bbox.center[1],
      ifCond(
        // y > 0
        lt(constOf(0), dir[1]),
        maxY(bbox),
        minY(bbox),
      ),
    ),
  ];
}

export const hasExactImpl = (_t: string) => true;

export const lowerBound = (
  [t, s]: [string, any],
  dir : Pt2,
  ): VarAD[] => {
    const box = underboxFromShape(t, s);

    return furthestPointAABB(box, ops.vnormalize(dir));
};

export const exact = (
  [t, s]: [string, any],
  dir : Pt2,
  ): VarAD[] => {
    if (!hasExactImpl(t)) {
      throw Error(`exact area query not supported for ${t}`)
    }

    // TODO: maybe this should be a precondition to avoid this computation if possible.
    const normDir = ops.vnormalize(dir);

    if (isRectLike(t)) {
      return furthestPointAABB(overboxFromShape(t, s), normDir);
    } else if (t === "Circle") {
      return ops.vmul(s.r.contents, normDir);
    } else {
      throw Error(`expected an exact furthest point implementation for ${t}, but did not find one`);
    }
};

export const upperBound = (
  [t, s]: [string, any],
  dir : Pt2,
  ): VarAD[] => {
    const box = overboxFromShape(t, s);

    return furthestPointAABB(box, ops.vnormalize(dir));
};
