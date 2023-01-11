import { ops } from "engine/Autodiff";
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
} from "engine/AutodiffFunctions";
import { Path } from "shapes/Path";
import { Polygon } from "shapes/Polygon";
import { Polyline } from "shapes/Polyline";
import * as ad from "types/ad";
import { turningNumber } from "./Functions";
import {
  consecutiveTriples,
  consecutiveTuples,
  extractPoints,
  isClosed,
} from "./Utils";

/**
 * All values in the list should be equal
 */
const equivalued = (x: ad.Num[]): ad.Num => {
  const mean = div(addN(x), x.length);
  return addN(x.map((y: ad.Num) => squared(sub(y, mean))));
};

export const constrDictCurves = {
  /**
   * The shape should not have any self-intersections
   */
  isSimple: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    throw new Error("Function not implemented."); // TODO
  },

  /**
   * The shape should be locally convex (all angles between consecutive edges whould have the same sign)
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
        ifCond(lte(mul(meanSign, angle), 0), 0, angle)
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
  isEquidistant: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
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
