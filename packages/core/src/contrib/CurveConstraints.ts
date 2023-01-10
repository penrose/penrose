import { ops } from "engine/Autodiff";
import {
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
import { consecutiveNTuples, extractPoints, isClosed } from "./Utils";

const equivalued = (x: ad.Num[]): ad.Num => {
  const mean = div(addN(x), x.length);
  return addN(x.map((y: ad.Num) => squared(sub(y, mean))));
};

export const constrDictCurves = {
  isSimple: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    throw new Error("Function not implemented."); // TODO
  },

  isLocallyConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const triples = consecutiveNTuples(points, 3, isClosed([t, s]));
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

  isConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const lc = constrDictCurves.isLocallyConvex([t, s]);
    const points = extractPoints([t, s]);
    const tn = turningNumber(points, isClosed([t, s]));
    return add(lc, tn);
  },

  isEquidistant: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveNTuples(points, 2, isClosed([t, s]));
    return equivalued(hs.map(([p1, p2]: ad.Num[][]) => ops.vdist(p1, p2)));
  },

  isEquiangular: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveNTuples(points, 3, isClosed([t, s]));
    return equivalued(
      hs.map(([p1, p2, p3]: ad.Num[][]) =>
        ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
      )
    );
  },
};
