import { ops } from "engine/Autodiff";
import { add, addN, div, squared, sub } from "engine/AutodiffFunctions";
import { Path } from "shapes/Path";
import { Polygon } from "shapes/Polygon";
import { Polyline } from "shapes/Polyline";
import * as ad from "types/ad";
import { turningNumber } from "./Functions";

const consecutiveNTuples = (
  points: [ad.Num, ad.Num][],
  tupleLength: number,
  closed: boolean
): [ad.Num, ad.Num][][] => {
  throw new Error("Function not implemented."); // TODO
};

const equivalued = (x: ad.Num[]): ad.Num => {
  const mean = div(addN(x), x.length);
  return addN(x.map((y: ad.Num) => squared(sub(y, mean))));
};

const extractPoints = ([t, s]: [string, Polyline | Polygon | Path]): [
  ad.Num,
  ad.Num
][] => {
  throw new Error("Function not implemented."); // TODO
};

export const constrDictCurves = {
  isClosed: ([t, s]: [string, Polyline | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    return ops.vdist(points[0], points[-1]);
  },

  isSimple: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    throw new Error("Function not implemented."); // TODO
  },

  isLocallyConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    throw new Error("Function not implemented."); // TODO
  },

  isConvex: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const lc = constrDictCurves.isLocallyConvex([t, s]);
    const points = extractPoints([t, s]);
    const tn = turningNumber(points);
    return add(lc, tn);
  },

  isEquidistant: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveNTuples(points, 2, t === "Polyline");
    return equivalued(hs.map(([p1, p2]: ad.Num[][]) => ops.vdist(p1, p2)));
  },

  isEquiangular: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    const points = extractPoints([t, s]);
    const hs = consecutiveNTuples(points, 3, t === "Polyline");
    return equivalued(
      hs.map(([p1, p2, p3]: ad.Num[][]) =>
        ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
      )
    );
  },
};
