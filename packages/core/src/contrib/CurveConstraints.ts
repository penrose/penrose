import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  lte,
  maxN,
  mul,
  pow,
  sign,
  sin,
  squared,
  sub,
  tan,
} from "../engine/AutodiffFunctions";
import { Path } from "../shapes/Path";
import { Polygon } from "../shapes/Polygon";
import { Polyline } from "../shapes/Polyline";
import * as ad from "../types/ad";
import { ConstrFunc } from "../types/functions";
import { shapeT, unionT } from "../utils/Util";
import {
  consecutiveTriples,
  consecutiveTuples,
  extractPoints,
  isClosed,
} from "./Utils";

/**
 * All values in the list should be equal
 */
export const equivalued = (x: ad.Num[]): ad.Num => {
  const mean = div(addN(x), x.length);
  return addN(x.map((y: ad.Num) => squared(sub(y, mean))));
};

enum CurvatureApproximationMode {
  FiniteDifferences = "FiniteDifferences",
  Angle = "Angle",
  SteinerLineSegment = "SteinerLineSegment",
  SteinerCorner = "SteinerCorner",
  OsculatingCircle = "OsculatingCircle",
}

/**
 * Returns discrete curvature approximation given three consecutive points
 */
export const curvature = (
  p1: [ad.Num, ad.Num],
  p2: [ad.Num, ad.Num],
  p3: [ad.Num, ad.Num],
  mode: CurvatureApproximationMode = CurvatureApproximationMode.Angle
): ad.Num => {
  // Finite difference approximation of the $\partial_s T = \kappa N$
  if (mode === CurvatureApproximationMode.FiniteDifferences) {
    const v12 = ops.vsub(p2, p1);
    const v23 = ops.vsub(p3, p2);
    // Tangent vectors corresponding to the 2 edges
    const t12 = ops.vnormalize(v12);
    const t23 = ops.vnormalize(v23);
    // Centers of the 2 edges
    const p12 = ops.vmul(0.5, ops.vadd(p1, p2));
    const p23 = ops.vmul(0.5, ops.vadd(p2, p3));
    // Distance between the edge centers
    const l123 = ops.vdist(p12, p23);
    return div(ops.vdist(t23, t12), l123);
  }

  // Curvature approximation schemes using angle adapted from [1].
  // [1] K. Crane, M. Wardetzky and J. Hass,
  //     "A Glimpse into Discrete Differential Geometry",
  //     Notices of the American Mathematical Society 64(10):1153-1159
  //     DOI: 10.1090/noti1578
  const v1 = ops.vsub(p2, p1);
  const v2 = ops.vsub(p3, p2);
  const angle = ops.angleFrom(v1, v2);

  // $\kappa^A$ from [1]
  if (mode === CurvatureApproximationMode.Angle) return angle;
  // $\kappa^B$ from [1]
  if (mode === CurvatureApproximationMode.SteinerLineSegment)
    return mul(2, sin(div(angle, 2)));
  // $\kappa^C$ from [1]
  if (mode === CurvatureApproximationMode.SteinerCorner)
    return mul(2, tan(div(angle, 2)));
  // $\kappa^D$ from [1]
  const w = ops.vdist(p1, p3);
  return div(mul(2, sin(div(angle, 2))), w);
};

/**
 * Returns the total length of polygonal chain given its nodes
 */
export const perimeter = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const sides = consecutiveTuples(points, closed);
  return addN(sides.map(([p1, p2]: [ad.Num, ad.Num][]) => ops.vdist(p1, p2)));
};

/**
 * Returns the signed area enclosed by a polygonal chain given its nodes
 */
export const signedArea = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const sides = consecutiveTuples(points, closed);
  return mul(
    0.5,
    addN(
      sides.map(([p1, p2]: [ad.Num, ad.Num][]) =>
        sub(mul(p1[0], p2[1]), mul(p1[1], p2[0]))
      )
    )
  );
};

/**
 * Returns the turning number of polygonal chain given its nodes
 */
export const turningNumber = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  return div(totalCurvature(points, closed), 2 * Math.PI);
};

/**
 * Returns the isoperimetric ratio (perimeter squared divided by enclosed area)
 */
export const isoperimetricRatio = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  return div(squared(perimeter(points, closed)), signedArea(points, closed));
};

/**
 * Returns integral of curvature along the curve
 */
export const totalCurvature = (
  points: [ad.Num, ad.Num][],
  closed: boolean,
  signed = true
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  if (signed) {
    return addN(
      triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) => curvature(p1, p2, p3))
    );
  }
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      absVal(curvature(p1, p2, p3))
    )
  );
};

/**
 * Returns integral of curvature squared along the curve
 */
export const elasticEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      mul(
        squared(
          curvature(p1, p2, p3, CurvatureApproximationMode.SteinerLineSegment)
        ),
        mul(0.5, mul(ops.vdist(p1, p2), ops.vdist(p2, p3)))
      )
    )
  );
};

/**
 * Returns the sum of all line segment lengths raised to `k`
 */
export const lengthK = (
  points: [ad.Num, ad.Num][],
  closed: boolean,
  k: number
): ad.Num => {
  const tuples = consecutiveTuples(points, closed);
  return addN(
    tuples.map(([p1, p2]: [ad.Num, ad.Num][]) => pow(ops.vdist(p1, p2), k))
  );
};

/**
 * Returns the maximum value of curvature along the curve
 */
export const maxCurvature = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return maxN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      absVal(curvature(p1, p2, p3, CurvatureApproximationMode.OsculatingCircle))
    )
  );
};

/**
 * Returns integral of curvature raised to `p` along the curve
 */
export const pElasticEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean,
  p = 2
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      mul(
        pow(
          curvature(p1, p2, p3, CurvatureApproximationMode.SteinerLineSegment),
          p
        ),
        mul(0.5, mul(ops.vdist(p1, p2), ops.vdist(p2, p3)))
      )
    )
  );
};

/**
 * Inflection energy of an order p
 */
export const inflectionEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean,
  p: number
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  const curvatures = triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
    curvature(p1, p2, p3, CurvatureApproximationMode.SteinerLineSegment)
  );
  const tuples = consecutiveTuples(curvatures, closed);
  return addN(
    tuples.map(([kappa1, kappa2]: [ad.Num, ad.Num]) =>
      pow(absVal(sub(kappa2, kappa1)), p)
    )
  );
};

export const constrDictCurves: { [k: string]: ConstrFunc } = {
  /**
   * The shape should be locally convex (all angles between consecutive edges would have the same sign)
   */
  isLocallyConvex: {
    name: "isLocallyConvex",
    description:
      "The shape should be locally convex (all angles between consecutive edges would have the same sign)",
    params: [
      {
        name: "s",
        description: "a shape",
        type: unionT(shapeT("Polyline"), shapeT("Polygon"), shapeT("Path")),
      },
    ],
    body: (s: Polyline<ad.Num> | Polygon<ad.Num> | Path<ad.Num>): ad.Num => {
      const points = extractPoints(s);
      const triples = consecutiveTriples(points, isClosed(s));
      const angles = triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
        ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
      );
      const meanSign = sign(addN(angles));
      return addN(
        angles.map((angle: ad.Num) =>
          ifCond(lte(mul(meanSign, angle), 0), squared(angle), 0)
        )
      );
    },
  },

  /**
   * The enclosed area should be convex
   * Implemented using local convexity penalty (`localPenalty`) and global turning number penalty (`globalPenalty`)
   */
  isConvex: {
    name: "isConvex",
    description: "The enclosed area should be convex",
    params: [
      {
        name: "s",
        description: "a shape",
        type: unionT(shapeT("Polyline"), shapeT("Polygon"), shapeT("Path")),
      },
    ],
    body: (s: Polyline<ad.Num> | Polygon<ad.Num> | Path<ad.Num>): ad.Num => {
      const localPenalty = constrDictCurves.isLocallyConvex.body(s);
      const points = extractPoints(s);
      const tn = turningNumber(points, isClosed(s));
      const globalPenalty = squared(sub(absVal(tn), 1));
      return add(localPenalty, globalPenalty);
    },
  },

  /**
   * All edges should have the same length
   */
  isEquilateral: {
    name: "isEquilateral",
    description: "All edges should have the same length",
    params: [
      {
        name: "s",
        description: "a shape",
        type: unionT(shapeT("Polyline"), shapeT("Polygon"), shapeT("Path")),
      },
    ],
    body: (s: Polyline<ad.Num> | Polygon<ad.Num> | Path<ad.Num>): ad.Num => {
      const points = extractPoints(s);
      const hs = consecutiveTuples(points, isClosed(s));
      return equivalued(hs.map(([p1, p2]: ad.Num[][]) => ops.vdist(p1, p2)));
    },
  },

  /**
   * All angles between consecutive edges should be equal
   */
  isEquiangular: {
    name: "isEquiangular",
    description: "All angles between consecutive edges should be equal",
    params: [
      {
        name: "s",
        description: "a shape",
        type: unionT(shapeT("Polyline"), shapeT("Polygon"), shapeT("Path")),
      },
    ],
    body: (s: Polyline<ad.Num> | Polygon<ad.Num> | Path<ad.Num>): ad.Num => {
      const points = extractPoints(s);
      const hs = consecutiveTriples(points, isClosed(s));
      return equivalued(
        hs.map(([p1, p2, p3]: ad.Num[][]) =>
          ops.angleFrom(ops.vsub(p2, p1), ops.vsub(p3, p2))
        )
      );
    },
  },
};
