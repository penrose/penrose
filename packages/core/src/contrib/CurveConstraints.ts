import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  addN,
  div,
  gt,
  ifCond,
  lt,
  lte,
  max,
  maxN,
  min,
  mul,
  or,
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
  consecutiveQuadruples,
  consecutiveTriples,
  consecutiveTuples,
  consecutiveTuples3D,
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

/**
 * Returns discrete curvature approximation given three consecutive points
 */
export const curvature = (
  p1: [ad.Num, ad.Num],
  p2: [ad.Num, ad.Num],
  p3: [ad.Num, ad.Num]
): ad.Num => {
  const v1 = ops.vsub(p2, p1);
  const v2 = ops.vsub(p3, p2);
  const angle = ops.angleFrom(v1, v2);
  return angle;
  // Alternative discrete curvature definition
  // return mul(2, sin(div(angle, 2)));
  // return mul(2, tan(div(angle, 2)));
};

/**
 * Returns discrete curvature approximation given three consecutive points
 */
export const curvatureSin = (
  p1: [ad.Num, ad.Num],
  p2: [ad.Num, ad.Num],
  p3: [ad.Num, ad.Num]
): ad.Num => {
  const v1 = ops.vsub(p2, p1);
  const v2 = ops.vsub(p3, p2);
  const angle = ops.angleFrom(v1, v2);
  // return angle;
  // Alternative discrete curvature definition
  return mul(2, sin(div(angle, 2)));
  // return mul(2, tan(div(angle, 2)));
};

/**
 * Returns discrete curvature approximation given three consecutive points
 */
export const curvatureTan = (
  p1: [ad.Num, ad.Num],
  p2: [ad.Num, ad.Num],
  p3: [ad.Num, ad.Num]
): ad.Num => {
  const v1 = ops.vsub(p2, p1);
  const v2 = ops.vsub(p3, p2);
  const angle = ops.angleFrom(v1, v2);
  // return angle;
  // Alternative discrete curvature definition
  // return mul(2, sin(div(angle, 2)));
  return mul(2, tan(div(angle, 2)));
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
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return addN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) => curvature(p1, p2, p3))
  );
};

const lineSegmentDistance = (
  [a0, a1]: [ad.Num, ad.Num, ad.Num][],
  [b0, b1]: [ad.Num, ad.Num, ad.Num][]
): ad.Num => {
  //     # Calculate denomitator
  //     A = a1 - a0
  //     B = b1 - b0
  const A = ops.vsub(a1, a0);
  const B = ops.vsub(b1, b0);

  //     magA = np.linalg.norm(A)
  //     magB = np.linalg.norm(B)
  const magA = ops.vdist(a0, a1);
  const magB = ops.vdist(b0, b1);

  //     _A = A / magA
  //     _B = B / magB
  const _A = ops.vnormalize(A);
  const _B = ops.vnormalize(B);

  //     cross = np.cross(_A, _B);
  //     denom = np.linalg.norm(cross)**2
  const cross = ops.cross3(_A, _B);
  const denom = ops.vnormsq(cross);

  //     # If lines are parallel (denom=0) test if lines overlap.
  //     # If they don't overlap then there is a closest point solution.
  //     # If they do overlap, there are infinite closest positions, but there is a closest distance
  //     if not denom:
  //         d0 = np.dot(_A,(b0-a0))
  //         # Overlap only possible with clamping
  //         if clampA0 or clampA1 or clampB0 or clampB1:
  //             d1 = np.dot(_A,(b1-a0))
  //             # Is segment B before A?
  //             if d0 <= 0 >= d1:
  //                 if clampA0 and clampB1:
  //                     if np.absolute(d0) < np.absolute(d1):
  //                         return a0,b0,np.linalg.norm(a0-b0)
  //                     return a0,b1,np.linalg.norm(a0-b1)
  //             # Is segment B after A?
  //             elif d0 >= magA <= d1:
  //                 if clampA1 and clampB0:
  //                     if np.absolute(d0) < np.absolute(d1):
  //                         return a1,b0,np.linalg.norm(a1-b0)
  //                     return a1,b1,np.linalg.norm(a1-b1)
  //         # Segments overlap, return distance between parallel segments
  //         return None,None,np.linalg.norm(((d0*_A)+a0)-b0)

  //     # Lines criss-cross: Calculate the projected closest points
  //     t = (b0 - a0);
  const t = ops.vsub(b0, a0);

  //     detA = np.linalg.det([t, _B, cross])
  //     detB = np.linalg.det([t, _A, cross])
  const detA = ops.vdot(t, ops.cross3(_B, cross));
  const detB = ops.vdot(t, ops.cross3(_A, cross));

  //     t0 = detA/denom;
  //     t1 = detB/denom;
  const t0 = div(detA, denom);
  const t1 = div(detB, denom);

  //     pA = a0 + (_A * t0) # Projected closest point on segment A
  //     pB = b0 + (_B * t1) # Projected closest point on segment B
  let pA = ops.vadd(a0, ops.vmul(t0, _A));
  let pB = ops.vadd(b0, ops.vmul(t1, _B));

  //     # Clamp projections
  //     if clampA0 or clampA1 or clampB0 or clampB1:
  //         if clampA0 and t0 < 0:
  //             pA = a0
  //         elif clampA1 and t0 > magA:
  //             pA = a1
  pA = [
    ifCond(lt(t0, 0), a0[0], ifCond(gt(t0, magA), a1[0], pA[0])),
    ifCond(lt(t0, 0), a0[1], ifCond(gt(t0, magA), a1[1], pA[1])),
    ifCond(lt(t0, 0), a0[2], ifCond(gt(t0, magA), a1[2], pA[2])),
  ];
  //         if clampB0 and t1 < 0:
  //             pB = b0
  //         elif clampB1 and t1 > magB:
  //             pB = b1
  pB = [
    ifCond(lt(t1, 0), b0[0], ifCond(gt(t1, magB), b1[0], pB[0])),
    ifCond(lt(t1, 0), b0[1], ifCond(gt(t1, magB), b1[1], pB[1])),
    ifCond(lt(t1, 0), b0[2], ifCond(gt(t1, magB), b1[2], pB[2])),
  ];
  //         # Clamp projection A
  //         if (clampA0 and t0 < 0) or (clampA1 and t0 > magA):
  //             dot = np.dot(_B,(pA-b0))
  //             if clampB0 and dot < 0:
  //                 dot = 0
  //             elif clampB1 and dot > magB:
  //                 dot = magB
  //             pB = b0 + (_B * dot)
  const dotB = min(magB, max(0, ops.vdot(_B, ops.vsub(pA, b0))));
  const newB = ops.vadd(b0, ops.vmul(dotB, _B));
  const condB = or(lt(t0, 0), gt(t0, magA));
  pA = [
    ifCond(condB, newB[0], pA[0]),
    ifCond(condB, newB[1], pA[1]),
    ifCond(condB, newB[2], pA[2]),
  ];

  //         # Clamp projection B
  //         if (clampB0 and t1 < 0) or (clampB1 and t1 > magB):
  //             dot = np.dot(_A,(pB-a0))
  //             if clampA0 and dot < 0:
  //                 dot = 0
  //             elif clampA1 and dot > magA:
  //                 dot = magA
  //             pA = a0 + (_A * dot)
  const dotA = min(magA, max(0, ops.vdot(_A, ops.vsub(pB, a0))));
  const newA = ops.vadd(a0, ops.vmul(dotA, _A));
  const condA = or(lt(t1, 0), gt(t1, magB));
  pB = [
    ifCond(condA, newA[0], pB[0]),
    ifCond(condA, newA[1], pB[1]),
    ifCond(condA, newA[2], pB[2]),
  ];

  //     return pA,pB,np.linalg.norm(pA-pB)
  return ops.vdist(pA, pB);
};

/**
 * Returns simple repulsive energy
 */
export const repulsiveEnergy3D = (
  points: [ad.Num, ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const tuples = consecutiveTuples3D(points, closed);
  const ts = [];
  for (let i = 0; i < tuples.length - 2; i++) {
    for (let j = i + 2; j < tuples.length; j++) {
      ts.push([tuples[i], tuples[j]]);
    }
  }
  const integrands = ts.map(([[p1, p2], [q1, q2]]) =>
    div(
      mul(ops.vdist(p1, p2), ops.vdist(q1, q2)),
      lineSegmentDistance([p1, p2], [q1, q2])
    )
  );
  return addN(integrands);
};

/**
 * Returns simple repulsive energy
 */
export const repulsiveEnergy3DSimple = (
  points: [ad.Num, ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const tuples = consecutiveTuples3D(points, closed);
  const ts = [];
  for (let i = 0; i < tuples.length - 1; i++) {
    for (let j = i + 1; j < tuples.length; j++) {
      ts.push([tuples[i], tuples[j]]);
    }
  }
  const integrands = ts.map(([[p1, p2], [q1, q2]]) =>
    div(
      mul(ops.vdist(p1, p2), ops.vdist(q1, q2)),
      ops.vdistsq(
        ops.vmul(0.5, ops.vadd(p1, p2)),
        ops.vmul(0.5, ops.vadd(q1, q2))
      )
    )
  );
  return addN(integrands);
};

/**
 * Returns simple repulsive energy
 */
export const repulsiveEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const tuples = consecutiveTuples(points, closed);
  const ts = [];
  for (let i = 0; i < tuples.length - 1; i++) {
    for (let j = i + 1; j < tuples.length; j++) {
      ts.push([tuples[i], tuples[j]]);
    }
  }
  const integrands = ts.map(([[p1, p2], [q1, q2]]) =>
    div(
      mul(ops.vdist(p1, p2), ops.vdist(q1, q2)),
      ops.vdistsq(
        ops.vmul(0.5, ops.vadd(p1, p2)),
        ops.vmul(0.5, ops.vadd(q1, q2))
      )
    )
  );
  return addN(integrands);
};

/**
 * Returns simple repulsive energy
 */
export const repulsiveEnergyMultiple = (
  points: [ad.Num, ad.Num][][],
  closed: boolean[]
): ad.Num => {
  let tuples: [[ad.Num, ad.Num], [ad.Num, ad.Num]][] = [];
  for (let i = 0; i < points.length - 1; i++) {
    tuples = [...tuples, ...consecutiveTuples(points[i], closed[i])];
  }
  const ts = [];
  for (let i = 0; i < tuples.length - 1; i++) {
    for (let j = i + 1; j < tuples.length; j++) {
      ts.push([tuples[i], tuples[j]]);
    }
  }
  const integrands = ts.map(([[p1, p2], [q1, q2]]) =>
    div(
      mul(ops.vdist(p1, p2), ops.vdist(q1, q2)),
      ops.vdistsq(
        ops.vmul(0.5, ops.vadd(p1, p2)),
        ops.vmul(0.5, ops.vadd(q1, q2))
      )
    )
  );
  return addN(integrands);
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
        squared(curvatureSin(p1, p2, p3)),
        mul(0.5, mul(ops.vdist(p1, p2), ops.vdist(p2, p3)))
      )
    )
  );
};

/**
 * todo
 */
export const newElasticEnergy = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveQuadruples(points, closed);
  return addN(
    triples.map(([p1, p2, p3, p4]: [ad.Num, ad.Num][]) =>
      mul(
        squared(
          div(
            sub(curvature(p1, p2, p3), curvature(p2, p3, p4)),
            add(1e-5, ops.vdist(p2, p3))
          )
        ),
        ops.vdist(p2, p3)
      )
    )
  );
};

/**
 * todo
 */
export const maxCurvature = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return maxN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      absVal(curvature(p1, p2, p3))
    )
  );
};

/**
 * todo
 */
export const maxCurvatureSin = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return maxN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      absVal(curvatureSin(p1, p2, p3))
    )
  );
};

/**
 * todo
 */
export const maxCurvatureTan = (
  points: [ad.Num, ad.Num][],
  closed: boolean
): ad.Num => {
  const triples = consecutiveTriples(points, closed);
  return maxN(
    triples.map(([p1, p2, p3]: [ad.Num, ad.Num][]) =>
      absVal(curvatureTan(p1, p2, p3))
    )
  );
};

/**
 * Returns integral of curvature squared along the curve
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
        pow(curvatureSin(p1, p2, p3), p),
        mul(0.5, mul(ops.vdist(p1, p2), ops.vdist(p2, p3)))
      )
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

  // noUTurn: ([t, s]: [string, Polyline]): ad.Num => {
  //   const points = extractPoints([t, s]);
  //   const hs = consecutiveTuples(points, isClosed([t, s]));
  //   const direction = ops.vnormalize(ops.vsub(hs[0][0], hs[0][1]));
  //   return addN(
  //     hs.map(([p1, p2]: ad.Num[][]) =>
  //       max(ops.vdot(direction, ops.vsub(p2, p1)), 0)
  //     )
  //   );
  // },

  // isFunction: ([t, s]: [string, Polyline]): ad.Num => {
  //   const points = extractPoints([t, s]);
  //   const hs = consecutiveTuples(points, isClosed([t, s]));
  //   const direction = [1, 0];
  //   return addN(
  //     hs.map(([p1, p2]: ad.Num[][]) =>
  //       max(ops.vdot(direction, ops.vsub(p2, p1)), 0)
  //     )
  //   );
  // },

  // isDescreasing: ([t, s]: [string, Polyline]): ad.Num => {
  //   const points = extractPoints([t, s]);
  //   const hs = consecutiveTuples(points, isClosed([t, s]));
  //   const direction = [0, 1];
  //   return addN(
  //     hs.map(([p1, p2]: ad.Num[][]) =>
  //       max(ops.vdot(direction, ops.vsub(p2, p1)), 0)
  //     )
  //   );
  // },

  // isIncreasing: ([t, s]: [string, Polyline]): ad.Num => {
  //   const points = extractPoints([t, s]);
  //   const hs = consecutiveTuples(points, isClosed([t, s]));
  //   const direction = [0, -1];
  //   return addN(
  //     hs.map(([p1, p2]: ad.Num[][]) =>
  //       max(ops.vdot(direction, ops.vsub(p2, p1)), 0)
  //     )
  //   );
  // },
};
