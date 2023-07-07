import { EPS_DENOM, ops } from "../engine/Autodiff.js";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  inverse,
  lt,
  max,
  mul,
  neg,
  squared,
  sub,
} from "../engine/AutodiffFunctions.js";
import { Line } from "../shapes/Line.js";
import { Shape } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { ObjFunc } from "../types/functions.js";
import {
  booleanT,
  linePts,
  noWarnFn,
  real2T,
  realNMT,
  realNT,
  realT,
  rectlikeT,
  shapeT,
  unionT,
} from "../utils/Util.js";
import { constrDictCurves } from "./CurveConstraints.js";
import { inDirection } from "./ObjectivesUtils.js";
import { bboxFromShape, shapeCenter } from "./Queries.js";
import {
  Linelike,
  Rectlike,
  closestPt_PtSeg,
  isLinelike,
  repelPoint,
  sampleSeg,
} from "./Utils.js";

// -------- Simple objective functions
// Do not require shape queries, operate directly with `ad.Num` parameters.
export const objDictSimple = {
  /**
   * Encourage the input value to be close to negative infinity
   */
  minimal: {
    name: "minimal",
    description: "Encourage the input value to be close to negative infinity",
    params: [{ name: "x", description: "Value", type: realT() }],
    body: noWarnFn((x: ad.Num): ad.Num => x),
  },

  /**
   * Encourage the input value to be close to infinity
   */
  maximal: {
    name: "maximal",
    description: "Encourage the input value to be close to infinity",
    params: [{ name: "x", description: "Value", type: realT() }],
    body: noWarnFn((x: ad.Num): ad.Num => neg(x)),
  },

  /**
   * Encourage the inputs to have the same value: `(x - y)^2`
   */
  equal: {
    name: "equal",
    description: "Encourage the inputs to have the same value: `(x - y)^2`",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn((x: ad.Num, y: ad.Num): ad.Num => squared(sub(x, y))),
  },

  nearVec: {
    name: "nearVec",
    description:
      "Encourage two vectors `v1` and `v2` to be near each other with distance `offset`.",
    params: [
      { name: "v1", type: realNT(), description: "a vector" },
      { name: "v2", type: realNT(), description: "a vector" },
      {
        name: "offset",
        type: realT(),
        description: "distance between two vectors",
      },
    ],
    body: noWarnFn((v1: ad.Num[], v2: ad.Num[], offset: ad.Num): ad.Num => {
      return sub(ops.vdistsq(v1, v2), squared(offset));
    }),
  },

  /**
   * Encourage x to be greater than or equal to y: `max(0,y - x)^2`
   */
  greaterThan: {
    name: "greaterThan",
    description:
      "Encourage x to be greater than or equal to y: `max(0,y - x)^2`",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn(
      (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(y, x)))
    ),
  },

  /**
   * Encourage x to be less than or equal to y: `max(0,x - y)^2`
   */
  lessThan: {
    name: "lessThan",
    description: "Encourage x to be less than or equal to y: `max(0,x - y)^2`",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn(
      (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(x, y)))
    ),
  },

  /**
   * Repel point `a` from another scalar `b` with weight `weight`.
   */
  repelPt: {
    name: "repelPt",
    description:
      "Repel point `a` from another scalar `b` with weight `weight`.",
    params: [
      { name: "weight", description: "Weight", type: realT() },
      { name: "a", description: "First point", type: realNT() },
      { name: "b", description: "Second point", type: realNT() },
    ],
    body: noWarnFn(
      (weight: ad.Num, a: ad.Num[], b: ad.Num[]): ad.Num =>
        mul(weight, inverse(add(ops.vdistsq(a, b), EPS_DENOM)))
    ),
  },

  /**
   * Repel scalar `c` from another scalar `d`.
   */
  repelScalar: {
    name: "repelScalar",
    description: "Repel scalar `c` from another scalar `d`.",
    params: [
      { name: "c", description: "First scalar", type: realT() },
      { name: "d", description: "Second scalar", type: realT() },
    ],
    body: noWarnFn((c: ad.Num, d: ad.Num): ad.Num => {
      // 1/(c-d)^2
      return inverse(add(squared(sub(c, d)), EPS_DENOM));
    }),
  },
};

// -------- General objective functions
// Defined for all shapes, generally require shape queries or call multiple specific objective functions.
export const objDictGeneral = {
  inDirection: {
    name: "inDirection",
    description:
      "Encourage the point `p` to be in the direction `direction` with respect to point `pRef`. The `direction` vector does not need to be normalized. The `offset` parameter is the shortest allowed distance between the points.",
    params: [
      { name: "p", type: real2T() },
      { name: "pRef", type: real2T() },
      { name: "direction", type: real2T() },
      { name: "offset", type: realT() },
    ],
    body: noWarnFn(inDirection),
  },
  /**
   * Encourage the center of `sTop` to be above the center of `sBottom`.
   * Only works for shapes with property `center`.
   */
  below: {
    name: "below",
    description:
      "Encourage the center of `bottom` to be below the center of `top`.",
    params: [
      {
        name: "bottom",
        type: shapeT("AnyShape"),
        description: "shape on the bottom",
      },
      {
        name: "top",
        type: shapeT("AnyShape"),
        description: "shape on the top",
      },
      {
        name: "offset",
        type: realT(),
        description: "distance between the two centers",
        default: 100,
      },
    ],
    body: noWarnFn(
      (bottom: Shape<ad.Num>, top: Shape<ad.Num>, offset = 100): ad.Num => {
        return inDirection(
          shapeCenter(bottom),
          shapeCenter(top),
          [0, 1],
          offset
        );
      }
    ),
  },

  /**
   * Encourage the center of `sBottom` to be below the center of `sTop`.
   */
  above: {
    name: "above",
    description:
      "Encourage the center of `top` to be above the center of `bottom`.",
    params: [
      {
        name: "top",
        type: shapeT("AnyShape"),
        description: "shape on the top",
      },
      {
        name: "bottom",
        type: shapeT("AnyShape"),
        description: "shape on the bottom",
      },
      {
        name: "offset",
        type: realT(),
        description: "distance between the two centers",
        default: 100,
      },
    ],
    body: noWarnFn(
      (top: Shape<ad.Num>, bottom: Shape<ad.Num>, offset = 100): ad.Num => {
        return inDirection(
          shapeCenter(top),
          shapeCenter(bottom),
          [0, 1],
          offset
        );
      }
    ),
  },

  /**
   * Encourage the center of `sLeft` to be leftwards to the center of `sRight`.
   */
  leftwards: {
    name: "leftwards",
    description:
      "Encourage the center of `left` to be leftwards to the center of `right`.",
    params: [
      {
        name: "left",
        type: shapeT("AnyShape"),
        description: "shape on the left",
      },
      {
        name: "right",
        type: shapeT("AnyShape"),
        description: "shape on the right",
      },
      {
        name: "offset",
        type: realT(),
        description: "distance between the two centers",
        default: 100,
      },
    ],
    body: noWarnFn(
      (left: Shape<ad.Num>, right: Shape<ad.Num>, offset = 100): ad.Num => {
        return inDirection(
          shapeCenter(left),
          shapeCenter(right),
          [1, 0],
          offset
        );
      }
    ),
  },

  /**
   * Encourage the center of `sRight` to be rightwards to the center of `sLeft`.
   */
  rightwards: {
    name: "rightwards",
    description:
      "Encourage the center of `right` to be rightwards to the center of `left`.",
    params: [
      {
        name: "right",
        type: shapeT("AnyShape"),
        description: "shape on the right",
      },
      {
        name: "left",
        type: shapeT("AnyShape"),
        description: "shape on the left",
      },
      {
        name: "offset",
        type: realT(),
        description: "distance between the two centers",
        default: 100,
      },
    ],
    body: noWarnFn(
      (right: Shape<ad.Num>, left: Shape<ad.Num>, offset = 100): ad.Num => {
        return inDirection(
          shapeCenter(right),
          shapeCenter(left),
          [1, 0],
          offset
        );
      }
    ),
  },

  /**
   * Encourage shape `s1` to have the same center position as shape `s2`.
   */
  sameCenter: {
    name: "sameCenter",
    description:
      "Encourage shape `s1` to have the same center position as shape `s2`.",
    params: [
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s2", type: shapeT("AnyShape"), description: "a shape" },
    ],
    body: noWarnFn((s1: Shape<ad.Num>, s2: Shape<ad.Num>): ad.Num => {
      const center1 = shapeCenter(s1);
      const center2 = shapeCenter(s2);
      return ops.vdistsq(center1, center2);
    }),
  },

  /**
   * Try to repel shapes `s1` and `s2` with some weight.
   */
  notTooClose: {
    name: "notTooClose",
    description: "Try to repel shapes `s1` and `s2` with some weight.",
    params: [
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s2", type: shapeT("AnyShape"), description: "a shape" },
      {
        name: "weight",
        type: realT(),
        description: "weight of repel",
        default: 10.0,
      },
    ],
    body: noWarnFn(
      (s1: Shape<ad.Num>, s2: Shape<ad.Num>, weight = 10.0): ad.Num => {
        // HACK: `notTooClose` typically needs to have a weight multiplied since its magnitude is small
        // TODO: find this out programmatically
        const repelWeight = 10e6;

        let res;

        // Repel a line `s1` from another shape `s2` with a center.
        if (isLinelike(s1)) {
          const line = s1;
          const c2 = shapeCenter(s2);
          const lineSamplePts = sampleSeg(linePts(line));
          const allForces = addN(
            lineSamplePts.map((p) => repelPoint(weight, c2, p))
          );
          res = mul(weight, allForces);
        } else {
          // Repel any two shapes with a center.
          // 1 / (d^2(cx, cy) + eps)
          res = inverse(
            add(ops.vdistsq(shapeCenter(s1), shapeCenter(s2)), EPS_DENOM)
          );
        }

        return mul(res, repelWeight);
      }
    ),
  },

  /**
   * Try to place shape `s1` near shape `s2` (putting their centers at the same place).
   */
  near: {
    name: "near",
    description:
      "Try to place shape `s1` near shape `s2` (putting their centers at the same place).",
    params: [
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s2", type: shapeT("AnyShape"), description: "a shape" },
      {
        name: "offset",
        type: realT(),
        description: "offset",
        default: 10.0,
      },
    ],
    body: noWarnFn(
      (s1: Shape<ad.Num>, s2: Shape<ad.Num>, offset = 10.0): ad.Num => {
        const res = absVal(ops.vdistsq(shapeCenter(s1), shapeCenter(s2)));
        return sub(res, squared(offset));
      }
    ),
  },

  /**
   * Try to place shape `s1` near a location `(x, y)`.
   */
  nearPt: {
    name: "nearPt",
    description: "Try to place shape `s1` near a location `(x, y)`.",
    params: [
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "x", type: realT(), description: "`x`" },
      { name: "y", type: realT(), description: "`y`" },
    ],
    body: noWarnFn((s1: Shape<ad.Num>, x: ad.Num, y: ad.Num): ad.Num => {
      return ops.vdistsq(shapeCenter(s1), [x, y]);
    }),
  },

  /**
   * Repel the angle between the p1-p0 and p1-p2 away from 0 and 180 degrees.
   * NOTE: angles more than `range` degrees from 0 or 180 deg are considered satisfied.
   */
  nonDegenerateAngle: {
    name: "nonDegenerateAngle",
    description: "Try to place shape `s1` near a location `(x, y)`.",
    params: [
      { name: "s0", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s2", type: shapeT("AnyShape"), description: "a shape" },
      { name: "strength", type: realT(), description: "strength", default: 20 },
      { name: "range", type: realT(), description: "range", default: 10 },
    ],
    body: noWarnFn(
      (
        s0: Shape<ad.Num>,
        s1: Shape<ad.Num>,
        s2: Shape<ad.Num>,
        strength = 20,
        range = 10
      ): ad.Num => {
        const c0 = shapeCenter(s0);
        const c1 = shapeCenter(s1);
        const c2 = shapeCenter(s2);

        const l1 = ops.vsub(c0, c1);
        const l2 = ops.vsub(c2, c1);
        const cosine = absVal(ops.vdot(ops.vnormalize(l1), ops.vnormalize(l2)));
        // angles that are more than `range` deg from 0 or 180 do not need to be pushed
        return ifCond(
          lt(cosine, range * (Math.PI / 180)),
          0,
          mul(strength, cosine)
        );
      }
    ),
  },
};

// -------- Specific objective functions
// Defined only for specific use-case or specific shapes.
export const objDictSpecific = {
  centerLabelAbove: {
    name: "centerLabelAbove",
    params: [
      { name: "s1", type: shapeT("Line") },
      { name: "s2", type: rectlikeT() },
      { name: "w", type: realT() },
    ],
    body: noWarnFn(
      (s1: Line<ad.Num>, s2: Rectlike<ad.Num>, w: ad.Num): ad.Num => {
        const [arr, text] = [s1, s2];
        const mx = div(add(arr.start.contents[0], arr.end.contents[0]), 2);
        const my = div(add(arr.start.contents[1], arr.end.contents[1]), 2);

        // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
        const textBB = bboxFromShape(text);
        const lh = squared(sub(mx, textBB.center[0]));
        const rh = squared(
          sub(add(my, mul(textBB.height, 1.1)), textBB.center[1])
        );
        return mul(add(lh, rh), w);
      }
    ),
  },

  /**
   * Try to center a label `s2` with respect to some shape `s1`.
   */
  centerLabel: {
    name: "centerLabel",
    description: "Try to center a label `s2` with respect to some shape `s1`.",
    params: [
      { name: "s1", type: unionT(shapeT("Line"), rectlikeT()) },
      { name: "s2", type: rectlikeT() },
      { name: "w", type: realT() },
      { name: "padding", type: realT(), default: 10 },
    ],
    body: noWarnFn(
      (
        s1: Linelike<ad.Num> | Rectlike<ad.Num>,
        s2: Rectlike<ad.Num>,
        w: number,
        padding = 10
      ): ad.Num => {
        if (isLinelike(s1)) {
          // The distance between the midpoint of the arrow and the center of the text should be approx. the label's "radius" plus some padding
          const [arr, text] = [s1, s2];
          const midpt = ops.vdiv(
            ops.vadd(arr.start.contents, arr.end.contents),
            2
          );
          const textBB = bboxFromShape(text);
          // is (x-y)^2 = x^2-2xy+y^2 better? or x^2 - y^2?
          return add(
            sub(ops.vdistsq(midpt, textBB.center), squared(textBB.width)),
            squared(padding)
          );
        } else {
          // Try to center label in the rectangle
          // TODO: This should be applied generically on any two GPIs with a center
          return ops.vdistsq(shapeCenter(s1), shapeCenter(s2));
        }
      }
    ),
  },

  /**
   * try to make distance between a point and a segment `s1` = padding.
   */
  pointLineDist: {
    name: "pointLineDist",
    description:
      "Try to make distance between a point and a segment `s1` equal to padding.",
    params: [
      { name: "point", type: real2T() },
      { name: "s1", type: shapeT("Line") },
      { name: "padding", type: realT() },
    ],
    body: noWarnFn(
      (point: ad.Num[], s1: Line<ad.Num>, padding: ad.Num): ad.Num => {
        return squared(
          sub(
            ops.vdist(
              closestPt_PtSeg(point, [s1.start.contents, s1.end.contents]),
              point
            ),
            padding
          )
        );
      }
    ),
  },

  /**
   * The shape should be regular (equiangular and equilateral)
   */
  isRegular: {
    name: "isRegular",
    description: "Try to make the shape regular",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: noWarnFn((points: ad.Num[][], closed: boolean): ad.Num => {
      const equilater = constrDictCurves.isEquilateral.body(
        points,
        closed
      ).value;
      const equiangular = constrDictCurves.isEquiangular.body(
        points,
        closed
      ).value;
      return add(equilater, equiangular);
    }),
  },

  /**
   * The shape should be equilateral
   */
  isEquilateral: {
    name: "isEquilateral",
    description: "Try to make the shape equilateral",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: noWarnFn((points: ad.Num[][], closed: boolean): ad.Num => {
      return constrDictCurves.isEquilateral.body(points, closed).value;
    }),
  },

  /**
   * The shape should be equiangular
   */
  isEquiangular: {
    name: "isEquiangular",
    description: "Try to make the shape equiangular",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: noWarnFn((points: ad.Num[][], closed: boolean): ad.Num => {
      return constrDictCurves.isEquiangular.body(points, closed).value;
    }),
  },
};

export const objDict = {
  ...objDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...objDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific objective functions.
  ...objDictSpecific, // Defined only for specific use-case or specific shapes.
};

// `_objDictVals` causes TypeScript to enforce that every function in
// `objDict` actually has type `ObjFunc` with the right function signature, etc.
const _objDictVals: ObjFunc[] = Object.values(objDict);
