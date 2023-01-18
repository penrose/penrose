import { inDirection } from "contrib/ObjectivesUtils";
import { bboxFromShape, shapeCenter } from "contrib/Queries";
import { closestPt_PtSeg, repelPoint, sampleSeg } from "contrib/Utils";
import { ops } from "engine/Autodiff";
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
} from "engine/AutodiffFunctions";
import { Path } from "shapes/Path";
import { Polygon } from "shapes/Polygon";
import { Polyline } from "shapes/Polyline";
import { shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import { linePts } from "utils/Util";
import { constrDictCurves } from "./CurveConstraints";

// -------- Simple objective functions
// Do not require shape queries, operate directly with `ad.Num` parameters.
export const objDictSimple = {
  /**
   * Encourage the input value to be close to negative infinity
   */
  minimal: (x: ad.Num): ad.Num => x,

  /**
   * Encourage the input value to be close to infinity
   */
  maximal: (x: ad.Num): ad.Num => neg(x),

  /**
   * Encourage the inputs to have the same value: `(x - y)^2`
   */
  equal: (x: ad.Num, y: ad.Num): ad.Num => squared(sub(x, y)),

  /**
   * Encourage x to be greater than or equal to y: `max(0,y - x)^2`
   */
  greaterThan: (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(y, x))),

  /**
   * Encourage x to be less than or equal to y: `max(0,x - y)^2`
   */
  lessThan: (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(x, y))),

  /**
   * Repel point `a` from another scalar `b` with weight `weight`.
   */
  repelPt: (weight: ad.Num, a: ad.Num[], b: ad.Num[]): ad.Num =>
    mul(weight, inverse(ops.vdistsq(a, b))),

  /**
   * Repel scalar `c` from another scalar `d`.
   */
  repelScalar: (c: ad.Num, d: ad.Num): ad.Num => {
    // 1/(c-d)^2
    return inverse(squared(sub(c, d)));
  },
};

// -------- General objective functions
// Defined for all shapes, generally require shape queries or call multiple specific objective functions.
export const objDictGeneral = {
  /**
   * Encourage the center of `sTop` to be above the center of `sBottom`.
   * Only works for shapes with property `center`.
   */
  below: (
    [tBottom, sBottom]: [string, any],
    [tTop, sTop]: [string, any],
    offset = 100
  ): ad.Num => {
    return inDirection([tBottom, sBottom], [tTop, sTop], [0, 1], offset);
  },

  /**
   * Encourage the center of `sBottom` to be below the center of `sTop`.
   */
  above: (
    [tTop, sTop]: [string, any],
    [tBottom, sBottom]: [string, any],
    offset = 100
  ): ad.Num => {
    return inDirection([tTop, sTop], [tBottom, sBottom], [0, 1], offset);
  },

  /**
   * Encourage the center of `sLeft` to be leftwards to the center of `sRight`.
   */
  leftwards: (
    [tLeft, sLeft]: [string, any],
    [tRight, sRight]: [string, any],
    offset = 100
  ): ad.Num => {
    return inDirection([tLeft, sLeft], [tRight, sRight], [1, 0], offset);
  },

  /**
   * Encourage the center of `sRight` to be rightwards to the center of `sLeft`.
   */
  rightwards: (
    [tRight, sRight]: [string, any],
    [tLeft, sLeft]: [string, any],
    offset = 100
  ): ad.Num => {
    return inDirection([tRight, sRight], [tLeft, sLeft], [1, 0], offset);
  },

  /**
   * Encourage shape `s1` to have the same center position as shape `s2`.
   */
  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]): ad.Num => {
    const center1 = shapeCenter([t1, s1]);
    const center2 = shapeCenter([t2, s2]);
    return ops.vdistsq(center1, center2);
  },

  /**
   * Try to repel shapes `s1` and `s2` with some weight.
   */
  notTooClose: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    weight = 10.0
  ): ad.Num => {
    // HACK: `notTooClose` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;

    let res;

    // Repel a line `s1` from another shape `s2` with a center.
    if (shapedefs[t1].isLinelike) {
      const line = s1;
      const c2 = shapeCenter([t2, s2]);
      const lineSamplePts = sampleSeg(linePts(line));
      const allForces = addN(
        lineSamplePts.map((p) => repelPoint(weight, c2, p))
      );
      res = mul(weight, allForces);
    } else {
      // Repel any two shapes with a center.
      // 1 / (d^2(cx, cy) + eps)
      res = inverse(ops.vdistsq(shapeCenter([t1, s1]), shapeCenter([t2, s2])));
    }

    return mul(res, repelWeight);
  },

  /**
   * Try to place shape `s1` near shape `s2` (putting their centers at the same place).
   */
  near: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset = 10.0
  ): ad.Num => {
    const res = absVal(
      ops.vdistsq(shapeCenter([t1, s1]), shapeCenter([t2, s2]))
    );
    return sub(res, squared(offset));
  },

  /**
   * Try to place shape `s1` near a location `(x, y)`.
   */
  nearPt: ([t1, s1]: [string, any], x: any, y: any): ad.Num => {
    return ops.vdistsq(shapeCenter([t1, s1]), [x, y]);
  },

  /**
   * Repel the angle between the p1-p0 and p1-p2 away from 0 and 180 degrees.
   * NOTE: angles more than `range` degrees from 0 or 180 deg are considered satisfied.
   */
  nonDegenerateAngle: (
    [t0, p0]: [string, any],
    [t1, p1]: [string, any],
    [t2, p2]: [string, any],
    strength = 20,
    range = 10
  ): ad.Num => {
    const c0 = shapeCenter([t0, p0]);
    const c1 = shapeCenter([t1, p1]);
    const c2 = shapeCenter([t2, p2]);

    const l1 = ops.vsub(c0, c1);
    const l2 = ops.vsub(c2, c1);
    const cosine = absVal(ops.vdot(ops.vnormalize(l1), ops.vnormalize(l2)));
    // angles that are more than `range` deg from 0 or 180 do not need to be pushed
    return ifCond(
      lt(cosine, range * (Math.PI / 180)),
      0,
      mul(strength, cosine)
    );
  },
};

// -------- Specific objective functions
// Defined only for specific use-case or specific shapes.
export const objDictSpecific = {
  centerLabelAbove: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    w: number
  ): ad.Num => {
    if (shapedefs[t1].isLinelike && shapedefs[t2].isRectlike) {
      const [arr, text] = [s1, s2];
      const mx = div(add(arr.start.contents[0], arr.end.contents[0]), 2);
      const my = div(add(arr.start.contents[1], arr.end.contents[1]), 2);

      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const textBB = bboxFromShape([t2, text]);
      const lh = squared(sub(mx, textBB.center[0]));
      const rh = squared(
        sub(add(my, mul(textBB.height, 1.1)), textBB.center[1])
      );
      return mul(add(lh, rh), w);
    } else throw Error("unsupported shapes");
  },

  /**
   * Try to center a label `s2` with respect to some shape `s1`.
   */
  centerLabel: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    w: number,
    padding = 10
  ): ad.Num => {
    if (shapedefs[t1].isLinelike && shapedefs[t2].isRectlike) {
      // The distance between the midpoint of the arrow and the center of the text should be approx. the label's "radius" plus some padding
      const [arr, text] = [s1, s2];
      const midpt = ops.vdiv(ops.vadd(arr.start.contents, arr.end.contents), 2);
      const textBB = bboxFromShape([t2, text]);
      // is (x-y)^2 = x^2-2xy+y^2 better? or x^2 - y^2?
      return add(
        sub(ops.vdistsq(midpt, textBB.center), squared(textBB.width)),
        squared(padding)
      );
    } else if (shapedefs[t1].isRectlike && shapedefs[t2].isRectlike) {
      // Try to center label in the rectangle
      // TODO: This should be applied generically on any two GPIs with a center
      return objDict.sameCenter([t1, s1], [t2, s2]);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`);
  },

  /**
   * try to make distance between a point and a segment `s1` = padding.
   */
  pointLineDist: (
    point: ad.Num[],
    [t1, s1]: [string, any],
    padding: ad.Num
  ): ad.Num => {
    if (!shapedefs[t1].isLinelike) {
      throw new Error(`pointLineDist: expected a point and a line, got ${t1}`);
    }
    return squared(
      sub(
        ops.vdist(
          closestPt_PtSeg(point, [s1.start.contents, s1.end.contents]),
          point
        ),
        padding
      )
    );
  },

  /**
   * The shape should be regular (equiangular and equilateral)
   */
  isRegular: ([t, s]: [string, Polyline | Polygon | Path]): ad.Num => {
    if (t !== "Polyline" && t !== "Polygon" && t !== "Path") {
      throw new Error(
        `isRegular: expected a polygon, polyline or path, got ${t}`
      );
    }
    const equilater = constrDictCurves.isEquilateral([t, s]);
    const equiangular = constrDictCurves.isEquiangular([t, s]);
    return add(equilater, equiangular);
  },
};

export const objDict = {
  ...objDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...objDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific objective functions.
  ...objDictSpecific, // Defined only for specific use-case or specific shapes.
};

// `_objDictVals` causes TypeScript to enforce that every function in
// `objDict` returns an `ad.Num`
const _objDictVals: ((...rest: never[]) => ad.Num)[] = Object.values(objDict);
