import { EPS_DENOM, ops } from "../engine/Autodiff";
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
} from "../engine/AutodiffFunctions";
import { Line } from "../shapes/Line";
import { Path } from "../shapes/Path";
import { Polygon } from "../shapes/Polygon";
import { Polyline } from "../shapes/Polyline";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import {
  objFunc as obj,
  ObjFunc,
  real2Arg as real2,
  realArg as real,
  realNArg as realN,
  rectlikeArg as rectlike,
  rectlikeT,
  shapeArg as shape,
  shapeT,
  unionArg as union,
} from "../types/functions";
import { linePts } from "../utils/Util";
import { constrDictCurves } from "./CurveConstraints";
import { inDirection } from "./ObjectivesUtils";
import { bboxFromShape, shapeCenter } from "./Queries";
import {
  closestPt_PtSeg,
  isLinelike,
  Linelike,
  Rectlike,
  repelPoint,
  sampleSeg,
} from "./Utils";

// -------- Simple objective functions
// Do not require shape queries, operate directly with `ad.Num` parameters.
export const objDictSimple: { [k: string]: ObjFunc } = {
  /**
   * Encourage the input value to be close to negative infinity
   */
  minimal: obj("minimal", [real("x")], (x: ad.Num): ad.Num => x),

  /**
   * Encourage the input value to be close to infinity
   */
  maximal: obj("maximal", [real("x")], (x: ad.Num): ad.Num => neg(x)),

  /**
   * Encourage the inputs to have the same value: `(x - y)^2`
   */
  equal: obj(
    "equal",
    [real("x"), real("y")],
    (x: ad.Num, y: ad.Num): ad.Num => squared(sub(x, y))
  ),

  /**
   * Encourage x to be greater than or equal to y: `max(0,y - x)^2`
   */
  greaterThan: obj(
    "greaterThan",
    [real("x"), real("y")],
    (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(y, x)))
  ),

  /**
   * Encourage x to be less than or equal to y: `max(0,x - y)^2`
   */
  lessThan: obj(
    "lessThan",
    [real("x"), real("y")],
    (x: ad.Num, y: ad.Num): ad.Num => squared(max(0, sub(x, y)))
  ),

  /**
   * Repel point `a` from another scalar `b` with weight `weight`.
   */
  repelPt: obj(
    "repelPt",
    [real("weight"), realN("a"), realN("b")],
    (weight: ad.Num, a: ad.Num[], b: ad.Num[]): ad.Num =>
      mul(weight, inverse(add(ops.vdistsq(a, b), EPS_DENOM)))
  ),

  /**
   * Repel scalar `c` from another scalar `d`.
   */
  repelScalar: obj(
    "repelScalar",
    [real("a"), real("b")],
    (c: ad.Num, d: ad.Num): ad.Num => {
      // 1/(c-d)^2
      return inverse(add(squared(sub(c, d)), EPS_DENOM));
    }
  ),
};

// -------- General objective functions
// Defined for all shapes, generally require shape queries or call multiple specific objective functions.
export const objDictGeneral: { [k: string]: ObjFunc } = {
  /**
   * Encourage the center of `sTop` to be above the center of `sBottom`.
   * Only works for shapes with property `center`.
   */
  below: obj(
    "below",
    [shape("bottom"), shape("top"), real("offset", 100)],
    (bottom: Shape<ad.Num>, top: Shape<ad.Num>, offset = 100): ad.Num => {
      return inDirection(bottom, top, [0, 1], offset);
    }
  ),

  /**
   * Encourage the center of `sBottom` to be below the center of `sTop`.
   */
  above: obj(
    "above",
    [shape("top"), shape("bottom"), real("offset", 100)],
    (top: Shape<ad.Num>, bottom: Shape<ad.Num>, offset = 100): ad.Num => {
      return inDirection(top, bottom, [0, 1], offset);
    }
  ),

  /**
   * Encourage the center of `sLeft` to be leftwards to the center of `sRight`.
   */
  leftwards: obj(
    "leftwards",
    [shape("left"), shape("right"), real("offset", 100)],
    (left: Shape<ad.Num>, right: Shape<ad.Num>, offset = 100): ad.Num => {
      return inDirection(left, right, [1, 0], offset);
    }
  ),

  /**
   * Encourage the center of `sRight` to be rightwards to the center of `sLeft`.
   */
  rightwards: obj(
    "rightwards",
    [shape("right"), shape("left"), real("offset", 100)],
    (right: Shape<ad.Num>, left: Shape<ad.Num>, offset = 100): ad.Num => {
      return inDirection(right, left, [1, 0], offset);
    }
  ),

  /**
   * Encourage shape `s1` to have the same center position as shape `s2`.
   */
  sameCenter: obj(
    "sameCenter",
    [shape("s1"), shape("s2")],
    (s1: Shape<ad.Num>, s2: Shape<ad.Num>): ad.Num => {
      const center1 = shapeCenter(s1);
      const center2 = shapeCenter(s2);
      return ops.vdistsq(center1, center2);
    }
  ),

  /**
   * Try to repel shapes `s1` and `s2` with some weight.
   */
  notTooClose: obj(
    "notTooClose",
    [shape("s1"), shape("s2"), real("weight", 10.0)],
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

  /**
   * Try to place shape `s1` near shape `s2` (putting their centers at the same place).
   */
  near: obj(
    "near",
    [shape("s1"), shape("s2"), real("offset", 10.0)],
    (s1: Shape<ad.Num>, s2: Shape<ad.Num>, offset = 10.0): ad.Num => {
      const res = absVal(ops.vdistsq(shapeCenter(s1), shapeCenter(s2)));
      return sub(res, squared(offset));
    }
  ),

  /**
   * Try to place shape `s1` near a location `(x, y)`.
   */
  nearPt: obj(
    "nearPt",
    [shape("s1"), real("x"), real("y")],
    (s1: Shape<ad.Num>, x: ad.Num, y: ad.Num): ad.Num => {
      return ops.vdistsq(shapeCenter(s1), [x, y]);
    }
  ),

  /**
   * Repel the angle between the p1-p0 and p1-p2 away from 0 and 180 degrees.
   * NOTE: angles more than `range` degrees from 0 or 180 deg are considered satisfied.
   */
  nonDegenerateAngle: obj(
    "nonDegenerateAngle",
    [
      shape("s0"),
      shape("s1"),
      shape("s2"),
      real("strength", 20),
      real("range", 10),
    ],
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
};

// -------- Specific objective functions
// Defined only for specific use-case or specific shapes.
export const objDictSpecific: { [k: string]: ObjFunc } = {
  centerLabelAbove: obj(
    "centerLabelAbove",
    [shape("s1", "Line"), rectlike("s2"), real("w")],
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

  /**
   * Try to center a label `s2` with respect to some shape `s1`.
   */
  centerLabel: obj(
    "centerLabel",
    [
      union("s1", shapeT("Line"), rectlikeT()),
      rectlike("s2"),
      real("w"),
      real("padding", 10),
    ],
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
        return objDict.sameCenter.body(s1, s2);
      }
    }
  ),

  /**
   * try to make distance between a point and a segment `s1` = padding.
   */
  pointLineDist: obj(
    "pointLineDist",
    [real2("point"), shape("s1", "Line"), real("padding")],
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

  /**
   * The shape should be regular (equiangular and equilateral)
   */
  isRegular: obj(
    "isRegular",
    [union("s", shapeT("Polyline"), shapeT("Polygon"), shapeT("Path"))],
    (s: Polyline<ad.Num> | Polygon<ad.Num> | Path<ad.Num>): ad.Num => {
      const equilater = constrDictCurves.isEquilateral.body(s);
      const equiangular = constrDictCurves.isEquiangular.body(s);
      return add(equilater, equiangular);
    }
  ),
};

export const objDict = {
  ...objDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...objDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific objective functions.
  ...objDictSpecific, // Defined only for specific use-case or specific shapes.
};

// `_objDictVals` causes TypeScript to enforce that every function in
// `objDict` returns an `ad.Num`
// const _objDictVals: ((...rest: never[]) => ad.Num)[] = Object.values(objDict);
