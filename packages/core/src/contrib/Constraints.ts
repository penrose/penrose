import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  ifCond,
  lt,
  max,
  min,
  mul,
  neg,
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { shapedefs } from "../shapes/Shapes";
import * as ad from "../types/ad";
import {
  atDistLabel,
  containsAABBs,
  containsCirclePolygon,
  containsCircleRectlike,
  containsCircles,
  containsPolygonCircle,
  containsPolygonPolygon,
  containsRectlikeCircle,
  overlappingAABBs,
  overlappingCircleEllipse,
  overlappingCircleLine,
  overlappingCircles,
  overlappingEllipse,
  overlappingPolygonEllipse,
  overlappingPolygons,
  overlappingRectlikeCircle,
  overlappingTextLine,
} from "./ConstraintsUtils";
import { constrDictCurves } from "./CurveConstraints";
import { bboxFromShape, shapeSize } from "./Queries";
import { inRange, overlap1D } from "./Utils";

// -------- Simple constraints
// Do not require shape queries, operate directly with `ad.Num` parameters.
const constrDictSimple = {
  /**
   * Require that the value `x` is equal to the value `y`
   */
  equal: (x: ad.Num, y: ad.Num) => {
    return absVal(sub(x, y));
  },

  /**
   * Require that the value `x` is less than the value `y` with optional padding `padding`
   */
  lessThan: (x: ad.Num, y: ad.Num, padding = 0) => {
    return add(sub(x, y), padding);
  },

  /**
   * Require that the value `x` is greater than the value `y` with optional padding `padding`
   */
  greaterThan: (x: ad.Num, y: ad.Num, padding = 0) => {
    return add(sub(y, x), padding);
  },

  /**
   * Require that the value `x` is less than the value `y`, with steeper penalty
   */
  lessThanSq: (x: ad.Num, y: ad.Num) => {
    // if x < y then 0 else (x - y)^2
    return ifCond(lt(x, y), 0, squared(sub(x, y)));
  },

  /**
   * Require that the value `x` is greater than the value `y`, with steeper penalty
   */
  greaterThanSq: (x: ad.Num, y: ad.Num) => {
    return ifCond(lt(y, x), 0, squared(sub(y, x)));
  },

  /**
   * Require that the value `x` is in the range defined by `[x0, x1]`.
   */
  inRange: (x: ad.Num, x0: ad.Num, x1: ad.Num) => {
    return mul(sub(x, x0), sub(x, x1));
  },

  /**
   * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
   */
  contains1D: (
    [l1, r1]: [ad.Num, ad.Num],
    [l2, r2]: [ad.Num, ad.Num]
  ): ad.Num => {
    // [if len2 <= len1,] require that (l2 > l1) & (r2 < r1)
    return add(max(0, sub(l1, l2)), max(0, sub(r2, r1)));
  },

  /**
   * Make scalar `c` disjoint from a range `left, right`.
   */
  disjointScalar: (c: ad.Num, left: ad.Num, right: ad.Num) => {
    const d = (x: ad.Num, y: ad.Num) => absVal(sub(x, y));

    // if (x \in [l, r]) then min(d(x,l), d(x,r)) else 0
    return ifCond(inRange(c, left, right), min(d(c, left), d(c, right)), 0);
  },

  /**
   * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
   */
  perpendicular: (q: ad.Num[], p: ad.Num[], r: ad.Num[]): ad.Num => {
    const v1 = ops.vsub(q, p);
    const v2 = ops.vsub(r, p);
    const dotProd = ops.vdot(v1, v2);
    return absVal(dotProd);
  },

  /**
   * Require that three points be collinear.
   * Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
   */
  collinear: (c1: ad.Num[], c2: ad.Num[], c3: ad.Num[]) => {
    const v1 = ops.vsub(c2, c1);
    const v2 = ops.vsub(c2, c3);
    return absVal(ops.cross2(v1, v2));
  },

  /**
   * Require that three points be collinear.
   * Depends on the specific ordering of points.
   */
  collinearOrdered: (c1: ad.Num[], c2: ad.Num[], c3: ad.Num[]) => {
    const v1 = ops.vnorm(ops.vsub(c1, c2));
    const v2 = ops.vnorm(ops.vsub(c2, c3));
    const v3 = ops.vnorm(ops.vsub(c1, c3));

    // Use triangle inequality (|v1| + |v2| <= |v3|) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
    return max(0, sub(add(v1, v2), v3));
  },
};

// -------- General constraints
// Defined for all shapes, generally require shape queries or call multiple specific constraints.
const constrDictGeneral = {
  /** Require that `shape` is on the canvas */
  onCanvas: (
    [shapeType, props]: any,
    canvasWidth: ad.Num,
    canvasHeight: ad.Num
  ) => {
    const box = bboxFromShape([shapeType, props]);
    const canvasXRange: [ad.Num, ad.Num] = [
      mul(canvasWidth, -0.5),
      mul(canvasWidth, 0.5),
    ];
    const canvasYRange: [ad.Num, ad.Num] = [
      mul(canvasHeight, -0.5),
      mul(canvasHeight, 0.5),
    ];
    return add(
      constrDict.contains1D(canvasXRange, BBox.xRange(box)),
      constrDict.contains1D(canvasYRange, BBox.yRange(box))
    );
  },
  /**
   * Require that a shape have a size greater than some constant minimum, based on the type of the shape.
   */
  minSize: ([shapeType, props]: [string, any], limit = 50) => {
    return sub(limit, shapeSize([shapeType, props]));
  },

  /**
   * Require that a shape have a size less than some constant maximum, based on the type of the shape.
   */
  maxSize: ([shapeType, props]: [string, any], limit: ad.Num) => {
    return sub(shapeSize([shapeType, props]), limit);
  },

  /**
   * Require that shape `s1` overlaps shape `s2` with some overlap `overlap`.
   * based on the type of the shape, and with an optional `overlap` between them
   * (e.g. if `s1` should be overlapping `s2` with margin `overlap`).
   */
  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    overlap: ad.Num = 0.0
  ) => {
    // Same shapes
    if (t1 === "Circle" && t2 === "Circle")
      return overlappingCircles([t1, s1], [t2, s2], overlap);
    else if (shapedefs[t1].isRectlike && shapedefs[t2].isRectlike)
      return overlappingAABBs([t1, s1], [t2, s2], overlap);
    // HACK: text/label-line, mainly to skip convex partitioning
    else if ((t1 === "Text" || t1 === "Equation") && t2 === "Line")
      return overlappingTextLine([t1, s1], [t2, s2], overlap);
    else if (t1 === "Line" && (t2 === "Text" || t2 === "Equation"))
      return overlappingTextLine([t2, s2], [t1, s1], overlap);
    else if (shapedefs[t1].isPolygonlike && shapedefs[t2].isPolygonlike)
      return overlappingPolygons([t1, s1], [t2, s2], overlap);
    else if (t1 === "Ellipse" && t2 === "Ellipse")
      return overlappingEllipse([t1, s1], [t2, s2], overlap);
    // Rectangle x Circle
    else if (shapedefs[t1].isRectlike && t2 === "Circle")
      return overlappingRectlikeCircle([t1, s1], [t2, s2], overlap);
    else if (t1 === "Circle" && shapedefs[t2].isRectlike)
      return overlappingRectlikeCircle([t2, s2], [t1, s1], overlap);
    // Polygon x Ellipse
    else if (shapedefs[t1].isPolygonlike && t2 === "Ellipse")
      return overlappingPolygonEllipse([t1, s1], [t2, s2], overlap);
    else if (t1 === "Ellipse" && shapedefs[t2].isPolygonlike)
      return overlappingPolygonEllipse([t2, s2], [t1, s1], overlap);
    // Circle x Ellipse
    else if (t1 === "Circle" && t2 === "Ellipse")
      return overlappingCircleEllipse([t1, s1], [t2, s2], overlap);
    else if (t1 === "Ellipse" && t2 === "Circle")
      return overlappingCircleEllipse([t2, s2], [t1, s1], overlap);
    // Circle x Line
    else if (t1 === "Circle" && t2 === "Line")
      return overlappingCircleLine([t1, s1], [t2, s2], overlap);
    else if (t1 === "Line" && t2 === "Circle")
      return overlappingCircleLine([t2, s2], [t1, s1], overlap);
    // Default to axis-aligned bounding boxes
    else return overlappingAABBs([t1, s1], [t2, s2], overlap);
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`,
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
   */
  disjoint: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding: ad.Num = 0.0
  ) => {
    return neg(constrDictGeneral.overlapping([t1, s1], [t2, s2], neg(padding)));
  },

  /**
   * Require that shape `s1` is touching shape `s2`.
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be touching `s2` with margin `padding`).
   */
  touching: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding: ad.Num = 0.0
  ) => {
    return absVal(
      constrDictGeneral.overlapping([t1, s1], [t2, s2], neg(padding))
    );
  },

  /**
   * Require that a shape `s1` contains another shape `s2`,
   * based on the type of the shape, and with an optional `padding` between the sizes of the shapes
   * (e.g. if `s1` should contain `s2` with margin `padding`).
   */
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 0.0
  ) => {
    if (t1 === "Circle" && t2 === "Circle")
      return containsCircles([t1, s1], [t2, s2], padding);
    else if (t1 === "Polygon" && t2 === "Polygon")
      return containsPolygonPolygon([t1, s1], [t2, s2], padding);
    else if (t1 === "Polygon" && t2 === "Circle")
      return containsPolygonCircle([t1, s1], [t2, s2], padding);
    else if (t1 === "Circle" && t2 === "Polygon")
      return containsCirclePolygon([t1, s1], [t2, s2], padding);
    else if (t1 === "Circle" && shapedefs[t2].isRectlike)
      return containsCircleRectlike([t1, s1], [t2, s2], padding);
    else if (shapedefs[t1].isRectlike && t2 === "Circle")
      return containsRectlikeCircle([t1, s1], [t2, s2], padding);
    else return containsAABBs([t1, s1], [t2, s2], padding);
  },

  /**
   * Require that shape `s1` is at a distance of `distance` from shape `s2`.
   */
  atDist: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    distance: number
  ) => {
    if (shapedefs[t2].isRectlike)
      return atDistLabel([t1, s1], [t2, s2], distance);
    else return constrDictGeneral.touching([t1, s1], [t2, s2], distance);
  },

  /**
   * Require that shape `s1` is smaller than `s2` with some relative padding `relativePadding`.
   */
  smallerThan: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    relativePadding = 0.4
  ) => {
    // s1 is smaller than s2
    const size1 = shapeSize([t1, s1]);
    const size2 = shapeSize([t2, s2]);
    const padding = mul(relativePadding, size2);
    return sub(sub(size1, size2), padding);
  },
};

// -------- Specific constraints
// Defined only for specific use-case or specific shapes.
const constrDictSpecific = {
  /**
   * Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.
   */
  disjointIntervals: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (!shapedefs[t1].isLinelike || !shapedefs[t2].isLinelike) {
      throw Error("expected two line-like shapes");
    }
    return overlap1D(
      [s1.start.contents[0], s1.end.contents[0]],
      [s2.start.contents[0], s2.end.contents[0]]
    );
  },
};

export const constrDict = {
  ...constrDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...constrDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific constrains.
  ...constrDictSpecific, // Defined only for specific use-case or specific shapes.
  ...constrDictCurves, // Curve-specific constraints.
};

// `_constrDictVals` causes TypeScript to enforce that every function in
// `constrDict` returns an `ad.Num`
const _constrDictVals: ((...rest: never[]) => ad.Num)[] = Object.values(
  constrDict
);
