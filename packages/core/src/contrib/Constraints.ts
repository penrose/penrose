import { constOf, constOfIf, ops } from "engine/Autodiff";
import {
  absVal,
  add,
  max,
  min,
  minN,
  mul,
  neg,
  squared,
  sub,
  ifCond,
  lt,
} from "engine/AutodiffFunctions";
import { inRange, overlap1D } from "contrib/Utils";
import { shapeCenter, shapeSize } from "contrib/Queries";
import * as _ from "lodash";
import { shapedefs } from "shapes/Shapes";
import {
  overlappingLines,
  overlappingCircles,
  overlappingPolygons,
  overlappingRectlikeCircle,
  overlappingCircleLine,
  overlappingAABBs,
  containsCircles,
  containsCircleRectlike,
  containsRectlikeCircle,
  containsSquareCircle,
  containsSquareLinelike,
  containsAABBs,
} from "contrib/ConstraintsUtils";
import { VarAD } from "types/ad";

// -------- Simple constraints
// Do not require shape quaries, operate directly with `VarAD` parameters.
const constrDictSimple = {
  /**
   * Require that the value `x` is equal to the value `y`
   */
  equal: (x: VarAD, y: VarAD) => {
    return absVal(sub(x, y));
  },

  /**
   * Require that the value `x` is greater than the value `y` with optional padding `padding`
   */
  greaterThan: (x: VarAD, y: VarAD, padding = 0) => {
    return add(sub(y, x), constOfIf(padding));
  },

  /**
   * Require that the value `x` is less than the value `y` with optional padding `padding`
   */
  lessThan: (x: VarAD, y: VarAD, padding = 0) => {
    return add(sub(x, y), constOfIf(padding));
  },

  /**
   * Require that the value `x` is less than the value `y`, with steeper penalty
   */
  lessThanSq: (x: VarAD, y: VarAD) => {
    // if x < y then 0 else (x - y)^2
    return ifCond(lt(x, y), constOf(0), squared(sub(x, y)));
  },

  /**
   * Require that the value `x` is in the range defined by `[x0, x1]`.
   */
  inRange: (x: VarAD, x0: VarAD, x1: VarAD) => {
    return mul(sub(x, x0), sub(x, x1));
  },

  /**
   * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
   */
  contains1D: ([l1, r1]: [VarAD, VarAD], [l2, r2]: [VarAD, VarAD]): VarAD => {
    // [if len2 <= len1,] require that (l2 > l1) & (r2 < r1)
    return add(
      constrDictSimple.lessThanSq(l1, l2),
      constrDictSimple.lessThanSq(r2, r1)
    );
  },

  /**
   * Make scalar `c` disjoint from a range `left, right`.
   */
  disjointScalar: (c: any, left: any, right: any) => {
    const d = (x: VarAD, y: VarAD) => absVal(sub(x, y));

    // if (x \in [l, r]) then min(d(x,l), d(x,r)) else 0
    return ifCond(
      inRange(c, left, right),
      min(d(c, left), d(c, right)),
      constOf(0)
    );
  },

  /**
   * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
   */
  perpendicular: (q: VarAD[], p: VarAD[], r: VarAD[]): VarAD => {
    const v1 = ops.vsub(q, p);
    const v2 = ops.vsub(r, p);
    const dotProd = ops.vdot(v1, v2);
    return absVal(dotProd);
  },

  /**
   * Require that three points be collinear.
   * Depends on the specific ordering of points.
   */
  collinear: (c1: VarAD[], c2: VarAD[], c3: VarAD[]) => {
    const v1 = ops.vsub(c1, c2);
    const v2 = ops.vsub(c2, c3);
    const v3 = ops.vsub(c1, c3);

    // Use triangle inequality (|v1| + |v2| <= |v3|) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
    return max(
      constOf(0),
      sub(add(ops.vnorm(v1), ops.vnorm(v2)), ops.vnorm(v3))
    );
  },

  /**
   * Require that three points be collinear.
   * Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
   */
  collinearUnordered: (c1: VarAD[], c2: VarAD[], c3: VarAD[]) => {
    const v1 = ops.vnorm(ops.vsub(c1, c2));
    const v2 = ops.vnorm(ops.vsub(c2, c3));
    const v3 = ops.vnorm(ops.vsub(c1, c3));

    // Use triangle inequality (|v1| + |v2| <= |v3|) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
    return max(constOf(0), minN([
      sub(add(v1, v2), v3),
      sub(add(v1, v3), v2),
      sub(add(v2, v3), v1),
    ]));
  },

}

// -------- General constraints
// Defined for all shapes, generally require shape queries or call multiple specific constraints.
const constrDictGeneral = {
  /**
   * Require that a shape have a size greater than some constant minimum, based on the type of the shape.
   */
  minSize: ([shapeType, props]: [string, any], limit = 50) => {
    return sub(constOfIf(limit), shapeSize([shapeType, props]));
  },

  /**
   * Require that a shape have a size less than some constant maximum, based on the type of the shape.
   */
  maxSize: ([shapeType, props]: [string, any], limit: number | VarAD) => {
    return sub(shapeSize([shapeType, props]), constOfIf(limit));
  },

  /**
   * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
   */
  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 0.0
  ) => {
    if (shapedefs[t1].isLinelike && shapedefs[t2].isLinelike)
      return overlappingLines([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Circle" && t2 === "Circle")
      return overlappingCircles([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Polygon" && t2 === "Polygon")
      return overlappingPolygons([t1, s1], [t2, s2], constOfIf(padding));
    else if (shapedefs[t1].isRectlike && t2 === "Circle")
      return overlappingRectlikeCircle([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Circle" && t2 === "Line")
      return overlappingCircleLine([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Line" && t2 === "Circle")
      return overlappingCircleLine([t2, s2], [t1, s1], constOfIf(padding));
    else {
      // TODO: After compilation time fix (issue #652), replace by:
      // return overlappingAABBsMinkowski([t1, s1], [t2, s2], constOfIf(padding));
      return overlappingAABBs([t1, s1], [t2, s2], constOfIf(padding));
    }
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`, 
   * based on the type of the shape, and with an optional `padding` between them 
   * (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
   */
  disjoint: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 0.0
  ) => {
    return neg(constrDictGeneral.overlapping([t1, s1], [t2, s2], padding));
  },

  /**
   * Require that shape `s1` is tangent to shape `s2`, 
   * based on the type of the shape, and with an optional `padding` between them 
   * (e.g. if `s1` should contain `s2` with margin `padding`).
   */
  tangentTo: (
    [t1, s1]: [string, any], 
    [t2, s2]: [string, any],
    padding = 0.0
  ) => {
    return absVal(constrDictGeneral.overlapping([t1, s1], [t2, s2], padding));
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
      return containsCircles([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Circle" && shapedefs[t2].isRectlike)
      return containsCircleRectlike([t1, s1], [t2, s2], constOfIf(padding));
    else if (shapedefs[t1].isRectlike && t1 !== "Square" && t2 === "Circle")
      return containsRectlikeCircle([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Square" && t2 === "Circle")
      return containsSquareCircle([t1, s1], [t2, s2], constOfIf(padding));
    else if (t1 === "Square" && shapedefs[t2].isLinelike)
      return containsSquareLinelike([t1, s1], [t2, s2], constOfIf(padding));
    else return containsAABBs([t1, s1], [t2, s2], constOfIf(padding));
  },

  /**
   * Require that label `s2` is at a distance of `distance` from shape `s1`.
   */
  atDist: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    distance: number
  ) => {
    return constrDictGeneral.tangentTo([t1, s1], [t2, s2], distance);
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
    const padding = mul(constOfIf(relativePadding), size2);
    return sub(sub(size1, size2), padding);
  },

}

// -------- Specific constraints
// Defined only for specific use-case or specific shapes.
const constrDictSpecific = {
  ptCircleIntersect: (p: VarAD[], [t, s]: [string, any]) => {
    if (t === "Circle") {
      const r = s.r.contents;
      const c = shapeCenter([t, s]);
      return squared(sub(ops.vdist(p, c), r));
    } else throw new Error(`${t} not supported for ptCircleIntersect`);
  },

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
  ...constrDictSimple, // Do not require shape quaries, operate directly with `VarAD` parameters.
  ...constrDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific constrains.
  ...constrDictSpecific, // Defined only for specific use-case or specific shapes.
};
