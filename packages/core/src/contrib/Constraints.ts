import {
  absVal,
  add,
  addN,
  constOf,
  constOfIf,
  div,
  max,
  min,
  mul,
  neg,
  ops,
  squared,
  sub,
  ifCond,
  lt,
} from "engine/Autodiff";
import {
  inRange,
  overlap1D,
  pointInBox,
  noIntersectCircles,
  atDistOutside,
} from "contrib/Utils";
import { 
  bboxFromShape,
  shapeCenter,
  shapeSize,
} from "contrib/Queries";
import * as _ from "lodash";
import { isLinelike, isRectlike } from "renderer/ShapeDef";
import { 
  overlappingLines,
  overlappingCircles,
  overlappingPolygons,
  overlappingAABBs,
  containsCircles, 
  containsCircleRectlike,
  containsRectlikeCircle,
  containsSquareCircle,
  containsSquareLinelike,
  containsAABBs,
} from "contrib/ConstraintsUtils";
import { VarAD } from "types/ad";
import * as BBox from "engine/BBox";

// -------- Simple constraints
// Do not require shape quaries, operate directly with `VarAD` parameters.
const constrDictSimple = {

  /**
   * Require that the value `x` is less than the value `y`
   */
  equal: (x: VarAD, y: VarAD) => {
    return absVal(sub(x, y))
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
    return add(constrDictSimple.lessThanSq(l1, l2), constrDictSimple.lessThanSq(r2, r1));
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
  maxSize: ([shapeType, props]: [string, any], limit: VarAD) => {
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
    if (isLinelike(t1) && isLinelike(t2))
      return overlappingLines([t1, s1], [t2, s2], padding);
    else if (t1 === "Circle" && t2 === "Circle")
      return overlappingCircles([t1, s1], [t2, s2], padding);
    else if (t1 === "Polygon" && t2 === "Polygon")
      return overlappingPolygons([t1, s1], [t2, s2], padding);
    else
      return overlappingAABBs([t1, s1], [t2, s2], padding);
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`, based on the type of the shape, and with an optional `padding` between them (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
   */
  disjoint: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 0.0
  ) => {
    return neg(constrDictGeneral.overlapping([t1, s1], [t2, s2], padding));
  },
  
  /**
   * Require that shape `s1` is tangent to shape `s2`.
   */
  tangentTo: (
    [t1, s1]: [string, any], 
    [t2, s2]: [string, any]
  ) => {
    return absVal(constrDictGeneral.overlapping([t1, s1], [t2, s2]));
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
    else if (t1 === "Circle" && isRectlike(t2))
      return containsCircleRectlike([t1, s1], [t2, s2], padding);
    else if (isRectlike(t1) && t1 !== "Square" && t2 === "Circle")
      return containsRectlikeCircle([t1, s1], [t2, s2], padding);
    else if (t1 === "Square" && t2 === "Circle")
      return containsSquareCircle([t1, s1], [t2, s2], padding);
    else if (t1 === "Square" && isLinelike(t2))
      return containsSquareLinelike([t1, s1], [t2, s2], padding);
    else
      return containsAABBs([t1, s1], [t2, s2], padding);
  },

  /**
   * Require that shape `s1` is smaller than `s2` with some relative padding `relativePadding`.
   */
  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any], relativePadding = 0.4) => {
    // s1 is smaller than s2
    const size1 = shapeSize([t1, s1]);
    const size2 = shapeSize([t2, s2]);
    const padding = mul(constOf(relativePadding), size2);
    return sub(sub(size1, size2), padding);
  },

  /**
   * Require that the `center`s of three shapes to be collinear. 
   * Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
   */
  collinearUnordered: (
    [t0, p0]: [string, any],
    [t1, p1]: [string, any],
    [t2, p2]: [string, any]
  ) => {
    const c1 = shapeCenter([t0, p0]);
    const c2 = shapeCenter([t1, p1]);
    const c3 = shapeCenter([t2, p2]);

    const v1 = ops.vnorm(ops.vsub(c1, c2));
    const v2 = ops.vnorm(ops.vsub(c2, c3));
    const v3 = ops.vnorm(ops.vsub(c1, c3));

    // Use triangle inequality (v1 + v2 <= v3) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
    return max(
      constOf(0),
      min(
        min(sub(add(v1, v2), v3), sub(add(v1, v3), v2)),
        sub(add(v2, v3), v1)
      )
    );
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
    if (!isLinelike(t1) || !isLinelike(t2)) {
      throw Error("expected two line-like shapes");
    }
    return overlap1D(
      [s1.start.contents[0], s1.end.contents[0]],
      [s2.start.contents[0], s2.end.contents[0]]
    );
  },

  /**
   * Require that label `s2` is at a distance of `padding` from a point-like shape `s1`.
   */
  atDist: ([t1, s1]: [string, any], [t2, s2]: [string, any], padding: VarAD) => {
    // TODO: Account for the size/radius of the initial point, rather than just the center

    if (isRectlike(t2)) {
      let pt;
      if (isLinelike(t1)) {
        // Position label close to the arrow's end
        pt = { x: s1.end.contents[0], y: s1.end.contents[1] };
      } else {
        // Only assume shape1 has a center
        pt = { x: s1.center.contents[0], y: s1.center.contents[1] };
      }

      // Get polygon of text (box)
      // TODO: Make this a GPI property
      // TODO: Do this properly; Port the matrix stuff in `textPolygonFn` / `textPolygonFn2` in Shapes.hs
      // I wrote a version simplified to work for rectangles
      const text = s2;
      const rect = bboxFromShape([t2, text]);
      
      return ifCond(
        pointInBox(pt, rect),
        // If the point is inside the box, push it outside w/ `noIntersect`
        noIntersectCircles(rect.center, rect.w, [pt.x, pt.y], constOf(2.0)),
        // If the point is outside the box, try to get the distance from the point to equal the desired distance
        atDistOutside(pt, rect, padding)
      );
    } else {
      throw Error(`unsupported shapes for 'atDist': ${t1}, ${t2}`);
    }
  },

}

export const constrDict = Object.assign({}, 
  constrDictSimple,
  constrDictGeneral,
  constrDictSpecific
);
