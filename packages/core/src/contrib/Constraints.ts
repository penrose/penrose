import {
  absVal,
  add,
  addN,
  constOf,
  constOfIf,
  div,
  EPS_DENOM,
  fns,
  gt,
  inverse,
  max,
  min,
  mul,
  neg,
  ops,
  squared,
  sub,
  varOf,
  ifCond,
  lt,
  eq,
  and,
  or,
  sqrt,
  minN,
  maxN,
  debug,
  cos,
} from "engine/Autodiff";
import * as _ from "lodash";
import { linePts } from "utils/OtherUtils";
import { findDef, isLinelike, isRectlike } from "renderer/ShapeDef";
import { VarAD } from "types/ad";
import { every } from "lodash";
import * as BBox from "engine/BBox";

export const objDict = {
  /**
   * Encourage the inputs to have the same value: `(x - y)^2`
   */
  equal: (x: VarAD, y: VarAD) => squared(sub(x, y)),

  /**
   * Encourage shape `top` to be above shape `bottom`. Only works for shapes with property `center`.
   */
  above: (
    [t1, top]: [string, any],
    [t2, bottom]: [string, any],
    offset = 100
  ) =>
    // (getY top - getY bottom - offset) ^ 2
    squared(
      sub(sub(top.center.contents[1], bottom.center.contents[1]), varOf(offset))
    ),

  /**
   * Encourage shape `s1` to have the same center position as shape `s2`. Only works for shapes with property `center`.
   */
  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return ops.vdistsq(fns.center(s1), fns.center(s2));
  },

  /**
   * Try to repel shapes `s1` and `s2` with some weight.
   */
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any], weight = 10.0) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;

    let res;

    // Repel a line `s1` from another shape `s2` with a center.
    if (isLinelike(t1)) {
      const line = s1;
      const c2 = fns.center(s2);
      const lineSamplePts = sampleSeg(linePts(line));
      const allForces = addN(
        lineSamplePts.map((p) => repelPoint(constOfIf(weight), c2, p))
      );
      res = mul(constOfIf(weight), allForces);
    } else {
      // Repel any two shapes with a center.
      // 1 / (d^2(cx, cy) + eps)
      res = inverse(ops.vdistsq(fns.center(s1), fns.center(s2)));
    }

    return mul(res, constOf(repelWeight));
  },

  repelPt: (c: VarAD, a: VarAD[], b: VarAD[]) =>
    div(c, add(ops.vdistsq(a, b), constOf(EPS_DENOM))),

  /**
   * Repel scalar `c` from another scalar `d`.
   */
  // TODO: Try to avoid NaNs/blowing up? add eps in denominator if c=d?
  repelScalar: (c: any, d: any) => {
    // 1/(c-d)^2
    return inverse(squared(sub(constOfIf(c), constOfIf(d))));
  },

  /**
   * Try to center the arrow `arr` between the shapes `s2` and `s3` (they can also be any shapes with a center).
   */
  centerArrow: (
    [t1, arr]: [string, any],
    [t2, s2]: [string, any],
    [t3, s3]: [string, any]
  ): VarAD => {
    const spacing = varOf(1.1); // arbitrary

    if (isLinelike(t1) && isRectlike(t2) && isRectlike(t3)) {
      const s2BB = bboxFromShape(t2, s2);
      const s3BB = bboxFromShape(t3, s3);
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, s2BB.center, s3BB.center, [
        mul(spacing, s2BB.h),
        neg(mul(s3BB.h, spacing)),
      ]);
    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

  /**
   * Encourage shape `bottom` to be below shape `top`. Only works for shapes with property `center`.
   */
  below: (
    [t1, bottom]: [string, any],
    [t2, top]: [string, any],
    offset = 100
  ) =>
    // TODO: can this be made more efficient (code-wise) by calling "above" and swapping arguments?
    squared(
      sub(
        sub(top.center.contents[1], bottom.center.contents[1]),
        constOfIf(offset)
      )
    ),

  centerLabelAbove: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    w: number
  ): VarAD => {
    if (isLinelike(t1) && isRectlike(t2)) {
      const [arr, text] = [s1, s2];
      const mx = div(
        add(arr.start.contents[0], arr.end.contents[0]),
        constOf(2.0)
      );
      const my = div(
        add(arr.start.contents[1], arr.end.contents[1]),
        constOf(2.0)
      );

      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const textBB = bboxFromShape(t2, text);
      const lh = squared(sub(mx, textBB.center[0]));
      const rh = squared(
        sub(add(my, mul(textBB.h, constOf(1.1))), textBB.center[1])
      );
      return mul(add(lh, rh), constOfIf(w));
    } else throw Error("unsupported shapes");
  },

  /**
   * Try to center a label `s2` with respect to some shape `s1`.
   */
  centerLabel: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    w: number
  ): VarAD => {
    if (isLinelike(t1) && isRectlike(t2)) {
      // The distance between the midpoint of the arrow and the center of the text should be approx. the label's "radius" plus some padding
      const [arr, text] = [s1, s2];
      const midpt = ops.vdiv(
        ops.vadd(arr.start.contents, arr.end.contents),
        constOf(2.0)
      );
      const padding = constOf(10);
      const textBB = bboxFromShape(t2, text);
      // is (x-y)^2 = x^2-2xy+y^2 better? or x^2 - y^2?
      return add(
        sub(ops.vdistsq(midpt, textBB.center), squared(textBB.w)),
        squared(padding)
      );
    } else if (isRectlike(t1) && isRectlike(t2)) {
      // Try to center label in the rectangle
      // TODO: This should be applied generically on any two GPIs with a center
      return objDict.sameCenter([t1, s1], [t2, s2]);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`);
  },

  /**
   * Try to place shape `s1` near shape `s2` (putting their centers at the same place).
   */
  near: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset = 10.0) => {
    // This only works for two objects with centers (x,y)
    const res = absVal(ops.vdistsq(fns.center(s1), fns.center(s2)));
    return sub(res, squared(constOfIf(offset)));
  },

  /**
   * Try to place shape `s1` near a location `(x, y)`.
   */
  nearPt: ([t1, s1]: [string, any], x: any, y: any) => {
    return ops.vdistsq(fns.center(s1), [constOfIf(x), constOfIf(y)]);
  },

  /**
   * Try to make scalar `c` near another scalar `goal`.
   */
  // TODO: Can these be typed as `VarAD`?
  nearScalar: (c: any, goal: any) => {
    return squared(sub(constOfIf(c), constOfIf(goal)));
  },
  /**
   * Repel the angle between the p1-p0 and p1-p2 away from 0 and 180 degrees.
   * NOTE: angles more than `range` degrees from 0 or 180 deg are considered satisfied.
   */
  nonDegenerateAngle: (
    [, p0]: [string, any],
    [, p1]: [string, any],
    [, p2]: [string, any],
    strength = 20,
    range = 10
  ) => {
    if (every([p0, p1, p2].map((props) => props["center"]))) {
      const c0 = fns.center(p0);
      const c1 = fns.center(p1);
      const c2 = fns.center(p2);

      const l1 = ops.vsub(c0, c1);
      const l2 = ops.vsub(c2, c1);
      const cosine = absVal(ops.vdot(ops.vnormalize(l1), ops.vnormalize(l2)));
      // angles that are more than `range` deg from 0 or 180 do not need to be pushed
      return ifCond(
        lt(cosine, varOf(range * (Math.PI / 180))),
        varOf(0),
        mul(constOfIf(strength), cosine)
      );
    } else {
      throw new Error(
        "nonDegenerateAngle: all input shapes need to have centers"
      );
    }
  },
  /**
   * try to make distance between a point and a segment `s1` = padding.
   */
  pointLineDist: (point: VarAD[], [t1, s1]: [string, any], padding: VarAD) => {
    if (!isLinelike(t1)) {
      throw new Error(`pointLineDist: expected a point and a line, got ${t1}`);
    }
    return squared(
      equalHard(
        ops.vdist(
          closestPt_PtSeg(point, [s1.start.contents, s1.end.contents]),
          point
        ),
        padding
      )
    );
  },
};

export const constrDict = {
  /**
   * Require that the end point of line `s1` is to the right of the start point
   */
  rightwards: ([t1, s1]: [string, any]) => {
    if (t1 === "Line") {
      const x1 = s1.start.contents[0];
      const x2 = s1.end.contents[0];
      return sub(x1, x2);
    } else {
      throw Error(`unsupported shape for 'rightwards': ${t1}`);
    }
  },

  /**
   * Require that a shape have a size less than some constant maximum, based on the type of the shape.
   */
  maxSize: ([shapeType, props]: [string, any], limit: VarAD) => {
    switch (shapeType) {
      case "Circle":
        return sub(props.r.contents, div(limit, constOf(2)));
      case "Square":
        return sub(props.side.contents, limit);
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },

  /**
   * Require that a shape have a size greater than some constant minimum, based on the type of the shape.
   */
  minSize: ([shapeType, props]: [string, any]) => {
    const limit = 20;

    if (isLinelike(shapeType)) {
      const minLen = 50;
      const vec = ops.vsub(props.end.contents, props.start.contents);
      return sub(constOf(minLen), ops.vnorm(vec));
    }

    switch (shapeType) {
      case "Circle":
        return sub(constOf(limit), props.r.contents);
      case "Square":
        return sub(constOf(limit), props.side.contents);
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },

  /**
   * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
   */
  contains1D: ([l1, r1]: [VarAD, VarAD], [l2, r2]: [VarAD, VarAD]): VarAD => {
    // [if len2 <= len1,] require that (l2 > l1) & (r2 < r1)
    return add(constrDict.lessThanSq(l1, l2), constrDict.lessThanSq(r2, r1));
  },

  /**
   * Require that a shape `s1` contains another shape `s2`, based on the type of the shape, and with an optional `offset` between the sizes of the shapes (e.g. if `s1` should contain `s2` with margin `offset`).
   */
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: VarAD
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const o = offset
        ? sub(sub(s1.r.contents, s2.r.contents), offset)
        : sub(s1.r.contents, s2.r.contents);
      const res = sub(d, o);
      return res;
    } else if (t1 === "Circle" && isRectlike(t2)) {
      const s2BBox = bboxFromShape(t2, s2);
      const d = ops.vdist(fns.center(s1), s2BBox.center);
      const textR = max(s2BBox.w, s2BBox.h);
      return add(sub(d, s1.r.contents), textR);
    } else if (isRectlike(t1) && t1 !== "Square" && t2 === "Circle") {
      // collect constants
      const halfW = mul( constOf(0.5), s1.w.contents ); // half rectangle width
      const halfH = mul( constOf(0.5), s1.h.contents ); // half rectangle height
      const [rx, ry] = s1.center.contents; // rectangle center
      const r = s2.r.contents; // circle radius
      const [cx, cy] = s2.center.contents; // circle center
      
      // Return maximum violation in either the x- or y-direction.
      // In each direction, the distance from the circle center (cx,cy) to
      // the rectangle center (rx,ry) must be no greater than the size of
      // the rectangle (w/h), minus the radius of the circle (r) and the
      // offset (o).  We can compute this violation via the function
      //    max( |cx-rx| - (w/2-r-o),
      //         |cy-ry| - (h/2-r-o) )
      const o = typeof(offset)!=='undefined'? offset : constOf(0.);
      return max( sub( absVal(sub(cx,rx)), sub(sub(halfW,r),o) ),
                  sub( absVal(sub(cy,ry)), sub(sub(halfH,r),o) ));
    } else if (t1 === "Square" && t2 === "Circle") {
      // dist (outerx, outery) (innerx, innery) - (0.5 * outer.side - inner.radius)
      const sq = s1.center.contents;
      const d = ops.vdist(sq, fns.center(s2));
      return sub(d, sub(mul(constOf(0.5), s1.side.contents), s2.r.contents));
    } else if (isRectlike(t1) && isRectlike(t2)) {
      const box1 = bboxFromShape(t1, s1);
      const box2 = bboxFromShape(t2, s2);

      // TODO: There are a lot of individual functions added -- should we optimize them individually with a 'fnAnd` construct?
      return add(
        constrDict.contains1D(BBox.xRange(box1), BBox.xRange(box2)),
        constrDict.contains1D(BBox.yRange(box1), BBox.yRange(box2))
      );
    } else if (t1 === "Square" && isLinelike(t2)) {
      const [[startX, startY], [endX, endY]] = linePts(s2);
      const [x, y] = fns.center(s1);

      const r = div(s1.side.contents, constOf(2.0));
      const f = constOf(0.75); // 0.25 padding
      //     (lx, ly) = ((x - side / 2) * 0.75, (y - side / 2) * 0.75)
      //     (rx, ry) = ((x + side / 2) * 0.75, (y + side / 2) * 0.75)
      // in inRange startX lx rx + inRange startY ly ry + inRange endX lx rx +
      //    inRange endY ly ry
      const [lx, ly] = [mul(sub(x, r), f), mul(sub(y, r), f)];
      const [rx, ry] = [mul(add(x, r), f), mul(add(y, r), f)];
      return addN([
        constrDict.inRange(startX, lx, rx),
        constrDict.inRange(startY, ly, ry),
        constrDict.inRange(endX, lx, rx),
        constrDict.inRange(endY, ly, ry),
      ]);
    } else throw new Error(`${[t1, t2]} not supported for contains`);
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
   * Make an AABB rectangle contain an AABB (vertical or horizontal) line. (Special case of rect-rect disjoint). AA = axis-aligned
   */
  containsRectLineAA: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (!isRectlike(t1) || !isLinelike(t2)) {
      throw Error("expected two line-like shapes");
    }

    const box = bboxFromShape(t1, s1);
    const line = bboxFromShape(t2, s2);

    // Contains line both vertically and horizontally
    return add(
      constrDict.contains1D(BBox.xRange(box), BBox.xRange(line)),
      constrDict.contains1D(BBox.yRange(box), BBox.yRange(line))
    );
  },

  /**
   * Make an AABB rectangle disjoint from a vertical or horizontal line.
   * (Special case of rect-rect disjoint)
   */
  disjointRectLineAA: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (!isRectlike(t1) || !isLinelike(t2)) {
      throw Error("expected two line-like shapes");
    }

    const box = bboxFromShape(t1, s1);
    const line = bboxFromShape(t2, s2);

    const overlapX = overlap1D(BBox.xRange(box), BBox.xRange(line));
    const overlapY = overlap1D(BBox.yRange(box), BBox.yRange(line));

    // Push away in both X and Y directions
    return mul(overlapX, overlapY);
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`, based on the type of the shape, and with an optional `offset` between them (e.g. if `s1` should be disjoint from `s2` with margin `offset`).
   */
  disjoint: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset = 0.0
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const o = [s1.r.contents, s2.r.contents, constOf(10.0)];
      return sub(addN(o), d);
    } else if (t1 === "Polygon" && t2 === "Polygon") {
      const cp1 = convexPartitions(s1.points.contents);
      const cp2 = convexPartitions(
        s2.points.contents.map((p: VarAD[]) => ops.vneg(p))
      );
      const sdf = maxN(
        cp1.map((p1) => minN(
          cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2)))
        )
      );
      return neg(sdf);
    } else if (isRectlike(t1) && isLinelike(t2)) {
      const seg = s2;
      const textBB = bboxFromShape(t1, s1);
      const centerT = textBB.center;
      const endpts = linePts(seg);
      const cp = closestPt_PtSeg(centerT, endpts);
      const lenApprox = div(textBB.w, constOf(2.0));
      return sub(add(lenApprox, constOfIf(offset)), ops.vdist(centerT, cp));
    } else if (isRectlike(t1) && isRectlike(t2)) {
      // Assuming AABB (they are axis-aligned [bounding] boxes)
      const box1 = bboxFromShape(t1, s1);
      const box2 = bboxFromShape(t2, s2);
      const [pc1, pc2] = rectangleDifference(box1, box2);
      const [xp, yp] = ops.vmul(constOf(0.5), ops.vadd(pc1, pc2));
      const [xr, yr] = ops.vmul(constOf(0.5), ops.vsub(pc2, pc1));
      const [xq, yq] = ops.vsub([absVal(xp), absVal(yp)], [xr, yr]);
      const e1 = sqrt(
        add(
          constOf(10e-15),
          add(
            squared(max(sub(xp, xr), constOf(0.0))),
            squared(max(sub(yp, yr), constOf(0.0)))
          )
        )
      );
      const e2 = neg(min(max(xq, yq), constOf(0.0)));
      return sub(e2, e1);
    } else {
      // TODO (new case): I guess we might need Rectangle disjoint from polyline? Unless they repel each other?
      throw new Error(`${[t1, t2]} not supported for disjoint`);
    }
  },

  /**
   * Require that two linelike shapes `l1` and `l2` are not crossing (with optional offset `offset` -- TODO: currently not accounted for).
   */
  notCrossing: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset = 5.0
  ) => {
    if (isLinelike(t1) && isLinelike(t2)) {
      // If the lines intersect, return the smallest distance squared between their endpoints (assuming the starts and ends "correspond" -- but not geometrically necessary.) TODO -- Try every pair of distances? (end -> start)
      // Else, return 0.
      // The idea is to minimize the distance between the 'crossing' endpoints, so the lines uncross. Though I guess taking the min may make this discontinuous?

      return ifCond(
        intersects(
          s1.start.contents,
          s1.end.contents,
          s2.start.contents,
          s2.end.contents
        ),
        min(
          ops.vdistsq(s1.start.contents, s2.start.contents),
          ops.vdistsq(s1.start.contents, s2.start.contents)
        ),
        constOf(0)
      );
    } else {
      throw new Error(`${[t1, t2]} not supported for notCrossing`);
    }
  },
  /**
   * Require that shape `s1` is smaller than `s2` with some offset `offset`.
   */
  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // s1 is smaller than s2
    const offset = mul(varOf(0.4), s2.r.contents);
    return sub(sub(s1.r.contents, s2.r.contents), offset);
  },

  /**
   * Require that shape `s1` outside of `s2` with some offset `padding`.
   */
  outsideOf: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (isRectlike(t1) && t2 === "Circle") {
      const s1BBox = bboxFromShape(t1, s1);
      const textR = max(s1BBox.w, s1BBox.h);
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      return sub(add(add(s2.r.contents, textR), constOfIf(padding)), d);
    } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  },

  /**
   * Require that shape `s1` overlaps shape `s2` with some offset `padding`.
   */
  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      return looseIntersect(
        fns.center(s1),
        s1.r.contents,
        fns.center(s2),
        s2.r.contents,
        constOfIf(padding)
      );
    } else throw new Error(`${[t1, t2]} not supported for overlapping`);
  },

  /**
   * Require that shape `s1` is tangent to shape `s2`.
   */
  tangentTo: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      // Since we want equality
      return absVal(sub(d, sub(r1, r2)));
    } else throw new Error(`${[t1, t2]} not supported for tangentTo`);
  },

  ptCircleIntersect: (p: VarAD[], [t, s]: [string, any]) => {
    if (t === "Circle") {
      const r = s.r.contents;
      const c = fns.center(s);
      return squared(sub(ops.vdist(p, c), r));
    } else throw new Error(`${t} not supported for ptCircleIntersect`);
  },

  /**
   * Require that label `s2` is at a distance of `offset` from a point-like shape `s1`.
   */
  atDist: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset: VarAD) => {
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
      const rect = bboxFromShape(t2, text);

      return ifCond(
        pointInBox(pt, rect),
        // If the point is inside the box, push it outside w/ `noIntersect`
        noIntersectCircles(rect.center, rect.w, [pt.x, pt.y], constOf(2.0)),
        // If the point is outside the box, try to get the distance from the point to equal the desired distance
        atDistOutside(pt, rect, offset)
      );
    } else {
      throw Error(`unsupported shapes for 'atDist': ${t1}, ${t2}`);
    }
  },

  /**
   * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
   */
  perpendicular: (q: VarAD[], p: VarAD[], r: VarAD[]): VarAD => {
    const v1 = ops.vsub(q, p);
    const v2 = ops.vsub(r, p);
    const dotProd = ops.vdot(v1, v2);
    return equalHard(dotProd, constOf(0.0));
  },

  /**
   * Require that the value `x` is in the range defined by `[x0, x1]`.
   */
  inRange: (x: VarAD, x0: VarAD, x1: VarAD) => {
    return max(constOf(0), mul(sub(x, x0), sub(x, x1)));
  },

  /**
   * Require that the value `x` is less than the value `y`
   */
  equal: (x: VarAD, y: VarAD) => {
    return equalHard(x, y);
  },

  /**
   * Require that the value `x` is less than the value `y` with optional offset `padding`
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
   * Require that three points be collinear.
   */
  collinear: (c1: VarAD[], c2: VarAD[], c3: VarAD[]) => {
    const v1 = ops.vsub(c1, c2);
    const v2 = ops.vsub(c2, c3);
    const v3 = ops.vsub(c1, c3);

    // Use triangle inequality (v1 + v2 <= v3) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
    return max(
      constOf(0),
      sub(add(ops.vnorm(v1), ops.vnorm(v2)), ops.vnorm(v3))
    );
  },

  /**
   * Require that the `center`s of three shapes to be collinear. Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
   */
  collinearUnordered: (
    [, p0]: [string, any],
    [, p1]: [string, any],
    [, p2]: [string, any]
  ) => {
    if (every([p0, p1, p2].map((props) => props["center"]))) {
      const c1 = fns.center(p0);
      const c2 = fns.center(p1);
      const c3 = fns.center(p2);

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
    } else {
      throw new Error("collinear: all input shapes need to have centers");
    }
  },
};

// -------- Helpers for writing objectives

/**
 * Check that the `inputs` list equals the `expected` list.
 */
const typesAre = (inputs: string[], expected: string[]): boolean =>
  inputs.length === expected.length &&
  _.every(_.zip(inputs, expected).map(([i, e]) => i === e));

// -------- (Hidden) helpers for objective/constraints/computations

/**
 * Require that `x` equals `y`.
 */
const equalHard = (x: VarAD, y: VarAD) => {
  // This is an equality constraint (x = c) via two inequality constraints (x <= c and x >= c)
  const valMax = max(x, y);
  const valMin = min(x, y);
  // TODO: I guess you could also use an absolute value?
  return sub(valMax, valMin);
};

/**
 * Require that a shape at `center1` with radius `r1` not intersect a shape at `center2` with radius `r2` with optional padding `padding`. (For a non-circle shape, its radius should be half of the shape's general "width")
 */
const noIntersectCircles = (
  center1: VarAD[],
  r1: VarAD,
  center2: VarAD[],
  r2: VarAD,
  padding = 10
): VarAD => {
  // noIntersect [[x1, y1, s1], [x2, y2, s2]] = - dist (x1, y1) (x2, y2) + (s1 + s2 + 10)
  const res = add(add(r1, r2), constOfIf(padding));
  return sub(res, ops.vdist(center1, center2));
};

/**
 * Require that a shape at `center1` with radius `r1` intersect a shape at `center2` with radius `r2`, with overlap amount `padding`.
 */
const looseIntersect = (
  center1: VarAD[],
  r1: VarAD,
  center2: VarAD[],
  r2: VarAD,
  padding: VarAD
): VarAD => {
  // looseIntersect [[x1, y1, s1], [x2, y2, s2]] = dist (x1, y1) (x2, y2) - (s1 + s2 - 10)
  const res = sub(add(r1, r2), padding);
  return sub(ops.vdist(center1, center2), res);
};

/**
 * Encourage that an arrow `arr` be centered between two shapes with centers `center1` and `center2`, and text size (?) `[o1, o2]`.
 */
const centerArrow2 = (
  arr: any,
  center1: VarAD[],
  center2: VarAD[],
  [o1, o2]: VarAD[]
): VarAD => {
  const vec = ops.vsub(center2, center1); // direction the arrow should point to
  const dir = ops.vnormalize(vec);

  let start = center1;
  let end = center2;

  // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality

  // TODO: add abs
  if (gt(ops.vnorm(vec), add(o1, absVal(o2)))) {
    start = ops.vadd(center1, ops.vmul(o1, dir));
    end = ops.vadd(center2, ops.vmul(o2, dir));
  }

  const fromPt = arr.start.contents;
  const toPt = arr.end.contents;

  return add(ops.vdistsq(fromPt, start), ops.vdistsq(toPt, end));
};

/**
 * Repel a vector `a` from a vector `b` with weight `c`.
 */
const repelPoint = (c: VarAD, a: VarAD[], b: VarAD[]) =>
  div(c, add(ops.vdistsq(a, b), constOf(EPS_DENOM)));

// ------- Polygon-related helpers

/**
 * Return true iff `p` is in rect `b`.
 */
const pointInBox = (p: any, rect: BBox.BBox): VarAD => {
  return and(
    and(lt(BBox.minX(rect), p.x), lt(p.x, BBox.maxX(rect))),
    and(lt(BBox.minY(rect), p.y), lt(p.y, BBox.maxY(rect)))
  );
};

/**
 * Helper function for atDist constraint.
 * If the point is outside the box, try to get the distance from the point to equal the desired distance.
 */
const atDistOutside = (pt: any, rect: BBox.BBox, offset: VarAD): VarAD => {
  const dsqRes = dsqBP(pt, rect);
  const WEIGHT = 1;
  return mul(constOf(WEIGHT), equalHard(dsqRes, squared(offset)));
};

/**
 * Assuming `rect` is an axis-aligned bounding box (AABB),
 * compute the positive distance squared from point `p` to box `rect` (not the signed distance).
 * https://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
 */
const dsqBP = (p: any, rect: BBox.BBox): VarAD => {
  const dx = max(
    max(sub(BBox.minX(rect), p.x), constOf(0.0)),
    sub(p.x, BBox.maxX(rect))
  );
  const dy = max(
    max(sub(BBox.minY(rect), p.y), constOf(0.0)),
    sub(p.y, BBox.maxY(rect))
  );
  return add(squared(dx), squared(dy));
};

/**
 * Linearly interpolate between left `l` and right `r` endpoints, at fraction `k` of interpolation.
 */
const lerp = (l: VarAD, r: VarAD, k: VarAD): VarAD => {
  // TODO: Rewrite the lerp code to be more concise
  return add(mul(l, sub(constOf(1.0), k)), mul(r, k));
};

/**
 * Linearly interpolate between vector `l` and vector `r` endpoints, at fraction `k` of interpolation.
 */
const lerp2 = (l: VarAD[], r: VarAD[], k: VarAD): [VarAD, VarAD] => {
  return [lerp(l[0], r[0], k), lerp(l[1], r[1], k)];
};

/**
 * Sample a line `line` at `NUM_SAMPLES` points uniformly.
 */
const sampleSeg = (line: VarAD[][]) => {
  const NUM_SAMPLES = 15;
  const NUM_SAMPLES2 = constOf(1 + NUM_SAMPLES);
  // TODO: Check that this covers the whole line, i.e. no off-by-one error
  const samples = _.range(1 + NUM_SAMPLES).map((i) => {
    const k = div(constOf(i), NUM_SAMPLES2);
    return lerp2(line[0], line[1], k);
  });

  return samples;
};

/**
 * Return the closest point on segment `[start, end]` to point `pt`.
 */
const closestPt_PtSeg = (pt: VarAD[], [start, end]: VarAD[][]): VarAD[] => {
  const EPS0 = varOf(10e-3);
  const lensq = max(ops.vdistsq(start, end), EPS0); // Avoid a divide-by-0 if the line is too small

  // If line seg looks like a point, the calculation just returns (something close to) `v`
  const dir = ops.vsub(end, start);
  // t = ((p -: v) `dotv` dir) / lensq -- project vector onto line seg and normalize
  const t = div(
    ops.vdot(ops.vsub(pt, start), dir),
    add(lensq, constOf(EPS_DENOM))
  );
  const t1 = clamp([0.0, 1.0], t);

  // v +: (t' *: dir) -- walk along vector of line seg
  return ops.vadd(start, ops.vmul(t1, dir));
};

/**
 * Clamp `x` in range `[l, r]`.
 */
const clamp = ([l, r]: number[], x: VarAD): VarAD => {
  return max(constOf(l), min(constOf(r), x));
};

/**
 * returns true iff the line from `(a,b)` -> `(c,d)` intersects with `(p,q)` -> `(r,s)`
 */
// https://stackoverflow.com/questions/9043805/test-if-two-lines-intersect-javascript-function
// TODO: Check this function more thoroughly (seems to work fine in Style, though)
export const intersectsSegSeg = (s1: VarAD[][], s2: VarAD[][]): VarAD => {
  return intersects(s1[0], s1[1], s2[0], s2[1]);
};

/**
 * returns true iff the line from `(a,b)` -> `(c,d)` intersects with `(p,q)` -> `(r,s)`
 */
// https://stackoverflow.com/questions/9043805/test-if-two-lines-intersect-javascript-function
// TODO: Check this function more thoroughly (seems to work fine in Style, though)
const intersects = (
  l1_p1: VarAD[],
  l1_p2: VarAD[],
  l2_p1: VarAD[],
  l2_p2: VarAD[]
): VarAD => {
  const [[a, b], [c, d]] = [l1_p1, l1_p2];
  const [[p, q], [r, s]] = [l2_p1, l2_p2];

  // const det = (c - a) * (s - q) - (r - p) * (d - b);
  const det = sub(mul(sub(c, a), sub(s, q)), mul(sub(r, p), sub(d, b)));
  // const lambda = ((s - q) * (r - a) + (p - r) * (s - b)) / det;
  // const gamma = ((b - d) * (r - a) + (c - a) * (s - b)) / det;
  const lambda = div(
    add(mul(sub(s, q), sub(r, a)), mul(sub(p, r), sub(s, b))),
    det
  );
  const gamma = div(
    add(mul(sub(b, d), sub(r, a)), mul(sub(c, a), sub(s, b))),
    det
  );
  const o = constOf(0);
  const l = constOf(1);
  const fals = constOf(0);

  // if (det === 0) { return false; }
  // return (0 < lambda && lambda < 1) && (0 < gamma && gamma < 1);
  return ifCond(
    eq(det, o),
    fals,
    and(and(lt(o, lambda), lt(lambda, l)), and(lt(o, gamma), lt(gamma, l)))
  );
};

/**
 * returns the intersection point between line segments, `s1` from `(a,b)` -> `(c,d)` and `s2` from `(p,q)` -> `(r,s)`. NOTE: Expects the line segments to intersect. You should use `intersect` to check if they do intersect.
 */
// https://stackoverflow.com/questions/9043805/test-if-two-lines-intersect-javascript-function
// https://stackoverflow.com/posts/58657254/revisions
export const intersectionSegSeg = (s1: VarAD[][], s2: VarAD[][]): VarAD[] => {
  const [l1_p1, l1_p2, l2_p1, l2_p2] = [s1[0], s1[1], s2[0], s2[1]];
  const [[a, b], [c, d]] = [l1_p1, l1_p2];
  const [[p, q], [r, s]] = [l2_p1, l2_p2];

  // Divide by 0 error...

  const det = sub(mul(sub(c, a), sub(s, q)), mul(sub(r, p), sub(d, b)));
  const lambda = div(
    add(mul(sub(s, q), sub(r, a)), mul(sub(p, r), sub(s, b))),
    det
  );
  const gamma = div(
    add(mul(sub(b, d), sub(r, a)), mul(sub(c, a), sub(s, b))),
    det
  );

  // from1 + lambda * dv
  const dv = ops.vsub(l1_p2, l1_p1);
  return ops.vmove(l1_p1, lambda, dv);
};

/**
 * Return the amount of overlap between two intervals in R. (0 if none)
 */
export const overlap1D = (
  [l1, r1]: [VarAD, VarAD],
  [l2, r2]: [VarAD, VarAD]
): VarAD => {
  const d = (x: VarAD, y: VarAD) => absVal(sub(x, y)); // Distance between two reals
  // const d = (x: VarAD, y: VarAD) => squared(sub(x, y)); // Distance squared, if just the asymptotic behavior matters
  return ifCond(
    or(lt(r1, l2), lt(r2, l1)), // disjoint intervals => overlap is 0
    constOf(0),
    min(d(l2, r1), d(l1, r2))
  );
};

/**
 * Return numerically-encoded boolean indicating whether `x \in [l, r]`.
 */
export const inRange = (x: VarAD, l: VarAD, r: VarAD): VarAD => {
  if (l.val > r.val) throw Error("invalid range"); // TODO do this range check better
  const fals = constOf(0);
  const tru = constOf(1);
  return ifCond(and(gt(x, l), lt(x, r)), tru, fals);
};

/**
 * Return numerically-encoded boolean indicating whether the two bboxes are disjoint.
 */
export const areDisjointBoxes = (a: BBox.BBox, b: BBox.BBox): VarAD => {
  const fals = constOf(0);
  const tru = constOf(1);

  const c1 = lt(BBox.maxX(a), BBox.minX(b));
  const c2 = gt(BBox.minX(a), BBox.maxX(b));
  const c3 = lt(BBox.maxY(a), BBox.minY(b));
  const c4 = gt(BBox.minY(a), BBox.maxY(b));

  return ifCond(or(or(or(c1, c2), c3), c4), tru, fals);
};

/**
 * Preconditions:
 *   (shape-specific)
 * Input: A shape.
 * Output: A new BBox
 */
export const bboxFromShape = (t: string, s: any): BBox.BBox => {
  return findDef(t).bbox(s);
};

// ------- Minkowski SDF helpers

/**
 * Compute coordinates of Minkowski sum of AABBs representing the first rectangle `box1` and the negative of the second rectangle `box2`.
 * Note: This is not the Minkowski difference in the classical sense, rather just a Minkowski sum of A and -B.
 */
const rectangleDifference = (box1: BBox.BBox, box2: BBox.BBox): VarAD[][] => {
  // Prepare coordinates
  const [xa1, xa2, ya1, ya2] = [
    BBox.minX(box1),
    BBox.maxX(box1),
    BBox.minY(box1),
    BBox.maxY(box1),
  ];
  const [xb1, xb2, yb1, yb2] = [
    BBox.minX(box2),
    BBox.maxX(box2),
    BBox.minY(box2),
    BBox.maxY(box2),
  ];
  // Compute coordinates of the new rectangle
  const xs = [sub(xa1, xb1), sub(xa2, xb2), sub(xa1, xb2), sub(xa2, xb1)];
  const ys = [sub(ya1, yb1), sub(ya2, yb2), sub(ya1, yb2), sub(ya2, yb1)];
  // Return corners
  return [
    [minN(xs), minN(ys)],
    [maxN(xs), maxN(ys)],
  ];
};

/**
 * Return -1.0 for negative number, +1.0 otherwise.
 */
const signOf = (x: VarAD): VarAD => {
  const negative = lt(x, constOf(0.0));
  return ifCond(negative, constOf(-1.0), constOf(1.0));
};

/**
 * Return outward unit normal vector to `lineSegment` with respect to `insidePoint`.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 */
const outwardUnitNormal = (
  lineSegment: VarAD[][],
  insidePoint: VarAD[]
): VarAD[] => {
  const normal = ops.vnormalize(
    ops.rot90(ops.vsub(lineSegment[1], lineSegment[0]))
  );
  const insideValue = ops.vdot(ops.vsub(insidePoint, lineSegment[0]), normal);
  return ops.vmul(neg(signOf(insideValue)), normal);
};

/**
 * Return value of the Signed Distance Function (SFD) of a half-plane evaluated at the origin.
 * @param lineSegment Two points defining a side of the first polygon.
 * @param otherPoints All vertices of the second polygon.
 * @param insidePoint Point inside of the half-plane.
 */
const halfPlaneSDF = (
  lineSegment: VarAD[][],
  otherPoints: VarAD[][],
  insidePoint: VarAD[]
): VarAD => {
  const normal = outwardUnitNormal(lineSegment, insidePoint);
  const alpha = ops.vdot(normal, lineSegment[0]);
  const alphaOther = maxN(otherPoints.map((p) => ops.vdot(normal, p)));
  return neg(add(alpha, alphaOther));
};

/**
 * Return value of one-sided Signed Distance Function (SDF) of the Minkowski sum of two polygons `p1` and `p2` evaluated at the origin.
 * Only half-planes related to sides of the first polygon `p1` are considered.
 * @param p1 Sequence of points defining the first polygon.
 * @param p2 Sequence of points defining the second polygon.
 */
const convexPolygonMinkowskiSDFOneSided = (
  p1: VarAD[][],
  p2: VarAD[][]
): VarAD => {
  const center = ops.vdiv(p1.reduce(ops.vadd), varOf(p1.length));
  const sides = Array.from({ length: p1.length }, (_, i) => i).map((i) => [
    p1[i],
    p1[i > 0 ? i - 1 : p1.length - 1],
  ]);
  const sdfs = sides.map((s: VarAD[][]) => halfPlaneSDF(s, p2, center));
  return maxN(sdfs);
};

/**
 * Return value of the Signed Distance Function (SDF) of the Minkowski sum of two polygons `p1` and `p2` evaluated at the origin.
 * @param p1 Sequence of points defining the first polygon.
 * @param p2 Sequence of points defining the second polygon.
 */
const convexPolygonMinkowskiSDF = (p1: VarAD[][], p2: VarAD[][]): VarAD => {
  return max(
    convexPolygonMinkowskiSDFOneSided(p1, p2),
    convexPolygonMinkowskiSDFOneSided(p2, p1)
  );
};

/**
 * Returns list of convex polygons comprising the original polygon.
 * @param p Sequence of points defining a polygon.
 */
const convexPartitions = (p: VarAD[][]): VarAD[][][] => {
  // TODO: Add convex partitioning algorithm for polygons.
  // See e.g.: https://doc.cgal.org/Manual/3.2/doc_html/cgal_manual/Partition_2/Chapter_main.html#Section_9.3
  return [p];
};
