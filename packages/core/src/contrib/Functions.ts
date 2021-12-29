import * as _ from "lodash";
import { bboxFromShape, inRange } from "contrib/Constraints"; // TODO move this into graphics utils?
import {
  absVal,
  add,
  addN,
  constOf,
  cos,
  acos,
  div,
  gt,
  ifCond,
  max,
  min,
  mul,
  neg,
  numOf,
  ops,
  sin,
  sqrt,
  sub,
  variableAD,
  varOf,
} from "engine/Autodiff";
import * as BBox from "engine/BBox";
import { maxBy, range } from "lodash";
import { PathBuilder } from "renderer/PathBuilder";
import { IVarAD, OptDebugInfo, Pt2, VarAD, VecAD } from "types/ad";
import {
  ArgVal,
  Color,
  IColorV,
  IFloatV,
  IPathDataV,
  IPtListV,
  IStrV,
  ITupV,
  IVectorV,
} from "types/value";
import { getStart, linePts } from "utils/OtherUtils";
import { randFloat } from "utils/Util";
import { isRectlike } from "renderer/ShapeDef";

/**
 * Static dictionary of computation functions
 * TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */

// NOTE: These all need to be written in terms of autodiff types
// These all return a Value<VarAD>
export const compDict = {
  // TODO: Refactor derivative + derivativePre to be inlined as one case in evaluator

  makePath: (
    start: [IVarAD, IVarAD],
    end: [IVarAD, IVarAD],
    curveHeight: IVarAD,
    padding: IVarAD
  ): IPathDataV<IVarAD> => {
    // Two vectors for moving from `start` to the control point: `unit` is the direction of vector [start, end] (along the line passing through both labels) and `normalVec` is perpendicular to `unit` through the `rot90` operation.
    const unit: IVarAD[] = ops.vnormalize(ops.vsub(start, end));
    const normalVec: IVarAD[] = rot90(toPt(unit));
    // There's only one control point in a quadratic bezier curve, and we want it to be equidistant to both `start` and `end`
    const halfLen: IVarAD = div(ops.vdist(start, end), constOf(2));
    const controlPt: IVarAD[] = ops.vmove(
      ops.vmove(end, halfLen, unit),
      curveHeight,
      normalVec
    );
    const curveEnd: IVarAD[] = ops.vmove(end, padding, unit);
    // Both the start and end points of the curve should be padded by some distance such that they don't overlap with the texts
    const path = new PathBuilder();
    return path
      .moveTo(toPt(ops.vmove(start, padding, ops.vneg(unit))))
      .quadraticCurveTo(toPt(controlPt), toPt(curveEnd))
      .getPath();
  },

  /**
   * Return the derivative of `varName`.
   * NOTE: This is a special system function. Don't change it!
   */
  derivative: (optDebugInfo: OptDebugInfo, varName: string): IFloatV<any> => {
    if (
      !optDebugInfo ||
      !("gradient" in optDebugInfo) ||
      !optDebugInfo.gradient.size
    ) {
      console.log("optDebugInfo", optDebugInfo);
      console.error(`no derivative found for '${varName}'; returning 0`);
      return {
        tag: "FloatV",
        contents: constOf(0.0),
      };
    }

    if (optDebugInfo.gradient.has(varName)) {
      return {
        tag: "FloatV",
        // TODO: Improve error if varName is not in map
        contents: constOf(optDebugInfo.gradient.get(varName) as number),
      };
    }

    console.error(
      `variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`
    );
    return {
      tag: "FloatV",
      contents: constOf(0.0),
    };
  },

  /**
   * Return the L-BFGS preconditioned derivative of `varName`.
   * NOTE: This is a special system function. Don't change it!
   */
  derivativePreconditioned: (
    optDebugInfo: OptDebugInfo,
    varName: string
  ): IFloatV<any> => {
    if (
      !optDebugInfo ||
      !("gradientPreconditioned" in optDebugInfo) ||
      !optDebugInfo.gradientPreconditioned.size
    ) {
      console.log("optDebugInfo", optDebugInfo);
      console.error(`no derivative found for '${varName}'; returning 0`);
      return {
        tag: "FloatV",
        contents: constOf(0.0),
      };
    }

    if (optDebugInfo.gradientPreconditioned.has(varName)) {
      return {
        tag: "FloatV",
        // TODO: Improve error if varName is not in map
        contents: constOf(
          optDebugInfo.gradientPreconditioned.get(varName) as number
        ),
      };
    }

    console.error(
      `variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`
    );
    return {
      tag: "FloatV",
      contents: constOf(0.0),
    };
  },

  /**
   * Return `i`th element of list `xs, assuming lists only hold floats.
   */
  get: (xs: VarAD[], i: number): IFloatV<any> => {
    const res = xs[i];
    return {
      tag: "FloatV",
      contents: res,
    };
  },

  getVar: (xs: VarAD[], i: VarAD): IFloatV<any> => {
    const res = xs[i.val];
    return {
      tag: "FloatV",
      contents: res,
    };
  },

  /**
   * Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).
   */
  rgba: (r: VarAD, g: VarAD, b: VarAD, a: VarAD): IColorV<VarAD> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "RGBA",
        contents: [r, g, b, a],
      },
    };
  },

  selectColor: (
    color1: Color<VarAD>,
    color2: Color<VarAD>,
    level: IVarAD
  ): IColorV<VarAD> => {
    if (level.val % 2 == 0)
      return {
        tag: "ColorV",
        contents: color1,
      };
    return {
      tag: "ColorV",
      contents: color2,
    };
  },

  /**
   * Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).
   */
  hsva: (h: VarAD, s: VarAD, v: VarAD, a: VarAD): IColorV<VarAD> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "HSVA",
        contents: [h, s, v, a],
      },
    };
  },

  /**
   * Return a paint of none (no paint)
   */
  none: (): IColorV<any> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "NONE",
      },
    };
  },

  /**
   * Return the cosine of input `rad` (in radians).
   */
  cos: (rad: VarAD): IFloatV<VarAD> => {
    // Accepts radians
    return {
      tag: "FloatV",
      contents: cos(rad),
    };
  },

  /**
   * Return the sine of input `rad` (in radians).
   */
  sin: (rad: VarAD): IFloatV<VarAD> => {
    // Accepts radians
    return {
      tag: "FloatV",
      contents: sin(rad),
    };
  },

  /**
   * Return the dot product of `v` and `w`.
   */
  dot: (v: VarAD[], w: VarAD[]): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: ops.vdot(v, w),
    };
  },

  /**
   * Return the length of the line or arrow shape `[type, props]`.
   */
  lineLength: ([type, props]: [string, any]): IFloatV<VarAD> => {
    const [p1, p2] = linePts(props);
    return {
      tag: "FloatV",
      contents: ops.vdist(p1, p2),
    };
  },

  /**
   * Return the length of the line or arrow shape `[type, props]`.
   */
  len: ([type, props]: [string, any]): IFloatV<VarAD> => {
    const [p1, p2] = linePts(props);
    return {
      tag: "FloatV",
      contents: ops.vdist(p1, p2),
    };
  },

  /**
   * Concatenate a list of strings
   */
  concat: (...strings: string[]): IStrV => {
    return {
      tag: "StrV",
      contents: strings.join(""),
    };
  },

  /**
   * Return the normalized version of vector `v`.
   */
  normalize: (v: VarAD[]): IVectorV<VarAD> => {
    return {
      tag: "VectorV",
      contents: ops.vnormalize(v),
    };
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  pathFromPoints: (pathType: string, pts: Pt2[]): IPathDataV<IVarAD> => {
    const path = new PathBuilder();
    const [start, ...tailpts] = pts;
    path.moveTo(start);
    tailpts.map((pt: Pt2) => path.lineTo(pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  quadraticCurveFromPoints: (
    pathType: string,
    pts: Pt2[]
  ): IPathDataV<IVarAD> => {
    const path = new PathBuilder();
    const [start, cp, second, ...tailpts] = pts;
    path.moveTo(start);
    path.quadraticCurveTo(cp, second);
    tailpts.map((pt: Pt2) => path.quadraticCurveJoin(pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  cubicCurveFromPoints: (pathType: string, pts: Pt2[]): IPathDataV<IVarAD> => {
    const path = new PathBuilder();
    const [start, cp1, cp2, second, ...tailpts] = pts;
    path.moveTo(start);
    path.bezierCurveTo(cp1, cp2, second);
    _.chunk(tailpts, 2).map(([cp, pt]) => path.cubicCurveJoin(cp, pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },

  /**
   * Return two points parallel to line `s1` using its normal line `s2`.
   */
  unitMark: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    t: string,
    padding: VarAD,
    barSize: VarAD
  ): IPtListV<VarAD> => {
    const [start1, end1] = linePts(s1);
    const [start2, end2] = linePts(s2);

    const dir = ops.vnormalize(ops.vsub(end2, start2));
    const normalDir = ops.vneg(dir);
    const markStart = ops.vmove(start1, padding, normalDir);
    const markEnd = ops.vmove(end1, padding, normalDir);

    return {
      tag: "PtListV",
      contents: [markStart, markEnd].map(toPt),
    };
  },

  /**
   * Return two points to "cap off" the line made in `unitMark`.
   */
  unitMark2: (
    [start, end]: [Pt2, Pt2],
    t: string,
    padding: VarAD,
    size: VarAD
  ): IPtListV<VarAD> => {
    const dir = ops.vnormalize(ops.vsub(end, start));
    const normalDir = rot90(toPt(dir));
    const base = t === "start" ? start : end;
    const [markStart, markEnd] = [
      ops.vmove(base, size, normalDir),
      ops.vmove(base, neg(size), normalDir),
    ];
    return {
      tag: "PtListV",
      contents: [markStart, markEnd].map(toPt),
    };
  },

  /**
   * Return series of elements that can render an arc SVG. See: https://css-tricks.com/svg-path-syntax-illustrated-guide/ for the "A" spec.
   * @param pathType: either "open" or "closed." whether the SVG should automatically draw a line between the final point and the start point
   * @param start: coordinate to start drawing the arc
   * @param end: coordinate to finish drawing the arc
   * @param radius: width and height of the ellipse to draw the arc along (i.e. [width, height])
   * @param rotation: angle in degrees to rotate ellipse about its center
   * @param largeArc: 0 to draw shorter of 2 arcs, 1 to draw longer
   * @param arcSweep: 0 to rotate CCW, 1 to rotate CW
   * @returns: Elements that can be passed to Path shape spec to render an SVG arc
   */
  arc: (
    pathType: string,
    start: Pt2,
    end: Pt2,
    radius: Pt2,
    rotation: IVarAD,
    largeArc: IVarAD,
    arcSweep: IVarAD
  ): IPathDataV<IVarAD> => {
    const path = new PathBuilder();
    path.moveTo(start).arcTo(radius, end, [rotation, largeArc, arcSweep]);
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },
  /**
   * Find the point that is located at dist r along a line between p1 and p2.
   * @param p1: start point of line segment
   * @param p2: endpoint of line segment
   * @param r: distance from p1 to travel along the line
   * @returns: vector representation of the point of intersection
   */
  ptOnLine: (p1: VarAD[], p2: VarAD[], r: VarAD): IVectorV<VarAD> => {
    // find unit vector pointing towards v2
    const unit = ops.vnormalize(ops.vsub(p2, p1));
    return { tag: "VectorV", contents: ops.vmove(p1, r, unit) };
  },
  /**
   * Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.
   * @param x1, y1: x, y coordinates of the circle/ellipse that the arc is drawn on
   * @param start: start point of the arc
   * @param end: end point of the arc
   * @returns: 0 or 1 depending on CCW or CW rotation
   */
  arcSweepFlag: ([x1, y1]: VarAD[], start: Pt2, end: Pt2): IFloatV<VarAD> => {
    const st = ops.vnormalize([sub(start[0], x1), sub(start[1], y1)]);
    const en = ops.vnormalize([sub(end[0], x1), sub(end[1], y1)]);
    const cross = ops.cross2(st, en);
    return {
      tag: "FloatV",
      contents: ifCond(gt(cross, varOf(0)), varOf(0), varOf(1)),
    };
  },
  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: (u: VarAD[], v: VarAD[]): IFloatV<VarAD> => {
    const theta = ops.angleBetween(u,v);
    return {
      tag: "FloatV",
      contents: theta,
    };
  },
  /**
   * Return the signed angle from vector `u` to vector `v`, in radians.
   * Assumes that both u and v are 2D vectors and have nonzero magnitude.
   * The returned value will be in the range [-pi,pi].
   */
  angleFrom: (u: VarAD[], v: VarAD[]): IFloatV<VarAD> => {
    const theta = ops.angleFrom(u,v);
    return {
      tag: "FloatV",
      contents: theta,
    };
  },
  /**
   * Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]
   */
  cross2D: (u: VarAD[], v: VarAD[]): IFloatV<VarAD> => {
    const det = sub( mul(u[0],v[1]), mul(u[1],v[0]) );
    return {
      tag: "FloatV",
      contents: det,
    };
  },
  /**
   * Return a point located at the midpoint between pts `start` and `end`
   */
  midpoint: (start: VarAD[], end: VarAD[]): IVectorV<VarAD> => {
    const midpointLoc = ops.vmul(constOf(0.5), ops.vadd(start, end));
    return {
      tag: "VectorV",
      contents: toPt(midpointLoc),
    };
  },
  /**
   * Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).
   */
  midpointOffset: ([t1, s1]: [string, any], padding: VarAD): ITupV<VarAD> => {
    if (t1 === "Arrow" || t1 === "Line") {
      const [start, end] = linePts(s1);
      // TODO: Cache these operations in Style!
      const normalDir = rot90v(ops.vnormalize(ops.vsub(end, start)));
      const midpointLoc = ops.vmul(constOf(0.5), ops.vadd(start, end));
      const midpointOffsetLoc = ops.vmove(midpointLoc, padding, normalDir);
      return {
        tag: "TupV",
        contents: toPt(midpointOffsetLoc),
      };
    } else {
      throw Error(`unsupported shape ${t1} in midpointOffset`);
    }
  },
  chevron: (
    // TODO reimplement with variable tick marks when #629 is merged
    [t1, s1]: [string, any],
    padding: VarAD,
    ticks: VarAD
  ): IPtListV<VarAD> => {
    if (t1 === "Arrow" || t1 === "Line") {
      // tickPlacement(padding, ticks);
      const [start, end] = linePts(s1);
      const dir = ops.vnormalize(ops.vsub(end, start)); // TODO make direction face "positive direction"
      const startDir = ops.vrot(dir, varOf(135));
      const endDir = ops.vrot(dir, varOf(225));
      const center = ops.vmul(constOf(0.5), ops.vadd(start, end));
      // if even, evenly divide tick marks about center. if odd, start in center and move outwards
      return {
        tag: "PtListV",
        contents: [
          ops.vmove(center, padding, startDir),
          center,
          ops.vmove(center, padding, endDir),
        ].map(toPt),
      };
    } else {
      throw Error(`unsupported shape ${t1} in chevron`);
    }
  },
  /**
   * Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).
   */
  innerPointOffset: (
    pt1: VarAD[],
    pt2: VarAD[],
    pt3: VarAD[],
    padding: VarAD
  ): IVectorV<VarAD> => {
    // unit vector towards first corner
    const vec1unit = ops.vnormalize(ops.vsub(pt2, pt1));
    const normalDir = ops.vneg(rot90v(vec1unit)); // rot90 rotates CW, neg to point in CCW direction

    // move along line between p1 and p2, then move perpendicularly
    const ref = ops.vmove(pt1, padding, vec1unit);
    const [xp, yp] = ops.vmove(ref, padding, normalDir);
    const [xn, yn] = ops.vmove(ref, padding, ops.vneg(normalDir));

    // unit vector towards end point
    const vec2unit = ops.vnormalize(ops.vsub(pt3, pt1));
    const endpt = ops.vmove(pt1, padding, vec2unit);

    // unit vector from midpoint to end point
    const intoEndUnit = ops.vnormalize(ops.vsub([xp, yp], endpt));
    // vector from B->E needs to be parallel to original vector, only care about positive 1 case bc intoEndUnit should point the same direction as vec1unit
    const cond = gt(ops.vdot(vec1unit, intoEndUnit), varOf(0.95));
    return {
      tag: "VectorV",
      contents: [ifCond(cond, xp, xn), ifCond(cond, yp, yn)],
    };
  },
  /**
   * Create equally spaced tick marks centered at the midpoint of a line
   * @param pt1: starting point of a line
   * @param pt2: endping point of a line
   * @param spacing: space in px between each tick
   * @param numTicks: number of tick marks to create
   * @param tickLength: 1/2 length of each tick
   */
  ticksOnLine: (
    pt1: VarAD[],
    pt2: VarAD[],
    spacing: VarAD,
    numTicks: VarAD,
    tickLength: VarAD
  ) => {
    const path = new PathBuilder();
    // calculate scalar multipliers to determine the placement of each tick mark
    const multipliers = tickPlacement(spacing, numTicks);
    const unit = ops.vnormalize(ops.vsub(pt2, pt1));
    const normalDir = ops.vneg(rot90v(unit)); // rot90 rotates CW, neg to point in CCW direction

    const mid = ops.vmul(constOf(0.5), ops.vadd(pt1, pt2));

    // start/end pts of each tick will be placed parallel to each other, offset at dist of tickLength
    // from the original pt1->pt2 line
    const [x1p, y1p] = ops.vmove(mid, tickLength, normalDir);
    const [x2p, y2p] = ops.vmove(mid, tickLength, ops.vneg(normalDir));

    multipliers.map((multiplier) => {
      const [sx, sy] = ops.vmove([x1p, y1p], multiplier, unit);
      const [ex, ey] = ops.vmove([x2p, y2p], multiplier, unit);
      path.moveTo([sx, sy]).lineTo([ex, ey]);
    });
    return path.getPath();
  },
  /**
   * Given two orthogonal segments that intersect at `intersection`, and a size `len`
   * return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.
   */
  orientedSquare: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    intersection: Pt2,
    len: VarAD
  ): IPathDataV<IVarAD> => {
    if (
      (t1 === "Arrow" || t1 === "Line") &&
      (t2 === "Arrow" || t2 === "Line")
    ) {
      const [seg1, seg2]: any = [linePts(s1), linePts(s2)];
      const [ptL, ptLR, ptR] = perpPathFlat(len, seg1, seg2);
      const path = new PathBuilder();
      return path
        .moveTo(toPt(ptL))
        .lineTo(toPt(ptLR))
        .lineTo(toPt(ptR))
        .lineTo(intersection)
        .closePath()
        .getPath();
    } else {
      throw Error("orientedSquare undefined for types ${t1}, ${t2}");
    }
  },

  /**
           * Figure out which side of the rectangle `[t1, s1]` the `start->end` line is hitting, assuming that `start` is located at the rect's center and `end` is located outside the rectangle, and return the size of the OTHER side. Also assuming axis-aligned rectangle. This is used for arrow placement in box-and-arrow diagrams.

       @deprecated Don't use this function, it does not fully work
           */
  intersectingSideSize: (
    start: VecAD,
    end: VecAD,
    [t1, s1]: [string, any]
  ): IFloatV<VarAD> => {
    // if (s1.rotation.contents) { throw Error("assumed AABB"); }
    if (!isRectlike(t1)) {
      throw Error("expected rect-like shape");
    }

    // TODO: Deal with start and end disjoint from rect, or start and end subset of rect
    const rect = bboxFromShape(t1, s1);

    // Intersects top or bottom => return w
    // i.e. endX \in [minX, maxX] -- if not this, the other must be true

    // Intersects right or left => return h
    // i.e. endY \in [minY, maxY]

    // Return the OTHER side, which is needed for arrow placement

    // TODO <
    // this function is wrong -- the `end` doesn't have to lie in any range, and the start always does
    // Find some other way to calculate what side intersects the ray between the points
    // Check if this works better WRT new disjoint rectangles, rect-line etc.

    const dim = ifCond(
      inRange(end[0], BBox.minX(rect), BBox.maxX(rect)),
      rect.h,
      rect.w
    );
    return { tag: "FloatV", contents: dim };
  },

  /**
   * Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).
   */
  triangle: (
    [t1, l1]: any,
    [t2, l2]: any,
    [t3, l3]: any
  ): IPathDataV<IVarAD> => {
    if (t1 === "Line" && t2 === "Line" && t3 === "Line") {
      const path = new PathBuilder();
      return path
        .moveTo(toPt(getStart(l1)))
        .lineTo(toPt(getStart(l2)))
        .lineTo(toPt(getStart(l3)))
        .closePath()
        .getPath();
    } else {
      console.error([t1, l1], [t2, l2], [t3, l3]);
      throw Error("Triangle function expected three lines");
    }
  },

  /**
   * Return the average of floats `x` and `y`.
   */
  average2: (x: VarAD, y: VarAD): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: div(add(x, y), constOf(2.0)),
    };
  },

  /**
   * Return the average of the floats in the list `xs`.
   */
  average: (xs: VarAD[]): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: div(addN(xs), max(constOf(1.0), constOf(xs.length))),
      // To avoid divide-by-0
    };
  },

  /**
   * Return the normalized version of vector `v`.
   */
  unit: (v: VarAD[]): IVectorV<VarAD> => {
    return {
      tag: "VectorV",
      contents: ops.vnormalize(v),
    };
  },

  /**
   * Sample a random color once, with opacity `alpha` and colorType `colorType` (`"rgb"` or `"hsv"`).
   */
  sampleColor: (alpha: VarAD, colorType: string): IColorV<VarAD> => {
    checkFloat(alpha);

    if (colorType === "rgb") {
      const rgb = range(3).map((_) => constOf(randFloat(0.1, 0.9)));

      return {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [rgb[0], rgb[1], rgb[2], alpha],
        },
      };
    } else if (colorType === "hsv") {
      const h = randFloat(0, 360);
      return {
        tag: "ColorV",
        contents: {
          tag: "HSVA",
          contents: [constOf(h), constOf(100), constOf(80), alpha], // HACK: for the color to look good
        },
      };
    } else throw new Error("unknown color type");
  },

  /**
   * Set the opacity of a color `color` to `frac`.
   */
  setOpacity: (color: Color<VarAD>, frac: VarAD): IColorV<VarAD> => {
    // If paint=none, opacity is irreelevant
    if (color.tag === "NONE") {
      return {
        tag: "ColorV",
        contents: color,
      };
      // Otherwise, retain tag and color; only modify opacity
    } else {
      const props = color.contents;
      return {
        tag: "ColorV",
        contents: {
          tag: color.tag,
          contents: [props[0], props[1], props[2], mul(frac, props[3])],
        },
      };
    }
  },

  /**
   * Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).
   */
  mul: (m: VarAD[][], v: VarAD[]): IVectorV<VarAD> => {
    if (!m.length) {
      throw Error("empty matrix");
    }
    if (!v.length) {
      throw Error("empty vector");
    }

    return {
      tag: "VectorV",
      contents: m.map((row) => ops.vdot(row, v)),
    };
  },

  // ------ Utility functions

  /**
   * Return the square root of the number `x`. (NOTE: if `x < 0`, you may get `NaN`s)
   */
  sqrt: (x: VarAD): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: sqrt(x) };
  },

  /**
   * Return the max of the numbers `x`, `y`.
   */
  max: (x: VarAD, y: VarAD): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: max(x, y) };
  },

  /**
   * Return the min of the numbers `x`, `y`.
   */
  min: (x: VarAD, y: VarAD): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: min(x, y) };
  },

  /**
   * Return the absolute value of the number `x`.
   */
  abs: (x: VarAD): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: absVal(x) };
  },

  /**
   * Return the sign of the number `x`.
   */
  sgn: (x: VarAD): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: div(x,absVal(x)) };
  },

  /**
   * Return the Euclidean norm of the vector `v`.
   */
  norm: (v: VarAD[]): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: ops.vnorm(v) };
  },

  /**
   * Return the Euclidean norm squared of the vector `v`.
   */
  normsq: (v: VarAD[]): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: ops.vnormsq(v) };
  },

  /**
   * Return the Euclidean distance between the vectors `v` and `w`.
   */
  vdist: (v: VarAD[], w: VarAD[]): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: ops.vdist(v, w) };
  },

  vmul: (s: VarAD, v: VarAD[]): IVectorV<VarAD> => {
    return { tag: "VectorV", contents: ops.vmul(s, v) };
  },

  /**
   * Return the Euclidean distance squared between the vectors `v` and `w`.
   */
  vdistsq: (v: VarAD[], w: VarAD[]): IFloatV<VarAD> => {
    return { tag: "FloatV", contents: ops.vdistsq(v, w) };
  },

  // ------ Geometry/graphics utils

  /**
   * Rotate a 2D vector `v` by 90 degrees counterclockwise.
   */
  rot90: (v: VarAD[]) => {
    if (v.length !== 2) {
      throw Error("expected 2D vector in `rot90`");
    }
    const [x, y] = v;
    return { tag: "VectorV", contents: [neg(y), x] };
  },
};

// Ignore this
export const checkComp = (fn: string, args: ArgVal<VarAD>[]) => {
  if (!compDict[fn]) throw new Error(`Computation function "${fn}" not found`);
};

// Make sure all arguments are not numbers (they should be VarADs if floats)
const checkFloat = (x: any) => {
  if (typeof x === "number") {
    throw Error("expected float converted to VarAD; got number (int?)");
  }
};

const toPt = (v: VecAD): Pt2 => {
  if (v.length !== 2) {
    throw Error("expected vector of length 2");
  }
  return [v[0], v[1]];
};

/**
 * Given two perpendicular vectors `[startR, endR]` and `[startL, endL]`, return a path that describes a perpendicular mark between them.
 */
const perpPathFlat = (
  len: VarAD,
  [startR, endR]: [VecAD, VecAD],
  [startL, endL]: [VecAD, VecAD]
): [VecAD, VecAD, VecAD] => {
  // perpPathFlat :: Autofloat a => a -> (Pt2 a, Pt2 a) -> (Pt2 a, Pt2 a) -> (Pt2 a, Pt2 a, Pt2 a)
  // perpPathFlat size (startR, endR) (startL, endL) =
  //   let dirR = normalize' $ endR -: startR
  //       dirL = normalize' $ endL -: startL
  //       ptL = startR +: (size *: dirL)
  //       ptR = startR +: (size *: dirR)
  //       ptLR = startR +: (size *: dirL) +: (size *: dirR)
  //   in (ptL, ptLR, ptR)
  const dirR = ops.vnormalize(ops.vsub(endR, startR));
  const dirL = ops.vnormalize(ops.vsub(endL, startL));
  const ptL = ops.vmove(startR, len, dirL); // ops.vadd(startR, ops.vmul(len, dirL));
  const ptR = ops.vmove(startR, len, dirR); // ops.vadd(startR, ops.vmul(len, dirR));
  const ptLR = ops.vadd(ptL, ops.vmul(len, dirR));
  return [ptL, ptLR, ptR];
};

/**
 * Rotate a 2D point `[x, y]` by 90 degrees counterclockwise.
 */
const rot90 = ([x, y]: Pt2): Pt2 => {
  return [neg(y), x];
};

/**
 * Rotate a 2D point `[x, y]` by 90 degrees clockwise.
 */
const rot90v = ([x, y]: VarAD[]): VarAD[] => {
  return [neg(y), x];
};

/**
 * Returns the point in `candidates` farthest from the points in `pts` (by sum).
 * Note: With the current autodiff system you cannot make discrete choices -- TODO debug why this code doesn't terminate in objective/gradient compilation
 * Do not use this!
 */
const furthestFrom = (pts: VarAD[][], candidates: VarAD[][]): VarAD[] => {
  if (!pts || pts.length === 0) {
    throw Error("Expected nonempty point list");
  }

  const ptDists: [VarAD[], VarAD][] = pts.map((p: VarAD[]) => [
    p,
    ops.vsum(candidates.map((pt) => ops.vdistsq(p, pt))),
  ]);
  const res = maxBy(ptDists, ([p, d]: [VarAD[], VarAD]) => numOf(d));

  if (!res || res.length < 2) {
    throw Error("expected point");
  }

  return res[0] as VarAD[];
};

const tickPlacement = (
  padding: VarAD,
  numPts: VarAD,
  multiplier = varOf(1)
): VarAD[] => {
  if (numOf(numPts) <= 0) throw Error(`number of ticks must be greater than 0`);
  const even = numOf(numPts) % 2 === 0;
  let pts = even ? [div(padding, varOf(2))] : [varOf(0)];
  for (let i = 1; i < numOf(numPts); i++) {
    if (even && i === 1) multiplier = neg(multiplier);
    const shift =
      i % 2 == 0
        ? mul(padding, mul(neg(varOf(i)), multiplier))
        : mul(padding, mul(varOf(i), multiplier));
    pts.push(add(pts[i - 1], shift));
  }
  return pts;
};
