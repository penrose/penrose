import { ops } from "../engine/Autodiff";
import {
  absVal,
  add,
  ifCond,
  lt,
  max,
  maxN,
  min,
  mul,
  neg,
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Line } from "../shapes/Line";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { ConstrFunc } from "../types/functions";
import { real2NT, real2T, realNT, realT, shapeT } from "../utils/Util";
import {
  atDistLabel,
  overlappingCircleEllipse,
  overlappingEllipse,
} from "./ConstraintsUtils";
import { constrDictCurves } from "./CurveConstraints";
import { containsConvexPolygonPoints, convexPartitions } from "./Minkowski";
import { bboxFromShape, bboxPts, shapeDistance, shapeSize } from "./Queries";
import { inRange, isRectlike, overlap1D } from "./Utils";

// -------- Simple constraints
// Do not require shape queries, operate directly with `ad.Num` parameters.
const constrDictSimple = {
  /**
   * Require that the value `x` is equal to the value `y`
   */
  equal: {
    name: "equal",
    description: "Require that the value `x` is equal to the value `y`",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: (x: ad.Num, y: ad.Num) => {
      return absVal(sub(x, y));
    },
  },

  /**
   * Require that the value `x` is less than the value `y` with optional padding `padding`
   */
  lessThan: {
    name: "lessThan",
    description:
      "Require that the value `x` is less than the value `y` with optional padding `padding`",

    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: (x: ad.Num, y: ad.Num, padding = 0) => {
      return add(sub(x, y), padding);
    },
  },

  /**
   * Require that the value `x` is greater than the value `y` with optional padding `padding`
   */
  greaterThan: {
    name: "greaterThan",
    description:
      "Require that the value `x` is greater than the value `y` with optional padding `padding`",

    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: (x: ad.Num, y: ad.Num, padding = 0) => {
      return add(sub(y, x), padding);
    },
  },

  /**
   * Require that the value `x` is less than the value `y`, with steeper penalty
   */
  lessThanSq: {
    name: "lessThanSq",
    description:
      "Require that the value `x` is less than the value `y`, with steeper penalty",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: (x: ad.Num, y: ad.Num) => {
      // if x < y then 0 else (x - y)^2
      return ifCond(lt(x, y), 0, squared(sub(x, y)));
    },
  },

  /**
   * Require that the value `x` is greater than the value `y`, with steeper penalty
   */
  greaterThanSq: {
    name: "greaterThanSq",
    description:
      "Require that the value `x` is greater than the value `y`, with steeper penalty",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: (x: ad.Num, y: ad.Num) => {
      return ifCond(lt(y, x), 0, squared(sub(y, x)));
    },
  },

  /**
   * Require that the value `x` is in the range defined by `[x0, x1]`.
   */
  inRange: {
    name: "inRange",
    description:
      "Require that the value `x` is in the range defined by `[x0, x1]`.",
    params: [
      { name: "x", description: "Value", type: realT() },
      { name: "x0", description: "Lower bound", type: realT() },
      { name: "x1", description: "Upper bound", type: realT() },
    ],
    body: (x: ad.Num, x0: ad.Num, x1: ad.Num) => {
      return add(
        ifCond(lt(x, x1), 0, sub(x, x1)),
        ifCond(lt(x0, x), 0, sub(x0, x))
      );
    },
  },

  /**
   * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
   */
  contains1D: {
    name: "contains1D",
    description:
      "Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.",

    params: [
      {
        name: "[l1, r1]",
        description: "First interval",
        type: real2T(),
      },
      {
        name: "[l2, r2]",
        description: "Second interval",
        type: real2T(),
      },
    ],
    body: ([l1, r1]: [ad.Num, ad.Num], [l2, r2]: [ad.Num, ad.Num]): ad.Num => {
      // [if len2 <= len1,] require that (l2 > l1) & (r2 < r1)
      return add(max(0, sub(l1, l2)), max(0, sub(r2, r1)));
    },
  },

  /**
   * Make scalar `c` disjoint from a range `left, right`.
   */
  disjointScalar: {
    name: "disjointScalar",
    description: "Make scalar `c` disjoint from a range `left, right`.",
    params: [
      { name: "c", description: "Scalar", type: realT() },
      { name: "left", description: "Left bound", type: realT() },
      { name: "right", description: "Right bound", type: realT() },
    ],
    body: (c: ad.Num, left: ad.Num, right: ad.Num) => {
      const d = (x: ad.Num, y: ad.Num) => absVal(sub(x, y));

      // if (x \in [l, r]) then min(d(x,l), d(x,r)) else 0
      return ifCond(inRange(c, left, right), min(d(c, left), d(c, right)), 0);
    },
  },

  /**
   * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
   */
  perpendicular: {
    name: "perpendicular",
    description:
      "Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.",
    params: [
      { name: "q", description: "First point", type: realNT() },
      { name: "p", description: "Second point", type: realNT() },
      { name: "r", description: "Third point", type: realNT() },
    ],
    body: (q: ad.Num[], p: ad.Num[], r: ad.Num[]): ad.Num => {
      const v1 = ops.vsub(q, p);
      const v2 = ops.vsub(r, p);
      const dotProd = ops.vdot(v1, v2);
      return absVal(dotProd);
    },
  },

  /**
   * Require that three points be collinear.
   * Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
   */
  collinear: {
    name: "collinear",
    description: `Require that three points be collinear. This does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.`,
    params: [
      { name: "c1", description: "First point", type: realNT() },
      { name: "c2", description: "Second point", type: realNT() },
      { name: "c3", description: "Third point", type: realNT() },
    ],
    body: (c1: ad.Num[], c2: ad.Num[], c3: ad.Num[]) => {
      const v1 = ops.vsub(c2, c1);
      const v2 = ops.vsub(c2, c3);
      return absVal(ops.cross2(v1, v2));
    },
  },

  /**
   * Require that three points be collinear.
   * Depends on the specific ordering of points.
   */
  collinearOrdered: {
    name: "collinearOrdered",
    description: `Require that three points be collinear and enforces the order of these points as provided.`,

    params: [
      { name: "c1", description: "First point", type: realNT() },
      { name: "c2", description: "Second point", type: realNT() },
      { name: "c3", description: "Third point", type: realNT() },
    ],
    body: (c1: ad.Num[], c2: ad.Num[], c3: ad.Num[]) => {
      const v1 = ops.vnorm(ops.vsub(c1, c2));
      const v2 = ops.vnorm(ops.vsub(c2, c3));
      const v3 = ops.vnorm(ops.vsub(c1, c3));

      // Use triangle inequality (|v1| + |v2| <= |v3|) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
      return max(0, sub(add(v1, v2), v3));
    },
  },
};

// -------- General constraints
// Defined for all shapes, generally require shape queries or call multiple specific constraints.
const constrDictGeneral = {
  /** Require that `shape` is on the canvas */
  onCanvas: {
    name: "onCanvas",
    description: "Require that `shape` is on the canvas",
    params: [
      { name: "shape", description: "Shape", type: shapeT("AnyShape") },
      { name: "canvasWidth", description: "Width of canvas", type: realT() },
      { name: "canvasHeight", description: "Height of canvas", type: realT() },
    ],
    body: (shape: Shape<ad.Num>, canvasWidth: ad.Num, canvasHeight: ad.Num) => {
      const box = bboxFromShape(shape);
      const canvasXRange: [ad.Num, ad.Num] = [
        mul(canvasWidth, -0.5),
        mul(canvasWidth, 0.5),
      ];
      const canvasYRange: [ad.Num, ad.Num] = [
        mul(canvasHeight, -0.5),
        mul(canvasHeight, 0.5),
      ];
      return add(
        constrDict.contains1D.body(canvasXRange, BBox.xRange(box)),
        constrDict.contains1D.body(canvasYRange, BBox.yRange(box))
      );
    },
  },
  /**
   * Require that a shape have a size greater than some constant minimum, based on the type of the shape.
   */
  minSize: {
    name: "minSize",
    description:
      "Require that a shape have a size greater than some constant minimum, based on the type of the shape.",

    params: [
      { name: "shape", description: "Shape", type: shapeT("AnyShape") },
      {
        name: "limit",
        description: "Minimum size",
        type: realT(),
        default: 50,
      },
    ],
    body: (shape: Shape<ad.Num>, limit = 50) => {
      return sub(limit, shapeSize(shape));
    },
  },

  /**
   * Require that a shape have a size less than some constant maximum, based on the type of the shape.
   */
  maxSize: {
    name: "maxSize",
    description:
      "Require that a shape have a size less than some constant maximum, based on the type of the shape.",

    params: [
      { name: "shape", description: "Shape", type: shapeT("AnyShape") },
      { name: "limit", description: "Maximum size", type: realT() },
    ],
    body: (shape: Shape<ad.Num>, limit: ad.Num) => {
      return sub(shapeSize(shape), limit);
    },
  },

  /**
   * Require that shape `s1` overlaps shape `s2` with some overlap `overlap`.
   * based on the type of the shape, and with an optional `overlap` between them
   * (e.g. if `s1` should be overlapping `s2` with margin `overlap`).
   */
  overlapping: {
    name: "overlapping",
    description: `Require that shape \`s1\` overlaps shape \`s2\` with some overlap \`overlap\`.
   based on the type of the shape, and with an optional \`overlap\` between them
   (e.g. if \`s1\` should be overlapping \`s2\` with margin \`overlap\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "overlap", description: "Overlap", type: realT(), default: 0 },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, overlap: ad.Num = 0) => {
      const t1 = s1.shapeType;
      const t2 = s2.shapeType;
      // for some cases with ellipses, we can't easily compute the distance
      if (t1 === "Ellipse" && t2 === "Ellipse")
        return overlappingEllipse(s1, s2, overlap);
      // Circle x Ellipse
      else if (t1 === "Circle" && t2 === "Ellipse")
        return overlappingCircleEllipse(s1, s2, overlap);
      else if (t1 === "Ellipse" && t2 === "Circle")
        return overlappingCircleEllipse(s2, s1, overlap);
      // for other cases, we know how to compute the distance, so we just use that
      else return add(shapeDistance(s1, s2), overlap);
    },
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`,
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
   */
  disjoint: {
    name: "disjoint",
    description: `Require that a shape \`s1\` is disjoint from shape \`s2\`, based on the type of the shape, and with an optional \`padding\` between them (e.g. if \`s1\` should be disjoint from \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, padding: ad.Num = 0) =>
      neg(constrDictGeneral.overlapping.body(s1, s2, neg(padding))),
  },

  /**
   * Require that shape `s1` is touching shape `s2`.
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be touching `s2` with margin `padding`).
   */
  touching: {
    name: "touching",
    description: `Require that shape \`s1\` is touching shape \`s2\` based on the type of the shape, and with an optional \`padding\` between them (e.g. if \`s1\` should be touching \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, padding: ad.Num = 0) =>
      absVal(constrDictGeneral.overlapping.body(s1, s2, neg(padding))),
  },

  /**
   * Require that a shape `s1` contains another shape `s2`,
   * based on the type of the shape, and with an optional `padding` between the sizes of the shapes
   * (e.g. if `s1` should contain `s2` with margin `padding`).
   */
  contains: {
    name: "contains",
    description: `Require that a shape \`s1\` contains another shape \`s2\`, based on the type of the shape, and with an optional \`padding\` between the sizes of the shapes (e.g. if \`s1\` should contain \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, padding = 0.0) => {
      const t1 = s1.shapeType,
        t2 = s2.shapeType;
      if (t1 === "Circle" && t2 === "Circle")
        return constrDict.containsCircles.body(
          [s1.center.contents[0], s1.center.contents[1]],
          s1.r.contents,
          [s2.center.contents[0], s2.center.contents[1]],
          s2.r.contents,
          padding
        );
      else if (t1 === "Polygon" && t2 === "Polygon") {
        return constrDict.containsPolygons.body(
          s1.points.contents.map((p) => [p[0], p[1]]),
          s2.points.contents.map((p) => [p[0], p[1]]),
          padding
        );
      } else if (t1 === "Polygon" && t2 === "Circle") {
        return constrDict.containsPolygonCircle.body(
          s1.points.contents.map((p) => [p[0], p[1]]),
          [s2.center.contents[0], s2.center.contents[1]],
          s2.r.contents,
          padding
        );
      } else if (t1 === "Circle" && t2 === "Polygon") {
        return constrDict.containsCirclePolygon.body(
          [s1.center.contents[0], s1.center.contents[1]],
          s1.r.contents,
          s2.points.contents.map((p) => [p[0], p[1]]),
          padding
        );
      } else if (t1 === "Circle" && isRectlike(s2)) {
        const rectPts = bboxPts(bboxFromShape(s2));
        return constrDict.containsCircleRect.body(
          [s1.center.contents[0], s1.center.contents[1]],
          s1.r.contents,
          rectPts,
          padding
        );
      } else if (isRectlike(s1) && t2 === "Circle") {
        const rectPts = bboxPts(bboxFromShape(s1));
        return constrDict.containsRectCircle.body(
          rectPts,
          [s2.center.contents[0], s2.center.contents[1]],
          s2.r.contents,
          padding
        );
      } else {
        const s1BboxPts = bboxPts(bboxFromShape(s1));
        const s2BboxPts = bboxPts(bboxFromShape(s2));
        return constrDict.containsRects.body(s1BboxPts, s2BboxPts, padding);
      }
    },
  },

  containsCircles: {
    name: "containsCircles",
    description: `Require that a circle \`c1\` contains another circle \`c2\` with optional margin \`padding\`.`,
    params: [
      { name: "c1", description: "Center of `c1`", type: real2T() },
      { name: "r1", description: "Radius of `c1`", type: realT() },
      { name: "c2", description: "Center of `c2`", type: real2T() },
      { name: "r2", description: "Radius of `c2`", type: realT() },
      {
        name: "padding",
        description: "Margin between the circles",
        type: realT(),
        default: 0,
      },
    ],
    body: (c1: ad.Pt2, r1: ad.Num, c2: ad.Pt2, r2: ad.Num, padding: ad.Num) => {
      const d = ops.vdist(c1, c2);
      const o = padding ? sub(sub(r1, r2), padding) : sub(r1, r2);
      return sub(d, o);
    },
  },

  containsPolygons: {
    name: "containsPolygons",
    description: `Require that a polygon \`p1\` contains another polygon \`p2\` with optional margin \`padding\`.`,
    params: [
      { name: "pts1", description: "List of points for `p1`", type: real2NT() },
      { name: "pts2", description: "List of points for `p2`", type: real2NT() },
      {
        name: "padding",
        description: "Margin between the polygons",
        type: realT(),
        default: 0,
      },
    ],
    body: (pts1: ad.Pt2[], pts2: ad.Pt2[], padding: ad.Num) => {
      return maxN(
        pts2.map((x) => constrDict.containsPolygonPoint.body(pts1, x, padding))
      );
    },
  },

  containsPolygonCircle: {
    name: "containsPolygonCircle",
    description: `Require that a polygon \`p\` contains circle \`c\` with optional margin \`padding\`.`,
    params: [
      { name: "pts", description: "List of points for `p`", type: real2NT() },
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "padding",
        description: "Margin between the polygon and the circle",
        type: realT(),
        default: 0,
      },
    ],
    body: (pts: ad.Pt2[], c: ad.Pt2, r: ad.Num, padding: ad.Num) => {
      return constrDict.containsPolygonPoint.body(pts, c, add(padding, r));
    },
  },

  containsPolygonPoint: {
    name: "containsPolygonPoint",
    description: `Require that a polygon \`p\` contains point \`pt\` with optional margin \`padding\`.`,
    params: [
      {
        name: "pts",
        description: "List of points for polygon `p`",
        type: real2NT(),
      },
      { name: "pt", description: "Location of point `pt`", type: real2T() },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (pts: ad.Pt2[], pt: ad.Pt2, padding: ad.Num) => {
      const cp1 = convexPartitions(pts);
      return maxN(
        cp1.map((p1) => containsConvexPolygonPoints(p1, pt, padding))
      );
    },
  },

  containsCirclePoint: {
    name: "containsCirclePoint",
    description: `Require that a circle \`c\` contains point \`pt\` with optional margin \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      { name: "pt", description: "Location of point `pt`", type: real2T() },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (c: ad.Pt2, r: ad.Num, pt: ad.Pt2, padding: ad.Num) => {
      return sub(add(ops.vdist(pt, c), padding), r);
    },
  },

  containsCirclePolygon: {
    name: "containsCirclePolygon",
    description: `Require that a circle \`c\` contains polygon \`p\` with optional margin \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "pts",
        description: "List of points for polygon `p`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (c: ad.Pt2, r: ad.Num, pts: ad.Pt2[], padding: ad.Num) => {
      return maxN(
        pts.map((pt) => constrDict.containsCirclePoint.body(c, r, pt, padding))
      );
    },
  },

  containsCircleRect: {
    name: "containsCircleRect",
    description: `Require that a circle \`c\` contains rectangle \`r\` with optional padding \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "rect",
        description: "The top-left and bottom-right points of rectangle `rect`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (c: ad.Pt2, r: ad.Num, rect: ad.Pt2[], padding: ad.Num) => {
      // Bad implementation
      // Treats the rectangle as a circle
      /*
      if (rect.length !== 2) {
        throw new Error("`rect` should be a list of two 2d-points.");
      }
      const bbox = BBox.bboxFromPoints(rect);
      const rectr = mul(0.5, max(bbox.width, bbox.height));
      const rectc = bbox.center;
      return constrDict.containsCircles.body(
        c,
        r,
        [rectc[0], rectc[1]],
        rectr,
        padding
      );
      */
      if (rect.length !== 2) {
        throw new Error("`rect` should be a list of two 2d-points.");
      }
      const bbox = BBox.bboxFromPoints(rect);
      const { topLeft, topRight, bottomLeft, bottomRight } = BBox.corners(bbox);
      return constrDict.containsCirclePolygon.body(
        c,
        r,
        [topLeft, topRight, bottomRight, bottomLeft],
        padding
      );
    },
  },

  containsRectCircle: {
    name: "containsRectCircle",
    description: `Require that a rectangle \`r\` contains a circle \`c\` with optional margin \`padding\`.`,
    params: [
      {
        name: "rect",
        description: "The top-left and bottom-right points of rectangle `rect`",
        type: real2NT(),
      },
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },

      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (rect: ad.Pt2[], c: ad.Pt2, r: ad.Num, padding: ad.Num) => {
      /*
      if (rect.length !== 2) {
        throw new Error("`rect` should be a list of two 2d-points.");
      }
      const [tl, br] = rect;
      const w = sub(br[0], tl[0]);
      const h = sub(tl[1], br[1]);
      const halfW = mul(0.5, w);
      const halfH = mul(0.5, h);
      const [rx, ry] = ops.vdiv(ops.vadd(tl, br), 2); // rectangle center
      const [cx, cy] = c; // circle center
      // Return maximum violation in either the x- or y-direction.
      // In each direction, the distance from the circle center (cx,cy) to
      // the rectangle center (rx,ry) must be no greater than the size of
      // the rectangle (w/h), minus the radius of the circle (r) and the
      // padding (o).  We can compute this violation via the function
      //    max( |cx-rx| - (w/2-r-o),
      //         |cy-ry| - (h/2-r-o) )
      return max(
        sub(absVal(sub(cx, rx)), sub(sub(halfW, r), padding)),
        sub(absVal(sub(cy, ry)), sub(sub(halfH, r), padding))
      );
      */
      if (rect.length !== 2) {
        throw new Error("`rect` should be a list of two 2d-points.");
      }
      const bbox = BBox.bboxFromPoints(rect);
      const { topLeft, topRight, bottomLeft, bottomRight } = BBox.corners(bbox);
      return constrDict.containsPolygonCircle.body(
        [topLeft, topRight, bottomRight, bottomLeft],
        c,
        r,
        padding
      );
    },
  },

  containsRects: {
    name: "containsRects",
    description: `Requires that \`rect1\` contains \`rect2\` with some optional margin \`padding\`.`,
    params: [
      {
        name: "rect1",
        description:
          "The top-left and bottom-right points of rectangle `rect1`",
        type: real2NT(),
      },
      {
        name: "rect2",
        description:
          "The top-left and bottom-right points of rectangle `rect2`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: (rect1: ad.Pt2[], rect2: ad.Pt2[], padding: ad.Num) => {
      /*
      // TODO: padding?

      const box1 = BBox.bboxFromPoints(rect1);
      const box2 = BBox.bboxFromPoints(rect2);

      const [[xl1, xr1], [xl2, xr2]] = [BBox.xRange(box1), BBox.xRange(box2)];
      const [[yl1, yr1], [yl2, yr2]] = [BBox.yRange(box1), BBox.yRange(box2)];
      return addN([
        ifCond(lt(xl1, xl2), 0, squared(sub(xl1, xl2))),
        ifCond(lt(xr2, xr1), 0, squared(sub(xr2, xr1))),
        ifCond(lt(yl1, yl2), 0, squared(sub(yl1, yl2))),
        ifCond(lt(yr2, yr1), 0, squared(sub(yr2, yr1))),
      ]);
      */
      if (rect1.length !== 2 || rect2.length !== 2) {
        throw new Error("Inputs should be lists of two 2d-points.");
      }
      const box1 = BBox.bboxFromPoints(rect1);
      const box2 = BBox.bboxFromPoints(rect2);
      const {
        topLeft: topLeft1,
        topRight: topRight1,
        bottomLeft: bottomLeft1,
        bottomRight: bottomRight1,
      } = BBox.corners(box1);
      const {
        topLeft: topLeft2,
        topRight: topRight2,
        bottomLeft: bottomLeft2,
        bottomRight: bottomRight2,
      } = BBox.corners(box2);

      return constrDict.containsPolygons.body(
        [topLeft1, topRight1, bottomRight1, bottomLeft1],
        [topLeft2, topRight2, bottomRight2, bottomLeft2],
        padding
      );
    },
  },

  /**
   * Require that shape `s1` is at a distance of `distance` from shape `s2`.
   */
  atDist: {
    name: "atDist",
    description:
      "Require that shape `s1` is at a distance of `distance` from shape `s2`.",
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "distance", description: "Distance", type: realT() },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, distance: ad.Num) => {
      if (isRectlike(s2)) return atDistLabel(s1, s2, distance);
      else return constrDictGeneral.touching.body(s1, s2, distance);
    },
  },

  /**
   * Require that shape `s1` is smaller than `s2` with some relative padding `relativePadding`.
   */
  smallerThan: {
    name: "smallerThan",
    description:
      "Require that shape `s1` is smaller than `s2` with some relative padding `relativePadding`.",

    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      {
        name: "relativePadding",
        description: "Relative padding",
        type: realT(),
        default: 0.4,
      },
    ],
    body: (s1: Shape<ad.Num>, s2: Shape<ad.Num>, relativePadding = 0.4) => {
      // s1 is smaller than s2
      const size1 = shapeSize(s1);
      const size2 = shapeSize(s2);
      const padding = mul(relativePadding, size2);
      return sub(sub(size1, size2), padding);
    },
  },
};

// -------- Specific constraints
// Defined only for specific use-case or specific shapes.
const constrDictSpecific = {
  /**
   * Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.
   */
  disjointIntervals: {
    name: "disjointIntervals",
    description:
      "Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.",

    params: [
      { name: "s1", description: "Line 1", type: shapeT("Line") },
      { name: "s2", description: "Line 2", type: shapeT("Line") },
    ],
    body: (s1: Line<ad.Num>, s2: Line<ad.Num>) => {
      return overlap1D(
        [s1.start.contents[0], s1.end.contents[0]],
        [s2.start.contents[0], s2.end.contents[0]]
      );
    },
  },
};

export const constrDict = {
  ...constrDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...constrDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific constrains.
  ...constrDictSpecific, // Defined only for specific use-case or specific shapes.
  ...constrDictCurves, // Curve-specific constraints.
};

// `_constrDictVals` causes TypeScript to enforce that every function in
// `constrDict` actually has type `ConstrFunc` with the right function signature, etc.
const _constrDictVals: ConstrFunc[] = Object.values(constrDict);
