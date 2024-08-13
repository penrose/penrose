import { Num, Line as PenroseLine, constrDict } from "@penrose/core";
import { Line, Shape, Vec2, VecN } from "./types.js";
import { toPenroseShape } from "./utils.js";

/**
 * Require that the value `x` is equal to the value `y`.
 * @param x First value
 * @param y Second value
 */
export const equal = (x: Num, y: Num) => constrDict.equal.body(x, y).value;

/**
 * Require that the value `x` is less than the value `y` with optional padding `padding`.
 * @param x First value
 * @param y Second value
 * @param padding Padding (default: 0)
 */
export const lessThan = (x: Num, y: Num, padding: number = 0) =>
  constrDict.lessThan.body(x, y, padding).value;

/**
 * Require that the value `x` is greater than the value `y` with optional padding `padding`.
 * @param x First value
 * @param y Second value
 * @param padding Padding (default: 0)
 */
export const greaterThan = (x: Num, y: Num, padding: number = 0) =>
  constrDict.greaterThan.body(x, y, padding).value;

/**
 * Require that the value `x` is less than the value `y`, with steeper penalty.
 * @param x First value
 * @param y Second value
 */
export const lessThanSq = (x: Num, y: Num) =>
  constrDict.lessThanSq.body(x, y).value;

/**
 * Require that the value `x` is greater than the value `y`, with steeper penalty.
 * @param x First value
 * @param y Second value
 */
export const greaterThanSq = (x: Num, y: Num) =>
  constrDict.greaterThanSq.body(x, y).value;

/**
 * Require that the value `x` is in the range defined by `[x0, x1]`.
 * @param x Value
 * @param x0 Lower bound
 * @param x1 Upper bound
 */
export const inRange = (x: Num, x0: Num, x1: Num) =>
  constrDict.inRange.body(x, x0, x1).value;

/**
 * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
 * @param interval1 First interval [l1, r1]
 * @param interval2 Second interval [l2, r2]
 */
export const contains1D = (interval1: Vec2, interval2: Vec2) =>
  constrDict.contains1D.body(interval1, interval2).value;

/**
 * Make scalar `c` disjoint from a range `left, right`.
 * @param c Scalar
 * @param left Left bound
 * @param right Right bound
 */
export const disjointScalar = (c: Num, left: Num, right: Num) =>
  constrDict.disjointScalar.body(c, left, right).value;

/**
 * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
 * @param q First point
 * @param p Second point
 * @param r Third point
 */
export const perpendicular = (q: VecN, p: VecN, r: VecN) =>
  constrDict.perpendicular.body(q, p, r).value;

/**
 * Require that three points be collinear. This does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
 * @param c1 First point
 * @param c2 Second point
 * @param c3 Third point
 */
export const collinear = (c1: VecN, c2: VecN, c3: VecN) =>
  constrDict.collinear.body(c1, c2, c3).value;

/**
 * Require that three points be collinear and enforces the order of these points as provided.
 * @param c1 First point
 * @param c2 Second point
 * @param c3 Third point
 */
export const collinearOrdered = (c1: VecN, c2: VecN, c3: VecN) =>
  constrDict.collinearOrdered.body(c1, c2, c3).value;

/**
 * Require that `shape` is on the canvas.
 * @param shape Shape
 * @param canvasWidth Width of canvas
 * @param canvasHeight Height of canvas
 */
export const onCanvas = (shape: Shape, canvasWidth: Num, canvasHeight: Num) =>
  constrDict.onCanvas.body(toPenroseShape(shape), canvasWidth, canvasHeight)
    .value;

/**
 * Require that shape `s1` overlaps shape `s2` with some overlap `overlap`.
 * Based on the type of the shape, and with an optional `overlap` between them
 * (e.g. if `s1` should be overlapping `s2` with margin `overlap`).
 * @param s1 Shape 1
 * @param s2 Shape 2
 * @param overlap Overlap (default: 0)
 */
export const overlapping = (s1: Shape, s2: Shape, overlap: Num = 0) =>
  constrDict.overlapping.body(toPenroseShape(s1), toPenroseShape(s2), overlap)
    .value;

/**
 * Require that ellipse `e1` overlaps with ellipse `e2` with some overlap `overlap`.
 * @param c1 Center of `e1`
 * @param rx1 Horizontal radius of `e1`
 * @param ry1 Vertical radius of `e1`
 * @param c2 Center of `e2`
 * @param rx2 Horizontal radius of `e2`
 * @param ry2 Vertical radius of `e2`
 * @param overlap The least amount of overlap (default: 0)
 */
export const overlappingEllipses = (
  c1: Vec2,
  rx1: Num,
  ry1: Num,
  c2: Vec2,
  rx2: Num,
  ry2: Num,
  overlap: Num = 0,
) =>
  constrDict.overlappingEllipses.body(c1, rx1, ry1, c2, rx2, ry2, overlap)
    .value;

/**
 * Require that circle `c` overlaps with ellipse `e` with some overlap `overlap`.
 * @param c1 Center of `c`
 * @param r1 Radius of `c`
 * @param c2 Center of `e`
 * @param rx2 Horizontal radius of `e`
 * @param ry2 Vertical radius of `e`
 * @param overlap The least amount of overlap (default: 0)
 */
export const overlappingCircleEllipse = (
  c1: Vec2,
  r1: Num,
  c2: Vec2,
  rx2: Num,
  ry2: Num,
  overlap: Num = 0,
) =>
  constrDict.overlappingCircleEllipse.body(c1, r1, c2, rx2, ry2, overlap).value;

/**
 * Require that a shape `s1` is disjoint from shape `s2`,
 * based on the type of the shape, and with an optional `padding` between them
 * (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
 * @param s1 Shape 1
 * @param s2 Shape 2
 * @param padding Padding (default: 0)
 */
export const disjoint = (s1: Shape, s2: Shape, padding: Num = 0) =>
  constrDict.disjoint.body(toPenroseShape(s1), toPenroseShape(s2), padding)
    .value;

/**
 * Require that shape `s1` is touching shape `s2`.
 * Based on the type of the shape, and with an optional `padding` between them
 * (e.g. if `s1` should be touching `s2` with margin `padding`).
 * @param s1 Shape 1
 * @param s2 Shape 2
 * @param padding Padding (default: 0)
 */
export const touching = (s1: Shape, s2: Shape, padding: Num = 0) =>
  constrDict.touching.body(toPenroseShape(s1), toPenroseShape(s2), padding)
    .value;

/**
 * Require that a shape `s1` contains another shape `s2`,
 * based on the type of the shape, and with an optional `padding` between the sizes of the shapes
 * (e.g. if `s1` should contain `s2` with margin `padding`).
 * @param s1 Shape 1
 * @param s2 Shape 2
 * @param padding Padding (default: 0)
 */
export const contains = (s1: Shape, s2: Shape, padding: Num = 0) =>
  constrDict.contains.body(toPenroseShape(s1), toPenroseShape(s2), padding)
    .value;

/**
 * Require that a circle `c1` contains another circle `c2` with optional margin `padding`.
 * @param c1 Center of `c1`
 * @param r1 Radius of `c1`
 * @param c2 Center of `c2`
 * @param r2 Radius of `c2`
 * @param padding Margin between the circles (default: 0)
 */
export const containsCircles = (
  c1: Vec2,
  r1: Num,
  c2: Vec2,
  r2: Num,
  padding: Num = 0,
) => constrDict.containsCircles.body(c1, r1, c2, r2, padding).value;

/**
 * Require that a polygon `p1` contains another polygon `p2` with optional margin `padding`.
 * @param pts1 List of points for `p1`
 * @param pts2 List of points for `p2`
 * @param padding Margin between the polygons (default: 0)
 */
export const containsPolys = (pts1: Vec2[], pts2: Vec2[], padding: Num = 0) =>
  constrDict.containsPolys.body(pts1, pts2, padding).value;

/**
 * Require that a polygon `p` contains circle `c` with optional margin `padding`.
 * @param pts List of points for `p`
 * @param c Center of `c`
 * @param r Radius of `c`
 * @param padding Margin between the polygon and the circle (default: 0)
 */
export const containsPolyCircle = (
  pts: Vec2[],
  c: Vec2,
  r: Num,
  padding: Num = 0,
) => constrDict.containsPolyCircle.body(pts, c, r, padding).value;

/**
 * Require that a polygon `p` contains point `pt` with optional margin `padding`.
 * @param pts List of points for polygon `p`
 * @param pt Location of point `pt`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsPolyPoint = (pts: Vec2[], pt: Vec2, padding: Num = 0) =>
  constrDict.containsPolyPoint.body(pts, pt, padding).value;

/**
 * Require that a circle `c` contains point `pt` with optional margin `padding`.
 * @param c Center of `c`
 * @param r Radius of `c`
 * @param pt Location of point `pt`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsCirclePoint = (
  c: Vec2,
  r: Num,
  pt: Vec2,
  padding: Num = 0,
) => constrDict.containsCirclePoint.body(c, r, pt, padding).value;

/**
 * Require that a circle `c` contains polygon `p` with optional margin `padding`.
 * @param c Center of `c`
 * @param r Radius of `c`
 * @param pts List of points for polygon `p`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsCirclePoly = (
  c: Vec2,
  r: Num,
  pts: Vec2[],
  padding: Num = 0,
) => constrDict.containsCirclePoly.body(c, r, pts, padding).value;

/**
 * Require that a circle `c` contains rectangle `r` with optional padding `padding`.
 * @param c Center of `c`
 * @param r Radius of `c`
 * @param rect The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsCircleRect = (
  c: Vec2,
  r: Num,
  rect: Vec2[],
  padding: Num = 0,
) => constrDict.containsCircleRect.body(c, r, rect, padding).value;

/**
 * Require that a rectangle `r` contains a circle `c` with optional margin `padding`.
 * @param rect The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect`
 * @param c Center of `c`
 * @param r Radius of `c`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsRectCircle = (
  rect: Vec2[],
  c: Vec2,
  r: Num,
  padding: Num = 0,
) => constrDict.containsRectCircle.body(rect, c, r, padding).value;

/**
 * Requires that `rect1` contains `rect2` with some optional margin `padding`.
 * @param rect1 The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect1`
 * @param rect2 The top-right, top-left, bottom-left, bottom-right points (in that order) points of rectangle `rect2`
 * @param padding Margin between the polygon and the point (default: 0)
 */
export const containsRects = (rect1: Vec2[], rect2: Vec2[], padding: Num = 0) =>
  constrDict.containsRects.body(rect1, rect2, padding).value;

/**
 * Requires that the bounding boxes for shapes within a shapeList are evenly spaced horizontally with a set padding.
 * @param shapes List containing the top-left, top-right, bottom-right, bottom-left points (in that order) of the axis-aligned bounding box of a shape
 * @param padding Margin between bounding boxes of shapes (default: 0)
 */
export const distributeHorizontally = (
  shapes: Shape[],
  padding: Num,
  topToBottom: boolean,
) =>
  constrDict.distributeHorizontally.body(
    shapes.map((shape) => toPenroseShape(shape)),
    padding,
    topToBottom,
  ).value;

/**
 * Requires that the bounding boxes for shapes within a shapeList are evenly spaced vertically with a set padding.
 * @param shapes List containing the top-left, top-right, bottom-right, bottom-left points (in that order) of the axis-aligned bounding box of a shape
 * @param padding Margin between bounding boxes of shapes (default: 0)
 */
export const distributeVertically = (
  shapes: Shape[],
  padding: Num,
  topToBottom: boolean,
) =>
  constrDict.distributeVertically.body(
    shapes.map((shape) => toPenroseShape(shape)),
    padding,
    topToBottom,
  ).value;

/**
 * Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.
 * @param s1 Line 1
 * @param s2 Line 2
 */
export const disjointIntervals = (s1: Line, s2: Line) =>
  constrDict.disjointIntervals.body(
    toPenroseShape(s1) as PenroseLine<Num>,
    toPenroseShape(s2) as PenroseLine<Num>,
  ).value;
