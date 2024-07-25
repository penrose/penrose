import {
  Num,
  Equation as PenroseEquation,
  Image as PenroseImage,
  Line as PenroseLine,
  Rectangle as PenroseRectangle,
  Text as PenroseText,
  objDict,
} from "@penrose/core";
import {
  Equation,
  Image,
  Line,
  Rectangle,
  Shape,
  Text,
  Vec2,
  VecN,
} from "../types.js";
import { toPenroseShape } from "../utils.js";

type PenroseRectLike =
  | PenroseRectangle<Num>
  | PenroseEquation<Num>
  | PenroseText<Num>
  | PenroseImage<Num>;

export type RectLike = Rectangle | Equation | Text | Image;

const objectives = {
  /**
   * Encourage the input value to be close to negative infinity
   * @param x Value
   */
  minimal: (x: Num): Num => objDict.minimal.body(x).value,

  /**
   * Encourage the input value to be close to infinity
   * @param x Value
   */
  maximal: (x: Num): Num => objDict.maximal.body(x).value,

  /**
   * Encourage the inputs to have the same value: `(x - y)^2`
   * @param x First value
   * @param y Second value
   */
  equal: (x: Num, y: Num): Num => objDict.equal.body(x, y).value,

  /**
   * Encourage two vectors `v1` and `v2` to be near each other with distance `offset`.
   * @param v1 A vector
   * @param v2 A vector
   * @param offset Distance between two vectors
   */
  nearVec: (v1: VecN, v2: VecN, offset: Num): Num =>
    objDict.nearVec.body(v1, v2, offset).value,

  /**
   * Encourage x to be greater than or equal to y: `max(0,y - x)^2`
   * @param x First value
   * @param y Second value
   */
  greaterThan: (x: Num, y: Num): Num => objDict.greaterThan.body(x, y).value,

  /**
   * Encourage x to be less than or equal to y: `max(0,x - y)^2`
   * @param x First value
   * @param y Second value
   */
  lessThan: (x: Num, y: Num): Num => objDict.lessThan.body(x, y).value,

  /**
   * Repel point `a` from another scalar `b` with weight `weight`.
   * @param weight Weight
   * @param a First point
   * @param b Second point
   */
  repelPt: (weight: Num, a: Vec2, b: Vec2): Num =>
    objDict.repelPt.body(weight, a, b).value,

  /**
   * Repel scalar `c` from another scalar `d`.
   * @param c First scalar
   * @param d Second scalar
   */
  repelScalar: (c: Num, d: Num): Num => objDict.repelScalar.body(c, d).value,

  /**
   * Encourage the point `p` to be in the direction `direction` with respect to point `pRef`. The `direction` vector does not need to be normalized. The `offset` parameter is the shortest allowed distance between the points.
   * @param p Point
   * @param pRef Reference point
   * @param direction Direction vector
   * @param offset Shortest allowed distance
   */
  inDirection: (p: Vec2, pRef: Vec2, direction: Vec2, offset: Num): Num =>
    objDict.inDirection.body(p, pRef, direction, offset).value,

  /**
   * Encourage the center of `bottom` to be below the center of `top`.
   * @param bottom Shape on the bottom
   * @param top Shape on the top
   * @param offset Distance between the two centers (default: 100)
   */
  below: (bottom: Shape, top: Shape, offset: Num = 100): Num =>
    objDict.below.body(toPenroseShape(bottom), toPenroseShape(top), offset)
      .value,

  /**
   * Encourage the center of `top` to be above the center of `bottom`.
   * @param top Shape on the top
   * @param bottom Shape on the bottom
   * @param offset Distance between the two centers (default: 100)
   */
  above: (top: Shape, bottom: Shape, offset: Num = 100): Num =>
    objDict.above.body(toPenroseShape(top), toPenroseShape(bottom), offset)
      .value,

  /**
   * Encourage the center of `left` to be leftwards to the center of `right`.
   * @param left Shape on the left
   * @param right Shape on the right
   * @param offset Distance between the two centers (default: 100)
   */
  leftwards: (left: Shape, right: Shape, offset: Num = 100): Num =>
    objDict.leftwards.body(toPenroseShape(left), toPenroseShape(right), offset)
      .value,

  /**
   * Encourage the center of `right` to be rightwards to the center of `left`.
   * @param right Shape on the right
   * @param left Shape on the left
   * @param offset Distance between the two centers (default: 100)
   */
  rightwards: (right: Shape, left: Shape, offset: Num = 100): Num =>
    objDict.rightwards.body(toPenroseShape(right), toPenroseShape(left), offset)
      .value,

  /**
   * Encourage shape `s1` to have the same center position as shape `s2`.
   * @param s1 A shape
   * @param s2 A shape
   */
  sameCenter: (s1: Shape, s2: Shape): Num =>
    objDict.sameCenter.body(toPenroseShape(s1), toPenroseShape(s2)).value,

  /**
   * Try to repel shapes `s1` and `s2` with some weight.
   * @param s1 A shape
   * @param s2 A shape
   * @param weight Weight of repel (default: 10.0)
   */
  notTooClose: (s1: Shape, s2: Shape, weight: Num = 10.0): Num =>
    objDict.notTooClose.body(toPenroseShape(s1), toPenroseShape(s2), weight)
      .value,

  /**
   * Try to place shape `s1` near shape `s2` (putting their centers at the same place).
   * @param s1 A shape
   * @param s2 A shape
   * @param offset Offset (default: 10.0)
   */
  near: (s1: Shape, s2: Shape, offset: Num = 10.0): Num =>
    objDict.near.body(toPenroseShape(s1), toPenroseShape(s2), offset).value,

  /**
   * Try to place shape `s1` near a location `(x, y)`.
   * @param s1 A shape
   * @param x X coordinate
   * @param y Y coordinate
   */
  nearPt: (s1: Shape, x: Num, y: Num): Num =>
    objDict.nearPt.body(toPenroseShape(s1), x, y).value,

  /**
   * Repel the angle between the p1-p0 and p1-p2 away from 0 and 180 degrees.
   * NOTE: angles more than `range` degrees from 0 or 180 deg are considered satisfied.
   * @param s0 A shape
   * @param s1 A shape
   * @param s2 A shape
   * @param strength Strength (default: 20)
   * @param range Range (default: 10)
   */
  nonDegenerateAngle: (
    s0: Shape,
    s1: Shape,
    s2: Shape,
    strength: Num = 20,
    range: Num = 10,
  ): Num =>
    objDict.nonDegenerateAngle.body(
      toPenroseShape(s0),
      toPenroseShape(s1),
      toPenroseShape(s2),
      strength,
      range,
    ).value,

  /**
   * Center label above a line
   * @param s1 A line
   * @param s2 A rect-like shape (rectangle, image, text, or equation)
   * @param w Weight
   */
  centerLabelAbove: (s1: Line, s2: RectLike, w: Num): Num =>
    objDict.centerLabelAbove.body(
      toPenroseShape(s1) as PenroseLine<Num>,
      toPenroseShape(s2) as PenroseRectLike,
      w,
    ).value,

  /**
   * Try to center a label `s2` with respect to some shape `s1`.
   * @param s1 A line or rect-like shape
   * @param s2 A rect-like shape (rectangle, image, text, or equation)
   * @param w Weight
   * @param padding Padding (default: 10)
   */
  centerLabel: (
    s1: RectLike | Line,
    s2: RectLike,
    w: number,
    padding: Num = 10,
  ): Num =>
    objDict.centerLabel.body(
      toPenroseShape(s1) as PenroseRectLike | PenroseLine<Num>,
      toPenroseShape(s2) as PenroseRectLike,
      w,
      padding,
    ).value,

  /**
   * Try to make distance between a point and a segment `s1` equal to padding.
   * @param point A point
   * @param s1 A line
   * @param padding Padding
   */
  pointLineDist: (point: Vec2, s1: Line, padding: Num): Num =>
    objDict.pointLineDist.body(
      point,
      toPenroseShape(s1) as PenroseLine<Num>,
      padding,
    ).value,

  /**
   * Try to make the shape regular (equiangular and equilateral)
   * @param points Points of polygonal chain
   * @param closed Whether the polygonic chain is closed
   */
  isRegular: (points: Vec2[], closed: boolean): Num =>
    objDict.isRegular.body(points, closed).value,

  /**
   * Try to make the shape equilateral
   * @param points Points of polygonal chain
   * @param closed Whether the polygonic chain is closed
   */
  isEquilateral: (points: Vec2[], closed: boolean): Num =>
    objDict.isEquilateral.body(points, closed).value,

  /**
   * Try to make the shape equiangular
   * @param points Points of polygonal chain
   * @param closed Whether the polygonic chain is closed
   */
  isEquiangular: (points: Vec2[], closed: boolean): Num =>
    objDict.isEquiangular.body(points, closed).value,
};

export default objectives;
