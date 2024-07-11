import { Num, Value, compDict } from "@penrose/core";
import { Color, PathData, Vec2, VecN } from "../types.js";

const context = {
  makeInput: () => {
    throw new Error("Dummy context was called");
  },
};

const mapColor = (colorV: Value.ColorV<Num>): Color => {
  if (colorV.contents.tag === "NONE") {
    return [0, 0, 0, 0];
  } else {
    return colorV.contents.contents;
  }
};

const toPenroseColor = (color: Color): Value.Color<Num> => {
  return {
    tag: "RGBA",
    contents: color,
  };
};

const computation = {
  /**
   * See https://github.com/penrose/penrose/issues/716
   * @param start Start point of the path
   * @param end End point of the path
   * @param curveHeight Height of the curve
   * @param padding Padding between the curve and the labels
   * @returns Path data
   */
  makePath: (
    start: Vec2,
    end: Vec2,
    curveHeight: Num,
    padding: Num,
  ): PathData =>
    compDict.makePath.body(context, start, end, curveHeight, padding).value
      .contents,

  /**
   * Return `i`th element of list `xs`, assuming lists only hold floats.
   * @param xs List of floats
   * @param i Index of the element to return
   * @returns The i-th element of xs
   */
  get: (xs: VecN, i: number): Num =>
    compDict.get.body(context, xs, i).value.contents,

  /**
   * Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).
   * @param r Red
   * @param g Green
   * @param b Blue
   * @param a Opacity
   * @returns RGBA color
   */
  rgba: (r: Num, g: Num, b: Num, a: Num): Color =>
    mapColor(compDict.rgba.body(context, r, g, b, a).value),

  /**
   * Select a color based on a level
   * @param color1 First color
   * @param color2 Second color
   * @param level Level
   * @returns Selected color
   */
  selectColor: (color1: Color, color2: Color, level: Num): Color =>
    mapColor(
      compDict.selectColor.body(
        context,
        toPenroseColor(color1),
        toPenroseColor(color2),
        level,
      ).value,
    ),

  /**
   * Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).
   * @param h Hue in [0, 360)
   * @param s Saturation in [0, 100]
   * @param v Value in [0, 100]
   * @param a Opacity
   * @returns HSVA color
   */
  hsva: (h: Num, s: Num, v: Num, a: Num): Color =>
    mapColor(compDict.hsva.body(context, h, s, v, a).value),

  /**
   * Return a paint of none (no paint)
   * @returns None color
   */
  none: (): Color => mapColor(compDict.none.body(context).value),

  /**
   * Index a point list using 1-based indexing.
   * @param points List of points
   * @param i 1-based index
   * @returns The i-th point
   */
  oneBasedElement: (points: Vec2[], i: number): Vec2 =>
    compDict.oneBasedElement.body(context, points, i).value.contents as Vec2,

  /**
   * Return `acosh(x)`.
   * @param x Input value
   * @returns acosh(x)
   */
  acosh: (x: Num): Num => compDict.acosh.body(context, x).value.contents,

  /**
   * Return `acos(x)`.
   * @param x Input value
   * @returns acos(x)
   */
  acos: (x: Num): Num => compDict.acos.body(context, x).value.contents,

  /**
   * Return `asin(x)`.
   * @param x Input value
   * @returns asin(x)
   */
  asin: (x: Num): Num => compDict.asin.body(context, x).value.contents,

  /**
   * Return `asinh(x)`.
   * @param x Input value
   * @returns asinh(x)
   */
  asinh: (x: Num): Num => compDict.asinh.body(context, x).value.contents,

  /**
   * Return `mod(a, n)`.
   * @param a Dividend
   * @param n Divisor
   * @returns a mod n
   */
  mod: (a: Num, n: Num): Num => compDict.mod.body(context, a, n).value.contents,

  /**
   * Return `atan(x)`.
   * @param x Input value
   * @returns atan(x)
   */
  atan: (x: Num): Num => compDict.atan.body(context, x).value.contents,

  /**
   * Return `atan2(y, x)`.
   * @param x x-coordinate
   * @param y y-coordinate
   * @returns atan2(y, x)
   */
  atan2: (x: Num, y: Num): Num =>
    compDict.atan2.body(context, x, y).value.contents,

  /**
   * Return `atanh(x)`.
   * @param x Input value
   * @returns atanh(x)
   */
  atanh: (x: Num): Num => compDict.atanh.body(context, x).value.contents,

  /**
   * Return `cbrt(x)`.
   * @param x Input value
   * @returns cbrt(x)
   */
  cbrt: (x: Num): Num => compDict.cbrt.body(context, x).value.contents,

  /**
   * Return `ceil(x)`.
   * @param x Input value
   * @returns ceil(x)
   */
  ceil: (x: Num): Num => compDict.ceil.body(context, x).value.contents,

  /**
   * Return `cos(x)`.
   * @param x Input value
   * @returns cos(x)
   */
  cos: (x: Num): Num => compDict.cos.body(context, x).value.contents,

  /**
   * Return `cosh(x)`.
   * @param x Input value
   * @returns cosh(x)
   */
  cosh: (x: Num): Num => compDict.cosh.body(context, x).value.contents,

  /**
   * Return `exp(x)`.
   * @param x Input value
   * @returns exp(x)
   */
  exp: (x: Num): Num => compDict.exp.body(context, x).value.contents,
};

export default computation;
