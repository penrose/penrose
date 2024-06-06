import _ from "lodash";
import { ops } from "../engine/Autodiff.js";
import {
  absVal,
  acos,
  acosh,
  add,
  addN,
  and,
  asin,
  asinh,
  atan,
  atan2,
  atanh,
  cbrt,
  ceil,
  cos,
  cosh,
  div,
  eq,
  exp,
  expm1,
  floor,
  gt,
  gte,
  ifCond,
  ln,
  log10,
  log1p,
  log2,
  lt,
  max,
  maxN,
  min,
  minN,
  mul,
  neg,
  not,
  or,
  pow,
  round,
  sign,
  sin,
  sinh,
  sqrt,
  squared,
  sub,
  tan,
  tanh,
  trunc,
} from "../engine/AutodiffFunctions.js";
import { bboxFromRectlike } from "../engine/BBox.js";
import { PathBuilder } from "../renderer/PathBuilder.js";
import { Circle } from "../shapes/Circle.js";
import { Ellipse } from "../shapes/Ellipse.js";
import { Group } from "../shapes/Group.js";
import { Line } from "../shapes/Line.js";
import { Path } from "../shapes/Path.js";
import { Polygon } from "../shapes/Polygon.js";
import { Polyline } from "../shapes/Polyline.js";
import { Context, uniform } from "../shapes/Samplers.js";
import { Shape } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { CompFunc, MayWarn } from "../types/functions.js";
import {
  ClipDataV,
  Color,
  ColorV,
  FloatV,
  LListV,
  ListV,
  MatrixV,
  PathCmd,
  PathDataV,
  PtListV,
  StrV,
  TupV,
  VectorV,
} from "../types/value.js";
import {
  booleanT,
  clipDataV,
  clipShape,
  colorT,
  colorTypeT,
  floatV,
  getStart,
  linePts,
  natT,
  noClip,
  noWarn,
  pathCmdT,
  pathDataListT,
  pathDataV,
  pathTypeT,
  posIntT,
  ptListV,
  real2NT,
  real2T,
  real3T,
  realNMT,
  realNT,
  realT,
  rectlikeT,
  shapeListT,
  shapeT,
  strV,
  stringT,
  unionT,
  unitT,
  valueT,
  vectorV,
} from "../utils/Util.js";
import {
  binormalVectors,
  centerOfMass,
  curvatures,
  elasticEnergy,
  evoluteCurve,
  inflectionEnergy,
  isoperimetricRatio,
  lengthK,
  maxCurvature,
  normalVectors,
  offsetCurve,
  pElasticEnergy,
  perimeter,
  signedArea,
  tangentVectors,
  totalCurvature,
  turningNumber,
} from "./Curves.js";
import {
  bboxFromShape,
  bboxPts,
  polygonLikePoints,
  rectLineDist,
  rectPts,
  shapeDistance,
  shapeDistanceCircleLine,
  shapeDistanceCircles,
  shapeDistanceLines,
  shapeDistancePolyEllipse,
  shapeDistancePolys,
  shapeDistanceRectCircle,
  shapeDistanceRectLine,
  shapeDistanceRectlikePolyline,
  shapeDistanceRects,
} from "./Queries.js";
import { Rectlike, clamp, isRectlike, numOf, toPt } from "./Utils.js";

/**
 * Static dictionary of computation functions
 * TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */

// NOTE: These all need to be written in terms of autodiff types
// These all return a Value<ad.Num>
export const compDict = {
  // TODO: Refactor derivative + derivativePre to be inlined as one case in evaluator

  makePath: {
    name: "makePath",
    description: "See https://github.com/penrose/penrose/issues/716",
    params: [
      { name: "start", description: "Start point of the path", type: real2T() },
      { name: "end", description: "End point of the path", type: real2T() },
      {
        name: "curveHeight",
        description: "Height of the curve",
        type: realT(),
      },
      {
        name: "padding",
        description: "Padding between the curve and the labels",
        type: realT(),
      },
    ],
    body: (
      _context: Context,
      start: [ad.Num, ad.Num],
      end: [ad.Num, ad.Num],
      curveHeight: ad.Num,
      padding: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      // Two vectors for moving from `start` to the control point: `unit` is the direction of vector [start, end] (along the line passing through both labels) and `normalVec` is perpendicular to `unit` through the `rot90` operation.
      const unit: ad.Num[] = ops.vnormalize(ops.vsub(start, end));
      const normalVec: ad.Num[] = ops.rot90(toPt(unit));
      // There's only one control point in a quadratic bezier curve, and we want it to be equidistant to both `start` and `end`
      const halfLen: ad.Num = div(ops.vdist(start, end), 2);
      const controlPt: ad.Num[] = ops.vmove(
        ops.vmove(end, halfLen, unit),
        curveHeight,
        normalVec,
      );
      const curveEnd: ad.Num[] = ops.vmove(end, padding, unit);
      // Both the start and end points of the curve should be padded by some distance such that they don't overlap with the texts
      const path = new PathBuilder();
      return noWarn(
        path
          .moveTo(toPt(ops.vmove(start, padding, ops.vneg(unit))))
          .quadraticCurveTo(toPt(controlPt), toPt(curveEnd))
          .getPath(),
      );
    },
    returns: pathCmdT(),
  },

  /**
   * Return `i`th element of list `xs, assuming lists only hold floats.
   */
  get: {
    name: "get",
    description:
      "Return `i`th element of list `xs, assuming lists only hold floats.",
    params: [
      { name: "xs", description: "List of floats", type: realNT() },
      {
        name: "i",
        description: "Index of the element to return",
        type: natT(),
      },
    ],
    body: (
      _context: Context,
      xs: ad.Num[],
      i: number,
    ): MayWarn<FloatV<ad.Num>> => {
      const res = xs[i];
      return noWarn({
        tag: "FloatV",
        contents: res,
      });
    },
    returns: realT(),
  },

  /**
   * Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).
   */
  rgba: {
    name: "rgba",
    description:
      "Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).",
    params: [
      { name: "r", description: "Red", type: unitT() },
      { name: "g", description: "Green", type: unitT() },
      { name: "b", description: "Blue", type: unitT() },
      { name: "a", description: "Opacity", type: unitT() },
    ],
    body: (
      _context: Context,
      r: ad.Num,
      g: ad.Num,
      b: ad.Num,
      a: ad.Num,
    ): MayWarn<ColorV<ad.Num>> => {
      return noWarn({
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [r, g, b, a],
        },
      });
    },
    returns: valueT("Color"),
  },

  selectColor: {
    name: "selectColor",
    params: [
      { name: "color1", description: "First color", type: colorT() },
      { name: "color2", description: "Second color", type: colorT() },
      { name: "level", description: "Level", type: realT() },
    ],
    body: (
      _context: Context,
      color1: Color<ad.Num>,
      color2: Color<ad.Num>,
      level: ad.Num,
    ): MayWarn<ColorV<ad.Num>> => {
      const half = div(level, 2);
      const even = eq(half, trunc(half)); // autodiff doesn't have a mod operator
      if (!(color1.tag === "RGBA" && color2.tag === "RGBA")) {
        throw Error("selectColor only supports RGBA");
      }
      return noWarn({
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          // https://github.com/penrose/penrose/issues/561
          contents: [
            ifCond(even, color1.contents[0], color2.contents[0]),
            ifCond(even, color1.contents[1], color2.contents[1]),
            ifCond(even, color1.contents[2], color2.contents[2]),
            ifCond(even, color1.contents[3], color2.contents[3]),
          ],
        },
      });
    },
    returns: colorT(),
  },

  /**
   * Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).
   */
  hsva: {
    name: "hsva",
    description:
      "Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).",
    params: [
      { name: "h", description: "Hue in [0, 360)", type: realT() },
      { name: "s", description: "Saturation in [0, 100]", type: realT() },
      { name: "v", description: "Value in [0, 100]", type: realT() },
      { name: "a", description: "Opacity", type: unitT() },
    ],
    body: (
      _context: Context,
      h: ad.Num,
      s: ad.Num,
      v: ad.Num,
      a: ad.Num,
    ): MayWarn<ColorV<ad.Num>> => {
      return noWarn({
        tag: "ColorV",
        contents: {
          tag: "HSVA",
          contents: [h, s, v, a],
        },
      });
    },
    returns: colorT(),
  },

  /**
   * Return a paint of none (no paint)
   */
  none: {
    name: "none",
    description: "Return a paint of none (no paint)",
    params: [],
    body: (_context: Context): MayWarn<ColorV<ad.Num>> => {
      return noWarn({
        tag: "ColorV",
        contents: {
          tag: "NONE",
        },
      });
    },
    returns: valueT("Color"),
  },

  oneBasedElement: {
    name: "oneBasedElement",
    description: "Index a point list using 1-based indexing.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "list of points",
      },
      { name: "i", type: posIntT(), description: "1-based index" },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      i: number,
    ): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: points[i - 1],
      });
    },
    returns: valueT("Real2"),
  },

  /**
   * Return `acosh(x)`.
   */
  acosh: {
    name: "acosh",
    description: "Return `acosh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: acosh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `acos(x)`.
   */
  acos: {
    name: "acos",
    description: "Return `acos(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: acos(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `asin(x)`.
   */
  asin: {
    name: "asin",
    description: "Return `asin(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: asin(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `asinh(x)`.
   */
  asinh: {
    name: "asinh",
    description: "Return `asinh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: asinh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `mod(a, n)`.
   */
  mod: {
    name: "mod",
    description: "Return `mod(a, n)`.",
    params: [
      { name: "a", description: "`a`", type: realT() },
      { name: "n", description: "`n`", type: realT() },
    ],
    body: (
      _context: Context,
      a: ad.Num,
      n: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: sub(a, mul(n, floor(div(a, n)))),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `atan(x)`.
   */
  atan: {
    name: "atan",
    description: "Return `atan(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: atan(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `atan2(y,x)`.
   */
  atan2: {
    name: "atan2",
    description: "Return `atan2(x, y)`.",
    params: [
      { name: "x", description: "`x`", type: realT() },
      { name: "y", description: "`y`", type: realT() },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: atan2(y, x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `atanh(x)`.
   */
  atanh: {
    name: "atanh",
    description: "Return `atanh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: atanh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `cbrt(x)`.
   */
  cbrt: {
    name: "cbrt",
    description: "Return `cbrt(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: cbrt(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `ceil(x)`.
   */
  ceil: {
    name: "ceil",
    description: "Return `ceil(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: ceil(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `cos(x)`.
   */
  cos: {
    name: "cos",
    description: "Return `cos(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: cos(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `cosh(x)`.
   */
  cosh: {
    name: "cosh",
    description: "Return `cosh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: cosh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `exp(x)`.
   */
  exp: {
    name: "exp",
    description: "Return `exp(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: exp(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `expm1(x)`.
   */
  expm1: {
    name: "expm1",
    description: "Return `expm1(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: expm1(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `floor(x)`.
   */
  floor: {
    name: "floor",
    description: "Return `floor(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: floor(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `log(x)`.
   */
  log: {
    name: "log",
    description: "Return `log(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: ln(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `log2(x)`.
   */
  log2: {
    name: "log2",
    description: "Return `log2(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: log2(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `log10(x)`.
   */
  log10: {
    name: "log10",
    description: "Return `log10(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: log10(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `log1p(x)`.
   */
  log1p: {
    name: "log1p",
    description: "Return `log1p(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: log1p(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `pow(x,y)`.
   */
  pow: {
    name: "pow",
    description: "Return `pow(x, y)`.",
    params: [
      { name: "x", description: "`x`", type: realT() },
      { name: "y", description: "`y`", type: realT() },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: pow(x, y),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `round(x)`.
   */
  round: {
    name: "round",
    description: "Return `round(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: round(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `sign(x)`.
   */
  sign: {
    name: "sign",
    description: "Return `sign(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: sign(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `sin(x)`.
   */
  sin: {
    name: "sin",
    description: "Return `sin(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: sin(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `sinh(x)`.
   */
  sinh: {
    name: "sinh",
    description: "Return `sinh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: sinh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `tan(x)`.
   */
  tan: {
    name: "tan",
    description: "Return `tan(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: tan(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `tanh(x)`.
   */
  tanh: {
    name: "tanh",
    description: "Return `tanh(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: tanh(x),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return `trunc(x)`.
   */
  trunc: {
    name: "trunc",
    description: "Return `trunc(x)`.",
    params: [{ name: "x", description: "`x`", type: realT() }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: trunc(x),
      });
    },
    returns: valueT("Real"),
  },

  sum: {
    name: "sum",
    description: "Return the sum of elements in a vector.",
    params: [{ name: "xs", description: "elements", type: realNT() }],
    body: (_context: Context, xs: ad.Num[]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: addN(xs),
      });
    },
    returns: realT(),
  },

  sumVectors: {
    name: "sumVectors",
    description: "Return the sum of vectors in a list of vectors.",
    params: [{ name: "vecs", description: "vectors", type: realNMT() }],
    body: (_context: Context, vecs: ad.Num[][]): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(sumVectors(vecs))),
    returns: realNT(),
  },

  maxList: {
    name: "maxList",
    description: "Return the maximum of the elements in a vector.",
    params: [{ name: "xs", description: "elements", type: realNT() }],
    body: (_context: Context, xs: ad.Num[]): MayWarn<FloatV<ad.Num>> =>
      noWarn({
        tag: "FloatV",
        contents: maxN(xs),
      }),
    returns: realT(),
  },

  minList: {
    name: "minList",
    description: "Return the minimum of the elements in a vector.",
    params: [{ name: "xs", description: "elements", type: realNT() }],
    body: (_context: Context, xs: ad.Num[]): MayWarn<FloatV<ad.Num>> =>
      noWarn({
        tag: "FloatV",
        contents: minN(xs),
      }),
    returns: realT(),
  },

  count: {
    name: "count",
    description: "Return the number of the elements in a vector.",
    params: [{ name: "xs", description: "elements", type: realNT() }],
    body: (_context: Context, xs: ad.Num[]): MayWarn<FloatV<ad.Num>> =>
      noWarn({
        tag: "FloatV",
        contents: xs.length,
      }),
    returns: realT(),
  },
  /**
   * Return the dot product of `v` and `w`.
   */
  dot: {
    name: "dot",
    description: "Return the dot product of `v` and `w`.",
    params: [
      { name: "v", description: "Vector `v`", type: realNT() },
      { name: "w", description: "Vector `w`", type: realNT() },
    ],
    body: (
      _context: Context,
      v: ad.Num[],
      w: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: ops.vdot(v, w),
      });
    },
    returns: valueT("Real"),
  },

  /* ============ MATRIX FUNCTIONS ============= */

  identity: {
    name: "identity",
    description:
      "`identity(n)` returns the $n \\times n$ identity matrix\n$$I = \\left[ \\begin{array}{cccc} 1 & 0 & \\cdots & 0 \\\\ 0 & 1 & \\cdots & 0 \\\\ \\vdots & \\vdots & \\ddots & \\vdots \\\\ 0 & 0 & \\cdots & 1 \\end{array} \\right].$$",
    params: [{ name: "n", description: "dimension", type: posIntT() }],
    body: (_context: Context, n: number): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: identity(n),
      });
    },
    returns: valueT("RealNM"),
  },

  diagonal: {
    name: "diagonal",
    description:
      "`diagonal(v)` takes a vector $v$ of length $n$, and returns the $n \\times n$ diagonal matrix\n$$D = \\left[ \\begin{array}{cccc} v_1 & 0 & \\cdots & 0 \\\\ 0 & v_2 & \\cdots & 0 \\\\ \\vdots & \\vdots & \\ddots & \\vdots \\\\ 0 & 0 & \\cdots & v_n \\end{array} \\right].$$",
    params: [
      { name: "v", description: "vector of diagonal entries", type: realNT() },
    ],
    body: (_context: Context, v: ad.Num[]): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: diagonal(v),
      });
    },
    returns: valueT("RealNM"),
  },

  trace: {
    name: "trace",
    description:
      "`trace(A)` takes a square matrix $A$, and returns the trace $\\text{tr}(A)$, equal to the sum of its diagonal entries.",
    params: [{ name: "A", description: "a square matrix", type: realNMT() }],
    body: (_context: Context, A: ad.Num[][]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: trace(A),
      });
    },
    returns: valueT("Real"),
  },

  determinant: {
    name: "determinant",
    description:
      "`determinant(A)` takes a $2 \\times 2$, $3 \\times 3$, or $4 \\times 4$ matrix $A$, and returns its determinant $\\text{det}(A)$.",
    params: [{ name: "A", description: "a square matrix", type: realNMT() }],
    body: (_context: Context, A: ad.Num[][]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: determinant(A),
      });
    },
    returns: valueT("Real"),
  },

  inverse: {
    name: "inverse",
    description:
      "`inverse(A)` takes a $2 \\times 2$, $3 \\times 3$, or $4 \\times 4$ matrix $A$, and returns its inverse $A^{-1}$.  If the matrix is not invertible, the result may be numerically invalid (with `INF` or `NaN` entries).",
    params: [
      { name: "A", description: "a 2x2, 3x3, or 4x4 matrix", type: realNMT() },
    ],
    body: (_context: Context, A: ad.Num[][]): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: inverse(A),
      });
    },
    returns: valueT("RealNM"),
  },

  outerProduct: {
    name: "outerProduct",
    description:
      "`outerProduct(u,v)` takes two vectors $u$, $v$ of equal length $n$, and returns the $n \\times n$ outer product matrix $A$, with entries $A_{ij} = u_i v_j$.",
    params: [
      { name: "v", description: "Vector `v`", type: realNT() },
      { name: "w", description: "Vector `w`", type: realNT() },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: outer(u, v),
      });
    },
    returns: valueT("RealNM"),
  },

  crossProductMatrix: {
    name: "crossProductMatrix",
    description:
      "`crossProductMatrix(v)` takes a 3-vector $v$, and returns a $3 \\times 3$ skew symmetric matrix $A^T = -A$ such that $Au = v \\times u$ for any vector $u$.",
    params: [{ name: "v", description: "Vector `v`", type: realNT() }],
    body: (_context: Context, v: ad.Num[]): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: crossProductMatrix(v),
      });
    },
    returns: valueT("RealNM"),
  },

  matrix: {
    name: "matrix",
    description:
      "`matrix(a,b,c,d,e,f)` specifies a transformation matrix\n$$\\left[ \\begin{array}{ccc} a & c & e \\\\ b & d & f \\\\ 0 & 0 & 1 \\end{array} \\right].$$\nThis function mirrors the SVG/CSS `matrix` transform function.",
    params: [
      { name: "a", description: "top left entry", type: realT() },
      { name: "b", description: "middle left entry", type: realT() },
      { name: "c", description: "top center entry", type: realT() },
      { name: "d", description: "middle center entry", type: realT() },
      { name: "e", description: "top right entry", type: realT() },
      { name: "f", description: "middle right entry", type: realT() },
    ],
    body: (
      _context: Context,
      a: ad.Num,
      b: ad.Num,
      c: ad.Num,
      d: ad.Num,
      e: ad.Num,
      f: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: matrix(a, b, c, d, e, f),
      });
    },
    returns: valueT("RealNM"),
  },

  matrix3d: {
    name: "matrix3d",
    description:
      "`matrix(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4)` specifies a transformation matrix\n$$\\left[ \\begin{array}{cccc} a1 & a2 & a3 & a4 \\\\ b1 & b2 & b3 & b4 \\\\ c1 & c2 & c3 & c4 \\\\ d1 & d2 & d3 & d4 \\end{array} \\right].$$\nThis function mirrors the CSS `matrix3d` transform function.",
    params: [
      { name: "a1", description: "1st column of 1st row", type: realT() },
      { name: "b1", description: "1st column of 2nd row", type: realT() },
      { name: "c1", description: "1st column of 3rd row", type: realT() },
      { name: "d1", description: "1st column of 4th row", type: realT() },
      { name: "a2", description: "2nd column of 1st row", type: realT() },
      { name: "b2", description: "2nd column of 2nd row", type: realT() },
      { name: "c2", description: "2nd column of 3rd row", type: realT() },
      { name: "d2", description: "2nd column of 4th row", type: realT() },
      { name: "a3", description: "3rd column of 1st row", type: realT() },
      { name: "b3", description: "3rd column of 2nd row", type: realT() },
      { name: "c3", description: "3rd column of 3rd row", type: realT() },
      { name: "d3", description: "3rd column of 4th row", type: realT() },
      { name: "a4", description: "4th column of 1st row", type: realT() },
      { name: "b4", description: "4th column of 2nd row", type: realT() },
      { name: "c4", description: "4th column of 3rd row", type: realT() },
      { name: "d4", description: "4th column of 4th row", type: realT() },
    ],
    body: (
      _context: Context,
      a1: ad.Num,
      b1: ad.Num,
      c1: ad.Num,
      d1: ad.Num,
      a2: ad.Num,
      b2: ad.Num,
      c2: ad.Num,
      d2: ad.Num,
      a3: ad.Num,
      b3: ad.Num,
      c3: ad.Num,
      d3: ad.Num,
      a4: ad.Num,
      b4: ad.Num,
      c4: ad.Num,
      d4: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: matrix3d(
          a1,
          a2,
          a3,
          a4,
          b1,
          b2,
          b3,
          b4,
          c1,
          c2,
          c3,
          c4,
          d1,
          d2,
          d3,
          d4,
        ),
      });
    },
    returns: valueT("RealNM"),
  },

  rotate: {
    name: "rotate",
    description:
      "`rotate(theta, [x], [y])` returns a counter-clockwise 2D rotation by an angle $\\theta$, optionally around the point $(x,y)$.  If no point is specified, the rotation is around the origin.  Since this transformation is in general an affine transformation, it is encoded in homogeneous coordinates via the $3 \\times 3$ matrix\n$$\\left[ \\begin{array}{rrr} \\cos(\\theta) & -\\sin(\\theta) & 0 \\\\ \\sin(\\theta) &  \\cos(\\theta) & 0 \\\\ 0 & 0 & 1 \\end{array} \\right].$$\nFor the $2 \\times 2$ linear version, see `rotate2d()`.)",
    params: [
      {
        name: "theta",
        description: "angle of rotation (in radians)",
        type: realT(),
      },
      {
        name: "x",
        description: "center of rotation (x coordinate)",
        type: realT(),
        default: 0,
      },
      {
        name: "y",
        description: "center of rotation (y coordinate)",
        type: realT(),
        default: 0,
      },
    ],
    body: (
      _context: Context,
      theta: ad.Num,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      const R = toHomogeneousMatrix(rotate2d(theta));
      const T = translate([x, y]);
      const Ti = translate([neg(x), neg(y)]);
      return noWarn({
        tag: "MatrixV",
        contents: ops.mmmul(T, ops.mmmul(R, Ti)),
      });
    },
    returns: valueT("RealNM"),
  },

  rotate2d: {
    name: "rotate2d",
    description:
      "`rotate2d(theta)` returns a 2D rotation around the origin by a given angle $\\theta$, encoded via the $2 \\times 2$ matrix\n$$\\left[ \\begin{array}{rr} \\cos(\\theta) & -\\sin(\\theta) \\\\ \\sin(\\theta) &  \\cos(\\theta) \\end{array} \\right].$$\nThis matrix cannot directly be composed with 2D affine transformations; for the $3 \\times 3$ affine version, see `rotate()`.",
    params: [
      {
        name: "theta",
        description: "angle of rotation (in radians)",
        type: realT(),
      },
    ],
    body: (_context: Context, theta: ad.Num): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: rotate2d(theta),
      });
    },
    returns: valueT("RealNM"),
  },

  rotate3d: {
    name: "rotate3d",
    description:
      "`rotate3d(theta, v)` returns a 3D rotation by an angle $\\theta$ around a unit axis $v$.  The matrix is constructed via Rodrigues' rotation formula\n$$I + \\sin(\\theta)\\hat{v} + (1-\\cos(\\theta))\\hat{v}^2,$$\nwhere $I$ is the $3 \\times 3$ identity matrix, and $\\hat{v}$ is the cross product matrix associated with $v$ (see `crossProductMatrix()`).  This matrix cannot directly be composed with 3D affine transformations; for the $4 \\times 4$ affine version, see `rotate3dh()`.",
    params: [
      {
        name: "theta",
        description: "angle of rotation (in radians)",
        type: realT(),
      },
      {
        name: "v",
        description: "axis of rotation (unit vector)",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      theta: ad.Num,
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: rotate3d(theta, v),
      });
    },
    returns: valueT("RealNM"),
  },

  rotate3dh: {
    name: "rotate3dh",
    description:
      "`rotate3dh(theta, v)` returns a 3D rotation by a given angle $\\theta$ around a unit axis $v$.  This transformation is encoded as a $4 \\times 4$ matrix in homogeneous coordinates, so that it can be composed with 3D affine transformations.  For the $3 \\times 3$ linear version, see `rotate3d()`.",
    params: [
      {
        name: "theta",
        description: "angle of rotation (in radians)",
        type: realT(),
      },
      {
        name: "v",
        description: "axis of rotation (unit vector)",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      theta: ad.Num,
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(rotate3d(theta, v)),
      });
    },
    returns: valueT("RealNM"),
  },

  scale: {
    name: "scale",
    description:
      "`scale(sx, sy)` returns a nonuniform scaling by factors $s_x$, $s_y$ along $x$, $y$ axes, encoded via the matrix\n$$\\left[ \\begin{array}{ccc} s_x & 0 & 0 \\\\ 0 & s_y & 0 \\\\ 0 & 0 & 1 \\end{array} \\right].$$\nThis transformation is encoded as a $3 \\times 3$ matrix in homogeneous coordinates, so that it can be composed with 2D affine transformations.  For the $2 \\times 2$ linear version, see `scale2d()`.",
    params: [
      { name: "sx", description: "horizontal scale factor", type: realT() },
      { name: "sy", description: "vertical scale factor", type: realT() },
    ],
    body: (
      _context: Context,
      sx: ad.Num,
      sy: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(scale2d(sx, sy)),
      });
    },
    returns: valueT("RealNM"),
  },

  scale2d: {
    name: "scale2d",
    description:
      "`scale2d(sx,sy)` returns a $2 \\times 2$ matrix representing nonuniform scaling by factors $s_x$, $s_y$ along $x$, $y$ axes, encoded via the matrix\n$$\\left[ \\begin{array}{cc} s_x & 0 \\\\ 0 & s_y \\end{array} \\right].$$\nThis transformation is encoded as a $2 \\times 2$ matrix that cannot directly be composed with 2D affine transformations.  For the $3 \\times 3$ affine version, see `rotate()`.",
    params: [
      { name: "sx", description: "horizontal scale factor", type: realT() },
      { name: "sy", description: "vertical scale factor", type: realT() },
    ],
    body: (
      _context: Context,
      sx: ad.Num,
      sy: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: scale2d(sx, sy),
      });
    },
    returns: valueT("RealNM"),
  },

  scale3d: {
    name: "scale3d",
    description:
      "`scale3d(sx, sy, sz)` returns a nonuniform scaling by factors $s_x$, $s_y$, $s_z$ along $x$, $y$, $z$ axes, via the matrix\n$$\\left[ \\begin{array}{ccc} s_x & 0 & 0 \\\\ 0 & s_y & 0 \\\\ 0 & 0 & s_z \\end{array} \\right].$$\nThis transformation is encoded as a $3 \\times 3$ matrix that cannot directly be composed with 3D affine transformations.  For the $4 \\times 4$ affine version, see `scale3dh()`.",
    params: [
      { name: "sx", description: "x scale factor", type: realT() },
      { name: "sy", description: "y scale factor", type: realT() },
      { name: "sz", description: "z scale factor", type: realT() },
    ],
    body: (
      _context: Context,
      sx: ad.Num,
      sy: ad.Num,
      sz: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: scale3d(sx, sy, sz),
      });
    },
    returns: valueT("RealNM"),
  },

  scale3dh: {
    name: "scale3dh",
    description:
      "`scale3dh(sx, sy, sz)` returns a $4 \\times 4$ matrix representing nonuniform scaling by factors $s_x$, $s_y$, $s_z$ along $x$, $y$, $z$ axes, via the matrix\n$$\\left[ \\begin{array}{cccc} s_x & 0 & 0 & 0 \\\\ 0 & s_y & 0 & 0 \\\\ 0 & 0 & s_z & 0 \\\\ 0 & 0 & 0 & 1 \\end{array} \\right].$$\nThis transformation is encoded as a $4 \\times 4$ matrix in homogeneous coordinates, so that it can be composed with 3D affine transformations.  For the $3 \\times 3$ linear version, see `scale3D()`.",
    params: [
      { name: "sx", description: "x scale factor", type: realT() },
      { name: "sy", description: "y scale factor", type: realT() },
      { name: "sz", description: "z scale factor", type: realT() },
    ],
    body: (
      _context: Context,
      sx: ad.Num,
      sy: ad.Num,
      sz: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(scale3d(sx, sy, sz)),
      });
    },
    returns: valueT("RealNM"),
  },

  skew: {
    name: "skew",
    description:
      "`skew(a_x, a_y)` takes angles $a_x$ and $a_y$, and returns a 2D skew transformation encoded by the matrix\n$$\\left[ \\begin{array}{ccc} 1 & \\tan(a_x) & 0 \\\\ \\tan(a_y) & 1 & 0 \\\\ 0 & 0 & 1 \\end{array} \\right].$$\nIf $a_y$ is not defined, its default value is 0, resulting in a purely horizontal skewing.  This transformation is encoded as a $3 \\times 3$ matrix in homogeneous coordinates, so that it can be composed with affine transformations.  For the linear version, see `skew2d()`.",
    params: [
      { name: "ax", description: "horizontal angle", type: realT() },
      { name: "ay", description: "vertical angle", type: realT(), default: 0 },
    ],
    body: (
      _context: Context,
      ax: ad.Num,
      ay: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(skew(ax, ay)),
      });
    },
    returns: valueT("RealNM"),
  },

  skew2d: {
    name: "skew2d",
    description:
      "`skew2d(a_x, a_y)` takes angles $a_x$ and $a_y$, and returns a 2D skew transformation encoded via the matrix\n$$\\left[ \\begin{array}{cc} 1 & \\tan(a_x) \\\\ \\tan(a_y) & 1 \\end{array} \\right].$$\nIf $a_y$ is not defined, its default value is 0, resulting in a purely horizontal skewing.  This transformation is encoded as a $2 \\times 2$ matrix that cannot directly be composed with 2D affine transformations.  For the $3 \\times 3$ affine version, see `skew()`.",
    params: [
      { name: "ax", description: "horizontal angle", type: realT() },
      { name: "ay", description: "vertical angle", type: realT(), default: 0 },
    ],
    body: (
      _context: Context,
      ax: ad.Num,
      ay: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: skew(ax, ay),
      });
    },
    returns: valueT("RealNM"),
  },

  shear: {
    name: "shear",
    description:
      "`shear(u,v)` takes two $n$-dimensional vectors $u$ and $v$, and returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$, i.e., that performs the transformation\n$$x \\mapsto x + \\langle v,x \\rangle u.$$\n This transformation is encoded as an $(n+1) \\times (n+1)$ matrix in homogeneous coordinates, so that it can be composed with affine transformations.  For the linear version, see `shear2d()` or `shear3d()`.",
    params: [
      { name: "u", description: "offset direction", type: realNT() },
      { name: "v", description: "shear axis", type: realNT() },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(shear(u, v)),
      });
    },
    returns: valueT("RealNM"),
  },

  shear2d: {
    name: "shear2d",
    description:
      "`shear2d(u,v)` takes two 2-dimensional vectors $u$ and $v$, and returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$, i.e., that performs the transformation\n$$x \\mapsto x + \\langle v,x \\rangle u.$$\n This transformation is encoded as a $2 \\times 2$ matrix that cannot directly be composed with 2-dimensional affine transformations.  For the affine version, see `shear()`.",
    params: [
      { name: "u", description: "offset direction", type: realNT() },
      { name: "v", description: "shear axis", type: realNT() },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      if (u.length !== 2) {
        throw new Error("Expected a 2D vector u");
      }
      if (v.length !== 2) {
        throw new Error("Expected a 2D vector v");
      }
      return noWarn({
        tag: "MatrixV",
        contents: shear(u, v),
      });
    },
    returns: valueT("RealNM"),
  },

  shear3d: {
    name: "shear3d",
    description:
      "`shear3d(u,v)` takes two 3-dimensional vectors $u$ and $v$, returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$, i.e., that performs the transformation\n$$x \\mapsto x + \\langle v,x \\rangle u.$$\n This transformation is encoded as a $3 \\times 3$ matrix that cannot directly be composed with 3-dimensional affine transformations.  For the affine version, see `shear()`.",
    params: [
      { name: "u", description: "offset direction", type: realNT() },
      { name: "v", description: "shear axis", type: realNT() },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      if (u.length !== 3) {
        throw new Error("Expected a 3D vector u");
      }
      if (v.length !== 3) {
        throw new Error("Expected a 3D vector v");
      }
      return noWarn({
        tag: "MatrixV",
        contents: shear(u, v),
      });
    },
    returns: valueT("RealNM"),
  },

  translate: {
    name: "translate",
    description:
      "`translate(x,y)` returns a translation by the given offset $(x,y)$.  If $y$ is not specified, it is assumed to be $0$.  Since translation is affine rather than linear, it is encoded as a $3 \\times 3$ matrix in homogeneous coordinates, namely\n$$T = \\left[ \\begin{array}{ccc} 1 & 0 & x \\\\ 0 & 1 & y \\\\ 0 & 0 & 1 \\end{array} \\right].$$",
    params: [
      { name: "x", description: "horizontal offset", type: realT() },
      { name: "y", description: "vertical offset", type: realT(), default: 0 },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: translate([x, y]),
      });
    },
    returns: valueT("RealNM"),
  },

  translate3dh: {
    name: "translate3dh",
    description:
      "`translate3dh(x,y,z)` returns a translation by $(x,y,z)$.  Since translation is affine rather than linear, it is encoded as a $4 \\times 4$ matrix in homogeneous coordinates, namely\n$$T = \\left[ \\begin{array}{cccc} 1 & 0 & 0 & x \\\\ 0 & 1 & 0 & y \\\\ 0 & 0 & 0 & z \\\\ 0 & 0 & 0 & 1 \\end{array} \\right].$$",
    params: [
      { name: "x", description: "x offset", type: realT() },
      { name: "y", description: "y offset", type: realT() },
      { name: "z", description: "z offset", type: realT() },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
      z: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: translate([x, y, z]),
      });
    },
    returns: valueT("RealNM"),
  },

  lookAt: {
    name: "lookAt",
    description:
      "`lookAt(eye, center, up)` returns a $4 \\times 4$ viewing matrix derived from an eye point $e$, a reference point $c$ indicating the center of the scene, and an up vector $u$.  The matrix maps the reference point to the negative z axis and the eye point to the origin. When a typical projection matrix is used, the center of the scene therefore maps to the center of the viewport. Similarly, the direction described by the up vector projected onto the viewing plane is mapped to the positive y axis so that it points upward in the viewport. The up vector must not be parallel to the line of sight from the eye point to the reference point.  The matrix is given explicitly by $V = MT$ where $T$ is a translation by $-e$ (a la `translate3dh`), and $M$ is given by\n$$M = \\left[ \\begin{array}{rrrc} s_1 & s_2 & s_3 & 0 \\\\ v_1 & v_2 & v_3 & 0 \\\\ -f_1 & -f_2 & -f_3 & 0 \\\\ 0 & 0 & 0 & 1 \\end{array} \\right],$$\nwhere $f := (c-e)/|c-e|$ is the unit vector from the eye to the center, $s = f \\times u$ is a vector pointing to the side, and $v = s \\times f$ is a vector in roughly the same direction as $u$ that completes an orthonormal basis with $s$ and $f$ (hence, $M$ is a rotation matrix).",
    params: [
      { name: "eye", description: "position of the eye point", type: realNT() },
      {
        name: "center",
        description: "position of the reference point",
        type: realNT(),
      },
      {
        name: "up",
        description: "unit vector in the upward direction",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      eye: ad.Num[],
      center: ad.Num[],
      up: ad.Num[],
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: lookAt(eye, center, up),
      });
    },
    returns: valueT("RealNM"),
  },

  perspective: {
    name: "perspective",
    description:
      "`perspective(fovy, aspect, [zNear], [zFar])` returns a $4 \\times 4$ perspective projection matrix, given a vertical field of view $\\theta_y$, an aspect ratio $a$, and optional near/far $z$-values, $z_0$ and $z_1$.  The aspect ratio should match the aspect ratio of the associated viewport.  For example, an aspect ratio of 2 means the viewer's angle of view is twice as wide in $x$ as it is in $y$.  Coordinates within $[z_0,z_1]$ will get mapped to depth values in the range $[0,1]$.  Letting $f := \\cot(\\theta_y/2)$, the perspective matrix is given by\n$$P = \\left[ \\begin{array}{cccc} f/a & 0 & 0 & 0 \\\\ 0 & f & 0 & 0 \\\\ 0 & 0 & \\frac{z_0+z_1}{z_0-z_1} & \\frac{2 z_0 z_1}{z_0 - z_1} \\\\ 0 & 0 & -1 & 0 \\end{array} \\right].$$",
    params: [
      {
        name: "fovy",
        description: "field of view angle, in degrees, in the y direction",
        type: realT(),
      },
      {
        name: "aspect",
        description:
          "aspect ratio that determines the field of view in the x direction, equal to the ratio of x (width) to y (height)",
        type: realT(),
      },
      {
        name: "zNear",
        description:
          "distance from the viewer to the near clipping plane (always positive), with a default value of 0.1",
        type: realT(),
        default: 0.1,
      },
      {
        name: "zFar",
        description:
          "distance from the viewer to the far clipping plane (always positive), with a default value of 100.0",
        type: realT(),
        default: 100.0,
      },
    ],
    body: (
      _context: Context,
      fovy: ad.Num,
      aspect: ad.Num,
      zNear: ad.Num,
      zFar: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: perspective(fovy, aspect, zNear, zFar),
      });
    },
    returns: valueT("RealNM"),
  },

  ortho: {
    name: "ortho",
    description:
      "`ortho(left, right, bottom, top, [zNear], [zFar])` returns a $4 \\times 4$ transformation that produces a parallel projection, given four horizontal/vertical clipping planes, and an optional near/far clipping plane.  Letting $l$, $r$, $b$, and $t$ denote the left/right/bottom/top clipping planes, respectively, the projection matrix is given by\n$$P = \\left[ \\begin{array}{cccc} \\frac{2}{r-l} & 0 & 0 & -u_x \\\\ 0 & \\frac{2}{t-b} & 0 & -u_y \\\\ 0 & 0 & \\frac{-2}{z_1-z_0} & -u_z \\\\ 0 & 0 & 0 & 1 \\\\ \\end{array} \\right],$$\nwhere\n$$\n\\begin{array}{rcl} u_x &=& (r + l) / (r - l), \\\\ u_y &=& (t + b) / (t - b), \\\\ u_z &=& (z_1 + z_0) / (z_1 - z_0).  \\end{array}$$",
    params: [
      {
        name: "Left",
        description: "coordinate of the left vertical clipping plane",
        type: realT(),
      },
      {
        name: "Right",
        description: "coordinate of the right vertical clipping plane",
        type: realT(),
      },
      {
        name: "Bottom",
        description: "coordinate of the bottom horizontal clipping plane",
        type: realT(),
      },
      {
        name: "Top",
        description: "coordinate of the top horizontal clipping plane",
        type: realT(),
      },
      {
        name: "zNear",
        description:
          "distance to the nearer depth clipping plane (negative if the plane is behind the viewer), with a default value of 0.1",
        type: realT(),
        default: 0.1,
      },
      {
        name: "zFar",
        description:
          "distance to the farther depth clipping plane (negative if the plane is behind the viewer), with a default value of 100",
        type: realT(),
        default: 100.0,
      },
    ],
    body: (
      _context: Context,
      Left: ad.Num,
      Right: ad.Num,
      Bottom: ad.Num,
      Top: ad.Num,
      zNear: ad.Num,
      zFar: ad.Num,
    ): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: ortho(Left, Right, Bottom, Top, zNear, zFar),
      });
    },
    returns: valueT("RealNM"),
  },

  project: {
    name: "project",
    description:
      "`project(p, model, proj, view)` transforms the specified 3D object coordinates $p$ into 2D window coordinates $q$ using a given $4 \\times 4$ model transformation $M$, $4 \\times 4$ projection transformation $P$, and viewport $V = [ \\begin{array}{cccc} x & y & w & h \\end{array} ]$, where $x$, $y$ give the lower-left corner of the canvas, and $w$, $h$ are its width and height.  To also recover the depth, see `projectDepth()`.  The projection is performed by first computing $r := PM\\hat{p}$, where $\\hat{p} := (p,1)$ are the homogeneous coordinates for $p$.  The first two components of $q := r/r_w$ (where $r_w$ is the final component of $r$) gives coordinates in the range $[-1,1] \\times [-1,1]$, which are then stretched to the range $[x,x+w] \\times [y,y+h]$.",
    params: [
      {
        name: "p",
        description: "3D object coordinates (x,y,z)",
        type: realNT(),
      },
      { name: "model", description: "4x4 modelview matrix", type: realNMT() },
      { name: "proj", description: "4x4 projection matrix", type: realNMT() },
      {
        name: "view",
        description: "viewport (x, y, width, height)",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      model: ad.Num[][],
      proj: ad.Num[][],
      view: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const q: ad.Num[] = project(p, model, proj, view);
      return noWarn({
        tag: "VectorV",
        contents: [q[0], q[1]],
      });
    },
    returns: valueT("RealN"),
  },

  projectDepth: {
    name: "projectDepth",
    description:
      "`projectDepth(p, model, proj, view)` transforms the specified 3D object coordinates $p$ into 2D window coordinates $q$ using a given $4 \\times 4$ model transformation $M$, $4 \\times 4$ projection transformation $P$, and viewport $V = [ \\begin{array}{cccc} x & y & w & h \\end{array} ]$, where $x$, $y$ give the lower-left corner of the canvas, and $w$, $h$ are its width and height.  It returns the projected coordinates, as well as the depth relative to the view.  See `project()` for further information.",
    params: [
      {
        name: "p",
        description: "3D object coordinates (x,y,z)",
        type: realNT(),
      },
      { name: "model", description: "4x4 modelview matrix", type: realNMT() },
      { name: "proj", description: "4x4 projection matrix", type: realNMT() },
      {
        name: "view",
        description: "viewport (x, y, width, height)",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      model: ad.Num[][],
      proj: ad.Num[][],
      view: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: project(p, model, proj, view),
      });
    },
    returns: valueT("RealN"),
  },

  projectList: {
    name: "projectList",
    description:
      "`projectList(P, model, proj, view)` transforms a list of 3D object coordinates $P = p_1, \\ldots, p_k$ into window 2D coordinates using a given $4 \\times 4$ model transformation $M$, $4 \\times 4$ projection transformation $P$, and viewport $V = [ \\begin{array}{cccc} x & y & w & h \\end{array} ]$, where $x$, $y$ give the lower-left corner of the canvas, and $w$, $h$ are its width and height.  Returns the list of projected 2D coordinates $q_1, \\ldots, q_k$.  See `project()` for further information.",
    params: [
      {
        name: "p",
        description: "list of 3D object coordinates (x,y,z)",
        type: realNMT(),
      },
      { name: "model", description: "4x4 modelview matrix", type: realNMT() },
      { name: "proj", description: "4x4 projection matrix", type: realNMT() },
      {
        name: "view",
        description: "viewport (x, y, width, height)",
        type: realNT(),
      },
    ],
    body: (
      _context: Context,
      p: ad.Num[][],
      model: ad.Num[][],
      proj: ad.Num[][],
      view: ad.Num[],
    ): MayWarn<PtListV<ad.Num>> => {
      const q: ad.Num[][] = [];
      for (let i = 0; i < p.length; i++) {
        q[i] = project(p[i], model, proj, view).slice(0, 2);
      }

      return noWarn({
        tag: "PtListV",
        contents: q.map(toPt),
      });
    },
    returns: valueT("Real2N"),
  },

  matrixMultiplyList: {
    name: "matrixMultiplyList",
    description:
      "`matrixMultiplyList(A, V)` multiplies each vector $v_1, \\ldots, v_k$ in the list $V$ by the given $n \\times n$ matrix $A$, returning the list of products.  List elements must all be vectors of length $n$.",
    params: [
      { name: "A", description: "`n`x`n` matrix", type: realNMT() },
      {
        name: "V",
        description: "list of `n`-dimensional vectors",
        type: realNMT(),
      },
    ],
    body: (
      _context: Context,
      A: ad.Num[][],
      V: ad.Num[][],
    ): MayWarn<MatrixV<ad.Num>> => {
      const AV: ad.Num[][] = [];
      for (let i = 0; i < V.length; i++) {
        AV[i] = ops.mvmul(A, V[i]);
      }

      return noWarn({
        tag: "MatrixV",
        contents: AV,
      });
    },
    returns: valueT("RealNM"),
  },

  fromHomogeneous: {
    name: "fromHomogeneous",
    description:
      "`fromHomogeneous(q)` takes a vector $q$ of length $n+1$, encoding a point in $n$-dimensional homogeneous coordinates, and returns a vector $p$ of length $n$, encoding the same point in Cartesian coordinates, i.e,. $p = (q_1, \\ldots, q_{k-1})/q_k$.",
    params: [
      { name: "q", description: "homogeneous coordinates", type: realNT() },
    ],
    body: (_context: Context, q: ad.Num[]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: fromHomogeneous(q),
      });
    },
    returns: valueT("RealN"),
  },

  fromHomogeneousList: {
    name: "fromHomogeneousList",
    description:
      "`fromHomogeneousList(Q)` takes a list $Q = q_1, \\ldots, q_k$ of vectors of length $n+1$, encoding points in $n$-dimensional homogeneous coordinates, and returns a list $P = p_1, \\ldots, p_k$ of vectors of length $n$, encoding the same points in Cartesian coordinates.",
    params: [
      {
        name: "Q",
        description: "list of points in homogeneous coordinates",
        type: realNMT(),
      },
    ],
    body: (_context: Context, Q: ad.Num[][]): MayWarn<MatrixV<ad.Num>> => {
      const P: ad.Num[][] = [];
      for (let i = 0; i < Q.length; i++) {
        P[i] = fromHomogeneous(Q[i]);
      }
      return noWarn({
        tag: "MatrixV",
        contents: P,
      });
    },
    returns: valueT("RealNM"),
  },

  toHomogeneous: {
    name: "toHomogeneous",
    description:
      "`toHomogeneous(p)` takes a vector $p$ of length $n$, encoding a point in $n$-dimensional Cartesian coordinates, and returns a vector $q$ of length $n+1$, encoding the same point in homogeneous coordinates, i.e., $q = (p,1)$.",
    params: [
      { name: "p", description: "Cartesian coordinates", type: realNT() },
    ],
    body: (_context: Context, p: ad.Num[]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: toHomogeneous(p),
      });
    },
    returns: valueT("RealN"),
  },

  toHomogeneousList: {
    name: "toHomogeneousList",
    description:
      "`toHomogeneousList(P)` takes a list $P = p_1, \\ldots, p_k$ of vectors of length $n$, encoding points in $n$-dimensional Cartesian coordinates, and returns a list $Q = q_1, \\ldots, q_k$ of vectors of length $n+1$, encoding the same points in homogeneous coordinates.",
    params: [
      {
        name: "P",
        description: "list of points in Cartesian coordinates",
        type: realNMT(),
      },
    ],
    body: (_context: Context, P: ad.Num[][]): MayWarn<MatrixV<ad.Num>> => {
      const Q: ad.Num[][] = [];
      for (let i = 0; i < P.length; i++) {
        Q[i] = toHomogeneous(P[i]);
      }
      return noWarn({
        tag: "MatrixV",
        contents: Q,
      });
    },
    returns: valueT("RealNM"),
  },

  toHomogeneousMatrix: {
    name: "toHomogeneousMatrix",
    description:
      "`toHomogeneousMatrix(A)` takes a square $n \\times n$ matrix $A$ representing a spatial transformation in $A$ dimensions, and returns an $(n+1) \\times (n+1)$ matrix representing the same transformation in homogeneous coordinates.",
    params: [
      {
        name: "A",
        description: "matrix encoding linear transformation",
        type: realNMT(),
      },
    ],
    body: (_context: Context, A: ad.Num[][]): MayWarn<MatrixV<ad.Num>> => {
      return noWarn({
        tag: "MatrixV",
        contents: toHomogeneousMatrix(A),
      });
    },
    returns: valueT("RealNM"),
  },

  /* ======== END MATRIX FUNCTIONS ======== */

  /**
   * Return the length of the line or arrow shape `[type, props]`.
   */
  length: {
    name: "length",
    description: "Return the length of the Line shape.",
    params: [{ name: "l", description: "A line", type: shapeT("Line") }],
    body: (_context: Context, shape: Line<ad.Num>): MayWarn<FloatV<ad.Num>> => {
      const [p1, p2] = linePts(shape);
      return noWarn({
        tag: "FloatV",
        contents: ops.vdist(p1, p2),
      });
    },
    returns: valueT("Real"),
  },
  /**
   * Return the normalized version of vector `v`.
   */
  normalize: {
    name: "normalize",
    description: "Return the normalized version of vector `v`.",
    params: [{ type: realNT(), name: "v", description: "Vector `v`" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: ops.vnormalize(v),
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  pathFromPoints: {
    name: "pathFromPoints",
    description:
      "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",
    params: [
      { name: "pathType", type: pathTypeT(), description: "Path Type" },
      { name: "pts", type: real2NT(), description: "List of points" },
    ],
    body: (
      _context: Context,
      pathType: string,
      pts: ad.Pt2[],
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      const [start, ...tailpts] = pts;
      path.moveTo(start);
      tailpts.forEach((pt: ad.Pt2) => path.lineTo(pt));
      if (pathType === "closed") path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  quadraticCurveFromPoints: {
    name: "quadraticCurveFromPoints",
    description:
      "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",
    params: [
      { name: "pathType", type: pathTypeT(), description: "Path Type" },
      { name: "pts", type: real2NT(), description: "List of points" },
    ],
    body: (
      _context: Context,
      pathType: string,
      pts: ad.Pt2[],
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      const [start, cp, second, ...tailpts] = pts;
      path.moveTo(start);
      path.quadraticCurveTo(cp, second);
      tailpts.forEach((pt: ad.Pt2) => path.quadraticCurveJoin(pt));
      if (pathType === "closed") path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Draw a curve interpolating three given points.
   * (Note that this is different from specifying the
   * three control points of a quadratic Bézier curve,
   * since a Bézier does not interpolate the middle
   * control point.)
   */
  interpolateQuadraticFromPoints: {
    name: "interpolateQuadraticFromPoints",
    description: `Draw a curve interpolating three given points.
    (Note that this is different from specifying the three control points of a quadratic Bézier curve, since a Bézier does not interpolate the middle control point.)`,
    params: [
      { name: "pathType", type: pathTypeT(), description: "Path Type" },
      { name: "p0", type: real2T(), description: "First point" },
      { name: "p1", type: real2T(), description: "Second point" },
      { name: "p2", type: real2T(), description: "Third point" },
    ],
    body: (
      _context: Context,
      pathType: string,
      p0: ad.Pt2,
      p1: ad.Pt2,
      p2: ad.Pt2,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      path.moveTo(p0);
      // Compute the control point location q1 such that the
      // quadratic curve interpolates the midpoint p1, namely,
      //    q1 = 2 p1 - (p0+p2)/2
      // (This expression can be derived by expressing the
      // interpolation condition in terms of the quadratic
      // Bernstein basis.)
      const q1 = ops.vsub(ops.vmul(2.0, p1), ops.vmul(0.5, ops.vadd(p0, p2)));
      if (!ad.isPt2(q1)) {
        // XXX kludge to force TypeScript to know that q1 has length 2; see GitHub issue #715
        throw new Error("vector ops did not preserve dimension");
      }
      path.quadraticCurveTo(q1, p2);
      if (pathType === "closed") path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  cubicCurveFromPoints: {
    name: "cubicCurveFromPoints",
    description:
      "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",

    params: [
      { type: pathTypeT(), name: "pathType", description: "Path type" },
      { type: real2NT(), name: "pts", description: "List of points" },
    ],
    body: (
      _context: Context,
      pathType: string,
      pts: ad.Pt2[],
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      const [start, cp1, cp2, second, ...tailpts] = pts;
      path.moveTo(start);
      path.bezierCurveTo(cp1, cp2, second);
      _.chunk(tailpts, 2).forEach(([cp, pt]) => path.cubicCurveJoin(cp, pt));
      if (pathType === "closed") path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  connectPaths: {
    name: "connectPaths",
    description:
      "Given a list of `PathData`s, returns a `PathData` representing the union of these paths with " +
      "lines connecting the start and end points.",

    params: [
      { type: pathTypeT(), name: "pathType", description: "Path type" },
      {
        type: pathDataListT(),
        name: "pathDataList",
        description: "List of path data",
      },
    ],
    body: (
      _context: Context,
      pathType: string,
      pathDataList: PathCmd<ad.Num>[][],
    ): MayWarn<PathDataV<ad.Num>> => {
      const connect = (startCmd: PathCmd<ad.Num>): PathCmd<ad.Num> => {
        return {
          cmd: "L",
          contents: startCmd.contents,
        };
      };
      const resPathData = PathBuilder.concatPaths(
        pathDataList,
        connect,
        pathType === "closed",
      );
      return noWarn(pathDataV(resPathData));
    },
    returns: valueT("PathCmd"),
  },

  concatenatePaths: {
    name: "concatenatePaths",
    description:
      "Given a list of `PathData`s, return the union of these paths.",

    params: [
      {
        type: pathDataListT(),
        name: "pathDataList",
        description: "List of path data",
      },
    ],
    body: (
      _context: Context,
      pathDataList: PathCmd<ad.Num>[][],
    ): MayWarn<PathDataV<ad.Num>> => {
      const resPathData = PathBuilder.concatPaths(pathDataList);
      return noWarn(pathDataV(resPathData));
    },
    returns: valueT("PathCmd"),
  },

  joinPaths: {
    name: "joinPaths",
    description:
      "Given a list of `PathData`s, join them into one SVG path. For correct results, the end points and start points" +
      "of each path must already coincide.",

    params: [
      {
        type: pathDataListT(),
        name: "pathDataList",
        description: "List of path data",
      },
    ],
    body: (
      _context: Context,
      pathDataList: PathCmd<ad.Num>[][],
    ): MayWarn<PathDataV<ad.Num>> => {
      const connect = () => null;
      const resPathData = PathBuilder.concatPaths(pathDataList, connect);
      return noWarn(pathDataV(resPathData));
    },
    returns: valueT("PathCmd"),
  },

  Penrose: {
    name: "Penrose",
    description: `Return path data describing an "impossible polygon."`,
    params: [
      {
        name: "center",
        type: realNT(),
        description: "(x,y) translation",
        default: [0, 0],
      },
      {
        name: "radius",
        type: realT(),
        description: "radius of outer polygon (must be positive)",
        default: 50,
      },
      {
        name: "holeSize",
        type: realT(),
        description:
          "radius of inner polygon as a fraction of the outer radius (in range (0,1])",
        default: 0.35,
      },
      {
        name: "angle",
        type: realT(),
        description: "angle of rotation",
        default: 0,
      },
      {
        name: "nSides",
        type: posIntT(),
        description: "number of sides (integer ≥ 3)",
        default: 5,
      },
      {
        name: "chirality",
        type: stringT(),
        description: 'either "cw" for clockwise, or "ccw" for counterclockwise',
        default: "ccw",
      },
    ],
    body: (
      _context: Context,
      center: ad.Num[],
      radius: ad.Num,
      holeSize: ad.Num,
      angle: ad.Num,
      nSides: number,
      chirality: string,
    ): MayWarn<PathDataV<ad.Num>> => {
      const n = Math.floor(Math.max(nSides, 3));
      const R = radius; // shorthand for outer radius
      const r = mul(holeSize, R); // inner radius
      const alpha = mul(Math.PI, div(sub(n, 2), n)); // interior angle of regular n-gon
      const w = div(sub(R, r), mul(4, cos(div(alpha, 2)))); // half-width
      const s = chirality === "cw" ? 1 : -1;

      // inner and outer polygons
      const a = [];
      const b = [];
      for (let k = 0; k < n; k++) {
        const theta = add(angle, (2 * k * Math.PI) / n);
        const p = [mul(s, sin(theta)), cos(theta)];
        a[k] = ops.vadd(center, ops.vmul(r, p));
        b[k] = ops.vadd(center, ops.vmul(R, p));
      }

      // unit edge vectors
      const u = [];
      for (let i = 0; i < n; i++) {
        const j = (i + 1) % n;
        u[i] = ops.vnormalize(ops.vsub(a[j], a[i]));
      }

      // inner and outer midpoints
      const c = [];
      const d = [];
      for (let i = 0; i < n; i++) {
        const l = (i - 1 + n) % n;
        c[i] = ops.vsub(a[i], ops.vmul(w, u[i]));
        d[i] = ops.vsub(b[i], ops.vmul(w, u[l]));
      }

      // add polygons to path
      const path = new PathBuilder();
      for (let i = 0; i < n; i++) {
        const j = (i + 1) % n;
        const k = (i + 2) % n;

        // XXX We could add another parameter that offsets the individual
        // XXX polygons, but with the current AD design this results in a
        // XXX huge number of AD variables, causing us to hit the `local
        // XXX count too large` issuse with wasm compilation.  Skipping
        // XXX for now.
        // const h = mul( 2.0, s ); // offset size, times sign
        // const q = offsetPolygon( [ d[i], b[i], d[j], c[k], a[k], c[j] ], h );

        const q = [d[i], b[i], d[j], c[k], a[k], c[j]];

        path.moveTo([q[0][0], q[0][1]]);
        path.lineTo([q[1][0], q[1][1]]);
        path.lineTo([q[2][0], q[2][1]]);
        path.lineTo([q[3][0], q[3][1]]);
        path.lineTo([q[4][0], q[4][1]]);
        path.lineTo([q[5][0], q[5][1]]);
        path.closePath();
      }

      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  firstPoint: {
    name: "firstPoint",
    description: "Returns the first point in a list.",
    params: [
      { name: "points", type: realNMT(), description: "list of points" },
    ],
    body: (_context: Context, points: ad.Num[][]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: points[0],
      });
    },
    returns: valueT("Real2"),
  },

  lastPoint: {
    name: "lastPoint",
    description: "Returns the last point in a list.",
    params: [
      { name: "points", type: realNMT(), description: "list of points" },
    ],
    body: (_context: Context, points: ad.Num[][]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: points[points.length - 1],
      });
    },
    returns: valueT("Real2"),
  },

  averagePoint: {
    name: "averagePoint",
    description: "Returns the average (mean) of all points in a list.",
    params: [
      { name: "points", type: realNMT(), description: "list of points" },
    ],
    body: (_context: Context, points: ad.Num[][]): MayWarn<VectorV<ad.Num>> => {
      const sum = sumVectors(points);
      const mean = ops.vdiv(sum, points.length);
      return noWarn({
        tag: "VectorV",
        contents: mean,
      });
    },
    returns: valueT("RealN"),
  },

  interpolatingSpline: {
    name: "interpolatingSpline",
    description:
      "Returns path data for a curve that smoothly interpolates the given points.  Interpolation is performed via a Catmull-Rom spline.",
    params: [
      {
        name: "pathType",
        type: pathTypeT(),
        description: `either "open" or "closed."`,
      },
      {
        name: "points",
        type: realNMT(),
        description: "points to be interpolated",
      },
      {
        name: "tension",
        type: realT(),
        description: "smoothness of curve (0=piecewise linear, .25=default)",
        default: 0.25,
      },
    ],
    body: (
      _context: Context,
      pathType: string,
      points: ad.Num[][],
      tension: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      return noWarn(catmullRom(_context, pathType, points, tension));
    },
    returns: valueT("PathCmd"),
  },

  diffusionProcess: {
    name: "diffusionProcess",
    description:
      "Return `n` points sampled from a diffusion process starting at `X0`, with covariance matrix `A` and constant drift `omega`.  This path approximately integrates the stochastic differential equation dX_t = omega dt + A dW_t, where W_t is a Wiener process.",
    params: [
      { name: "n", type: posIntT(), description: "number of points" },
      { name: "X0", type: real2T(), description: "starting location" },
      { name: "A", type: realNMT(), description: "covariance matrix" },
      { name: "omega", type: real2T(), description: "drift direction" },
    ],
    body: (
      _context: Context,
      n: number,
      X0: ad.Num[],
      A: ad.Num[][],
      omega: ad.Num[],
    ): MayWarn<PtListV<ad.Num>> => {
      const Xt = diffusionProcess(_context, n, X0, A, omega);
      return noWarn({
        tag: "PtListV",
        contents: Xt.map(toPt),
      });
    },
    returns: valueT("Real2N"),
  },

  /**
   * Return two points parallel to line `s1` using its normal line `s2`.
   */
  unitMark: {
    name: "unitMark",
    description:
      "Return two points parallel to line `s1` using its normal line `s2`.",
    params: [
      { name: "s1", type: shapeT("Line") },
      { name: "s2", type: shapeT("Line") },
      { name: "padding", type: realT() },
    ],
    body: (
      _context: Context,
      s1: Line<ad.Num>,
      s2: Line<ad.Num>,
      padding: ad.Num,
    ): MayWarn<PtListV<ad.Num>> => {
      const [start1, end1] = linePts(s1);
      const [start2, end2] = linePts(s2);

      const dir = ops.vnormalize(ops.vsub(end2, start2));
      const normalDir = ops.vneg(dir);
      const markStart = ops.vmove(start1, padding, normalDir);
      const markEnd = ops.vmove(end1, padding, normalDir);

      return noWarn({
        tag: "PtListV",
        contents: [markStart, markEnd].map(toPt),
      });
    },
    returns: valueT("Real2N"),
  },

  /**
   * Return two points to "cap off" the line made in `unitMark`.
   */
  unitMark2: {
    name: "unitMark2",
    description: 'Return two points to "cap off" the line made in `unitMark`.',
    params: [
      { name: "[start, end]", type: real2NT() },
      { name: "t", type: stringT() },
      { name: "size", type: realT() },
    ],
    body: (
      _context: Context,
      [start, end]: [ad.Pt2, ad.Pt2],
      t: string,
      size: ad.Num,
    ): MayWarn<PtListV<ad.Num>> => {
      const dir = ops.vnormalize(ops.vsub(end, start));
      const normalDir = ops.rot90(toPt(dir));
      const base = t === "start" ? start : end;
      const [markStart, markEnd] = [
        ops.vmove(base, size, normalDir),
        ops.vmove(base, neg(size), normalDir),
      ];
      return noWarn({
        tag: "PtListV",
        contents: [markStart, markEnd].map(toPt),
      });
    },
    returns: valueT("Real2N"),
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
  arc: {
    name: "arc",
    description: `Return series of elements that can render an arc SVG. See: https://css-tricks.com/svg-path-syntax-illustrated-guide/ for the "A" spec. Returns elements that can be passed to Path shape spec to render an SVG arc.`,
    params: [
      {
        name: "pathType",
        type: pathTypeT(),
        description: `The path type: either "open" or "closed." whether the SVG should automatically draw a line between the final point and the start point`,
      },
      {
        name: "start",
        type: real2T(),
        description: "coordinate to start drawing the arc",
      },
      {
        name: "end",
        type: real2T(),
        description: "coordinate to finish drawing the arc",
      },
      {
        name: "[width, height]",
        type: real2T(),
        description: "width and height of the ellipse to draw the arc along",
      },
      {
        name: "rotation",
        type: realT(),
        description: "angle in degrees to rotate ellipse about its center",
      },
      {
        name: "largeArc",
        type: realT(),
        description: "0 to draw shorter of 2 arcs, 1 to draw longer",
      },
      {
        name: "arcSweep",
        type: realT(),
        description: "0 to rotate CCW, 1 to rotate CW",
      },
    ],
    body: (
      _context: Context,
      pathType: string,
      start: ad.Pt2,
      end: ad.Pt2,
      radius: ad.Pt2,
      rotation: ad.Num,
      largeArc: ad.Num,
      arcSweep: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      path.moveTo(start).arcTo(radius, end, [rotation, largeArc, arcSweep]);
      if (pathType === "closed") path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  circularArc: {
    name: "circularArc",
    description: `Return path data that describes a circular arc.  The arc is equivalent to the parametric curve center + r*(cos(t),sin(t)) for t in the range [theta0,theta1].  More general arcs (e.g., along an ellipse) can be drawn using arc().`,
    params: [
      {
        name: "pathType",
        type: pathTypeT(),
        description: `The path type: either "open" or "closed." whether the SVG should automatically draw a line between the final point and the start point`,
      },
      {
        name: "center",
        type: real2T(),
        description: "circle center",
      },
      {
        name: "r",
        type: realT(),
        description: "circle radius",
      },
      {
        name: "theta0",
        type: realT(),
        description: "start angle in radians",
      },
      {
        name: "theta1",
        type: realT(),
        description: "end angle in radians",
      },
    ],
    body: (
      _context: Context,
      pathType: string,
      center: ad.Pt2,
      r: ad.Num,
      theta0: ad.Num,
      theta1: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      //path.moveTo(start).arcTo(radius, end, [rotation, largeArc, arcSweep]);
      const u0 = [mul(r, cos(theta0)), mul(r, sin(theta0))];
      const u1 = [mul(r, cos(theta1)), mul(r, sin(theta1))];
      const x0 = toPt(ops.vadd(center, u0));
      const x1 = toPt(ops.vadd(center, u1));
      const largeArc = ifCond(gt(absVal(sub(theta1, theta0)), Math.PI), 1, 0);
      const arcSweep = ifCond(gt(theta0, theta1), 1, 0);
      path.moveTo(x0).arcTo([r, r], x1, [0, largeArc, arcSweep]);
      if (pathType === "closed") {
        path.lineTo(center);
        path.closePath();
      }
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  repeatedArcs: {
    name: "repeatedArcs",
    description:
      "Generate multiple concentric arcs. Useful for denoting equal angles.",
    params: [
      {
        name: "innerStart",
        type: real2T(),
        description: "coordinate to start drawing the inner arc",
      },
      {
        name: "innerEnd",
        type: real2T(),
        description: "coordinate to end the inner arc",
      },
      {
        name: "outerStart",
        type: real2T(),
        description: "coordinate to start drawing the outer arc",
      },
      {
        name: "outerEnd",
        type: real2T(),
        description: "coordinate to end the outer arc",
      },
      {
        name: "innerRadius",
        type: real2T(),
        description:
          "radii of the ellipse to draw the inner arc along (width, height)",
      },
      {
        name: "repeat",
        type: posIntT(),
        description: "number of times to repeat the arc",
      },
      {
        name: "spacing",
        type: realT(),
        description: "spacing between arcs",
      },
      {
        name: "arcSweep",
        type: realT(),
        description: "arc length to sweep",
      },
    ],
    body: (
      _context: Context,
      innerStart: ad.Pt2,
      innerEnd: ad.Pt2,
      outerStart: ad.Pt2,
      outerEnd: ad.Pt2,
      innerRadius: ad.Pt2,
      repeat: number,
      spacing: ad.Num,
      arcSweep: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      const startDir = ops.vnormalize(ops.vsub(outerStart, innerStart));
      const endDir = ops.vnormalize(ops.vsub(outerEnd, innerEnd));
      let start: ad.Pt2 = innerStart;
      let end: ad.Pt2 = innerEnd;
      let radius = innerRadius;
      for (let i = 0; i < repeat; i++) {
        path.moveTo(start).arcTo(radius, end, [0, 0, arcSweep]);
        // TODO: avoid casting to `ad.Pt2`
        start = ops.vmove(start, spacing, startDir) as ad.Pt2;
        end = ops.vmove(end, spacing, endDir) as ad.Pt2;
        radius = ops.vadd(radius, [spacing, spacing]) as ad.Pt2;
      }
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Return series of elements that render a "wedge", which is the same as the arc above except that it's connected to the circle center and filled
   * @param center: center of the circle on which the arc sits
   * @param start: coordinate to start drawing the arc
   * @param end: coordinate to finish drawing the arc
   * @param radius: width and height of the ellipse to draw the arc along (i.e. [width, height])
   * @param rotation: angle in degrees to rotate ellipse about its center
   * @param largeArc: 0 to draw shorter of 2 arcs, 1 to draw longer
   * @param arcSweep: 0 to rotate CCW, 1 to rotate CW
   * @returns: Elements that can be passed to Path shape spec to render an SVG arc
   */
  wedge: {
    name: "wedge",
    description: `Return series of elements that render a "wedge", which is the same as the arc above except that it's connected to the circle center and filled. Returns elements that can be passed to Path shape spec to render an SVG arc.`,
    params: [
      {
        name: "center",
        type: real2T(),
        description: "center of the circle on which the arc sits",
      },
      {
        name: "start",
        type: real2T(),
        description: "coordinate to start drawing the arc",
      },
      {
        name: "end",
        type: real2T(),
        description: "coordinate to finish drawing the arc",
      },
      {
        name: "radius",
        type: real2T(),
        description:
          "width and height of the ellipse to draw the arc along (i.e. [width, height])",
      },
      {
        name: "rotation",
        type: realT(),
        description: "angle in degrees to rotate ellipse about its center",
      },
      {
        name: "largeArc",
        type: realT(),
        description: "0 to draw shorter of 2 arcs, 1 to draw longer",
      },
      {
        name: "arcSweep",
        type: realT(),
        description: "0 to rotate CCW, 1 to rotate CW",
      },
    ],
    body: (
      _context: Context,
      center: ad.Pt2,
      start: ad.Pt2,
      end: ad.Pt2,
      radius: ad.Pt2,
      rotation: ad.Num,
      largeArc: ad.Num,
      arcSweep: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      path
        .moveTo(start)
        .arcTo(radius, end, [rotation, largeArc, arcSweep])
        .lineTo(center);
      path.closePath();
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },
  /**
   * Find the point that is located at dist r along a line between p1 and p2.
   * @param p1: start point of line segment
   * @param p2: endpoint of line segment
   * @param r: distance from p1 to travel along the line
   * @returns: vector representation of the point of intersection
   */
  ptOnLine: {
    name: "ptOnLine",
    description:
      "Find the point that is located at dist r along a line between p1 and p2. Returns vector representation of the point of intersection.",
    params: [
      {
        name: "p1",
        type: realNT(),
        description: "start point of line segment",
      },
      { name: "p2", type: realNT(), description: "endpoint of line segment" },
      {
        name: "r",
        type: realT(),
        description: "distance from p1 to travel along the line",
      },
    ],
    body: (
      _context: Context,
      p1: ad.Num[],
      p2: ad.Num[],
      r: ad.Num,
    ): MayWarn<VectorV<ad.Num>> => {
      // find unit vector pointing towards v2
      const unit = ops.vnormalize(ops.vsub(p2, p1));
      return noWarn({ tag: "VectorV", contents: ops.vmove(p1, r, unit) });
    },
    returns: valueT("RealN"),
  },
  /**
   * Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.
   * @param x1, y1: x, y coordinates of the circle/ellipse that the arc is drawn on
   * @param start: start point of the arc
   * @param end: end point of the arc
   * @returns: 0 or 1 depending on CCW or CW rotation
   */
  arcSweepFlag: {
    name: "arcSweepFlag",
    description:
      "Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.",

    params: [
      {
        name: "[x1, y1]",
        type: real2T(),
        description:
          "x, y coordinates of the circle/ellipse that the arc is drawn on",
      },
      { name: "start", type: real2T(), description: "start point of the arc" },
      { name: "end", type: real2T(), description: "end point of the arc" },
    ],
    body: (
      _context: Context,
      [x1, y1]: ad.Num[],
      start: ad.Pt2,
      end: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> => {
      const st = ops.vnormalize([sub(start[0], x1), sub(start[1], y1)]);
      const en = ops.vnormalize([sub(end[0], x1), sub(end[1], y1)]);
      const cross = ops.cross2(st, en);
      return noWarn({
        tag: "FloatV",
        contents: ifCond(gt(cross, 0), 0, 1),
      });
    },
    returns: valueT("Real"),
  },
  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: {
    name: "angleBetween",
    description:
      "Return the unsigned angle between vectors `u, v`, in radians. Assumes that both u and v have nonzero magnitude. The returned value will be in the range [0,pi].",
    params: [
      { name: "u", type: realNT(), description: "A vector" },
      { name: "v", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      const theta = ops.angleBetween(u, v);
      return noWarn({
        tag: "FloatV",
        contents: theta,
      });
    },
    returns: valueT("Real"),
  },
  /**
   * Return the signed angle from vector `u` to vector `v`, in radians.
   * Assumes that both u and v are 2D vectors and have nonzero magnitude.
   * The returned value will be in the range [-pi,pi].
   */
  angleFrom: {
    name: "angleFrom",
    description:
      "Return the signed angle from vector `u` to vector `v`, in radians. Assumes that both u and v are 2D vectors and have nonzero magnitude. The returned value will be in the range [-pi,pi].",
    params: [
      { name: "u", type: realNT(), description: "A vector" },
      { name: "v", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      const theta = ops.angleFrom(u, v);
      return noWarn({
        tag: "FloatV",
        contents: theta,
      });
    },
    returns: valueT("Real"),
  },
  /**
   * Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]
   */
  cross2D: {
    name: "cross2D",
    description:
      "Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]",
    params: [
      { name: "u", type: real2T(), description: "A vector" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      const det = sub(mul(u[0], v[1]), mul(u[1], v[0]));
      return noWarn({
        tag: "FloatV",
        contents: det,
      });
    },
    returns: valueT("Real"),
  },
  /**
   * Return the 3D cross product of `u` and `v`.
   */
  cross: {
    name: "cross",
    description: "Return the 3D cross product of 3D vectors `u` and `v`.",
    params: [
      { name: "u", type: real3T(), description: "A vector" },
      { name: "v", type: real3T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      u: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const result = ops.cross3(u, v);
      return noWarn({
        tag: "VectorV",
        contents: result,
      });
    },
    returns: real3T(),
  },
  /**
   * Return the intersection of a line passing through
   * `a0` and `a1` with a line passing through `b0` and `b1`
   */
  lineLineIntersection: {
    name: "lineLineIntersection",
    description:
      "Return the intersection of a line passing through `a0` and `a1` with a line passing through `b0` and `b1`.",
    params: [
      { name: "a0", type: real2T(), description: "First point of first line" },
      { name: "a1", type: real2T(), description: "Second point of first line" },
      { name: "b0", type: real2T(), description: "First point of second line" },
      {
        name: "b1",
        type: real2T(),
        description: "Second point of second line",
      },
    ],
    body: (
      _context: Context,
      a0: ad.Num[],
      a1: ad.Num[],
      b0: ad.Num[],
      b1: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const A0 = [a0[0], a0[1], 1];
      const A1 = [a1[0], a1[1], 1];
      const B0 = [b0[0], b0[1], 1];
      const B1 = [b1[0], b1[1], 1];
      const X = ops.cross3(ops.cross3(A0, A1), ops.cross3(B0, B1));
      const x = [div(X[0], X[2]), div(X[1], X[2])];
      return noWarn({
        tag: "VectorV",
        contents: toPt(x),
      });
    },
    returns: valueT("Real2"),
  },
  /**
   * Return a point located at the midpoint between pts `start` and `end`
   */
  midpoint: {
    name: "midpoint",
    description:
      "Return a point located at the midpoint between pts `start` and `end`",
    params: [
      { name: "start", type: realNT(), description: "First point" },
      { name: "end", type: realNT(), description: "Second point" },
    ],
    body: (
      _context: Context,
      start: ad.Num[],
      end: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const midpointLoc = ops.vmul(0.5, ops.vadd(start, end));
      return noWarn({
        tag: "VectorV",
        contents: midpointLoc,
      });
    },
    returns: valueT("RealN"),
  },
  /**
   * Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).
   */
  midpointOffset: {
    name: "midpointOffset",
    description:
      "Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).",
    params: [
      { name: "s1", type: shapeT("Line"), description: "A line" },
      {
        name: "padding",
        type: realT(),
        description: "Padding between midpoint and label",
      },
    ],
    body: (
      _context: Context,
      s1: Line<ad.Num>,
      padding: ad.Num,
    ): MayWarn<TupV<ad.Num>> => {
      const [start, end] = linePts(s1);
      // TODO: Cache these operations in Style!
      const normalDir = ops.rot90(ops.vnormalize(ops.vsub(end, start)));
      const midpointLoc = ops.vmul(0.5, ops.vadd(start, end));
      const midpointOffsetLoc = ops.vmove(midpointLoc, padding, normalDir);
      return noWarn({
        tag: "TupV",
        contents: toPt(midpointOffsetLoc),
      });
    },
    returns: valueT("Real2"),
  },
  chevron: {
    name: "chevron",
    description:
      "Return a list of points for a chevron shape comprised of two line segments intersecting at a right angle at the midpoint of `s1`, which can then be passed to `pathFromPoints` to draw the chevron.",
    params: [
      { name: "s1", type: shapeT("Line"), description: "A line" },
      {
        name: "padding",
        type: realT(),
        description: "Length of each line segment",
      },
    ],
    body: (
      _context: Context,
      // TODO reimplement with variable tick marks when #629 is merged
      s1: Line<ad.Num>,
      padding: ad.Num,
    ): MayWarn<PtListV<ad.Num>> => {
      // tickPlacement(padding, ticks);
      const [start, end] = linePts(s1);
      const dir = ops.vnormalize(ops.vsub(end, start)); // TODO make direction face "positive direction"
      const startDir = ops.vrot(dir, 135);
      const endDir = ops.vrot(dir, 225);
      const center = ops.vmul(0.5, ops.vadd(start, end));
      // if even, evenly divide tick marks about center. if odd, start in center and move outwards
      return noWarn({
        tag: "PtListV",
        contents: [
          ops.vmove(center, padding, startDir),
          center,
          ops.vmove(center, padding, endDir),
        ].map(toPt),
      });
    },
    returns: valueT("Real2N"),
  },
  /**
   * Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).
   */
  innerPointOffset: {
    name: "innerPointOffset",
    description:
      "Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).",

    params: [
      { name: "pt1", type: real2T(), description: "First point" },
      { name: "pt2", type: real2T(), description: "Second point" },
      { name: "pt3", type: real2T(), description: "Third point" },
      {
        name: "padding",
        type: realT(),
        description: "Offset from line to returned point",
      },
    ],
    body: (
      _context: Context,
      pt1: ad.Num[],
      pt2: ad.Num[],
      pt3: ad.Num[],
      padding: ad.Num,
    ): MayWarn<VectorV<ad.Num>> => {
      // unit vector towards first corner
      const vec1unit = ops.vnormalize(ops.vsub(pt2, pt1));
      const normalDir = ops.vneg(ops.rot90(vec1unit)); // rot90 rotates CW, neg to point in CCW direction

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
      const cond = gt(ops.vdot(vec1unit, intoEndUnit), 0.95);
      return noWarn({
        tag: "VectorV",
        contents: [ifCond(cond, xp, xn), ifCond(cond, yp, yn)],
      });
    },
    returns: valueT("Real2"),
  },
  /**
   * Create equally spaced tick marks centered at the midpoint of a line
   * @param pt1: starting point of a line
   * @param pt2: endping point of a line
   * @param spacing: space in px between each tick
   * @param numTicks: number of tick marks to create
   * @param tickLength: 1/2 length of each tick
   */
  ticksOnLine: {
    name: "ticksOnLine",
    description:
      "Create equally spaced tick marks centered at the midpoint of a line",
    params: [
      { name: "pt1", type: real2T(), description: "starting point of a line" },
      { name: "pt2", type: real2T(), description: "ending point of a line" },
      {
        name: "spacing",
        type: realT(),
        description: "space in px between each tick",
      },
      {
        name: "numTicks",
        type: posIntT(),
        description: "number of tick marks to create",
      },
      {
        name: "tickLength",
        type: realT(),
        description: "1/2 length of each tick",
      },
    ],
    body: (
      _context: Context,
      pt1: ad.Num[],
      pt2: ad.Num[],
      spacing: ad.Num,
      numTicks: ad.Num,
      tickLength: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      // calculate scalar multipliers to determine the placement of each tick mark
      const multipliers = tickPlacement(spacing, numOf(numTicks));
      const unit = ops.vnormalize(ops.vsub(pt2, pt1));
      const normalDir = ops.vneg(ops.rot90(unit)); // rot90 rotates CW, neg to point in CCW direction

      const mid = ops.vmul(0.5, ops.vadd(pt1, pt2));

      // start/end pts of each tick will be placed parallel to each other, offset at dist of tickLength
      // from the original pt1->pt2 line
      const [x1p, y1p] = ops.vmove(mid, tickLength, normalDir);
      const [x2p, y2p] = ops.vmove(mid, tickLength, ops.vneg(normalDir));

      multipliers.forEach((multiplier) => {
        const [sx, sy] = ops.vmove([x1p, y1p], multiplier, unit);
        const [ex, ey] = ops.vmove([x2p, y2p], multiplier, unit);
        path.moveTo([sx, sy]).lineTo([ex, ey]);
      });
      return noWarn(path.getPath());
    },
    returns: valueT("PathCmd"),
  },
  /**
   * Given two orthogonal segments that intersect at `intersection`, and a size `len`
   * return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.
   */
  orientedSquare: {
    name: "orientedSquare",
    description:
      "Given two orthogonal segments that intersect at `intersection`, and a size `len` return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.",

    params: [
      { name: "s1", type: shapeT("Line"), description: "First line segment" },
      { name: "s2", type: shapeT("Line"), description: "Second line segment" },
      {
        name: "intersection",
        type: real2T(),
        description: "Point of intersection",
      },
      {
        name: "len",
        type: realT(),
        description: "Side length of square marker",
      },
    ],
    body: (
      _context: Context,
      s1: Line<ad.Num>,
      s2: Line<ad.Num>,
      intersection: ad.Pt2,
      len: ad.Num,
    ): MayWarn<PathDataV<ad.Num>> => {
      const [seg1, seg2] = [linePts(s1), linePts(s2)];
      const [ptL, ptLR, ptR] = perpPathFlat(len, seg1, seg2);
      const path = new PathBuilder();
      return noWarn(
        path
          .moveTo(toPt(ptL))
          .lineTo(toPt(ptLR))
          .lineTo(toPt(ptR))
          .lineTo(intersection)
          .closePath()
          .getPath(),
      );
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).
   */
  triangle: {
    name: "triangle",
    description:
      "Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).",
    params: [
      { name: "l1", type: shapeT("Line"), description: "First line" },
      { name: "l2", type: shapeT("Line"), description: "Second line" },
      { name: "l3", type: shapeT("Line"), description: "Third line" },
    ],
    body: (
      _context: Context,
      l1: Line<ad.Num>,
      l2: Line<ad.Num>,
      l3: Line<ad.Num>,
    ): MayWarn<PathDataV<ad.Num>> => {
      const path = new PathBuilder();
      return noWarn(
        path
          .moveTo(toPt(getStart(l1)))
          .lineTo(toPt(getStart(l2)))
          .lineTo(toPt(getStart(l3)))
          .closePath()
          .getPath(),
      );
    },
    returns: valueT("PathCmd"),
  },

  /**
   * Return the average of floats `x` and `y`.
   */
  average2: {
    name: "average2",
    description: "Return the average of floats `x` and `y`.",
    params: [
      { name: "x", type: realT(), description: "`x`" },
      { name: "y", type: realT(), description: "`y`" },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: div(add(x, y), 2),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the average of the floats in the list `xs`.
   */
  average: {
    name: "average",
    description: "Return the average of the floats in the list `xs`.",
    params: [{ name: "xs", type: realNT(), description: "`xs`" }],
    body: (_context: Context, xs: ad.Num[]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: div(addN(xs), max(1, xs.length)),
        // To avoid divide-by-0
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the normalized version of vector `v`.
   */
  unit: {
    name: "unit",
    description: "Return the normalized version of vector `v`.",
    params: [{ name: "v", type: realNT(), description: "`v`" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<VectorV<ad.Num>> => {
      return noWarn({
        tag: "VectorV",
        contents: ops.vnormalize(v),
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Return a uniform random value between minVal and maxValue.
   */
  random: {
    name: "random",
    description:
      "Uniformly sample a random value in the range from `minVal` to `maxVal`.",
    params: [
      { name: "minVal", type: realT(), description: "minimum value" },
      { name: "maxVal", type: realT(), description: "maximum value" },
    ],
    body: (
      { makeInput }: Context,
      minVal: ad.Num,
      maxVal: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      if (typeof minVal === "number" && typeof maxVal === "number") {
        const val = makeInput({
          init: { tag: "Sampled", sampler: uniform(minVal, maxVal) },
          stages: new Set(),
        });

        return noWarn({
          tag: "FloatV",
          contents: val,
        });
      } else {
        throw new Error(
          "Expects the minimum and maximum values to be constants. Got a computed or optimized value instead.",
        );
      }
    },
    returns: valueT("Real"),
  },

  /**
   * Return a uniform random value between 0 and 1
   */
  unitRandom: {
    name: "unitRandom",
    description: "Uniformly sample a random value in the range [0,1).",
    params: [],
    body: ({ makeInput }: Context): MayWarn<FloatV<ad.Num>> => {
      const val = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });

      return noWarn({
        tag: "FloatV",
        contents: val,
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return a random value sampled from the uniform distribution on the unit disk.
   */
  diskRandom: {
    name: "diskRandom",
    description: "Sample the uniform distribution on the unit disk.",
    params: [],
    body: ({ makeInput }: Context): MayWarn<VectorV<ad.Num>> => {
      const u1 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });
      const u2 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });

      // From the section "Sampling the Unit Disk" in Arvo, "Stratified Sampling of 2-Manifolds" (2001)
      const x = [
        mul(sqrt(u1), cos(mul(2 * Math.PI, u2))),
        mul(sqrt(u1), sin(mul(2 * Math.PI, u2))),
      ];

      return noWarn({
        tag: "VectorV",
        contents: x,
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Return a random value sampled from the uniform distribution on the unit circle.
   */
  circleRandom: {
    name: "circleRandom",
    description: "Sample the uniform distribution on the unit circle.",
    params: [],
    body: ({ makeInput }: Context): MayWarn<VectorV<ad.Num>> => {
      const u = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 2 * Math.PI) },
        stages: new Set(),
      });

      const x = [cos(u), sin(u)];

      return noWarn({
        tag: "VectorV",
        contents: x,
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Return a random value sampled from the uniform distribution on the unit sphere.
   */
  sphereRandom: {
    name: "sphereRandom",
    description: "Sample the uniform distribution on the unit sphere.",
    params: [],
    body: ({ makeInput }: Context): MayWarn<VectorV<ad.Num>> => {
      const u1 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });
      const u2 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });

      // Adapted from the section "Sampling the Unit Hemisphere" in Arvo, "Stratified Sampling of 2-Manifolds" (2001)
      const z = sub(1, mul(2, u1));
      const r = sqrt(clamp([0, 1], sub(1, mul(z, z))));
      const phi = mul(2 * Math.PI, u2);
      const x = [mul(r, cos(phi)), mul(r, sin(phi)), z];

      return noWarn({
        tag: "VectorV",
        contents: x,
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Return a random value sampled from a normal distribution with mean 0 and standard deviation 1.
   */
  normalRandom: {
    name: "normalRandom",
    description:
      "Sample a normal distribution with mean 0 and standard deviation 1.",
    params: [],
    body: ({ makeInput }: Context): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: randn({ makeInput }),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return a random point sampled from the uniform distribution on a triangle with vertices a, b, c.
   */
  triangleRandom: {
    name: "triangleRandom",
    description:
      "Sample a point from the uniform distribution over a triangle with vertices `a`, `b`, and `c`.",
    params: [
      { name: "a", type: real2T(), description: "First vertex" },
      { name: "b", type: real2T(), description: "Second vertex" },
      { name: "c", type: real2T(), description: "Third vertex" },
    ],
    body: (
      { makeInput }: Context,
      a: ad.Num[],
      b: ad.Num[],
      c: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const u1 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });
      const u2 = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 1) },
        stages: new Set(),
      });

      // Following method SamplePlanarTriangle from Arvo, "Stratified Sampling of 2-Manifolds" (2001)
      const s = sqrt(u1);
      const t = u2;
      const x = ops.vadd(
        ops.vadd(ops.vmul(sub(1, s), a), ops.vmul(mul(s, sub(1, t)), b)),
        ops.vmul(mul(s, t), c),
      );

      return noWarn({
        tag: "VectorV",
        contents: x,
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Sample a random color once, with opacity `alpha` and colorType `colorType` (`"rgb"` or `"hsv"`).
   */
  sampleColor: {
    name: "sampleColor",
    description:
      'Sample a random color once, with opacity `alpha` and color type `colorType` (`"rgb"` or `"hsv"`).',
    params: [
      { name: "alpha", type: unitT(), description: "Opacity" },
      { name: "colorType", type: colorTypeT(), description: "Color model" },
    ],
    body: (
      { makeInput }: Context,
      alpha: ad.Num,
      colorType: "rgb" | "hsv",
    ): MayWarn<ColorV<ad.Num>> => {
      if (colorType === "rgb") {
        const rgb = _.range(3).map(() =>
          makeInput({
            init: { tag: "Sampled", sampler: uniform(0.1, 0.9) },
            stages: new Set(),
          }),
        );

        return noWarn({
          tag: "ColorV",
          contents: {
            tag: "RGBA",
            contents: [rgb[0], rgb[1], rgb[2], alpha],
          },
        });
      } else {
        const h = makeInput({
          init: { tag: "Sampled", sampler: uniform(0, 360) },
          stages: new Set(),
        });
        return noWarn({
          tag: "ColorV",
          contents: {
            tag: "HSVA",
            contents: [h, 100, 80, alpha], // HACK: for the color to look good
          },
        });
      }
    },
    returns: valueT("Color"),
  },

  /**
   * Return a uniform index value between minIndex and maxIndex.
   */
  randomIndex: {
    name: "randomIndex",
    description:
      "Uniformly sample a random integer value in the range from `minIndex` to `maxIndex`.",
    params: [
      { name: "minIndex", type: realT(), description: "minimum index" },
      { name: "maxIndex", type: realT(), description: "maximum index" },
    ],
    body: (
      { makeInput }: Context,
      minIndex: ad.Num,
      maxIndex: ad.Num,
    ): MayWarn<FloatV<number>> => {
      if (typeof minIndex === "number" && typeof maxIndex === "number") {
        const randomFloat = makeInput({
          init: { tag: "Sampled", sampler: uniform(minIndex, maxIndex) },
          stages: new Set(),
        });
        const randomInt = Math.floor(randomFloat.val);
        return noWarn({ tag: "FloatV", contents: randomInt });
      } else {
        throw new Error(
          "Expects the minimum and maximum indices to be constants. Got a computed or optimized value instead.",
        );
      }
    },
    returns: valueT("PosInt"),
  },

  /**
   * Set the opacity of a color `color` to `frac`.
   */
  setOpacity: {
    name: "setOpacity",
    description: "Set the opacity of a color `color` to `frac`.",
    params: [
      { name: "color", type: colorT(), description: "Color" },
      { name: "frac", type: unitT(), description: "Opacity" },
    ],
    body: (
      _context: Context,
      color: Color<ad.Num>,
      frac: ad.Num,
    ): MayWarn<ColorV<ad.Num>> => {
      // If paint=none, opacity is irreelevant
      if (color.tag === "NONE") {
        return noWarn({
          tag: "ColorV",
          contents: color,
        });
        // Otherwise, retain tag and color; only modify opacity
      } else {
        const props = color.contents;
        return noWarn({
          tag: "ColorV",
          contents: {
            tag: color.tag,
            contents: [props[0], props[1], props[2], mul(frac, props[3])],
          },
        });
      }
    },
    returns: valueT("Color"),
  },

  /**
   * Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).
   */
  mul: {
    name: "mul",
    description:
      "Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).",
    params: [
      { name: "m", type: realNMT(), description: "A matrix" },
      { name: "v", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      m: ad.Num[][],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      if (!m.length) {
        throw Error("empty matrix");
      }
      if (!v.length) {
        throw Error("empty vector");
      }

      return noWarn({
        tag: "VectorV",
        contents: m.map((row) => ops.vdot(row, v)),
      });
    },
    returns: valueT("RealN"),
  },

  // ------ Triangle centers

  /**
   * Return the barycenter of the triangle with vertices `a`, `b`, `c`.
   */
  barycenter: {
    name: "barycenter",
    description:
      "Return the barycenter of the triangle with vertices `a`, `b`, `c`.",

    params: [
      { name: "a", type: real2T(), description: "First vertex" },
      { name: "b", type: real2T(), description: "Second vertex" },
      { name: "c", type: real2T(), description: "Third vertex" },
    ],
    body: (
      _context: Context,
      a: ad.Num[],
      b: ad.Num[],
      c: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      const x = ops.vmul(1 / 3, ops.vadd(a, ops.vadd(b, c)));
      return noWarn({
        tag: "VectorV",
        contents: toPt(x),
      });
    },
    returns: valueT("Real2"),
  },

  /**
   * Return the circumcenter of the triangle with vertices `p`, `q`, `r`.
   */
  circumcenter: {
    name: "circumcenter",
    description:
      "Return the circumcenter of the triangle with vertices `p`, `q`, `r`.",

    params: [
      { name: "p", type: real2T(), description: "First vertex" },
      { name: "q", type: real2T(), description: "Second vertex" },
      { name: "r", type: real2T(), description: "Third vertex" },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      q: ad.Num[],
      r: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      // edge vectors
      const u = ops.vsub(r, q);
      const v = ops.vsub(p, r);
      const w = ops.vsub(q, p);

      // side lengths
      const a = ops.vnorm(u);
      const b = ops.vnorm(v);
      const c = ops.vnorm(w);

      // homogeneous barycentric coordinates for circumcenter
      const hp = neg(mul(div(a, mul(b, c)), ops.vdot(w, v)));
      const hq = neg(mul(div(b, mul(c, a)), ops.vdot(u, w)));
      const hr = neg(mul(div(c, mul(a, b)), ops.vdot(v, u)));

      // normalize to get barycentric coordinates for circumcenter
      const H = add(add(hp, hq), hr);
      const bp = div(hp, H);
      const bq = div(hq, H);
      const br = div(hr, H);

      // circumcenter
      const x = ops.vadd(
        ops.vadd(ops.vmul(bp, p), ops.vmul(bq, q)),
        ops.vmul(br, r),
      );

      return noWarn({
        tag: "VectorV",
        contents: toPt(x),
      });
    },
    returns: valueT("Real2"),
  },

  /**
   * Return the circumradius of the triangle with vertices `p`, `q`, `r`.
   */
  circumradius: {
    name: "circumradius",
    description:
      "Return the circumradius of the triangle with vertices `p`, `q`, `r`.",
    params: [
      { name: "p", type: real2T(), description: "First vertex" },
      { name: "q", type: real2T(), description: "Second vertex" },
      { name: "r", type: real2T(), description: "Third vertex" },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      q: ad.Num[],
      r: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      // side lengths
      const a = ops.vnorm(ops.vsub(r, q));
      const b = ops.vnorm(ops.vsub(p, r));
      const c = ops.vnorm(ops.vsub(q, p));

      // semiperimeter
      const s = mul(0.5, add(add(a, b), c));

      // circumradius, computed as
      // R = (abc)/(4 sqrt( s(a+b-s)(a+c-s)(b+c-s) ) )
      const R = div(
        mul(mul(a, b), c),
        mul(
          4,
          sqrt(
            mul(
              mul(mul(s, sub(add(a, b), s)), sub(add(a, c), s)),
              sub(add(b, c), s),
            ),
          ),
        ),
      );

      return noWarn({
        tag: "FloatV",
        contents: R,
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the incenter of the triangle with vertices `p`, `q`, `r`.
   */
  incenter: {
    name: "incenter",
    description:
      "Return the incenter of the triangle with vertices `p`, `q`, `r`.",

    params: [
      { name: "p", type: real2T(), description: "First vertex" },
      { name: "q", type: real2T(), description: "Second vertex" },
      { name: "r", type: real2T(), description: "Third vertex" },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      q: ad.Num[],
      r: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      // side lengths
      const a = ops.vnorm(ops.vsub(r, q));
      const b = ops.vnorm(ops.vsub(p, r));
      const c = ops.vnorm(ops.vsub(q, p));

      // barycentric coordinates for incenter
      const s = add(add(a, b), c);
      const bp = div(a, s);
      const bq = div(b, s);
      const br = div(c, s);

      // incenter
      const x = ops.vadd(
        ops.vadd(ops.vmul(bp, p), ops.vmul(bq, q)),
        ops.vmul(br, r),
      );

      return noWarn({
        tag: "VectorV",
        contents: toPt(x),
      });
    },
    returns: valueT("Real2"),
  },

  /**
   * Return the inradius of the triangle with vertices `p`, `q`, `r`.
   */
  inradius: {
    name: "inradius",
    description:
      "Return the inradius of the triangle with vertices `p`, `q`, `r`.",

    params: [
      { name: "p", type: real2T(), description: "First vertex" },
      { name: "q", type: real2T(), description: "Second vertex" },
      { name: "r", type: real2T(), description: "Third vertex" },
    ],
    body: (
      _context: Context,
      p: ad.Num[],
      q: ad.Num[],
      r: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      // side lengths
      const a = ops.vnorm(ops.vsub(r, q));
      const b = ops.vnorm(ops.vsub(p, r));
      const c = ops.vnorm(ops.vsub(q, p));

      // semiperimeter
      const s = mul(0.5, add(add(a, b), c));

      // inradius
      const R = sqrt(div(mul(mul(sub(s, a), sub(s, b)), sub(s, c)), s));

      return noWarn({
        tag: "FloatV",
        contents: R,
      });
    },
    returns: valueT("Real"),
  },

  // ------ Utility functions

  /**
   * Return the square of the number `x`.
   */
  sqr: {
    name: "sqr",
    description: "Return the square of the number `x`.",
    params: [{ name: "x", type: realT(), description: "`x`" }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: squared(x) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the square root of the number `x`. (NOTE: if `x < 0`, you may get `NaN`s)
   */
  sqrt: {
    name: "sqrt",
    description:
      "Return the square root of number `x`. (Note: if `x < 0` you may get `NaN`s)",
    params: [{ name: "x", type: realT(), description: "`x`" }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: sqrt(x) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the max of the numbers `x`, `y`.
   */
  max: {
    name: "max",
    description: "Return the max of the numbers `x`, `y`.",
    params: [
      { name: "x", type: realT(), description: "`x`" },
      { name: "y", type: realT(), description: "`y`" },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: max(x, y) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the min of the numbers `x`, `y`.
   */
  min: {
    name: "min",
    description: "Return the min of the numbers `x`, `y`.",
    params: [
      { name: "x", type: realT(), description: "`x`" },
      { name: "y", type: realT(), description: "`y`" },
    ],
    body: (
      _context: Context,
      x: ad.Num,
      y: ad.Num,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: min(x, y) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the absolute value of the number `x`.
   */
  abs: {
    name: "abs",
    description: "Return the absolute value of the number `x`.",
    params: [{ name: "x", type: realT(), description: "`x`" }],
    body: (_context: Context, x: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: absVal(x) });
    },
    returns: valueT("Real"),
  },

  /**
   * Convert the angle `theta` from degrees to radians.
   */
  toRadians: {
    name: "toRadians",
    description: "Convert the angle `theta` from degrees to radians.",
    params: [{ name: "theta", type: realT(), description: "`theta`" }],
    body: (_context: Context, theta: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: mul(Math.PI / 180, theta),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Convert the angle `theta` from radians to degrees.
   */
  toDegrees: {
    name: "toDegrees",
    description: "Convert the angle `theta` from radians to degrees.",
    params: [{ name: "theta", type: realT(), description: "`theta`" }],
    body: (_context: Context, theta: ad.Num): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: mul(180 / Math.PI, theta),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the Euclidean norm of the vector `v`.
   */
  norm: {
    name: "norm",
    description: "Return the Euclidean norm of the vector `v`.",
    params: [{ name: "v", type: realNT(), description: "A vector" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: ops.vnorm(v) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the Euclidean norm squared of the vector `v`.
   */
  normsq: {
    name: "normsq",
    description: "Return the Euclidean norm squared of the vector `v`.",
    params: [{ name: "v", type: realNT(), description: "A vector" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: ops.vnormsq(v) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the Euclidean distance between the vectors `v` and `w`.
   */
  vdist: {
    name: "vdist",
    description:
      "Return the Euclidean distance between the vectors `v` and `w`.",
    params: [
      { name: "v", type: realNT(), description: "A vector" },
      { name: "w", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      v: ad.Num[],
      w: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: ops.vdist(v, w) });
    },
    returns: valueT("Real"),
  },

  vmul: {
    name: "vmul",
    description: "Returns the scalar-vector product.",
    params: [
      { name: "s", type: realT(), description: "A scalar" },
      { name: "v", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      s: ad.Num,
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      return noWarn({ tag: "VectorV", contents: ops.vmul(s, v) });
    },
    returns: valueT("RealN"),
  },

  /**
   * Return the Euclidean distance squared between the vectors `v` and `w`.
   */
  vdistsq: {
    name: "vdistsq",
    description:
      "Return the Euclidean distance squared between the vectors `v` and `w`.",
    params: [
      { name: "v", type: realNT(), description: "A vector" },
      { name: "w", type: realNT(), description: "A vector" },
    ],
    body: (
      _context: Context,
      v: ad.Num[],
      w: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: ops.vdistsq(v, w) });
    },
    returns: valueT("Real"),
  },

  /**
   * Return the angle made by the vector `v` with the positive x-axis.
   */
  angleOf: {
    name: "angleOf",
    description:
      "Return the angle made by the vector `v` with the positive x-axis.",
    params: [{ name: "v", type: realNT(), description: "A vector" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: atan2(v[1], v[0]) });
    },
    returns: valueT("Real"),
  },

  // ------ Mathematical constants

  /**
   * Base e of the natural logarithm.
   */
  MathE: {
    name: "MathE",
    description: "Base e of the natural logarithm.",
    params: [],
    body: (_context: Context): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: Math.E,
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Ratio of the circumference of a circle to its diameter.
   */
  MathPI: {
    name: "MathPI",
    description: "Ratio of the circumference of a circle to its diameter.",
    params: [],
    body: (_context: Context): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: Math.PI,
      });
    },
    returns: valueT("Real"),
  },

  // ------ Geometry/graphics utils

  /**
   * Rotate a 2D vector `v` by 90 degrees counterclockwise.
   */
  rot90: {
    name: "rot90",
    description: "Rotate a 2D vector `v` by 90 degrees counterclockwise.",
    params: [{ name: "v", type: real2T(), description: "A vector" }],
    body: (_context: Context, v: ad.Num[]): MayWarn<VectorV<ad.Num>> => {
      if (v.length !== 2) {
        throw Error("expected 2D vector in `rot90`");
      }
      const [x, y] = v;
      return noWarn({ tag: "VectorV", contents: [neg(y), x] });
    },
    returns: valueT("Real2"),
  },

  /**
   * Rotate a 2D vector `v` by theta degrees counterclockwise.
   */
  rotateBy: {
    name: "rotateBy",
    description: "Rotate a 2D vector `v` by theta degrees counterclockwise.",
    params: [
      { name: "v", type: real2T(), description: "A vector" },
      {
        name: "theta",
        type: realT(),
        description: "degrees to rotate counterclockwise",
      },
    ],
    body: (
      _context: Context,
      v: ad.Num[],
      theta: ad.Num,
    ): MayWarn<VectorV<ad.Num>> => {
      if (v.length !== 2) {
        throw Error("expected 2D vector in `rotateBy`");
      }
      const [x, y] = v;
      const X = add(mul(cos(theta), x), mul(sin(theta), y));
      const Y = add(neg(mul(sin(theta), x)), mul(cos(theta), y));
      return noWarn({ tag: "VectorV", contents: [X, Y] });
    },
    returns: valueT("Real2"),
  },

  //#region signed distance Style functions
  signedDistance: {
    name: "signedDistance",
    description: "Return the signed distance between a shape and a point",
    params: [
      {
        name: "s",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      shape: Shape<ad.Num>,
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> => noWarn(floatV(signedDistance(shape, pt))),
    returns: valueT("Real"),
  },

  signedDistanceRect: {
    name: "signedDistanceRect",
    description: "Returns the distance between a rect and a point",
    params: [
      { name: "rect", type: real2NT() },
      { name: "pt", type: real2T() },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> => noWarn(floatV(signedDistanceRect(rect, pt))),
    returns: realT(),
  },

  signedDistanceCircle: {
    name: "signedDistanceCircle",
    description: "Returns the distance between a circle and a point",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      r: ad.Num,
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistanceCircle(c, r, pt))),
    returns: realT(),
  },

  signedDistancePolygon: {
    name: "signedDistancePolygon",
    description: "Returns the distance between a polygon and a point",
    params: [
      { name: "pts", type: real2NT(), description: "points of the polygon" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistancePolygon(pts, pt))),
    returns: realT(),
  },

  signedDistanceEllipse: {
    name: "signedDistanceEllipse",
    description: "Returns the distance between an ellipse and a point",
    params: [
      { name: "c", type: real2T(), description: "center of ellipse" },
      {
        name: "rx",
        type: realT(),
        description: "horizontal radius of ellipse",
      },
      { name: "ry", type: realT(), description: "vertical radius of ellipse" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      rx: ad.Num,
      ry: ad.Num,
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistanceEllipse(c, rx, ry, pt))),
    returns: realT(),
  },

  signedDistanceLine: {
    name: "signedDistanceLine",
    description: "Returns the distance between a line and a point",
    params: [
      { name: "start", type: real2T(), description: "start of line" },
      { name: "end", type: real2T(), description: "end of line" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      start: ad.Pt2,
      end: ad.Pt2,
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistanceLine(start, end, pt))),
    returns: realT(),
  },

  signedDistancePolyline: {
    name: "signedDistancePolyline",
    description: "Returns the distance between a line and a polyline",
    params: [
      { name: "pts", type: real2NT(), description: "points of the polyline" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistancePolyline(pts, pt))),
    returns: realT(),
  },

  signedDistanceGroup: {
    name: "signedDistanceGroup",
    description:
      "Returns the signed distance between a group of shapes and a point",
    params: [
      { name: "shapes", type: shapeListT() },
      { name: "pt", type: real2T() },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      pt: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(signedDistanceGroup(shapes, pt))),
    returns: realT(),
  },
  //#endregion

  /**
   * Construct a unit vector u in the direction of the
   * given angle theta (in radians).
   */
  unitVector: {
    name: "unitVector",
    description:
      "Construct a unit vector u in the direction of the given angle theta (in radians).",
    params: [{ name: "theta", type: realT(), description: "direction" }],
    body: (_context: Context, theta: ad.Num): MayWarn<VectorV<ad.Num>> => {
      return noWarn({ tag: "VectorV", contents: [cos(theta), sin(theta)] });
    },
    returns: valueT("Real2"),
  },

  //#region ray intersection (and normal) Style functions
  rayIntersect: {
    name: "rayIntersect",
    description:
      "Given a point p and vector v, find the first point where the ray r(t)=p+tv intersects the given shape S.  If there are no intersections, returns p.",
    params: [
      {
        name: "S",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      S:
        | Circle<ad.Num>
        | Rectlike<ad.Num>
        | Line<ad.Num>
        | Polyline<ad.Num>
        | Polygon<ad.Num>
        | Ellipse<ad.Num>
        | Group<ad.Num>,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> => {
      return noWarn(vectorV(safeRI(rawRayIntersect(S, p, v), p)));
    },
    returns: valueT("Real2"),
  },
  rayIntersectDistance: {
    name: "rayIntersectDistance",
    description:
      "Given a point p and vector v, returns the distance to the first point where the ray r(t)=p+tv intersects the shape S.  If there are no intersections, returns Infinity.",
    params: [
      {
        name: "S",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      S:
        | Circle<ad.Num>
        | Rectlike<ad.Num>
        | Line<ad.Num>
        | Polyline<ad.Num>
        | Polygon<ad.Num>
        | Ellipse<ad.Num>
        | Group<ad.Num>,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn(floatV(distRI(rawRayIntersect(S, p, v), p)));
    },
    returns: valueT("Real"),
  },
  rayIntersectCircle: {
    name: "rayIntersectCircle",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      r: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectCircle(c, r, p, v), p))),
    returns: valueT("Real2"),
  },
  rayIntersectCircleDistance: {
    name: "rayIntersectCircleDistance",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      r: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectCircle(c, r, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectEllipse: {
    name: "rayIntersectEllipse",
    params: [
      { name: "c", type: real2T() },
      { name: "rx", type: realT() },
      { name: "ry", type: realT() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      rx: ad.Num,
      ry: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectEllipse(c, rx, ry, p, v), p))),
    returns: real2T(),
  },
  rayIntersectEllipseDistance: {
    name: "rayIntersectEllipseDistance",
    params: [
      { name: "c", type: real2T() },
      { name: "rx", type: realT() },
      { name: "ry", type: realT() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      rx: ad.Num,
      ry: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectEllipse(c, rx, ry, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectLine: {
    name: "rayIntersectLine",
    params: [
      { name: "start", type: real2T() },
      { name: "end", type: real2T() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      start: ad.Num[],
      end: ad.Num[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectLine(start, end, p, v), p))),
    returns: real2T(),
  },
  rayIntersectLineDistance: {
    name: "rayIntersectLineDistance",
    params: [
      { name: "start", type: real2T() },
      { name: "end", type: real2T() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      start: ad.Num[],
      end: ad.Num[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectLine(start, end, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectRect: {
    name: "rayIntersectRect",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectRect(rect, p, v), p))),
    returns: real2T(),
  },
  rayIntersectRectDistance: {
    name: "rayIntersectRectDistance",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectRect(rect, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectPoly: {
    name: "rayIntersectPoly",
    params: [
      {
        name: "pts",
        type: real2NT(),
      },
      {
        name: "closed",
        type: booleanT(),
      },

      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      closed: boolean,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectPoly(pts, closed, p, v), p))),
    returns: real2T(),
  },
  rayIntersectPolyDistance: {
    name: "rayIntersectPolyDistance",
    params: [
      {
        name: "pts",
        type: real2NT(),
      },
      {
        name: "closed",
        type: booleanT(),
      },

      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      closed: boolean,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectPoly(pts, closed, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectGroup: {
    name: "rayIntersectGroup",
    params: [
      { name: "shapes", type: shapeListT() },

      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRI(rawRayIntersectGroup(shapes, p, v), p))),
    returns: real2T(),
  },
  rayIntersectGroupDistance: {
    name: "rayIntersectGroupDistance",
    params: [
      { name: "shapes", type: shapeListT() },

      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(distRI(rawRayIntersectGroup(shapes, p, v), p))),
    returns: valueT("Real"),
  },
  rayIntersectNormal: {
    name: "rayIntersectNormal",
    description:
      "Given a point p and vector v, find the unit normal at the first point where the ray r(t)=p+tv intersects the given shape S.  If there are no intersections, returns (0,0).",
    params: [
      {
        name: "S",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      S:
        | Circle<ad.Num>
        | Rectlike<ad.Num>
        | Line<ad.Num>
        | Polyline<ad.Num>
        | Polygon<ad.Num>
        | Ellipse<ad.Num>
        | Group<ad.Num>,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersect(S, p, v), p))),
    returns: valueT("Real2"),
  },
  rayIntersectNormalCircle: {
    name: "rayIntersectNormalCircle",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      r: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectCircle(c, r, p, v), p))),
    returns: valueT("Real2"),
  },
  rayIntersectNormalEllipse: {
    name: "rayIntersectNormalEllipse",
    params: [
      { name: "c", type: real2T() },
      { name: "rx", type: realT() },
      { name: "ry", type: realT() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      c: ad.Num[],
      rx: ad.Num,
      ry: ad.Num,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectEllipse(c, rx, ry, p, v), p))),
    returns: real2T(),
  },
  rayIntersectNormalLine: {
    name: "rayIntersectNormalLine",
    params: [
      { name: "start", type: real2T() },
      { name: "end", type: real2T() },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      start: ad.Num[],
      end: ad.Num[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectLine(start, end, p, v), p))),
    returns: real2T(),
  },
  rayIntersectNormalRect: {
    name: "rayIntersectNormalRect",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle",
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectRect(rect, p, v), p))),
    returns: real2T(),
  },
  rayIntersectNormalPoly: {
    name: "rayIntersectNormalPoly",
    params: [
      {
        name: "pts",
        type: real2NT(),
      },
      {
        name: "closed",
        type: booleanT(),
      },
      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      closed: boolean,
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectPoly(pts, closed, p, v), p))),
    returns: real2T(),
  },
  rayIntersectNormalGroup: {
    name: "rayIntersectNormalGroup",
    params: [
      { name: "shapes", type: shapeListT() },

      { name: "p", type: real2T(), description: "A point" },
      { name: "v", type: real2T(), description: "A vector" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      p: ad.Num[],
      v: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeRIN(rawRayIntersectGroup(shapes, p, v), p))),
    returns: real2T(),
  },
  //#endregion

  //#region closest point style functions
  closestPoint: {
    name: "closestPoint",
    description:
      "Returns a point on the shape s closest to a query point p.  If this point is not unique, an arbitrary choice is made.",
    params: [
      {
        name: "s",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A vector" },
    ],
    body: (_context: Context, shape: Shape<ad.Num>, p: ad.Pt2) =>
      noWarn(vectorV(closestPoint(shape, p))),
    returns: valueT("Real2"),
  },
  closestPointCircle: {
    name: "closestPointCircle",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      r: ad.Num,
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(closestPointCircle(c, r, pt))),
    returns: valueT("Real2"),
  },
  closestPointRect: {
    name: "closestPointRect",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle",
      },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> => noWarn(vectorV(closestPointRect(rect, pt))),
    returns: valueT("Real2"),
  },
  closestPointLine: {
    name: "closestPointLine",
    params: [
      { name: "start", type: real2T(), description: "start point of line" },
      { name: "end", type: real2T(), description: "end point of line" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      start: ad.Pt2,
      end: ad.Pt2,
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(closestPointLine(start, end, pt))),
    returns: valueT("Real2"),
  },
  closestPointEllipse: {
    name: "closestPointEllipse",
    params: [
      { name: "c", type: real2T(), description: "center of ellipse" },
      {
        name: "rx",
        type: realT(),
        description: "horizontal radius of ellipse",
      },
      { name: "ry", type: realT(), description: "vertical radius of ellipse" },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      rx: ad.Num,
      ry: ad.Num,
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(closestPointEllipse(c, rx, ry, pt))),
    returns: valueT("Real2"),
  },
  closestPointPoly: {
    name: "closestPointPoly",
    params: [
      { name: "pts", type: real2NT(), description: "points of the polygon" },
      {
        name: "closed",
        type: booleanT(),
        description: "whether or not the polygon is closed",
      },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      closed: boolean,
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(closestPointPoly(pts, closed, pt))),
    returns: valueT("Real2"),
  },
  closestPointGroup: {
    name: "closestPointGroup",
    params: [
      {
        name: "shapes",
        type: shapeListT(),
        description: "shapes of the group",
      },
      { name: "pt", type: real2T(), description: "the point" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      pt: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(closestPointGroup(shapes, pt))),
    returns: real2T(),
  },
  //#endregion

  //#region closest silhouette point style functions
  closestSilhouettePoint: {
    name: "closestSilhouettePoint",
    description:
      "Returns a point on the visibility silhouette of shape s closest to a query point p.  If this point is not unique, an arbitrary choice is made.  If no such point exists, the query point p is returned.",
    params: [
      {
        name: "s",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      s:
        | Circle<ad.Num>
        | Rectlike<ad.Num>
        | Line<ad.Num>
        | Polyline<ad.Num>
        | Polygon<ad.Num>
        | Ellipse<ad.Num>
        | Group<ad.Num>,
      p: ad.Num[],
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePoint(s, p), p))),
    returns: valueT("Real2"),
  },
  closestSilhouettePointCircle: {
    name: "closestSilhouettePointCircle",
    params: [
      { name: "c", type: real2T(), description: "center of circle" },
      { name: "r", type: realT(), description: "radius of circle" },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      r: ad.Num,
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointCircle(c, r, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointEllipse: {
    name: "closestSilhouettePointEllipse",
    params: [
      { name: "c", type: real2T() },
      { name: "rx", type: realT() },
      { name: "ry", type: realT() },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      rx: ad.Num,
      ry: ad.Num,
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(closestPointEllipse(c, rx, ry, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointLine: {
    name: "closestSilhouettePointLine",
    params: [
      { name: "start", type: real2T() },
      { name: "end", type: real2T() },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      start: ad.Pt2,
      end: ad.Pt2,
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointLine(start, end, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointRect: {
    name: "closestSilhouettePointRect",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle",
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointRect(rect, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointPolyline: {
    name: "closestSilhouettePointPolyline",
    params: [
      {
        name: "points",
        type: real2NT(),
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      points: ad.Pt2[],
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointPolyline(points, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointPolygon: {
    name: "closestSilhouettePointPolygon",
    params: [
      {
        name: "points",
        type: real2NT(),
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      points: ad.Pt2[],
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointPolygon(points, p), p))),
    returns: real2T(),
  },
  closestSilhouettePointGroup: {
    name: "closestSilhouettePointGroup",
    params: [
      {
        name: "shapes",
        type: shapeListT(),
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      p: ad.Pt2,
    ): MayWarn<VectorV<ad.Num>> =>
      noWarn(vectorV(safeCSP(rawClosestSilhouettePointGroup(shapes, p), p))),
    returns: real2T(),
  },
  //#endregion

  closestSilhouetteDistance: {
    name: "closestSilhouetteDistance",
    description:
      "Returns the distance to the closest point on the visibility silhouette of shape s relative to query point p.  If no such point exists, returns Infinity.",
    params: [
      {
        name: "s",
        type: unionT(
          rectlikeT(),
          shapeT("Circle"),
          shapeT("Polygon"),
          shapeT("Line"),
          shapeT("Polyline"),
          shapeT("Ellipse"),
          shapeT("Group"),
        ),
        description: "A shape",
      },
      { name: "p", type: real2T(), description: "A point" },
    ],
    body: (
      _context: Context,
      s:
        | Circle<ad.Num>
        | Rectlike<ad.Num>
        | Line<ad.Num>
        | Polyline<ad.Num>
        | Polygon<ad.Num>
        | Ellipse<ad.Num>
        | Group<ad.Num>,
      p: ad.Num[],
    ): MayWarn<FloatV<ad.Num>> => {
      const q = rawClosestSilhouettePoint(s, p);
      const d = ifCond(eq(q[0], Infinity), Infinity, ops.vdist(p, q));
      return noWarn({ tag: "FloatV", contents: d });
    },
    returns: valueT("Real"),
  },

  rectLineDist: {
    name: "rectLineDist",
    description:
      "Return the distance between a rectangle (defined using the bottom-left and top-right points) and a line (defined using start and end points)",
    params: [
      {
        name: "bottomLeft",
        type: real2T(),
        description: "bottom-left point of rectangle",
      },
      {
        name: "topRight",
        type: real2T(),
        description: "top-right point of rectangle",
      },
      { name: "start", type: real2T(), description: "start point of line" },
      { name: "end", type: real2T(), description: "end point of line" },
    ],
    body: (
      _context: Context,
      bottomLeft: ad.Pt2,
      topRight: ad.Pt2,
      start: ad.Pt2,
      end: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(
        floatV(
          rectLineDist(
            bottomLeft[0],
            bottomLeft[1],
            topRight[0],
            topRight[1],
            start[0],
            start[1],
            end[0],
            end[1],
          ),
        ),
      ),
    returns: valueT("Real"),
  },

  //#region shape distance style functions
  shapeDistance: {
    name: "shapeDistance",
    description: "Return the distance between two shapes.",
    params: [
      { name: "s1", type: shapeT("AnyShape"), description: "a shape" },
      { name: "s2", type: shapeT("AnyShape"), description: "a shape" },
    ],
    body: (
      _context: Context,
      s1: Shape<ad.Num>,
      s2: Shape<ad.Num>,
    ): MayWarn<FloatV<ad.Num>> => {
      const dist = shapeDistance(s1, s2);
      return {
        value: floatV(dist.value),
        warnings: dist.warnings,
      };
    },
    returns: valueT("Real"),
  },
  shapeDistanceCircles: {
    name: "shapeDistanceCircles",
    description: "Return the distance between two circles.",
    params: [
      { name: "c1", type: real2T(), description: "center of first circle" },
      { name: "r1", type: realT(), description: "radius of first circle" },
      { name: "c2", type: real2T(), description: "center of second circle" },
      { name: "r2", type: realT(), description: "radius of second circle" },
    ],
    body: (
      _context: Context,
      c1: ad.Pt2,
      r1: ad.Num,
      c2: ad.Pt2,
      r2: ad.Num,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceCircles(c1, r1, c2, r2))),
    returns: realT(),
  },
  shapeDistanceRects: {
    name: "shapeDistanceRects",
    description: "Return the distance between two rectangles.",
    params: [
      {
        name: "rect1",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the first rectangle.",
      },
      {
        name: "rect2",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the second rectangle.",
      },
    ],
    body: (
      _context: Context,
      rect1: ad.Pt2[],
      rect2: ad.Pt2[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceRects(rect1, rect2))),
    returns: realT(),
  },
  shapeDistanceRectLine: {
    name: "shapeDistanceRectLine",
    description: "Returns the distance between a rectangle and a line.",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle.",
      },
      {
        name: "start",
        type: real2T(),
        description: "The start point of the line",
      },
      { name: "end", type: real2T(), description: "The end point of the line" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      start: ad.Pt2,
      end: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceRectLine(rect, start, end))),
    returns: realT(),
  },
  shapeDistanceRectlikePolyline: {
    name: "shapeDistanceRectlikePolyline",
    description: "Returns the distance between a rectangle and a polyline.",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle.",
      },
      {
        name: "points",
        type: realNMT(),
        description: "points of polyline",
      },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      points: ad.Num[][],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceRectlikePolyline(rect, points))),
    returns: realT(),
  },
  shapeDistancePolys: {
    name: "shapeDistancePolys",
    description: "Returns the distance between two polygons.",
    params: [
      {
        name: "pts1",
        type: real2NT(),
        description: "The list of points for the first polygon",
      },
      {
        name: "pts2",
        type: real2NT(),
        description: "The list of points for the second polygon",
      },
    ],
    body: (
      _context: Context,
      pts1: ad.Pt2[],
      pts2: ad.Pt2[],
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistancePolys(pts1, pts2))),
    returns: realT(),
  },
  shapeDistanceRectCircle: {
    name: "shapeDistanceRectCircle",
    description: "Returns the distance between a rectangle and a circle.",
    params: [
      {
        name: "rect",
        type: real2NT(),
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle.",
      },
      { name: "c", type: real2T(), description: "center of the circle" },
      { name: "r", type: realT(), description: "radius of the circle" },
    ],
    body: (
      _context: Context,
      rect: ad.Pt2[],
      c: ad.Pt2,
      r: ad.Num,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceRectCircle(rect, c, r))),
    returns: realT(),
  },
  shapeDistancePolyEllipse: {
    name: "shapeDistancePolyEllipse",
    description: "Returns the distance between a polygon and an ellipse.",
    params: [
      {
        name: "pts",
        type: real2NT(),
        description: "The list of points for the polygon",
      },
      { name: "c", type: real2T(), description: "center of the ellipse" },
      {
        name: "rx",
        type: realT(),
        description: "horizontal radius of ellipse",
      },
      { name: "ry", type: realT(), description: "vertical radius of ellipse" },
    ],
    body: (
      _context: Context,
      pts: ad.Pt2[],
      c: ad.Pt2,
      rx: ad.Num,
      ry: ad.Num,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistancePolyEllipse(pts, c, rx, ry))),
    returns: realT(),
  },
  shapeDistanceCircleLine: {
    name: "shapeDistanceCircleLine",
    description: "Returns the distance between a circle and a line.",
    params: [
      { name: "c", type: real2T(), description: "center of the circle" },
      { name: "r", type: realT(), description: "radius of the circle" },
      { name: "start", type: real2T(), description: "start point of line" },
      { name: "end", type: real2T(), description: "end point of line" },
    ],
    body: (
      _context: Context,
      c: ad.Pt2,
      r: ad.Num,
      start: ad.Pt2,
      end: ad.Pt2,
    ): MayWarn<FloatV<ad.Num>> =>
      noWarn(floatV(shapeDistanceCircleLine(c, r, start, end))),
    returns: realT(),
  },
  shapeDistanceLines: {
    name: "shapeDistanceLines",
    description: "Returns the distance between two lines.",
    params: [
      { name: "start1", type: real2T(), description: "start point of line" },
      { name: "end1", type: real2T(), description: "end point of line" },
      { name: "start2", type: real2T(), description: "start point of line" },
      { name: "end2", type: real2T(), description: "end point of line" },
    ],
    body: (
      _context: Context,
      start1: ad.Pt2,
      end1: ad.Pt2,
      start2: ad.Pt2,
      end2: ad.Pt2,
    ) => noWarn(floatV(shapeDistanceLines(start1, end1, start2, end2))),
    returns: realT(),
  },
  //#endregion

  /**
   * Returns the signed area enclosed by a polygonal chain given its nodes
   */
  signedArea: {
    name: "signedArea",
    description:
      "Returns the signed area enclosed by a polygonal chain given its nodes",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: signedArea(points, closed) });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns the turning number of polygonal chain given its nodes
   */
  turningNumber: {
    name: "turningNumber",
    description:
      "Returns the turning number of polygonal chain given its nodes",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: turningNumber(points, closed),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns the total length of polygonal chain given its nodes
   */
  perimeter: {
    name: "perimeter",
    description: "Returns the total length of polygonal chain given its nodes",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of polygonal chain",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the polygonic chain is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: perimeter(points, closed) });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns the isoperimetric ratio (perimeter squared divided by enclosed area)
   */
  isoperimetricRatio: {
    name: "isoperimetricRatio",
    description:
      "Returns the isoperimetric ratio (perimeter squared divided by enclosed area)",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether the curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: isoperimetricRatio(points, closed),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns integral of curvature squared along the curve
   */
  elasticEnergy: {
    name: "elasticEnergy",
    description: "Returns integral of curvature squared along the curve",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: elasticEnergy(points, closed) });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns integral of curvature along the curve
   */
  totalCurvature: {
    name: "totalCurvature",
    description: "Returns integral of curvature along the curve",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
      {
        name: "signed",
        type: booleanT(),
        description: "whether curvature is signed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
      signed = true,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: totalCurvature(points, closed, signed),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns the sum of all line segment lengths raised to `k`
   */
  lengthK: {
    name: "lengthK",
    description: "Returns the sum of all line segment lengths raised to `k`",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
      {
        name: "k",
        type: realT(),
        description: "exponent for line segments",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
      k: number,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: lengthK(points, closed, k) });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns the maximum value of curvature along the curve
   */
  maxCurvature: {
    name: "maxCurvature",
    description: "Returns the maximum value of curvature along the curve",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({ tag: "FloatV", contents: maxCurvature(points, closed) });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns integral of curvature raised to `p` along the curve
   */
  pElasticEnergy: {
    name: "pElasticEnergy",
    description: "Returns integral of curvature raised to `p` along the curve",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
      {
        name: "p",
        type: realT(),
        description: "exponent for curvature",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
      p: number,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: pElasticEnergy(points, closed, p),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns integral of curvature derivative raised to `p` along the curve
   */
  inflectionEnergy: {
    name: "inflectionEnergy",
    description:
      "Returns integral of curvature derivative raised to `p` along the curve",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
      {
        name: "p",
        type: realT(),
        description: "exponent for curvature derivative",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
      p: number,
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: inflectionEnergy(points, closed, p),
      });
    },
    returns: valueT("Real"),
  },

  /**
   * Returns list of `n` tangent vectors given a list of `n` points.
   */
  tangentVectors: {
    name: "tangentVectors",
    description:
      "Returns list of `n` tangent vectors given a list of `n` points.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<LListV<ad.Num>> => {
      return noWarn({
        tag: "LListV",
        contents: tangentVectors(points, closed),
      });
    },
    returns: valueT("RealNM"),
  },

  /**
   * Returns list of `n` normal vectors given a list of `n` points.
   * If points are 2D, it calculates a normal vector as a perpendicular vector to the tangent.
   * Otherwise, it calculates the principal normal vector.
   */
  normalVectors: {
    name: "normalVectors",
    description:
      "Returns list of `n` normal vectors given a list of `n` points.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<LListV<ad.Num>> => {
      return noWarn({
        tag: "LListV",
        contents: normalVectors(points, closed),
      });
    },
    returns: valueT("RealNM"),
  },

  /**
   * Returns list of `n` binormal vectors given a list of `n` points.
   */
  binormalVectors: {
    name: "binormalVectors",
    description:
      "Returns list of `n` binormal vectors given a list of `n` points.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<LListV<ad.Num>> => {
      return noWarn({
        tag: "LListV",
        contents: binormalVectors(points, closed),
      });
    },
    returns: valueT("RealNM"),
  },

  /**
   * Returns evolute curve from a list of `n` points.
   */
  evoluteCurve: {
    name: "evoluteCurve",
    description: "Returns evolute curve from a list of `n` points.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<LListV<ad.Num>> => {
      return noWarn({
        tag: "LListV",
        contents: evoluteCurve(points, closed),
      });
    },
    returns: valueT("RealNM"),
  },

  /**
   * Returns an offset version of the input curve.
   */
  offsetCurve: {
    name: "offsetCurve",
    description: "Returns an offset version of the input curve.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
      {
        name: "magnitude",
        type: realT(),
        description: "magnitude of the offset",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
      magnitude: ad.Num,
    ): MayWarn<LListV<ad.Num>> => {
      return noWarn({
        tag: "LListV",
        contents: offsetCurve(points, closed, magnitude),
      });
    },
    returns: valueT("RealNM"),
  },

  /**
   * Returns list of `n` curvature values given a list of `n` points.
   */
  curvatures: {
    name: "curvatures",
    description:
      "Returns list of `n` curvature values given a list of `n` points.",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "points of curve",
      },
      {
        name: "closed",
        type: booleanT(),
        description: "whether curve is closed",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      closed: boolean,
    ): MayWarn<ListV<ad.Num>> => {
      return noWarn({
        tag: "ListV",
        contents: curvatures(points, closed),
      });
    },
    returns: valueT("RealN"),
  },

  /**
   * Returns center of mass for a 2D point cloud
   */
  centerOfMass: {
    name: "centerOfMass",
    description: "Returns center of mass for a 2D point cloud",
    params: [
      {
        name: "points",
        type: real2NT(),
        description: "points of curve",
      },
    ],
    body: (
      _context: Context,
      points: [ad.Num, ad.Num][],
    ): MayWarn<VectorV<ad.Num>> => {
      return noWarn({ tag: "VectorV", contents: centerOfMass(points) });
    },
    returns: valueT("Real2"),
  },

  noClip: {
    name: "noClip",
    description: "Describes no shape clipping",
    params: [],
    body: (_context: Context): MayWarn<ClipDataV<ad.Num>> =>
      noWarn(clipDataV(noClip())),
    returns: valueT("ClipData"),
  },

  clip: {
    name: "clip",
    description: "Describes clipping to a shape",
    params: [{ name: "shape", type: shapeT("AnyShape") }],
    body: (
      _context: Context,
      shape: Shape<ad.Num>,
    ): MayWarn<ClipDataV<ad.Num>> => noWarn(clipDataV(clipShape(shape))),
    returns: valueT("ClipData"),
  },

  bboxPts: {
    name: "bboxPts",
    description:
      "Returns the top-left, top-right, bottom-right, bottom-left points (in that order) of the axis-aligned bounding box of a shape",
    params: [{ name: "s", type: shapeT("AnyShape"), description: "a shape" }],
    body: (_context: Context, s: Shape<ad.Num>): MayWarn<PtListV<ad.Num>> => {
      return noWarn({ tag: "PtListV", contents: bboxPts(bboxFromShape(s)) });
    },
    returns: valueT("Real2N"),
  },

  tsneEnergy: {
    name: "tsneEnergy",
    description: "Returns T-SNE energy",
    params: [
      {
        name: "points",
        type: realNMT(),
        description: "high dimensional points",
      },
      {
        name: "projectedPoints",
        type: realNMT(),
        description: "projected, low dimensional points",
      },
    ],
    body: (
      _context: Context,
      points: ad.Num[][],
      projectedPoints: ad.Num[][],
    ): MayWarn<FloatV<ad.Num>> => {
      return noWarn({
        tag: "FloatV",
        contents: calculateTsneEnergy(points, projectedPoints),
      });
    },
    returns: valueT("Real"),
  },
  rectPts: {
    name: "rectPts",
    description:
      "Returns the top-left, top-right, bottom-right, bottom-left points of a rect-like shape. This takes into account rotation.",
    params: [{ name: "s", type: rectlikeT() }],
    body: (
      _context: Context,
      s: Rectlike<ad.Num>,
    ): MayWarn<PtListV<ad.Num>> => {
      return noWarn(
        ptListV(
          rectPts(
            s.center.contents,
            s.width.contents,
            s.height.contents,
            s.rotation.contents,
          ),
        ),
      );
    },
    returns: real2NT(),
  },
  TeXify: {
    name: "TeXify",
    description:
      'Returns the TeX-fied version of a string where subscripts are handled in a way suitable for equation rendering. For example, "hello_world" becomes "{hello}_{world}".',
    params: [{ name: "str", type: stringT() }],
    body: (_context: Context, str: string): MayWarn<StrV> => {
      const splitted = str.split("_");
      return noWarn(strV(TeXifyHelper(splitted)));
    },
    returns: stringT(),
  },

  repeat: {
    name: "repeat",
    description: `Returns a vector of n elements of K`,
    params: [
      { name: "n", type: natT() },
      { name: "k", type: realT() },
    ],
    body: (
      _context: Context,
      n: ad.Num,
      k: ad.Num,
    ): MayWarn<VectorV<ad.Num>> => {
      if (typeof n !== "number" || !Number.isInteger(n)) {
        throw new Error("`n` must be a fixed integer");
      }

      const arr: ad.Num[] = new Array(n).fill(k);
      return noWarn(vectorV(arr));
    },
    returns: realNT(),
  },
};

const TeXifyHelper = (segments: string[]): string => {
  // This function performs cascading.
  if (segments.length === 0) {
    return "";
  }

  const [first, ...rest] = segments;
  if (rest.length === 0) {
    return `{${first}}`;
  } else {
    return `{${first}}_{${TeXifyHelper(rest)}}`;
  }
};

// `_compDictVals` causes TypeScript to enforce that every function in
// `compDict` actually has type `CompFunc` with the right function signature, etc.
const _compDictVals: CompFunc[] = Object.values(compDict);

const sumVectors = (vecs: ad.Num[][]): ad.Num[] => {
  if (vecs.length === 0) {
    throw new Error("Expect a non-empty list of vectors");
  }
  const vlen = vecs[0].length;
  const zeros: ad.Num[] = new Array(vlen).fill(0);
  return vecs.reduce((curr, v) => ops.vadd(curr, v), zeros);
};

/*
  float msign(in float x) { return (x<0.0)?-1.0:1.0; }
*/
export const msign = (x: ad.Num): ad.Num => {
  return ifCond(lt(x, 0), -1, 1);
};

//#region Signed Distance Functions

const signedDistance = (s: Shape<ad.Num>, p: ad.Pt2): ad.Num => {
  if (isRectlike(s)) {
    return signedDistanceRect(bboxPts(bboxFromShape(s)), p);
  } else if (s.shapeType === "Circle") {
    return signedDistanceCircle(toPt(s.center.contents), s.r.contents, p);
  } else if (s.shapeType === "Polygon") {
    return signedDistancePolygon(polygonLikePoints(s), p);
  } else if (s.shapeType === "Line") {
    return signedDistanceLine(toPt(s.start.contents), toPt(s.end.contents), p);
  } else if (s.shapeType === "Polyline") {
    return signedDistancePolyline(polygonLikePoints(s), p);
  } else if (s.shapeType === "Group") {
    return signedDistanceGroup(s.shapes.contents, p);
  } else {
    throw new Error(
      `Shape type ${s.shapeType} is not supported by signedDistance`,
    );
  }
};

// All math borrowed from:
// https://iquilezles.org/articles/distfunctions2d/

export const signedDistanceRect = (rect: ad.Pt2[], pt: ad.Pt2): ad.Num => {
  /*  
    
    axis-aligned rectangle:
    float sdBox( in vec2 p, in vec2 b )
    {
      vec2 d = abs(p)-b;
      return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
    } 
    */
  if (rect.length !== 4) {
    throw new Error("Expects rect to have four points");
  }
  const [tr, tl, bl] = rect;
  const center = ops.vmul(0.5, ops.vadd(tr, bl));
  const width = sub(tr[0], tl[0]);
  const height = sub(tl[1], bl[1]);

  const absp = ops.vabs(ops.vsub(pt, center));
  const b = [div(width, 2), div(height, 2)];
  const d = ops.vsub(absp, b);
  return add(ops.vnorm(ops.vmax(d, [0.0, 0.0])), min(max(d[0], d[1]), 0.0));
};

export const signedDistanceCircle = (
  c: ad.Pt2,
  r: ad.Num,
  pt: ad.Pt2,
): ad.Num => {
  /*     
      float sdCircle( vec2 p, float r )
      {
        return length(p) - r;
      } 
  */
  const pOffset = ops.vsub(pt, c);
  return sub(ops.vnorm(pOffset), r);
};

export const signedDistancePolygon = (pts: ad.Pt2[], pt: ad.Pt2): ad.Num => {
  /*
      float sdPolygon( in vec2[N] v, in vec2 p )
      {
          float d = dot(p-v[0],p-v[0]);
          float s = 1.0;
          for( int i=0, j=N-1; i<N; j=i, i++ )
          {
              vec2 e = v[j] - v[i];
              vec2 w =    p - v[i];
              vec2 b = w - e*clamp( dot(w,e)/dot(e,e), 0.0, 1.0 );
              d = min( d, dot(b,b) );
              bvec3 c = bvec3(p.y>=v[i].y,p.y<v[j].y,e.x*w.y>e.y*w.x);
              if( all(c) || all(not(c)) ) s*=-1.0;  
          }
          return s*sqrt(d);
      }
  */
  const v = pts;
  let d = ops.vdot(ops.vsub(pt, v[0]), ops.vsub(pt, v[0]));
  let ess: ad.Num = 1.0;
  let j = v.length - 1;
  for (let i = 0; i < v.length; i++) {
    const e = ops.vsub(v[j], v[i]);
    const w = ops.vsub(pt, v[i]);
    const clampedVal = clamp([0, 1], div(ops.vdot(w, e), ops.vdot(e, e)));
    const b = ops.vsub(w, ops.vmul(clampedVal, e));
    d = min(d, ops.vdot(b, b));
    const c1 = gte(pt[1], v[i][1]);
    const c2 = lt(pt[1], v[j][1]);
    const c3 = gt(mul(e[0], w[1]), mul(e[1], w[0]));
    const c4 = and(and(c1, c2), c3);
    const c5 = not(c1);
    const c6 = not(c2);
    const c7 = not(c3);
    const c8 = and(and(c5, c6), c7);
    const negEss = mul(-1, ess);
    ess = ifCond(or(c4, c8), negEss, ess);
    // last line to match for loop in code we are borrowing from
    j = i;
  }
  return mul(ess, sqrt(d));
};

export const signedDistanceEllipse = (
  center: ad.Pt2,
  radiusx: ad.Num,
  radiusy: ad.Num,
  pInput: ad.Pt2,
) => {
  /*
    Ported code is here: https://www.shadertoy.com/view/4sS3zz
  */
  // if = abs( p );
  // if( p.x>p.y ){ p=p.yx; ab=ab.yx; }
  const pOffset = ops.vsub(pInput, center);
  const pUnswizzled = ops.vabs(pOffset);
  const abUnswizzled = [radiusx, radiusy];
  const p = [];
  const ab = [];
  p[0] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    pUnswizzled[1],
    pUnswizzled[0],
  );
  p[1] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    pUnswizzled[0],
    pUnswizzled[1],
  );
  ab[0] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    abUnswizzled[1],
    abUnswizzled[0],
  );
  ab[1] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    abUnswizzled[0],
    abUnswizzled[1],
  );
  // float l = ab.y*ab.y - ab.x*ab.x;
  const l = sub(squared(ab[1]), squared(ab[0]));
  // float m = ab.x*p.x/l;
  const m = div(mul(ab[0], p[0]), l);
  // float m2 = m*m;
  const m2 = squared(m);
  // float n = ab.y*p.y/l;
  const n = div(mul(ab[1], p[1]), l);
  // float n2 = n*n;
  const n2 = squared(n);
  // float c = (m2+n2-1.0)/3.0; float c3 = c*c*c;
  const c = div(sub(add(m2, n2), 1), 3);
  const c3 = mul(mul(c, c), c);
  // float q = c3 + m2*n2*2.0;
  const q = add(c3, mul(m2, mul(n2, 2)));
  // float d = c3 + m2*n2;
  const d = add(c3, mul(m2, n2));
  // float g = m + m*n2;
  const g = add(m, mul(m, n2));

  //if branch
  // float h = acos(q/c3)/3.0;
  const hif = div(acos(div(q, c3)), 3);
  // float s = cos(h) + 2.0;
  const sif = add(cos(hif), 2);
  // float t = sin(h)*sqrt(3.0);
  const tif = mul(sin(hif), sqrt(3));
  // float rx = sqrt( m2-c*(s+t) );
  const rxif = sqrt(sub(m2, mul(c, add(sif, tif))));
  // float ry = sqrt( m2-c*(s-t) );
  const ryif = sqrt(sub(m2, mul(c, sub(sif, tif))));
  // co = ry + sign(l)*rx + abs(g)/(rx*ry);
  const coif = add(
    add(ryif, mul(sign(l), rxif)),
    div(absVal(g), mul(rxif, ryif)),
  );
  // elsebranch
  // float h = 2.0*m*n*sqrt(d);
  const h = mul(2, mul(m, mul(n, sqrt(d))));
  // float s = msign(q+h)*pow( abs(q+h), 1.0/3.0 );
  const onethird = div(1, 3);
  const s = mul(msign(add(q, h)), pow(absVal(add(q, h)), onethird));
  // float t = msign(q-h)*pow( abs(q-h), 1.0/3.0 );
  const t = mul(msign(sub(q, h)), pow(absVal(sub(q, h)), onethird));
  // float rx = -(s+t) - c*4.0 + 2.0*m2;
  const rx = add(sub(neg(add(s, t)), mul(c, 4)), mul(2, m2));
  // float ry =  (s-t)*sqrt(3.0);
  const ry = mul(sub(s, t), sqrt(3));
  // float rm = sqrt( rx*rx + ry*ry );
  const rm = sqrt(add(squared(rx), squared(ry)));
  // co = ry/sqrt(rm-rx) + 2.0*g/rm;
  const coelse = add(div(ry, sqrt(sub(rm, rx))), mul(2, div(g, rm)));

  // co = (co-m)/2.0;
  // if (d<0.0)
  const co_pred = ifCond(lt(d, 0), coif, coelse);
  const co = div(sub(co_pred, m), 2);

  // float si = sqrt( max(1.0-co*co,0.0) );
  const si = sqrt(max(sub(1, squared(co)), 0));
  // vec2 r = ab * vec2(co,si);
  const r = ops.vproduct(ab, [co, si]);
  // return length(r-p) * msign(p.y-r.y);
  return mul(ops.vnorm(ops.vsub(r, p)), msign(sub(p[1], r[1])));
};

export const signedDistanceLine = (
  start: ad.Pt2,
  end: ad.Pt2,
  pt: ad.Pt2,
): ad.Num => {
  /*
  Computes the signed distance for a line 
    float sdSegment( in vec2 p, in vec2 a, in vec2 b )
    {
      vec2 pa = p-a, ba = b-a;
      float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
      return length( pa - ba*h );
    }
  */
  const pa = ops.vsub(pt, start);
  const ba = ops.vsub(end, start);
  const h = clamp([0, 1], div(ops.vdot(pa, ba), ops.vdot(ba, ba)));
  return ops.vnorm(ops.vsub(pa, ops.vmul(h, ba)));
};

export const signedDistancePolyline = (pts: ad.Pt2[], pt: ad.Pt2): ad.Num => {
  const dists: ad.Num[] = [];
  for (let i = 0; i < pts.length - 1; i++) {
    const start = pts[i];
    const end = pts[i + 1];
    dists[i] = signedDistanceLine(start, end, pt);
  }
  return minN(dists);
};

export const signedDistanceGroup = (
  ss: Shape<ad.Num>[],
  pt: ad.Pt2,
): ad.Num => {
  const dists = ss.map((s) => signedDistance(s, pt));
  return minN(dists);
};

//#endregion

//#region Ray intersection
// `raw` functions may return `Infinity` when ray intersection does not exist.
// `safe` functions return `p` if `Infinity` is detected.

// safe ray intersection
export const safeRI = (hit: ad.Num[][], p: ad.Num[]): ad.Num[] => {
  const x = hit[0];
  const x0 = ifCond(eq(absVal(x[0]), Infinity), p[0], x[0]);
  const x1 = ifCond(eq(absVal(x[1]), Infinity), p[1], x[1]);
  return [x0, x1];
};

// safe ray intersection normal
export const safeRIN = (hit: ad.Num[][], p: ad.Num[]): ad.Num[] => {
  const x = hit[0];
  const n = hit[1];
  const s = ops.vdot(n, ops.vsub(p, x));
  const n0 = ifCond(
    eq(absVal(x[0]), Infinity),
    0.0,
    ifCond(lt(s, 0), neg(n[0]), n[0]),
  );
  const n1 = ifCond(
    eq(absVal(x[1]), Infinity),
    0.0,
    ifCond(lt(s, 0), neg(n[1]), n[1]),
  );
  return [n0, n1];
};

// ray intersection distance
export const distRI = (hit: ad.Num[][], p: ad.Num[]): ad.Num => {
  const x = hit[0]; // hit location
  // if the point is at infinity, return an infinite distance;
  // otherwise, compute and return the distance to the hit point
  const t = ifCond(eq(absVal(x[0]), Infinity), Infinity, ops.vdist(p, x));
  return t;
};

export const rawRayIntersect = (
  s:
    | Circle<ad.Num>
    | Rectlike<ad.Num>
    | Line<ad.Num>
    | Polyline<ad.Num>
    | Polygon<ad.Num>
    | Ellipse<ad.Num>
    | Path<ad.Num>
    | Group<ad.Num>,
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  const t = s.shapeType;
  if (t === "Circle") {
    return rawRayIntersectCircle(s.center.contents, s.r.contents, p, v);
  } else if (
    t === "Rectangle" ||
    t === "Text" ||
    t === "Equation" ||
    t === "Image"
  ) {
    const c = s.center.contents;
    const w = s.width.contents;
    const h = s.height.contents;
    const x0 = sub(c[0], div(w, 2));
    const x1 = add(c[0], div(w, 2));
    const y0 = sub(c[1], div(h, 2));
    const y1 = add(c[1], div(h, 2));
    return rawRayIntersectRectHelper(x0, x1, y0, y1, p, v);
  } else if (t === "Line") {
    return rawRayIntersectLine(s.start.contents, s.end.contents, p, v);
  } else if (t === "Polyline") {
    return rawRayIntersectPoly(s.points.contents, false, p, v);
  } else if (t === "Polygon") {
    return rawRayIntersectPoly(s.points.contents, true, p, v);
  } else if (t === "Ellipse") {
    return rawRayIntersectEllipse(
      s.center.contents,
      s.rx.contents,
      s.ry.contents,
      p,
      v,
    );
  } else if (t === "Path") {
    throw new Error("Ray intersection not handled for Path");
  } else {
    return rawRayIntersectGroup(s.shapes.contents, p, v);
  }
};

export const rawRayIntersectCircle = (
  c: ad.Num[],
  r: ad.Num,
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  return rawRayIntersectCircleCoords(p, v, c, r);
};

export const rawRayIntersectEllipse = (
  center: ad.Num[],
  rx: ad.Num,
  ry: ad.Num,
  p0: ad.Num[],
  v0: ad.Num[],
): ad.Num[][] => {
  // map ray data to coordinate system for unit circle
  const r = [rx, ry];
  const c0 = center;
  const p = ops.ewvvdiv(p0, r);
  const v = ops.ewvvdiv(v0, r);
  const c = ops.ewvvdiv(c0, r);

  const hit = rawRayIntersectCircleCoords(p, v, c, 1);

  // map hit point and normal back to ellipse coordinate system
  const x = ops.ewvvmul(hit[0], r);
  const n = ops.vnormalize([
    mul(div(r[1], r[0]), sub(x[0], c0[0])),
    mul(div(r[0], r[1]), sub(x[1], c0[1])),
  ]);

  return [x, n];
};

const rawRayIntersectCircleCoords = (
  p: ad.Num[],
  v: ad.Num[],
  c: ad.Num[],
  r: ad.Num,
): ad.Num[][] => {
  const w = ops.vnormalize(v);
  const u = ops.vsub(p, c);
  const B = neg(ops.vdot(u, w));
  const C = sub(ops.vdot(u, u), mul(r, r));
  const D = sub(mul(B, B), C);
  const t1 = ifCond(lt(D, 0), Infinity, sub(B, sqrt(D)));
  const t2 = ifCond(lt(D, 0), Infinity, add(B, sqrt(D)));
  const t = ifCond(gt(t1, 0), t1, ifCond(gt(t2, 0), t2, Infinity));
  const x = ops.vadd(p, ops.vmul(t, w));
  const n = ops.vnormalize(ops.vsub(x, c));
  return [x, n];
};

export const rawRayIntersectLine = (
  start: ad.Num[],
  end: ad.Num[],
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  return rawRayIntersectLineCoords(p, v, start, end);
};

export const rawRayIntersectRect = (
  [tr, tl, bl, br]: ad.Num[][],
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  const x0 = tl[0],
    x1 = tr[0];
  const y0 = bl[1],
    y1 = tl[1];
  return rawRayIntersectRectHelper(x0, x1, y0, y1, p, v);
};

const rawRayIntersectRectHelper = (
  x0: ad.Num,
  x1: ad.Num,
  y0: ad.Num,
  y1: ad.Num,
  p: ad.Num[],
  v: ad.Num[],
) => {
  const points = [
    [x0, y0],
    [x1, y0],
    [x1, y1],
    [x0, y1],
    [x0, y0],
  ];
  const firstHits: ad.Num[][][] = [];
  const dist: ad.Num[] = [];
  for (let i = 0; i < 4; i++) {
    const a = points[i];
    const b = points[i + 1];
    firstHits[i] = rawRayIntersectLineCoords(p, v, a, b);
    dist[i] = ops.vdist(p, firstHits[i][0]);
  }

  let hitX: ad.Num = Infinity;
  let hitY: ad.Num = Infinity;
  let nrmX: ad.Num = Infinity;
  let nrmY: ad.Num = Infinity;
  let firstDist: ad.Num = Infinity;
  for (let i = 0; i < 4; i++) {
    firstDist = ifCond(lt(firstDist, dist[i]), firstDist, dist[i]);
    hitX = ifCond(eq(firstDist, dist[i]), firstHits[i][0][0], hitX);
    hitY = ifCond(eq(firstDist, dist[i]), firstHits[i][0][1], hitY);
    nrmX = ifCond(eq(firstDist, dist[i]), firstHits[i][1][0], nrmX);
    nrmY = ifCond(eq(firstDist, dist[i]), firstHits[i][1][1], nrmY);
  }
  return [
    [hitX, hitY],
    [nrmX, nrmY],
  ];
};

export const rawRayIntersectPoly = (
  points: ad.Num[][],
  closed: boolean,
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  const pts = closed ? [...points, points[0]] : points;

  const firstHits: ad.Num[][][] = [];
  const dist: ad.Num[] = [];
  for (let i = 0; i < pts.length - 1; i++) {
    const a = pts[i];
    const b = pts[i + 1];
    firstHits[i] = rawRayIntersectLineCoords(p, v, a, b);
    dist[i] = ops.vdist(p, firstHits[i][0]);
  }
  let firstDist: ad.Num = Infinity;
  let hitX: ad.Num = Infinity;
  let hitY: ad.Num = Infinity;
  let nrmX: ad.Num = 0;
  let nrmY: ad.Num = 0;
  for (let i = 0; i < pts.length - 1; i++) {
    firstDist = ifCond(lt(firstDist, dist[i]), firstDist, dist[i]);
    hitX = ifCond(eq(firstDist, dist[i]), firstHits[i][0][0], hitX);
    hitY = ifCond(eq(firstDist, dist[i]), firstHits[i][0][1], hitY);
    nrmX = ifCond(eq(firstDist, dist[i]), firstHits[i][1][0], nrmX);
    nrmY = ifCond(eq(firstDist, dist[i]), firstHits[i][1][1], nrmY);
  }
  return [
    [hitX, hitY],
    [nrmX, nrmY],
  ];
};

const rawRayIntersectLineCoords = (
  p: ad.Num[],
  v: ad.Num[],
  a: ad.Num[],
  b: ad.Num[],
): ad.Num[][] => {
  const u = ops.vsub(b, a);
  const w = ops.vsub(p, a);
  const d = ops.cross2(v, u);
  const s = div(ops.cross2(v, w), d);
  const t = div(ops.cross2(u, w), d);

  // position
  const T = ifCond(
    lt(t, 0),
    Infinity,
    ifCond(lt(s, 0), Infinity, ifCond(gt(s, 1), Infinity, t)),
  );
  const x = ops.vadd(p, ops.vmul(T, v));

  // normal
  const n = ops.vnormalize(ops.rot90(u));
  const nX = ifCond(
    lt(t, 0),
    0,
    ifCond(lt(s, 0), 0, ifCond(gt(s, 1), 0, n[0])),
  );
  const nY = ifCond(
    lt(t, 0),
    0,
    ifCond(lt(s, 0), 0, ifCond(gt(s, 1), 0, n[1])),
  );

  return [x, [nX, nY]];
};

export const rawRayIntersectGroup = (
  shapes: Shape<ad.Num>[],
  p: ad.Num[],
  v: ad.Num[],
): ad.Num[][] => {
  // t === "Group"
  const firstHits = shapes.map((shape) => rawRayIntersect(shape, p, v));
  const dist = firstHits.map((hit) => ops.vdist(hit[0], p));
  let hitX: ad.Num = Infinity;
  let hitY: ad.Num = Infinity;
  let nrmX: ad.Num = Infinity;
  let nrmY: ad.Num = Infinity;
  let firstDist: ad.Num = Infinity;
  for (let i = 0; i < shapes.length; i++) {
    firstDist = ifCond(lt(firstDist, dist[i]), firstDist, dist[i]);
    hitX = ifCond(eq(firstDist, dist[i]), firstHits[i][0][0], hitX);
    hitY = ifCond(eq(firstDist, dist[i]), firstHits[i][0][1], hitY);
    nrmX = ifCond(eq(firstDist, dist[i]), firstHits[i][1][0], nrmX);
    nrmY = ifCond(eq(firstDist, dist[i]), firstHits[i][1][1], nrmY);
  }
  return [
    [hitX, hitY],
    [nrmX, nrmY],
  ];
};
//#endregion

//#region closest point

export const closestPoint = (
  s:
    | Circle<ad.Num>
    | Rectlike<ad.Num>
    | Line<ad.Num>
    | Polyline<ad.Num>
    | Polygon<ad.Num>
    | Ellipse<ad.Num>
    | Path<ad.Num>
    | Group<ad.Num>,
  p: ad.Num[],
): ad.Num[] => {
  const t = s.shapeType;
  if (t === "Circle") {
    return closestPointCircle(s.center.contents, s.r.contents, p);
  } else if (
    t === "Rectangle" ||
    t === "Text" ||
    t === "Equation" ||
    t === "Image"
  ) {
    return closestPointRect(bboxPts(bboxFromRectlike(s)), p);
  } else if (t === "Line") {
    return closestPointLine(s.start.contents, s.end.contents, p);
  } else if (t === "Polyline") {
    return closestPointPoly(s.points.contents, false, p);
  } else if (t === "Polygon") {
    return closestPointPoly(s.points.contents, true, p);
  } else if (t === "Ellipse") {
    return closestPointEllipse(
      s.center.contents,
      s.rx.contents,
      s.ry.contents,
      p,
    );
  } else if (t === "Path") {
    throw new Error("Closest point queries not handled for Path");
  } else {
    // t === "Group"
    return closestPointGroup(s.shapes.contents, p);
  }
};

export const closestPointCircle = (
  c: ad.Num[],
  r: ad.Num,
  p: ad.Num[],
): ad.Num[] => {
  /**
   * Implementing formula
   * V = P - C
   * return C + (V/|V|)*r
   */
  const pOffset = ops.vsub(p, c);
  const normOffset = ops.vnorm(pOffset);
  const unitVector = ops.vdiv(pOffset, normOffset);
  const pOnCircumferenceOffset = ops.vmul(r, unitVector);
  const pOnCircumference = ops.vadd(c, pOnCircumferenceOffset);
  return pOnCircumference;
};

export const closestPointLine = (
  start: ad.Num[],
  end: ad.Num[],
  p: ad.Num[],
): ad.Num[] => {
  return closestPointLineCoords(p, start, end);
};

export const closestPointPoly = (
  points: ad.Num[][],
  closed: boolean,
  p: ad.Num[],
): ad.Num[] => {
  const allPts = closed ? [...points, points[0]] : points;

  const closestPoints: ad.Num[][] = [];
  const dist: ad.Num[] = [];
  for (let i = 0; i < allPts.length - 1; i++) {
    const a = allPts[i];
    const b = allPts[i + 1];
    closestPoints[i] = closestPointLineCoords(p, a, b);
    dist[i] = ops.vdist(p, closestPoints[i]);
  }
  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < allPts.length - 1; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(eq(minDist, dist[i]), closestPoints[i][0], closestX);
    closestY = ifCond(eq(minDist, dist[i]), closestPoints[i][1], closestY);
  }
  return [closestX, closestY];
};

export const closestPointRect = (rect: ad.Num[][], p: ad.Num[]): ad.Num[] => {
  const [tr, tl, bl] = rect;
  const l = tl[0],
    t = bl[1];
  const w = sub(tr[0], tl[0]);
  const h = sub(tl[1], bl[1]);
  let [x, y] = p;
  const r = add(l, w);
  const b = add(t, h);
  x = clamp([l, r], x);
  y = clamp([t, b], y);
  const dl = absVal(sub(x, l));
  const dr = absVal(sub(x, r));
  const dt = absVal(sub(y, t));
  const db = absVal(sub(y, b));
  const m = min(min(min(dl, dr), dt), db);
  let retX: ad.Num = ifCond(or(eq(m, dt), eq(m, db)), x, r);
  retX = ifCond(eq(m, dl), l, retX);
  let retY: ad.Num = ifCond(or(eq(m, dl), eq(m, dr)), y, t);
  retY = ifCond(eq(m, db), b, retY);
  return [retX, retY];
};

export const closestPointEllipse = (
  c: ad.Num[],
  rx: ad.Num,
  ry: ad.Num,
  p: ad.Num[],
): ad.Num[] => {
  return closestPointEllipseCoords(rx, ry, c, p);
};

export const closestPointGroup = (
  shapes: Shape<ad.Num>[],
  p: ad.Num[],
): ad.Num[] => {
  const closestPoints = shapes.map((shape) => closestPoint(shape, p));
  const dist = closestPoints.map((point) => ops.vdist(point, p));
  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < shapes.length; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(eq(minDist, dist[i]), closestPoints[i][0], closestX);
    closestY = ifCond(eq(minDist, dist[i]), closestPoints[i][1], closestY);
  }
  return [closestX, closestY];
};

const closestPointLineCoords = (
  p: ad.Num[],
  a: ad.Num[],
  b: ad.Num[],
): ad.Num[] => {
  const a_to_p = [sub(p[0], a[0]), sub(p[1], a[1])];
  const a_to_b = [sub(b[0], a[0]), sub(b[1], a[1])];
  const atb2 = add(squared(a_to_b[0]), squared(a_to_b[1]));
  const atp_dot_atb = add(mul(a_to_p[0], a_to_b[0]), mul(a_to_p[1], a_to_b[1]));
  const t = clamp([0, 1], div(atp_dot_atb, atb2));
  return [add(a[0], mul(a_to_b[0], t)), add(a[1], mul(a_to_b[1], t))];
};

// Note: approximates the solution via Newton's method (but in practice is quite accurate, even for one or two iterations)
const closestPointEllipseCoords = (
  a: ad.Num, // horizontal radius
  b: ad.Num, // vertical radius
  c: ad.Num[], // center
  p0: ad.Num[], // query point
): ad.Num[] => {
  const nNewtonIterations = 2;

  const p = ops.vsub(p0, c);
  let t = atan2(mul(a, p[1]), mul(b, p[0]));
  for (let i = 0; i < nNewtonIterations; i++) {
    const a2 = mul(a, a);
    const b2 = mul(b, b);
    const n0 = mul(mul(a, p[0]), sin(t));
    const n1 = mul(b, p[1]);
    const n2 = mul(sub(a2, b2), sin(t));
    const n3 = mul(cos(t), add(n1, n2));
    const d0 = mul(a, mul(p[0], cos(t)));
    const d1 = mul(sub(a2, b2), cos(mul(2, t)));
    const d2 = mul(b, mul(p[1], sin(t)));
    t = sub(t, div(sub(n3, n0), sub(sub(d1, d2), d0)));
  }
  const y0 = mul(a, cos(t));
  const y1 = mul(b, sin(t));
  return ops.vadd([y0, y1], c);
};

//#endregion

//#region closest silhouette point

// `raw` functions may return `Infinity` when ray intersection does not exist.
// `safe` function return `p` if `Infinity` is detected.
export const safeCSP = (q: ad.Num[], p: ad.Num[]): ad.Num[] => {
  const qx = ifCond(eq(q[0], Infinity), p[0], q[0]);
  const qy = ifCond(eq(q[1], Infinity), p[1], q[1]);
  return [qx, qy];
};
/* Returns the closest point on the visibility
 * silhouette of shape S relative to point p.
 * If there is no silhouette, returns Infinity. */
export const rawClosestSilhouettePoint = (
  s:
    | Circle<ad.Num>
    | Rectlike<ad.Num>
    | Line<ad.Num>
    | Polyline<ad.Num>
    | Polygon<ad.Num>
    | Ellipse<ad.Num>
    | Path<ad.Num>
    | Group<ad.Num>,
  p: ad.Num[],
): ad.Num[] => {
  const t = s.shapeType;
  if (t === "Circle") {
    return rawClosestSilhouettePointCircle(s.center.contents, s.r.contents, p);
  } else if (
    t === "Rectangle" ||
    t === "Text" ||
    t === "Equation" ||
    t === "Image"
  ) {
    return rawClosestSilhouettePointRect(bboxPts(bboxFromRectlike(s)), p);
  } else if (t === "Line") {
    return rawClosestSilhouettePointLine(s.start.contents, s.end.contents, p);
  } else if (t === "Polyline") {
    return rawClosestSilhouettePointPolyline(s.points.contents, p);
  } else if (t === "Polygon") {
    return rawClosestSilhouettePointPolygon(s.points.contents, p);
  } else if (t === "Ellipse") {
    return rawClosestSilhouettePointEllipse(
      s.center.contents,
      s.rx.contents,
      s.ry.contents,
      p,
    );
  } else if (t === "Path") {
    throw new Error("Silhouette queries not handled for Path");
  } else {
    // t === "Group"
    return rawClosestSilhouettePointGroup(s.shapes.contents, p);
  }
};

export const rawClosestSilhouettePointCircle = (
  c: ad.Num[],
  r: ad.Num,
  p: ad.Num[],
): ad.Num[] => {
  const y = rawClosestSilhouettePointEllipseCoords(ops.vsub(p, c), r, r);
  return ops.vadd(y, c);
};

export const rawClosestSilhouettePointEllipse = (
  c: ad.Num[],
  rx: ad.Num,
  ry: ad.Num,
  p: ad.Num[],
): ad.Num[] => {
  const y = rawClosestSilhouettePointEllipseCoords(ops.vsub(p, c), rx, ry);
  return ops.vadd(y, c);
};

export const rawClosestSilhouettePointLine = (
  start: ad.Num[],
  end: ad.Num[],
  p: ad.Num[],
): ad.Num[] => {
  const a = start,
    b = end;
  const da = ops.vdistsq(p, a);
  const db = ops.vdistsq(p, b);
  const y0 = ifCond(lt(da, db), a[0], b[0]);
  const y1 = ifCond(lt(da, db), a[1], b[1]);
  return [y0, y1];
};

/* Computes the closest silhouette on an ellipse with radii a0,b0 relative to a
 * point p0, assuming the ellipse has already been translated to the origin and
 * rotated to be axis-aligned. */
const rawClosestSilhouettePointEllipseCoords = (
  p0: ad.Num[],
  a0: ad.Num,
  b0: ad.Num,
): ad.Num[] => {
  const b = div(b0, a0);
  const b2 = mul(b, b);
  const p = ops.vdiv(p0, a0);
  const x = p[0];
  const y = p[1];
  const x2 = mul(x, x);
  const y2 = mul(y, y);
  const d = add(mul(b2, x2), y2);
  const e = add(mul(mul(b2, sub(x2, 1)), y2), mul(y2, y2));
  const f = mul(b2, x);
  const g = mul(b2, y2);
  const u0 = ifCond(lt(e, 0), Infinity, mul(a0, div(sub(f, sqrt(e)), d)));
  const u1 = ifCond(
    lt(e, 0),
    Infinity,
    mul(a0, div(add(g, mul(mul(x, b2), sqrt(e))), mul(d, y))),
  );
  const v0 = ifCond(lt(e, 0), Infinity, mul(a0, div(add(f, sqrt(e)), d)));
  const v1 = ifCond(
    lt(e, 0),
    Infinity,
    mul(a0, div(sub(g, mul(mul(x, b2), sqrt(e))), mul(d, y))),
  );
  const du = ops.vdist([u0, u1], p0);
  const dv = ops.vdist([v0, v1], p0);
  const z0 = ifCond(lt(du, dv), u0, v0);
  const z1 = ifCond(lt(du, dv), u1, v1);
  return [z0, z1];
};

export const rawClosestSilhouettePointRect = (
  [tr, tl, bl, br]: ad.Num[][],
  p: ad.Num[],
): ad.Num[] => {
  const x0 = tl[0],
    x1 = tr[0];
  const y0 = bl[1],
    y1 = tl[1];

  const points = [
    [x0, y0],
    [x1, y0],
    [x1, y1],
    [x0, y1],
  ];
  const closestSilhouettePoints: ad.Num[][] = [];
  const dist: ad.Num[] = [];
  for (let i = 0; i < 4; i++) {
    const a = points[i];
    const b = points[(i + 1) % 4];
    const c = points[(i + 2) % 4];
    closestSilhouettePoints[i] = rawClosestSilhouettePointCorner(p, a, b, c);
    dist[i] = ops.vdist(p, closestSilhouettePoints[i]);
  }

  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < 4; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][0],
      closestX,
    );
    closestY = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][1],
      closestY,
    );
  }
  return [closestX, closestY];
};

export const rawClosestSilhouettePointPolyline = (
  pts: ad.Num[][],
  p: ad.Num[],
): ad.Num[] => {
  const closestSilhouettePoints: ad.Num[][] = [];
  const dist: ad.Num[] = [];

  // interior points
  for (let i = 0; i < pts.length - 2; i++) {
    const a = pts[i];
    const b = pts[i + 1];
    const c = pts[i + 2];
    closestSilhouettePoints[i] = rawClosestSilhouettePointCorner(p, a, b, c);
    dist[i] = ops.vdist(p, closestSilhouettePoints[i]);
  }
  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < pts.length - 2; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][0],
      closestX,
    );
    closestY = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][1],
      closestY,
    );
  }

  //endpoints
  const q0 = pts[0];
  const dist0 = ops.vdist(p, q0);
  minDist = ifCond(lt(minDist, dist0), minDist, dist0);
  closestX = ifCond(eq(minDist, dist0), q0[0], closestX);
  closestY = ifCond(eq(minDist, dist0), q0[1], closestY);
  const qN = pts[pts.length - 1];
  const distN = ops.vdist(p, qN);
  minDist = ifCond(lt(minDist, distN), minDist, distN);
  closestX = ifCond(eq(minDist, distN), qN[0], closestX);
  closestY = ifCond(eq(minDist, distN), qN[1], closestY);

  return [closestX, closestY];
};

export const rawClosestSilhouettePointPolygon = (
  pts: ad.Num[][],
  p: ad.Num[],
): ad.Num[] => {
  const closestSilhouettePoints: ad.Num[][] = [];
  const dist: ad.Num[] = [];
  for (let i = 0; i < pts.length; i++) {
    const j = (i + 1) % pts.length;
    const k = (i + 2) % pts.length;
    const a = pts[i];
    const b = pts[j];
    const c = pts[k];
    closestSilhouettePoints[i] = rawClosestSilhouettePointCorner(p, a, b, c);
    dist[i] = ops.vdist(p, closestSilhouettePoints[i]);
  }
  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < pts.length; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][0],
      closestX,
    );
    closestY = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][1],
      closestY,
    );
  }
  return [closestX, closestY];
};

/* Given three points a, b, c describing a pair of line segments ab, bc and a
 * query point p, returns b if it is a silhouette point, relative to p, and
 * (Infinity,Infinity) otherwise. */
const rawClosestSilhouettePointCorner = (
  p: ad.Num[],
  a: ad.Num[],
  b: ad.Num[],
  c: ad.Num[],
): ad.Num[] => {
  const s = mul(
    ops.cross2(ops.vsub(b, a), ops.vsub(p, a)),
    ops.cross2(ops.vsub(c, b), ops.vsub(p, b)),
  );
  const y0 = ifCond(lt(s, 0), b[0], Infinity);
  const y1 = ifCond(lt(s, 0), b[1], Infinity);
  return [y0, y1];
};

export const rawClosestSilhouettePointGroup = (
  shapes: Shape<ad.Num>[],
  p: ad.Num[],
): ad.Num[] => {
  const closestSilhouettePoints = shapes.map((shape) =>
    rawClosestSilhouettePoint(shape, p),
  );
  const dist = closestSilhouettePoints.map((point) => ops.vdist(point, p));
  let closestX: ad.Num = Infinity;
  let closestY: ad.Num = Infinity;
  let minDist: ad.Num = Infinity;
  for (let i = 0; i < shapes.length; i++) {
    minDist = ifCond(lt(minDist, dist[i]), minDist, dist[i]);
    closestX = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][0],
      closestX,
    );
    closestY = ifCond(
      eq(minDist, dist[i]),
      closestSilhouettePoints[i][1],
      closestY,
    );
  }
  return [closestX, closestY];
};

//#endregion

/**
 * Given two perpendicular vectors `[startR, endR]` and `[startL, endL]`, return a path that describes a perpendicular mark between them.
 */
const perpPathFlat = (
  len: ad.Num,
  [startR, endR]: [ad.Num[], ad.Num[]],
  [startL, endL]: [ad.Num[], ad.Num[]],
): [ad.Num[], ad.Num[], ad.Num[]] => {
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

const tickPlacement = (
  padding: ad.Num,
  numPts: number,
  multiplier: ad.Num = 1,
): ad.Num[] => {
  if (numPts <= 0) throw Error(`number of ticks must be greater than 0`);
  const even = numPts % 2 === 0;
  const pts: ad.Num[] = even ? [div(padding, 2)] : [0];
  for (let i = 1; i < numPts; i++) {
    if (even && i === 1) multiplier = neg(multiplier);
    const shift =
      i % 2 === 0
        ? mul(padding, mul(neg(i), multiplier))
        : mul(padding, mul(i, multiplier));
    pts.push(add(pts[i - 1], shift));
  }
  return pts;
};

/**
 * Function to calculate the pairwise similarity matrix
 * for the original high-dimensional data.
 */
const calculateProbabilityMatrixHighDim = (x: ad.Num[][]): ad.Num[][] => {
  const m = x.length;
  const probabilities = Array(m)
    .fill(0)
    .map(() => Array(m).fill(0));

  for (let i = 0; i < m; i++) {
    for (let j = i + 1; j < m; j++) {
      const distance = ops.vdist(x[i], x[j]);
      const value = exp(neg(div(distance, 2)));
      probabilities[i][j] = value;
      probabilities[j][i] = value; // Symmetric
    }
  }

  // Normalize probabilities
  const sum = addN(probabilities.flat());
  return probabilities.map((row) => row.map((value) => div(value, sum)));
};

/**
 * Function to calculate the pairwise similarity matrix
 * for the low-dimensional representation of the data.
 */
const calculateProbabilityMatrixLowDim = (y: ad.Num[][]): ad.Num[][] => {
  const n = y.length;
  const probabilities = Array(n)
    .fill(0)
    .map(() => Array(n).fill(0));

  for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
      const distance = ops.vdist(y[i], y[j]);
      const value = div(1, add(1, distance));
      probabilities[i][j] = value;
      probabilities[j][i] = value; // Symmetric
    }
  }

  // Normalize probabilities
  const sum = addN(probabilities.flat());
  return probabilities.map((row) => row.map((value) => div(value, sum)));
};

/**
 * Function to calculate the Kullback-Leibler divergence
 * between two probability distributions.
 */
const calculateKLDivergence = (p: ad.Num[][], q: ad.Num[][]): ad.Num => {
  return addN(
    p.map((row, i) =>
      addN(
        row.map((p_ij, j) =>
          mul(
            p_ij,
            ln(div(add(p_ij, Number.EPSILON), add(q[i][j], Number.EPSILON))),
          ),
        ),
      ),
    ),
  );
};

/**
 * Function to calculate the t-SNE energy (cost)
 * for a given high-dimensional data and its low-dimensional representation.
 */
const calculateTsneEnergy = (x: ad.Num[][], y: ad.Num[][]): ad.Num => {
  const p = calculateProbabilityMatrixHighDim(x);
  const q = calculateProbabilityMatrixLowDim(y);

  return calculateKLDivergence(p, q);
};

/**
 *  Return the signed distance to an axis-aligned rectangle:
 *  float sdBox( in vec2 p, in vec2 b )
 *  {
 *    vec2 d = abs(p)-b;
 *    return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
 *  }
 */
export const sdfRect = (
  center: ad.Num[],
  width: ad.Num,
  height: ad.Num,
  p: ad.Num[],
): ad.Num => {
  const absp = ops.vabs(ops.vsub(p, center));
  const b = [div(width, 2), div(height, 2)];
  const d = ops.vsub(absp, b);
  return add(ops.vnorm(ops.vmax(d, [0.0, 0.0])), min(max(d[0], d[1]), 0.0));
};

const randn = ({ makeInput }: Context): ad.Num => {
  const u1 = makeInput({
    init: { tag: "Sampled", sampler: uniform(0, 1) },
    stages: new Set(),
  });
  const u2 = makeInput({
    init: { tag: "Sampled", sampler: uniform(0, 1) },
    stages: new Set(),
  });

  return mul(sqrt(mul(-2, ln(u1))), cos(mul(2 * Math.PI, u2)));
};

const catmullRom = (
  _context: Context,
  pathType: string,
  points: ad.Num[][],
  tension: ad.Num,
): PathDataV<ad.Num> => {
  const n = points.length;

  // compute tangents, assuming curve is closed
  const tangents: ad.Num[][] = [];
  for (let j = 0; j < n; j++) {
    const i = (j - 1 + n) % n;
    const k = (j + 1) % n;
    tangents[j] = ops.vmul(tension, ops.vsub(points[k], points[i]));
  }

  // if path is open, replace first/last tangents
  if (pathType === "open") {
    tangents[0] = ops.vmul(tension, ops.vsub(points[1], points[0]));
    tangents[n - 1] = ops.vmul(tension, ops.vsub(points[n - 1], points[n - 2]));
  }

  const path = new PathBuilder();
  path.moveTo(toPt(points[0]));
  const m = pathType === "open" ? n - 1 : n;
  for (let i = 0; i < m; i++) {
    const j = (i + 1) % n;
    path.bezierCurveTo(
      toPt(ops.vadd(points[i], tangents[i])),
      toPt(ops.vsub(points[j], tangents[j])),
      toPt(points[j]),
    );
  }

  if (pathType === "closed") path.closePath();

  return path.getPath();
};

const diffusionProcess = (
  _context: Context,
  n: number,
  X0: ad.Num[],
  A: ad.Num[][],
  omega: ad.Num[],
): ad.Num[][] => {
  const Xt: ad.Num[][] = [];
  Xt[0] = X0;
  for (let i = 1; i < n; i++) {
    const Wt = [randn(_context), randn(_context)];
    Xt[i] = ops.vadd(ops.vadd(Xt[i - 1], ops.mvmul(A, Wt)), omega);
  }

  return Xt;
};

// Returns the n x n identity matrix.
const identity = (n: number): ad.Num[][] => {
  const I: ad.Num[][] = [];
  for (let i = 0; i < n; i++) {
    I[i] = [];
    for (let j = 0; j < n; j++) {
      I[i][j] = i === j ? 1 : 0;
    }
  }
  return I;
};

// Given a vector v of length n, returns a
// diagonal matrix D with diagonal entries v.
const diagonal = (v: ad.Num[]): ad.Num[][] => {
  const n = v.length;
  const D: ad.Num[][] = [];
  for (let i = 0; i < n; i++) {
    D[i] = [];
    for (let j = 0; j < n; j++) {
      D[i][j] = i === j ? v[i] : 0;
    }
  }
  return D;
};

// Given a square matrix A, returns the sum
// of diagonal entries.
const trace = (A: ad.Num[][]): ad.Num => {
  const n = A.length;
  let sum: ad.Num = 0;
  for (let i = 0; i < n; i++) {
    sum = add(sum, A[i][i]);
  }
  return sum;
};

// Given a 2x2, 3x3, or 4x4 matrix A, returns its determinant.
const determinant = (A: ad.Num[][]): ad.Num => {
  const n = A.length;
  if (n === 2) {
    return sub(mul(A[0][0], A[1][1]), mul(A[0][1], A[1][0]));
  } else if (n === 3) {
    return add(
      add(
        mul(A[0][0], sub(mul(A[1][1], A[2][2]), mul(A[1][2], A[2][1]))),
        mul(A[0][1], sub(mul(A[1][2], A[2][0]), mul(A[1][0], A[2][2]))),
      ),
      mul(A[0][2], sub(mul(A[1][0], A[2][1]), mul(A[1][1], A[2][0]))),
    );
  } else if (n === 4) {
    const C00 = add(
      add(
        sub(
          mul(mul(A[1][2], A[2][3]), A[3][1]),
          mul(mul(A[1][3], A[2][2]), A[3][1]),
        ),
        sub(
          mul(mul(A[1][3], A[2][1]), A[3][2]),
          mul(mul(A[1][1], A[2][3]), A[3][2]),
        ),
      ),
      sub(
        mul(mul(A[1][1], A[2][2]), A[3][3]),
        mul(mul(A[1][2], A[2][1]), A[3][3]),
      ),
    );
    const C10 = add(
      add(
        sub(
          mul(mul(A[1][3], A[2][2]), A[3][0]),
          mul(mul(A[1][2], A[2][3]), A[3][0]),
        ),
        sub(
          mul(mul(A[1][0], A[2][3]), A[3][2]),
          mul(mul(A[1][3], A[2][0]), A[3][2]),
        ),
      ),
      sub(
        mul(mul(A[1][2], A[2][0]), A[3][3]),
        mul(mul(A[1][0], A[2][2]), A[3][3]),
      ),
    );
    const C20 = add(
      add(
        sub(
          mul(mul(A[1][1], A[2][3]), A[3][0]),
          mul(mul(A[1][3], A[2][1]), A[3][0]),
        ),
        sub(
          mul(mul(A[1][3], A[2][0]), A[3][1]),
          mul(mul(A[1][0], A[2][3]), A[3][1]),
        ),
      ),
      sub(
        mul(mul(A[1][0], A[2][1]), A[3][3]),
        mul(mul(A[1][1], A[2][0]), A[3][3]),
      ),
    );
    const C30 = add(
      add(
        sub(
          mul(mul(A[1][2], A[2][1]), A[3][0]),
          mul(mul(A[1][1], A[2][2]), A[3][0]),
        ),
        sub(
          mul(mul(A[1][0], A[2][2]), A[3][1]),
          mul(mul(A[1][2], A[2][0]), A[3][1]),
        ),
      ),
      sub(
        mul(mul(A[1][1], A[2][0]), A[3][2]),
        mul(mul(A[1][0], A[2][1]), A[3][2]),
      ),
    );
    return add(
      add(add(mul(A[0][0], C00), mul(A[0][1], C10)), mul(A[0][2], C20)),
      mul(A[0][3], C30),
    );
  } else {
    throw Error("matrix must be 2x2, 3x3, or 4x4");
  }

  return 0;
};

// Given a 2x2, 3x3, or 4x4 matrix A, returns its inverse.  If the matrix is
// not invertible, evaluation of this function within the optimizer
// may produce a numerically invalid matrix (with INF or NaN entries).
const inverse = (A: ad.Num[][]): ad.Num[][] => {
  const n = A.length;
  const C: ad.Num[][] = [];
  let detA: ad.Num = 0;

  if (n === 2) {
    C[0] = [A[1][1], neg(A[0][1])];
    C[1] = [neg(A[1][0]), A[0][0]];
    detA = add(mul(A[0][0], C[0][0]), mul(A[0][1], C[1][0]));
  } else if (n === 3) {
    C[0] = [
      sub(mul(A[1][1], A[2][2]), mul(A[1][2], A[2][1])),
      sub(mul(A[0][2], A[2][1]), mul(A[0][1], A[2][2])),
      sub(mul(A[0][1], A[1][2]), mul(A[0][2], A[1][1])),
    ];
    C[1] = [
      sub(mul(A[1][2], A[2][0]), mul(A[1][0], A[2][2])),
      sub(mul(A[0][0], A[2][2]), mul(A[0][2], A[2][0])),
      sub(mul(A[0][2], A[1][0]), mul(A[0][0], A[1][2])),
    ];
    C[2] = [
      sub(mul(A[1][0], A[2][1]), mul(A[1][1], A[2][0])),
      sub(mul(A[0][1], A[2][0]), mul(A[0][0], A[2][1])),
      sub(mul(A[0][0], A[1][1]), mul(A[0][1], A[1][0])),
    ];
    detA = add(
      add(mul(A[0][0], C[0][0]), mul(A[0][1], C[1][0])),
      mul(A[0][2], C[2][0]),
    );
  } else if (n === 4) {
    C[0] = [
      add(
        add(
          sub(
            mul(mul(A[1][2], A[2][3]), A[3][1]),
            mul(mul(A[1][3], A[2][2]), A[3][1]),
          ),
          sub(
            mul(mul(A[1][3], A[2][1]), A[3][2]),
            mul(mul(A[1][1], A[2][3]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[1][1], A[2][2]), A[3][3]),
          mul(mul(A[1][2], A[2][1]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][3], A[2][2]), A[3][1]),
            mul(mul(A[0][2], A[2][3]), A[3][1]),
          ),
          sub(
            mul(mul(A[0][1], A[2][3]), A[3][2]),
            mul(mul(A[0][3], A[2][1]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[0][2], A[2][1]), A[3][3]),
          mul(mul(A[0][1], A[2][2]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][2], A[1][3]), A[3][1]),
            mul(mul(A[0][3], A[1][2]), A[3][1]),
          ),
          sub(
            mul(mul(A[0][3], A[1][1]), A[3][2]),
            mul(mul(A[0][1], A[1][3]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[0][1], A[1][2]), A[3][3]),
          mul(mul(A[0][2], A[1][1]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][3], A[1][2]), A[2][1]),
            mul(mul(A[0][2], A[1][3]), A[2][1]),
          ),
          sub(
            mul(mul(A[0][1], A[1][3]), A[2][2]),
            mul(mul(A[0][3], A[1][1]), A[2][2]),
          ),
        ),
        sub(
          mul(mul(A[0][2], A[1][1]), A[2][3]),
          mul(mul(A[0][1], A[1][2]), A[2][3]),
        ),
      ),
    ];
    C[1] = [
      add(
        add(
          sub(
            mul(mul(A[1][3], A[2][2]), A[3][0]),
            mul(mul(A[1][2], A[2][3]), A[3][0]),
          ),
          sub(
            mul(mul(A[1][0], A[2][3]), A[3][2]),
            mul(mul(A[1][3], A[2][0]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[1][2], A[2][0]), A[3][3]),
          mul(mul(A[1][0], A[2][2]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][2], A[2][3]), A[3][0]),
            mul(mul(A[0][3], A[2][2]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][3], A[2][0]), A[3][2]),
            mul(mul(A[0][0], A[2][3]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[0][0], A[2][2]), A[3][3]),
          mul(mul(A[0][2], A[2][0]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][3], A[1][2]), A[3][0]),
            mul(mul(A[0][2], A[1][3]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][0], A[1][3]), A[3][2]),
            mul(mul(A[0][3], A[1][0]), A[3][2]),
          ),
        ),
        sub(
          mul(mul(A[0][2], A[1][0]), A[3][3]),
          mul(mul(A[0][0], A[1][2]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][2], A[1][3]), A[2][0]),
            mul(mul(A[0][3], A[1][2]), A[2][0]),
          ),
          sub(
            mul(mul(A[0][3], A[1][0]), A[2][2]),
            mul(mul(A[0][0], A[1][3]), A[2][2]),
          ),
        ),
        sub(
          mul(mul(A[0][0], A[1][2]), A[2][3]),
          mul(mul(A[0][2], A[1][0]), A[2][3]),
        ),
      ),
    ];
    C[2] = [
      add(
        add(
          sub(
            mul(mul(A[1][1], A[2][3]), A[3][0]),
            mul(mul(A[1][3], A[2][1]), A[3][0]),
          ),
          sub(
            mul(mul(A[1][3], A[2][0]), A[3][1]),
            mul(mul(A[1][0], A[2][3]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[1][0], A[2][1]), A[3][3]),
          mul(mul(A[1][1], A[2][0]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][3], A[2][1]), A[3][0]),
            mul(mul(A[0][1], A[2][3]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][0], A[2][3]), A[3][1]),
            mul(mul(A[0][3], A[2][0]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[0][1], A[2][0]), A[3][3]),
          mul(mul(A[0][0], A[2][1]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][1], A[1][3]), A[3][0]),
            mul(mul(A[0][3], A[1][1]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][3], A[1][0]), A[3][1]),
            mul(mul(A[0][0], A[1][3]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[0][0], A[1][1]), A[3][3]),
          mul(mul(A[0][1], A[1][0]), A[3][3]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][3], A[1][1]), A[2][0]),
            mul(mul(A[0][1], A[1][3]), A[2][0]),
          ),
          sub(
            mul(mul(A[0][0], A[1][3]), A[2][1]),
            mul(mul(A[0][3], A[1][0]), A[2][1]),
          ),
        ),
        sub(
          mul(mul(A[0][1], A[1][0]), A[2][3]),
          mul(mul(A[0][0], A[1][1]), A[2][3]),
        ),
      ),
    ];
    C[3] = [
      add(
        add(
          sub(
            mul(mul(A[1][2], A[2][1]), A[3][0]),
            mul(mul(A[1][1], A[2][2]), A[3][0]),
          ),
          sub(
            mul(mul(A[1][0], A[2][2]), A[3][1]),
            mul(mul(A[1][2], A[2][0]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[1][1], A[2][0]), A[3][2]),
          mul(mul(A[1][0], A[2][1]), A[3][2]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][1], A[2][2]), A[3][0]),
            mul(mul(A[0][2], A[2][1]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][2], A[2][0]), A[3][1]),
            mul(mul(A[0][0], A[2][2]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[0][0], A[2][1]), A[3][2]),
          mul(mul(A[0][1], A[2][0]), A[3][2]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][2], A[1][1]), A[3][0]),
            mul(mul(A[0][1], A[1][2]), A[3][0]),
          ),
          sub(
            mul(mul(A[0][0], A[1][2]), A[3][1]),
            mul(mul(A[0][2], A[1][0]), A[3][1]),
          ),
        ),
        sub(
          mul(mul(A[0][1], A[1][0]), A[3][2]),
          mul(mul(A[0][0], A[1][1]), A[3][2]),
        ),
      ),
      add(
        add(
          sub(
            mul(mul(A[0][1], A[1][2]), A[2][0]),
            mul(mul(A[0][2], A[1][1]), A[2][0]),
          ),
          sub(
            mul(mul(A[0][2], A[1][0]), A[2][1]),
            mul(mul(A[0][0], A[1][2]), A[2][1]),
          ),
        ),
        sub(
          mul(mul(A[0][0], A[1][1]), A[2][2]),
          mul(mul(A[0][1], A[1][0]), A[2][2]),
        ),
      ),
    ];
    detA = add(
      add(
        add(mul(A[0][0], C[0][0]), mul(A[0][1], C[1][0])),
        mul(A[0][2], C[2][0]),
      ),
      mul(A[0][3], C[3][0]),
    );
  } else {
    throw Error("matrix must be 2x2, 3x3, or 4x4");
  }

  return ops.msdiv(C, detA);
};

// Given a vector q of length n+1, encoding a point in n-dimensional homogeneous coordinates,
// returns a vector p of length n, encoding the same point in Cartesian coordinates.
const fromHomogeneous = (q: ad.Num[]): ad.Num[] => {
  const n = q.length - 1;
  const p: ad.Num[] = [];
  for (let i = 0; i < n; i++) {
    p[i] = div(q[i], q[n]);
  }
  return p;
};

// Given a vector p of length n, encoding a point in n-dimensional Cartesian coordinates,
// returns a vector q of length n+1, encoding the same point in homogeneous coordinates.
const toHomogeneous = (p: ad.Num[]): ad.Num[] => {
  const n = p.length;
  const q: ad.Num[] = [];
  for (let i = 0; i < n; i++) {
    q[i] = p[i];
  }
  q[n] = 1;
  return q;
};

// Given a square n x n matrix A representing a spatial transformation in n
// dimensions, returns an (n+1) x (n+1) matrix representing the same
// transformation in homogeneous coordinates.
const toHomogeneousMatrix = (A: ad.Num[][]): ad.Num[][] => {
  const n = A.length;
  const B: ad.Num[][] = [];
  for (let i = 0; i < n + 1; i++) {
    B[i] = [];
    for (let j = 0; j < n + 1; j++) {
      B[i][j] = 0;
    }
  }
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      B[i][j] = A[i][j];
    }
  }
  B[n][n] = 1;
  return B;
};

// Given n-dimensional vectors u and v, returns the n x n matrix
// given by their outer product.
const outer = (u: ad.Num[], v: ad.Num[]): ad.Num[][] => {
  if (u.length !== v.length) {
    throw Error("vectors must have equal length");
  }

  const A: ad.Num[][] = [];
  for (let i = 0; i < u.length; i++) {
    const row = v.map((e) => mul(u[i], e));
    A.push(row);
  }

  return A;
};

// Given angles `ax` and `ay`, returns a 2x2 matrix
// skewing an element on the 2D plane.
const skew = (ax: ad.Num, ay: ad.Num): ad.Num[][] => {
  return [
    [1, tan(ax)],
    [tan(ay), 1],
  ];
};

// Given a 3-vector v, returns a 3x3 skew symmetric
// matrix v̂ such that v̂u = v x u for any vector u.
const crossProductMatrix = (v: ad.Num[]): ad.Num[][] => {
  if (v.length !== 3) {
    throw Error("vector must have length 3");
  }

  return [
    [0, neg(v[2]), v[1]],
    [v[2], 0, neg(v[0])],
    [neg(v[1]), v[0], 0],
  ];
};

// Returns the 2x2 matrix representing a rotation by a given angle theta.
const rotate2d = (theta: ad.Num): ad.Num[][] => {
  return [
    [cos(theta), neg(sin(theta))],
    [sin(theta), cos(theta)],
  ];
};

// Given an angle theta and a unit 3-vector v, returns
// the 3x3 matrix representing a rotation by theta around v.
const rotate3d = (theta: ad.Num, v: ad.Num[]): ad.Num[][] => {
  if (v.length !== 3) {
    throw Error("rotation axis must have length 3");
  }

  // Construct matrix via Rodrigues' formula
  //    I + sin(θ)v̂ + (1-cos(θ))v̂²
  // where v̂ is the skew-symmetric matrix such
  // that v̂u = v x u for any vector u.
  const I = identity(3);
  const vhat = crossProductMatrix(v);
  return ops.mmadd(
    ops.mmadd(I, ops.smmul(sin(theta), vhat)),
    ops.smmul(sub(1, cos(theta)), ops.mmmul(vhat, vhat)),
  );
};

// Returns a 2x2 matrix representing nonuniform scaling by factors sx, sy
// along x, y axes, respectively.
const scale2d = (sx: ad.Num, sy: ad.Num): ad.Num[][] => {
  return diagonal([sx, sy]);
};

// Returns a 3x3 matrix representing nonuniform scaling by factors sx, sy, sz
// along x, y, z axes, respectively.
const scale3d = (sx: ad.Num, sy: ad.Num, sz: ad.Num): ad.Num[][] => {
  return diagonal([sx, sy, sz]);
};

// Given n-dimensional vectors u and v, returns an n x n matrix A such that
// Ax displaces any given point x in the direction u according to its extent
// along the direction v, i.e., Ax = x + <v,x>u.
const shear = (u: ad.Num[], v: ad.Num[]): ad.Num[][] => {
  if (u.length !== v.length) {
    throw Error("vectors must have equal length");
  }

  const n = v.length;
  const I = identity(n);
  return ops.mmadd(I, outer(u, v));
};

// Given an n-dimensional vector v, returns an (n+1)x(n+1) matrix representing a
// translation of n-dimensional space by v, encoded in homogeneous coordinates.
const translate = (v: ad.Num[]): ad.Num[][] => {
  const n = v.length;
  const A = identity(n + 1);
  for (let i = 0; i < n; i++) {
    A[i][n] = v[i];
  }
  return A;
};

// (adapted from gluLookAt man page:)
// lookAt returns a 4x4 viewing matrix derived from an eye point, a reference point
// indicating the center of the scene, and an up vector.  The matrix maps the
// reference point to the negative z axis and the eye point to the origin. When
// a typical projection matrix is used, the center of the scene therefore maps
// to the center of the viewport. Similarly, the direction described by the up
// vector projected onto the viewing plane is mapped to the positive y axis so
// that it points upward in the viewport. The up vector must not be parallel to
// the line of sight from the eye point to the reference point.
const lookAt = (eye: ad.Num[], center: ad.Num[], up: ad.Num[]): ad.Num[][] => {
  if (eye.length !== 3) {
    throw Error("eye vector must have length 3");
  }
  if (center.length !== 3) {
    throw Error("center vector must have length 3");
  }
  if (up.length !== 3) {
    throw Error("up vector must have length 3");
  }

  const forward = ops.vnormalize(ops.vsub(center, eye));
  const side = ops.vnormalize(ops.cross3(forward, up));
  const vert = ops.cross3(side, forward);

  const M: ad.Num[][] = [];
  for (let i = 0; i < 4; i++) {
    M[i] = [];
  }

  M[0][0] = side[0];
  M[0][1] = side[1];
  M[0][2] = side[2];
  M[0][3] = 0;

  M[1][0] = vert[0];
  M[1][1] = vert[1];
  M[1][2] = vert[2];
  M[1][3] = 0;

  M[2][0] = neg(forward[0]);
  M[2][1] = neg(forward[1]);
  M[2][2] = neg(forward[2]);
  M[2][3] = 0;

  M[3][0] = 0;
  M[3][1] = 0;
  M[3][2] = 0;
  M[3][3] = 1;

  const T = translate(ops.vneg(eye));
  return ops.mmmul(M, T);
};

// (adapted from gluPerspective man page:)
// perspective sets up a 4x4 perspective projection matrix.
// The aspect ratio should match the aspect ratio of the associated viewport.
// For example, aspect = 2.0 means the viewer's angle of view is twice as wide
// in x as it is in y.  If the viewport is twice as wide as it is tall, it displays
// the image without distortion.
// fovy    Specifies the field of view angle, in degrees, in the y direction.
// aspect  Specifies the aspect ratio that determines the field of view in the x direction.  The
//         aspect ratio is the ratio of x (width) to y (height).
// zNear   Specifies the distance from the viewer to the near clipping plane (always positive).
// zFar    Specifies the distance from the viewer to the far clipping plane (always positive).
const perspective = (
  fovy: ad.Num,
  aspect: ad.Num,
  zNear: ad.Num,
  zFar: ad.Num,
): ad.Num[][] => {
  const radians = mul(Math.PI / 180, fovy);
  const f = div(radians, 2);
  const deltaZ = sub(zFar, zNear);
  const cotangent = div(1, tan(f));

  const M = identity(4);
  M[0][0] = div(cotangent, aspect);
  M[1][1] = neg(cotangent);
  M[2][2] = div(neg(add(zFar, zNear)), deltaZ);
  M[3][2] = -1;
  M[2][3] = mul(2, div(mul(zNear, zFar), deltaZ));
  M[3][3] = 0;

  return M;
};

// (adapted from glOrtho man page:)
// ortho describes a 4x4 transformation that produces a parallel projection.
// left, right Specify the coordinates for the left and right vertical clipping planes.
// bottom, top Specify the coordinates for the bottom and top horizontal clipping planes.
// zNear, zFar Specify the distances to the nearer and farther depth clipping planes.  These values
//             are negative if the plane is to be behind the viewer.
const ortho = (
  Left: ad.Num,
  Right: ad.Num,
  Bottom: ad.Num,
  Top: ad.Num,
  zNear: ad.Num,
  zFar: ad.Num,
): ad.Num[][] => {
  const M = identity(4);
  M[0][0] = div(2, sub(Right, Left));
  M[1][1] = div(2, sub(Top, Bottom));
  M[2][2] = div(-2, sub(zFar, zNear));
  M[0][3] = div(add(Right, Left), sub(Left, Right));
  M[1][3] = div(add(Top, Bottom), sub(Bottom, Top));
  M[2][3] = div(add(zFar, zNear), sub(zNear, zFar));
  return M;
};

// (adapted from gluProject man page:)
// project transforms the specified object coordinates into window coordinates using a given
// model and projection transformation, and a given viewport.  It returns the projected x,y
// coordinates, as well as the depth relative to the view.
// p       3D object coordinates (x,y,z)
// model   4x4 modelview matrix
// proj    4x4 projection matrix
// view    viewport (x, y, width, height)
const project = (
  p: ad.Num[],
  modelMatrix: ad.Num[][],
  projMatrix: ad.Num[][],
  viewport: ad.Num[],
): ad.Num[] => {
  // apply modelview and projection transformations
  const q = ops.mvmul(projMatrix, ops.mvmul(modelMatrix, toHomogeneous(p)));

  // homogeneous divide by w to get x, y, z
  let r = ops.vdiv(q, q[3]).slice(0, 3);

  // map x, y and z to range 0-1
  r = ops.vadd(ops.vmul(0.5, r), [0.5, 0.5, 0.5]);

  // map x,y to viewport
  r[0] = add(mul(r[0], viewport[2]), viewport[0]);
  r[1] = add(mul(r[1], viewport[3]), viewport[1]);

  // return x, y, and depth
  return r;
};

// Construct a 3x3 matrix with entries
// [ a c e ]
// [ b d f ]
// [ 0 0 1 ]
// (Note: this function corresponds to the SVG/CSS `matrix` function.)
const matrix = (
  a: ad.Num,
  b: ad.Num,
  c: ad.Num,
  d: ad.Num,
  e: ad.Num,
  f: ad.Num,
): ad.Num[][] => {
  return [
    [a, c, e],
    [b, d, f],
    [0, 0, 1],
  ];
};

// Construct a 4x4 matrix with entries
// [ a1 a2 a3 a4 ]
// [ b1 b2 b3 b4 ]
// [ c1 c2 c3 c4 ]
// [ d1 d2 d3 d4 ]
// (Note: this function corresponds to the CSS `matrix3d` function.)
const matrix3d = (
  a1: ad.Num,
  b1: ad.Num,
  c1: ad.Num,
  d1: ad.Num,
  a2: ad.Num,
  b2: ad.Num,
  c2: ad.Num,
  d2: ad.Num,
  a3: ad.Num,
  b3: ad.Num,
  c3: ad.Num,
  d3: ad.Num,
  a4: ad.Num,
  b4: ad.Num,
  c4: ad.Num,
  d4: ad.Num,
): ad.Num[][] => {
  return [
    [a1, a2, a3, a4],
    [b1, b2, b3, b4],
    [c1, c2, c3, c4],
    [d1, d2, d3, d4],
  ];
};

const offsetPolygon = (
  p: ad.Num[][], // vertices
  w: ad.Num, // width
): ad.Num[][] => {
  const n = p.length;

  // unit edge vectors
  const e = [];
  for (let i = 0; i < n; i++) {
    const j = (i + 1) % n;
    e[i] = ops.vnormalize(ops.vsub(p[j], p[i]));
  }

  // unit vertex normals
  const v = [];
  for (let i = 0; i < n; i++) {
    const k = (i + n - 1) % n;
    v[i] = ops.vnormalize(ops.rot90(ops.vadd(e[i], e[k])));
  }

  // offset polygon
  const q = [];
  for (let i = 0; i < n; i++) {
    const r = div(w, ops.cross2(e[i], v[i]));
    q[i] = ops.vsub(p[i], ops.vmul(r, v[i]));
  }

  return q;
};
