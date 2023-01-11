import { bboxFromShape } from "contrib/Queries";
import { clamp, inRange, numOf } from "contrib/Utils";
import { ops } from "engine/Autodiff";
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
} from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import _ from "lodash";
import { PathBuilder } from "renderer/PathBuilder";
import { Ellipse } from "shapes/Ellipse";
import { Line } from "shapes/Line";
import { Polyline } from "shapes/Polyline";
import { Context, uniform } from "shapes/Samplers";
import { shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import { Computation } from "types/functions";
import {
  ArgVal,
  Color,
  ColorV,
  FloatV,
  PathDataV,
  PtListV,
  StrV,
  TupV,
  Value,
  VectorV,
} from "types/value";
import { getStart, linePts } from "utils/Util";

// NOTE: These all need to be written in terms of autodiff types
// These all return a Value<ad.Num>
const makePath: Computation<PathDataV<ad.Num>> = {
  name: "makePath",
  documentation: `See https://github.com/penrose/penrose/issues/716`,
  returns: "path",
  arguments: [
    { name: "start", description: "Start point of the path", type: "r2" },
    { name: "end", description: "End point of the path", type: "r2" },
    { name: "curveHeight", description: "Height of the curve", type: "real" },
    {
      name: "padding",
      description: "Padding between the curve and the labels",
      type: "real",
    },
  ],
  definition: (
    _context: Context,
    start: [ad.Num, ad.Num],
    end: [ad.Num, ad.Num],
    curveHeight: ad.Num,
    padding: ad.Num
  ): PathDataV<ad.Num> => {
    // Two vectors for moving from `start` to the control point: `unit` is the direction of vector [start, end] (along the line passing through both labels) and `normalVec` is perpendicular to `unit` through the `rot90` operation.
    const unit: ad.Num[] = ops.vnormalize(ops.vsub(start, end));
    const normalVec: ad.Num[] = ops.rot90(toPt(unit));
    // There's only one control point in a quadratic bezier curve, and we want it to be equidistant to both `start` and `end`
    const halfLen: ad.Num = div(ops.vdist(start, end), 2);
    const controlPt: ad.Num[] = ops.vmove(
      ops.vmove(end, halfLen, unit),
      curveHeight,
      normalVec
    );
    const curveEnd: ad.Num[] = ops.vmove(end, padding, unit);
    // Both the start and end points of the curve should be padded by some distance such that they don't overlap with the texts
    const path = new PathBuilder();
    return path
      .moveTo(toPt(ops.vmove(start, padding, ops.vneg(unit))))
      .quadraticCurveTo(toPt(controlPt), toPt(curveEnd))
      .getPath();
  },
};

const get: Computation<FloatV<ad.Num>> = {
  name: "get",
  documentation:
    "Return `i`th element of list `xs, assuming lists only hold floats.",
  arguments: [
    { name: "xs", description: "List of floats", type: "rn" },
    { name: "i", description: "Index of the element to return", type: "nat" },
  ],
  returns: "real",
  definition: (_context: Context, xs: ad.Num[], i: number): FloatV<ad.Num> => {
    const res = xs[i];
    return {
      tag: "FloatV",
      contents: res,
    };
  },
};

const rgba: Computation<ColorV<ad.Num>> = {
  name: "rgba",
  documentation:
    "Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).",
  returns: "color",
  arguments: [
    { name: "r", description: "Red", type: "unit" },
    { name: "g", description: "Green", type: "unit" },
    { name: "b", description: "Blue", type: "unit" },
    { name: "a", description: "Opacity", type: "unit" },
  ],
  definition: (
    _context: Context,
    r: ad.Num,
    g: ad.Num,
    b: ad.Num,
    a: ad.Num
  ): ColorV<ad.Num> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "RGBA",
        contents: [r, g, b, a],
      },
    };
  },
};

const selectColor: Computation<ColorV<ad.Num>> = {
  name: "selectColor",
  documentation: "TODO",
  returns: "color",
  arguments: [
    { name: "color1", description: "First color", type: "color" },
    { name: "color2", description: "Second color", type: "color" },
    { name: "level", description: "Level", type: "real" },
  ],
  definition: (
    _context: Context,
    color1: Color<ad.Num>,
    color2: Color<ad.Num>,
    level: ad.Num
  ): ColorV<ad.Num> => {
    const half = div(level, 2);
    const even = eq(half, trunc(half)); // autodiff doesn't have a mod operator
    if (!(color1.tag === "RGBA" && color2.tag === "RGBA")) {
      throw Error("selectColor only supports RGBA");
    }
    return {
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
    };
  },
};

const hsva: Computation<ColorV<ad.Num>> = {
  name: "hsva",
  documentation:
    "Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).",
  returns: "color",
  arguments: [
    { name: "h", description: "Hue", type: "unit" },
    { name: "s", description: "Saturation", type: "unit" },
    { name: "v", description: "Value", type: "unit" },
    { name: "a", description: "Opacity", type: "unit" },
  ],
  definition: (
    _context: Context,
    h: ad.Num,
    s: ad.Num,
    v: ad.Num,
    a: ad.Num
  ): ColorV<ad.Num> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "HSVA",
        contents: [h, s, v, a],
      },
    };
  },
};

const none: Computation<ColorV<ad.Num>> = {
  name: "none",
  documentation: "Return a paint of none (no paint)",
  returns: "color",
  arguments: [],
  definition: (_context: Context): ColorV<ad.Num> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "NONE",
      },
    };
  },
};

const acoshComputation: Computation<FloatV<ad.Num>> = {
  name: "acosh",
  documentation: "Return `acosh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: acosh(x),
    };
  },
};

const acosComputation: Computation<FloatV<ad.Num>> = {
  name: "acos",
  documentation: "Return `acos(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: acos(x),
    };
  },
};

const asinComputation: Computation<FloatV<ad.Num>> = {
  name: "asin",
  documentation: "Return `asin(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: asin(x) };
  },
};

const asinhComputation: Computation<FloatV<ad.Num>> = {
  name: "asinh",
  documentation: "Return `asinh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: asinh(x) };
  },
};

const atanComputation: Computation<FloatV<ad.Num>> = {
  name: "atan",
  documentation: "Return `atan(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: atan(x) };
  },
};

const atan2Computation: Computation<FloatV<ad.Num>> = {
  name: "atan2",
  documentation: "Return `atan2(x, y)`.",
  returns: "real",
  arguments: [
    { name: "x", description: "`x`", type: "real" },
    { name: "y", description: "`y`", type: "real" },
  ],
  definition: (_context: Context, x: ad.Num, y: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: atan2(x, y) };
  },
};

const atanhComputation: Computation<FloatV<ad.Num>> = {
  name: "atanh",
  documentation: "Return `atanh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: atanh(x) };
  },
};

const cbrtComputation: Computation<FloatV<ad.Num>> = {
  name: "cbrt",
  documentation: "Return `cbrt(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: cbrt(x) };
  },
};

const ceilComputation: Computation<FloatV<ad.Num>> = {
  name: "ceil",
  documentation: "Return `ceil(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ceil(x) };
  },
};

const cosComputation: Computation<FloatV<ad.Num>> = {
  name: "cos",
  documentation: "Return `cos(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: cos(x) };
  },
};

const coshComputation: Computation<FloatV<ad.Num>> = {
  name: "cosh",
  documentation: "Return `cosh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: cosh(x) };
  },
};

const expComputation: Computation<FloatV<ad.Num>> = {
  name: "exp",
  documentation: "Return `exp(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: exp(x),
    };
  },
};

const expm1Computation: Computation<FloatV<ad.Num>> = {
  name: "expm1",
  documentation: "Return `expm1(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: expm1(x) };
  },
};

const floorComputation: Computation<FloatV<ad.Num>> = {
  name: "floor",
  documentation: "Return `floor(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: floor(x) };
  },
};

const logComputation: Computation<FloatV<ad.Num>> = {
  name: "log",
  documentation: "Return `log(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: ln(x),
    };
  },
};

const log2Computation: Computation<FloatV<ad.Num>> = {
  name: "log2",
  documentation: "Return `log2(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: log2(x) };
  },
};

const log10Computation: Computation<FloatV<ad.Num>> = {
  name: "log10",
  documentation: "Return `log10(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: log10(x) };
  },
};

const log1pComputation: Computation<FloatV<ad.Num>> = {
  name: "log1p",
  documentation: "Return `log1p(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: log1p(x) };
  },
};

const powComputation: Computation<FloatV<ad.Num>> = {
  name: "pow",
  documentation: "Return `pow(x, y)`.",
  returns: "real",
  arguments: [
    { name: "x", description: "`x`", type: "real" },
    { name: "y", description: "`y`", type: "real" },
  ],
  definition: (_context: Context, x: ad.Num, y: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: pow(x, y) };
  },
};

const roundComputation: Computation<FloatV<ad.Num>> = {
  name: "round",
  documentation: "Return `round(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: round(x) };
  },
};

const signComputation: Computation<FloatV<ad.Num>> = {
  name: "sign",
  documentation: "Return `sign(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: sign(x) };
  },
};

const sinComputation: Computation<FloatV<ad.Num>> = {
  name: "sin",
  documentation: "Return `sin(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: sin(x) };
  },
};

const sinhComputation: Computation<FloatV<ad.Num>> = {
  name: "sinh",
  documentation: "Return `sinh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: sinh(x) };
  },
};

const tanComputation: Computation<FloatV<ad.Num>> = {
  name: "tan",
  documentation: "Return `tan(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: tan(x) };
  },
};

const tanhComputation: Computation<FloatV<ad.Num>> = {
  name: "tanh",
  documentation: "Return `tanh(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: tanh(x) };
  },
};

const truncComputation: Computation<FloatV<ad.Num>> = {
  name: "trunc",
  documentation: "Return `trunc(x)`.",
  returns: "real",
  arguments: [{ name: "x", description: "`x`", type: "real" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: trunc(x) };
  },
};

const dotComputation: Computation<FloatV<ad.Num>> = {
  name: "dot",
  documentation: "Return the dot product of `v` and `w`.",
  returns: "real",
  arguments: [
    { name: "v", description: "Vector `v`", type: "rn" },
    { name: "w", description: "Vector `w`", type: "rn" },
  ],
  definition: (
    _context: Context,
    v: VectorV<ad.Num>,
    w: VectorV<ad.Num>
  ): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ops.vdot(v, w) };
  },
};

const lengthComputation: Computation<FloatV<ad.Num>> = {
  name: "length",
  documentation: "Return the length of the line or arrow shape `[type, props]`",
  returns: "real",
  arguments: [{ name: "l", description: "A line", type: "line" }],
  definition: (
    _context: Context,
    [t, props]: [string, any]
  ): FloatV<ad.Num> => {
    if (!shapedefs[t].isLinelike) {
      throw Error("length expects a line-like shape");
    } else {
      const [p1, p2] = linePts(props);
      return {
        tag: "FloatV",
        contents: ops.vdist(p1, p2),
      };
    }
  },
};

const concatComputation: Computation<StrV> = {
  definition: (_context: Context, ...strings: string[]): StrV => {
    return {
      tag: "StrV",
      contents: strings.join(""),
    };
  },
  documentation: "Concatenate a list of strings",
  name: "concat",
  returns: "string",
  arguments: [
    {
      name: "...strings",
      description: "A list of strings (kwargs)",
      type: "stringlist",
    },
  ],
};

const normalizeComputation: Computation<VectorV<ad.Num>> = {
  name: "normalize",
  documentation: "Return the normalized version of vector `v`.",
  arguments: [{ type: "rn", name: "v", description: "Vector `v`" }],
  returns: "rn",
  definition: (_context: Context, v: ad.Num[]): VectorV<ad.Num> => {
    return {
      tag: "VectorV",
      contents: ops.vnormalize(v),
    };
  },
};

const pathFromPoints: Computation<PathDataV<ad.Num>> = {
  name: "pathFromPoints",
  documentation:
    "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",
  arguments: [
    { name: "pathType", type: "pathtype", description: "Path Type" },
    { name: "pts", type: "r2n", description: "List of points" },
  ],
  returns: "path",
  definition: (
    _context: Context,
    pathType: string,
    pts: ad.Pt2[]
  ): PathDataV<ad.Num> => {
    const path = new PathBuilder();
    const [start, ...tailpts] = pts;
    path.moveTo(start);
    tailpts.forEach((pt: ad.Pt2) => path.lineTo(pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },
};

const quadraticCurveFromPoints: Computation<PathDataV<ad.Num>> = {
  name: "quadraticCurveFromPoints",
  documentation:
    "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",
  arguments: [
    { name: "pathType", type: "pathtype", description: "Path Type" },
    { name: "pts", type: "r2n", description: "List of points" },
  ],
  returns: "path",
  definition: (
    _context: Context,
    pathType: string,
    pts: ad.Pt2[]
  ): PathDataV<ad.Num> => {
    const path = new PathBuilder();
    const [start, cp, second, ...tailpts] = pts;
    path.moveTo(start);
    path.quadraticCurveTo(cp, second);
    tailpts.forEach((pt: ad.Pt2) => path.quadraticCurveJoin(pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },
};

const interpolateQuadraticFromPoints: Computation<PathDataV<ad.Num>> = {
  name: "interpolateQuadraticFromPoints",
  documentation: `Draw a curve interpolating three given points.
  (Note that this is different from specifying the three control points of a quadratic Bézier curve, since a Bézier does not interpolate the middle control point.)`,
  arguments: [
    { name: "pathType", type: "pathtype", description: "Path Type" },
    { name: "p0", type: "r2", description: "First point" },
    { name: "p1", type: "r2", description: "Second point" },
    { name: "p2", type: "r2", description: "Third point" },
  ],
  returns: "path",
  definition: (
    _context: Context,
    pathType: string,
    p0: ad.Pt2,
    p1: ad.Pt2,
    p2: ad.Pt2
  ): PathDataV<ad.Num> => {
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
    return path.getPath();
  },
};

const cubicCurveFromPoints: Computation<PathDataV<ad.Num>> = {
  name: "cubicCurveFromPoints",
  documentation:
    "Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.",
  arguments: [
    { type: "pathtype", name: "pathType", description: "Path type" },
    { type: "r2n", name: "pts", description: "List of points" },
  ],
  returns: "path",
  definition: (
    _context: Context,
    pathType: string,
    pts: ad.Pt2[]
  ): PathDataV<ad.Num> => {
    const path = new PathBuilder();
    const [start, cp1, cp2, second, ...tailpts] = pts;
    path.moveTo(start);
    path.bezierCurveTo(cp1, cp2, second);
    _.chunk(tailpts, 2).forEach(([cp, pt]) => path.cubicCurveJoin(cp, pt));
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },
};

const arc: Computation<PathDataV<ad.Num>> = {
  name: "arc",
  documentation: `Return series of elements that can render an arc SVG. See: https://css-tricks.com/svg-path-syntax-illustrated-guide/ for the "A" spec. Returns elements that can be passed to Path shape spec to render an SVG arc.`,
  returns: "path",
  arguments: [
    {
      name: "pathType",
      type: "pathtype",
      description: `The path type: either "open" or "closed." whether the SVG should automatically draw a line between the final point and the start point`,
    },
    {
      name: "start",
      type: "r2",
      description: "coordinate to start drawing the arc",
    },
    {
      name: "end",
      type: "r2",
      description: "coordinate to finish drawing the arc",
    },
    {
      name: "[width, height]",
      type: "r2",
      description: "width and height of the ellipse to draw the arc along",
    },
    {
      name: "rotation",
      type: "real",
      description: "angle in degrees to rotate ellipse about its center",
    },
    {
      name: "largeArc",
      type: "real",
      description: "0 to draw shorter of 2 arcs, 1 to draw longer",
    },
    {
      name: "arcSweep",
      type: "real",
      description: "0 to rotate CCW, 1 to rotate CW",
    },
  ],
  definition: (
    _context: Context,
    pathType: string,
    start: ad.Pt2,
    end: ad.Pt2,
    radius: ad.Pt2,
    rotation: ad.Num,
    largeArc: ad.Num,
    arcSweep: ad.Num
  ): PathDataV<ad.Num> => {
    const path = new PathBuilder();
    path.moveTo(start).arcTo(radius, end, [rotation, largeArc, arcSweep]);
    if (pathType === "closed") path.closePath();
    return path.getPath();
  },
};

const repeatedArcs: Computation<PathDataV<ad.Num>> = {
  name: "repeatedArcs",
  documentation:
    "Generate multiple concentric arcs. Useful for denoting equal angles.",
  returns: "path",
  arguments: [
    {
      name: "innerStart",
      type: "r2",
      description: "coordinate to start drawing the inner arc",
    },
    {
      name: "innerEnd",
      type: "r2",
      description: "coordinate to end the inner arc",
    },
    {
      name: "outerStart",
      type: "r2",
      description: "coordinate to start drawing the outer arc",
    },
    {
      name: "outerEnd",
      type: "r2",
      description: "coordinate to end the outer arc",
    },
    {
      name: "innerRadius",
      type: "r2",
      description:
        "radii of the ellipse to draw the inner arc along (width, height)",
    },
    {
      name: "repeat",
      type: "posint",
      description: "number of times to repeat the arc",
    },
    {
      name: "spacing",
      type: "real",
      description: "spacing between arcs",
    },
    {
      name: "arcSweep",
      type: "real",
      description: "arc length to sweep",
    },
  ],
  definition: (
    _context: Context,
    innerStart: ad.Pt2,
    innerEnd: ad.Pt2,
    outerStart: ad.Pt2,
    outerEnd: ad.Pt2,
    innerRadius: ad.Pt2,
    repeat: ad.Num,
    spacing: ad.Num,
    arcSweep: ad.Num
  ): PathDataV<ad.Num> => {
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
    return path.getPath();
  },
};

const wedge: Computation<PathDataV<ad.Num>> = {
  name: "wedge",
  documentation: `Return series of elements that render a "wedge", which is the same as the arc above except that it's connected to the circle center and filled. Returns elements that can be passed to Path shape spec to render an SVG arc.`,
  returns: "path",
  arguments: [
    {
      name: "center",
      type: "r2",
      description: "center of the circle on which the arc sits",
    },
    {
      name: "start",
      type: "r2",
      description: "coordinate to start drawing the arc",
    },
    {
      name: "end",
      type: "r2",
      description: "coordinate to finish drawing the arc",
    },
    {
      name: "radius",
      type: "r2",
      description:
        "width and height of the ellipse to draw the arc along (i.e. [width, height])",
    },
    {
      name: "rotation",
      type: "real",
      description: "angle in degrees to rotate ellipse about its center",
    },
    {
      name: "largeArc",
      type: "real",
      description: "0 to draw shorter of 2 arcs, 1 to draw longer",
    },
    {
      name: "arcSweep",
      type: "real",
      description: "0 to rotate CCW, 1 to rotate CW",
    },
  ],
  definition: (
    _context: Context,
    center: ad.Pt2,
    start: ad.Pt2,
    end: ad.Pt2,
    radius: ad.Pt2,
    rotation: ad.Num,
    largeArc: ad.Num,
    arcSweep: ad.Num
  ): PathDataV<ad.Num> => {
    const path = new PathBuilder();
    path
      .moveTo(start)
      .arcTo(radius, end, [rotation, largeArc, arcSweep])
      .lineTo(center);
    path.closePath();
    return path.getPath();
  },
};

const ptOnLine: Computation<VectorV<ad.Num>> = {
  name: "ptOnLine",
  documentation:
    "Find the point that is located at dist r along a line between p1 and p2. Returns vector representation of the point of intersection.",
  returns: "rn",
  arguments: [
    { name: "p1", type: "rn", description: "start point of line segment" },
    { name: "p2", type: "rn", description: "endpoint of line segment" },
    {
      name: "r",
      type: "real",
      description: "distance from p1 to travel along the line",
    },
  ],
  definition: (
    _context: Context,
    p1: ad.Num[],
    p2: ad.Num[],
    r: ad.Num
  ): VectorV<ad.Num> => {
    // find unit vector pointing towards v2
    const unit = ops.vnormalize(ops.vsub(p2, p1));
    return { tag: "VectorV", contents: ops.vmove(p1, r, unit) };
  },
};

const arcSweepFlag: Computation<FloatV<ad.Num>> = {
  name: "arcSweepFlag",
  documentation:
    "Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.",
  returns: "real",
  arguments: [
    {
      name: "p1",
      type: "r2",
      description:
        "x, y coordinates of the circle/ellipse that the arc is drawn on",
    },
    { name: "start", type: "r2", description: "start point of the arc" },
    { name: "end", type: "r2", description: "end point of the arc" },
  ],
  definition: (
    _context: Context,
    [x1, y1]: ad.Num[],
    start: ad.Pt2,
    end: ad.Pt2
  ): FloatV<ad.Num> => {
    const st = ops.vnormalize([sub(start[0], x1), sub(start[1], y1)]);
    const en = ops.vnormalize([sub(end[0], x1), sub(end[1], y1)]);
    const cross = ops.cross2(st, en);
    return {
      tag: "FloatV",
      contents: ifCond(gt(cross, 0), 0, 1),
    };
  },
};

const angleBetween: Computation<FloatV<ad.Num>> = {
  name: "angleBetween",
  documentation:
    "Return the unsigned angle between vectors `u, v`, in radians. Assumes that both u and v have nonzero magnitude. The returned value will be in the range [0,pi].",
  returns: "real",
  arguments: [
    { name: "u", type: "rn", description: "A vector" },
    { name: "v", type: "rn", description: "A vector" },
  ],
  definition: (_context: Context, u: ad.Num[], v: ad.Num[]): FloatV<ad.Num> => {
    const theta = ops.angleBetween(u, v);
    return {
      tag: "FloatV",
      contents: theta,
    };
  },
};

const angleFrom: Computation<FloatV<ad.Num>> = {
  name: "angleFrom",
  documentation:
    "Return the signed angle from vector `u` to vector `v`, in radians. Assumes that both u and v are 2D vectors and have nonzero magnitude. The returned value will be in the range [-pi,pi].",
  returns: "real",
  arguments: [
    { name: "u", type: "rn", description: "A vector" },
    { name: "v", type: "rn", description: "A vector" },
  ],
  definition: (_context: Context, u: ad.Num[], v: ad.Num[]): FloatV<ad.Num> => {
    const theta = ops.angleFrom(u, v);
    return {
      tag: "FloatV",
      contents: theta,
    };
  },
};

const cross2D: Computation<FloatV<ad.Num>> = {
  name: "cross2D",
  documentation:
    "Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]",
  returns: "real",
  arguments: [
    { name: "u", type: "rn", description: "A vector" },
    { name: "v", type: "rn", description: "A vector" },
  ],
  definition: (_context: Context, u: ad.Num[], v: ad.Num[]): FloatV<ad.Num> => {
    const det = sub(mul(u[0], v[1]), mul(u[1], v[0]));
    return {
      tag: "FloatV",
      contents: det,
    };
  },
};

const lineLineIntersection: Computation<VectorV<ad.Num>> = {
  name: "lineLineIntersection",
  documentation:
    "Return the intersection of a line passing through `a0` and `a1` with a line passing through `b0` and `b1`",
  returns: "r2",
  arguments: [
    { name: "a0", type: "r2", description: "First point of first line" },
    { name: "a1", type: "r2", description: "Second point of first line" },
    { name: "b0", type: "r2", description: "First point of second line" },
    { name: "b1", type: "r2", description: "Second point of second line" },
  ],
  definition: (
    _context: Context,
    a0: ad.Num[],
    a1: ad.Num[],
    b0: ad.Num[],
    b1: ad.Num[]
  ): VectorV<ad.Num> => {
    const A0 = [a0[0], a0[1], 1];
    const A1 = [a1[0], a1[1], 1];
    const B0 = [b0[0], b0[1], 1];
    const B1 = [b1[0], b1[1], 1];
    const X = ops.cross3(ops.cross3(A0, A1), ops.cross3(B0, B1));
    const x = [div(X[0], X[2]), div(X[1], X[2])];
    return {
      tag: "VectorV",
      contents: toPt(x),
    };
  },
};

const midpoint: Computation<VectorV<ad.Num>> = {
  name: "midpoint",
  documentation:
    "Return a point located at the midpoint between pts `start` and `end`",
  returns: "rn",
  arguments: [
    { name: "start", type: "rn", description: "First point" },
    { name: "end", type: "rn", description: "Second point" },
  ],
  definition: (
    _context: Context,
    start: ad.Num[],
    end: ad.Num[]
  ): VectorV<ad.Num> => {
    const midpointLoc = ops.vmul(0.5, ops.vadd(start, end));
    return {
      tag: "VectorV",
      contents: toPt(midpointLoc),
    };
  },
};

const midpointOffset: Computation<TupV<ad.Num>> = {
  name: "midpointOffset",
  documentation:
    "Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).",
  returns: "r2",
  arguments: [
    { name: "s1", type: "line", description: "A line" },
    {
      name: "padding",
      type: "real",
      description: "Padding between midpoint and label",
    },
  ],
  definition: (
    _context: Context,
    [t1, s1]: [string, any],
    padding: ad.Num
  ): TupV<ad.Num> => {
    if (t1 === "Arrow" || t1 === "Line") {
      const [start, end] = linePts(s1);
      // TODO: Cache these operations in Style!
      const normalDir = ops.rot90(ops.vnormalize(ops.vsub(end, start)));
      const midpointLoc = ops.vmul(0.5, ops.vadd(start, end));
      const midpointOffsetLoc = ops.vmove(midpointLoc, padding, normalDir);
      return {
        tag: "TupV",
        contents: toPt(midpointOffsetLoc),
      };
    } else {
      throw Error(`unsupported shape ${t1} in midpointOffset`);
    }
  },
};

const chevron: Computation<PtListV<ad.Num>> = {
  name: "chevron",
  documentation:
    "Return a list of points for a chevron shape comprised of two line segments intersecting at a right angle at the midpoint of `s1`, which can then be passed to `pathFromPoints` to draw the chevron.",
  returns: "r2n",
  arguments: [
    { name: "s1", type: "line", description: "A line" },
    {
      name: "padding",
      type: "real",
      description: "Length of each line segment",
    },
  ],
  definition: (
    _context: Context,
    // TODO reimplement with variable tick marks when #629 is merged
    [t1, s1]: [string, any],
    padding: ad.Num
  ): PtListV<ad.Num> => {
    if (t1 === "Arrow" || t1 === "Line") {
      // tickPlacement(padding, ticks);
      const [start, end] = linePts(s1);
      const dir = ops.vnormalize(ops.vsub(end, start)); // TODO make direction face "positive direction"
      const startDir = ops.vrot(dir, 135);
      const endDir = ops.vrot(dir, 225);
      const center = ops.vmul(0.5, ops.vadd(start, end));
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
};

const innerPointOffset: Computation<VectorV<ad.Num>> = {
  name: "innerPointOffset",
  documentation:
    "Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).",
  returns: "r2",
  arguments: [
    { name: "pt1", type: "r2", description: "First point" },
    { name: "pt2", type: "r2", description: "Second point" },
    { name: "pt3", type: "r2", description: "Third point" },
    {
      name: "padding",
      type: "real",
      description: "Offset from line to returned point",
    },
  ],
  definition: (
    _context: Context,
    pt1: ad.Num[],
    pt2: ad.Num[],
    pt3: ad.Num[],
    padding: ad.Num
  ): VectorV<ad.Num> => {
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
    return {
      tag: "VectorV",
      contents: [ifCond(cond, xp, xn), ifCond(cond, yp, yn)],
    };
  },
};

const ticksOnLine: Computation<PathDataV<ad.Num>> = {
  name: "ticksOnLine",
  documentation:
    "Create equally spaced tick marks centered at the midpoint of a line",
  returns: "path",
  arguments: [
    { name: "pt1", type: "r2", description: "starting point of a line" },
    { name: "pt2", type: "r2", description: "ending point of a line" },
    {
      name: "spacing",
      type: "real",
      description: "space in px between each tick",
    },
    {
      name: "numTicks",
      type: "posint",
      description: "number of tick marks to create",
    },
    {
      name: "tickLength",
      type: "real",
      description: "1/2 length of each tick",
    },
  ],
  definition: (
    _context: Context,
    pt1: ad.Num[],
    pt2: ad.Num[],
    spacing: ad.Num,
    numTicks: ad.Num,
    tickLength: ad.Num
  ): PathDataV<ad.Num> => {
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
    return path.getPath();
  },
};

const orientedSquare: Computation<PathDataV<ad.Num>> = {
  name: "orientedSquare",
  documentation:
    "Given two orthogonal segments that intersect at `intersection`, and a size `len` return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.",
  returns: "path",
  arguments: [
    { name: "s1", type: "line", description: "First line segment" },
    { name: "s2", type: "line", description: "Second line segment" },
    { name: "intersection", type: "r2", description: "Point of intersection" },
    { name: "len", type: "real", description: "Side length of square marker" },
  ],
  definition: (
    _context: Context,
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    intersection: ad.Pt2,
    len: ad.Num
  ): PathDataV<ad.Num> => {
    if (
      (t1 === "Arrow" || t1 === "Line") &&
      (t2 === "Arrow" || t2 === "Line")
    ) {
      const [seg1, seg2] = [linePts(s1), linePts(s2)];
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
      throw Error(`orientedSquare undefined for types ${t1}, ${t2}`);
    }
  },
};

const triangle: Computation<PathDataV<ad.Num>> = {
  name: "triangle",
  documentation:
    "Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).",
  returns: "path",
  arguments: [
    { name: "l1", type: "line", description: "First line" },
    { name: "l2", type: "line", description: "Second line" },
    { name: "l3", type: "line", description: "Third line" },
  ],
  definition: (
    _context: Context,
    [t1, l1]: any,
    [t2, l2]: any,
    [t3, l3]: any
  ): PathDataV<ad.Num> => {
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
};

const average2: Computation<FloatV<ad.Num>> = {
  name: "average2",
  documentation: "Return the average of floats `x` and `y`.",
  returns: "real",
  arguments: [
    { name: "x", type: "real", description: "`x`" },
    { name: "y", type: "real", description: "`y`" },
  ],
  definition: (_context: Context, x: ad.Num, y: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: div(add(x, y), 2),
    };
  },
};

const average: Computation<FloatV<ad.Num>> = {
  name: "average",
  documentation: "Return the average of the floats in the list `xs`.",
  returns: "real",
  arguments: [{ name: "xs", type: "rn", description: "`xs`" }],
  definition: (_context: Context, xs: ad.Num[]): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: div(addN(xs), max(1, xs.length)),
      // To avoid divide-by-0
    };
  },
};

const unit: Computation<VectorV<ad.Num>> = {
  name: "unit",
  documentation: "Return the normalized version of vector `v`.",
  returns: "rn",
  arguments: [{ name: "v", type: "rn", description: "`v`" }],
  definition: (_context: Context, v: ad.Num[]): VectorV<ad.Num> => {
    return {
      tag: "VectorV",
      contents: ops.vnormalize(v),
    };
  },
};

const sampleColor: Computation<ColorV<ad.Num>> = {
  name: "sampleColor",
  documentation:
    'Sample a random color once, with opacity `alpha` and colorType `colorType` (`"rgb"` or `"hsv"`).',
  returns: "color",
  arguments: [
    { name: "alpha", type: "unit", description: "Opacity" },
    { name: "colorType", type: "colortype", description: "Color model" },
  ],
  definition: (
    { makeInput }: Context,
    alpha: ad.Num,
    colorType: string
  ): ColorV<ad.Num> => {
    if (colorType === "rgb") {
      const rgb = _.range(3).map(() =>
        makeInput({
          init: { tag: "Sampled", sampler: uniform(0.1, 0.9) },
          stages: new Set(),
        })
      );

      return {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [rgb[0], rgb[1], rgb[2], alpha],
        },
      };
    } else if (colorType === "hsv") {
      const h = makeInput({
        init: { tag: "Sampled", sampler: uniform(0, 360) },
        stages: new Set(),
      });
      return {
        tag: "ColorV",
        contents: {
          tag: "HSVA",
          contents: [h, 100, 80, alpha], // HACK: for the color to look good
        },
      };
    } else throw new Error("unknown color type");
  },
};

const setOpacity: Computation<ColorV<ad.Num>> = {
  name: "setOpacity",
  documentation: "Set the opacity of a color `color` to `frac`.",
  returns: "color",
  arguments: [
    { name: "color", type: "color", description: "Color" },
    { name: "frac", type: "unit", description: "Opacity" },
  ],
  definition: (
    _context: Context,
    color: Color<ad.Num>,
    frac: ad.Num
  ): ColorV<ad.Num> => {
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
};

const mulComputation: Computation<VectorV<ad.Num>> = {
  name: "mul",
  documentation:
    "Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).",
  returns: "rn",
  arguments: [
    { name: "m", type: "rnm", description: "A matrix" },
    { name: "v", type: "rn", description: "A vector" },
  ],
  definition: (
    _context: Context,
    m: ad.Num[][],
    v: ad.Num[]
  ): VectorV<ad.Num> => {
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
};

const barycenter: Computation<VectorV<ad.Num>> = {
  name: "barycenter",
  documentation:
    "Return the barycenter of the triangle with vertices `a`, `b`, `c`.",
  returns: "r2",
  arguments: [
    { name: "a", type: "r2", description: "First vertex" },
    { name: "b", type: "r2", description: "Second vertex" },
    { name: "c", type: "r2", description: "Third vertex" },
  ],
  definition: (
    _context: Context,
    a: ad.Num[],
    b: ad.Num[],
    c: ad.Num[]
  ): VectorV<ad.Num> => {
    const x = ops.vmul(1 / 3, ops.vadd(a, ops.vadd(b, c)));
    return {
      tag: "VectorV",
      contents: toPt(x),
    };
  },
};

const circumcenter: Computation<VectorV<ad.Num>> = {
  name: "circumcenter",
  documentation:
    "Return the circumcenter of the triangle with vertices `p`, `q`, `r`.",
  returns: "r2",
  arguments: [
    { name: "p", type: "r2", description: "First vertex" },
    { name: "q", type: "r2", description: "Second vertex" },
    { name: "r", type: "r2", description: "Third vertex" },
  ],
  definition: (
    _context: Context,
    p: ad.Num[],
    q: ad.Num[],
    r: ad.Num[]
  ): VectorV<ad.Num> => {
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
      ops.vmul(br, r)
    );

    return {
      tag: "VectorV",
      contents: toPt(x),
    };
  },
};

const circumradius: Computation<FloatV<ad.Num>> = {
  name: "circumradius",
  documentation:
    "Return the circumradius of the triangle with vertices `p`, `q`, `r`.",
  returns: "real",
  arguments: [
    { name: "p", type: "r2", description: "First vertex" },
    { name: "q", type: "r2", description: "Second vertex" },
    { name: "r", type: "r2", description: "Third vertex" },
  ],
  definition: (
    _context: Context,
    p: ad.Num[],
    q: ad.Num[],
    r: ad.Num[]
  ): FloatV<ad.Num> => {
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
            sub(add(b, c), s)
          )
        )
      )
    );

    return {
      tag: "FloatV",
      contents: R,
    };
  },
};

const incenter: Computation<VectorV<ad.Num>> = {
  name: "incenter",
  documentation:
    "Return the incenter of the triangle with vertices `p`, `q`, `r`.",
  returns: "r2",
  arguments: [
    { name: "p", type: "r2", description: "First vertex" },
    { name: "q", type: "r2", description: "Second vertex" },
    { name: "r", type: "r2", description: "Third vertex" },
  ],
  definition: (
    _context: Context,
    p: ad.Num[],
    q: ad.Num[],
    r: ad.Num[]
  ): VectorV<ad.Num> => {
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
      ops.vmul(br, r)
    );

    return {
      tag: "VectorV",
      contents: toPt(x),
    };
  },
};

const inradius: Computation<FloatV<ad.Num>> = {
  name: "inradius",
  documentation:
    "Return the inradius of the triangle with vertices `p`, `q`, `r`.",
  returns: "r2",
  arguments: [
    { name: "p", type: "r2", description: "First vertex" },
    { name: "q", type: "r2", description: "Second vertex" },
    { name: "r", type: "r2", description: "Third vertex" },
  ],
  definition: (
    _context: Context,
    p: ad.Num[],
    q: ad.Num[],
    r: ad.Num[]
  ): FloatV<ad.Num> => {
    // side lengths
    const a = ops.vnorm(ops.vsub(r, q));
    const b = ops.vnorm(ops.vsub(p, r));
    const c = ops.vnorm(ops.vsub(q, p));

    // semiperimeter
    const s = mul(0.5, add(add(a, b), c));

    // inradius
    const R = sqrt(div(mul(mul(sub(s, a), sub(s, b)), sub(s, c)), s));

    return {
      tag: "FloatV",
      contents: R,
    };
  },
};

const sqr: Computation<FloatV<ad.Num>> = {
  name: "sqr",
  documentation: "Return the square of the number `x`.",
  returns: "real",
  arguments: [{ name: "x", type: "real", description: "`x`" }],
  definition: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: squared(x) };
  },
};

const sqrtComputation: Computation<FloatV<ad.Num>> = {};

const maxComputation: Computation<FloatV<ad.Num>> = {};

const minComputation: Computation<FloatV<ad.Num>> = {};

const absComputation: Computation<FloatV<ad.Num>> = {};

const toRadians: Computation<FloatV<ad.Num>> = {};

const toDegrees: Computation<FloatV<ad.Num>> = {};

const norm: Computation<FloatV<ad.Num>> = {};

const normsq: Computation<FloatV<ad.Num>> = {};

const vdist: Computation<FloatV<ad.Num>> = {};

const vmul: Computation<VectorV<ad.Num>> = {};

const vdistsq: Computation<FloatV<ad.Num>> = {};

const angleOf: Computation<FloatV<ad.Num>> = {};

const MathE: Computation<FloatV<ad.Num>> = {};

const MathPI: Computation<FloatV<ad.Num>> = {};

const rot90: Computation<VectorV<ad.Num>> = {};

const rotateBy: Computation<VectorV<ad.Num>> = {};

const signedDistance: Computation<FloatV<ad.Num>> = {};

const unitVector: Computation<VectorV<ad.Num>> = {};

/**
 * Static dictionary of computation functions
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */
export const compDict = {
  // TODO: Refactor derivative + derivativePre to be inlined as one case in evaluator

  makePath: makePath.definition,

  /**
   * Return `i`th element of list `xs, assuming lists only hold floats.
   */
  get: get.definition,

  /**
   * Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).
   */
  rgba: rgba.definition,

  selectColor: selectColor.definition,

  /**
   * Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).
   */
  hsva: hsva.definition,

  /**
   * Return a paint of none (no paint)
   */
  none: none.definition,

  /**
   * Return `acosh(x)`.
   */
  acosh: acoshComputation.definition,

  /**
   * Return `acos(x)`.
   */
  acos: acosComputation.definition,

  /**
   * Return `asin(x)`.
   */
  asin: asinComputation.definition,

  /**
   * Return `asinh(x)`.
   */
  asinh: asinhComputation.definition,

  /**
   * Return `atan(x)`.
   */
  atan: atanComputation.definition,

  /**
   * Return `atan2(y,x)`.
   */
  atan2: atan2Computation.definition,

  /**
   * Return `atanh(x)`.
   */
  atanh: atanhComputation.definition,

  /**
   * Return `cbrt(x)`.
   */
  cbrt: cbrtComputation.definition,

  /**
   * Return `ceil(x)`.
   */
  ceil: ceilComputation.definition,

  /**
   * Return `cos(x)`.
   */
  cos: cosComputation.definition,

  /**
   * Return `cosh(x)`.
   */
  cosh: coshComputation.definition,

  /**
   * Return `exp(x)`.
   */
  exp: expComputation.definition,

  /**
   * Return `expm1(x)`.
   */
  expm1: expm1Computation.definition,

  /**
   * Return `floor(x)`.
   */
  floor: floorComputation.definition,

  /**
   * Return `log(x)`.
   */
  log: logComputation.definition,

  /**
   * Return `log2(x)`.
   */
  log2: log2Computation.definition,

  /**
   * Return `log10(x)`.
   */
  log10: log10Computation.definition,

  /**
   * Return `log1p(x)`.
   */
  log1p: log1pComputation.definition,

  /**
   * Return `pow(x,y)`.
   */
  pow: powComputation.definition,

  /**
   * Return `round(x)`.
   */
  round: roundComputation.definition,

  /**
   * Return `sign(x)`.
   */
  sign: signComputation.definition,

  /**
   * Return `sin(x)`.
   */
  sin: sinComputation.definition,

  /**
   * Return `sinh(x)`.
   */
  sinh: sinhComputation.definition,

  /**
   * Return `tan(x)`.
   */
  tan: tanComputation.definition,

  /**
   * Return `tanh(x)`.
   */
  tanh: tanhComputation.definition,

  /**
   * Return `trunc(x)`.
   */
  trunc: truncComputation.definition,

  /**
   * Return the dot product of `v` and `w`.
   */
  dot: dotComputation.definition,

  /**
   * Return the length of the line or arrow shape `[type, props]`.
   */
  length: lengthComputation.definition,

  /**
   * Concatenate a list of strings
   */
  concat: concatComputation.definition,

  /**
   * Return the normalized version of vector `v`.
   */
  normalize: normalizeComputation.definition,

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  pathFromPoints: pathFromPoints.definition,

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  quadraticCurveFromPoints: quadraticCurveFromPoints.definition,

  /**
   * Draw a curve interpolating three given points.
   * (Note that this is different from specifying the
   * three control points of a quadratic Bézier curve,
   * since a Bézier does not interpolate the middle
   * control point.)
   */
  interpolateQuadraticFromPoints: interpolateQuadraticFromPoints.definition,

  /**
   * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
   */
  cubicCurveFromPoints: cubicCurveFromPoints.definition,

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
  arc: arc.definition,

  repeatedArcs: repeatedArcs.definition,

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
  wedge: wedge.definition,
  /**
   * Find the point that is located at dist r along a line between p1 and p2.
   * @param p1: start point of line segment
   * @param p2: endpoint of line segment
   * @param r: distance from p1 to travel along the line
   * @returns: vector representation of the point of intersection
   */
  ptOnLine: ptOnLine.definition,
  /**
   * Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.
   * @param x1, y1: x, y coordinates of the circle/ellipse that the arc is drawn on
   * @param start: start point of the arc
   * @param end: end point of the arc
   * @returns: 0 or 1 depending on CCW or CW rotation
   */
  arcSweepFlag: arcSweepFlag.definition,
  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: angleBetween.definition,
  /**
   * Return the signed angle from vector `u` to vector `v`, in radians.
   * Assumes that both u and v are 2D vectors and have nonzero magnitude.
   * The returned value will be in the range [-pi,pi].
   */
  angleFrom: angleFrom.definition,
  /**
   * Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]
   */
  cross2D: cross2D.definition,
  /**
   * Return the intersection of a line passing through
   * `a0` and `a1` with a line passing through `b0` and `b1`
   */
  lineLineIntersection: lineLineIntersection.definition,
  /**
   * Return a point located at the midpoint between pts `start` and `end`
   */
  midpoint: midpoint.definition,
  /**
   * Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).
   */
  midpointOffset: midpointOffset.definition,
  chevron: chevron.definition,
  /**
   * Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).
   */
  innerPointOffset: innerPointOffset.definition,
  /**
   * Create equally spaced tick marks centered at the midpoint of a line
   * @param pt1: starting point of a line
   * @param pt2: endping point of a line
   * @param spacing: space in px between each tick
   * @param numTicks: number of tick marks to create
   * @param tickLength: 1/2 length of each tick
   */
  ticksOnLine: ticksOnLine.definition,
  /**
   * Given two orthogonal segments that intersect at `intersection`, and a size `len`
   * return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.
   */
  orientedSquare: orientedSquare.definition,

  /**
           * Figure out which side of the rectangle `[t1, s1]` the `start->end` line is hitting, assuming that `start` is located at the rect's center and `end` is located outside the rectangle, and return the size of the OTHER side. Also assuming axis-aligned rectangle. This is used for arrow placement in box-and-arrow diagrams.

       @deprecated Don't use this function, it does not fully work
           */
  intersectingSideSize: (
    _context: Context,
    start: ad.Num[],
    end: ad.Num[],
    [t1, s1]: [string, any]
  ): FloatV<ad.Num> => {
    // if (s1.rotation.contents) { throw Error("assumed AABB"); }
    if (!shapedefs[t1].isRectlike) {
      throw Error("expected rect-like shape");
    }

    // TODO: Deal with start and end disjoint from rect, or start and end subset of rect
    const rect = bboxFromShape([t1, s1]);

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
      rect.height,
      rect.width
    );
    return { tag: "FloatV", contents: dim };
  },

  /**
   * Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).
   */
  triangle: triangle.definition,

  /**
   * Return the average of floats `x` and `y`.
   */
  average2: average2.definition,

  /**
   * Return the average of the floats in the list `xs`.
   */
  average: average.definition,

  /**
   * Return the normalized version of vector `v`.
   */
  unit: unit.definition,

  /**
   * Sample a random color once, with opacity `alpha` and colorType `colorType` (`"rgb"` or `"hsv"`).
   */
  sampleColor: sampleColor.definition,

  /**
   * Set the opacity of a color `color` to `frac`.
   */
  setOpacity: setOpacity.definition,

  /**
   * Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).
   */
  mul: mulComputation.definition,

  // ------ Triangle centers

  /**
   * Return the barycenter of the triangle with vertices `a`, `b`, `c`.
   */
  barycenter: barycenter.definition,

  /**
   * Return the circumcenter of the triangle with vertices `p`, `q`, `r`.
   */
  circumcenter: circumcenter.definition,

  /**
   * Return the circumradius of the triangle with vertices `p`, `q`, `r`.
   */
  circumradius: circumradius.definition,

  /**
   * Return the incenter of the triangle with vertices `p`, `q`, `r`.
   */
  incenter: incenter.definition,

  /**
   * Return the inradius of the triangle with vertices `p`, `q`, `r`.
   */
  inradius: inradius.definition,

  // ------ Utility functions

  /**
   * Return the square of the number `x`.
   */
  sqr: sqr.definition,

  /**
   * Return the square root of the number `x`. (NOTE: if `x < 0`, you may get `NaN`s)
   */
  sqrt: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: sqrt(x) };
  },

  /**
   * Return the max of the numbers `x`, `y`.
   */
  max: (_context: Context, x: ad.Num, y: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: max(x, y) };
  },

  /**
   * Return the min of the numbers `x`, `y`.
   */
  min: (_context: Context, x: ad.Num, y: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: min(x, y) };
  },

  /**
   * Return the absolute value of the number `x`.
   */
  abs: (_context: Context, x: ad.Num): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: absVal(x) };
  },

  /**
   * Convert the angle `theta` from degrees to radians.
   */
  toRadians: (_context: Context, theta: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: mul(Math.PI / 180, theta),
    };
  },

  /**
   * Convert the angle `theta` from radians to degrees.
   */
  toDegrees: (_context: Context, theta: ad.Num): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: mul(180 / Math.PI, theta),
    };
  },

  /**
   * Return the Euclidean norm of the vector `v`.
   */
  norm: (_context: Context, v: ad.Num[]): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ops.vnorm(v) };
  },

  /**
   * Return the Euclidean norm squared of the vector `v`.
   */
  normsq: (_context: Context, v: ad.Num[]): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ops.vnormsq(v) };
  },

  /**
   * Return the Euclidean distance between the vectors `v` and `w`.
   */
  vdist: (_context: Context, v: ad.Num[], w: ad.Num[]): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ops.vdist(v, w) };
  },

  vmul: (_context: Context, s: ad.Num, v: ad.Num[]): VectorV<ad.Num> => {
    return { tag: "VectorV", contents: ops.vmul(s, v) };
  },

  /**
   * Return the Euclidean distance squared between the vectors `v` and `w`.
   */
  vdistsq: (_context: Context, v: ad.Num[], w: ad.Num[]): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: ops.vdistsq(v, w) };
  },

  /**
   * Return the angle made by the vector `v` with the positive x-axis.
   */
  angleOf: (_context: Context, v: ad.Num[]): FloatV<ad.Num> => {
    return { tag: "FloatV", contents: atan2(v[1], v[0]) };
  },

  // ------ Mathematical constants

  /**
   * Base e of the natural logarithm.
   */
  MathE: (_context: Context): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: Math.E,
    };
  },

  /**
   * Ratio of the circumference of a circle to its diameter.
   */
  MathPI: (_context: Context): FloatV<ad.Num> => {
    return {
      tag: "FloatV",
      contents: Math.PI,
    };
  },

  // ------ Geometry/graphics utils

  /**
   * Rotate a 2D vector `v` by 90 degrees counterclockwise.
   */
  rot90: (_context: Context, v: ad.Num[]): VectorV<ad.Num> => {
    if (v.length !== 2) {
      throw Error("expected 2D vector in `rot90`");
    }
    const [x, y] = v;
    return { tag: "VectorV", contents: [neg(y), x] };
  },

  /**
   * Rotate a 2D vector `v` by theta degrees counterclockwise.
   */
  rotateBy: (
    _context: Context,
    v: ad.Num[],
    theta: ad.Num
  ): VectorV<ad.Num> => {
    if (v.length !== 2) {
      throw Error("expected 2D vector in `rotateBy`");
    }
    const [x, y] = v;
    const X = add(mul(cos(theta), x), mul(sin(theta), y));
    const Y = add(neg(mul(sin(theta), x)), mul(cos(theta), y));
    return { tag: "VectorV", contents: [X, Y] };
  },

  signedDistance: (
    _context: Context,
    [t, s]: [string, any],
    p: ad.Num[]
  ): FloatV<ad.Num> => {
    /*  
    All math borrowed from:
    https://iquilezles.org/articles/distfunctions2d/
    
    axis-aligned rectangle:
    float sdBox( in vec2 p, in vec2 b )
    {
      vec2 d = abs(p)-b;
      return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
    } 
    */
    if (
      t === "Rectangle" ||
      t === "Text" ||
      t === "Equation" ||
      t === "Image"
    ) {
      const absp = ops.vabs(ops.vsub(p, s.center.contents));
      const b = [div(s.width.contents, 2), div(s.height.contents, 2)];
      const d = ops.vsub(absp, b);
      const result = add(
        ops.vnorm(ops.vmax(d, [0.0, 0.0])),
        min(max(d[0], d[1]), 0.0)
      );
      return {
        tag: "FloatV",
        contents: result,
      };
    } else if (t === "Circle") {
      /*     
      float sdCircle( vec2 p, float r )
      {
        return length(p) - r;
      } 
      */
      const pOffset = ops.vsub(p, s.center.contents);
      const result = sub(ops.vnorm(pOffset), s.r.contents);
      return {
        tag: "FloatV",
        contents: result,
      };
    } else if (t === "Polygon") {
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
      const v = s.points.contents;
      let d = ops.vdot(ops.vsub(p, v[0]), ops.vsub(p, v[0]));
      let ess: ad.Num = 1.0;
      let j = v.length - 1;
      for (let i = 0; i < v.length; i++) {
        const e = ops.vsub(v[j], v[i]);
        const w = ops.vsub(p, v[i]);
        const clampedVal = clamp([0, 1], div(ops.vdot(w, e), ops.vdot(e, e)));
        const b = ops.vsub(w, ops.vmul(clampedVal, e));
        d = min(d, ops.vdot(b, b));
        const c1 = gte(p[1], v[i][1]);
        const c2 = lt(p[1], v[j][1]);
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
      const result = mul(ess, sqrt(d));
      return {
        tag: "FloatV",
        contents: result,
      };
    } else if (t === "Line") {
      return {
        tag: "FloatV",
        contents: sdLine(s, p),
      };
    } else if (t === "Polyline") {
      return {
        tag: "FloatV",
        contents: sdPolyline(s, p),
      };
    } else if (t === "Ellipse") {
      throw Error(`unsupported shape ${t} in distanceShapeToPoint`);
      // return {
      //   tag: "FloatV",
      //   contents: sdEllipse(s, p),
      // };
    } else if (t === "Path") {
      throw Error(`unsupported shape ${t} in distanceShapeToPoint`);
    } else {
      throw Error(`unsupported shape ${t} in distanceShapeToPoint`);
    }
  },

  /**
   * Construct a unit vector u in the direction of the
   * given angle theta (in radians).
   */
  unitVector: (_context: Context, theta: ad.Num): VectorV<ad.Num> => {
    return { tag: "VectorV", contents: [cos(theta), sin(theta)] };
  },
};

/*
  Computes the signed distance for a line 
  float sdSegment( in vec2 p, in vec2 a, in vec2 b )
  {
    vec2 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h );
  }
*/
const sdLine = (s: Line, p: ad.Num[]): ad.Num => {
  return sdLineAsNums(s.start.contents, s.end.contents, p);
};

const sdLineAsNums = (a: ad.Num[], b: ad.Num[], p: ad.Num[]): ad.Num => {
  const pa = ops.vsub(p, a);
  const ba = ops.vsub(b, a);
  const h = clamp([0, 1], div(ops.vdot(pa, ba), ops.vdot(ba, ba)));
  return ops.vnorm(ops.vsub(pa, ops.vmul(h, ba)));
};

const sdPolyline = (s: Polyline, p: ad.Num[]): ad.Num => {
  const dists: ad.Num[] = [];
  for (let i = 0; i < s.points.contents.length - 1; i++) {
    const start = s.points.contents[i];
    const end = s.points.contents[i + 1];
    dists[i] = sdLineAsNums(start, end, p);
  }
  return minN(dists);
};

export const sdEllipse = (s: Ellipse, p: ad.Num[]): ad.Num => {
  return sdEllipseAsNums(s.rx.contents, s.ry.contents, s.center.contents, p);
};

/*
  float msign(in float x) { return (x<0.0)?-1.0:1.0; }
*/
export const msign = (x: ad.Num): ad.Num => {
  return ifCond(lt(x, 0), -1, 1);
};

/*
  Ported code is here: https://www.shadertoy.com/view/4sS3zz
*/
export const sdEllipseAsNums = (
  radiusx: ad.Num,
  radiusy: ad.Num,
  center: ad.Num[],
  pInput: ad.Num[]
): ad.Num => {
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
    pUnswizzled[0]
  );
  p[1] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    pUnswizzled[0],
    pUnswizzled[1]
  );
  ab[0] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    abUnswizzled[1],
    abUnswizzled[0]
  );
  ab[1] = ifCond(
    gt(pUnswizzled[0], pUnswizzled[1]),
    abUnswizzled[0],
    abUnswizzled[1]
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
    div(absVal(g), mul(rxif, ryif))
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

// `_compDictVals` causes TypeScript to enforce that every function in
// `compDict` takes a `Context` as its first parameter and returns a `Value`
const _compDictVals: ((
  context: Context,
  ...rest: never[]
) => Value<ad.Num>)[] = Object.values(compDict);

// Ignore this
export const checkComp = (fn: string, args: ArgVal<ad.Num>[]): void => {
  if (!compDict[fn]) throw new Error(`Computation function "${fn}" not found`);
};

const toPt = (v: ad.Num[]): ad.Pt2 => {
  if (v.length !== 2) {
    throw Error("expected vector of length 2");
  }
  return [v[0], v[1]];
};

/**
 * Given two perpendicular vectors `[startR, endR]` and `[startL, endL]`, return a path that describes a perpendicular mark between them.
 */
const perpPathFlat = (
  len: ad.Num,
  [startR, endR]: [ad.Num[], ad.Num[]],
  [startL, endL]: [ad.Num[], ad.Num[]]
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
  multiplier: ad.Num = 1
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
