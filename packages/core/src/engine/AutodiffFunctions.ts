import * as ad from "types/ad";
import { VarAD } from "types/ad";

/**
 * Return `v + w`.
 */
export const add = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "+",
  left: v,
  right: w,
});

/**
 * Return the sum of elements in `xs`.
 */
export const addN = (xs: VarAD[]): ad.Nary => ({
  tag: "Nary",
  op: "addN",
  params: xs,
});

/**
 * Return `v * w`.
 */
export const mul = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "*",
  left: v,
  right: w,
});

/**
 * Return `v - w`.
 */
export const sub = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "-",
  left: v,
  right: w,
});

/**
 * Return `v / w`.
 */
export const div = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "/",
  left: v,
  right: w,
});

/**
 * Return `max(v, w)`.
 */
export const max = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "max",
  left: v,
  right: w,
});

/**
 * Return `min(v, w)`.
 */
export const min = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "min",
  left: v,
  right: w,
});

/**
 * Return `maxN(xs)`.
 */
export const maxN = (xs: VarAD[]): ad.Nary => ({
  tag: "Nary",
  op: "maxN",
  params: xs,
});

/**
 * Return `minN(xs)`.
 */
export const minN = (xs: VarAD[]): ad.Nary => ({
  tag: "Nary",
  op: "minN",
  params: xs,
});

/**
 * Returns the two-argument arctangent `atan2(y, x)`, which
 * describes the angle made by a vector (x,y) with the x-axis.
 * Returns a value in radians, in the range [-pi,pi].
 */
export const atan2 = (y: VarAD, x: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "atan2",
  left: y,
  right: x,
});

/**
 * Returns `pow(x,y)`.
 */
export const pow = (x: VarAD, y: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "pow",
  left: x,
  right: y,
});

// --- Unary ops

/**
 * Return `-v`.
 */
export const neg = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "neg",
  param: v,
});

/**
 * Return `v * v`.
 */
export const squared = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "squared",
  param: v,
});

/**
 * Return `sqrt(v)`.
 */
export const sqrt = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "sqrt",
  param: v,
});

/**
 * Return `1 / v`.
 */
export const inverse = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "inverse",
  param: v,
});

/**
 * Return `|v|`.
 */
export const absVal = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "abs",
  param: v,
});

/**
 * Return `acosh(x)`.
 */
export const acosh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "acosh",
  param: x,
});

/**
 * Return `acos(x)`.
 */
export const acos = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "acos",
  param: x,
});

/**
 * Return `asin(x)`.
 */
export const asin = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "asin",
  param: x,
});

/**
 * Return `asinh(x)`.
 */
export const asinh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "asinh",
  param: x,
});

/**
 * Return `atan(x)`.
 */
export const atan = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "atan",
  param: x,
});

/**
 * Return `atanh(x)`.
 */
export const atanh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "atanh",
  param: x,
});

/**
 * Return `cbrt(x)`.
 */
export const cbrt = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "cbrt",
  param: x,
});

/**
 * Return `ceil(x)`.
 */
export const ceil = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "ceil",
  param: x,
});

/**
 * Return `cos(x)`.
 */
export const cos = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "cos",
  param: x,
});

/**
 * Return `cosh(x)`.
 */
export const cosh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "cosh",
  param: x,
});

/**
 * Return `exp(x)`.
 */
export const exp = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "exp",
  param: x,
});

/**
 * Return `expm1(x)`.
 */
export const expm1 = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "expm1",
  param: x,
});

/**
 * Return `floor(x)`.
 */
export const floor = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "floor",
  param: x,
});

/**
 * Return the natural logarithm `ln(v)` (i.e., log base e).
 */
export const ln = (v: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "ln",
  param: v,
});

/**
 * Return `log2(x)`.
 */
export const log2 = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "log2",
  param: x,
});

/**
 * Return `log10(x)`.
 */
export const log10 = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "log10",
  param: x,
});

/**
 * Return `log1p(x)`.
 */
export const log1p = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "log1p",
  param: x,
});

/**
 * Return `round(x)`.
 */
export const round = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "round",
  param: x,
});

/**
 * Return `sign(x)`.
 */
export const sign = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "sign",
  param: x,
});

/**
 * Return `sin(x)`.
 */
export const sin = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "sin",
  param: x,
});

/**
 * Return `sinh(x)`.
 */
export const sinh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "sinh",
  param: x,
});

/**
 * Return `tan(x)`.
 */
export const tan = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "tan",
  param: x,
});

/**
 * Return `tanh(x)`.
 */
export const tanh = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "tanh",
  param: x,
});

/**
 * Return `trunc(x)`.
 */
export const trunc = (x: VarAD): ad.Unary => ({
  tag: "Unary",
  unop: "trunc",
  param: x,
});

// ------- Discontinuous / noGrad ops

/**
 * Return a conditional `v > w`.
 */
export const gt = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: ">",
  left: v,
  right: w,
});

/**
 * Return a conditional `v < w`.
 */
export const lt = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "<",
  left: v,
  right: w,
});

/**
 * Return a conditional `v == w`. (TODO: Maybe check if they are equal up to a tolerance?)
 * Note, the 1.0, 0.0 stuff is irrelevant, in the codegen they are boolean
 */
export const eq = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "===",
  left: v,
  right: w,
});

/**
 * Return a boolean (number) `v && w`
 */
export const and = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "&&",
  left: v,
  right: w,
});

/**
 * Return a boolean (number) `v || w`
 */
export const or = (v: VarAD, w: VarAD): ad.Binary => ({
  tag: "Binary",
  binop: "||",
  left: v,
  right: w,
});

/**
 * Return a conditional `if(cond) then v else w`.
 */
export const ifCond = (cond: VarAD, v: VarAD, w: VarAD): ad.Ternary => ({
  tag: "Ternary",
  cond,
  then: v,
  els: w,
});
