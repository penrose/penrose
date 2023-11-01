import * as tf from "@tensorflow/tfjs";
import * as ad from "../types/ad.js";

const binary =
  (f: (v: ad.Num, w: ad.Num) => ad.Num) =>
  (v: number | ad.Num, w: number | ad.Num): ad.Num => {
    if (typeof v === "number") v = tf.scalar(v);
    if (typeof w === "number") w = tf.scalar(w);
    return f(v, w);
  };

/**
 * Return `v + w`.
 */
export const add = binary((v, w) => v.add(w));

/**
 * Return the sum of elements in `xs`.
 */
export const addN = (xs: ad.Num[]): ad.Num => tf.addN(xs);

/**
 * Return `v * w`.
 */
export const mul = binary((v, w) => v.mul(w));

/**
 * Return `v - w`.
 */
export const sub = binary((v, w) => v.sub(w));

/**
 * Return `v / w`.
 */
export const div = binary((v, w) => v.div(w));

/**
 * Return `max(v, w)`.
 */
export const max = binary((v, w) => v.maximum(w));

/**
 * Return `min(v, w)`.
 */
export const min = binary((v, w) => v.minimum(w));

/**
 * Return `maxN(xs)`.
 */
export const maxN = (xs: ad.Num[]): ad.Num => tf.max(tf.stack(xs));

/**
 * Return `minN(xs)`.
 */
export const minN = (xs: ad.Num[]): ad.Num => tf.min(tf.stack(xs));

/**
 * Returns the two-argument arctangent `atan2(y, x)`, which
 * describes the angle made by a vector (x,y) with the x-axis.
 * Returns a value in radians, in the range [-pi,pi].
 */
export const atan2 = binary((v, w) => v.atan2(w));

/**
 * Returns `pow(v,w)`.
 */
export const pow = binary((v, w) => v.pow(w));

// --- Unary ops

const unary =
  (f: (v: ad.Num) => ad.Num) =>
  (v: number | ad.Num): ad.Num => {
    if (typeof v === "number") v = tf.scalar(v);
    return f(v);
  };

/**
 * Return `-v`.
 */
export const neg = unary((v) => v.neg());

/**
 * Return `v * v`.
 */
export const squared = unary((v) => v.square());

/**
 * Return `sqrt(v)`.
 */
export const sqrt = unary((v) => v.sqrt());

/**
 * Return `1 / v`.
 */
export const inverse = unary((v) => v.reciprocal());

/**
 * Return `|v|`.
 */
export const absVal = unary((v) => v.abs());

/**
 * Return `acosh(v)`.
 */
export const acosh = unary((v) => v.acosh());

/**
 * Return `acos(v)`.
 */
export const acos = unary((v) => v.acos());

/**
 * Return `asin(v)`.
 */
export const asin = unary((v) => v.asin());

/**
 * Return `asinh(v)`.
 */
export const asinh = unary((v) => v.asinh());

/**
 * Return `atan(v)`.
 */
export const atan = unary((v) => v.atan());

/**
 * Return `atanh(v)`.
 */
export const atanh = unary((v) => v.atanh());

/**
 * Return `cbrt(v)`.
 */
export const cbrt = unary((v) => v.pow(1 / 3));

/**
 * Return `ceil(v)`.
 */
export const ceil = unary((v) => v.ceil());

/**
 * Return `cos(v)`.
 */
export const cos = unary((v) => v.cos());

/**
 * Return `cosh(v)`.
 */
export const cosh = unary((v) => v.cosh());

/**
 * Return `exp(v)`.
 */
export const exp = unary((v) => v.exp());

/**
 * Return `expm1(v)`.
 */
export const expm1 = unary((v) => v.expm1());

/**
 * Return `floor(v)`.
 */
export const floor = unary((v) => v.floor());

/**
 * Return the natural logarithm `ln(v)` (i.e., log base e).
 */
export const ln = unary((v) => v.log());

/**
 * Return `log2(v)`.
 */
export const log2 = unary((v) => v.log().div(Math.log(2)));

/**
 * Return `log10(v)`.
 */
export const log10 = unary((v) => v.log().div(Math.log(10)));

/**
 * Return `log1p(v)`.
 */
export const log1p = unary((v) => v.log1p());

/**
 * Return `round(v)`.
 */
export const round = unary((v) => v.round());

/**
 * Return `sign(v)`.
 */
export const sign = unary((v) => v.sign());

/**
 * Return `sin(v)`.
 */
export const sin = unary((v) => v.sin());

/**
 * Return `sinh(v)`.
 */
export const sinh = unary((v) => v.sinh());

/**
 * Return `tan(v)`.
 */
export const tan = unary((v) => v.tan());

/**
 * Return `tanh(v)`.
 */
export const tanh = unary((v) => v.tanh());

// ------- Discontinuous / noGrad ops

const comp =
  (f: (v: ad.Num, w: ad.Num) => ad.Bool) =>
  (v: number | ad.Num, w: number | ad.Num): ad.Bool => {
    if (typeof v === "number") v = tf.scalar(v);
    if (typeof w === "number") w = tf.scalar(w);
    return f(v, w);
  };

/**
 * Return a conditional `v > w`.
 */
export const gt = comp((v, w) => v.greater(w));

/**
 * Return a conditional `v < w`.
 */
export const lt = comp((v, w) => v.less(w));

/**
 * Return a conditional `v >= w`.
 */
export const gte = comp((v, w) => v.greaterEqual(w));

/**
 * Return a conditional `v <= w`.
 */
export const lte = comp((v, w) => v.lessEqual(w));

/**
 * Return a conditional `v == w`. (TODO: Maybe check if they are equal up to a tolerance?)
 */
export const eq = comp((v, w) => v.equal(w));

const logic = (f: (v: ad.Bool, w: ad.Bool) => ad.Bool) => f;

/**
 * Return a boolean `v && w`
 */
export const and = logic((v, w) => v.logicalAnd(w));

/**
 * Return a boolean `v || w`
 */
export const or = logic((v, w) => v.logicalOr(w));

/**
 * Return a boolean `v !== w`
 */
export const xor = logic((v, w) => v.logicalXor(w));

export const not = (v: ad.Bool): ad.Bool => v.logicalNot();

/**
 * Return a conditional `if(cond) then v else w`.
 */
export const ifCond = (
  cond: ad.Bool,
  v: number | ad.Num,
  w: number | ad.Num,
): ad.Num => {
  if (typeof v === "number") v = tf.scalar(v);
  if (typeof w === "number") w = tf.scalar(w);
  return tf.where(cond, v, w);
};

// --- Vector ops

/**
 * Return the roots of the monic polynomial with degree `coeffs.length` where
 * the coefficient on the term with degree `i` is `coeffs[i]`. Any root with a
 * nonzero imaginary component is replaced with `NaN`.
 */
export const polyRoots = (coeffs: ad.Num[]): ad.Num[] => {
  throw Error("unsupported");
};
