import * as ad from "types/ad";
import { VarAD } from "types/ad";

// HACK: The previous implementation of the symbolic differentiation engine gave
// each VarAD a `parentsAD` array, which determined the order in which the
// partial derivatives for that node would be added together. This matters
// because floating-point addition is not associative. On creation, each VarAD
// would append itself to the `parentsAD` of each of its children, so each
// `parentsAD` was sorted by node creation time. Therefore, in order to give the
// exact same results as the previous engine, this new engine uses a global
// counter to give (almost) every VarAD a unique index `i`, to use for sorting
// edges while constructing gradient nodes. Obviously this is bad because it
// means that the result of the optimizer depends not just on the structure of
// the computation graph but also on the exact order in which its parts are
// constructed. This should be immediately removed after the new engine is
// merged into `main`, but for bookkeeping, it is nice for the initial rewrite
// to produce diagrams identical to those produced by the previous engine.
let count = 0;

const binary = (binop: ad.Binary["binop"]) => (
  v: VarAD,
  w: VarAD
): ad.Binary => ({ tag: "Binary", binop, i: count++, left: v, right: w });

const nary = (op: ad.Nary["op"], bin: (v: VarAD, w: VarAD) => ad.Binary) => (
  xs: VarAD[]
): VarAD => {
  // interestingly, special-casing 1 and 2 args like this actually affects the
  // gradient by a nontrivial amount in some cases
  switch (xs.length) {
    case 1: {
      return xs[0];
    }
    case 2: {
      return bin(xs[0], xs[1]);
    }
    default: {
      return { tag: "Nary", i: count++, op, params: xs };
    }
  }
};

/**
 * Return `v + w`.
 */
export const add = binary("+");

/**
 * Return the sum of elements in `xs`.
 */
export const addN = nary("addN", add);

/**
 * Return `v * w`.
 */
export const mul = binary("*");

/**
 * Return `v - w`.
 */
export const sub = binary("-");

/**
 * Return `v / w`.
 */
export const div = binary("/");

/**
 * Return `max(v, w)`.
 */
export const max = binary("max");

/**
 * Return `min(v, w)`.
 */
export const min = binary("min");

/**
 * Return `maxN(xs)`.
 */
export const maxN = nary("maxN", max);

/**
 * Return `minN(xs)`.
 */
export const minN = nary("minN", min);

/**
 * Returns the two-argument arctangent `atan2(y, x)`, which
 * describes the angle made by a vector (x,y) with the x-axis.
 * Returns a value in radians, in the range [-pi,pi].
 */
export const atan2 = (y: VarAD, x: VarAD): ad.Binary => ({
  tag: "Binary",
  i: count++,
  binop: "atan2",
  left: y,
  right: x,
});

/**
 * Returns `pow(v,w)`.
 */
export const pow = binary("pow");

// --- Unary ops

const unary = (unop: ad.Unary["unop"]) => (v: VarAD): ad.Unary => ({
  tag: "Unary",
  i: count++,
  unop,
  param: v,
});

/**
 * Return `-v`.
 */
export const neg = unary("neg");

/**
 * Return `v * v`.
 */
export const squared = unary("squared");

/**
 * Return `sqrt(v)`.
 */
export const sqrt = unary("sqrt");

/**
 * Return `1 / v`.
 */
export const inverse = unary("inverse");

/**
 * Return `|v|`.
 */
export const absVal = unary("abs");

/**
 * Return `acosh(v)`.
 */
export const acosh = unary("acosh");

/**
 * Return `acos(v)`.
 */
export const acos = unary("acos");

/**
 * Return `asin(v)`.
 */
export const asin = unary("asin");

/**
 * Return `asinh(v)`.
 */
export const asinh = unary("asinh");

/**
 * Return `atan(v)`.
 */
export const atan = unary("atan");

/**
 * Return `atanh(v)`.
 */
export const atanh = unary("atanh");

/**
 * Return `cbrt(v)`.
 */
export const cbrt = unary("cbrt");

/**
 * Return `ceil(v)`.
 */
export const ceil = unary("ceil");

/**
 * Return `cos(v)`.
 */
export const cos = unary("cos");

/**
 * Return `cosh(v)`.
 */
export const cosh = unary("cosh");

/**
 * Return `exp(v)`.
 */
export const exp = unary("exp");

/**
 * Return `expm1(v)`.
 */
export const expm1 = unary("expm1");

/**
 * Return `floor(v)`.
 */
export const floor = unary("floor");

/**
 * Return the natural logarithm `ln(v)` (i.e., log base e).
 */
export const ln = unary("log");

/**
 * Return `log2(v)`.
 */
export const log2 = unary("log2");

/**
 * Return `log10(v)`.
 */
export const log10 = unary("log10");

/**
 * Return `log1p(v)`.
 */
export const log1p = unary("log1p");

/**
 * Return `round(v)`.
 */
export const round = unary("round");

/**
 * Return `sign(v)`.
 */
export const sign = unary("sign");

/**
 * Return `sin(v)`.
 */
export const sin = unary("sin");

/**
 * Return `sinh(v)`.
 */
export const sinh = unary("sinh");

/**
 * Return `tan(v)`.
 */
export const tan = unary("tan");

/**
 * Return `tanh(v)`.
 */
export const tanh = unary("tanh");

/**
 * Return `trunc(v)`.
 */
export const trunc = unary("trunc");

// ------- Discontinuous / noGrad ops

/**
 * Return a conditional `v > w`.
 */
export const gt = binary(">");

/**
 * Return a conditional `v < w`.
 */
export const lt = binary("<");

/**
 * Return a conditional `v == w`. (TODO: Maybe check if they are equal up to a tolerance?)
 */
export const eq = binary("===");

/**
 * Return a boolean `v && w`
 */
export const and = binary("&&");

/**
 * Return a boolean `v || w`
 */
export const or = binary("||");

/**
 * Return a conditional `if(cond) then v else w`.
 */
export const ifCond = (cond: VarAD, v: VarAD, w: VarAD): ad.Ternary => ({
  tag: "Ternary",
  i: count++,
  cond,
  then: v,
  els: w,
});
