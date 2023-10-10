import { Params, start, stepUntil } from "@penrose/optimizer";
import consola from "consola";
import _ from "lodash";
import * as rose from "rose";
import * as builtins from "./Builtins.js";

//#region Types for implicit autodiff graph

export type Expr = Bool | Num | Vec | Rec;

export type Bool = Comp | Logic | Not | Index | Member | Call;

export type Num =
  | number
  | Var
  | Unary
  | Binary
  | Ternary
  | Nary
  | Index
  | Member
  | Call;

export type Vec = PolyRoots | Index | Member | Call;

export type Rec = Index | Member | Call;

export interface Var {
  tag: "Var";
  val: number;
}

export interface Unary {
  tag: "Unary";
  unop:
    | "neg"
    | "squared"
    | "sqrt"
    | "inverse"
    | "abs"
    | "acosh"
    | "acos"
    | "asin"
    | "asinh"
    | "atan"
    | "atanh"
    | "cbrt"
    | "ceil"
    | "cos"
    | "cosh"
    | "exp"
    | "expm1"
    | "floor"
    | "log"
    | "log2"
    | "log10"
    | "log1p"
    | "round"
    | "sign"
    | "sin"
    | "sinh"
    | "tan"
    | "tanh"
    | "trunc";
  param: Num;
}

export interface Binary {
  tag: "Binary";
  binop: "+" | "*" | "-" | "/" | "max" | "min" | "atan2" | "pow";
  left: Num;
  right: Num;
}

export interface Comp {
  tag: "Comp";
  binop: ">" | "<" | "===" | ">=" | "<=";
  left: Num;
  right: Num;
}

export interface Logic {
  tag: "Logic";
  binop: "&&" | "||" | "!==";
  left: Bool;
  right: Bool;
}

export interface Not {
  tag: "Not";
  param: Bool;
}

export interface Ternary {
  tag: "Ternary";
  cond: Bool;
  then: Num;
  els: Num;
}

export interface Nary {
  tag: "Nary";
  op: "addN" | "maxN" | "minN";
  params: Num[];
}

export interface PolyRoots {
  tag: "PolyRoots";
  // coefficients of a monic polynomial with degree `coeffs.length`
  coeffs: Num[];
}

export interface Index {
  tag: "Index";
  index: number;
  vec: Vec;
}

export interface Member {
  tag: "Member";
  member: string;
  rec: Rec;
}

export interface Call {
  tag: "Call";
  func: rose.Fn;
  args: Expr[];
}

//#endregion

//#region Types for compiled autodiff graph

/**
 * A structure used to collect the various outputs of a `Gradient` function.
 * This is generic in the concrete number type, because it can also be useful in
 * situations where the elements are, for instance, computation graph nodes.
 */
export interface Outputs<T> {
  /** Derivatives of primary output with respect to inputs. */
  gradient: Map<Var, T>;
  /** Primary output. */
  primary: T;
  /** Secondary outputs. */
  secondary: T[];
}

export type Compiled = (
  inputs: (x: Var) => number,
  mask?: boolean[],
) => Outputs<number>;

export interface OptOutputs {
  phi: number; // see `Fn` from `@penrose/optimizer`
  objectives: number[];
  constraints: number[];
}

export interface Masks {
  inputMask: boolean[];
  objMask: boolean[];
  constrMask: boolean[];
}

// you can think of the `Fn` type from `@penrose/optimizer` as this type
// partially applied with `masks` and projecting out the `phi` field
export type Gradient = (
  masks: Masks,
  inputs: Float64Array,
  weight: number,
  grad: Float64Array,
) => OptOutputs;

export interface Description {
  /** zero by default */
  objective?: Num;
  /** empty by default */
  constraints?: Num[];
}

export interface Options {
  /** always false by default */
  until?(): boolean;
}

export interface Run {
  converged: boolean;
  /** doesn't include frozen */
  vals: Map<Var, number>;
  /** returns a new `Run`, leaving this one unchanged */
  run(opts: Options): Run;
}

export interface Config {
  /** uses `val` field by default */
  vals?(x: Var): number;
  /** always false by default */
  freeze?(x: Var): boolean;
}

export interface Problem {
  start(config: Config): Run;
}

//#endregion

//#region Types for generalizing our system autodiff

export type Pt2 = [Num, Num];

export const isPt2 = (vec: Num[]): vec is Pt2 => vec.length === 2;

//#endregion

// To view logs, use LogLevel.Trace, otherwese LogLevel.Warn
// const log = consola.create({ level: LogLevel.Trace }).withScope("Optimizer");
export const logAD = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Optimizer");

export const EPS_DENOM = builtins.epsilon; // Avoid divide-by-zero in denominator

export const variable = (val: number): Var => ({ tag: "Var", val });

const binary =
  (binop: Binary["binop"]) =>
  (v: Num, w: Num): Num => ({
    tag: "Binary",
    binop,
    left: v,
    right: w,
  });

const nary =
  (op: Nary["op"], bin: (v: Num, w: Num) => Num) =>
  (xs: Num[]): Num => {
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
        return { tag: "Nary", op, params: xs };
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
export const atan2 = (y: Num, x: Num): Num => ({
  tag: "Binary",
  binop: "atan2",
  left: y,
  right: x,
});

/**
 * Returns `pow(v,w)`.
 */
export const pow = binary("pow");

// --- Unary ops

const unary =
  (unop: Unary["unop"]) =>
  (v: Num): Num => ({
    tag: "Unary",
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

const comp =
  (binop: Comp["binop"]) =>
  (v: Num, w: Num): Bool => ({
    tag: "Comp",
    binop,
    left: v,
    right: w,
  });

/**
 * Return a conditional `v > w`.
 */
export const gt = comp(">");

/**
 * Return a conditional `v < w`.
 */
export const lt = comp("<");

/**
 * Return a conditional `v >= w`.
 */
export const gte = comp(">=");

/**
 * Return a conditional `v <= w`.
 */
export const lte = comp("<=");

/**
 * Return a conditional `v == w`. (TODO: Maybe check if they are equal up to a tolerance?)
 */
export const eq = comp("===");

const logic =
  (binop: Logic["binop"]) =>
  (v: Bool, w: Bool): Bool => ({
    tag: "Logic",
    binop,
    left: v,
    right: w,
  });

/**
 * Return a boolean `v && w`
 */
export const and = logic("&&");

/**
 * Return a boolean `v || w`
 */
export const or = logic("||");

/**
 * Return a boolean `v !== w`
 */
export const xor = logic("!==");

export const not = (v: Bool): Bool => ({ tag: "Not", param: v });

/**
 * Return a conditional `if(cond) then v else w`.
 */
export const ifCond = (cond: Bool, v: Num, w: Num): Num => ({
  tag: "Ternary",
  cond,
  then: v,
  els: w,
});

// --- Vector ops

/**
 * Return the roots of the monic polynomial with degree `coeffs.length` where
 * the coefficient on the term with degree `i` is `coeffs[i]`. Any root with a
 * nonzero imaginary component is replaced with `NaN`.
 */
export const polyRoots = (coeffs: Num[]): Num[] => {
  const nexus: PolyRoots = { tag: "PolyRoots", coeffs };
  return coeffs.map((coeff, index) => ({ tag: "Index", index, vec: nexus }));
};

/**
 * Some vector operations that can be used on `Num`.
 */
export const ops = {
  // Note that these ops MUST use the custom var ops for grads
  // Note that these ops are hardcoded to assume they are not applied to grad nodes

  /**
   * Return the norm of the 2-vector `[c1, c2]`.
   */
  norm: (c1: Num, c2: Num): Num => ops.vnorm([c1, c2]),

  /**
   * Return the Euclidean distance between scalars `c1, c2`.
   */
  dist: (c1: Num, c2: Num): Num => ops.vnorm([c1, c2]),

  /**
   * Return the sum of vectors `v1, v2`.
   */
  vadd: (v1: Num[], v2: Num[]): Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, add);
    return res;
  },

  /**
   * Return the sum of matrices `A1, A2`.
   */
  mmadd: (A1: Num[][], A2: Num[][]): Num[][] => {
    if (A1.length !== A2.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result = [];
    for (let i = 0; i < A1.length; i++) {
      const row = [];
      for (let j = 0; j < A1.length; j++) {
        row.push(add(A1[i][j], A2[i][j]));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Return the difference of matrices `A1, A2`.
   */
  mmsub: (A1: Num[][], A2: Num[][]): Num[][] => {
    if (A1.length !== A2.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result = [];
    for (let i = 0; i < A1.length; i++) {
      const row = [];
      for (let j = 0; j < A1.length; j++) {
        row.push(sub(A1[i][j], A2[i][j]));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Return the elementwise product of matrices `A1, A2`.
   */
  ewmmmul: (A1: Num[][], A2: Num[][]): Num[][] => {
    if (A1.length !== A2.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result = [];
    for (let i = 0; i < A1.length; i++) {
      const row = [];
      for (let j = 0; j < A1.length; j++) {
        row.push(mul(A1[i][j], A2[i][j]));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Return the elementwise quotient of matrices `A1, A2`.
   */
  ewmmdiv: (A1: Num[][], A2: Num[][]): Num[][] => {
    if (A1.length !== A2.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result = [];
    for (let i = 0; i < A1.length; i++) {
      const row = [];
      for (let j = 0; j < A1.length; j++) {
        row.push(div(A1[i][j], A2[i][j]));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Return the difference of vectors `v1` and `v2`.
   */
  vsub: (v1: Num[], v2: Num[]): Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, sub);
    return res;
  },

  /**
   * Return the elementwise product of vectors `v1` and `v2`.
   */
  ewvvmul: (v1: Num[], v2: Num[]): Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, mul);
    return res;
  },

  /**
   * Return the elementwise quotient of vectors `v1` and `v2`.
   */
  ewvvdiv: (v1: Num[], v2: Num[]): Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, div);
    return res;
  },

  /**
   * Return the Euclidean norm squared of vector `v`.
   */
  vnormsq: (v: Num[]): Num => {
    const res = v.map((e) => squared(e));
    return _.reduce(res, (x: Num, y) => add(x, y), 0);
    // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
  },

  /**
   * Return the Euclidean norm of vector `v`.
   */
  vnorm: (v: Num[]): Num => {
    const res = ops.vnormsq(v);
    return sqrt(res);
  },

  /**
   * Return the vector `v` multiplied by scalar `c`.
   */
  vmul: (c: Num, v: Num[]): Num[] => {
    return v.map((e) => mul(c, e));
  },

  /**
   * Return the scalar `c` times the Matrix `A`.
   */
  smmul: (c: Num, A: Num[][]): Num[][] => {
    return A.map(function (row) {
      return row.map((e) => mul(c, e));
    });
  },

  /**
   * Return the matrix `A` multiplied by vector `v`, i.e., Av.
   */
  mvmul: (A: Num[][], v: Num[]): Num[] => {
    if (A.length !== v.length) {
      throw Error("expected matrix and vector of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result: Num[] = [];
    for (let i = 0; i < v.length; i++) {
      const summands = _.zipWith(A[i], v, mul);
      result.push(summands.reduce((x: Num, y) => add(x, y), 0));
    }
    return result;
  },

  /**
   * Return the vector `v` multiplied by matrix `A`, i.e., v^T A.
   */
  vmmul: (v: Num[], A: Num[][]): Num[] => {
    if (A.length !== v.length) {
      throw Error("expected matrix and vector of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    // The easiest way to do left multiplication is to first
    // transpose the matrix A, since (A^T v)^T = v^T A.
    const AT: Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: Num[] = [];
      for (let j = 0; j < A.length; j++) {
        row.push(A[j][i]);
      }
      AT.push(row);
    }

    // Now we can just do an ordinary matrix-vector multiply with AT
    const result: Num[] = [];
    for (let i = 0; i < v.length; i++) {
      const summands = _.zipWith(AT[i], v, mul);
      result.push(summands.reduce((x: Num, y) => add(x, y), 0));
    }
    return result;
  },

  /**
   * Return the matrix `A` multiplied by matrix `B`.
   */
  mmmul: (A: Num[][], B: Num[][]): Num[][] => {
    if (A.length !== B.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    // To implement via reduction, need to turn the columns of B into rows,
    // i.e., need to construct the transpose matrix B'
    const BT: Num[][] = [];
    for (let i = 0; i < B.length; i++) {
      const row: Num[] = [];
      for (let j = 0; j < B.length; j++) {
        row.push(B[j][i]);
      }
      BT.push(row);
    }

    // Compute A*B via dot products of rows of A with rows of B'
    const result: Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: Num[] = [];
      for (let j = 0; j < A.length; j++) {
        const summands = _.zipWith(A[i], BT[j], mul);
        row.push(summands.reduce((x: Num, y) => add(x, y), 0));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Returns the entrywise product of two vectors, `v1` and `v2`
   */
  vproduct: (v1: Num[], v2: Num[]): Num[] => {
    const vresult = [];
    for (let i = 0; i < v1.length; i++) {
      vresult[i] = mul(v1[i], v2[i]);
    }
    return vresult;
  },

  /**
   * Return the entrywise absolute value of the vector `v`
   */
  vabs: (v: Num[]): Num[] => {
    return v.map((e) => absVal(e));
  },

  /**
   * Return the maximum value of each component of the vectors `v1` and `v2`
   */
  vmax: (v1: Num[], v2: Num[]): Num[] => {
    const vresult = [];
    for (let i = 0; i < v1.length; i++) {
      vresult[i] = max(v1[i], v2[i]);
    }
    return vresult;
  },

  /**
   * Return the vector `v`, scaled by `-1`.
   */
  vneg: (v: Num[]): Num[] => {
    return ops.vmul(-1, v);
  },

  /**
   * Return the transpose of the matrix `A`.
   */
  mtrans: (A: Num[][]): Num[][] => {
    const AT: Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: Num[] = [];
      for (let j = 0; j < A.length; j++) {
        row.push(A[j][i]);
      }
      AT.push(row);
    }
    return AT;
  },

  /**
   * Return the vector `v` divided by scalar `c`.
   */
  vdiv: (v: Num[], c: Num): Num[] => {
    return v.map((e) => div(e, c));
  },

  /**
   * Return the Matrix `A` divided by scalar `c`.
   */
  msdiv: (A: Num[][], c: Num): Num[][] => {
    return A.map(function (row) {
      return row.map((e) => div(e, c));
    });
  },

  /**
   * Return the vector `v`, normalized.
   */
  vnormalize: (v: Num[]): Num[] => {
    const vsize = add(ops.vnorm(v), EPS_DENOM);
    return ops.vdiv(v, vsize);
  },

  /**
   * Return the Euclidean distance between vectors `v` and `w`.
   */
  vdist: (v: Num[], w: Num[]): Num => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }
    return ops.vnorm(ops.vsub(v, w));
  },

  /**
   * Return the Euclidean distance squared between vectors `v` and `w`.
   */
  vdistsq: (v: Num[], w: Num[]): Num => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }

    return ops.vnormsq(ops.vsub(v, w));
  },

  /**
   * Return the dot product of vectors `v1, v2`.
   * Note: if you want to compute a norm squared, use `vnormsq` instead, it generates a smaller computational graph
   */
  vdot: (v1: Num[], v2: Num[]): Num => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, mul);
    return _.reduce(res, (x: Num, y) => add(x, y), 0);
  },

  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: (u: Num[], v: Num[]): Num => {
    if (u.length !== v.length) {
      throw Error("expected vectors of same length");
    }

    // Due to floating point error, the dot product of
    // two normalized vectors may fall slightly outside
    // the range [-1,1].  To prevent acos from producing
    // a NaN value, we therefore scale down the result
    // of the dot product by a factor s slightly below 1.
    const s = 1 - 1e-10;

    return acos(mul(s, ops.vdot(ops.vnormalize(u), ops.vnormalize(v))));
  },

  /**
   * Return the signed angle from vector `u` to vector `v`, in radians.
   * Assumes that both u and v are 2D vectors and have nonzero magnitude.
   * The returned value will be in the range [-pi,pi].
   */
  angleFrom: (u: Num[], v: Num[]): Num => {
    if (u.length !== v.length) {
      throw Error("expected vectors of same length");
    }

    return atan2(
      ops.cross2(u, v), // y = |u||v|sin(theta)
      ops.vdot(u, v), // x = |u||v|cos(theta)
    );
  },

  /**
   * Return the sum of elements in vector `v`.
   */
  vsum: (v: Num[]): Num => {
    return _.reduce(v, (x: Num, y) => add(x, y), 0);
  },

  /**
   * Return `v + c * u`.
   */
  vmove: (v: Num[], c: Num, u: Num[]): Num[] => {
    return ops.vadd(v, ops.vmul(c, u));
  },

  /**
   * Rotate a 2D point `[x, y]` by 90 degrees counterclockwise.
   */
  rot90: ([x, y]: Num[]): Num[] => {
    return [neg(y), x];
  },

  /**
   * Rotate a 2D point `[x, y]` by a degrees counterclockwise.
   */
  vrot: ([x, y]: Num[], a: Num): Num[] => {
    const angle = div(mul(a, Math.PI), 180);
    const x2 = sub(mul(cos(angle), x), mul(sin(angle), y));
    const y2 = add(mul(sin(angle), x), mul(cos(angle), y));
    return [x2, y2];
  },

  /**
   * Return 2D determinant/cross product of 2D vectors
   */
  cross2: (u: Num[], v: Num[]): Num => {
    if (u.length !== 2 || v.length !== 2) {
      throw Error("expected two 2-vectors");
    }
    return sub(mul(u[0], v[1]), mul(u[1], v[0]));
  },

  /**
   * Return 3D cross product of 3D vectors
   */
  cross3: (u: Num[], v: Num[]): Num[] => {
    if (u.length !== 3 || v.length !== 3) {
      throw Error("expected two 3-vectors");
    }
    return [
      sub(mul(u[1], v[2]), mul(u[2], v[1])),
      sub(mul(u[2], v[0]), mul(u[0], v[2])),
      sub(mul(u[0], v[1]), mul(u[1], v[0])),
    ];
  },

  /**
   * Return outer product matrix uv^T.  Vectors u and v must have
   * the same length.
   *
   * NOTE: This functionality is duplicated in `outerProduct()`
   * from Functions.ts.  Since `outerProduct` has a more directly
   * interpretable name, we may wish to deprecate `vouter` and
   * move `outerProduct` into `Autodiff.ts` in a future release.
   */
  vouter: (u: Num[], v: Num[]): Num[][] => {
    if (u.length !== v.length) {
      throw Error("vectors must have same length");
    }

    const result: Num[][] = [];
    for (let i = 0; i < u.length; i++) {
      const row = v.map((e) => mul(u[i], e));
      result.push(row);
    }

    return result;
  },
};

export const fns = {
  /**
   * Return the penalty `max(x, 0)`.
   */
  toPenalty: (x: Num): Num => {
    return squared(max(x, 0));
  },

  /**
   * Return the center of a shape.
   */
  center: (props: any): Num[] => {
    return props.center.contents;
  },
};

const sum = (n: number, f: (i: number) => rose.Real): rose.Real => {
  if (n === 0) return 0;
  let x = f(0);
  for (let i = 1; i < n; i++) x = rose.add(x, f(i));
  return x;
};

const penalty = rose.fn([rose.Real], rose.Real, (x) => {
  const y = builtins.max(x, 0);
  return rose.mul(y, y);
});

interface Node {
  indegree?: number;
  successors: Expr[];
}

interface Topsort {
  sorted: Expr[];
  nodes: Map<Expr, Node>;
}

const topsort = (seed: (set: (x: Expr) => void) => void): Topsort => {
  const nodes = new Map<Expr, Node>();
  const sorted: Expr[] = [];

  const stack: Expr[] = [];
  const set = (x: Expr): Node => {
    let node = nodes.get(x);
    if (node === undefined) {
      stack.push(x);
      node = { successors: [] };
      nodes.set(x, node);
    }
    return node;
  };
  seed((x) => {
    set(x);
  });
  const make = (x: Expr): number => {
    const succ = (y: Expr): void => {
      set(y).successors.push(x);
    };
    if (typeof x === "number") return 0;
    switch (x.tag) {
      case "Var":
        return 0;
      case "Not":
      case "Unary": {
        succ(x.param);
        return 1;
      }
      case "Binary":
      case "Comp":
      case "Logic": {
        succ(x.left);
        succ(x.right);
        return 2;
      }
      case "Ternary": {
        succ(x.cond);
        succ(x.then);
        succ(x.els);
        return 3;
      }
      case "Nary": {
        x.params.forEach(succ);
        return x.params.length;
      }
      case "PolyRoots": {
        x.coeffs.forEach(succ);
        return x.coeffs.length;
      }
      case "Index": {
        succ(x.vec);
        return 1;
      }
      case "Member": {
        succ(x.rec);
        return 1;
      }
      case "Call": {
        x.args.forEach(succ);
        return x.args.length;
      }
    }
  };
  while (stack.length > 0) {
    const x = stack.pop()!;
    const node = nodes.get(x)!;
    const n = node.indegree ?? make(x);
    node.indegree = n;
    if (n === 0) sorted.push(x);
  }

  return { nodes, sorted };
};

type RoseVal = rose.Bool | rose.Real | rose.Vec<unknown>;

const emitGraph = (
  { sorted, nodes }: Topsort,
  vars: Map<Expr, RoseVal>,
): void => {
  const emitUnary = (y: Unary): rose.Real => {
    const x = vars.get(y.param) as rose.Real;
    switch (y.unop) {
      case "neg":
        return rose.neg(x);
      case "squared":
        return rose.mul(x, x);
      case "sqrt":
        return builtins.sqrt(x);
      case "inverse":
        return rose.div(1, x);
      case "abs":
        return rose.abs(x);
      case "acosh":
        return builtins.acosh(x);
      case "acos":
        return builtins.acos(x);
      case "asin":
        return builtins.asin(x);
      case "asinh":
        return builtins.asinh(x);
      case "atan":
        return builtins.atan(x);
      case "atanh":
        return builtins.atanh(x);
      case "cbrt":
        return builtins.cbrt(x);
      case "ceil":
        return rose.ceil(x);
      case "cos":
        return builtins.cos(x);
      case "cosh":
        return builtins.cosh(x);
      case "exp":
        return builtins.exp(x);
      case "expm1":
        return builtins.expm1(x);
      case "floor":
        return rose.ceil(x);
      case "log":
        return builtins.log(x);
      case "log2":
        return builtins.log2(x);
      case "log10":
        return builtins.log10(x);
      case "log1p":
        return builtins.log1p(x);
      case "round":
        throw Error("rounding not supported");
      case "sign":
        return rose.sign(x);
      case "sin":
        return builtins.sin(x);
      case "sinh":
        return builtins.sinh(x);
      case "tan":
        return builtins.tan(x);
      case "tanh":
        return builtins.tanh(x);
      case "trunc":
        return rose.trunc(x);
    }
  };
  const emitBinary = (z: Binary): rose.Real => {
    const x = vars.get(z.left) as rose.Real;
    const y = vars.get(z.right) as rose.Real;
    switch (z.binop) {
      case "+":
        return rose.add(x, y);
      case "*":
        return rose.mul(x, y);
      case "-":
        return rose.sub(x, y);
      case "/":
        return rose.div(x, y);
      case "max":
        return builtins.max(x, y);
      case "min":
        return builtins.min(x, y);
      case "atan2":
        return builtins.atan2(x, y);
      case "pow":
        return builtins.pow(x, y);
    }
  };
  const emitComp = (z: Comp): rose.Bool => {
    const x = vars.get(z.left) as rose.Real;
    const y = vars.get(z.right) as rose.Real;
    switch (z.binop) {
      case ">":
        return rose.gt(x, y);
      case "<":
        return rose.lt(x, y);
      case "===":
        return rose.eq(x, y);
      case ">=":
        return rose.geq(x, y);
      case "<=":
        return rose.leq(x, y);
    }
  };
  const emitLogic = (r: Logic): rose.Bool => {
    const p = vars.get(r.left) as rose.Bool;
    const q = vars.get(r.right) as rose.Bool;
    switch (r.binop) {
      case "&&":
        return rose.and(p, q);
      case "||":
        return rose.or(p, q);
      case "!==":
        return rose.xor(p, q);
    }
  };
  const emitNary = (y: Nary): rose.Real => {
    const xs = y.params.map((x) => vars.get(x) as rose.Real);
    if (xs.length === 0) {
      switch (y.op) {
        case "addN":
          return 0;
        case "maxN":
          return -Infinity;
        case "minN":
          return Infinity;
      }
    } else {
      return xs.reduce((a, b) => {
        switch (y.op) {
          case "addN":
            return rose.add(a, b);
          case "maxN":
            return builtins.max(a, b);
          case "minN":
            return builtins.min(a, b);
        }
      });
    }
  };
  const emit = (x: Expr): RoseVal => {
    if (typeof x === "number") return x;
    switch (x.tag) {
      case "Var":
        return vars.get(x)!;
      case "Not":
        return rose.not(vars.get(x.param) as rose.Bool);
      case "Unary":
        return emitUnary(x);
      case "Binary":
        return emitBinary(x);
      case "Comp":
        return emitComp(x);
      case "Logic":
        return emitLogic(x);
      case "Ternary":
        return rose.select(
          vars.get(x.cond) as rose.Bool,
          rose.Real,
          vars.get(x.then) as rose.Real,
          vars.get(x.els) as rose.Real,
        );
      case "Nary":
        return emitNary(x);
      case "PolyRoots":
        throw Error("polynomial roots not supported");
      case "Index":
        return (vars.get(x.vec) as rose.Vec<unknown>)[x.index] as RoseVal;
      case "Member":
        return (vars.get(x.rec) as any)[x.member] as RoseVal;
      case "Call":
        return (x.func as any)(...x.args.map((y) => vars.get(y)));
    }
  };
  for (let i = 0; i < sorted.length; i++) {
    const x = sorted[i];
    vars.set(x, emit(x));
    for (const y of nodes.get(x)!.successors) {
      const node = nodes.get(y)!;
      const n = node.indegree!;
      if (n === 1) sorted.push(y);
      node.indegree = n - 1;
    }
  }
};

/** Generate an energy function from the current state (using `Num`s only) */
export const genGradient = async (
  inputs: Var[],
  objectives: Num[],
  constraints: Num[],
): Promise<Gradient> => {
  const n = inputs.length;
  const o = objectives.length;
  const c = constraints.length;

  const single = (x: Num) =>
    rose.fn([rose.Vec(n, rose.Real)], rose.Real, (varying) => {
      const graph = topsort((set) => {
        set(x);
      });
      const vars = new Map<Expr, RoseVal>();
      for (let i = 0; i < n; i++) {
        const v = inputs[i];
        if (graph.nodes.has(v)) vars.set(v, varying[i]);
      }
      emitGraph(graph, vars);
      return vars.get(x) as rose.Real;
    });

  const objFns = objectives.map(single);
  const constrFns = constraints.map(single);

  const basic = rose.fn(
    [rose.Vec(n, rose.Real)],
    { objectives: rose.Vec(o, rose.Real), constraints: rose.Vec(c, rose.Real) },
    (varying) => ({
      objectives: objFns.map((f) => f(varying)),
      constraints: constrFns.map((f) => f(varying)),
    }),
  );

  // fs.writeFileSync("pprint.txt", basic[rose.inner].pprint());

  const full = rose.fn(
    [
      rose.Vec(n, rose.Real),
      rose.Real,
      rose.Vec(o, rose.Bool),
      rose.Vec(c, rose.Bool),
    ],
    {
      phi: rose.Real,
      gradient: rose.Vec(n, rose.Real),
      objectives: rose.Vec(o, rose.Real),
      constraints: rose.Vec(c, rose.Real),
    },
    (varying, weight, objMask, constrMask) => {
      const { ret, grad } = rose.vjp(basic)(varying);
      const { objectives: objs, constraints: constrs } = ret;
      const Pair = rose.struct({ x: rose.Real, d: rose.Real });
      const zero = { x: 0, d: 0 };
      const masked = rose.vec(o, Pair, (i) =>
        rose.select(objMask[i], Pair, { x: objs[i], d: 1 }, zero),
      );
      const penalties = rose.vec(c, Pair, (i) => {
        const { ret: x, grad } = rose.vjp(penalty)(constrs[i]);
        return rose.select(constrMask[i], Pair, { x, d: grad(weight) }, zero);
      });
      return {
        phi: rose.add(
          sum(o, (i) => masked[i].x),
          rose.mul(
            weight,
            sum(c, (i) => penalties[i].x),
          ),
        ),
        gradient: grad({
          objectives: rose.vec(o, rose.Real, (i) => masked[i].d),
          constraints: rose.vec(c, rose.Real, (i) => penalties[i].d),
        }),
        objectives: objs,
        constraints: constrs,
      };
    },
  );

  const f = await rose.compile(full);

  return (
    { inputMask, objMask, constrMask }: Masks,
    inputs: Float64Array,
    weight: number,
    grad: Float64Array,
  ): OptOutputs => {
    if (inputMask.length !== n)
      throw Error(
        `expected ${n} inputs, got input mask with length ${inputMask.length}`,
      );
    if (objMask.length !== o)
      throw Error(
        `expected ${o} objectives, got objective mask with length ${objMask.length}`,
      );
    if (constrMask.length !== c)
      throw Error(
        `expected ${c} constraints, got constraint mask with length ${constrMask.length}`,
      );
    if (inputs.length !== n)
      throw Error(`expected ${n} inputs, got ${inputs.length}`);
    if (grad.length !== n)
      throw Error(
        `expected ${n} inputs, got gradient with length ${grad.length}`,
      );

    const out = f(Array.from(inputs), weight, objMask, constrMask);
    const { phi, gradient, objectives: objs, constraints: constrs } = out;
    for (let i = 0; i < n; i++) grad[i] = inputMask[i] ? gradient[i] : 0;
    return {
      phi,
      objectives: objMask.map((p, i) => (p ? objs[i] : 0)),
      constraints: constrMask.map((p, i) => (p ? constrs[i] : 0)),
    };
  };
};

const isConverged = (params: Params): boolean =>
  params.optStatus === "EPConverged";

export const problem = async (desc: Description): Promise<Problem> => {
  const obj = desc.objective ?? 0;
  const constrs = desc.constraints ?? [];
  const graph = topsort((set) => {
    set(obj);
    constrs.forEach(set);
  });
  const inputs: Var[] = [];
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") inputs.push(x);
  }
  const m = constrs.length;
  const n = inputs.length;

  const basic = rose.fn(
    [{ inputs: rose.Vec(n, rose.Real), weight: rose.Real }],
    rose.Real,
    ({ inputs: varying, weight }) => {
      const vars = new Map<Expr, RoseVal>();
      for (let i = 0; i < n; i++) vars.set(inputs[i], varying[i]);
      emitGraph(graph, vars);
      const pen = sum(m, (i) => penalty(vars.get(constrs[i]) as rose.Real));
      return rose.add(vars.get(obj) as rose.Real, rose.mul(weight, pen));
    },
  );

  const full = rose.fn(
    [rose.Vec(n, rose.Real), rose.Real],
    { phi: rose.Real, gradient: rose.Vec(n, rose.Real) },
    (varying, weight) => {
      const { ret: phi, grad } = rose.vjp(basic)({ inputs: varying, weight });
      return { phi, gradient: grad(1).inputs };
    },
  );

  const f = await rose.compile(full);

  return {
    start: (conf) => {
      const vals = conf.vals ?? ((x: Var) => x.val);
      const freeze = conf.freeze ?? (() => false);
      const mask: boolean[] = [];
      const init: number[] = [];
      // populate inputs with initial values from `vals`
      inputs.forEach((x, i) => {
        mask[i] = !freeze(x);
        init[i] = vals(x); // skip the weight input
      });
      const wrap = (xs: number[], params: Params): Run => {
        const unfrozen = new Map<Var, number>();
        // give back the optimized values
        inputs.forEach((x, i) => {
          if (!freeze(x)) unfrozen.set(x, xs[i]);
        });
        return {
          converged: isConverged(params),
          vals: unfrozen,
          run: ({ until }) => {
            // allocate a new array to store inputs
            const arr = new Float64Array(xs);
            let stop = false;
            let after = params;
            // ESLint complains that `stop` is always falsy, but it's wrong
            while (!(stop || isConverged(after))) {
              after = stepUntil(
                (
                  v: Float64Array /*read-only*/,
                  weight: number,
                  grad: Float64Array /*write-only*/,
                ): number => {
                  if (v.length !== n)
                    throw Error(`expected ${n} inputs, got ${v.length}`);
                  if (grad.length !== n)
                    throw Error(
                      `expected ${n} inputs, got gradient with length ${grad.length}`,
                    );
                  const { phi, gradient } = f(Array.from(v), weight);
                  for (let i = 0; i < n; i++)
                    grad[i] = mask[i] ? gradient[i] : 0;
                  return phi;
                },
                arr,
                after,
                () => {
                  if (until) stop = until();
                  return stop;
                },
              );
            }
            return wrap(Array.from(arr), after);
          },
        };
      };
      return wrap(init, start(n));
    },
  };
};

const makeFn = (xs: Num[]): { inputs: Var[]; f: rose.Fn } => {
  const graph = topsort((set) => {
    xs.forEach(set);
  });
  const inputs: Var[] = [];
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") inputs.push(x);
  }
  const m = xs.length;
  const n = inputs.length;
  const f = rose.fn([rose.Vec(n, rose.Real)], rose.Vec(m, rose.Real), (v) => {
    const vars = new Map<Expr, RoseVal>();
    for (let i = 0; i < n; i++) vars.set(inputs[i], v[i]);
    emitGraph(graph, vars);
    return xs.map((x) => vars.get(x) as rose.Real);
  });
  return { inputs, f };
};

export const interp = (
  xs: Num[],
): ((inputs: (x: Var) => number) => number[]) => {
  const { inputs, f } = makeFn(xs);
  const g = rose.interp(f as any) as any;
  return (vals) => g(inputs.map((x) => vals(x)));
};

export const compile = async (
  xs: Num[],
): Promise<(inputs: (x: Var) => number) => number[]> => {
  const { inputs, f } = makeFn(xs);
  const g = (await rose.compile(f as any)) as any;
  return (vals) => g(inputs.map((x) => vals(x)));
};
