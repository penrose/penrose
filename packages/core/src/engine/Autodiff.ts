import consola, { LogLevels } from "consola";
import _ from "lodash";
import { EigenvalueDecomposition, Matrix } from "ml-matrix";
import * as rose from "rose";
import * as ad from "../types/ad.js";
import { topsort } from "../utils/Util.js";
import {
  absVal,
  acos,
  add,
  atan2,
  cos,
  div,
  max,
  mul,
  neg,
  sin,
  sqrt,
  squared,
  sub,
} from "./AutodiffFunctions.js";
import * as builtins from "./Builtins.js";
import { Params, start, stepUntil } from "./Optimizer.js";

type SymbolicParams<T> = {
  [K in keyof T]: rose.Symbolic<T[K]>;
};

type ValueParams<T> = {
  [K in keyof T]: rose.Value<T[K]>;
};

export type FromRose<T> = T extends rose.Bools
  ? ad.Bool
  : T extends rose.Reals
  ? ad.Num
  : T extends rose.Vecs<unknown, infer V>
  ? FromRose<V>[]
  : { [K in keyof T]: FromRose<T[K]> };

type RoseArgs<T> = {
  [K in keyof T]: FromRose<T[K]>;
};

// generate Penrose autodiff nodes representing glue code to create Rose values,
// e.g. to call a Rose function
const toRose = <T>(t: T): ((x: FromRose<T>) => ad.Expr) => {
  if (t === rose.Bool || t === rose.Real) return (x: any) => x;
  const syms = Object.getOwnPropertySymbols(t);
  // this "elem" symbol is internal to Rose so we can't just import it; we can
  // extract it in this manner by finding a symbol with the right `description`,
  // but that is specifically bypassing encapsulation, and therefore might break
  // unexpectedly
  const sym = syms.find((sym) => sym.description === "elem");
  if (sym === undefined) {
    const mems = Object.entries(t as any).map(
      ([k, v]): [string, (x: any) => ad.Expr] => [k, toRose(v)],
    );
    return (x: any) => ({
      tag: "LitRec",
      mems: Object.fromEntries(mems.map(([k, mem]) => [k, mem(x[k])])),
    });
  } else {
    const elem = toRose((t as any)[sym]);
    return (x: any) => ({ tag: "LitVec", elems: x.map(elem) });
  }
};

// generate Rose values from Penrose autodiff nodes that represented glue code,
// e.g. to call a Rose function
const fromRose = <T>(t: T): ((x: ad.Expr) => FromRose<T>) => {
  if (t === rose.Bool || t === rose.Real) return (x: any) => x;
  const syms = Object.getOwnPropertySymbols(t);
  // see above comment explaining why this is sus
  const sym = syms.find((sym) => sym.description === "elem");
  if (sym === undefined) {
    const mems = Object.entries(t as any).map(
      ([k, v]): [string, (x: ad.Expr) => any] => [k, fromRose(v)],
    );
    return (x: any): any =>
      Object.fromEntries(
        mems.map(([k, mem]) => [k, mem({ tag: "Member", rec: x, member: k })]),
      );
  } else {
    const n = (t as any)[syms.find((sym) => sym.description === "index")!];
    const elem = fromRose((t as any)[sym]);
    return (x: any): any => {
      const elems = [];
      for (let i = 0; i < n; i++)
        elems.push(elem({ tag: "Index", vec: x, index: i }));
      return elems;
    };
  }
};

export const fn = <const P extends readonly unknown[], const R>(
  params: P,
  ret: R,
  f: (...args: SymbolicParams<P>) => rose.Value<R>,
): rose.Fn &
  ((...args: ValueParams<P>) => rose.Symbolic<R>) & {
    rose(...args: RoseArgs<P>): FromRose<R>;
  } => {
  const g: any = rose.fn(params, ret, f);
  const paramFns = params.map(toRose);
  const retFn = fromRose(ret);
  g.rose = (...args: any): any =>
    retFn({ tag: "Call", fn: g, args: paramFns.map((h, i) => h(args[i])) });
  return g;
};

// To view logs, use LogLevel.Trace, otherwese LogLevel.Warn
// const log = consola.create({ level: LogLevel.Trace }).withScope("Optimizer");
export const logAD = consola
  .create({ level: LogLevels.warn })
  .withTag("Optimizer");

export const EPS_DENOM = builtins.epsilon; // Avoid divide-by-zero in denominator

export const variable = (val: number): ad.Var => ({ tag: "Var", val });

/**
 * Some vector operations that can be used on `Num`.
 */
export const ops = {
  // Note that these ops MUST use the custom var ops for grads
  // Note that these ops are hardcoded to assume they are not applied to grad nodes

  /**
   * Return the norm of the 2-vector `[c1, c2]`.
   */
  norm: (c1: ad.Num, c2: ad.Num): ad.Num => ops.vnorm([c1, c2]),

  /**
   * Return the Euclidean distance between scalars `c1, c2`.
   */
  dist: (c1: ad.Num, c2: ad.Num): ad.Num => ops.vnorm([c1, c2]),

  /**
   * Return the sum of vectors `v1, v2`.
   */
  vadd: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, add);
    return res;
  },

  /**
   * Return the sum of matrices `A1, A2`.
   */
  mmadd: (A1: ad.Num[][], A2: ad.Num[][]): ad.Num[][] => {
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
  mmsub: (A1: ad.Num[][], A2: ad.Num[][]): ad.Num[][] => {
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
  ewmmmul: (A1: ad.Num[][], A2: ad.Num[][]): ad.Num[][] => {
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
  ewmmdiv: (A1: ad.Num[][], A2: ad.Num[][]): ad.Num[][] => {
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
  vsub: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, sub);
    return res;
  },

  /**
   * Return the elementwise product of vectors `v1` and `v2`.
   */
  ewvvmul: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, mul);
    return res;
  },

  /**
   * Return the elementwise quotient of vectors `v1` and `v2`.
   */
  ewvvdiv: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, div);
    return res;
  },

  /**
   * Return the Euclidean norm squared of vector `v`.
   */
  vnormsq: (v: ad.Num[]): ad.Num => {
    const res = v.map((e) => squared(e));
    return _.reduce(res, (x: ad.Num, y) => add(x, y), 0);
    // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
  },

  /**
   * Return the Euclidean norm of vector `v`.
   */
  vnorm: (v: ad.Num[]): ad.Num => {
    const res = ops.vnormsq(v);
    return sqrt(res);
  },

  /**
   * Return the vector `v` multiplied by scalar `c`.
   */
  vmul: (c: ad.Num, v: ad.Num[]): ad.Num[] => {
    return v.map((e) => mul(c, e));
  },

  /**
   * Return the scalar `c` times the Matrix `A`.
   */
  smmul: (c: ad.Num, A: ad.Num[][]): ad.Num[][] => {
    return A.map(function (row) {
      return row.map((e) => mul(c, e));
    });
  },

  /**
   * Return the matrix `A` multiplied by vector `v`, i.e., Av.
   */
  mvmul: (A: ad.Num[][], v: ad.Num[]): ad.Num[] => {
    if (A.length !== v.length) {
      throw Error("expected matrix and vector of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    const result: ad.Num[] = [];
    for (let i = 0; i < v.length; i++) {
      const summands = _.zipWith(A[i], v, mul);
      result.push(summands.reduce((x: ad.Num, y) => add(x, y), 0));
    }
    return result;
  },

  /**
   * Return the vector `v` multiplied by matrix `A`, i.e., v^T A.
   */
  vmmul: (v: ad.Num[], A: ad.Num[][]): ad.Num[] => {
    if (A.length !== v.length) {
      throw Error("expected matrix and vector of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    // The easiest way to do left multiplication is to first
    // transpose the matrix A, since (A^T v)^T = v^T A.
    const AT: ad.Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: ad.Num[] = [];
      for (let j = 0; j < A.length; j++) {
        row.push(A[j][i]);
      }
      AT.push(row);
    }

    // Now we can just do an ordinary matrix-vector multiply with AT
    const result: ad.Num[] = [];
    for (let i = 0; i < v.length; i++) {
      const summands = _.zipWith(AT[i], v, mul);
      result.push(summands.reduce((x: ad.Num, y) => add(x, y), 0));
    }
    return result;
  },

  /**
   * Return the matrix `A` multiplied by matrix `B`.
   */
  mmmul: (A: ad.Num[][], B: ad.Num[][]): ad.Num[][] => {
    if (A.length !== B.length) {
      throw Error("expected matrices of same size");
      // note that we don't check the column dimensions separately,
      // since we support only square (NxN) matrices
    }

    // To implement via reduction, need to turn the columns of B into rows,
    // i.e., need to construct the transpose matrix B'
    const BT: ad.Num[][] = [];
    for (let i = 0; i < B.length; i++) {
      const row: ad.Num[] = [];
      for (let j = 0; j < B.length; j++) {
        row.push(B[j][i]);
      }
      BT.push(row);
    }

    // Compute A*B via dot products of rows of A with rows of B'
    const result: ad.Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: ad.Num[] = [];
      for (let j = 0; j < A.length; j++) {
        const summands = _.zipWith(A[i], BT[j], mul);
        row.push(summands.reduce((x: ad.Num, y) => add(x, y), 0));
      }
      result.push(row);
    }
    return result;
  },

  /**
   * Returns the entrywise product of two vectors, `v1` and `v2`
   */
  vproduct: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    const vresult = [];
    for (let i = 0; i < v1.length; i++) {
      vresult[i] = mul(v1[i], v2[i]);
    }
    return vresult;
  },

  /**
   * Return the entrywise absolute value of the vector `v`
   */
  vabs: (v: ad.Num[]): ad.Num[] => {
    return v.map((e) => absVal(e));
  },

  /**
   * Return the maximum value of each component of the vectors `v1` and `v2`
   */
  vmax: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    const vresult = [];
    for (let i = 0; i < v1.length; i++) {
      vresult[i] = max(v1[i], v2[i]);
    }
    return vresult;
  },

  /**
   * Return the vector `v`, scaled by `-1`.
   */
  vneg: (v: ad.Num[]): ad.Num[] => {
    return ops.vmul(-1, v);
  },

  /**
   * Return the transpose of the matrix `A`.
   */
  mtrans: (A: ad.Num[][]): ad.Num[][] => {
    const AT: ad.Num[][] = [];
    for (let i = 0; i < A.length; i++) {
      const row: ad.Num[] = [];
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
  vdiv: (v: ad.Num[], c: ad.Num): ad.Num[] => {
    return v.map((e) => div(e, c));
  },

  /**
   * Return the Matrix `A` divided by scalar `c`.
   */
  msdiv: (A: ad.Num[][], c: ad.Num): ad.Num[][] => {
    return A.map(function (row) {
      return row.map((e) => div(e, c));
    });
  },

  /**
   * Return the vector `v`, normalized.
   */
  vnormalize: (v: ad.Num[]): ad.Num[] => {
    const vsize = add(ops.vnorm(v), EPS_DENOM);
    return ops.vdiv(v, vsize);
  },

  /**
   * Return the Euclidean distance between vectors `v` and `w`.
   */
  vdist: (v: ad.Num[], w: ad.Num[]): ad.Num => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }
    return ops.vnorm(ops.vsub(v, w));
  },

  /**
   * Return the Euclidean distance squared between vectors `v` and `w`.
   */
  vdistsq: (v: ad.Num[], w: ad.Num[]): ad.Num => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }

    return ops.vnormsq(ops.vsub(v, w));
  },

  /**
   * Return the dot product of vectors `v1, v2`.
   * Note: if you want to compute a norm squared, use `vnormsq` instead, it generates a smaller computational graph
   */
  vdot: (v1: ad.Num[], v2: ad.Num[]): ad.Num => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, mul);
    return _.reduce(res, (x: ad.Num, y) => add(x, y), 0);
  },

  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: (u: ad.Num[], v: ad.Num[]): ad.Num => {
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
  angleFrom: (u: ad.Num[], v: ad.Num[]): ad.Num => {
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
  vsum: (v: ad.Num[]): ad.Num => {
    return _.reduce(v, (x: ad.Num, y) => add(x, y), 0);
  },

  /**
   * Return `v + c * u`.
   */
  vmove: (v: ad.Num[], c: ad.Num, u: ad.Num[]): ad.Num[] => {
    return ops.vadd(v, ops.vmul(c, u));
  },

  /**
   * Rotate a 2D point `[x, y]` by 90 degrees counterclockwise.
   */
  rot90: ([x, y]: ad.Num[]): ad.Num[] => {
    return [neg(y), x];
  },

  /**
   * Rotate a 2D point `[x, y]` by a degrees counterclockwise.
   */
  vrot: ([x, y]: ad.Num[], a: ad.Num): ad.Num[] => {
    const angle = div(mul(a, Math.PI), 180);
    const x2 = sub(mul(cos(angle), x), mul(sin(angle), y));
    const y2 = add(mul(sin(angle), x), mul(cos(angle), y));
    return [x2, y2];
  },

  /**
   * Return 2D determinant/cross product of 2D vectors
   */
  cross2: (u: ad.Num[], v: ad.Num[]): ad.Num => {
    if (u.length !== 2 || v.length !== 2) {
      throw Error("expected two 2-vectors");
    }
    return sub(mul(u[0], v[1]), mul(u[1], v[0]));
  },

  /**
   * Return 3D cross product of 3D vectors
   */
  cross3: (u: ad.Num[], v: ad.Num[]): ad.Num[] => {
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
  vouter: (u: ad.Num[], v: ad.Num[]): ad.Num[][] => {
    if (u.length !== v.length) {
      throw Error("vectors must have same length");
    }

    const result: ad.Num[][] = [];
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
  toPenalty: (x: ad.Num): ad.Num => {
    return squared(max(x, 0));
  },

  /**
   * Return the center of a shape.
   */
  center: (props: any): ad.Num[] => {
    return props.center.contents;
  },
};

/**
 * Replaces the contents of `v` with the roots of the monic polynomial whose
 * degree is the length of the vector and whose coefficient with a given degree
 * is the element of the vector at that index. Any root with a nonzero imaginary
 * component is replaced with `NaN`.
 */
export const polyRootsImpl = (v: Float64Array): void => {
  const n = v.length;
  // https://en.wikipedia.org/wiki/Companion_matrix
  const m = Matrix.zeros(n, n);
  for (let i = 0; i + 1 < n; i++) {
    m.set(i + 1, i, 1);
    m.set(i, n - 1, -v[i]);
  }
  m.set(n - 1, n - 1, -v[n - 1]);

  // the characteristic polynomial of the companion matrix is equal to the
  // original polynomial, so by finding the eigenvalues of the companion matrix,
  // we get the roots of its characteristic polynomial and thus of the original
  // polynomial
  const r = new EigenvalueDecomposition(m);
  for (let i = 0; i < n; i++) {
    // as mentioned in the `polyRoots` docstring in `engine/AutodiffFunctions`,
    // we discard any non-real root and replace with `NaN`
    v[i] = r.imaginaryEigenvalues[i] === 0 ? r.realEigenvalues[i] : NaN;
  }
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

function* predsExpr(x: ad.Expr): Generator<ad.Expr, void, undefined> {
  if (typeof x === "number") return;
  switch (x.tag) {
    case "Var":
      return;
    case "Not":
    case "Unary":
      yield x.param;
      return;
    case "Binary":
    case "Comp":
    case "Logic": {
      yield x.left;
      yield x.right;
      return;
    }
    case "Ternary": {
      yield x.cond;
      yield x.then;
      yield x.els;
      return;
    }
    case "Nary": {
      yield* x.params;
      return;
    }
    case "LitVec": {
      yield* x.elems;
      return;
    }
    case "PolyRoots": {
      yield* x.coeffs;
      return;
    }
    case "LitRec": {
      yield* Object.values(x.mems);
      return;
    }
    case "Index": {
      yield x.vec;
      return;
    }
    case "Member": {
      yield x.rec;
      return;
    }
    case "Call": {
      yield* x.args;
      return;
    }
  }
}

// this type is very incomplete but it excludes `undefined` so at least it makes
// sure we handle all necessary cases in `switch` statements
type RoseVal =
  | rose.Bool
  | rose.Real
  | rose.Vec<unknown>
  | RoseVal[]
  | { [K: string]: RoseVal };

const emitUnary = (
  get: (x: ad.Expr) => RoseVal,
  { unop, param }: ad.Unary,
): rose.Real => {
  const x = get(param) as rose.Real;
  switch (unop) {
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
      return rose.floor(x);
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

const emitBinary = (
  get: (x: ad.Expr) => RoseVal,
  { binop, left, right }: ad.Binary,
): rose.Real => {
  const x = get(left) as rose.Real;
  const y = get(right) as rose.Real;
  switch (binop) {
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

const emitComp = (
  get: (x: ad.Expr) => RoseVal,
  { binop, left, right }: ad.Comp,
): rose.Bool => {
  const x = get(left) as rose.Real;
  const y = get(right) as rose.Real;
  switch (binop) {
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

const emitLogic = (
  get: (x: ad.Expr) => RoseVal,
  { binop, left, right }: ad.Logic,
): rose.Bool => {
  const p = get(left) as rose.Bool;
  const q = get(right) as rose.Bool;
  switch (binop) {
    case "&&":
      return rose.and(p, q);
    case "||":
      return rose.or(p, q);
    case "!==":
      return rose.xor(p, q);
  }
};

const emitNary = (
  get: (x: ad.Expr) => RoseVal,
  { op, params }: ad.Nary,
): rose.Real => {
  const xs = params.map((x) => get(x) as rose.Real);
  if (xs.length === 0) {
    switch (op) {
      case "addN":
        return 0;
      case "maxN":
        return -Infinity;
      case "minN":
        return Infinity;
    }
  } else {
    return xs.reduce((a, b) => {
      switch (op) {
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

const emitExpr = (
  get: (x: ad.Expr) => RoseVal,
  getVar: (x: ad.Var) => rose.Real,
  x: ad.Expr,
): RoseVal => {
  if (typeof x === "number") return x;
  switch (x.tag) {
    case "Var":
      return getVar(x);
    case "Not":
      return rose.not(get(x.param) as rose.Bool);
    case "Unary":
      return emitUnary(get, x);
    case "Binary":
      return emitBinary(get, x);
    case "Comp":
      return emitComp(get, x);
    case "Logic":
      return emitLogic(get, x);
    case "Ternary":
      return rose.select(
        get(x.cond) as rose.Bool,
        rose.Real,
        get(x.then) as rose.Real,
        get(x.els) as rose.Real,
      );
    case "Nary":
      return emitNary(get, x);
    case "LitVec":
      return x.elems.map((elem) => get(elem));
    case "PolyRoots":
      throw Error("polynomial roots not supported");
    case "LitRec":
      return Object.fromEntries(
        Object.entries(x.mems).map(([k, v]) => [k, get(v)]),
      );
    case "Index":
      return (get(x.vec) as RoseVal[])[x.index];
    case "Member":
      return (get(x.rec) as Record<string, RoseVal>)[x.member];
    case "Call":
      return (x.fn as any)(...x.args.map((arg) => get(arg)));
  }
};

const emitGraph = (
  getVar: (x: ad.Var) => rose.Real,
  xs: ad.Expr[],
): Map<ad.Expr, RoseVal> => {
  const vals = new Map<ad.Expr, RoseVal>();
  for (const x of xs) {
    const val = emitExpr((x) => vals.get(x)!, getVar, x);
    vals.set(x, val);
  }
  return vals;
};

/** NOTE: this is a global memory instance for each thread. Another option is to expose this in the `core` API,
 * but it wouldn't make sense for each diagram to have their own memory instance.
 * If that's the case, it's a tricky to expose this all the way at the core API level and ask the user to pass
 * a `Memory` instance around.
 **/
const memory = new WebAssembly.Memory({ initial: 0 });

/** Generate an energy function from the current state (using `Num`s only) */
export const genGradient = async (
  inputs: ad.Var[],
  objectives: ad.Num[],
  constraints: ad.Num[],
): Promise<ad.Gradient> => {
  const n = inputs.length;
  const o = objectives.length;
  const c = constraints.length;

  const indices = new Map<ad.Var, number>(inputs.map((x, i) => [x, i]));

  const single = (y: ad.Num) =>
    rose.fn([rose.Vec(n, rose.Real)], rose.Real, (varying) => {
      const sorted = topsort(predsExpr, [y]);
      const vals = emitGraph((x) => varying[indices.get(x)!], sorted);
      return vals.get(y) as rose.Real;
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

  const f = await rose.compile(full, { memory });

  return (
    { inputMask, objMask, constrMask }: ad.Masks,
    inputs: Float64Array,
    weight: number,
    grad: Float64Array,
  ): ad.OptOutputs => {
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

export const problem = async (desc: ad.Description): Promise<ad.Problem> => {
  const obj = desc.objective ?? 0;
  const constrs = desc.constraints ?? [];
  const m = constrs.length;

  const sorted = topsort(predsExpr, [obj, ...constrs]);
  const inputs: ad.Var[] = [];
  const indices = new Map<ad.Var, number>();
  for (const x of sorted) {
    if (typeof x !== "number" && x.tag === "Var") {
      inputs.push(x);
      indices.set(x, indices.size);
    }
  }
  const n = inputs.length;

  const basic = rose.fn(
    [{ inputs: rose.Vec(n, rose.Real), weight: rose.Real }],
    rose.Real,
    ({ inputs: varying, weight }) => {
      const vals = emitGraph((x) => varying[indices.get(x)!], sorted);
      const pen = sum(m, (i) => penalty(vals.get(constrs[i]) as rose.Real));
      return rose.add(vals.get(obj) as rose.Real, rose.mul(weight, pen));
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

  const f = await rose.compile(full, { memory });

  return {
    start: (conf) => {
      const vals = conf.vals ?? ((x: ad.Var) => x.val);
      const freeze = conf.freeze ?? (() => false);
      const mask: boolean[] = [];
      const init: number[] = [];
      // populate inputs with initial values from `vals`
      inputs.forEach((x, i) => {
        mask[i] = !freeze(x);
        init[i] = vals(x); // skip the weight input
      });
      const wrap = (xs: number[], params: Params): ad.Run => {
        const unfrozen = new Map<ad.Var, number>();
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

const makeFn = (ys: ad.Num[]): { inputs: ad.Var[]; f: rose.Fn } => {
  const sorted = topsort(predsExpr, ys);

  const inputs: ad.Var[] = [];
  const indices = new Map<ad.Var, number>();
  for (const x of sorted) {
    if (typeof x !== "number" && x.tag === "Var") {
      inputs.push(x);
      indices.set(x, indices.size);
    }
  }

  const m = ys.length;
  const n = inputs.length;
  const f = rose.fn([rose.Vec(n, rose.Real)], rose.Vec(m, rose.Real), (v) => {
    const vals = emitGraph((x) => v[indices.get(x)!], sorted);
    return ys.map((y) => vals.get(y) as rose.Real);
  });
  return { inputs, f };
};

export const interp = (
  ys: ad.Num[],
): ((inputs: (x: ad.Var) => number) => number[]) => {
  const { inputs, f } = makeFn(ys);
  const g = rose.interp(f as any) as any;
  return (vals) => g(inputs.map((x) => vals(x)));
};

export const compile = async (
  ys: ad.Num[],
): Promise<(inputs: (x: ad.Var) => number) => number[]> => {
  const { inputs, f } = makeFn(ys);
  const g = (await rose.compile(f as any, { memory })) as any;
  return (vals) => g(inputs.map((x) => vals(x)));
};
