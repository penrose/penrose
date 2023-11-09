import consola from "consola";
import _ from "lodash";
import { EigenvalueDecomposition, Matrix } from "ml-matrix";
import * as ad from "../types/ad.js";
import { topsort, zip2 } from "../utils/Util.js";
import {
  absVal,
  acos,
  add,
  addN,
  atan2,
  cos,
  cosh,
  div,
  eq,
  gt,
  ifCond,
  ln,
  lt,
  max,
  mul,
  neg,
  sign,
  sin,
  sinh,
  sqrt,
  squared,
  sub,
} from "./AutodiffFunctions.js";
import { Params, start, stepUntil } from "./Optimizer.js";

// To view logs, use LogLevel.Trace, otherwese LogLevel.Warn
// const log = consola.create({ level: LogLevel.Trace }).withScope("Optimizer");
export const logAD = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Optimizer");

export const EPS_DENOM = 10e-6; // Avoid divide-by-zero in denominator

export const variable = (val: number): ad.Var => ({ tag: "Var", val });

/**
 * Some vector operations that can be used on `ad.Num`.
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

const builtins = {
  polyRoots: (v: number[]): Float64Array => {
    const w = new Float64Array(v);
    polyRootsImpl(w);
    return w;
  },
};

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
    case "PolyRoots": {
      yield* x.coeffs;
      return;
    }
    case "Index": {
      yield x.vec;
      return;
    }
  }
}

const gradUnary = (y: ad.Unary, dy: ad.Num): ad.Num => {
  const x = y.param;
  switch (y.unop) {
    case "neg":
      return neg(dy);
    case "squared":
      return mul(dy, mul(2, x));
    case "sqrt":
      // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
      return mul(dy, div(1 / 2, max(EPS_DENOM, y)));
    case "inverse":
      return mul(dy, neg(squared(y)));
    case "abs":
      return mul(dy, sign(x));
    case "acosh":
      return div(dy, mul(sqrt(sub(x, 1)), sqrt(add(x, 1))));
    case "acos":
      return div(dy, neg(sqrt(sub(1, squared(x)))));
    case "asin":
      return div(dy, sqrt(sub(1, squared(x))));
    case "asinh":
      return div(dy, sqrt(add(1, squared(x))));
    case "atan":
      return div(dy, add(1, squared(x)));
    case "atanh":
      return div(dy, sub(1, squared(x)));
    case "cbrt":
      return mul(dy, div(1 / 3, squared(x)));
    case "ceil":
    case "floor":
    case "round":
    case "sign":
    case "trunc":
      return 0;
    case "cos":
      return mul(dy, neg(sin(x)));
    case "cosh":
      return mul(dy, sinh(x));
    case "exp":
      return mul(dy, y);
    case "expm1":
      return mul(dy, add(y, 1));
    case "log":
      return div(dy, x);
    case "log2":
      return mul(dy, div(Math.LOG2E, x));
    case "log10":
      return mul(dy, div(Math.LOG10E, x));
    case "log1p":
      return div(dy, add(1, x));
    case "sin":
      return mul(dy, cos(x));
    case "sinh":
      return mul(dy, cosh(x));
    case "tan":
      return mul(dy, add(1, squared(y)));
    case "tanh":
      return mul(dy, sub(1, squared(y)));
  }
};

function* gradsBinary(z: ad.Binary, dz: ad.Num) {
  const { binop, left: x, right: y } = z;
  switch (binop) {
    case "+": {
      yield dz;
      yield dz;
      return;
    }
    case "*": {
      yield mul(dz, y);
      yield mul(dz, x);
      return;
    }
    case "-": {
      yield dz;
      yield neg(dz);
      return;
    }
    case "/": {
      const dx = div(dz, y);
      yield dx;
      yield mul(dx, neg(z));
      return;
    }
    case "max": {
      const cond = gt(x, y);
      yield ifCond(cond, dz, 0);
      yield ifCond(cond, 0, dz);
      return;
    }
    case "min": {
      const cond = lt(x, y);
      yield ifCond(cond, dz, 0);
      yield ifCond(cond, 0, dz);
      return;
    }
    case "atan2": {
      const dw = div(dz, add(squared(x), squared(y)));
      yield mul(dw, y);
      yield mul(dw, neg(x));
      return;
    }
    case "pow": {
      const dw = mul(dz, z);
      yield mul(dw, div(y, x));
      yield mul(dw, ln(x));
      return;
    }
  }
}

function* gradsNary(y: ad.Nary, dy: ad.Num) {
  for (const x of y.params) {
    switch (y.op) {
      case "addN":
        yield dy;
        continue;
      case "maxN":
        yield ifCond(lt(x, y), 0, dy);
        continue;
      case "minN":
        yield ifCond(gt(x, y), 0, dy);
        continue;
    }
  }
}

// https://www.skewray.com/articles/how-do-the-roots-of-a-polynomial-depend-on-the-coefficients
const gradsPolyRoots = (v: ad.PolyRoots, dv: ad.Num[]): ad.Num[] => {
  const n = v.coeffs.length;
  const derivCoeffs: ad.Num[] = v.coeffs.map((c, i) => mul(i, c));
  derivCoeffs.shift();
  // the polynomial is assumed monic, so `x.coeffs` doesn't include the
  // coefficient 1 on the highest-degree term
  derivCoeffs.push(n);

  const sensitivities: ad.Num[][] = v.coeffs.map((_, index) => {
    const t: ad.Num = { tag: "Index", index, vec: v }; // a root

    let power: ad.Num = 1;
    const powers: ad.Num[] = [power];
    for (let i = 1; i < n; i++) {
      power = mul(power, t);
      powers.push(power);
    }

    const minusDerivative = neg(
      addN(zip2(derivCoeffs, powers).map(([c, p]) => mul(c, p))),
    );

    // if the root is `NaN` then it doesn't contribute to the gradient
    const real = eq(t, t);
    return powers.map((p) => ifCond(real, div(p, minusDerivative), 0));
  });

  return v.coeffs.map((child, i) =>
    addN(sensitivities.map((row, j) => mul(dv[j], row[i]))),
  );
};

const singleton = (dx: ad.Num): ad.Nary => {
  return { tag: "Nary", op: "addN", params: [dx] };
};

const gradsGraph = (y: ad.Num, dy: ad.Num): Map<ad.Num, ad.Num> => {
  const grads = new Map<ad.Num, ad.Nary>();
  grads.set(y, singleton(dy));
  const get = (x: ad.Num): ad.Num => grads.get(x) ?? 0;
  const accum = (x: ad.Num, dx: ad.Num) => {
    const grad = grads.get(x);
    if (grad === undefined) grads.set(x, singleton(dx));
    else grad.params.push(dx);
  };
  const vecGrads = new Map<ad.Vec, ad.Num[]>();
  for (const x of topsort(predsExpr, [y]).reverse()) {
    if (typeof x === "number") continue;
    switch (x.tag) {
      case "Var":
      case "Not":
      case "Comp":
      case "Logic":
        continue;
      case "Unary":
        accum(x.param, gradUnary(x, get(x)));
        continue;
      case "Binary": {
        const [da, db] = gradsBinary(x, get(x));
        accum(x.left, da);
        accum(x.right, db);
        continue;
      }
      case "Ternary": {
        const dx = get(x);
        accum(x.then, ifCond(x.cond, dx, 0));
        accum(x.els, ifCond(x.cond, 0, dx));
        continue;
      }
      case "Nary": {
        let i = 0;
        for (const da of gradsNary(x, get(x))) {
          accum(x.params[i], da);
          i++;
        }
        continue;
      }
      case "PolyRoots": {
        let i = 0;
        for (const da of gradsPolyRoots(x, vecGrads.get(x) ?? [])) {
          accum(x.coeffs[i], da);
          i++;
        }
        continue;
      }
      case "Index": {
        const { vec: v, index: i } = x;
        let dv = vecGrads.get(v);
        if (dv === undefined) {
          dv = [];
          vecGrads.set(v, dv);
        }
        if (i in dv) throw Error("multiple accesses to same vector element");
        dv[i] = get(x);
        continue;
      }
    }
  }
  return grads;
};

const emitUnary = (
  name: (x: ad.Expr) => string,
  { unop, param }: ad.Unary,
): string => {
  const x = name(param);
  switch (unop) {
    case "neg":
      return `-${x}`;
    case "squared":
      return `${x}*${x}`;
    case "inverse":
      return `1/${x}`;
    case "sqrt":
    case "abs":
    case "acosh":
    case "acos":
    case "asin":
    case "asinh":
    case "atan":
    case "atanh":
    case "cbrt":
    case "ceil":
    case "cos":
    case "cosh":
    case "exp":
    case "expm1":
    case "floor":
    case "log":
    case "log2":
    case "log10":
    case "log1p":
    case "round":
    case "sign":
    case "sin":
    case "sinh":
    case "tan":
    case "tanh":
    case "trunc": {
      return `Math.${unop}(${x})`;
    }
  }
};

const emitBinary = (
  name: (x: ad.Expr) => string,
  { binop, left, right }: ad.Binary | ad.Comp | ad.Logic,
): string => {
  const x = name(left);
  const y = name(right);
  switch (binop) {
    case "+":
    case "*":
    case "-":
    case "/":
    case ">":
    case "<":
    case ">=":
    case "<=":
    case "===":
    case "!==":
    case "&&":
    case "||":
      return `${x}${binop}${y}`;
    case "max":
    case "min":
    case "atan2":
    case "pow":
      return `Math.${binop}(${x},${y})`;
  }
};

const emitNary = (
  name: (x: ad.Expr) => string,
  { op, params }: ad.Nary,
): string => {
  switch (op) {
    case "addN":
      return params.length > 0 ? params.map(name).join("+") : "0";
    case "maxN":
      return `Math.max(${params.map(name).join(",")})`;
    case "minN":
      return `Math.min(${params.map(name).join(",")})`;
  }
};

const emitExpr = (
  name: (x: ad.Expr) => string,
  varCode: (x: ad.Var) => string,
  x: ad.Expr,
): string => {
  if (typeof x === "number") return `${x}`;
  switch (x.tag) {
    case "Var":
      return varCode(x);
    case "Not":
      return `!${name(x.param)}`;
    case "Unary":
      return emitUnary(name, x);
    case "Binary":
    case "Comp":
    case "Logic":
      return emitBinary(name, x);
    case "Ternary":
      return `${name(x.cond)}?${name(x.then)}:${name(x.els)}`;
    case "Nary":
      return emitNary(name, x);
    case "PolyRoots":
      return `builtins.polyRoots([${x.coeffs.map(name).join(",")}])`;
    case "Index":
      return `${name(x.vec)}[${x.index}]`;
  }
};

/**
 * This function assumes all variable names starting with `_` to be available.
 */
const emitGraph = (
  varCode: (x: ad.Var) => string,
  xs: ad.Expr[],
): { vars: Map<ad.Expr, string>; code: string[] } => {
  const code: string[] = [];
  const vars = new Map<ad.Expr, string>();
  for (const x of xs) {
    const lhs = `_${vars.size}`;
    const rhs = emitExpr((y) => vars.get(y)!, varCode, x);
    code.push(`const ${lhs}=${rhs}`);
    vars.set(x, lhs);
  }
  return { vars, code };
};

/** Generate an energy function from the current state (using `Num`s only) */
export const genGradient = (
  inputs: ad.Var[],
  objectives: ad.Num[],
  constraints: ad.Num[],
): ad.Gradient => {
  const n = inputs.length;
  const o = objectives.length;
  const c = constraints.length;

  const indices = new Map<ad.Var, number>(inputs.map((x, i) => [x, i]));

  const single = (f: (x: ad.Num) => ad.Num, y: ad.Num) => {
    const z = f(y);
    const dz = variable(1);
    const grads = gradsGraph(z, dz);
    const dx = inputs.map((w) => grads.get(w) ?? 0);
    const sorted = topsort(predsExpr, [y, z, ...dx]);
    const varCode = (x: ad.Var) => (x === dz ? "dz" : `x[${indices.get(x)}]`);
    const { vars, code } = emitGraph(varCode, sorted);
    dx.forEach((grad, i) => {
      if (grad !== 0) code.push(`dx[${i}]+=${vars.get(grad)}`);
    });
    code.push(`return {y:${vars.get(y)},z:${vars.get(z)}}`);
    const g = new Function("builtins", "x", "dx", "dz", code.join("\n"));
    return (
      x: Float64Array,
      dx: Float64Array,
      dz: number,
    ): { y: number; z: number } => g(builtins, x, dx, dz);
  };

  const objFns = objectives.map((x) => single((y) => y, x));
  const constrFns = constraints.map((x) => single(fns.toPenalty, x));

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

    grad.fill(0);

    let obj = 0;
    const objs = objFns.map((f, i) => {
      if (objMask[i]) {
        const { y, z } = f(inputs, grad, 1);
        obj += z;
        return y;
      } else return 0;
    });

    let constr = 0;
    const constrs = constrFns.map((f, i) => {
      if (constrMask[i]) {
        const { y, z } = f(inputs, grad, weight);
        constr += z;
        return y;
      } else return 0;
    });

    inputMask.forEach((p, i) => {
      if (!p) grad[i] = 0;
    });

    return {
      phi: obj + weight * constr,
      objectives: objs,
      constraints: constrs,
    };
  };
};

const isConverged = (params: Params): boolean =>
  params.optStatus === "EPConverged";

export const problem = async (desc: ad.Description): Promise<ad.Problem> => {
  const obj = desc.objective ?? 0;
  const constrs = desc.constraints ?? [];

  const lambda = variable(0);
  const y = add(obj, mul(lambda, addN(constrs.map(fns.toPenalty))));
  const grads = gradsGraph(y, 1);

  const indices = new Map<ad.Var, number>();
  for (const x of grads.keys()) {
    if (typeof x !== "number" && x.tag === "Var" && x !== lambda)
      indices.set(x, indices.size);
  }
  const n = indices.size;

  const sinks = [y];
  for (const x of indices.keys()) sinks.push(grads.get(x)!);
  const sorted = topsort(predsExpr, sinks);
  const varCode = (x: ad.Var) =>
    x === lambda ? "weight" : `x[${indices.get(x)}]`;
  const { vars, code } = emitGraph(varCode, sorted);
  for (const [x, i] of indices)
    code.push(`dx[${i}]+=${vars.get(grads.get(x)!)}`);
  code.push(`return ${vars.get(y)}`);
  const f = new Function("builtins", "x", "weight", "dx", code.join("\n"));

  return {
    start: (conf) => {
      const vals = conf.vals ?? ((x: ad.Var) => x.val);
      const freeze = conf.freeze ?? (() => false);
      const mask: boolean[] = [];
      const init: number[] = [];
      // populate inputs with initial values from `vals`
      for (const [x, i] of indices) {
        mask[i] = !freeze(x);
        init[i] = vals(x);
      }
      const wrap = (xs: number[], params: Params): ad.Run => {
        const unfrozen = new Map<ad.Var, number>();
        // give back the optimized values
        for (const [x, i] of indices) {
          if (!freeze(x)) unfrozen.set(x, xs[i]);
        }
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
                  grad.fill(0);
                  const phi = f(builtins, v, weight, grad);
                  mask.forEach((p, i) => {
                    if (!p) grad[i] = 0;
                  });
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

export const compile = (
  ys: ad.Num[],
): ((inputs: (x: ad.Var) => number) => number[]) => {
  const sorted = topsort(predsExpr, ys);

  const indices = new Map<ad.Var, number>();
  for (const x of sorted) {
    if (typeof x !== "number" && x.tag === "Var") indices.set(x, indices.size);
  }

  const { vars, code } = emitGraph((x) => `x[${indices.get(x)}]`, sorted);
  code.push(`return [${ys.map((x) => vars.get(x)).join(",")}]`);
  const f = new Function("builtins", "x", code.join("\n"));

  return (vals) => {
    const xs = new Float64Array(indices.size);
    for (const [x, i] of indices) xs[i] = vals(x);
    return f(builtins, xs);
  };
};
