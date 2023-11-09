import consola from "consola";
import _ from "lodash";
import { EigenvalueDecomposition, Matrix } from "ml-matrix";
import * as ad from "../types/ad.js";
import { topsort, unwrap, zip2 } from "../utils/Util.js";
import * as wasm from "../utils/Wasm.js";
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

// ----- Autodiff

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

// ----- Codegen

// Traverses the computational graph of ops obtained by interpreting the energy function, and generates WebAssembly code corresponding to just the ops

const importModule = "";
const importMemoryName = "";
const exportFunctionName = "";

type BuiltinType = "unary" | "binary" | "polyRoots";

const builtins = new Map<string, BuiltinType>([
  ["inverse", "unary"],

  ["acos", "unary"],
  ["acosh", "unary"],
  ["asin", "unary"],
  ["asinh", "unary"],
  ["atan", "unary"],
  ["atanh", "unary"],
  ["cbrt", "unary"],
  ["cos", "unary"],
  ["cosh", "unary"],
  ["exp", "unary"],
  ["expm1", "unary"],
  ["log", "unary"],
  ["log1p", "unary"],
  ["log10", "unary"],
  ["log2", "unary"],
  ["sign", "unary"],
  ["sin", "unary"],
  ["sinh", "unary"],
  ["tan", "unary"],
  ["tanh", "unary"],

  ["atan2", "binary"],
  ["pow", "binary"],

  ["polyRoots", "polyRoots"],
]);

const bytesI32 = Int32Array.BYTES_PER_ELEMENT;
const logAlignI32 = Math.log2(bytesI32);

const bytesF64 = Float64Array.BYTES_PER_ELEMENT;
const logAlignF64 = Math.log2(bytesF64);

interface Signature {
  param: { [name: string]: number };
  result: number[];
}

const funcTypes = {
  unary: { param: { x: wasm.TYPE.f64 }, result: [wasm.TYPE.f64] },
  binary: {
    param: { x: wasm.TYPE.f64, y: wasm.TYPE.f64 },
    result: [wasm.TYPE.f64],
  },
  polyRoots: {
    param: { pointer: wasm.TYPE.i32, size: wasm.TYPE.i32 },
    result: [],
  },
  addend: {
    param: {
      input: wasm.TYPE.i32,
      gradient: wasm.TYPE.i32,
      secondary: wasm.TYPE.i32,
      stackPointer: wasm.TYPE.i32,
    },
    result: [wasm.TYPE.f64],
  },
  sum: {
    param: {
      input: wasm.TYPE.i32,
      mask: wasm.TYPE.i32,
      gradient: wasm.TYPE.i32,
      secondary: wasm.TYPE.i32,
      stackPointer: wasm.TYPE.i32,
    },
    result: [wasm.TYPE.f64],
  },
};

const getTypeIndex = (kind: string): number =>
  Object.keys(funcTypes).indexOf(kind);

const getParamIndex = (sig: Signature, name: string): number =>
  Object.keys(sig.param).indexOf(name);

const builtindex = new Map([...builtins.keys()].map((name, i) => [name, i]));

const getBuiltindex = (name: string): number =>
  unwrap(builtindex.get(name), () => `unknown builtin: ${name}`);

const typeSection = (t: wasm.Target): void => {
  t.int(Object.keys(funcTypes).length);

  for (const { param, result } of Object.values(funcTypes)) {
    t.byte(wasm.TYPE.FUNCTION);
    t.int(Object.keys(param).length);
    for (const typ of Object.values(param)) t.byte(typ);
    t.int(result.length);
    for (const typ of result) t.byte(typ);
  }
};

const importSection = (t: wasm.Target): void => {
  const numImports = 1 + builtins.size;
  t.int(numImports);

  const minPages = 1;
  t.ascii(importModule);
  t.ascii(importMemoryName);
  t.byte(wasm.IMPORT.MEMORY);
  t.byte(wasm.LIMITS.NO_MAXIMUM);
  t.int(minPages);

  [...builtins.entries()].forEach(([, kind], i) => {
    t.ascii(importModule);
    t.ascii(i.toString(36));
    t.byte(wasm.IMPORT.FUNCTION);
    t.int(getTypeIndex(kind));
  });
};

const functionSection = (t: wasm.Target, numAddends: number): void => {
  t.int(numAddends + 1);
  for (let i = 0; i < numAddends; i++) t.int(getTypeIndex("addend"));
  t.int(getTypeIndex("sum"));
};

const exportSection = (t: wasm.Target, numAddends: number): void => {
  const numExports = 1;
  t.int(numExports);

  const funcIndex = builtins.size + numAddends;
  t.ascii(exportFunctionName);
  t.byte(wasm.EXPORT.FUNCTION);
  t.int(funcIndex);
};

const modulePrefix = (gradientFunctionSizes: number[]): wasm.Module => {
  const numSections = 5;
  const numAddends = gradientFunctionSizes.length - 1;

  const typeSectionCount = new wasm.Count();
  typeSection(typeSectionCount);
  const typeSectionSize = typeSectionCount.size;

  const importSectionCount = new wasm.Count();
  importSection(importSectionCount);
  const importSectionSize = importSectionCount.size;

  const functionSectionCount = new wasm.Count();
  functionSection(functionSectionCount, numAddends);
  const functionSectionSize = functionSectionCount.size;

  const exportSectionCount = new wasm.Count();
  exportSection(exportSectionCount, numAddends);
  const exportSectionSize = exportSectionCount.size;

  const codeSectionSize = gradientFunctionSizes
    .map((n) => wasm.intSize(n) + n)
    .reduce((a, b) => a + b, wasm.intSize(gradientFunctionSizes.length));

  const sumSectionSizes =
    numSections +
    wasm.intSize(typeSectionSize) +
    typeSectionSize +
    wasm.intSize(importSectionSize) +
    importSectionSize +
    wasm.intSize(functionSectionSize) +
    functionSectionSize +
    wasm.intSize(exportSectionSize) +
    exportSectionSize +
    wasm.intSize(codeSectionSize) +
    codeSectionSize;

  const mod = new wasm.Module(sumSectionSizes);

  mod.byte(wasm.SECTION.TYPE);
  mod.int(typeSectionSize);
  typeSection(mod);

  mod.byte(wasm.SECTION.IMPORT);
  mod.int(importSectionSize);
  importSection(mod);

  mod.byte(wasm.SECTION.FUNCTION);
  mod.int(functionSectionSize);
  functionSection(mod, numAddends);

  mod.byte(wasm.SECTION.EXPORT);
  mod.int(exportSectionSize);
  exportSection(mod, numAddends);

  mod.byte(wasm.SECTION.CODE);
  mod.int(codeSectionSize);
  mod.int(gradientFunctionSizes.length);

  return mod;
};

const compileUnary = (
  t: wasm.Target,
  { unop }: ad.Unary,
  param: number,
): void => {
  switch (unop) {
    case "squared": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.mul);

      return;
    }
    case "round": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.nearest);

      return;
    }
    case "neg":
    case "sqrt":
    case "abs":
    case "ceil":
    case "floor":
    case "trunc": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64[unop]);

      return;
    }
    case "acosh":
    case "acos":
    case "asin":
    case "asinh":
    case "atan":
    case "atanh":
    case "cbrt":
    case "cos":
    case "cosh":
    case "exp":
    case "expm1":
    case "log":
    case "log2":
    case "log10":
    case "log1p":
    case "sin":
    case "sinh":
    case "tan":
    case "tanh":
    case "inverse":
    case "sign": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex(unop));

      return;
    }
  }
};

const binaryOps = {
  "+": wasm.OP.f64.add,
  "-": wasm.OP.f64.sub,
  "*": wasm.OP.f64.mul,
  "/": wasm.OP.f64.div,
  max: wasm.OP.f64.max,
  min: wasm.OP.f64.min,

  ">": wasm.OP.f64.gt,
  "<": wasm.OP.f64.lt,
  "===": wasm.OP.f64.eq,
  ">=": wasm.OP.f64.ge,
  "<=": wasm.OP.f64.le,

  "&&": wasm.OP.i32.and,
  "||": wasm.OP.i32.or,
  "!==": wasm.OP.i32.xor,
};

const compileBinary = (
  t: wasm.Target,
  { binop }: ad.Binary | ad.Comp | ad.Logic,
  left: number,
  right: number,
): void => {
  switch (binop) {
    case "+":
    case "*":
    case "-":
    case "/":
    case "max":
    case "min":
    case ">":
    case "<":
    case "===":
    case ">=":
    case "<=":
    case "&&":
    case "||":
    case "!==": {
      t.byte(wasm.OP.local.get);
      t.int(left);

      t.byte(wasm.OP.local.get);
      t.int(right);

      t.byte(binaryOps[binop]);

      return;
    }
    case "atan2":
    case "pow": {
      t.byte(wasm.OP.local.get);
      t.int(left);

      t.byte(wasm.OP.local.get);
      t.int(right);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex(binop));

      return;
    }
  }
};

const nullaryVals = {
  addN: 0,
  maxN: -Infinity,
  minN: Infinity,
};

const naryOps = {
  addN: wasm.OP.f64.add,
  maxN: wasm.OP.f64.max,
  minN: wasm.OP.f64.min,
};

const compileNary = (
  t: wasm.Target,
  { op }: ad.Nary,
  params: number[],
): void => {
  if (params.length === 0) {
    // only spend bytes on an f64 constant when necessary
    t.byte(wasm.OP.f64.const);
    t.f64(nullaryVals[op]);
  } else {
    t.byte(wasm.OP.local.get);
    t.int(params[0]);

    for (const param of params.slice(1)) {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(naryOps[op]);
    }
  }
};

const compileNode = (
  t: wasm.Target,
  get: (child: ad.Expr) => number,
  node: Exclude<ad.Expr, ad.Var>,
): void => {
  if (typeof node === "number") {
    t.byte(wasm.OP.f64.const);
    t.f64(node);

    return;
  }
  switch (node.tag) {
    case "Not": {
      const child = get(node.param);

      t.byte(wasm.OP.local.get);
      t.int(child);

      t.byte(wasm.OP.i32.eqz);

      return;
    }
    case "Unary": {
      const param = get(node.param);
      compileUnary(t, node, param);
      return;
    }
    case "Binary":
    case "Comp":
    case "Logic": {
      const left = get(node.left);
      const right = get(node.right);
      compileBinary(t, node, left, right);
      return;
    }
    case "Ternary": {
      const cond = get(node.cond);
      const then = get(node.then);
      const els = get(node.els);

      t.byte(wasm.OP.local.get);
      t.int(then);

      t.byte(wasm.OP.local.get);
      t.int(els);

      t.byte(wasm.OP.local.get);
      t.int(cond);

      t.byte(wasm.OP.select);

      return;
    }
    case "Nary": {
      compileNary(t, node, node.params.map(get));
      return;
    }
    case "PolyRoots": {
      node.coeffs.forEach((index, i) => {
        t.byte(wasm.OP.local.get);
        t.int(getParamIndex(funcTypes.addend, "stackPointer"));

        t.byte(wasm.OP.local.get);
        t.int(get(index));

        t.byte(wasm.OP.f64.store);
        t.int(logAlignF64);
        t.int(i * bytesF64);
      });

      t.byte(wasm.OP.local.get);
      t.int(getParamIndex(funcTypes.addend, "stackPointer"));

      t.byte(wasm.OP.i32.const);
      t.int(node.coeffs.length);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex("polyRoots"));

      for (let i = 0; i < node.coeffs.length; i++) {
        t.byte(wasm.OP.local.get);
        t.int(getParamIndex(funcTypes.addend, "stackPointer"));

        t.byte(wasm.OP.f64.load);
        t.int(logAlignF64);
        t.int(i * bytesF64);
      }

      return;
    }
    case "Index": {
      const vec = get(node.vec);

      t.byte(wasm.OP.local.get);
      t.int(vec + node.index);

      return;
    }
  }
};

type Typename = "i32" | "f64";

const getLayout = (node: ad.Expr): { typename: Typename; count: number } => {
  if (typeof node === "number") return { typename: "f64", count: 1 };
  switch (node.tag) {
    case "Comp":
    case "Logic":
    case "Not": {
      return { typename: "i32", count: 1 };
    }
    case "Var":
    case "Unary":
    case "Binary":
    case "Ternary":
    case "Nary":
    case "Index": {
      return { typename: "f64", count: 1 };
    }
    case "PolyRoots": {
      return { typename: "f64", count: node.coeffs.length };
    }
  }
};

interface Local {
  typename: Typename;
  index: number;
}

interface Locals {
  counts: { i32: number; f64: number };
  indices: Map<ad.Expr, Local>;
}

const numAddendParams = Object.keys(funcTypes.addend.param).length;

const getIndex = (locals: Locals, node: ad.Expr): number => {
  const local = unwrap(locals.indices.get(node), () => "missing local");
  return (
    numAddendParams +
    (local.typename === "i32" ? 0 : locals.counts.i32) +
    local.index
  );
};

const compileGraph = (
  t: wasm.Target,
  inputs: Map<ad.Var, number>,
  grads: Map<ad.Var, ad.Num>,
  nodes: ad.Expr[],
): void => {
  const counts = { i32: 0, f64: 0 };
  const indices = new Map<ad.Expr, Local>();
  for (const node of nodes) {
    const { typename, count } = getLayout(node);
    indices.set(node, { typename, index: counts[typename] });
    counts[typename] += count;
  }
  const locals = { counts, indices };

  const numLocalDecls = Object.keys(counts).length;
  t.int(numLocalDecls);

  t.int(counts.i32);
  t.byte(wasm.TYPE.i32);

  t.int(counts.f64);
  t.byte(wasm.TYPE.f64);

  for (const [node, key] of inputs) {
    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "input"));

    t.byte(wasm.OP.f64.load);
    t.int(logAlignF64);
    t.int(key * bytesF64);

    t.byte(wasm.OP.local.set);
    t.int(getIndex(locals, node));
  }

  for (const node of nodes) {
    // we already generated code for the inputs
    if (typeof node === "number" || node.tag !== "Var") {
      compileNode(t, (child) => getIndex(locals, child), node);

      const index = getIndex(locals, node);
      for (let i = getLayout(node).count - 1; i >= 0; i--) {
        t.byte(wasm.OP.local.set);
        t.int(index + i);
      }
    }
  }

  for (const [x, grad] of grads) {
    const i = unwrap(inputs.get(x), () => "input not found");

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "gradient"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "gradient"));

    t.byte(wasm.OP.f64.load);
    t.int(logAlignF64);
    t.int(i * bytesF64);

    t.byte(wasm.OP.local.get);
    t.int(getIndex(locals, grad));

    t.byte(wasm.OP.f64.add);

    t.byte(wasm.OP.f64.store);
    t.int(logAlignF64);
    t.int(i * bytesF64);
  }

  secondary.forEach((id, i) => {
    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "secondary"));

    t.byte(wasm.OP.local.get);
    t.int(getIndex(locals, id));

    t.byte(wasm.OP.f64.store);
    t.int(logAlignF64);
    t.int(i * bytesF64);
  });

  t.byte(wasm.OP.local.get);
  t.int(getIndex(locals, primary));

  t.byte(wasm.END);
};

// assume the gradient and secondary outputs are already initialized to zero
// before this code is run
const compileSum = (t: wasm.Target, numAddends: number): void => {
  const numLocals = 0;
  t.int(numLocals);

  t.byte(wasm.OP.f64.const);
  t.f64(0);

  for (let i = 0; i < numAddends; i++) {
    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.sum, "mask"));

    t.byte(wasm.OP.i32.load);
    t.int(logAlignI32);
    t.int(i * bytesI32);

    t.byte(wasm.OP.if);
    t.int(getTypeIndex("unary"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.sum, "input"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.sum, "gradient"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.sum, "secondary"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.sum, "stackPointer"));

    t.byte(wasm.OP.call);
    t.int(builtins.size + i);

    t.byte(wasm.OP.f64.add);

    t.byte(wasm.END);
  }

  t.byte(wasm.END);
};

const genBytes = (graphs: ad.Graph[]): Uint8Array => {
  const secondaryKeys = new Map<number, number>();
  for (const { secondary } of graphs) {
    // `forEach` ignores holes
    secondary.forEach((id, i) => {
      secondaryKeys.set(i, (secondaryKeys.get(i) ?? 0) + 1);
    });
  }
  for (const [k, n] of secondaryKeys) {
    if (n > 1) throw Error(`secondary output ${k} is present in ${n} graphs`);
  }

  const sizes = graphs.map((g) => {
    const count = new wasm.Count();
    compileGraph(count, g);
    return count.size;
  });
  const mainCount = new wasm.Count();
  compileSum(mainCount, graphs.length);

  const mod = modulePrefix([...sizes, mainCount.size]);
  for (const [g, size] of zip2(graphs, sizes)) {
    mod.int(size);
    compileGraph(mod, g);
  }
  mod.int(mainCount.size);
  compileSum(mod, graphs.length);

  if (mod.count.size !== mod.bytes.length)
    throw Error(
      `allocated ${mod.bytes.length} bytes but used ${mod.count.size}`,
    );
  return mod.bytes;
};

interface Metadata {
  numInputs: number;
  numSecondary: number;

  offsetInputs: number;
  offsetMask: number;
  offsetGradient: number;
  offsetSecondary: number;
  offsetStack: number;

  memory: WebAssembly.Memory;

  arrInputs: Float64Array;
  arrMask: Int32Array;
  arrGrad: Float64Array;
  arrSecondary: Float64Array;
}

const makeMeta = (graphs: ad.Graph[]): Metadata => {
  const offsetInputs = 0;
  const numInputs = Math.max(
    0,
    ...graphs.flatMap(({ graph }) =>
      getInputNodes(graph).map(({ label: { key } }) => key + 1),
    ),
  );

  const offsetMask = offsetInputs + numInputs * bytesF64;

  const offsetGradient = offsetMask + Math.ceil(graphs.length / 2) * bytesF64;

  const offsetSecondary = offsetGradient + numInputs * bytesF64;
  const numSecondary = Math.max(0, ...graphs.map((g) => g.secondary.length));

  const offsetStack = offsetSecondary + numSecondary * bytesF64;

  // each WebAssembly memory page is 64 KiB, and we add one more for the stack
  const memory = new WebAssembly.Memory({
    initial: Math.ceil(offsetStack / (64 * 1024)) + 1,
  });
  const { buffer } = memory;

  return {
    numInputs,
    numSecondary,

    offsetInputs,
    offsetMask,
    offsetGradient,
    offsetSecondary,
    offsetStack,

    memory,

    arrInputs: new Float64Array(buffer, offsetInputs, numInputs),
    arrMask: new Int32Array(buffer, offsetMask, graphs.length),
    arrGrad: new Float64Array(buffer, offsetGradient, numInputs),
    arrSecondary: new Float64Array(buffer, offsetSecondary, numSecondary),
  };
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

const makeImports = (memory: WebAssembly.Memory): WebAssembly.Imports => ({
  [importModule]: {
    [importMemoryName]: memory,
    ...Object.fromEntries(
      [...builtins.keys()].map((name, i) => [
        i.toString(36),
        {
          inverse: (x: number): number => 1 / x,

          acos: Math.acos,
          acosh: Math.acosh,
          asin: Math.asin,
          asinh: Math.asinh,
          atan: Math.atan,
          atanh: Math.atanh,
          cbrt: Math.cbrt,
          cos: Math.cos,
          cosh: Math.cosh,
          exp: Math.exp,
          expm1: Math.expm1,
          log: Math.log,
          log1p: Math.log1p,
          log10: Math.log10,
          log2: Math.log2,
          sign: Math.sign,
          sin: Math.sin,
          sinh: Math.sinh,
          tan: Math.tan,
          tanh: Math.tanh,

          atan2: Math.atan2,
          pow: Math.pow,

          polyRoots: (p: number, n: number): void => {
            polyRootsImpl(new Float64Array(memory.buffer, p, n));
          },
        }[name],
      ]),
    ),
  },
});

const getExport = (
  meta: Metadata,
  instance: WebAssembly.Instance,
): (() => number) => {
  // we generated a WebAssembly function which exports a function that takes in
  // integers representing pointers to the various arrays it deals with
  const f = instance.exports[exportFunctionName] as (
    input: number,
    mask: number,
    gradient: number,
    secondary: number,
    stackPointer: number,
  ) => number;
  return () =>
    f(
      meta.offsetInputs,
      meta.offsetMask,
      meta.offsetGradient,
      meta.offsetSecondary,
      meta.offsetStack,
    );
};

const makeCompiled = (
  graphs: ad.Graph[],
  meta: Metadata,
  instance: WebAssembly.Instance,
): ad.Compiled => {
  const indices = new Map<ad.Var, number>();
  for (const { graph, nodes } of graphs) {
    for (const [x, id] of nodes) {
      if (typeof x !== "number" && x.tag === "Var") {
        const prev = indices.get(x);
        const key = getInputKey(graph, id);
        if (prev !== undefined && prev !== key)
          throw Error(`input with multiple keys: ${prev} and ${key}`);
        indices.set(x, key);
      }
    }
  }

  const f = getExport(meta, instance);
  // we wrap our Wasm function in a JavaScript function which instead thinks in
  // terms of arrays, using the `meta` data to translate between the two
  return (
    inputs: (x: ad.Var) => number,
    mask?: boolean[],
  ): ad.Outputs<number> => {
    for (const [x, i] of indices) meta.arrInputs[i] = inputs(x);
    for (let i = 0; i < graphs.length; i++)
      meta.arrMask[i] = mask !== undefined && i in mask && !mask[i] ? 0 : 1;
    meta.arrGrad.fill(0);
    meta.arrSecondary.fill(0);
    const primary = f();
    const gradient = new Map<ad.Var, number>();
    for (const [x, i] of indices) gradient.set(x, meta.arrGrad[i]);
    return {
      gradient,
      primary,
      secondary: Array.from(meta.arrSecondary),
    };
  };
};

/** Generate an energy function from the current state (using `ad.Num`s only) */
export const genGradient = async (
  inputs: ad.Var[],
  objectives: ad.Num[],
  constraints: ad.Num[],
): Promise<ad.Gradient> => {
  const n = inputs.length;

  // This changes with the EP round, gets bigger to weight the constraints.
  // Therefore it's marked as an input to the generated objective function,
  // which can be partially applied with the ep weight. But its initial `val`
  // gets compiled away, so we just set it to zero here.
  const lambda = variable(0);

  const indices = new Map(inputs.map((x, i) => [x, i]));
  indices.set(lambda, n);
  const getKey = (x: ad.Var): number =>
    unwrap(indices.get(x), () => "missing input");

  const objs = objectives.map((x, i) => {
    const secondary = [];
    secondary[i] = x;
    return makeGraph({ primary: x, secondary }, getKey);
  });
  const constrs = constraints.map((x, i) => {
    const secondary = [];
    secondary[objectives.length + i] = x;
    return makeGraph(
      { primary: mul(lambda, fns.toPenalty(x)), secondary },
      getKey,
    );
  });

  const graphs = [...objs, ...constrs];
  const meta = makeMeta(graphs);
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(genBytes(graphs)),
    makeImports(meta.memory),
  );
  const f = getExport(meta, instance);

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
    if (objMask.length !== objectives.length)
      throw Error(
        `expected ${objectives.length} objectives, got objective mask with length ${objMask.length}`,
      );
    if (constrMask.length !== constraints.length)
      throw Error(
        `expected ${constraints.length} constraints, got constraint mask with length ${constrMask.length}`,
      );
    if (inputs.length !== n)
      throw Error(`expected ${n} inputs, got ${inputs.length}`);
    if (grad.length !== n)
      throw Error(
        `expected ${n} inputs, got gradient with length ${grad.length}`,
      );

    // the computation graph might not use all the inputs, so we truncate the
    // inputs we're given, to avoid a `RangeError`
    meta.arrInputs.set(inputs.subarray(0, meta.numInputs));
    meta.arrInputs[n] = weight;
    for (let j = 0; j < objectives.length; j++)
      meta.arrMask[j] = objMask[j] ? 1 : 0;
    for (let k = 0; k < constraints.length; k++)
      meta.arrMask[objectives.length + k] = constrMask[k] ? 1 : 0;
    meta.arrGrad.fill(0);
    meta.arrSecondary.fill(0);
    const phi = f();
    for (let i = 0; i < n; i++)
      grad[i] = i < meta.numInputs && !inputMask[i] ? 0 : meta.arrGrad[i];
    return {
      phi,
      objectives: Array.from(meta.arrSecondary.subarray(0, objectives.length)),
      constraints: Array.from(meta.arrSecondary.subarray(objectives.length)),
    };
  };
};

const isConverged = (params: Params): boolean =>
  params.optStatus === "EPConverged";

export const problem = async ({
  objective,
  constraints,
}: ad.Description): Promise<ad.Problem> => {
  // `vars` keep track of all the inputs across all constraints and objective, and the weight
  const vars = new Map<ad.Var, number>();
  // add in the weight
  const lambda = variable(0);
  // make the comp graphs for obj and constrs
  const getKey = (x: ad.Var): number => {
    if (x === lambda) return 0;
    let i = vars.get(x);
    if (i === undefined) {
      i = vars.size + 1;
      vars.set(x, i);
    }
    return i;
  };
  const obj = primaryGraph(objective ?? 0, getKey);
  const constrs = (constraints ?? []).map((x) =>
    primaryGraph(mul(lambda, fns.toPenalty(x)), getKey),
  );
  const graphs = [obj, ...constrs];
  const meta = makeMeta(graphs);
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(genBytes(graphs)),
    makeImports(meta.memory),
  );
  const f = getExport(meta, instance);
  const n = vars.size;

  return {
    start: (conf) => {
      const vals = conf.vals ?? ((x: ad.Var) => x.val);
      const freeze = conf.freeze ?? (() => false);
      const mask: boolean[] = [];
      const init: number[] = [];
      // populate inputs with initial values from `vals`
      for (const [x, i] of vars) {
        mask[i - 1] = !freeze(x);
        init[i - 1] = vals(x); // skip the weight input
      }
      const wrap = (xs: number[], params: Params): ad.Run => {
        const unfrozen = new Map<ad.Var, number>();
        // give back the optimized values
        for (const [x, i] of vars) if (!freeze(x)) unfrozen.set(x, xs[i - 1]);
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
                  inputs: Float64Array /*read-only*/,
                  weight: number,
                  grad: Float64Array /*write-only*/,
                ): number => {
                  if (inputs.length !== n)
                    throw Error(`expected ${n} inputs, got ${inputs.length}`);
                  if (grad.length !== n)
                    throw Error(
                      `expected ${n} inputs, got gradient with length ${grad.length}`,
                    );
                  meta.arrInputs.set(inputs.subarray(0, n), 1);
                  // the first input is the weight
                  meta.arrInputs[0] = weight;
                  // we don't use addend masks, so they are set to 1
                  meta.arrMask.fill(1);
                  meta.arrGrad.fill(0);
                  meta.arrSecondary.fill(0);
                  const phi = f();
                  for (let i = 0; i < n; i++)
                    grad[i] =
                      i < meta.numInputs && !mask[i] ? 0 : meta.arrGrad[i + 1];
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

export const compile = async (
  xs: ad.Num[],
): Promise<(inputs: (x: ad.Var) => number) => number[]> => {
  const indices = new Map<ad.Var, number>();
  const graph = secondaryGraph(xs, (x: ad.Var): number => {
    let i = indices.get(x);
    if (i === undefined) {
      i = indices.size;
      indices.set(x, i);
    }
    return i;
  });
  const graphs = [graph];
  const meta = makeMeta(graphs);
  meta.arrMask[0] = 1; // only one graph, always run it
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(genBytes(graphs)),
    makeImports(meta.memory),
  );
  const f = getExport(meta, instance);
  return (inputs: (x: ad.Var) => number): number[] => {
    for (const [x, i] of indices) meta.arrInputs[i] = inputs(x);
    f();
    return Array.from(meta.arrSecondary);
  };
};

export const compileSync = (
  xs: ad.Num[],
): ((inputs: (x: ad.Var) => number) => number[]) => {
  const indices = new Map<ad.Var, number>();
  const graph = secondaryGraph(xs, (x: ad.Var): number => {
    let i = indices.get(x);
    if (i === undefined) {
      i = indices.size;
      indices.set(x, i);
    }
    return i;
  });
  const graphs = [graph];
  const meta = makeMeta(graphs);
  meta.arrMask[0] = 1; // only one graph, always run it
  const instance = new WebAssembly.Instance(
    new WebAssembly.Module(genBytes(graphs)),
    makeImports(meta.memory),
  );
  const f = getExport(meta, instance);
  return (inputs: (x: ad.Var) => number): number[] => {
    for (const [x, i] of indices) meta.arrInputs[i] = inputs(x);
    f();
    return Array.from(meta.arrSecondary);
  };
};
