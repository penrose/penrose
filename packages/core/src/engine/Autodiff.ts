import * as tf from "@tensorflow/tfjs";
import consola from "consola";
import _ from "lodash";
import { EigenvalueDecomposition, Matrix } from "ml-matrix";
import * as ad from "../types/ad.js";
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

interface Node {
  indegree?: number;
  successors: ad.Expr[];
}

interface Topsort {
  sorted: ad.Expr[];
  nodes: Map<ad.Expr, Node>;
}

const topsort = (seed: (set: (x: ad.Expr) => void) => void): Topsort => {
  const nodes = new Map<ad.Expr, Node>();
  const sorted: ad.Expr[] = [];

  const stack: ad.Expr[] = [];
  const set = (x: ad.Expr): Node => {
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
  const make = (x: ad.Expr): number => {
    const succ = (y: ad.Expr): void => {
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

const emitGraph = (
  { sorted, nodes }: Topsort,
  inputs: Map<ad.Var, number>,
): { vars: Map<ad.Expr, number>; code: string[] } => {
  const vars = new Map<ad.Expr, number>();
  const emitUnary = (y: ad.Unary): string => {
    const x = vars.get(y.param)!;
    switch (y.unop) {
      case "neg":
        return `x${x}.neg()`;
      case "squared":
        return `x${x}.square()`;
      case "sqrt":
        return `x${x}.sqrt()`;
      case "inverse":
        return `x${x}.reciprocal()`;
      case "abs":
        return `x${x}.abs()`;
      case "acosh":
        return `x${x}.acosh()`;
      case "acos":
        return `x${x}.acos()`;
      case "asin":
        return `x${x}.asin()`;
      case "asinh":
        return `x${x}.asinh()`;
      case "atan":
        return `x${x}.atan()`;
      case "atanh":
        return `x${x}.atanh()`;
      case "cbrt":
        return `x${x}.pow(1 / 3)`;
      case "ceil":
        return `x${x}.ceil()`;
      case "cos":
        return `x${x}.cos()`;
      case "cosh":
        return `x${x}.cosh()`;
      case "exp":
        return `x${x}.exp()`;
      case "expm1":
        return `x${x}.expm1()`;
      case "floor":
        return `x${x}.floor()`;
      case "log":
        return `x${x}.log()`;
      case "log2":
        return `x${x}.log().div(Math.log(2))`;
      case "log10":
        return `x${x}.log().div(Math.log(10))`;
      case "log1p":
        return `x${x}.log1p()`;
      case "round":
        throw `x${x}.round()`;
      case "sign":
        return `x${x}.sign()`;
      case "sin":
        return `x${x}.sin()`;
      case "sinh":
        return `x${x}.sinh()`;
      case "tan":
        return `x${x}.tan()`;
      case "tanh":
        return `x${x}.tanh()`;
      case "trunc":
        throw Error("trunc not supported");
    }
  };
  const emitBinary = (z: ad.Binary): string => {
    const x = vars.get(z.left)!;
    const y = vars.get(z.right)!;
    switch (z.binop) {
      case "+":
        return `x${x}.add(x${y})`;
      case "*":
        return `x${x}.mul(x${y})`;
      case "-":
        return `x${x}.sub(x${y})`;
      case "/":
        return `x${x}.div(x${y})`;
      case "max":
        return `x${x}.maximum(x${y})`;
      case "min":
        return `x${x}.minimum(x${y})`;
      case "atan2":
        return `x${x}.atan2(x${y})`;
      case "pow":
        return `x${x}.pow(x${y})`;
    }
  };
  const emitComp = (z: ad.Comp): string => {
    const x = vars.get(z.left)!;
    const y = vars.get(z.right)!;
    switch (z.binop) {
      case ">":
        return `x${x}.greater(x${y})`;
      case "<":
        return `x${x}.less(x${y})`;
      case "===":
        return `x${x}.equal(x${y})`;
      case ">=":
        return `x${x}.greaterEqual(x${y})`;
      case "<=":
        return `x${x}.lessEqual(x${y})`;
    }
  };
  const emitLogic = (r: ad.Logic): string => {
    const p = vars.get(r.left)!;
    const q = vars.get(r.right)!;
    switch (r.binop) {
      case "&&":
        return `x${p}.logicalAnd(x${q})`;
      case "||":
        return `x${p}.logicalOr(x${q})`;
      case "!==":
        return `x${p}.logicalXor(x${q})`;
    }
  };
  const emitNary = (y: ad.Nary): string => {
    const xs = y.params.map((x) => `x${vars.get(x)!}`).join(", ");
    switch (y.op) {
      case "addN":
        return xs.length === 0 ? "tf.scalar(0)" : `tf.addN([${xs}])`;
      case "maxN":
        return `tf.max(tf.stack([${xs}]))`;
      case "minN":
        return `tf.min(tf.stack([${xs}]))`;
    }
  };
  const emit = (x: ad.Expr): string => {
    if (typeof x === "number") return `tf.scalar(${x})`;
    switch (x.tag) {
      case "Var":
        return `inputs[${inputs.get(x)!}]`;
      case "Not":
        return `x${vars.get(x.param)!}.logicalNot()`;
      case "Unary":
        return emitUnary(x);
      case "Binary":
        return emitBinary(x);
      case "Comp":
        return emitComp(x);
      case "Logic":
        return emitLogic(x);
      case "Ternary":
        return `tf.where(x${vars.get(x.cond)!}, x${vars.get(
          x.then,
        )!}, x${vars.get(x.els)!})`;
      case "Nary":
        return emitNary(x);
      case "PolyRoots":
        throw Error("polynomial roots not supported");
      case "Index":
        return `x${vars.get(x.vec)!}[${x.index}]`;
    }
  };
  const code: string[] = [];
  for (let i = 0; i < sorted.length; i++) {
    const x = sorted[i];
    code.push(`const x${i} = ${emit(x)};`);
    vars.set(x, i);
    for (const y of nodes.get(x)!.successors) {
      const node = nodes.get(y)!;
      const n = node.indegree!;
      if (n === 1) sorted.push(y);
      node.indegree = n - 1;
    }
  }
  return { vars, code };
};

/** Generate an energy function from the current state (using `Num`s only) */
export const genGradient = async (
  inputs: ad.Var[],
  objectives: ad.Num[],
  constraints: ad.Num[],
): Promise<ad.Gradient> => {
  const n = inputs.length;
  const o = objectives.length;
  const c = constraints.length;

  const inputsMap = new Map<ad.Var, number>(inputs.map((x, i) => [x, i]));

  const single = (x: ad.Num) => {
    const graph = topsort((set) => {
      set(x);
    });
    const { code, vars } = emitGraph(graph, inputsMap);
    const ret = vars.get(x)!;
    const f = new Function(
      "tf",
      "inputs",
      `${code.join("\n")}\nreturn x${ret};`,
    );
    return (xs: tf.Scalar[]): tf.Scalar => f(tf, xs);
  };

  const objFns = objectives.map(single);
  const constrFns = constraints.map(single);

  const full = (
    varying: tf.Scalar[],
    weight: number,
    objMask: boolean[],
    constrMask: boolean[],
  ): {
    phi: tf.Scalar;
    objectives: tf.Scalar[];
    constraints: tf.Scalar[];
  } => {
    const objectives = objFns.map((f) => f(varying));
    const constraints = constrFns.map((f) => f(varying));
    return {
      phi: (o === 0
        ? tf.scalar(0)
        : tf.addN(objectives.map((x, i) => (objMask[i] ? x : 0)))
      ).add(
        (c === 0
          ? tf.scalar(0)
          : tf.addN(
              constraints.map((x, i) =>
                constrMask[i] ? x.relu().square() : 0,
              ),
            )
        ).mul(weight),
      ),
      objectives,
      constraints,
    };
  };

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

    let phi: number = 0;
    let objectives: number[] = [];
    let constraints: number[] = [];
    tf.tidy(() => {
      const wrapped = tf.grads((...varying: tf.Tensor[]) => {
        const out = full(varying as tf.Scalar[], weight, objMask, constrMask);
        phi = out.phi.arraySync();
        objectives = objMask.map((p, i) =>
          p ? out.objectives[i].arraySync() : 0,
        );
        constraints = constrMask.map((p, i) =>
          p ? out.constraints[i].arraySync() : 0,
        );
        return out.phi.add(n === 0 ? 0 : tf.addN(varying).mul(0));
      });
      const gradient = wrapped(
        Array.from(inputs).map((x) => tf.scalar(x)),
      ) as tf.Scalar[];
      for (let i = 0; i < n; i++)
        grad[i] = inputMask[i] ? gradient[i].arraySync() : 0;
    });
    return { phi, objectives, constraints };
  };
};

const isConverged = (params: Params): boolean =>
  params.optStatus === "EPConverged";

export const problem = async (desc: ad.Description): Promise<ad.Problem> => {
  const obj = desc.objective ?? 0;
  const constrs = desc.constraints ?? [];
  const graph = topsort((set) => {
    set(obj);
    constrs.forEach(set);
  });
  const inputs: ad.Var[] = [];
  const inputsMap = new Map<ad.Var, number>();
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") {
      inputsMap.set(x, inputs.length);
      inputs.push(x);
    }
  }
  const m = constrs.length;
  const n = inputs.length;

  const { code, vars } = emitGraph(graph, inputsMap);
  const basic = new Function(
    "tf",
    "inputs",
    `${code.join("\n")}\nreturn { objective: x${vars.get(
      obj,
    )!}, constraints: [${constrs
      .map((x) => `x${vars.get(x)!}`)
      .join(", ")}] };`,
  );

  const full = (varying: tf.Scalar[], weight: number): tf.Scalar => {
    const out = basic(tf, varying);
    return out.objective.add(
      m === 0
        ? 0
        : tf
            .addN(out.constraints.map((x: tf.Scalar) => x.relu().square()))
            .mul(weight),
    );
  };

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
                  let phi: number = 0;
                  tf.tidy(() => {
                    const wrapped = tf.grads((...varying: tf.Tensor[]) => {
                      const out = full(varying as tf.Scalar[], weight);
                      phi = out.arraySync();
                      return out;
                    });
                    const gradient = wrapped(
                      Array.from(v).map((x) => tf.scalar(x)),
                    ) as tf.Scalar[];
                    for (let i = 0; i < n; i++)
                      grad[i] = mask[i] ? gradient[i].arraySync() : 0;
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

const makeFn = (
  xs: ad.Num[],
): { inputs: ad.Var[]; f: (nums: number[]) => number[] } => {
  const graph = topsort((set) => {
    xs.forEach(set);
  });
  const inputs: ad.Var[] = [];
  const inputsMap = new Map<ad.Var, number>();
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") {
      inputsMap.set(x, inputs.length);
      inputs.push(x);
    }
  }
  const { code, vars } = emitGraph(graph, inputsMap);
  const f = new Function(
    "tf",
    "inputs",
    `${code.join("\n")}\nreturn [${xs
      .map((x) => `x${vars.get(x)!}`)
      .join(", ")}];`,
  );
  return {
    inputs,
    f: (nums) =>
      f(
        tf,
        nums.map((x) => tf.scalar(x)),
      ).map((x: tf.Scalar) => x.arraySync()),
  };
};

export const interp = (
  xs: ad.Num[],
): ((inputs: (x: ad.Var) => number) => number[]) => {
  const { inputs, f } = makeFn(xs);
  return (vals) => f(inputs.map((x) => vals(x)));
};

export const compile = async (
  xs: ad.Num[],
): Promise<(inputs: (x: ad.Var) => number) => number[]> => {
  const { inputs, f } = makeFn(xs);
  return (vals) => f(inputs.map((x) => vals(x)));
};
