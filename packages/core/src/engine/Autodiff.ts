import { Params, start, stepUntil } from "@penrose/optimizer";
import consola from "consola";
import _ from "lodash";
import * as rose from "rose";
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
import * as builtins from "./Builtins.js";

// To view logs, use LogLevel.Trace, otherwese LogLevel.Warn
// const log = consola.create({ level: LogLevel.Trace }).withScope("Optimizer");
export const logAD = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Optimizer");

export const EPS_DENOM = builtins.epsilon; // Avoid divide-by-zero in denominator

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
  vars: Map<ad.Expr, rose.Bool | rose.Real>,
): void => {
  const emitUnary = (y: ad.Unary): rose.Real => {
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
  const emitBinary = (z: ad.Binary): rose.Real => {
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
  const emitComp = (z: ad.Comp): rose.Bool => {
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
  const emitLogic = (r: ad.Logic): rose.Bool => {
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
  const emitNary = (y: ad.Nary): rose.Real => {
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
  const emit = (x: ad.Expr): rose.Bool | rose.Real => {
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
      case "Index":
        throw Error("polynomial roots not supported");
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

/** Generate an energy function from the current state (using `ad.Num`s only) */
export const genGradient = async (
  inputs: ad.Var[],
  objectives: ad.Num[],
  constraints: ad.Num[],
): Promise<ad.Gradient> => {
  const n = inputs.length;
  const o = objectives.length;
  const c = constraints.length;

  const single = (x: ad.Num) =>
    rose.fn([rose.Vec(n, rose.Real)], rose.Real, (varying) => {
      const graph = topsort((set) => {
        set(x);
      });
      const vars = new Map<ad.Expr, rose.Bool | rose.Real>();
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
      const Pair = { x: rose.Real, d: rose.Real } as const;
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

  const f = rose.interp(full);

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
  const graph = topsort((set) => {
    set(obj);
    constrs.forEach(set);
  });
  const inputs: ad.Var[] = [];
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") inputs.push(x);
  }
  const m = constrs.length;
  const n = inputs.length;

  const basic = rose.fn(
    [{ inputs: rose.Vec(n, rose.Real), weight: rose.Real }],
    rose.Real,
    ({ inputs: varying, weight }) => {
      const vars = new Map<ad.Expr, rose.Bool | rose.Real>();
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

  const f = rose.interp(full);

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

export const compileSync = (
  xs: ad.Num[],
): ((inputs: (x: ad.Var) => number) => number[]) => {
  const graph = topsort((set) => {
    xs.forEach(set);
  });
  const inputs: ad.Var[] = [];
  for (const x of graph.sorted) {
    if (typeof x !== "number" && x.tag === "Var") inputs.push(x);
  }
  const m = xs.length;
  const n = inputs.length;
  const f = rose.fn([rose.Vec(n, rose.Real)], rose.Vec(m, rose.Real), (v) => {
    const vars = new Map<ad.Expr, rose.Bool | rose.Real>();
    for (let i = 0; i < n; i++) vars.set(inputs[i], v[i]);
    emitGraph(graph, vars);
    return xs.map((x) => vars.get(x) as rose.Real);
  });
  const g = rose.interp(f);
  return (vals) => g(inputs.map((x) => vals(x))) as any;
};

export const compile = async (
  xs: ad.Num[],
): Promise<(inputs: (x: ad.Var) => number) => number[]> => compileSync(xs);
