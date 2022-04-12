import * as _ from "lodash";
import { VarAD } from "types/ad";
import { EPS_DENOM, gvarOf, logAD, noGrad, variableAD } from "./Autodiff";

/**
 * Return `v + w`.
 */
export const add = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val + w.val, "+");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: gvarOf(1.0) });
    w.parentsAD.push({ node: z, sensitivityNode: gvarOf(1.0) });

    z.childrenAD.push({ node: v, sensitivityNode: gvarOf(1.0) });
    z.childrenAD.push({ node: w, sensitivityNode: gvarOf(1.0) });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return the sum of elements in `xs`.
 */
export const addN = (xs: VarAD[], isCompNode = true): VarAD => {
  // N-way add
  // TODO: Do argument list length checking for other ops generically
  if (xs.length === 1) {
    return xs[0];
  } else if (xs.length === 2) {
    return add(xs[0], xs[1], isCompNode);
  } else {
    const z = variableAD(_.sum(_.map(xs, (x) => x.val)), "+ list");
    z.isCompNode = isCompNode;

    if (isCompNode) {
      for (const x of xs) {
        x.parentsAD.push({ node: z, sensitivityNode: gvarOf(1.0) });
        z.childrenAD.push({ node: x, sensitivityNode: gvarOf(1.0) });
      }
    } else {
      for (const x of xs) {
        x.parentsADGrad.push({ node: z, sensitivityNode: undefined });
        z.childrenADGrad.push({ node: x, sensitivityNode: undefined });
      }
    }

    return z;
  }
};

/**
 * Return `v * w`.
 */
export const mul = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val * w.val, "*");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: w });
    w.parentsAD.push({ node: z, sensitivityNode: v });

    z.childrenAD.push({ node: v, sensitivityNode: w });
    z.childrenAD.push({ node: w, sensitivityNode: v });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `v - w`.
 */
export const sub = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val - w.val, "-");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: gvarOf(1.0) });
    w.parentsAD.push({ node: z, sensitivityNode: gvarOf(-1.0) });

    z.childrenAD.push({ node: v, sensitivityNode: gvarOf(1.0) });
    z.childrenAD.push({ node: w, sensitivityNode: gvarOf(-1.0) });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `v / w`.
 */
export const div = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  if (Math.abs(w.val) < 10e-10) {
    throw Error("divide by zero");
  }

  const z = variableAD(v.val / w.val, "/");
  z.isCompNode = isCompNode;

  // grad(v/w) = [1/w, -v/w^2]
  if (isCompNode) {
    const vnode = div(gvarOf(1.0), w, false);
    const w0node = squared(w, false);
    // const w1node = max(epsdg, w0node, false); // TODO: Why does this make it get stuck? w1node is not even used
    const wnode = neg(div(v, w0node, false), false);

    v.parentsAD.push({ node: z, sensitivityNode: vnode });
    w.parentsAD.push({ node: z, sensitivityNode: wnode });

    z.childrenAD.push({ node: v, sensitivityNode: vnode });
    z.childrenAD.push({ node: w, sensitivityNode: wnode });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `max(v, w)`.
 */
export const max = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(Math.max(v.val, w.val), "max");
  z.isCompNode = isCompNode;

  // const vFn = (arg: "unit"): number => v.val > w.val ? 1.0 : 0.0;
  // const wFn = (arg: "unit"): number => v.val > w.val ? 0.0 : 1.0;

  const cond = gt(v, w, false);

  const vNode = ifCond(cond, gvarOf(1.0), gvarOf(0.0), false);
  const wNode = ifCond(cond, gvarOf(0.0), gvarOf(1.0), false);
  // NOTE: this adds a conditional to the computational graph itself, so the sensitivities change based on the input values
  // Note also the closure attached to each sensitivityFn, which has references to v and w (which have references to their values)

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: vNode });
    w.parentsAD.push({ node: z, sensitivityNode: wNode });

    z.childrenAD.push({ node: v, sensitivityNode: vNode });
    z.childrenAD.push({ node: w, sensitivityNode: wNode });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `min(v, w)`.
 */
export const min = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(Math.min(v.val, w.val), "min");
  z.isCompNode = isCompNode;

  // const vFn = (arg: "unit"): number =< v.val < w.val ? 1.0 : 0.0;
  // const wFn = (arg: "unit"): number =< v.val < w.val ? 0.0 : 1.0;

  const cond = lt(v, w, false);

  const vNode = ifCond(cond, gvarOf(1.0), gvarOf(0.0), false);
  const wNode = ifCond(cond, gvarOf(0.0), gvarOf(1.0), false);
  // NOTE: this adds a conditional to the computational graph itself, so the sensitivities change based on the input values
  // Note also the closure attached to each sensitivityFn, which has references to v and w (which have references to their values)

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: vNode });
    w.parentsAD.push({ node: z, sensitivityNode: wNode });

    z.childrenAD.push({ node: v, sensitivityNode: vNode });
    z.childrenAD.push({ node: w, sensitivityNode: wNode });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `maxN(xs)`.
 */
export const maxN = (xs: VarAD[], isCompNode = true): VarAD => {
  if (xs.length === 0) {
    logAD.trace("node", xs);
    throw Error("argument list to maxN is empty; expected 1+ elements");
  } else if (xs.length === 1) {
    return xs[0];
  } else if (xs.length === 2) {
    return max(xs[0], xs[1], isCompNode);
  } else {
    const z = variableAD(Math.max(...xs.map((x) => x.val)), "max list");
    z.isCompNode = isCompNode;

    if (isCompNode) {
      for (const x of xs) {
        const xNode = ifCond(lt(x, z, false), gvarOf(0.0), gvarOf(1.0), false);
        x.parentsAD.push({ node: z, sensitivityNode: xNode });
        z.childrenAD.push({ node: x, sensitivityNode: xNode });
      }
    } else {
      for (const x of xs) {
        x.parentsADGrad.push({ node: z, sensitivityNode: undefined });
        z.childrenADGrad.push({ node: x, sensitivityNode: undefined });
      }
    }
    return z;
  }
};

/**
 * Return `minN(xs)`.
 */
export const minN = (xs: VarAD[], isCompNode = true): VarAD => {
  if (xs.length === 0) {
    logAD.trace("node", xs);
    throw Error("argument list to minN is empty; expected 1+ elements");
  } else if (xs.length === 1) {
    return xs[0];
  } else if (xs.length === 2) {
    return min(xs[0], xs[1], isCompNode);
  } else {
    const z = variableAD(Math.min(...xs.map((x) => x.val)), "min list");
    z.isCompNode = isCompNode;

    if (isCompNode) {
      for (const x of xs) {
        const xNode = ifCond(gt(x, z, false), gvarOf(0.0), gvarOf(1.0), false);
        x.parentsAD.push({ node: z, sensitivityNode: xNode });
        z.childrenAD.push({ node: x, sensitivityNode: xNode });
      }
    } else {
      for (const x of xs) {
        x.parentsADGrad.push({ node: z, sensitivityNode: undefined });
        z.childrenADGrad.push({ node: x, sensitivityNode: undefined });
      }
    }
    return z;
  }
};

/**
 * Returns the two-argument arctangent `atan2(y, x)`, which
 * describes the angle made by a vector (x,y) with the x-axis.
 * Returns a value in radians, in the range [-pi,pi].
 */
export const atan2 = (y: VarAD, x: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(Math.atan2(y.val, x.val), "atan2");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    // construct the derivatives
    // d/dx atan2(y,x) = -y/(x^2 + y^2)
    // d/dy atan2(y,x) =  x/(x^2 + y^2)
    const denom = add(squared(x, false), squared(y, false), false);
    const xnode = div(neg(y, false), denom, false);
    const ynode = div(x, denom, false);

    y.parentsAD.push({ node: z, sensitivityNode: ynode });
    x.parentsAD.push({ node: z, sensitivityNode: xnode });

    z.childrenAD.push({ node: y, sensitivityNode: ynode });
    z.childrenAD.push({ node: x, sensitivityNode: xnode });
  } else {
    y.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    x.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: y, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Returns `pow(x,y)`.
 */
export const pow = (x: VarAD, y: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(Math.pow(x.val, y.val), "pow");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    const xnode = mul(pow(x, sub(y, gvarOf(1.0), false), false), y, false);
    const ynode = mul(pow(x, y, false), ln(x, false), false);

    y.parentsAD.push({ node: z, sensitivityNode: ynode });
    x.parentsAD.push({ node: z, sensitivityNode: xnode });

    z.childrenAD.push({ node: y, sensitivityNode: ynode });
    z.childrenAD.push({ node: x, sensitivityNode: xnode });
  } else {
    y.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    x.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: y, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return z;
};

// --- Unary ops

/**
 * Return `-v`.
 */
export const neg = (v: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(-v.val, "- (unary)");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    v.parentsAD.push({ node: z, sensitivityNode: gvarOf(-1.0) });

    z.childrenAD.push({ node: v, sensitivityNode: gvarOf(-1.0) });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `v * v`.
 */
export const squared = (v: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val * v.val, "squared");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    const node = mul(gvarOf(2.0), v, false);
    v.parentsAD.push({ node: z, sensitivityNode: node });

    z.childrenAD.push({ node: v, sensitivityNode: node });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `sqrt(v)`.
 */
export const sqrt = (v: VarAD, isCompNode = true): VarAD => {
  // NOTE: Watch out for negative numbers in sqrt
  // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
  const z = variableAD(Math.sqrt(v.val), "sqrt");
  z.isCompNode = isCompNode;

  // XXX Looks like this is for debugging?
  // const dzDv = (arg: "unit"): number => {
  //   if (v.val < 0) {
  //     logAD.trace(`negative arg ${v.val} in sqrt`);
  //   }
  //   return 1.0 / (2.0 * Math.sqrt(Math.max(0, v.val) + EPS_DENOM));
  // };

  // TODO: How to do the checks in this graph? I guess sqrt should have a special evaluation/gradient rule?

  // It's important to only construct gradNode if this is a compnode, otherwise it will make recursive calls to the function ops and blow the stack
  if (isCompNode) {
    const gradNode = div(
      gvarOf(1.0),
      mul(gvarOf(2.0), max(gvarOf(EPS_DENOM), sqrt(v, false), false), false),
      false
    );
    v.parentsAD.push({ node: z, sensitivityNode: gradNode });
    z.childrenAD.push({ node: v, sensitivityNode: gradNode });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `1 / v`.
 */
export const inverse = (v: VarAD, isCompNode = true): VarAD => {
  // TODO: Avoid numerical instability
  const z = variableAD(1 / (v.val + EPS_DENOM), "inverse");
  z.isCompNode = isCompNode;

  // -1/(x^2 + epsilon) -- This takes care of the divide-by-zero gradient problem
  if (isCompNode) {
    const node = neg(
      inverse(add(squared(v, false), gvarOf(EPS_DENOM), false), false),
      false
    );
    v.parentsAD.push({ node: z, sensitivityNode: node });

    z.childrenAD.push({ node: v, sensitivityNode: node });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `|v|`.
 */
export const absVal = (v: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(Math.abs(v.val), "abs");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    // x / (|x| + epsilon)
    const node = div(v, add(absVal(v, false), gvarOf(EPS_DENOM), false), false);
    v.parentsAD.push({ node: z, sensitivityNode: node });

    z.childrenAD.push({ node: v, sensitivityNode: node });
  } else {
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });

    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return `acosh(x)`.
 */
export const acosh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.acosh(x.val), "acosh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      mul(
        sqrt(sub(x, gvarOf(1.0), false), false),
        sqrt(add(x, gvarOf(1.0), false), false),
        false
      ),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `acos(x)`.
 */
export const acos = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.acos(x.val), "acos");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = neg(
      div(
        gvarOf(1.0),
        sqrt(sub(gvarOf(1.0), mul(x, x, false), false), false),
        false
      ),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `asin(x)`.
 */
export const asin = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.asin(x.val), "asin");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      sqrt(sub(gvarOf(1.0), mul(x, x, false), false), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `asinh(x)`.
 */
export const asinh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.asinh(x.val), "asinh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      sqrt(add(gvarOf(1.0), mul(x, x, false), false), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `atan(x)`.
 */
export const atan = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.atan(x.val), "atan");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      add(gvarOf(1.0), mul(x, x, false), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `atanh(x)`.
 */
export const atanh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.atanh(x.val), "atanh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      sub(gvarOf(1.0), mul(x, x, false), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `cbrt(x)`.
 */
export const cbrt = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cbrt(x.val), "cbrt");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      mul(gvarOf(3.0), squared(cbrt(x, false), false), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `ceil(x)`.
 */
export const ceil = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.ceil(x.val), "ceil");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = gvarOf(0.0);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `cos(x)`.
 */
export const cos = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cos(x.val), "cos");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = neg(sin(x, false), false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `cosh(x)`.
 */
export const cosh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.cosh(x.val), "cosh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = sinh(x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `exp(x)`.
 */
export const exp = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.exp(x.val), "exp");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = exp(x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `expm1(x)`.
 */
export const expm1 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.expm1(x.val), "expm1");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = exp(x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `floor(x)`.
 */
export const floor = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.floor(x.val), "floor");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = gvarOf(0.0);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return the natural logarithm `ln(v)` (i.e., log base e).
 */
export const ln = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log(x.val), "log");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(gvarOf(1.0), x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `log2(x)`.
 */
export const log2 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log2(x.val), "log2");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      mul(x, gvarOf(0.6931471805599453), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `log10(x)`.
 */
export const log10 = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log10(x.val), "log10");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(
      gvarOf(1.0),
      mul(x, gvarOf(2.302585092994046), false),
      false
    );
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `log1p(x)`.
 */
export const log1p = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.log1p(x.val), "log1p");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = div(gvarOf(1.0), add(gvarOf(1.0), x, false), false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `round(x)`.
 */
export const round = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.round(x.val), "round");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = gvarOf(0.0);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `sign(x)`.
 */
export const sign = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.sign(x.val), "sign");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = gvarOf(0.0);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `sin(x)`.
 */
export const sin = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.sin(x.val), "sin");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = cos(x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `sinh(x)`.
 */
export const sinh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.sinh(x.val), "sinh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = cosh(x, false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `tan(x)`.
 */
export const tan = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.tan(x.val), "tan");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = squared(div(gvarOf(1.0), cos(x, false), false), false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `tanh(x)`.
 */
export const tanh = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.tanh(x.val), "tanh");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = squared(div(gvarOf(1.0), cosh(x, false), false), false);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

/**
 * Return `trunc(x)`.
 */
export const trunc = (x: VarAD, isCompNode = true): VarAD => {
  const y = variableAD(Math.trunc(x.val), "trunc");
  y.isCompNode = isCompNode;

  if (isCompNode) {
    const node = gvarOf(0.0);
    x.parentsAD.push({ node: y, sensitivityNode: node });
    y.childrenAD.push({ node: x, sensitivityNode: node });
  } else {
    x.parentsADGrad.push({ node: y, sensitivityNode: undefined });
    y.childrenADGrad.push({ node: x, sensitivityNode: undefined });
  }

  return y;
};

// ------- Discontinuous / noGrad ops

/**
 * Return a conditional `v > w`.
 */
export const gt = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  // returns a boolean, which is converted to number

  const z = variableAD(v.val > w.val ? 1.0 : 0.0, "gt");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    z.childrenAD.push({ node: v, sensitivityNode: noGrad });
    z.childrenAD.push({ node: w, sensitivityNode: noGrad });

    v.parentsAD.push({ node: z, sensitivityNode: noGrad });
    w.parentsAD.push({ node: z, sensitivityNode: noGrad });
  } else {
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return a conditional `v < w`.
 */
export const lt = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  // returns a boolean, which is converted to number
  const z = variableAD(v.val < w.val ? 1.0 : 0.0, "lt");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    z.childrenAD.push({ node: v, sensitivityNode: noGrad });
    z.childrenAD.push({ node: w, sensitivityNode: noGrad });

    v.parentsAD.push({ node: z, sensitivityNode: noGrad });
    w.parentsAD.push({ node: z, sensitivityNode: noGrad });
  } else {
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return a conditional `v == w`. (TODO: Maybe check if they are equal up to a tolerance?)
 * Note, the 1.0, 0.0 stuff is irrelevant, in the codegen they are boolean
 */
export const eq = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  // returns a boolean, which is converted to number
  const z = variableAD(v.val === w.val ? 1.0 : 0.0, "eq");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    z.childrenAD.push({ node: v, sensitivityNode: noGrad });
    z.childrenAD.push({ node: w, sensitivityNode: noGrad });

    v.parentsAD.push({ node: z, sensitivityNode: noGrad });
    w.parentsAD.push({ node: z, sensitivityNode: noGrad });
  } else {
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return a boolean (number) `v && w`
 */
export const and = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val * w.val, "and");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    z.childrenAD.push({ node: v, sensitivityNode: noGrad });
    z.childrenAD.push({ node: w, sensitivityNode: noGrad });

    v.parentsAD.push({ node: z, sensitivityNode: noGrad });
    w.parentsAD.push({ node: z, sensitivityNode: noGrad });
  } else {
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return a boolean (number) `v || w`
 */
export const or = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
  const z = variableAD(v.val + w.val, "or");
  z.isCompNode = isCompNode;

  if (isCompNode) {
    z.childrenAD.push({ node: v, sensitivityNode: noGrad });
    z.childrenAD.push({ node: w, sensitivityNode: noGrad });

    v.parentsAD.push({ node: z, sensitivityNode: noGrad });
    w.parentsAD.push({ node: z, sensitivityNode: noGrad });
  } else {
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};

/**
 * Return a conditional `if(cond) then v else w`.
 */
export const ifCond = (
  cond: VarAD,
  v: VarAD,
  w: VarAD,
  isCompNode = true
): VarAD => {
  // When the computation graph is evaluated, either v or w is evaluated and returned, depending on the boolean value cond in the generated gradient

  const z = variableAD(cond.val ? v.val : w.val, "ifCond"); // No value?
  z.isCompNode = isCompNode;

  if (isCompNode) {
    const vNode = ifCond(cond, gvarOf(1.0), gvarOf(0.0), false);
    const wNode = ifCond(cond, gvarOf(0.0), gvarOf(1.0), false);

    z.childrenAD.push({ node: cond, sensitivityNode: noGrad });
    z.childrenAD.push({ node: v, sensitivityNode: vNode });
    z.childrenAD.push({ node: w, sensitivityNode: wNode });

    cond.parentsAD.push({ node: z, sensitivityNode: noGrad });
    v.parentsAD.push({ node: z, sensitivityNode: vNode });
    w.parentsAD.push({ node: z, sensitivityNode: wNode });
  } else {
    z.childrenADGrad.push({ node: cond, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: v, sensitivityNode: undefined });
    z.childrenADGrad.push({ node: w, sensitivityNode: undefined });

    cond.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    v.parentsADGrad.push({ node: z, sensitivityNode: undefined });
    w.parentsADGrad.push({ node: z, sensitivityNode: undefined });
  }

  return z;
};
