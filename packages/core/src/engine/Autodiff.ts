import { Queue } from "@datastructures-js/queue";
import consola, { LogLevel } from "consola";
import * as _ from "lodash";
import { GradGraphs, IVarAD, VarAD } from "types/ad";
import { WeightInfo } from "types/state";
import { safe } from "utils/Util";
import {
  acos,
  add,
  addN,
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
} from "./AutodiffFunctions";

// To view logs, use LogLevel.Trace, otherwese LogLevel.Warn
// const log = consola.create({ level: LogLevel.Trace }).withScope("Optimizer");
export const logAD = consola
  .create({ level: LogLevel.Warn })
  .withScope("Optimizer");

export const EPS_DENOM = 10e-6; // Avoid divide-by-zero in denominator

// Reverse-mode AD
// Implementation adapted from https://rufflewind.com/2016-12-30/reverse-mode-automatic-differentiation and https://github.com/Rufflewind/revad/blob/eb3978b3ccdfa8189f3ff59d1ecee71f51c33fd7/revad.py

// NOTE: VARIABLES ARE MUTATED DURING AD CALCULATION

// ----- Core AD code

/**
 * Make a number into a `VarAD`.
 */
export const varOf = (x: number, vname = "", metadata = ""): VarAD => {
  if (typeof x !== "number") {
    console.error("x", x);
    throw Error("expected number");
  }

  return variableAD(x, vname, metadata);
};

/**
 * Return a new `VarAD` that's a constant.
 */
export const constOf = (x: number): VarAD => {
  if (typeof x !== "number") {
    console.error("x", x);
    throw Error("expected number");
  }

  return variableAD(x, String(x), "const");
};

export const constOfIf = (x: number | VarAD): VarAD => {
  if (typeof x === "number") {
    return variableAD(x, String(x), "const");
  }

  return x;
};

/**
 * Return the numerical value held in a `VarAD`.
 */
export const numOf = (x: VarAD): number => x.val;

/**
 * Make a number into a `VarAD`.
 */
export const differentiable = (e: number): VarAD => {
  // log.trace("making it differentiable", e);
  return varOf(e);
};

/**
 * Make a number into a `VarAD`. Don't use this!
 */
export const variableAD = (
  x: number,
  vname = "",
  metadata = "",
  isCompNode = true
): VarAD => {
  const opName = vname ? vname : String(x);

  return {
    metadata,
    op: opName,
    isInput: false,
    val: x,
    isCompNode,
    parentsAD: [],
    childrenAD: [],
    parentsADGrad: [],
    childrenADGrad: [],
    gradVal: undefined,
    gradNode: undefined,
    index: -100,
    id: -1,

    debug: false,
    debugInfo: "",

    nodeVisited: false,
    name: "",
  };
};

/**
 * Make a number into a gradient `VarAD`. Don't use this!
 */
export const gvarOf = (x: number, vname = "", metadata = ""): VarAD => {
  if (typeof x !== "number") {
    console.error("x", x);
    throw Error("expected number");
  }

  // Grad var, level 1
  return variableAD(x, vname, metadata, false);
};

/**
 * Return a variable with no gradient.
 */
export const noGrad: VarAD = gvarOf(0.0, "noGrad");

export const markInput = (v: VarAD, i: number): IVarAD => {
  v.isInput = true;
  v.index = i;
  return v;
};

// Copies the input numbers and returns a new list of vars marked as inputs
export const makeADInputVars = (xs: number[]): VarAD[] => {
  const xsCopy = [...xs];
  const xsVars = xsCopy.map((x, i) => markInput(variableAD(x), i));
  // Need to mark these so we know what's special when generating the function code
  return xsVars;
};

// This should only be applied to a leaf node
// It moves forward from the leaf in question, then recursively arrives at the seed node (one output parent), caching each gradient value for an intermediate note
// (However, it doesn't calculate gradient for any other leaf nodes, though they do use the cached intermediate values)

//            (ds/ds = 1)
//                s
//               ^ ^
//              /   \
//               ...
//        z1 = ...  z2 = ...
//              ^   ^
//  dz1/dv = ... \ / dz2/dv = ...
//               (v) = ...
//
//  ds/dv = ds/sz1 * dz1/dv + ds/dz2 * dz2/dv + ...
// The recursive parts are ds/dzi (the parents further up)

// grad(v) means ds/dv (s is the single output seed)
// do not use outside this file
export const _gradADSymbolic = (v: VarAD): VarAD => {
  // Already computed/cached the gradient
  if (v.gradNode !== undefined) {
    return v.gradNode;
  }

  // Build subgraph
  let res;
  if (v.parentsAD.length === 0) {
    // node has no parents, so setting grad to 0 (it doesn't influence the output)
    res = gvarOf(0, "0", "no gradient");
  } else {
    // normal reverse-mode AD chain rule
    // The result is built via pointers to subgraphs that are already built in child nodes of the original comp graph
    res = addN(
      v.parentsAD.map((parent) =>
        mul(
          safe(parent.sensitivityNode, "expected VarAD but got undefined"),
          _gradADSymbolic(parent.node),
          false
        )
      ),
      false
    );
  }

  // Mark node as done
  v.gradNode = res;

  // Result is a gradient
  res.isCompNode = false;

  // Note that it does not return v
  return res;
};

export const _gradAllSymbolic = (
  energyGraph: VarAD,
  xsVars: VarAD[]
): VarAD[] => {
  energyGraph.gradNode = variableAD(1.0);
  const dxs = xsVars.map(_gradADSymbolic); // Computes it per variable, mutating the graph to set cached results and reuse them
  const gradxs = xsVars.map((x: VarAD) =>
    safe(x.gradNode, "expected VarAD but got undefined")
  );
  return gradxs;
};

// ----- Ops (extensible)
// NOTE: These all update the graph and return new variables that should be used to build the ops
// NOTE: For the resulting var `z`, z's parents and grad are uninitialized
// TODO: Factor out op helper to deal with graph-building boilerplate

// --- Binary ops

//                (+) (z := v + w)     -- parent
//               ^   ^
// dz/dv = 1    /     \    dz/dw = 1   -- sensitivities
//             v       w               -- children

// TODO: Put these in ops dict
// NOTE: The names of these ops matter for opMap, don't change them

// The point of making the sensitivity nodes here is that when the gradient is computed, each child needs to know what its partial derivative was, which depends on its position (e.g. either the first or second arg in x * y has a different sensitivity). This can't be looked up in, say, a dict
// You have to build it inline bc it involves references to the variables

const check = (
  isCompNode: boolean,
  sensitivityNode: VarAD
): VarAD | undefined => {
  return isCompNode ? sensitivityNode : undefined;
};

// ------------ Meta / debug ops

/**
 * Mutates a node `v` to store log info. Dumps node value (during evaluation) to the console. You must use the node that `debug` returns, otherwise the debug information will not appear.
 * For more documentation on how to use this function, see the Penrose wiki page.
 */
export const debug = (v: VarAD, debugInfo = "no additional info"): VarAD => {
  v.debug = true;
  v.debugInfo = debugInfo;
  return v;
};

// Useful constants

export const zero: VarAD = constOf(0);

// to prevent 1/0 (infinity). put it in the denominator
export const epsd: VarAD = constOf(10e-10);

// Useful grad constants
// TODO -- But it seems to be bad to use them...

export const zeroG: VarAD = gvarOf(0.0);

export const oneG: VarAD = gvarOf(1.0);

export const negoneG: VarAD = gvarOf(1.0);

export const epsdg: VarAD = gvarOf(10e-10);

// ----------------- Other ops

/**
 * Some vector operations that can be used on `VarAD`.
 */
export const ops = {
  // Note that these ops MUST use the custom var ops for grads
  // Note that these ops are hardcoded to assume they are not applied to grad nodes

  /**
   * Return the norm of the 2-vector `[c1, c2]`.
   */
  norm: (c1: VarAD, c2: VarAD): VarAD => ops.vnorm([c1, c2]),

  /**
   * Return the Euclidean distance between scalars `c1, c2`.
   */
  dist: (c1: VarAD, c2: VarAD): VarAD => ops.vnorm([c1, c2]),

  /**
   * Return the sum of vectors `v1, v2.
   */
  vadd: (v1: VarAD[], v2: VarAD[]): VarAD[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, add);
    return res;
  },

  /**
   * Return the difference of vectors `v1, v2.
   */
  vsub: (v1: VarAD[], v2: VarAD[]): VarAD[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, sub);
    return res;
  },

  /**
   * Return the Euclidean norm squared of vector `v`.
   */
  vnormsq: (v: VarAD[]): VarAD => {
    const res = v.map((e) => squared(e));
    return _.reduce(res, (x, y) => add(x, y, true), variableAD(0.0)); // TODO: Will this one (var(0)) have its memory freed?
    // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
  },

  /**
   * Return the Euclidean norm of vector `v`.
   */
  vnorm: (v: VarAD[]): VarAD => {
    const res = ops.vnormsq(v);
    return sqrt(res);
  },

  /**
   * Return the vector `v` multiplied by scalar `c`.
   */
  vmul: (c: VarAD, v: VarAD[]): VarAD[] => {
    return v.map((e) => mul(c, e));
  },

  /**
   * Return the vector `v`, scaled by `-1`.
   */
  vneg: (v: VarAD[]): VarAD[] => {
    return ops.vmul(constOf(-1.0), v);
  },

  /**
   * Return the vector `v` divided by scalar `c`.
   */
  vdiv: (v: VarAD[], c: VarAD): VarAD[] => {
    return v.map((e) => div(e, c));
  },

  /**
   * Return the vector `v`, normalized.
   */
  vnormalize: (v: VarAD[]): VarAD[] => {
    const vsize = add(ops.vnorm(v), varOf(EPS_DENOM));
    return ops.vdiv(v, vsize);
  },

  /**
   * Return the Euclidean distance between vectors `v` and `w`.
   */
  vdist: (v: VarAD[], w: VarAD[]): VarAD => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }
    return ops.vnorm(ops.vsub(v, w));
  },

  /**
   * Return the Euclidean distance squared between vectors `v` and `w`.
   */
  vdistsq: (v: VarAD[], w: VarAD[]): VarAD => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }

    return ops.vnormsq(ops.vsub(v, w));
  },

  /**
   * Return the dot product of vectors `v1, v2`.
   * Note: if you want to compute a norm squared, use `vnormsq` instead, it generates a smaller computational graph
   */
  vdot: (v1: VarAD[], v2: VarAD[]): VarAD => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, mul);
    return _.reduce(res, (x, y) => add(x, y, true), variableAD(0.0));
  },

  /**
   * Return the unsigned angle between vectors `u, v`, in radians.
   * Assumes that both u and v have nonzero magnitude.
   * The returned value will be in the range [0,pi].
   */
  angleBetween: (u: VarAD[], v: VarAD[]): VarAD => {
    if (u.length !== v.length) {
      throw Error("expected vectors of same length");
    }

    // Due to floating point error, the dot product of
    // two normalized vectors may fall slightly outside
    // the range [-1,1].  To prevent acos from producing
    // a NaN value, we therefore scale down the result
    // of the dot product by a factor s slightly below 1.
    const s = 1 - 1e-10;

    return acos(mul(varOf(s), ops.vdot(ops.vnormalize(u), ops.vnormalize(v))));
  },

  /**
   * Return the signed angle from vector `u` to vector `v`, in radians.
   * Assumes that both u and v are 2D vectors and have nonzero magnitude.
   * The returned value will be in the range [-pi,pi].
   */
  angleFrom: (u: VarAD[], v: VarAD[]): VarAD => {
    if (u.length !== v.length) {
      throw Error("expected vectors of same length");
    }

    return atan2(
      ops.cross2(u, v), // y = |u||v|sin(theta)
      ops.vdot(u, v) // x = |u||v|cos(theta)
    );
  },

  /**
   * Return the sum of elements in vector `v`.
   */
  vsum: (v: VarAD[]): VarAD => {
    return _.reduce(v, (x, y) => add(x, y, true), variableAD(0.0));
  },

  /**
   * Return `v + c * u`.
   */
  vmove: (v: VarAD[], c: VarAD, u: VarAD[]): VarAD[] => {
    return ops.vadd(v, ops.vmul(c, u));
  },

  /**
   * Rotate a 2D point `[x, y]` by 90 degrees counterclockwise.
   */
  rot90: ([x, y]: VarAD[]): VarAD[] => {
    return [neg(y), x];
  },

  /**
   * Rotate a 2D point `[x, y]` by a degrees counterclockwise.
   */
  vrot: ([x, y]: VarAD[], a: VarAD): VarAD[] => {
    const angle = div(mul(a, varOf(Math.PI)), varOf(180));
    const x2 = sub(mul(cos(angle), x), mul(sin(angle), y));
    const y2 = add(mul(sin(angle), x), mul(cos(angle), y));
    return [x2, y2];
  },

  /**
   * Return 2D determinant/cross product of 2D vectors
   */
  cross2: (u: VarAD[], v: VarAD[]): VarAD => {
    if (u.length !== 2 || v.length !== 2) {
      throw Error("expected two 2-vectors");
    }
    return sub(mul(u[0], v[1]), mul(u[1], v[0]));
  },

  /**
   * Return 3D cross product of 3D vectors
   */
  cross3: (u: VarAD[], v: VarAD[]): VarAD[] => {
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
   * Return the angle between two 2D vectors `v` and `w` in radians.
   * From https://github.com/thi-ng/umbrella/blob/develop/packages/vectors/src/angle-between.ts#L11
   * NOTE: This function has not been thoroughly tested
   */
  angleBetween2: (v: VarAD[], w: VarAD[]): VarAD => {
    if (v.length !== 2 || w.length !== 2) {
      throw Error("expected two 2-vectors");
    }
    const t = atan2(ops.cross2(v, w), ops.vdot(v, w));
    return t;
  },
};

export const fns = {
  /**
   * Return the penalty `max(x, 0)`.
   */
  toPenalty: (x: VarAD): VarAD => {
    return squared(max(x, variableAD(0.0)));
  },

  /**
   * Return the center of a shape.
   */
  center: (props: any): VarAD[] => {
    return props.center.contents;
  },
};

// ----- Codegen

// Traverses the computational graph of ops obtained by interpreting the energy function, and generates code corresponding to just the ops (in plain js), which is then turned into an evaluable js function via the Function constructor

// Example of constructing an n-ary function by calling the Function constructor: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/Function

// const args = ["x0", "x1", "x2"];
// const inputs = [0, 1, 2];
// const f = new Function(...args, 'return x0 + x1 + x2');
// log.trace(f(...inputs));

// (Returns `3`)

// Wrapper since energy only has one output

export const _genEnergyFn = (
  xs: VarAD[],
  z: IVarAD,
  weight: VarAD | undefined
): any => _genCode(xs, [z], "energy", weight);

// Generate code for multi-output function, given its computational graph and a setting for its outputs
// NOTE: Generates a function that expects inputs to be passed in the same order as here, and the inputs should be sorted by their index
// NOTE: Modifies the input computational graph `outputs` to set and clear visited nodes
export const _genCode = (
  inputs: VarAD[],
  outputs: IVarAD[],
  setting: string,
  weightNode: VarAD | undefined
): any => {
  let counter = 0;
  let progInputs: string[] = [];
  let progStmts: string[] = [];
  let progOutputs: string[] = [];

  logAD.trace(
    "genCode inputs, outputs, weightNode, setting",
    inputs,
    outputs,
    weightNode,
    setting
  );

  let inputsNew;
  logAD.trace("has weight?", weightNode !== undefined);
  if (weightNode === undefined) {
    inputsNew = inputs;
  } else {
    inputsNew = [weightNode].concat(inputs);
  }

  // Just traverse + name the inputs first (they have no children, so the traversal stops there), then work backward from the outputs
  // The inputs are the EP weight + original xsVars (if it's energy) or just the xsVars (if it's gradient)
  for (const x of inputsNew) {
    const res = traverseGraph(counter, x, setting);

    if (res.inputs.length !== 1) {
      throw Error("expected one input from an input var traversal");
    }
    progInputs = progInputs.concat(res.inputs.map((e: any) => e.name));
    progStmts = progStmts.concat(res.prog);
    progOutputs = progOutputs.concat(res.output);

    // For any code generated for the next output, start on fresh index
    counter = res.counter + 1;
  }

  // For each output, traverse the graph and combine the results sequentially
  for (const z of outputs) {
    const res = traverseGraph(counter, z, setting);
    progStmts = progStmts.concat(res.prog);
    progOutputs = progOutputs.concat(res.output);

    // For any code generated for the next output, start on fresh index
    counter = res.counter + 1;

    // log.trace("output node traversed", z);
    // log.trace("res stmts", res.prog);
    // log.trace("res output", res.output);
  }

  let returnStmt = "";

  if (setting === "energy") {
    // Return single scalar
    if (!progOutputs || !progOutputs[0]) {
      throw Error("not enough energy outputs -- need exactly 1");
    }
    returnStmt = `return ${progOutputs[0]};`;
  } else if (setting === "grad") {
    // Return list of scalars
    const outputNamesStr = progOutputs.join(", ");
    returnStmt = `return [${outputNamesStr}];`;
  }

  const progStr = progStmts.concat([returnStmt]).join("\n");
  // log.trace("progInputs", "progStr", progInputs, progStr);

  const f = new Function(...progInputs, progStr);
  logAD.trace("generated f with setting =", setting, "\n", f);

  let g;
  if (weightNode === undefined) {
    // So you can call the function without spread
    // hasWeight is for "normal" functions that aren't wrapped in the EP cycle (such as the symbolic gradient unit tests)
    g = (xs: number[]) => f(...xs);
  } else {
    // Curry the function so it can be partially applied with the EP weight later, without regenerating the function
    g = (weight: number[]) => {
      return (xs: number[]) => {
        const xs2 = [weight].concat(xs);
        return f(...xs2);
      };
    };
  }
  logAD.trace("overall function generated (g):", g);
  clearVisitedNodes(inputsNew.concat(outputs));

  return g;
};

// NOTE: Mutates z to store that the node was visited, and what its name is
// `i` is the counter, the initial parameter for generating var names
// `i` starts with 0 for the first call, children name themselves with the passed-in index (so you know the child's name) and pass their counter back up. Parents do the work of incrementing
const traverseGraph = (i: number, z: IVarAD, setting: string): any => {
  const c = "x"; // Base character for var names
  const childType = z.isCompNode ? "childrenAD" : "childrenADGrad";

  // If this node was already visited, return its name (cached), and counter should not increment
  if (z.nodeVisited) {
    return {
      counter: i,
      prog: [],
      inputs: [],
      output: [],
      references: [z.name],
    };
  }

  // Parents do the work of incrementing
  if (z[childType].length === 0) {
    const leafName = c + String(i);

    // Mark node as visited, with its name as reference for its computed/cached value
    z.id = i;
    z.nodeVisited = true;
    z.name = leafName;

    // Distinguish between inputs and constants
    if (z.isInput) {
      // Just return self name for function binding
      return {
        counter: i,
        prog: [],
        inputs: [{ name: leafName, index: z.index }],
        output: [],
        references: [],
      };
    }

    const stmts = [];
    let stmt;
    // Otherwise bind const in body
    if (z.op === "noGrad") {
      stmt = `const ${leafName} = 0.0; // No grad`;
    } else {
      stmt = `const ${leafName} = ${z.op};`;
    }

    stmts.push(stmt);

    if (z.debug) {
      const stmt2 = `console.log("${z.debugInfo} (var ${leafName}) | value: ", ${leafName}, "during ${setting} evaluation");`;
      stmts.push(stmt2);
    }

    return {
      counter: i,
      prog: stmts,
      inputs: [],
      output: leafName,
      references: [],
    };
  } else if (z[childType].length === 1) {
    // Unary op
    // debugger;

    const child = z[childType][0].node;
    const res = traverseGraph(i, child, setting);

    let childName;
    let parCounter;
    if (res.references[0]) {
      // Just refer to child if the node was already visited
      // And don't increment counter, since we just looked up a reference, didn't make a new child node
      childName = res.references[0];
      parCounter = res.counter;
    } else {
      childName = c + String(res.counter);
      parCounter = res.counter + 1;
    }

    const parName = c + String(parCounter);

    // Mark node as visited with name as reference
    z.id = parCounter;
    z.nodeVisited = true;
    z.name = parName;

    const op = z.op;

    const stmts = [];
    let stmt;

    if (z.op === "squared") {
      stmt = `const ${parName} = ${childName} * ${childName};`;
    } else if (z.op === "sqrt") {
      stmt = `const ${parName} = Math.sqrt(${childName});`;
    } else if (z.op === "acosh") {
      stmt = `const ${parName} = Math.acosh(${childName});`;
    } else if (z.op === "acos") {
      stmt = `const ${parName} = Math.acos(${childName});`;
    } else if (z.op === "asin") {
      stmt = `const ${parName} = Math.asin(${childName});`;
    } else if (z.op === "asinh") {
      stmt = `const ${parName} = Math.asinh(${childName});`;
    } else if (z.op === "atan") {
      stmt = `const ${parName} = Math.atan(${childName});`;
    } else if (z.op === "atanh") {
      stmt = `const ${parName} = Math.atanh(${childName});`;
    } else if (z.op === "cbrt") {
      stmt = `const ${parName} = Math.cbrt(${childName});`;
    } else if (z.op === "ceil") {
      stmt = `const ${parName} = Math.ceil(${childName});`;
    } else if (z.op === "cos") {
      stmt = `const ${parName} = Math.cos(${childName});`;
    } else if (z.op === "cosh") {
      stmt = `const ${parName} = Math.cosh(${childName});`;
    } else if (z.op === "exp") {
      stmt = `const ${parName} = Math.exp(${childName});`;
    } else if (z.op === "expm1") {
      stmt = `const ${parName} = Math.expm1(${childName});`;
    } else if (z.op === "floor") {
      stmt = `const ${parName} = Math.floor(${childName});`;
    } else if (z.op === "log") {
      stmt = `const ${parName} = Math.log(${childName});`;
    } else if (z.op === "log2") {
      stmt = `const ${parName} = Math.log2(${childName});`;
    } else if (z.op === "log10") {
      stmt = `const ${parName} = Math.log10(${childName});`;
    } else if (z.op === "log1p") {
      stmt = `const ${parName} = Math.log1p(${childName});`;
    } else if (z.op === "round") {
      stmt = `const ${parName} = Math.round(${childName});`;
    } else if (z.op === "sign") {
      stmt = `const ${parName} = Math.sign(${childName});`;
    } else if (z.op === "sin") {
      stmt = `const ${parName} = Math.sin(${childName});`;
    } else if (z.op === "sinh") {
      stmt = `const ${parName} = Math.sinh(${childName});`;
    } else if (z.op === "tan") {
      stmt = `const ${parName} = Math.tan(${childName});`;
    } else if (z.op === "tanh") {
      stmt = `const ${parName} = Math.tanh(${childName});`;
    } else if (z.op === "trunc") {
      stmt = `const ${parName} = Math.trunc(${childName});`;
    } else if (z.op === "+ list") {
      // TODO: Get rid of unary +
      stmt = `const ${parName} = ${childName};`;
    } else if (z.op === "min list") {
      stmt = `const ${parName} = ${childName};`;
    } else if (z.op === "max list") {
      stmt = `const ${parName} = ${childName};`;
    } else if (z.op === "inverse") {
      stmt = `const ${parName} = 1.0 / (${childName} + ${EPS_DENOM});`;
    } else if (z.op === "- (unary)") {
      stmt = `const ${parName} = -${childName};`;
    } else if (z.op === "abs") {
      stmt = `const ${parName} = Math.abs(${childName});`;
    } else {
      stmt = `const ${parName} = (${op})(${childName});`;
    }

    stmts.push(stmt);

    if (z.debug) {
      const stmt2 = `console.log("${z.debugInfo} (var ${parName}) | value: ", ${parName}, "during ${setting} evaluation");`;
      stmts.push(stmt2);
    }

    return {
      counter: parCounter,
      prog: res.prog.concat(stmts),
      inputs: res.inputs,
      output: parName,
      references: [],
    };
  } else if (z[childType].length === 2) {
    // Binary op
    // TODO: refactor repeated code below into the for loop as in ternary
    const child0 = z[childType][0].node;
    const child1 = z[childType][1].node;

    const res0 = traverseGraph(i, child0, setting);
    let childName0;
    let nextCounter;
    if (res0.references[0]) {
      childName0 = res0.references[0];
      nextCounter = res0.counter;
    } else {
      childName0 = c + String(res0.counter);
      nextCounter = res0.counter + 1;
    }

    const res1 = traverseGraph(nextCounter, child1, setting);
    let childName1;
    let parCounter;
    if (res1.references[0]) {
      // Just refer to child if the node was already visited
      childName1 = res1.references[0];
      parCounter = res1.counter;
    } else {
      childName1 = c + String(res1.counter);
      parCounter = res1.counter + 1;
    }

    const parName = c + String(parCounter);

    // Mark node as visited with name as reference
    z.id = parCounter;
    z.nodeVisited = true;
    z.name = parName;

    const op = z.op;
    const stmts = [];
    let stmt;
    if (op === "max") {
      stmt = `const ${parName} = Math.max(${childName0}, ${childName1});`;
    } else if (op === "min") {
      stmt = `const ${parName} = Math.min(${childName0}, ${childName1});`;
    } else if (z.op === "gt") {
      stmt = `const ${parName} = ${childName0} > ${childName1};`;
    } else if (z.op === "lt") {
      stmt = `const ${parName} = ${childName0} < ${childName1};`;
    } else if (z.op === "pow") {
      stmt = `const ${parName} = Math.pow(${childName0},${childName1});`;
    } else if (z.op === "and") {
      stmt = `const ${parName} = ${childName0} && ${childName1};`;
    } else if (z.op === "or") {
      stmt = `const ${parName} = ${childName0} || ${childName1};`;
    } else if (z.op === "eq") {
      stmt = `const ${parName} = ${childName0} === ${childName1};`;
    } else if (z.op === "+ list") {
      stmt = `const ${parName} = ${childName0} + ${childName1};`;
    } else if (z.op === "div") {
      stmt = `const ${parName} = ${childName0} / (${childName1} + ${EPS_DENOM});`;
    } else if (z.op === "atan2") {
      stmt = `const ${parName} = Math.atan2(${childName0}, ${childName1});`;
    } else {
      stmt = `const ${parName} = ${childName0} ${op} ${childName1};`;
    }

    stmts.push(stmt);

    if (z.debug) {
      const stmt2 = `console.log("${z.debugInfo} (var ${parName}) | value: ", ${parName}, "during ${setting} evaluation");`;
      stmts.push(stmt2);
    }

    // Array efficiency?
    return {
      counter: parCounter,
      prog: res0.prog.concat(res1.prog).concat(stmts),
      inputs: res0.inputs.concat(res1.inputs),
      output: parName,
      references: [],
    };
  } else {
    // N-ary node
    const childNodes = z[childType].map((e) => e.node);

    const childNames = [];
    let prog: string[] = [];
    let inputs: string[] = [];
    let counter = i;

    // Evaluate each child and get its generated code, inputs, and name first
    for (const childNode of childNodes) {
      const res = traverseGraph(counter, childNode, setting);
      prog = prog.concat(res.prog);
      inputs = inputs.concat(res.inputs);

      // Child was already visited; don't generate code again, just a reference
      if (res.references[0]) {
        childNames.push(res.references[0]);
        counter = res.counter;
      } else {
        childNames.push(c + String(res.counter));
        counter = res.counter + 1;
      }
    }

    const parName = c + String(counter);

    // Mark node as visited with name as reference
    // TODO: factor out these 3 lines from all cases
    z.id = counter;
    z.nodeVisited = true;
    z.name = parName;

    const op = z.op;
    const stmts = [];
    let stmt;

    // Deals with ifCond nodes (ternary)
    // (eval c; eval d; eval e; const xNUM = c ? d : e;)
    // codegen doesn't short-circuit -- it generates code for both branches of the `if` first
    // TODO: Fix this so it doesn't *evaluate* both branches of the `if`, just the one that's relevant to the condition

    if (op === "ifCond") {
      if (childNames.length !== 3) {
        logAD.trace("args", childNames);
        throw Error("expected three args to if cond");
      }

      stmt = `const ${parName} = ${childNames[0]} ? ${childNames[1]} : ${childNames[2]};`;
    } else if (op === "+ list") {
      const childList = "[".concat(childNames.join(", ")).concat("]");
      stmt = `const ${parName} = ${childList}.reduce((x, y) => x + y);`;
    } else if (op === "min list") {
      const childList = "[".concat(childNames.join(", ")).concat("]");
      stmt = `const ${parName} = ${childList}.reduce((x, y) => Math.min(x, y));`;
    } else if (op === "max list") {
      const childList = "[".concat(childNames.join(", ")).concat("]");
      stmt = `const ${parName} = ${childList}.reduce((x, y) => Math.max(x, y));`;
    } else {
      logAD.trace("node", z, z.op);
      throw Error("unknown n-ary operation");
    }

    stmts.push(stmt);

    // TODO: Factor out this code, which is repeated 3 times
    if (z.debug) {
      const stmt2 = `console.log("${z.debugInfo} (var ${parName}) | value: ", ${parName}, "during ${setting} evaluation");`;
      stmts.push(stmt2);
    }

    return {
      counter,
      prog: prog.concat(stmts),
      inputs,
      output: parName,
      references: [],
    };
  }
};

// Use this function after synthesizing an energy function, if you want to
// synthesize the gradient as well, since they both rely on mutating the
// computational graph to mark the visited nodes and their generated names
export const clearVisitedNodes = (nodeList: VarAD[]): void => {
  const q = new Queue<IVarAD>();
  const discoveredNodes = new Set<IVarAD>();
  nodeList.forEach((z) => {
    discoveredNodes.add(z);
    z.nodeVisited = false;
    q.enqueue(z);
  });
  while (q.size() > 0) {
    const v = q.dequeue();
    v.childrenAD.forEach((e) => {
      if (!discoveredNodes.has(e.node)) {
        discoveredNodes.add(e.node);
        e.node.nodeVisited = false;
        q.enqueue(e.node);
      }
    });
    v.childrenADGrad.forEach((e) => {
      if (!discoveredNodes.has(e.node)) {
        discoveredNodes.add(e.node);
        e.node.nodeVisited = false;
        q.enqueue(e.node);
      }
    });
    v.parentsAD.forEach((e) => {
      if (!discoveredNodes.has(e.node)) {
        discoveredNodes.add(e.node);
        e.node.nodeVisited = false;
        q.enqueue(e.node);
      }
    });
    v.parentsADGrad.forEach((e) => {
      if (!discoveredNodes.has(e.node)) {
        discoveredNodes.add(e.node);
        e.node.nodeVisited = false;
        q.enqueue(e.node);
      }
    });
  }
};

// Mutates z (top node) to clear all vals and gradients of its children
// NOTE that this will zero all the nodes in the graph, including the leaves (such as the stepEP parameters)
export const clearGraphTopDown = (z: VarAD): void => {
  z.val = 0;
  z.gradVal = undefined;
  z.childrenAD.forEach((e) => clearGraphTopDown(e.node));
};

const clearGraphBottomUp = (xs: VarAD[]) => {
  xs.forEach((x) => {
    x.val = 0;
    x.gradVal = undefined;
    clearGraphBottomUp(x.parentsAD.map((p) => p.node));
  });
};

// Mutates xsVars (leaf nodes) to set their values to the inputs in xs (and name them accordingly by value)
// NOTE: the xsVars should already have been set as inputs via makeAdInputVars
// NOTE: implicitly, the orders of the values need to match the order of variables
const setInputs = (xsVars: VarAD[], xs: number[]) => {
  xsVars.forEach((v, i) => {
    const val = xs[i];
    v.val = val;
    v.op = String(val);
  });
};

const setWeights = (info: WeightInfo) => {
  info.constrWeightNode.val = info.constrWeight;
  info.constrWeightNode.op = String(info.constrWeight);

  info.epWeightNode.val = info.epWeight;
  info.epWeightNode.op = String(info.epWeight);
};

// Given an energyGraph of f, clears the graph and returns the compiled energy and gradient of f as functions
// xsVars are the leaves, energyGraph is the topmost parent of the computational graph
export const energyAndGradCompiled = (
  xs: number[],
  xsVars: VarAD[],
  energyGraph: VarAD,
  weightInfo: WeightInfo | undefined,
  debug = false
) => {
  // Zero xsvars vals, gradients, and caching setting
  clearGraphBottomUp(xsVars);
  clearVisitedNodes([energyGraph]);

  // Set the weight nodes to have the right weight values (may have been updated at some point during the opt)
  if (weightInfo !== undefined) {
    setWeights(weightInfo);
  }

  // Set the leaves of the graph to have the new input values
  setInputs(xsVars, xs);

  // Build symbolic gradient of f at xs on the energy graph
  // Note that this does NOT include the weight (i.e. is called on `xsVars`, not `xsVarsWithWeight`! Because the EP weight is not a degree of freedom)
  const gradGraph = _gradAllSymbolic(energyGraph, xsVars);

  const epWeightNode: VarAD | undefined = weightInfo?.epWeightNode; // Generate energy and gradient without weight

  const graphs: GradGraphs = {
    inputs: xsVars,
    energyOutput: energyGraph,
    gradOutputs: gradGraph,
    weight: epWeightNode,
  };

  // Synthesize energy and gradient code
  const f0 = _genEnergyFn(graphs.inputs, graphs.energyOutput, graphs.weight);
  const gradGen = _genCode(
    graphs.inputs,
    graphs.gradOutputs,
    "grad",
    graphs.weight
  );

  // Return the energy and grad on the input, as well as updated energy graph
  return {
    graphs,
    f: f0,
    gradf: gradGen,
  };
};
