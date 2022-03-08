import { Queue } from "@datastructures-js/queue";
import consola, { LogLevel } from "consola";
import * as graphlib from "graphlib";
import * as _ from "lodash";
import * as ad from "types/ad";
import { VarAD } from "types/ad";
import { safe } from "utils/Util";
import {
  acos,
  add,
  atan2,
  cos,
  cosh,
  div,
  exp,
  gt,
  ifCond,
  inverse,
  ln,
  lt,
  max,
  mul,
  neg,
  pow,
  sin,
  sinh,
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

export const input = (name: string): ad.Input => ({ tag: "Input", name });

// every VarAD is already an ad.Node, but this function removes all the children
const makeNode = (x: VarAD): ad.Node => {
  if (typeof x === "number") {
    return x;
  }
  const node: ad.Node = x; // get some typechecking by not using x after this
  const { tag } = node;
  switch (tag) {
    case "Input": {
      const { name } = node;
      return { tag, name };
    }
    case "Unary": {
      const { unop } = node;
      return { tag, unop };
    }
    case "Binary": {
      const { binop } = node;
      return { tag, binop };
    }
    case "Ternary": {
      return { tag };
    }
    case "Nary": {
      const { op } = node;
      return { tag, op };
    }
    case "Debug": {
      const { info } = node;
      return { tag, info };
    }
  }
};

const unarySensitivity = (z: ad.Unary): VarAD => {
  const { unop, param: v } = z;
  switch (unop) {
    case "neg": {
      return -1;
    }
    case "squared": {
      return mul(2, v);
    }
    case "sqrt": {
      // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
      return div(1, mul(2, max(EPS_DENOM, z)));
    }
    case "inverse": {
      // This takes care of the divide-by-zero gradient problem
      return neg(inverse(add(squared(v), EPS_DENOM)));
    }
    case "abs": {
      return div(v, add(z, EPS_DENOM));
    }
    case "acosh": {
      return div(1, mul(sqrt(sub(v, 1)), sqrt(add(v, 1))));
    }
    case "acos": {
      return neg(div(1, sqrt(sub(1, mul(v, v)))));
    }
    case "asin": {
      return div(1, sqrt(sub(1, mul(v, v))));
    }
    case "asinh": {
      return div(1, sqrt(add(1, mul(v, v))));
    }
    case "atan": {
      return div(1, add(1, mul(v, v)));
    }
    case "atanh": {
      return div(1, sub(1, mul(v, v)));
    }
    case "cbrt": {
      return div(1, mul(3, squared(z)));
    }
    case "ceil":
    case "floor":
    case "round":
    case "sign":
    case "trunc": {
      return 0;
    }
    case "cos": {
      return neg(sin(v));
    }
    case "cosh": {
      return sinh(v);
    }
    case "exp":
    case "expm1": {
      return exp(v);
    }
    case "log": {
      return div(1, v);
    }
    case "log2": {
      return div(1, mul(v, 1 / Math.LOG2E));
    }
    case "log10": {
      return div(1, mul(v, 1 / Math.LOG10E));
    }
    case "log1p": {
      return div(1, add(1, v));
    }
    case "sin": {
      return cos(v);
    }
    case "sinh": {
      return cosh(v);
    }
    case "tan": {
      return squared(div(1, cos(v)));
    }
    case "tanh": {
      return squared(div(1, cosh(v)));
    }
  }
};

const binarySensitivities = (
  z: ad.Binary
): { left: VarAD | undefined; right: VarAD | undefined } => {
  const { binop, left: v, right: w } = z;
  switch (binop) {
    case "+": {
      return { left: 1, right: 1 };
    }
    case "*": {
      return { left: w, right: v };
    }
    case "-": {
      return { left: 1, right: -1 };
    }
    case "/": {
      return { left: div(1, w), right: neg(div(v, squared(w))) };
    }
    case "max": {
      const cond = gt(v, w);
      return { left: ifCond(cond, 1, 0), right: ifCond(cond, 0, 1) };
    }
    case "min": {
      const cond = lt(v, w);
      return { left: ifCond(cond, 1, 0), right: ifCond(cond, 0, 1) };
    }
    case "atan2": {
      const y = v;
      const x = w;
      const denom = add(squared(x), squared(y));
      return { left: div(x, denom), right: div(neg(y), denom) };
    }
    case "pow": {
      return { left: mul(pow(v, sub(w, 1)), w), right: mul(z, ln(v)) };
    }
    case ">":
    case "<":
    case "===":
    case "&&":
    case "||": {
      return { left: undefined, right: undefined };
    }
  }
};

const indexToNaryEdge = (index: number): ad.NaryEdge => `${index}`;
const naryEdgeToIndex = (name: ad.NaryEdge) => parseInt(name, 10);

interface Child {
  child: VarAD;
  name: ad.Edge;
  sensitivity: VarAD | undefined;
}

const children = (x: VarAD): Child[] => {
  if (typeof x === "number") {
    return [];
  }
  switch (x.tag) {
    case "Input": {
      return [];
    }
    case "Unary": {
      return [
        { child: x.param, name: undefined, sensitivity: unarySensitivity(x) },
      ];
    }
    case "Binary": {
      const { left, right } = binarySensitivities(x);
      return [
        { child: x.left, name: "left", sensitivity: left },
        { child: x.right, name: "right", sensitivity: right },
      ];
    }
    case "Ternary": {
      return [
        { child: x.cond, name: "cond", sensitivity: undefined },
        { child: x.then, name: "then", sensitivity: ifCond(x.cond, 1, 0) },
        { child: x.els, name: "els", sensitivity: ifCond(x.cond, 0, 1) },
      ];
    }
    case "Nary": {
      return x.params.map((child, i) => {
        const c = { child, name: indexToNaryEdge(i) };
        switch (x.op) {
          case "addN": {
            return { ...c, sensitivity: 1 };
          }
          case "maxN": {
            return { ...c, sensitivity: ifCond(lt(child, x), 0, 1) };
          }
          case "minN": {
            return { ...c, sensitivity: ifCond(gt(child, x), 0, 1) };
          }
        }
      });
    }
    case "Debug": {
      return [{ child: x.node, name: undefined, sensitivity: 1 }];
    }
  }
};

// make a graph node ID that could also be used as a JavaScript variable name
const indexToID = (index: number) => `_${index}`;

export const makeGraph = (outputs: VarAD[]): ad.Graph => {
  const graph = new graphlib.Graph({ multigraph: true });
  const nodes = new Map<VarAD, string>();
  const edges: [Child, VarAD][] = [];

  // Queue constructor doesn't clone its argument, so we must
  const queue = new Queue([...outputs]);
  while (!queue.isEmpty()) {
    const x = queue.dequeue();
    if (!nodes.has(x)) {
      const name = indexToID(graph.nodeCount());
      graph.setNode(name, makeNode(x));
      nodes.set(x, name);
      for (const edge of children(x)) {
        edges.push([edge, x]);
        queue.enqueue(edge.child);
      }
    }
  }

  for (const [{ child, name }, parent] of edges) {
    graph.setEdge(
      safe(nodes.get(child), "missing child"),
      safe(nodes.get(parent), "missing parent"),
      undefined,
      name
    );
  }

  // this relies on the fact that the outputs were the first things in the queue
  return { graph, outputs: outputs.map((_, i) => indexToID(i)), nodes };
};

// graph is the graph field of some ad.Graph
const getInputs = (
  graph: graphlib.Graph
): { id: string; label: ad.InputNode }[] => {
  const inputs = [];
  // every input must be a source
  for (const id of graph.sources()) {
    const label: ad.Node = graph.node(id);
    // other non-const sources include n-ary nodes with an empty params array
    if (typeof label !== "number" && label.tag === "Input") {
      inputs.push({ id, label });
    }
  }
  return inputs;
};

/**
 * Mutate graph (but not any of the `VarAD`s used to construct it) to add, for
 * each input, an output for the partial derivative of graph.outputs[output]
 * with respect to that input.
 * @returns a map from each input name to the index of its partial derivative in
 * graph.outputs
 */
export const addGradient = (
  { graph, outputs }: ad.Graph,
  output: number
): Map<string, number> => {
  const m = new Map<string, number>();
  for (const { label } of getInputs(graph)) {
    const derivativeID = indexToID(graph.nodeCount());
    graph.setNode(derivativeID, 0); // TODO: put the actual derivative
    m.set(label.name, outputs.length);
    outputs.push(derivativeID);
  }
  return m;
};

// ------------ Meta / debug ops

/**
 * Creates a wrapper node around a node `v` to store log info. Dumps node value (during evaluation) to the console. You must use the node that `debug` returns, otherwise the debug information will not appear.
 * For more documentation on how to use this function, see the Penrose wiki page.
 */
export const debug = (v: VarAD, info = "no additional info"): ad.Debug => ({
  tag: "Debug",
  node: v,
  info,
});

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
    return _.reduce(res, (x: VarAD, y) => add(x, y), 0);
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
    return ops.vmul(-1, v);
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
    const vsize = add(ops.vnorm(v), EPS_DENOM);
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
    return _.reduce(res, (x: VarAD, y) => add(x, y), 0);
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

    return acos(mul(s, ops.vdot(ops.vnormalize(u), ops.vnormalize(v))));
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
    return _.reduce(v, (x: VarAD, y) => add(x, y), 0);
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
    const angle = mul(a, Math.PI / 180);
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
    return squared(max(x, 0));
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

const compileUnary = ({ unop }: ad.UnaryNode, param: string): string => {
  switch (unop) {
    case "neg": {
      return `-${param}`;
    }
    case "squared": {
      return `${param} * ${param}`;
    }
    case "inverse": {
      return `1 / (${param} + ${EPS_DENOM})`;
    }
    case "sqrt": // NOTE: Watch out for negative numbers in sqrt
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
      return `Math.${unop}(${param})`;
    }
  }
};

const compileBinary = (
  { binop }: ad.BinaryNode,
  left: string,
  right: string
): string => {
  switch (binop) {
    case "+":
    case "*":
    case "-":
    case "/":
    case ">":
    case "<":
    case "===":
    case "&&":
    case "||": {
      return `${left} ${binop} ${right}`;
    }
    case "max":
    case "min":
    case "atan2":
    case "pow": {
      return `Math.${binop}(${left}, ${right})`;
    }
  }
};

const compileNary = ({ op }: ad.NaryNode, params: string[]): string => {
  switch (op) {
    case "addN": {
      return params.length > 0 ? params.join(" + ") : "0";
    }
    case "maxN": {
      return `Math.max(${params.join(", ")})`;
    }
    case "minN": {
      return `Math.min(${params.join(", ")})`;
    }
  }
};

const compileNode = (node: ad.Node, preds: Map<ad.Edge, string>): string => {
  if (typeof node === "number") {
    return `${node}`;
  }
  switch (node.tag) {
    case "Input": {
      return node.name;
    }
    case "Unary": {
      return compileUnary(node, safe(preds.get(undefined), "missing param"));
    }
    case "Binary": {
      return compileBinary(
        node,
        safe(preds.get("left"), "missing left"),
        safe(preds.get("right"), "missing right")
      );
    }
    case "Ternary": {
      const cond = safe(preds.get("cond"), "missing cond");
      const then = safe(preds.get("then"), "missing then");
      const els = safe(preds.get("els"), "missing els");
      return `${cond} ? ${then} : ${els}`;
    }
    case "Nary": {
      const params = [];
      for (const [i, x] of preds.entries()) {
        // TODO: get rid of this typecast
        params[naryEdgeToIndex(i as ad.NaryEdge)] = x;
      }
      return compileNary(node, params);
    }
    case "Debug": {
      const info = JSON.stringify(node.info);
      const child = safe(preds.get(undefined), "missing node");
      return `console.log(${info}, " | value: ", ${child}), ${child}`;
    }
  }
};

export const genCode = ({ graph, outputs }: ad.Graph): ad.Compiled => {
  const stmts = getInputs(graph).map(
    ({ id, label: { name } }) =>
      `const ${id} = inputs.get(${JSON.stringify(name)});`
  );
  for (const id of graphlib.alg.topsort(graph)) {
    const node: ad.Node = graph.node(id);
    // we already generated code for the inputs
    if (typeof node === "number" || node.tag !== "Input") {
      const edges = graph.inEdges(id);
      // the type of inEdges says it can return void
      if (!Array.isArray(edges)) {
        throw Error("expected inEdges to be an array");
      }
      const preds = new Map(
        // TODO: get rid of this typecast
        edges.map(({ v, name }) => [name as ad.Edge, v])
      );
      stmts.push(`const ${id} = ${compileNode(node, preds)};`);
    }
  }
  stmts.push(`return [${outputs.join(", ")}];`);
  const f = new Function("inputs", stmts.join("\n"));
  return (inputs) => f(inputs);
};
