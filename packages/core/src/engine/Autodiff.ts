import { Queue } from "@datastructures-js/queue";
import consola, { LogLevel } from "consola";
import * as graphlib from "graphlib";
import * as _ from "lodash";
import * as ad from "types/ad";
import { GradGraphs, VarAD } from "types/ad";
import { WeightInfo } from "types/state";
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

export const input = ({
  index,
  val,
}: {
  index: number;
  val: number;
}): ad.Input => ({ tag: "Input", index, val });

export const makeADInputVars = (xs: number[], start = 0): ad.Input[] =>
  xs.map((val, i) => input({ index: start + i, val }));

// every VarAD is already an ad.Node, but this function removes all the children
const makeNode = (x: VarAD): ad.Node => {
  if (typeof x === "number") {
    return x;
  }
  const node: ad.Node = x; // get some typechecking by not using x after this
  const { tag } = node;
  switch (tag) {
    case "Input": {
      const { index } = node;
      return { tag, index };
    }
    case "Unary": {
      const { i, unop } = node;
      return { tag, i, unop };
    }
    case "Binary": {
      const { i, binop } = node;
      return { tag, i, binop };
    }
    case "Ternary": {
      const { i } = node;
      return { tag, i };
    }
    case "Nary": {
      const { i, op } = node;
      return { tag, i, op };
    }
    case "Debug": {
      const { i, info } = node;
      return { tag, i, info };
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
    case "exp": {
      return z;
    }
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

// note that this function constructs the sensitivities even when we don't need
// them, such as for nodes in secondary outputs or the gradient
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

const indexToID = (index: number): ad.Id => `_${index}`;

// graph is the graph field of some ad.Graph
const getInputs = (
  graph: graphlib.Graph
): { id: ad.Id; label: ad.InputNode }[] => {
  const inputs = [];
  // every input must be a source
  for (const id of graph.sources()) {
    const label: ad.Node = graph.node(id);
    // other non-const sources include n-ary nodes with an empty params array
    if (typeof label !== "number" && label.tag === "Input") {
      inputs.push({ id: id as ad.Id, label }); // TODO: get rid of this typecast
    }
  }
  return inputs;
};

export const makeGraph = (
  outputs: Omit<ad.Outputs<VarAD>, "gradient">
): ad.Graph => {
  const graph = new graphlib.Graph({ multigraph: true });
  const nodes = new Map<VarAD, ad.Id>();

  // we use this queue to essentially do a breadth-first search by following
  // VarAD child pointers; it gets reused a few times because we add nodes in
  // multiple stages
  const queue = new Queue<VarAD>();
  // at each stage, we need to add the edges after adding all the nodes, because
  // when we first look at a node and its in-edges, its children are not
  // guaranteed to exist in the graph yet, so we fill this queue during the
  // node-adding part and then go through it during the edge-adding part,
  // leaving it empty in preparation for the next stage; so the first element of
  // every tuple in this queue stores information about the edge and child, and
  // the second element of the tuple is the parent
  const edges = new Queue<[Child, VarAD]>();

  // only call setNode in this one place, ensuring that we always use indexToID
  const newNode = (node: ad.Node): ad.Id => {
    const id = indexToID(graph.nodeCount());
    graph.setNode(id, node);
    return id;
  };

  // ensure that x is represented in the graph we're building, and if it wasn't
  // already there, enqueue its children and in-edges (so queue and edges,
  // respectively, should both be emptied after calling this)
  const addNode = (x: VarAD): ad.Id => {
    let name = nodes.get(x);
    if (name === undefined) {
      name = newNode(makeNode(x));
      nodes.set(x, name);
      for (const edge of children(x)) {
        edges.enqueue([edge, x]);
        queue.enqueue(edge.child);
      }
    }
    return name;
  };

  const addEdge = (
    child: VarAD,
    parent: VarAD,
    name: ad.Edge
  ): [ad.Id, ad.Id] => {
    const v = safe(nodes.get(child), "missing child");
    const w = safe(nodes.get(parent), "missing parent");
    graph.setEdge(v, w, undefined, name);
    return [v, w];
  };

  // add all the nodes subtended by the primary output; we do these first, in a
  // separate stage, because these are the only nodes for which we actually need
  // to use the sensitivities of their in-edges, and then after we add the
  // edges, we need to get a topological sort of just these nodes
  const primary = addNode(outputs.primary);
  while (!queue.isEmpty()) {
    addNode(queue.dequeue());
  }

  // we need to keep track of these sensitivities so we can add them as nodes
  // right after this, but we also need to know which edge each came from for
  // when we construct the gradient nodes later; note that this simple string
  // concatenation doesn't cause any problems, because no stringified Edge
  // contains an underscore, and every Id starts with an underscore, so it's
  // essentially just three components separated by underscores
  const sensitivities = new Map<`${ad.Edge}${ad.Id}${ad.Id}`, VarAD>();
  while (!edges.isEmpty()) {
    const [{ child, name, sensitivity }, parent] = edges.dequeue();
    const [v, w] = addEdge(child, parent, name);
    // this check for undefined isn't really necessary because we do a similar
    // check later when we look up sensitivities in this map, but it slightly
    // simplifies the value type for the map and also means we store a bit less
    if (sensitivity !== undefined) {
      sensitivities.set(`${name}${v}${w}`, sensitivity);
    }
  }
  // the topsort function always constructs a new array so it's OK that we
  // reverse it in-place; then we can use this reverse topological sort later
  // when we construct all the gradient nodes, because it ensures that the
  // gradients of a node's parents are always available before the node itself;
  // note that we need to compute this right now, because we're just about to
  // add the sensitivity nodes to the graph, and we don't want to try to compute
  // the gradients of those sensitivities
  const primaryNodes = graphlib.alg.topsort(graph).reverse();

  for (const x of sensitivities.values()) {
    addNode(x);
  }
  while (!queue.isEmpty()) {
    addNode(queue.dequeue());
  }
  while (!edges.isEmpty()) {
    const [{ child, name }, parent] = edges.dequeue();
    addEdge(child, parent, name);
  }

  // map from each primary node ID to the ID of its gradient node
  const gradNodes = new Map<ad.Id, ad.Id>();
  for (const idString of primaryNodes) {
    const id = idString as ad.Id; // TODO: get rid of this typecast
    if (id === primary) {
      // use addNode instead of newNode in case there's already a 1 in the graph
      gradNodes.set(id, addNode(1));
      continue;
    }

    const grad: ad.NaryNode = { tag: "Nary", i: -1, op: "addN" };
    const gradID = newNode(grad);
    gradNodes.set(id, gradID);
    const addends = [];

    const edges = graph.outEdges(id);
    // the type of outEdges says it can return void
    if (!Array.isArray(edges)) {
      throw Error("expected outEdges to be an array");
    }
    // HACK: see comment on `count` in engine/AutodiffFunctions
    edges.sort((a, b) => graph.node(a.w).i - graph.node(b.w).i);
    // we call graph.setEdge in this loop, so it may seem like it would be
    // possible for for those edges to get incorrectly included as addends in
    // other gradient nodes; however, that is not the case, because none of
    // those edges appear in our sensitivities map
    for (const { w, name } of edges) {
      const parentID = w as ad.Id; // TODO: get rid of this typecast
      const sensitivity = sensitivities.get(
        `${name as ad.Edge}${id}${parentID}` // TODO: get rid of this typecast
      );
      if (sensitivity !== undefined) {
        const sensitivityID = safe(
          nodes.get(sensitivity),
          "missing sensitivity"
        );
        const parentGradID = safe(
          gradNodes.get(parentID),
          "missing parent grad"
        );

        const addend: ad.BinaryNode = { tag: "Binary", i: -1, binop: "*" };
        const addendID = newNode(addend);

        const left: ad.BinaryEdge = "left";
        const right: ad.BinaryEdge = "right";
        graph.setEdge(sensitivityID, addendID, undefined, left);
        graph.setEdge(parentGradID, addendID, undefined, right);

        addends.push(addendID);
      }
    }

    addends.forEach((addendID, i) =>
      graph.setEdge(addendID, gradID, undefined, indexToNaryEdge(i))
    );
  }

  // easiest case: final stage, just add all the nodes and edges for the
  // secondary outputs
  const secondary = outputs.secondary.map(addNode);
  while (!queue.isEmpty()) {
    addNode(queue.dequeue());
  }
  while (!edges.isEmpty()) {
    const [{ child, name }, parent] = edges.dequeue();
    addEdge(child, parent, name);
  }

  // we wait until after adding all the nodes before get the IDs for the input
  // gradients, because some of the inputs may only be reachable from the
  // secondary outputs instead of the primary output; this isn't really
  // necessary, because the gradients for all those inputs are just zero, so the
  // caller could just substitute zero whenever the gradient is missing a key,
  // but it's probably a bit less surprising if we always include an array
  // element for the gradient on any input that is reachable even from the
  // secondary outputs
  const gradient: ad.Id[] = [];
  for (const {
    id,
    label: { index },
  } of getInputs(graph)) {
    if (index in gradient) {
      throw Error(`duplicate Input index: ${index}`);
    }
    // note that it's very easy for the set of Input indices to not be
    // contiguous, e.g. if some inputs end up not being used in any of the
    // computations in the graph; but even if that happens, it's actually OK
    // (see the comment in the implementation of genCode below)
    gradient[index] = gradNodes.get(id) ?? addNode(0);
  }

  return { graph, nodes, gradient, primary, secondary };
};

/**
 * Construct a graph with a primary output but no secondary outputs.
 */
export const primaryGraph = (output: VarAD): ad.Graph =>
  makeGraph({ primary: output, secondary: [] });

/**
 * Construct a graph from an array of only secondary outputs, for which we don't
 * care about the gradient. The primary output is just the constant 1.
 */
export const secondaryGraph = (outputs: VarAD[]): ad.Graph =>
  // use 1 because makeGraph always constructs a constant gradient node 1 for
  // the primary output, and so if that's already present in the graph then we
  // have one fewer node total
  makeGraph({ primary: 1, secondary: outputs });

// ------------ Meta / debug ops

/**
 * Creates a wrapper node around a node `v` to store log info. Dumps node value (during evaluation) to the console. You must use the node that `debug` returns, otherwise the debug information will not appear.
 * For more documentation on how to use this function, see the Penrose wiki page.
 */
export const debug = (v: VarAD, info = "no additional info"): ad.Debug => ({
  tag: "Debug",
  i: -1, // HACK: see comment on `count` in engine/AutodiffFunctions
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
    const angle = div(mul(a, Math.PI), 180);
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

const compileUnary = ({ unop }: ad.UnaryNode, param: ad.Id): string => {
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
  left: ad.Id,
  right: ad.Id
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

const compileNary = ({ op }: ad.NaryNode, params: ad.Id[]): string => {
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

const compileNode = (
  node: Exclude<ad.Node, ad.InputNode>,
  preds: Map<ad.Edge, ad.Id>
): string => {
  if (typeof node === "number") {
    return `${node}`;
  }
  switch (node.tag) {
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
      const params: ad.Id[] = [];
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

export const genCode = ({
  graph,
  gradient,
  primary,
  secondary,
}: ad.Graph): ad.Compiled => {
  const stmts = getInputs(graph).map(
    ({ id, label: { index } }) => `const ${id} = inputs[${index}];`
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
        // TODO: get rid of these typecasts
        edges.map(({ v, name }) => [name as ad.Edge, v as ad.Id])
      );
      stmts.push(`const ${id} = ${compileNode(node, preds)};`);
    }
  }
  const fields = [
    // somehow this actually works! if the gradient array is not contiguous, any
    // gaps will just be filled in by commas when we call join, and that is
    // valid JavaScript syntax for array literals with gaps, so everything works
    // out perfectly
    `gradient: [${gradient.join(", ")}]`,
    `primary: ${primary}`,
    `secondary: [${secondary.join(", ")}]`,
  ];
  stmts.push(`return { ${fields.join(", ")} };`);
  const f = new Function("inputs", stmts.join("\n"));
  return (inputs) => f(inputs);
};

// Mutates xsVars (leaf nodes) to set their values to the inputs in xs (and name them accordingly by value)
// NOTE: the xsVars should already have been set as inputs via makeAdInputVars
// NOTE: implicitly, the orders of the values need to match the order of variables
const setInputs = (xsVars: ad.Input[], xs: number[]) => {
  const l1 = xsVars.length;
  const l2 = xs.length;
  if (l1 !== l2) {
    throw Error(`xsVars and xs shouldn't be different lengths: ${l1} vs ${l2}`);
  }
  xsVars.forEach((v, i) => {
    v.val = xs[i];
  });
};

const setWeights = (info: WeightInfo) => {
  info.epWeightNode.val = info.epWeight;
};

// Given an energyGraph of f, returns the compiled energy and gradient of f as functions
// xsVars are the leaves, energyGraph is the topmost parent of the computational graph
export const energyAndGradCompiled = (
  xs: number[],
  xsVars: ad.Input[],
  energyGraph: VarAD,
  weightInfo: WeightInfo | undefined
): { graphs: GradGraphs; f: ad.Compiled } => {
  // Set the weight nodes to have the right weight values (may have been updated at some point during the opt)
  if (weightInfo !== undefined) {
    setWeights(weightInfo);
  }

  // Set the leaves of the graph to have the new input values
  setInputs(xsVars, xs);

  // Build an actual graph from the implicit VarAD structure
  // Build symbolic gradient of f at xs on the energy graph
  const explicitGraph = primaryGraph(energyGraph);

  const epWeightNode: VarAD | undefined = weightInfo?.epWeightNode; // Generate energy and gradient without weight

  const graphs: GradGraphs = {
    inputs: xsVars,
    weight: epWeightNode,
  };

  // Synthesize energy and gradient code
  const f0 = genCode(explicitGraph);
  if (epWeightNode !== undefined && epWeightNode.index !== 0) {
    throw Error("epWeightNode must be the first input");
  }
  const allInputs =
    epWeightNode === undefined ? xsVars : [epWeightNode, ...xsVars];
  const f: ad.Compiled = (inputs) => {
    const outputs = f0(inputs);
    const { gradient } = outputs;
    return {
      ...outputs,
      // fill in any gaps, in case some inputs weren't used in the graph
      gradient: allInputs.map((v, i) => (i in gradient ? gradient[i] : 0)),
    };
  };

  // Return the energy and grad on the input, as well as updated energy graph
  return { graphs, f };
};
