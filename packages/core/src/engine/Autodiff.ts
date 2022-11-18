import { Queue } from "@datastructures-js/queue";
import {
  builtins,
  exportFunctionName,
  exportTableName,
  Gradient,
  importMemoryModule,
  importMemoryName,
  Outputs,
} from "@penrose/optimizer";
import consola, { LogLevel } from "consola";
import * as _ from "lodash";
import { EigenvalueDecomposition, Matrix } from "ml-matrix";
import * as ad from "types/ad";
import { Multidigraph } from "utils/Graph";
import { safe, zip2 } from "utils/Util";
import * as wasm from "utils/Wasm";
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
  sign,
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

export const input = ({ key, val }: Omit<ad.Input, "tag">): ad.Input => ({
  tag: "Input",
  key,
  val,
});

// every ad.Num is already an ad.Node, but this function returns a new object
// with all the children removed
const makeNode = (x: ad.Expr): ad.Node => {
  if (typeof x === "number") {
    return x;
  }
  const node: ad.Node = x; // get some typechecking by not using x after this
  const { tag } = node;
  switch (tag) {
    case "Input": {
      const { key } = node;
      return { tag, key };
    }
    case "Not": {
      return { tag };
    }
    case "Unary": {
      const { unop } = node;
      return { tag, unop };
    }
    case "Binary": {
      const { binop } = node;
      return { tag, binop };
    }
    case "Comp": {
      const { binop } = node;
      return { tag, binop };
    }
    case "Logic": {
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
    case "PolyRoots": {
      return { tag };
    }
    case "Index": {
      const { index } = node;
      return { tag, index };
    }
    case "Debug": {
      const { info } = node;
      return { tag, info };
    }
  }
};

const unarySensitivity = (z: ad.Unary): ad.Num => {
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
      return sign(v);
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

const binarySensitivities = (z: ad.Binary): { left: ad.Num; right: ad.Num } => {
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
  }
};

const indexToNaryEdge = (index: number): ad.NaryEdge => `${index}`;
const naryEdgeToIndex = (name: ad.NaryEdge) => parseInt(name, 10);

// return the index at which `edge` appeared when returned from the `children`
// function defined below
const rankEdge = (edge: ad.Edge): number => {
  switch (edge) {
    case undefined:
    case "left":
    case "cond": {
      return 0;
    }
    case "right":
    case "then": {
      return 1;
    }
    case "els": {
      return 2;
    }
    default: {
      return naryEdgeToIndex(edge);
    }
  }
};

interface Child {
  child: ad.Expr;
  name: ad.Edge;
  sensitivity: ad.Num[][]; // rows for parent, columns for child
}

// note that this function constructs the sensitivities even when we don't need
// them, such as for nodes in secondary outputs or the gradient
const children = (x: ad.Expr): Child[] => {
  if (typeof x === "number") {
    return [];
  }
  switch (x.tag) {
    case "Input": {
      return [];
    }
    case "Not": {
      return [
        {
          child: x.param,
          name: undefined,
          sensitivity: [],
        },
      ];
    }
    case "Unary": {
      return [
        {
          child: x.param,
          name: undefined,
          sensitivity: [[unarySensitivity(x)]],
        },
      ];
    }
    case "Binary": {
      const { left, right } = binarySensitivities(x);
      return [
        { child: x.left, name: "left", sensitivity: [[left]] },
        { child: x.right, name: "right", sensitivity: [[right]] },
      ];
    }
    case "Comp":
    case "Logic": {
      return [
        { child: x.left, name: "left", sensitivity: [] },
        { child: x.right, name: "right", sensitivity: [] },
      ];
    }
    case "Ternary": {
      return [
        { child: x.cond, name: "cond", sensitivity: [[]] },
        { child: x.then, name: "then", sensitivity: [[ifCond(x.cond, 1, 0)]] },
        { child: x.els, name: "els", sensitivity: [[ifCond(x.cond, 0, 1)]] },
      ];
    }
    case "Nary": {
      return x.params.map((child, i) => {
        const c = { child, name: indexToNaryEdge(i) };
        switch (x.op) {
          case "addN": {
            return { ...c, sensitivity: [[1]] };
          }
          case "maxN": {
            return { ...c, sensitivity: [[ifCond(lt(child, x), 0, 1)]] };
          }
          case "minN": {
            return { ...c, sensitivity: [[ifCond(gt(child, x), 0, 1)]] };
          }
        }
      });
    }
    case "PolyRoots": {
      // https://www.skewray.com/articles/how-do-the-roots-of-a-polynomial-depend-on-the-coefficients

      const n = x.coeffs.length;
      const derivCoeffs: ad.Num[] = x.coeffs.map((c, i) => mul(i, c));
      derivCoeffs.shift();
      // the polynomial is assumed monic, so `x.coeffs` doesn't include the
      // coefficient 1 on the highest-degree term
      derivCoeffs.push(n);

      const sensitivities: ad.Num[][] = x.coeffs.map((_, index) => {
        const t: ad.Num = { tag: "Index", index, vec: x }; // a root

        let power: ad.Num = 1;
        const powers: ad.Num[] = [power];
        for (let i = 1; i < n; i++) {
          power = mul(power, t);
          powers.push(power);
        }

        const minusDerivative = neg(
          addN(zip2(derivCoeffs, powers).map(([c, p]) => mul(c, p)))
        );

        // if the root is `NaN` then it doesn't contribute to the gradient
        const real = eq(t, t);
        return powers.map((p) => ifCond(real, div(p, minusDerivative), 0));
      });

      return x.coeffs.map((child, i) => ({
        child,
        name: indexToNaryEdge(i),
        sensitivity: sensitivities.map((row) => [row[i]]),
      }));
    }
    case "Index": {
      // this node doesn't know how many elements are in `vec`, so here we just
      // leave everything else undefined, to be treated as zeroes later
      const row = [];
      row[x.index] = 1;
      return [{ child: x.vec, name: undefined, sensitivity: [row] }];
    }
    case "Debug": {
      return [{ child: x.node, name: undefined, sensitivity: [[1]] }];
    }
  }
};

const indexToID = (index: number): ad.Id => `_${index}`;
const idToIndex = (id: ad.Id): number => parseInt(id.slice(1), 10);

const getInputs = (
  graph: ad.Graph["graph"]
): { id: ad.Id; label: ad.InputNode }[] => {
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
 * Construct an explicit graph from a primary output and array of secondary
 * outputs. All out-edges relevant to computing the gradient can be considered
 * totally ordered, first by the node the edge points to (where the nodes are
 * numbered by doing a breadth-first search from the primary output using the
 * `children` function) and then by the name of the edge (again according to the
 * order given by the `children` function). The partial derivatives contributing
 * to any given gradient node are added up according to that total order.
 */
export const makeGraph = (
  outputs: Omit<Outputs<ad.Num>, "gradient">
): ad.Graph => {
  const graph = new Multidigraph<ad.Id, ad.Node, ad.Edge>();
  const nodes = new Map<ad.Expr, ad.Id>();

  // we use this queue to essentially do a breadth-first search by following
  // `ad.Expr` child pointers; it gets reused a few times because we add nodes
  // in multiple stages
  const queue = new Queue<ad.Expr>();
  // at each stage, we need to add the edges after adding all the nodes, because
  // when we first look at a node and its in-edges, its children are not
  // guaranteed to exist in the graph yet, so we fill this queue during the
  // node-adding part and then go through it during the edge-adding part,
  // leaving it empty in preparation for the next stage; so the first element of
  // every tuple in this queue stores information about the edge and child, and
  // the second element of the tuple is the parent
  const edges = new Queue<[Child, ad.Expr]>();

  // only call setNode in this one place, ensuring that we always use indexToID
  const newNode = (node: ad.Node): ad.Id => {
    const id = indexToID(graph.nodeCount());
    graph.setNode(id, node);
    return id;
  };

  // ensure that x is represented in the graph we're building, and if it wasn't
  // already there, enqueue its children and in-edges (so queue and edges,
  // respectively, should both be emptied after calling this)
  const addNode = (x: ad.Expr): ad.Id => {
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
    child: ad.Expr,
    parent: ad.Expr,
    name: ad.Edge
  ): [ad.Id, ad.Id] => {
    const v = safe(nodes.get(child), "missing child");
    const w = safe(nodes.get(parent), "missing parent");
    graph.setEdge({ v, w, name });
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
  const sensitivities = new Map<`${ad.Edge}${ad.Id}${ad.Id}`, ad.Num[][]>();
  while (!edges.isEmpty()) {
    const [{ child, name, sensitivity }, parent] = edges.dequeue();
    const [v, w] = addEdge(child, parent, name);
    sensitivities.set(`${name}${v}${w}`, sensitivity);
  }
  // we can use this reverse topological sort later when we construct all the
  // gradient nodes, because it ensures that the gradients of a node's parents
  // are always available before the node itself; note that we need to compute
  // this right now, because we're just about to add the sensitivity nodes to
  // the graph, and we don't want to try to compute the gradients of those
  // sensitivities
  const primaryNodes = [...graph.topsort()].reverse();

  for (const matrix of sensitivities.values()) {
    // `forEach` ignores holes
    matrix.forEach((row) => {
      row.forEach(addNode);
    });
  }
  while (!queue.isEmpty()) {
    addNode(queue.dequeue());
  }
  while (!edges.isEmpty()) {
    const [{ child, name }, parent] = edges.dequeue();
    addEdge(child, parent, name);
  }

  // map from each primary node ID to the IDs of its gradient nodes
  const gradNodes = new Map<ad.Id, ad.Id[]>();
  for (const id of primaryNodes) {
    if (id === primary) {
      // use addNode instead of newNode in case there's already a 1 in the graph
      gradNodes.set(id, [addNode(1)]);
      continue;
    }

    // our node needs to have some number of gradient nodes, depending on its
    // type, so we assemble an array of the addends for each gradient node; we
    // don't need to know the length of this array ahead of time, because
    // JavaScript allows holes in arrays, so instead of actually looking at the
    // node to see what type it is, we just accumulate into whatever slots are
    // mentioned by the sensitivities of our out-edges, and let all else be zero
    const grad: ad.Id[][] = [];

    // control the order in which partial derivatives are added
    const edges = [...graph.outEdges(id)].sort((a, b) =>
      a.w === b.w
        ? rankEdge(a.name) - rankEdge(b.name)
        : idToIndex(a.w) - idToIndex(b.w)
    );

    // we call graph.setEdge in this loop, so it may seem like it would be
    // possible for those edges to get incorrectly included as addends in other
    // gradient nodes; however, that is not the case, because none of those
    // edges appear in our sensitivities map
    for (const { w, name } of edges) {
      const matrix = sensitivities.get(`${name}${id}${w}`);
      if (matrix !== undefined) {
        // `forEach` ignores holes
        matrix.forEach((row, i) => {
          row.forEach((x, j) => {
            const sensitivityID = safe(nodes.get(x), "missing sensitivity");
            const parentGradIDs = safe(gradNodes.get(w), "missing parent grad");
            if (i in parentGradIDs) {
              const parentGradID = parentGradIDs[i];

              const addendID = newNode({ tag: "Binary", binop: "*" });
              graph.setEdge({ v: sensitivityID, w: addendID, name: "left" });
              graph.setEdge({ v: parentGradID, w: addendID, name: "right" });
              if (!(j in grad)) {
                grad[j] = [];
              }
              grad[j].push(addendID);
            }
          });
        });
      }
    }

    gradNodes.set(
      id,
      // `map` skips holes but also preserves indices
      grad.map((addends) => {
        const gradID = newNode({ tag: "Nary", op: "addN" });
        addends.forEach((addendID, i) =>
          graph.setEdge({ v: addendID, w: gradID, name: indexToNaryEdge(i) })
        );
        return gradID;
      })
    );
  }

  // we get the IDs for the input gradients before adding all the secondary
  // nodes, because some of the inputs may only be reachable from the secondary
  // outputs instead of the primary output; really, the gradients for all those
  // inputs are just zero, so the caller needs to substitute zero whenever the
  // gradient is missing a key
  const gradient: ad.Id[] = [];
  for (const {
    id,
    label: { key },
  } of getInputs(graph)) {
    if (key in gradient) {
      throw Error(`duplicate Input key: ${key}`);
    }
    // note that it's very easy for the set of Input indices to not be
    // contiguous, e.g. if some inputs end up not being used in any of the
    // computations in the graph; but even if that happens, it's actually OK
    gradient[key] = safe(gradNodes.get(id), "missing gradient")[0];
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

  return { graph, nodes, gradient, primary, secondary };
};

/**
 * Construct a graph with a primary output but no secondary outputs.
 */
export const primaryGraph = (output: ad.Num): ad.Graph =>
  makeGraph({ primary: output, secondary: [] });

/**
 * Construct a graph from an array of only secondary outputs, for which we don't
 * care about the gradient. The primary output is just the constant 1.
 */
export const secondaryGraph = (outputs: ad.Num[]): ad.Graph =>
  // use 1 because makeGraph always constructs a constant gradient node 1 for
  // the primary output, and so if that's already present in the graph then we
  // have one fewer node total
  makeGraph({ primary: 1, secondary: outputs });

// ------------ Meta / debug ops

/**
 * Creates a wrapper node around a node `v` to store log info. Dumps node value (during evaluation) to the console. You must use the node that `debug` returns, otherwise the debug information will not appear.
 * For more documentation on how to use this function, see the Penrose wiki page.
 */
export const debug = (v: ad.Num, info = "no additional info"): ad.Debug => ({
  tag: "Debug",
  node: v,
  info,
});

// ----------------- Other ops

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
   * Return the sum of vectors `v1, v2.
   */
  vadd: (v1: ad.Num[], v2: ad.Num[]): ad.Num[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, add);
    return res;
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
   * Return the vector `v` divided by scalar `c`.
   */
  vdiv: (v: ad.Num[], c: ad.Num): ad.Num[] => {
    return v.map((e) => div(e, c));
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
      ops.vdot(u, v) // x = |u||v|cos(theta)
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

// ----- Codegen

// Traverses the computational graph of ops obtained by interpreting the energy function, and generates WebAssembly code corresponding to just the ops

const alignDouble = 3;

const numFuncTypes = 3;
const typeIndexUnary = 0;
const typeIndexBinary = 1;
const typeIndexGradient = 2;

const numGradientParams = 3;
const gradientParamInput = 0;
const gradientParamGradient = 1;
const gradientParamSecondary = 2;

const numFunctions = 1;

const typeSection = (t: wasm.Target): void => {
  t.int(numFuncTypes);

  // typeIndexUnary
  const numUnaryParams = 1;
  const numUnaryReturns = 1;
  t.byte(wasm.TYPE.FUNCTION);
  t.int(numUnaryParams);
  t.byte(wasm.TYPE.f64);
  t.int(numUnaryReturns);
  t.byte(wasm.TYPE.f64);

  // typeIndexBinary
  const numBinaryParams = 2;
  const numBinaryReturns = 1;
  t.byte(wasm.TYPE.FUNCTION);
  t.int(numBinaryParams);
  t.byte(wasm.TYPE.f64);
  t.byte(wasm.TYPE.f64);
  t.int(numBinaryReturns);
  t.byte(wasm.TYPE.f64);

  // typeIndexGradient
  const numGradientReturns = 1;
  t.byte(wasm.TYPE.FUNCTION);
  t.int(numGradientParams);
  for (let i = 0; i < numGradientParams; i++) t.byte(wasm.TYPE.i32);
  t.int(numGradientReturns);
  t.byte(wasm.TYPE.f64);
};

const importSection = (t: wasm.Target): void => {
  const numImports = 1;
  t.int(numImports);

  const minPages = 1;
  t.ascii(importMemoryModule);
  t.ascii(importMemoryName);
  t.byte(wasm.IMPORT.MEMORY);
  t.byte(wasm.LIMITS.NO_MAXIMUM);
  t.int(minPages);
};

const functionSection = (t: wasm.Target): void => {
  t.int(numFunctions);
  t.int(typeIndexGradient);
};

const tableSection = (t: wasm.Target): void => {
  const numTables = 1;
  t.int(numTables);

  const minEntries = builtins.length;
  const maxEntries = builtins.length;
  t.byte(wasm.TYPE.FUNCREF);
  t.byte(wasm.LIMITS.MAXIMUM);
  t.int(minEntries);
  t.int(maxEntries);
};

const exportSection = (t: wasm.Target): void => {
  const numExports = 2;
  t.int(numExports);

  const tableIndex = 0;
  t.ascii(exportTableName);
  t.byte(wasm.EXPORT.TABLE);
  t.int(tableIndex);

  const funcIndex = 0;
  t.ascii(exportFunctionName);
  t.byte(wasm.EXPORT.FUNCTION);
  t.int(funcIndex);
};

const modulePrefix = (gradientFunctionSize: number): wasm.Module => {
  const numSections = 6;

  const typeSectionCount = new wasm.Count();
  typeSection(typeSectionCount);
  const typeSectionSize = typeSectionCount.size;

  const importSectionCount = new wasm.Count();
  importSection(importSectionCount);
  const importSectionSize = importSectionCount.size;

  const functionSectionCount = new wasm.Count();
  functionSection(functionSectionCount);
  const functionSectionSize = functionSectionCount.size;

  const tableSectionCount = new wasm.Count();
  tableSection(tableSectionCount);
  const tableSectionSize = tableSectionCount.size;

  const exportSectionCount = new wasm.Count();
  exportSection(exportSectionCount);
  const exportSectionSize = exportSectionCount.size;

  const codeSectionSize =
    wasm.intSize(numFunctions) +
    wasm.intSize(gradientFunctionSize) +
    gradientFunctionSize;

  const sumSectionSizes =
    numSections +
    wasm.intSize(typeSectionSize) +
    typeSectionSize +
    wasm.intSize(importSectionSize) +
    importSectionSize +
    wasm.intSize(functionSectionSize) +
    functionSectionSize +
    wasm.intSize(tableSectionSize) +
    tableSectionSize +
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
  functionSection(mod);

  mod.byte(wasm.SECTION.TABLE);
  mod.int(tableSectionSize);
  tableSection(mod);

  mod.byte(wasm.SECTION.EXPORT);
  mod.int(exportSectionSize);
  exportSection(mod);

  mod.byte(wasm.SECTION.CODE);
  mod.int(codeSectionSize);
  mod.int(numFunctions);
  mod.int(gradientFunctionSize);

  return mod;
};

const compileUnary = (
  t: wasm.Target,
  { unop }: ad.UnaryNode,
  param: number
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
    case "inverse": {
      t.byte(wasm.OP.f64.const);
      t.f64(1);

      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.const);
      t.f64(EPS_DENOM);

      t.byte(wasm.OP.f64.add);

      t.byte(wasm.OP.f64.div);

      return;
    }
    case "round": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.nearest);

      return;
    }
    case "sign": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.const);
      t.f64(1);

      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.copysign);

      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.f64.const);
      t.f64(0);

      t.byte(wasm.OP.f64.eq);

      t.byte(wasm.OP.select);

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
    case "tanh": {
      t.byte(wasm.OP.local.get);
      t.int(param);

      t.byte(wasm.OP.i32.const);
      t.int(builtins.indexOf(unop));

      t.byte(wasm.OP.call_indirect);
      t.int(typeIndexUnary);
      t.int(0);

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
};

const compileBinary = (
  t: wasm.Target,
  { binop }: ad.BinaryNode | ad.CompNode | ad.LogicNode,
  left: number,
  right: number
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
    case "||": {
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

      t.byte(wasm.OP.i32.const);
      t.int(builtins.indexOf(binop));

      t.byte(wasm.OP.call_indirect);
      t.int(typeIndexBinary);
      t.int(0);

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
  { op }: ad.NaryNode,
  params: number[]
): void => {
  t.byte(wasm.OP.f64.const);
  t.f64(nullaryVals[op]);

  for (const param of params) {
    t.byte(wasm.OP.local.get);
    t.int(param);

    t.byte(naryOps[op]);
  }
};

const naryParams = (preds: Map<ad.Edge, number>): number[] => {
  const params: number[] = [];
  for (const [i, x] of preds.entries()) {
    if (
      i === undefined ||
      i === "left" ||
      i === "right" ||
      i === "cond" ||
      i === "then" ||
      i === "els"
    ) {
      throw Error("expected NaryEdge");
    }
    params[naryEdgeToIndex(i)] = x;
  }
  return params;
};

const compileNode = (
  t: wasm.Target,
  node: Exclude<ad.Node, ad.InputNode>,
  preds: Map<ad.Edge, number>
): void => {
  if (typeof node === "number") {
    t.byte(wasm.OP.f64.const);
    t.f64(node);

    return;
  }
  switch (node.tag) {
    case "Not": {
      const child = safe(preds.get(undefined), "missing node");

      t.byte(wasm.OP.local.get);
      t.int(child);

      t.byte(wasm.OP.i32.eqz);

      return;
    }
    case "Unary": {
      compileUnary(t, node, safe(preds.get(undefined), "missing param"));
      return;
    }
    case "Binary":
    case "Comp":
    case "Logic": {
      compileBinary(
        t,
        node,
        safe(preds.get("left"), "missing left"),
        safe(preds.get("right"), "missing right")
      );
      return;
    }
    case "Ternary": {
      const cond = safe(preds.get("cond"), "missing cond");
      const then = safe(preds.get("then"), "missing then");
      const els = safe(preds.get("els"), "missing els");

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
      compileNary(t, node, naryParams(preds));
      return;
    }
    case "PolyRoots": {
      throw Error("TODO");
    }
    case "Index": {
      throw Error("TODO");
    }
    case "Debug": {
      throw Error("TODO");
    }
  }
};

const compileType = (node: ad.Node): number => {
  if (typeof node === "number") {
    return wasm.TYPE.f64;
  } else {
    switch (node.tag) {
      case "Comp":
      case "Logic":
      case "Not": {
        return wasm.TYPE.i32;
      }
      case "Input":
      case "Unary":
      case "Binary":
      case "Ternary":
      case "Nary":
      case "Index":
      case "Debug": {
        return wasm.TYPE.f64;
      }
      case "PolyRoots": {
        throw Error("TODO");
      }
    }
  }
};

const compileWriteArray = (t: wasm.Target, arr: ad.Id[], baseLocal: number) => {
  arr.forEach((id, i) => {
    t.byte(wasm.OP.local.get);
    t.int(baseLocal);

    t.byte(wasm.OP.local.get);
    t.int(numGradientParams + idToIndex(id));

    t.byte(wasm.OP.f64.store);
    t.int(alignDouble);
    t.int(i * Float64Array.BYTES_PER_ELEMENT);
  });
};

const compileGraph = (
  t: wasm.Target,
  { graph, gradient, primary, secondary }: ad.Graph
): void => {
  const numLocals = graph.nodeCount();
  t.int(numLocals);
  for (const id of [...graph.nodes()].sort(
    (a, b) => idToIndex(a) - idToIndex(b)
  )) {
    t.int(1);
    t.byte(compileType(graph.node(id)));
  }

  for (const {
    id,
    label: { key },
  } of getInputs(graph)) {
    t.byte(wasm.OP.local.get);
    t.int(gradientParamInput);

    t.byte(wasm.OP.f64.load);
    t.int(alignDouble);
    t.int(key * Float64Array.BYTES_PER_ELEMENT);

    t.byte(wasm.OP.local.set);
    t.int(numGradientParams + idToIndex(id));
  }

  for (const id of graph.topsort()) {
    const node = graph.node(id);
    // we already generated code for the inputs
    if (typeof node === "number" || node.tag !== "Input") {
      const preds = new Map(
        graph
          .inEdges(id)
          .map(({ v, name }) => [name, numGradientParams + idToIndex(v)])
      );

      compileNode(t, node, preds);

      t.byte(wasm.OP.local.set);
      t.int(numGradientParams + idToIndex(id));
    }
  }

  compileWriteArray(t, gradient, gradientParamGradient);

  compileWriteArray(t, secondary, gradientParamSecondary);

  t.byte(wasm.OP.local.get);
  t.int(numGradientParams + idToIndex(primary));

  t.byte(wasm.END);
};

const polyRoots = (coeffs: number[]): number[] => {
  const n = coeffs.length;
  // https://en.wikipedia.org/wiki/Companion_matrix
  const companion = Matrix.zeros(n, n);
  for (let i = 0; i + 1 < n; i++) {
    companion.set(i + 1, i, 1);
    companion.set(i, n - 1, -coeffs[i]);
  }
  companion.set(n - 1, n - 1, -coeffs[n - 1]);

  // the characteristic polynomial of the companion matrix is equal to the
  // original polynomial, so by finding the eigenvalues of the companion matrix,
  // we get the roots of its characteristic polynomial and thus of the original
  // polynomial
  const decomp = new EigenvalueDecomposition(companion);
  return zip2(
    decomp.realEigenvalues,
    decomp.imaginaryEigenvalues
    // as mentioned in the `polyRoots` docstring in `engine/AutodiffFunctions`,
    // we discard any non-real root and replace with `NaN`
  ).map(([r, i]) => (i === 0 ? r : NaN));
};

export const genCode = (g: ad.Graph): Gradient => {
  const count = new wasm.Count();
  compileGraph(count, g);
  const mod = modulePrefix(count.size);
  compileGraph(mod, g);
  if (mod.count.size !== mod.bytes.length)
    throw Error(
      `allocated ${mod.bytes.length} bytes but used ${mod.count.size}`
    );
  return new Gradient(mod.bytes, g.secondary.length);
};
