import { Queue } from "@datastructures-js/queue";
import {
  alignStackPointer,
  builtins,
  exportFunctionName,
  Gradient,
  importMemoryName,
  importModule,
  Outputs,
} from "@penrose/optimizer";
import consola from "consola";
import _ from "lodash";
import * as ad from "types/ad";
import Graph from "utils/Graph";
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
  .create({ level: (consola as any).LogLevel.Warn })
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
      const { degree } = node;
      return { tag, degree };
    }
    case "Index": {
      const { index } = node;
      return { tag, index };
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

interface Child {
  child: ad.Expr;
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
      return [{ child: x.param, sensitivity: [] }];
    }
    case "Unary": {
      return [{ child: x.param, sensitivity: [[unarySensitivity(x)]] }];
    }
    case "Binary": {
      const { left, right } = binarySensitivities(x);
      return [
        { child: x.left, sensitivity: [[left]] },
        { child: x.right, sensitivity: [[right]] },
      ];
    }
    case "Comp":
    case "Logic": {
      return [
        { child: x.left, sensitivity: [] },
        { child: x.right, sensitivity: [] },
      ];
    }
    case "Ternary": {
      return [
        { child: x.cond, sensitivity: [[]] },
        { child: x.then, sensitivity: [[ifCond(x.cond, 1, 0)]] },
        { child: x.els, sensitivity: [[ifCond(x.cond, 0, 1)]] },
      ];
    }
    case "Nary": {
      return x.params.map((child) => {
        switch (x.op) {
          case "addN": {
            return { child, sensitivity: [[1]] };
          }
          case "maxN": {
            return { child, sensitivity: [[ifCond(lt(child, x), 0, 1)]] };
          }
          case "minN": {
            return { child, sensitivity: [[ifCond(gt(child, x), 0, 1)]] };
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
        sensitivity: sensitivities.map((row) => [row[i]]),
      }));
    }
    case "Index": {
      // this node doesn't know how many elements are in `vec`, so here we just
      // leave everything else undefined, to be treated as zeroes later
      const row = [];
      row[x.index] = 1;
      return [{ child: x.vec, sensitivity: [row] }];
    }
  }
};

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
  const graph = new Graph<ad.Id, ad.Node, ad.Edge>();
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
  // every tuple in this queue stores information about the edge and child, the
  // second element is the index of the edge with respect to the parent, and the
  // third element of the tuple is the parent
  const edges = new Queue<[Child, ad.Edge, ad.Expr]>();

  // only call setNode in this one place, ensuring that we always use indexToID
  const newNode = (node: ad.Node): ad.Id => {
    const id = graph.nodeCount();
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
      children(x).forEach((edge, index) => {
        edges.enqueue([edge, index, x]);
        queue.enqueue(edge.child);
      });
    }
    return name;
  };

  const addEdge = (
    child: ad.Expr,
    parent: ad.Expr,
    e: ad.Edge
  ): [ad.Id, ad.Id] => {
    const i = safe(nodes.get(child), "missing child");
    const j = safe(nodes.get(parent), "missing parent");
    graph.setEdge({ i, j, e });
    return [i, j];
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
  const sensitivities = new Map<`${ad.Edge}_${ad.Id}_${ad.Id}`, ad.Num[][]>();
  while (!edges.isEmpty()) {
    const [{ child, sensitivity }, index, parent] = edges.dequeue();
    const [v, w] = addEdge(child, parent, index);
    sensitivities.set(`${index}_${v}_${w}`, sensitivity);
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
    const [{ child }, index, parent] = edges.dequeue();
    addEdge(child, parent, index);
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
      a.j === b.j ? a.e - b.e : a.j - b.j
    );

    // we call graph.setEdge in this loop, so it may seem like it would be
    // possible for those edges to get incorrectly included as addends in other
    // gradient nodes; however, that is not the case, because none of those
    // edges appear in our sensitivities map
    for (const { j: w, e } of edges) {
      const matrix = sensitivities.get(`${e}_${id}_${w}`);
      if (matrix !== undefined) {
        // `forEach` ignores holes
        matrix.forEach((row, i) => {
          row.forEach((x, j) => {
            const sensitivityID = safe(nodes.get(x), "missing sensitivity");
            const parentGradIDs = safe(gradNodes.get(w), "missing parent grad");
            if (i in parentGradIDs) {
              const parentGradID = parentGradIDs[i];

              const addendID = newNode({ tag: "Binary", binop: "*" });
              graph.setEdge({ i: sensitivityID, j: addendID, e: 0 });
              graph.setEdge({ i: parentGradID, j: addendID, e: 1 });
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
        if (addends.length === 0) {
          // instead of newNode, in case there's already a 0 in the graph
          return addNode(0);
        } else {
          const gradID = newNode({ tag: "Nary", op: "addN" });
          addends.forEach((addendID, i) => {
            graph.setEdge({ i: addendID, j: gradID, e: i });
          });
          return gradID;
        }
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
    // (see the comment in the implementation of genCode below)
    gradient[key] = safe(gradNodes.get(id), "missing gradient")[0];
  }

  // easiest case: final stage, just add all the nodes and edges for the
  // secondary outputs
  const secondary = outputs.secondary.map(addNode);
  while (!queue.isEmpty()) {
    addNode(queue.dequeue());
  }
  while (!edges.isEmpty()) {
    const [{ child }, index, parent] = edges.dequeue();
    addEdge(child, parent, index);
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
       let row = [];
       for (let j = 0; j < A1.length; j++) {
          row.push( add( A1[i][j], A2[i][j] ));
       }
       result.push( row );
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
       let row = [];
       for (let j = 0; j < A1.length; j++) {
          row.push( sub( A1[i][j], A2[i][j] ));
       }
       result.push( row );
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
     return A.map( function(row) {
        return row.map( (e) => mul(c, e) );
     } );
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
        const summands = _.zipWith( A[i], v, mul );
        result.push( summands.reduce( (x: ad.Num, y) => add(x, y), 0) );
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
     for (let i = 0; i < A.length; i++ ) {
        const row: ad.Num[] = [];
        for (let j = 0; j < A.length; j++ ) {
           row.push( A[j][i] );
        }
        AT.push( row );
     }

     // Now we can just do an ordinary matrix-vector multiply with AT
     const result: ad.Num[] = [];
     for (let i = 0; i < v.length; i++) {
        const summands = _.zipWith( AT[i], v, mul );
        result.push( summands.reduce( (x: ad.Num, y) => add(x, y), 0) );
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
     for (let i = 0; i < B.length; i++ ) {
        const row: ad.Num[] = [];
        for (let j = 0; j < B.length; j++ ) {
           row.push( B[j][i] );
        }
        BT.push( row );
     }

     // Compute A*B via dot products of rows of A with rows of B'
     const result: ad.Num[][] = [];
     for (let i = 0; i < A.length; i++) {
        const row : ad.Num[] = [];
        for (let j = 0; j < A.length; j++) {
           const summands = _.zipWith( A[i], BT[j], mul );
           row.push( summands.reduce( (x: ad.Num, y) => add(x, y), 0) );
        }
        result.push( row );
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
     for (let i = 0; i < A.length; i++ ) {
        const row: ad.Num[] = [];
        for (let j = 0; j < A.length; j++ ) {
           row.push( A[j][i] );
        }
        AT.push( row );
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
     return A.map( function(row) {
        return row.map( (e) => div(e, c) );
     } );
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

const bytesI32 = Int32Array.BYTES_PER_ELEMENT;
const logAlignI32 = Math.log2(bytesI32);

const bytesF64 = Float64Array.BYTES_PER_ELEMENT;
const logAlignF64 = Math.log2(bytesF64);

interface Signature {
  param: { [name: string]: number };
  result: number[];
  local?: { [name: string]: number };
}

const funcTypes = {
  addToStackPointer: {
    param: { size: wasm.TYPE.i32 },
    result: [wasm.TYPE.i32],
  },
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
    },
    result: [wasm.TYPE.f64],
    local: { stackPointer: wasm.TYPE.i32 },
  },
  sum: {
    param: {
      input: wasm.TYPE.i32,
      mask: wasm.TYPE.i32,
      gradient: wasm.TYPE.i32,
      secondary: wasm.TYPE.i32,
    },
    result: [wasm.TYPE.f64],
  },
};

const getTypeIndex = (kind: string): number =>
  Object.keys(funcTypes).indexOf(kind);

const getParamIndex = (sig: Signature, name: string): number =>
  Object.keys(sig.param).indexOf(name);

const getLocalIndex = (sig: Signature, name: string): number =>
  Object.keys(sig.param).length +
  Object.keys(safe(sig.local, "no locals")).indexOf(name);

const builtindex = new Map([...builtins.keys()].map((name, i) => [name, i]));

const getBuiltindex = (name: string): number =>
  safe(builtindex.get(name), "unknown builtin");

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
      t.int(getBuiltindex(`penrose_${unop}`));

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

      t.byte(wasm.OP.call);
      t.int(getBuiltindex(`penrose_${binop}`));

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
  node: Exclude<ad.Node, ad.InputNode>,
  preds: number[]
): void => {
  if (typeof node === "number") {
    t.byte(wasm.OP.f64.const);
    t.f64(node);

    return;
  }
  switch (node.tag) {
    case "Not": {
      const [child] = preds;

      t.byte(wasm.OP.local.get);
      t.int(child);

      t.byte(wasm.OP.i32.eqz);

      return;
    }
    case "Unary": {
      const [param] = preds;
      compileUnary(t, node, param);
      return;
    }
    case "Binary":
    case "Comp":
    case "Logic": {
      const [left, right] = preds;
      compileBinary(t, node, left, right);
      return;
    }
    case "Ternary": {
      const [cond, then, els] = preds;

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
      compileNary(t, node, preds);
      return;
    }
    case "PolyRoots": {
      const bytes =
        Math.ceil((node.degree * bytesF64) / alignStackPointer) *
        alignStackPointer;

      t.byte(wasm.OP.i32.const);
      t.int(-bytes);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex("__wbindgen_add_to_stack_pointer"));

      t.byte(wasm.OP.local.tee);
      t.int(getLocalIndex(funcTypes.addend, "stackPointer"));

      preds.forEach((index, i) => {
        t.byte(wasm.OP.local.get);
        t.int(getLocalIndex(funcTypes.addend, "stackPointer"));

        t.byte(wasm.OP.local.get);
        t.int(index);

        t.byte(wasm.OP.f64.store);
        t.int(logAlignF64);
        t.int(i * bytesF64);
      });

      t.byte(wasm.OP.i32.const);
      t.int(node.degree);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex("penrose_poly_roots"));

      for (let i = 0; i < node.degree; i++) {
        t.byte(wasm.OP.local.get);
        t.int(getLocalIndex(funcTypes.addend, "stackPointer"));

        t.byte(wasm.OP.f64.load);
        t.int(logAlignF64);
        t.int(i * bytesF64);
      }

      t.byte(wasm.OP.i32.const);
      t.int(bytes);

      t.byte(wasm.OP.call);
      t.int(getBuiltindex("__wbindgen_add_to_stack_pointer"));

      t.byte(wasm.OP.drop);

      return;
    }
    case "Index": {
      const [vec] = preds;

      t.byte(wasm.OP.local.get);
      t.int(vec + node.index);

      return;
    }
  }
};

type Typename = "i32" | "f64";

const getLayout = (node: ad.Node): { typename: Typename; count: number } => {
  if (typeof node === "number") {
    return { typename: "f64", count: 1 };
  } else {
    switch (node.tag) {
      case "Comp":
      case "Logic":
      case "Not": {
        return { typename: "i32", count: 1 };
      }
      case "Input":
      case "Unary":
      case "Binary":
      case "Ternary":
      case "Nary":
      case "Index": {
        return { typename: "f64", count: 1 };
      }
      case "PolyRoots": {
        return { typename: "f64", count: node.degree };
      }
    }
  }
};

interface Local {
  typename: Typename;
  index: number;
}

interface Locals {
  counts: { i32: number; f64: number };
  indices: Map<ad.Id, Local>;
}

const numAddendParams = Object.keys(funcTypes.addend.param).length;

const getIndex = (locals: Locals, id: ad.Id): number => {
  const local = safe(locals.indices.get(id), "missing local");
  return (
    numAddendParams +
    (local.typename === "i32" ? 0 : locals.counts.i32) +
    local.index
  );
};

const compileGraph = (
  t: wasm.Target,
  { graph, gradient, primary, secondary }: ad.Graph
): void => {
  const counts = { i32: 1, f64: 0 }; // `stackPointer`
  const indices = new Map<ad.Id, Local>();
  for (const id of graph.nodes()) {
    const node = graph.node(id);
    const { typename, count } = getLayout(node);
    indices.set(id, { typename, index: counts[typename] });
    counts[typename] += count;
  }
  const locals = { counts, indices };

  const numLocalDecls = Object.keys(counts).length;
  t.int(numLocalDecls);

  for (const [typename, count] of Object.entries(counts)) {
    t.int(count);
    t.byte(wasm.TYPE[typename]);
  }

  for (const {
    id,
    label: { key },
  } of getInputs(graph)) {
    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "input"));

    t.byte(wasm.OP.f64.load);
    t.int(logAlignF64);
    t.int(key * bytesF64);

    t.byte(wasm.OP.local.set);
    t.int(getIndex(locals, id));
  }

  for (const id of graph.topsort()) {
    const node = graph.node(id);
    // we already generated code for the inputs
    if (typeof node === "number" || node.tag !== "Input") {
      const preds: number[] = [];
      for (const { i: v, e } of graph.inEdges(id)) {
        preds[e] = getIndex(locals, v);
      }

      compileNode(t, node, preds);

      const index = getIndex(locals, id);
      for (let i = getLayout(node).count - 1; i >= 0; i--) {
        t.byte(wasm.OP.local.set);
        t.int(index + i);
      }
    }
  }

  gradient.forEach((id, i) => {
    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "gradient"));

    t.byte(wasm.OP.local.get);
    t.int(getParamIndex(funcTypes.addend, "gradient"));

    t.byte(wasm.OP.f64.load);
    t.int(logAlignF64);
    t.int(i * bytesF64);

    t.byte(wasm.OP.local.get);
    t.int(getIndex(locals, id));

    t.byte(wasm.OP.f64.add);

    t.byte(wasm.OP.f64.store);
    t.int(logAlignF64);
    t.int(i * bytesF64);
  });

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
      `allocated ${mod.bytes.length} bytes but used ${mod.count.size}`
    );
  return mod.bytes;
};

/**
 * Compile an array of graphs into a function to compute the sum of their
 * primary outputs. The gradients are also summed. The keys present in the
 * secondary outputs must be disjoint; they all go into the same array, so the
 * expected secondary outputs would be ambiguous if keys were shared.
 * @param graphs an array of graphs to compile
 * @returns a compiled/instantiated WebAssembly function
 */
export const genCode = async (...graphs: ad.Graph[]): Promise<Gradient> =>
  await Gradient.make(
    await WebAssembly.compile(genBytes(graphs)),
    graphs.length,
    Math.max(0, ...graphs.map((g) => g.secondary.length))
  );

/**
 * Synchronous version of `genCode`. Should not be used in the browser because
 * this will fail if the generated module is larger than 4 kilobytes, but
 * currently is used in convex partitioning for convenience.
 */
export const genCodeSync = (...graphs: ad.Graph[]): Gradient =>
  Gradient.makeSync(
    new WebAssembly.Module(genBytes(graphs)),
    graphs.length,
    Math.max(0, ...graphs.map((g) => g.secondary.length))
  );
