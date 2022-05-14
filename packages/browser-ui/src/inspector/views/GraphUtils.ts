import {
  PenroseFn,
  prettyPrintExpr,
  prettyPrintFn,
  prettyPrintPath,
  secondaryGraph,
} from "@penrose/core";
import * as ad from "@penrose/core/build/dist/types/ad";
import { A } from "@penrose/core/build/dist/types/ast";
import { Fn } from "@penrose/core/build/dist/types/state";
import { Path } from "@penrose/core/build/dist/types/style";
import { EdgeDefinition, ElementsDefinition, NodeDefinition } from "cytoscape";

// TODO: The nodes and edges are left untyped for now because adding new keys to them (e.g. `DOF: true`) is used to style their CSS - not sure how to accommodate this as a TS type

export type PGraph = ElementsDefinition;
/*{
  // Graph for cytoscape (see `graph1` for example + schema)
  nodes: any;
  edges: any;
}
*/

/**
 * Flatten an array of arrays into a single array of elements
 * I.e., merge<string>( string[][] ): string[]
 */
const merge = <T>(arr: T[][]): T[] => ([] as T[]).concat(...arr);

const labelNode = (node: ad.Node): string => {
  if (typeof node === "number") {
    return `${node}`;
  }
  switch (node.tag) {
    case "Input": {
      return `x${node.key}`;
    }
    case "Unary": {
      return node.unop;
    }
    case "Binary":
    case "Comp":
    case "Logic": {
      return node.binop;
    }
    case "Ternary": {
      return "?:";
    }
    case "Nary": {
      return node.op;
    }
    case "PolyRoots": {
      return "polyRoots";
    }
    case "Index": {
      return `[${node.index}]`;
    }
    case "Debug": {
      return JSON.stringify(node.info);
    }
  }
};

/**
 * For building atomic op graph.
 * Given a parent node, returns the graph corresponding to nodes and edges of children
 * Returns unique nodes after all nodes are merged
 * TODO: Add type for graph and ad.Num
 */
export const traverseUnique = (par: ad.Num): PGraph => {
  // TODO: do not navigate graph outside core
  const { graph } = secondaryGraph([par]);
  return {
    nodes: graph
      .nodes()
      .map((id) => ({ data: { id, label: labelNode(graph.node(id)) } })),
    edges: graph
      .edges()
      .map(({ v, w }) => ({ data: { source: w, target: v } })), // note: flipped
  };
};

/**
 * Convert from `traverseUnique` schema to cytoscape schema
 */
export const convertSchema = (graph: PGraph): PGraph => {
  const { nodes, edges } = graph;

  const nodes2 = nodes.map((e) => ({
    data: {
      id: String(e.data.id), // this needs to be unique
      label: e.data.label,
    },
  }));

  const edges2 = edges.map((e) => ({
    data: {
      id: String(e.data.source) + " -> " + String(e.data.target), // NOTE: No duplicate edges
      source: String(e.data.source),
      target: String(e.data.target),
      // No label
    },
  }));

  return { nodes: nodes2, edges: edges2 };
};

// -----

/**
 * Make nodes and edges related to showing one DOF node (p is a varying path)
 */
export const toGraphDOF = (p: Path<A>, allArgs: string[]): PGraph => {
  const empty = { nodes: [], edges: [] };

  if (p.tag === "FieldPath") {
    return empty;
  } else if (p.tag === "PropertyPath") {
    const fp: Path<A> = {
      ...p,
      tag: "FieldPath",
      name: p.name,
      field: p.field,
    };

    if (!allArgs.includes(prettyPrintPath(fp))) {
      return empty;
    }

    // TODO: Could all the prettyprinting cause performance issues for large graphs?
    // Connect X.shape to X.shape.property
    const edge = {
      data: {
        id: prettyPrintPath(fp) + " -> " + prettyPrintPath(p),
        source: prettyPrintPath(fp),
        target: prettyPrintPath(p),
      },
    };

    return { nodes: [], edges: [edge] };
  } else if (p.tag === "AccessPath") {
    // If X.shape exists, connect X.shape to X.shape.property to X.shape.property[i]
    const graph0 = toGraphDOF(p.path, allArgs);

    // Make node for X.shape.property (the one for X.shape.property[i] already exists)
    const propNode = {
      data: {
        id: prettyPrintPath(p.path),
        label: prettyPrintPath(p.path),
      },
    };

    // Make edge for X.shape.property to X.shape.property[i]
    const propEdge = {
      data: {
        id: prettyPrintPath(p.path) + " -> " + prettyPrintPath(p),
        source: prettyPrintPath(p.path),
        target: prettyPrintPath(p),
      },
    };

    return {
      // TODO: Use graph combination function
      nodes: graph0.nodes.concat([propNode]),
      edges: graph0.edges.concat([propEdge]),
    };
  } else if (p.tag === "LocalVar" || "InternalLocalVar") {
    throw Error("unexpected node type: local var in varying var");
  }

  return empty;
};

/**
 * Make nodes and edges related to showing DOF (degrees of
 * freedom) nodes
 */
export const toGraphDOFs = (
  varyingPaths: Path<A>[],
  allFns: Fn[],
  allArgs: string[]
): PGraph => {
  const varyingPathNodes = varyingPaths.map((p) => {
    const res = prettyPrintPath(p);
    return {
      data: {
        id: res,
        label: res,
        DOF: true,
      },
    };
  });

  // Put in edges for varyingPathNodes
  // The edges connecting it are: one additional edge for each layer of nesting, plus a node
  // e.g. A.shape -> A.shape.center -> A.shape.center[0]

  let intermediateNodes: NodeDefinition[] = [];
  let intermediateEdges: EdgeDefinition[] = [];

  for (const p of varyingPaths) {
    // If the varying path is a shape arg AND its shape arg appears in the function arguments, then make intermediate nodes to connect the varying path with the shape arg
    const pGraph = toGraphDOF(p, allArgs);
    intermediateNodes = intermediateNodes.concat(pGraph?.nodes);
    intermediateEdges = intermediateEdges.concat(pGraph?.edges);
  }

  return {
    nodes: merge([varyingPathNodes, intermediateNodes]),
    edges: merge([intermediateEdges]),
  };
};

// For opt fn graph
// TODO: Hover to show outgoing edges (cytoscape)
// TODO / NOTE: This does not work with inline computations (e.g. f(g(p), x)). Everything needs to be a path
export const toGraphOpt = (
  objfns: PenroseFn[],
  constrfns: PenroseFn[],
  varyingPaths: Path<A>[]
): PGraph => {
  // One node for each unique path, id = path name, name = path name
  // One node for each unique obj/constr application, id = the function w/ its args, name = function name
  // One edge for each function=>path application, id = from + to names, name = none

  const allFns = objfns.concat(constrfns);
  const allArgs: string[] = merge(
    allFns.map((f) => f.fargs.map(prettyPrintExpr))
  ); // This also includes constants, etc.

  const argNodes = allArgs.map((p) => ({
    data: {
      id: p,
      label: p,
    },
  }));

  const fnNodes = allFns.map((f) => ({
    data: {
      id: prettyPrintFn(f),
      label: f.fname,
      [f.optType]: true,
    },
  }));

  const varyingGraph = toGraphDOFs(varyingPaths, allFns, allArgs);

  const nodes = merge([argNodes, fnNodes, varyingGraph.nodes]);

  const fnEdges = merge(
    allFns.map((f) =>
      f.fargs.map((arg) => ({
        data: {
          id: prettyPrintFn(f) + " -> " + prettyPrintExpr(arg),
          source: prettyPrintFn(f), // Matches existing fn ids
          target: prettyPrintExpr(arg), // Matches existing path ids
        },
      }))
    )
  );

  const edges = merge([fnEdges, varyingGraph.edges]);

  return {
    nodes,
    edges,
  };
};
