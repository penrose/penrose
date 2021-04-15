import * as React from "react";
import IViewProps from "./IViewProps";
import cytoscape from "cytoscape";
import { uniqBy } from "lodash";
import dagre from "cytoscape-dagre";
import {
  ShapeTypes,
  PenroseState,
  PenroseFn,
  prettyPrintFn,
  prettyPrintPath,
  prettyPrintExpr,
} from "@penrose/core";

cytoscape.use(dagre);

// import { VarAD } from "@penrose/core/";

// TODO: Verify this is correct on small examples
// TODO: Only render the graph once. Otherwise, it's rerendered on each frame, which makes things substantially slower
// TODO: Visualize the gradient too (multi inputs)

// Flatten a list of lists into a single list of elements
const merge = (arr: any) => [].concat.apply([], arr);

// Given a parent node, returns the graph corresponding to nodes and edges of children
// May contain duplicate nodes
// TODO: Move this to utils?
// TODO: Add type for graph and VarAD
const traverseGraphTopDown = (par: any): any => {
  const parNode = { id: par.id, label: par.op };
  const edges = par.children.map((edge) => ({
    from: parNode.id,
    to: edge.node.id,
  }));

  const subgraphs = par.children.map((edge) => traverseGraphTopDown(edge.node));
  const subnodes = merge(subgraphs.map((g) => g.nodes));
  const subedges = merge(subgraphs.map((g) => g.edges));

  return {
    nodes: [parNode].concat(subnodes),
    edges: edges.concat(subedges),
  };
};

// For building atomic op graph. Returns unique nodes after all nodes are merged
const traverseUnique = (par: any): any => {
  const g = traverseGraphTopDown(par);
  return {
    ...g,
    nodes: uniqBy(g.nodes, (e: any) => e.id),
  };
};

// Convert from `traverseUnique` schema to cytoscape schema
const convertSchema = (graph: any): any => {
  const { nodes, edges } = graph;

  const nodes2 = nodes.map((e) => ({
    data: {
      id: String(e.id), // this needs to be unique
      label: e.label,
    },
  }));

  const edges2 = edges.map((e) => ({
    data: {
      id: String(e.from) + " -> " + String(e.to), // NOTE: No duplicate edges
      source: String(e.from),
      target: String(e.to),
      // No label
    },
  }));

  return { nodes: nodes2, edges: edges2 };
};

// -----

// For opt fn graph
// TODO: Import `Fn` for the types
// TODO: Hover to show outgoing edges (cytoscape)
// TODO / NOTE: This does not work with inline computations (e.g. f(g(p), x)). Everything needs to be a path
const toGraphOpt = (objfns: PenroseFn[], constrfns: PenroseFn[]): any => {
  // One node for each unique path, id = path name, name = path name
  // One node for each unique obj/constr application, id = the function w/ its args, name = function name
  // One edge for each function=>path application, id = from + to names, name = none

  // TODO: Could instead be the union of shapePaths and varyingPaths if we want to show all optimizable paths, not just the ones that are optimized
  const allFns = objfns.concat(constrfns);
  const allPaths: string[] = merge(
    allFns.map((f) => f.fargs.map(prettyPrintExpr))
  ); // TODO: This also includes constants, etc.
  const pathNodes = allPaths.map(p => ({
    data: {
      id: p,
      label: p,
    },
  }));

  // TODO: Show objectives separately from constraints?? Or at least style them differently
  const fnNodes = allFns.map(f => ({
    data: {
      id: prettyPrintFn(f),
      label: f.fname,
      type: f.optType,
    },
  }));

  const nodes = pathNodes.concat(fnNodes);

  const edges = merge(
    allFns.map(f =>
      f.fargs.map((arg) => ({
        data: {
          id: prettyPrintFn(f) + " -> " + prettyPrintExpr(arg),
          source: prettyPrintFn(f), // Matches existing fn ids
          target: prettyPrintExpr(arg), // Matches existing path ids
        },
      }))
    )
  );

  return {
    nodes,
    edges,
  };
};

// ----------

// For translation (render) graph

// TODO: import translation
const toGraphTrans = (trans: any, varyingPaths: any) => {
    const tr = trans.trMap;

    // Top nodes = All Substance objects, fields, and properties [Translation]
    // + Middle nodes = All computations -- involving all operations and paths
    // + Leaf nodes = All constants, paths, and varying vars

    // Edges from top to middle = All subpaths (from Substance objects to their fields and/or properties)
    // Edges from middle to bottom = All path values (assignments to paths) and computations

    // TODO: Write using a for-loop, and a form of checkBlockExpr

    let nodes: any[] = [];
    let edges: any[] = [];

    for (const [subObj, fieldDict] of Object.entries(tr)) {
        const subNode = {
            data: {
                id: subObj,
                label: subObj,
                type: "sub obj"
            }
        };

        nodes.push(subNode);

        // cytoscape insanity - can't use "x.y" syntax in string arguments as it somehow escapes the string and causes a crash
        for (const [field, fexpr] of Object.entries(fieldDict)) {

            const fieldStr = subObj + ":" + field;

            const fieldNode = {
                data: {
                    id: fieldStr,
                    label: field,
                    type: "field"
                }
            };

            const fieldEdge = {
                data: {
                    id: subObj + " -> " + fieldStr,
                    source: subObj,
                    target: fieldStr
                }
            };

            nodes.push(fieldNode);
            edges.push(fieldEdge);

            if (fexpr.tag === "FExpr") {
                // TODO <<< Do FExpr: TagExpr<VarAD>
                // Look up if varying path, and make special ? node
                // Else use checkBlockExpr 

            } else if (fexpr.tag === "FGPI") {
                const [typ, props] = fexpr.contents;
                const typeStr = fieldStr + ":" + typ; 
                const typeNode = {
                    data: {
                        id: typeStr,
                        label: typ,
                        type: "shape ctor"
                    }
                };

                const typeEdge = {
                    data: {
                        id: fieldStr + " -> " + typ,
                        source: fieldStr,
                        target: typeStr
                    }
                };

                console.log("fexpr", fexpr, typeStr, typeNode, typeEdge);

                nodes.push(typeNode);
                edges.push(typeEdge);

                for (const [prop, propExpr] of Object.entries(props)) {
                const propStr = typeStr + ":" + prop; // Because more than one shape can have the same property
                    const propNode = {
                        data: {
                            id: propStr,
                            label: prop,
                            type: "property"
                        }
                    };

                    const propEdge = {
                        data: {
                            id: typeStr + " -> " + propStr,
                            source: typeStr,
                            target: propStr
                        }
                    };

                console.log("prop", prop, propStr, propNode, propEdge);

                    nodes.push(propNode);
                    edges.push(propEdge);

                    // TODO <<< Do propExpr: TagExpr<VarAD>
                    // Look up if varying path, and make special ? node
                    // Else use checkBlockExpr 

                } // end property loop

            } // end gpi case

        } // end field case

    } // end sub obj case

    return { nodes, edges };
};

// ----------
const CompGraph: React.FC<IViewProps> = ({ frame, history }: IViewProps) => {
    if (!frame) {
      return (
        <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
          no frame
        </div>
      );
    }

    // Find nodes and edges for atomic op graph, from top energy node
    const graphAtomic = convertSchema(traverseUnique(frame.params.energyGraph));

    // Find nodes and edges for opt comp graph
    const graphOpt = toGraphOpt(frame.objFns, frame.constrFns);

    // Find nodes and edges for translation graph (computational/render graph of shapes)
    const graphTrans = toGraphTrans(frame.translation, frame.varyingPaths);

    // const graph = graphAtomic; // NOTE: Add dropdown to choose between the above two graphs
    // const graph = graphOpt; // NOTE: Add dropdown to choose between the above two graphs
    const graph = graphTrans; // NOTE: Add dropdown to choose between the above two graphs

    console.log("graph", graph);

    console.log(
      "graph # nodes",
      graph.nodes.length,
      "# edges",
      graph.edges.length
    );

    const graphRef = React.useRef<HTMLDivElement>(null);
    React.useEffect(() => {
      if (graphRef.current !== null) {
        const cy = cytoscape({
          container: graphRef.current, // container to render in

          elements: graph,

          style: [
            // the stylesheet for the graph
            {
              selector: "node",
              style: {
                "background-color": "#666",
              },
            },

              {
              selector: "node[label]",
              style: {
                label: "data(label)", // label comes from a field of the node
              },
            },

            {
              selector: "edge",
              style: {
                width: 3,
                "line-color": "#ccc",
                "target-arrow-color": "#ccc",
                "target-arrow-shape": "triangle",
                "curve-style": "bezier",
              },
            },
          ],

          // layout: {
          //   name: "grid",
          //   rows: 1,
          //   fit: true
          // }
        });

        cy.layout({
          name: "dagre",
        }).run();

        return () => {
          cy.destroy();
        };
      }
    }, []);

    return (
      <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
        <div style={{ fontSize: "12px" }}>
          Computation graph of the energy (atomic ops only)
      </div>
        <div
          ref={graphRef}
          style={{ width: "100%", height: "100%", flexGrow: 1 }}
        />
      </div>
    );
  };
export default CompGraph;
