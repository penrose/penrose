import * as React from "react";
import IViewProps from "./IViewProps";
import cytoscape from "cytoscape";
import { uniqBy } from "lodash";
import dagre from "cytoscape-dagre";

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
  const edges = par.children.map(edge => ({
    from: parNode.id,
    to: edge.node.id
  }));

  const subgraphs = par.children.map(edge => traverseGraphTopDown(edge.node));
  const subnodes = merge(subgraphs.map(g => g.nodes));
  const subedges = merge(subgraphs.map(g => g.edges));

  return {
    nodes: [parNode].concat(subnodes),
    edges: edges.concat(subedges)
  };
};

// Return unique nodes after all nodes are merged
const traverseUnique = (par: any): any => {
  const g = traverseGraphTopDown(par);
  return {
    ...g,
    nodes: uniqBy(g.nodes, (e: any) => e.id)
  };
};

const CompGraph: React.FC<IViewProps> = ({ frame, history }: IViewProps) => {
  if (!frame) {
    return (
      <div style={{ padding: "1em", fontSize: "1em", color: "#4f4f4f" }}>
        no frame
      </div>
    );
  }

  const top = frame.params.energyGraph;
  const graph = traverseUnique(top);

  // convert between schemas
  const { nodes, edges } = graph;
  const nodes2 = nodes.map(e => ({
    data: {
      id: String(e.id),
      label: e.label
    }
  }));
  const edges2 = edges.map(e => ({
    data: {
      id: String(e.from) + " " + String(e.to), // NOTE: No duplicate edges
      source: String(e.from),
      target: String(e.to)
      // No label
    }
  }));

  console.log("graph # nodes", nodes2.length, "# edges", edges2.length);

  const graphRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    if (graphRef.current !== null) {
      const cy = cytoscape({
        container: graphRef.current, // container to render in

        elements: {
          nodes: nodes2,
          edges: edges2
        },

        style: [
          // the stylesheet for the graph
          {
            selector: "node",
            style: {
              "background-color": "#666",
              label: "data(label)"
            }
          },

          {
            selector: "edge",
            style: {
              width: 3,
              "line-color": "#ccc",
              "target-arrow-color": "#ccc",
              "target-arrow-shape": "triangle",
              "curve-style": "bezier"
            }
          }
        ]

        // layout: {
        //   name: "grid",
        //   rows: 1,
        //   fit: true
        // }
      });

      cy.layout({
        name: "dagre"
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
