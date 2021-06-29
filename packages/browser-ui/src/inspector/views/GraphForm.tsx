import * as React from "react";
import cytoscape from "cytoscape";
import dagre from "cytoscape-dagre";

const style = [
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
];

const graph1 = [
  { data: { id: "a" } },
  { data: { id: "b" } },
  {
    data: {
      id: "ab",
      source: "a",
      target: "b",
    },
  },
];

const graph2 = [
  { data: { id: "a" } },
  { data: { id: "b" } },
  { data: { id: "c" } },
  {
    data: {
      id: "ab",
      source: "a",
      target: "b",
    },
  },
  {
    data: {
      id: "ac",
      source: "a",
      target: "c",
    },
  },
];

function GraphForm() {
  const [items] = React.useState([
    { label: "none", value: "none" },
    { label: "Optimization functions", value: "opt" },
    { label: "Atomic operations in energy", value: "atomic" },
    { label: "Translation", value: "trans" },
  ]);

  const [value, setValue] = React.useState("none"); // init value of state to use

  const graphRef: React.RefObject<HTMLDivElement> = React.useRef<HTMLDivElement>(
    null
  );

  React.useEffect(() => {
    console.log(`running effect with ${value}`);
    if (graphRef.current !== null) {
      const cy = cytoscape({
        container: graphRef.current, // container to render in
        elements: value === "trans" ? graph1 : graph2,
        style: style,
      });

      cy.layout({
        name: "dagre",
      }).run();

      return () => {
        cy.destroy();
      };
    }
  });

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div>Graph to show: {value}</div>

      <select value={value} onChange={(e) => setValue(e.currentTarget.value)}>
        {items.map(({ label, value }) => (
          <option key={value} value={value}>
            {label}
          </option>
        ))}
      </select>

      <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
        <div
          ref={graphRef}
          style={{ width: "100%", height: "100%", flexGrow: 1 }}
        />
      </div>
    </div>
  );
}

export default GraphForm;
