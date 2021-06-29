import * as React from "react";
import cytoscape from "cytoscape";
import dagre from "cytoscape-dagre";

// const renderGraph = () => {
//   const graph = [
//     { data: { id: 'a' } },
//     { data: { id: 'b' } },
//     {
//       data: {
//         id: 'ab',
//         source: 'a',
//         target: 'b'
//       }
//     }];

//   const graphRef: React.RefObject<HTMLDivElement> = React.useRef<HTMLDivElement>(null);

//   React.useEffect(() => {
//     if (graphRef.current !== null) {
//       const cy = cytoscape({
//         container: graphRef.current, // container to render in

//         elements: graph,

//         style: [
//           // the stylesheet for the graph
//           {
//             selector: "node",
//             style: {
//               "background-color": "#666",
//             },
//           },

//           {
//             selector: "node[label]",
//             style: {
//               label: "data(label)", // label comes from a field of the node
//             },
//           },

//           {
//             selector: "edge",
//             style: {
//               width: 3,
//               "line-color": "#ccc",
//               "target-arrow-color": "#ccc",
//               "target-arrow-shape": "triangle",
//               "curve-style": "bezier",
//             },
//           },
//         ],
//       });

//       cy.layout({
//         name: "dagre",
//       }).run();

//       return () => {
//         cy.destroy();
//       };
//     }
//   }, []);
// };

class GraphForm extends React.Component<{}, { value: string }> {
  constructor(props) {
    super(props);
    this.state = { value: "atomic" };

    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleChange(event) {
    this.setState({ value: event.target.value });
  }

  handleSubmit(event) {
    console.log("The chosen graph is: " + this.state.value);

    // renderGraph();

    event.preventDefault();
  }

  render() {
    return (
      <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
        <form onSubmit={this.handleSubmit}>
          <label>
            Pick the graph:
            <select value={this.state.value} onChange={this.handleChange}>
              <option value="atomic">Atomic operations in energy</option>
              <option value="opt">Optimization functions</option>
              <option value="translation">Translation</option>
            </select>
          </label>
          <input type="submit" value="Submit" />
        </form>

        <div style={{ width: "100%", height: "100%", flexGrow: 1 }} />
      </div>
    );
  }
}

export default GraphForm;
