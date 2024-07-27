import { IJsonModel, Layout, Model, TabNode } from "flexlayout-react";
import "flexlayout-react/style/light.css";
import AnimatedRenderer from "./components/AnimatedRenderer.tsx";
import Renderer from "./components/Renderer.tsx";
import EigenvectorsDiagram from "./examples/eigen.tsx";
import GraphComponent from "./examples/graph.tsx";
import { sets } from "./examples/sets.ts";
import { tire } from "./examples/tire.ts";

const setsDiagram = await sets();
const tireDiagram = await tire();

const layoutJson: IJsonModel = {
  global: {},
  borders: [],
  layout: {
    type: "row",
    weight: 100,
    children: [
      {
        type: "tabset",
        weight: 50,
        children: [
          {
            type: "tab",
            name: "Sets",
            component: "Sets",
          },
          {
            type: "tab",
            name: "Eigenvector",
            component: "Eigenvector",
          },
          {
            type: "tab",
            name: "Graph",
            component: "Graph",
          },
          {
            type: "tab",
            name: "Tire",
            component: "Tire",
          },
        ],
      },
    ],
  },
};

const model = Model.fromJson(layoutJson);

function App() {
  const factory = (node: TabNode) => {
    let element: JSX.Element;
    switch (node.getComponent()) {
      case "Sets":
        element = <Renderer diagram={setsDiagram} />;
        break;

      case "Eigenvector":
        element = <EigenvectorsDiagram />;
        break;

      case "Graph":
        element = <GraphComponent />;
        break;

      case "Tire":
        element = <AnimatedRenderer diagram={tireDiagram} />;
        break;
    }

    return (
      <div
        style={{
          height: "100%",
        }}
      >
        {element!}
      </div>
    );
  };

  return <Layout model={model} factory={factory} />;
}

export default App;
