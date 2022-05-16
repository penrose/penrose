import { Layout, Model, TabNode } from "flexlayout-react";
import { useCallback } from "react";
import ProgramEditor from "./components/ProgramEditor";

const layoutModel = Model.fromJson({
  global: {
    tabEnableClose: false,
  },
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
            name: ".sub",
            component: "programEditor",
            config: {
              kind: "substance",
            },
          },
          {
            type: "tab",
            name: ".sty",
            component: "programEditor",
            config: {
              kind: "style",
            },
          },
          {
            type: "tab",
            name: ".dsl",
            component: "programEditor",
            config: {
              kind: "domain",
            },
          },
        ],
      },
      {
        type: "tabset",
        weight: 50,
        children: [
          {
            type: "tab",
            name: "Diagram",
            component: "diagram",
          },
        ],
      },
    ],
  },
});

function App() {
  const panelFactory = useCallback((node: TabNode) => {
    switch (node.getComponent()) {
      case "programEditor":
        return <ProgramEditor kind={node.getConfig().kind} />;
      case "diagram":
        return <div>diagram</div>;
    }
    return <div>Placeholder</div>;
  }, []);
  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      Penrose!!!
      <div style={{ position: "relative", flex: 1 }}>
        <Layout model={layoutModel} factory={panelFactory} />
      </div>
    </div>
  );
}

export default App;
