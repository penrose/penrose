import Renderer from "./components/Renderer.tsx";
import { basisVectors } from "./examples/eigen.ts";
import GraphComponent from "./examples/graph.tsx";
import { sets } from "./examples/sets.ts";

const setsDiagram = await sets();
const basisVectorsDiagram = await basisVectors();

function App() {
  return (
    <>
      <div
        style={{
          display: "flex",
          flexWrap: "wrap",
          position: "absolute",
          justifyContent: "space-evenly",
          alignItems: "center",
          width: "100%",
          height: "100%",
        }}
      >
        <div
          style={{
            minWidth: "33%",
          }}
        >
          <Renderer diagram={setsDiagram} />
        </div>
        <div
          style={{
            minWidth: "33%",
          }}
        >
          <Renderer diagram={basisVectorsDiagram} />
        </div>
        <div
          style={{
            minWidth: "33%",
          }}
        >
          <GraphComponent />
        </div>
      </div>
    </>
  );
}

export default App;
