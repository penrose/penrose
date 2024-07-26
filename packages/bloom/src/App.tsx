import Renderer from "./components/Renderer.tsx";
import EigenvectorDiagram from "./examples/eigen.tsx";
import GraphComponent from "./examples/graph.tsx";
import { sets } from "./examples/sets.ts";
import { tire } from "./examples/tire.ts";
import AnimatedRenderer from "./components/AnimatedRenderer.tsx";

const setsDiagram = await sets();
const tireDiagram = await tire();

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
            minWidth: "30%",
          }}
        >
          <Renderer diagram={setsDiagram} />
        </div>
        <div
          style={{
            minWidth: "30%",
          }}
        >
          <EigenvectorDiagram />
        </div>
        <div
          style={{
            minWidth: "30%",
          }}
        >
          <GraphComponent />
        </div>
        <div
          style={{
            minWidth: "30%",
          }}
        >
          <AnimatedRenderer diagram={tireDiagram} />
        </div>
      </div>
    </>
  );
}

export default App;
