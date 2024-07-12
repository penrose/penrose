// import { circles } from "./examples/simple.js";
import Renderer from "./components/Renderer.tsx";
import { basisVectors } from "./examples/eigen.ts";
import { sets } from "./examples/sets.ts";

const setsDiagram = await sets();
const basisVectorsDiagram = await basisVectors();

function App() {
  return (
    <>
      <div
        style={{
          display: "grid",
        }}
      >
        <div
          style={{
            gridRow: "1",
            gridColumn: "1",
          }}
        >
          <Renderer diagram={setsDiagram} />
        </div>
        <div
          style={{
            gridRow: "1",
            gridColumn: "2",
          }}
        >
          <Renderer diagram={basisVectorsDiagram} />
        </div>
      </div>
    </>
  );
}

export default App;
