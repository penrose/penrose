// import { circles } from "./examples/simple.js";
import { basisVectors } from "./examples/eigen.js";
import Renderer from "./renderer/Renderer.tsx";

const diagram = await basisVectors();

function App() {
  return (
    <>
      <Renderer diagram={diagram} />
    </>
  );
}

export default App;
