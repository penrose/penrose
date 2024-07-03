// import { circles } from "./examples/simple.js";
import Renderer from "./renderer/Renderer.tsx";
import { sets } from "./examples/sets.js";

const diagram = await sets();

function App() {
  return (
    <>
      <Renderer diagram={diagram} />
    </>
  );
}

export default App;
