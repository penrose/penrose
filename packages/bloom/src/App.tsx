// import { circles } from "./examples/simple.js";
import { sets } from "./examples/sets.js";
import Renderer from "./renderer/Renderer.tsx";

const diagram = await sets();

function App() {
  return (
    <>
      <Renderer diagram={diagram} />
    </>
  );
}

export default App;
