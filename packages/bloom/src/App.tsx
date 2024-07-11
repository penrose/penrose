// import { circles } from "./examples/simple.js";
import Renderer from "./components/Renderer.tsx";
import { sets } from "./examples/sets.ts";

const diagram = await sets();

function App() {
  return (
    <>
      <Renderer diagram={diagram} />
    </>
  );
}

export default App;
