import seedrandom from "seedrandom";
import texts from "./texts";

const elem = await texts(
  seedrandom("2"),
  [800, 800],
  // ["Nimo", "Keenan", "Jiri", "Leo", "Sam"]
  ["Nimo", "Keenan", "Jiri"]
);

function App() {
  return <div style={{ width: "100%", height: "100%" }}>{elem}</div>;
}

export default App;
