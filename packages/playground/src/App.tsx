import { useEffect, useRef, useState } from "react";
import seedrandom from "seedrandom";
import curve from "./closedElasticCurve";
import texts from "./texts";

const seed = seedrandom(Math.random().toString());
const textsExample = await texts(
  seed,
  [500, 500],
  ["Melchior", "Balthasar", "Casper"]
);

const { problem, points } = await curve(seed, [500, 500], 100, 1000);

function App() {
  // Use useRef for mutable variables that we want to persist
  // without triggering a re-render on their change
  const requestRef = useRef<number>();

  const [count, setCount] = useState(0);
  const [params, setParams] = useState(problem.step(1));

  const previousTimeRef = useRef<number>();

  const animate = (time: number) => {
    setParams(problem.step(100));
    if (previousTimeRef.current != undefined) {
      const deltaTime = time - previousTimeRef.current;
      setCount((prevCount) => prevCount + deltaTime * 0.01);
    }
    previousTimeRef.current = time;
    if (params.optStatus !== "EPConverged")
      requestRef.current = requestAnimationFrame(animate);
  };

  useEffect(() => {
    requestRef.current = requestAnimationFrame(animate);
    return () => cancelAnimationFrame(requestRef.current!);
  }, []); // Make sure the effect runs only once

  return (
    <>
      Frame #: {Math.floor(count)}
      <br />
      Status: {params.optStatus}
      <div
        style={{
          width: "500px",
          height: "500px",
          border: "1px solid #000",
        }}
      >
        <svg
          version="1.2"
          xmlns="http://www.ws.org/2000/svg"
          width={500}
          height={500}
        >
          <polygon
            points={points.map(([x, y]) => `${x.val},${y.val}`).join(" ")}
            strokeWidth={2}
            stroke="#000"
            fill="#92d53e70"
          />
        </svg>
      </div>
      <div
        style={{
          width: "500px",
          height: "500px",
          border: "1px solid #000",
        }}
      >
        {textsExample}
      </div>
    </>
  );
}

export default App;
