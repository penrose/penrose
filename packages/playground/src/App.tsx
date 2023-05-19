import {
  dist,
  elasticEnergy,
  eq,
  equivalued,
  perimeter,
  pow,
  problem,
  scalar,
  sub,
} from "@penrose/core";
import { Input } from "@penrose/core/dist/types/ad";
import seedrandom from "seedrandom";
import { createEffect, createSignal, on, onCleanup } from "solid-js";

const App = () => {
  const [ready, setReady] = createSignal<boolean>(false);
  const [step, setStep] = createSignal<number>(0);
  const [status, setStatus] = createSignal<string>("");
  const [length, setLength] = createSignal<number>(1000);
  const [numPoints, setNumPoints] = createSignal<number>(100);
  const [w, h] = [500, 500];
  const rng = seedrandom("test");

  const [pts, rerun] = createSignal<Input[][]>([], { equals: false });
  let p: any;

  createEffect(
    on([length, numPoints], async () => {
      rerun(
        Array.from({ length: numPoints() }, () => [
          scalar(rng() * w),
          scalar(rng() * h),
        ])
      );
      p = problem(pow(sub(elasticEnergy(pts() as any, true), 0), 2), [
        eq(perimeter(pts() as any, true), length()),
        equivalued(
          pts().map((_, i) => dist(pts()[i], pts()[(i + 1) % numPoints()]))
        ),
      ]);
      p.then(setReady);
      p.then(setStep(0));
    })
  );

  createEffect(async () => {
    if (ready()) {
      let frame = requestAnimationFrame(loop);
      const problem = await p;
      function loop(t: number) {
        const { optStatus } = problem.step(50);
        setStatus(optStatus);
        setStep((i) => i + 1);
        rerun(pts());
        if (optStatus !== "UnconstrainedConverged") {
          frame = requestAnimationFrame(loop);
          setReady(false);
        }
      }

      onCleanup(() => cancelAnimationFrame(frame));
    }
  });

  return (
    <>
      <p>Step {step()}</p>
      <p>Status: {status()}</p>
      <div>
        Length:
        <input
          type="range"
          min="100"
          max="3000"
          value="1000"
          class="slider"
          onChange={(n) => setLength(+n.target.value)}
        />
        {length()}
      </div>
      <div>
        Number of points:
        <input
          type="range"
          min="10"
          max="200"
          value="100"
          class="slider"
          onChange={(n) => setNumPoints(+n.target.value)}
        />
        {numPoints()}
      </div>
      <svg
        version="1.2"
        xmlns="http://www.ws.org/2000/svg"
        width={w}
        height={h}
      >
        <polygon
          points={pts()
            .map(([x, y]) => `${x.val},${y.val}`)
            .join(" ")}
          stroke-width={2}
          stroke="#000"
          fill="#92d53e70"
        />
      </svg>
    </>
  );
};

export default App;
