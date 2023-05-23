import {
  Input,
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
import seedrandom from "seedrandom";
import { createEffect, createSignal, on, onCleanup } from "solid-js";
import { RotatingTriangles } from "./triangles.js";

const slider = (
  curr: number,
  [min, max]: [number, number],
  label: string,
  onChange: (n: number) => void
) => (
  <div>
    {label}
    <input
      type="range"
      min={min}
      max={max}
      step={(max - min) / 100}
      value={curr}
      class="slider"
      onInput={(n) => onChange(+n.target.value)}
      onChange={(n) => onChange(+n.target.value)}
    />
    {curr}
  </div>
);

const Curves = () => {
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
      <div>{slider(length(), [100, 3000], "Length: ", setLength)}</div>
      <div>
        {slider(numPoints(), [10, 200], "Number of points: ", setNumPoints)}
      </div>
      <svg
        version="1.2"
        xmlns="http://www.w3.org/2000/svg"
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

export default RotatingTriangles;
