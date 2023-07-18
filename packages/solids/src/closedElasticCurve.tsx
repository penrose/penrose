import {
  Run,
  Var,
  dist,
  elasticEnergy,
  eq,
  equivalued,
  perimeter,
  pow,
  problem,
  sub,
  variable,
} from "@penrose/core";
import seedrandom from "seedrandom";
import { createEffect, createSignal, on, onCleanup } from "solid-js";
import { default as Slider } from "./Slider.jsx";

export default () => {
  const [ready, setReady] = createSignal<boolean>(false);
  const [step, setStep] = createSignal<number>(0);
  const [status, setStatus] = createSignal<boolean>(false);
  const [length, setLength] = createSignal<number>(1000);
  const [numPoints, setNumPoints] = createSignal<number>(100);
  const [w, h] = [500, 500];
  const rng = seedrandom("test");

  const [pts, rerun] = createSignal<Var[][]>([], { equals: false });
  let r: Promise<Run>;

  createEffect(
    on([length, numPoints], () => {
      rerun(
        Array.from({ length: numPoints() }, () => [
          variable(rng() * w),
          variable(rng() * h),
        ]),
      );
      r = problem({
        objective: pow(sub(elasticEnergy(pts() as any, true), 0), 2),
        constraints: [
          eq(perimeter(pts() as any, true), length()),
          equivalued(
            pts().map((_, i) => dist(pts()[i], pts()[(i + 1) % numPoints()])),
          ),
        ],
      }).then((p) => p.start({}));
      r.then(() => {
        setReady(true);
        setStep(0);
      });
    }),
  );

  createEffect(async () => {
    if (ready()) {
      let run = await r;
      const loop = () => {
        let i = 0;
        run = run.run({ until: () => ++i > 50 });
        const { converged, vals } = run;
        for (const [v, x] of vals) v.val = x;
        setStatus(converged);
        setStep((i) => i + 1);
        rerun(pts());
        if (!converged) {
          frame = requestAnimationFrame(loop);
          setReady(false);
        }
      };
      let frame = requestAnimationFrame(loop);
      onCleanup(() => cancelAnimationFrame(frame));
    }
  });

  return (
    <>
      <p>Step {step()}</p>
      <p>Converged? {status().toString()}</p>
      <Slider
        curr={length()}
        min={100}
        max={3000}
        label="Length: "
        onChange={setLength}
      />
      <Slider
        curr={numPoints()}
        min={10}
        max={200}
        label="Number of points: "
        onChange={(n) => setNumPoints(Math.floor(n))}
      />
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
