import {
  add,
  cos,
  dist,
  div,
  elasticEnergy,
  eq,
  equivalued,
  mul,
  numsOf,
  perimeter,
  pow,
  problem,
  scalar,
  sin,
  sub,
} from "@penrose/core";
import { Input, Num } from "@penrose/core/dist/types/ad.js";
import seedrandom from "seedrandom";
import { createEffect, createSignal, on, onCleanup } from "solid-js";

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

const App = () => {
  const theta = scalar(1);
  const [w, h] = [500, 500];
  const planeSize = 50;
  const planeHeight = -40;
  const cZ = -160;
  // const rng = seedrandom("test");
  const rng = seedrandom();

  // plane
  const q00 = [-planeSize, planeHeight, -planeSize];
  const q10 = [planeSize, planeHeight, -planeSize];
  const q01 = [-planeSize, planeHeight, planeSize];
  const q11 = [planeSize, planeHeight, planeSize];

  const rotate = (vec: Num[], theta: Num) => [
    add(mul(vec[0], cos(theta)), mul(vec[2], sin(theta))),
    vec[1],
    sub(mul(vec[2], cos(theta)), mul(vec[0], sin(theta))),
  ];
  const Qs = [q00, q10, q11, q01].map((v) => rotate(v, theta));

  const perspective = (vec: Num[]) =>
    [vec[0], vec[1]].map((v) => mul(div(w, sub(vec[2], cZ)), v));

  const ps = Qs.map(perspective);
  const pts = () =>
    ps.map((p) => numsOf(p)).map((p) => [p[0] + w / 2, -p[1] + h / 2]);
  const [points, rerunPoints] = createSignal<number[][]>(pts(), {
    equals: false,
  });

  // triangles
  const c = 0.9 * Math.min(planeSize, Math.abs(planeHeight));
  const inputs = (n: number) =>
    Array.from({ length: n }, () => scalar(-c + rng() * 2 * c));
  const [qi, qj, qk] = [inputs(3), inputs(3), inputs(3)].map((p) =>
    rotate(p, theta)
  );
  const triangle = () => {
    const ps = [qi, qj, qk].map(perspective);
    return ps.map((p) => numsOf(p)).map((p) => [p[0] + w / 2, -p[1] + h / 2]);
  };
  const [tri, rerunTri] = createSignal<number[][]>(triangle(), {
    equals: false,
  });
  const shadow = () => {
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = rs.map(perspective);
    return ss.map((p) => numsOf(p)).map((p) => [p[0] + w / 2, -p[1] + h / 2]);
  };
  const [shad, rerunShad] = createSignal<number[][]>(shadow());

  return (
    <>
      <h1>Daily worship of the TRIANGLE.</h1>
      <div>
        {slider(theta.val, [0, 10], "Camera rotation: ", (n) => {
          theta.val = n;
          rerunPoints(pts());
          rerunTri(triangle());
          rerunShad(shadow());
        })}
      </div>
      <svg
        version="1.2"
        xmlns="http://www.ws.org/2000/svg"
        width={800}
        height={800}
      >
        <polygon
          points={points().join(" ")}
          fill={"#0003"}
          stroke={"#aaa"}
          stroke-width={0.5}
        ></polygon>
        <polygon
          points={tri().join(" ")}
          fill={"#34379aaa"}
          stroke={"#1b1f8a"}
          stroke-width={0.5}
        ></polygon>
        <polygon points={shad().join(" ")} fill={"#0002"}></polygon>
      </svg>
    </>
  );
};

export default App;
