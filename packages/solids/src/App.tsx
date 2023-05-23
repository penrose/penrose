import {
  Input,
  Num,
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
import seedrandom from "seedrandom";
import { createEffect, createSignal, on, onCleanup } from "solid-js";
import { createMutable } from "solid-js/store";

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
  const t = scalar(0);
  const [w, h] = [500, 500];
  const planeSize = 50;
  const planeHeight = -40;
  const cZ = -160;
  // const rng = seedrandom("test");
  const rng = seedrandom();

  const rotate = (vec: Num[], theta: Num) => [
    add(mul(vec[0], cos(theta)), mul(vec[2], sin(theta))),
    vec[1],
    sub(mul(vec[2], cos(theta)), mul(vec[0], sin(theta))),
  ];
  const perspective = (vec: Num[]) =>
    [vec[0], vec[1]].map((v) => mul(div(w, sub(vec[2], cZ)), v));
  const toCanvas = (p: number[]): number[] => [p[0] + w / 2, -p[1] + h / 2];

  // plane
  const plane = (theta: Input) => {
    const q00 = [-planeSize, planeHeight, -planeSize];
    const q10 = [planeSize, planeHeight, -planeSize];
    const q01 = [-planeSize, planeHeight, planeSize];
    const q11 = [planeSize, planeHeight, planeSize];

    const Qs = [q00, q10, q11, q01].map((v) => rotate(v, theta));

    const ps = Qs.map(perspective);
    const points = ps.map((p) => numsOf(p)).map(toCanvas);
    return (
      <polygon
        points={points.join(" ")}
        fill={"#0003"}
        stroke={"#aaa"}
        stroke-width={0.5}
      ></polygon>
    );
  };

  // triangles
  const triangleWithShadow = (
    qs: Input[][],
    theta: Input,
    fillColor: string
  ) => {
    // triangle
    const [qi, qj, qk] = qs.map((p) => rotate(p, theta));
    const ps = [qi, qj, qk].map(perspective);
    const triangle = ps.map((p) => numsOf(p)).map(toCanvas);
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = rs.map(perspective);
    const shadow = ss.map((p) => numsOf(p)).map(toCanvas);
    return (
      <g>
        <polygon
          points={triangle.join(" ")}
          fill={fillColor}
          stroke={"#1b1f8a"}
          stroke-width={0.5}
        ></polygon>
        <polygon points={shadow.join(" ")} fill={"#0002"}></polygon>
      </g>
    );
  };

  const c = 0.9 * Math.min(planeSize, Math.abs(planeHeight));
  const inputs = (n: number) =>
    Array.from({ length: n }, () => scalar(-c + rng() * 2 * c));
  const tri1 = [inputs(3), inputs(3), inputs(3)];
  const tri2 = [inputs(3), inputs(3), inputs(3)];

  const theta = createMutable<Input>(t);

  // the slider has to be defined inline so the entire slider is not re-rendered. Otherwise, the slider will allow continuous sliding because it gets re-rendered after each slide
  const onSlide = (n: number) => {
    theta.val = n;
  };

  return (
    <>
      <h1>Drag the slider to rotate the camera.</h1>
      <div>
        <div>
          Camera rotation
          <input
            type="range"
            min={0}
            max={10}
            step={0.1}
            value={theta.val}
            class="slider"
            onInput={(n) => onSlide(+n.target.value)}
            onChange={(n) => onSlide(+n.target.value)}
          />
          {theta.val}
        </div>
      </div>
      <svg
        version="1.2"
        xmlns="http://www.ws.org/2000/svg"
        width={800}
        height={800}
      >
        {plane(theta)}
        {triangleWithShadow(tri1, theta, "#34379a")}
        {triangleWithShadow(tri2, theta, "#340000")}
      </svg>
    </>
  );
};

export default App;
