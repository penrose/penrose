import {
  Num,
  Var,
  add,
  cos,
  div,
  mul,
  sin,
  sub,
  variable,
} from "@penrose/core";
import seedrandom from "seedrandom";
import { Accessor, createEffect, createSignal, on } from "solid-js";
import { SetStoreFunction, createStore } from "solid-js/store";
import { numSignal } from "./util.js";

type Sampler = (x: number) => number;

const sample = <T,>(
  seed: Accessor<string>,
  f: (makeVar: (sampler: Sampler) => Var) => T
): T => {
  const vars: { sampler: Sampler; setter: SetStoreFunction<Var> }[] = [];
  const rng = seedrandom(seed());
  let ok = true;
  const res = f((sampler) => {
    if (!ok) throw Error("can't keep sampling after scope is done");
    const [x, setter] = createStore(variable(sampler(rng())));
    vars.push({ sampler, setter });
    return x;
  });
  ok = false;
  createEffect(
    on(
      seed,
      (s) => {
        const rng = seedrandom(s);
        vars.forEach(({ sampler, setter }) => setter({ val: sampler(rng()) }));
      },
      { defer: true }
    )
  );
  return res;
};

export interface TriangleProps {
  seed: string;
  theta: Num;
}

export const Triangles = (props: TriangleProps) => {
  const [w, h] = [500, 500];
  const planeSize = 50;
  const planeHeight = -40;
  const cZ = -160;

  const rotate = (vec: Num[], theta: Num) => [
    add(mul(vec[0], cos(theta)), mul(vec[2], sin(theta))),
    vec[1],
    sub(mul(vec[2], cos(theta)), mul(vec[0], sin(theta))),
  ];
  const perspective = (vec: Num[]): [Num, Num] => {
    const f = (v: Num) => mul(div(w, sub(vec[2], cZ)), v);
    return [f(vec[0]), f(vec[1])];
  };
  const toCanvas = (p: number[]): number[] => [p[0] + w / 2, -p[1] + h / 2];

  // plane
  const q00 = [-planeSize, planeHeight, -planeSize];
  const q10 = [planeSize, planeHeight, -planeSize];
  const q01 = [-planeSize, planeHeight, planeSize];
  const q11 = [planeSize, planeHeight, planeSize];

  const Qs = [q00, q10, q11, q01].map((v) => rotate(v, props.theta));
  const ps = Qs.map(perspective);
  const points = ps.map((p) => p.map(numSignal));
  const plane = (
    <polygon
      points={points.map(([x, y]) => toCanvas([x(), y()])).join(" ")}
      fill={"#0003"}
      stroke={"#aaa"}
      stroke-width={0.5}
    ></polygon>
  );

  // triangles
  const triangleWithShadow = (qs: Num[][], fillColor: string) => {
    // triangle
    const [qi, qj, qk] = qs.map((p) => rotate(p, props.theta));
    const ps = [qi, qj, qk].map(perspective);
    const triangle = ps.map((p) => p.map(numSignal));
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = rs.map(perspective);
    const shadow = ss.map((p) => p.map(numSignal));
    return (
      <>
        <polygon
          points={triangle.map(([x, y]) => toCanvas([x(), y()])).join(" ")}
          fill={fillColor}
          stroke={"#1b1f8a"}
          stroke-width={0.5}
        ></polygon>
        <polygon
          points={shadow.map(([x, y]) => toCanvas([x(), y()])).join(" ")}
          fill={"#0002"}
        ></polygon>
      </>
    );
  };

  const c = 0.9 * Math.min(planeSize, Math.abs(planeHeight));
  const { qs1, qs2 } = sample(
    () => props.seed,
    (makeVar) => {
      const corners = (n: number) =>
        Array.from({ length: n }, () => makeVar((x) => -c + x * 2 * c));
      return {
        qs1: [corners(3), corners(3), corners(3)],
        qs2: [corners(3), corners(3), corners(3)],
      };
    }
  );
  const tri1 = triangleWithShadow(qs1, "#34379a");
  const tri2 = triangleWithShadow(qs2, "#340000");

  return (
    <svg version="1.2" xmlns="http://www.w3.org/2000/svg" width={w} height={h}>
      {plane}
      {tri1}
      {tri2}
    </svg>
  );
};

export const RotatingTriangles = () => {
  const [seed, setSeed] = createSignal("skadoosh");
  const [theta, setTheta] = createStore(variable(0));

  const onSlide = (n: number) => {
    setTheta({ val: n });
  };

  return (
    <>
      <h1>Drag the slider to rotate the camera.</h1>
      <div>
        <div>
          Seed{" "}
          <input
            value={seed()}
            onInput={(e) => setSeed(e.target.value)}
            onChange={(e) => setSeed(e.target.value)}
          ></input>
        </div>
        <div>
          Camera rotation{" "}
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
      <Triangles seed={seed()} theta={theta} />
    </>
  );
};
