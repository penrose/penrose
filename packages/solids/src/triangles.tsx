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
import { createSignal } from "solid-js";
import { createMutable } from "solid-js/store";
import { numSignal } from "./util.js";

export interface TriangleProps {
  seed: string;
  theta: Num;
}

export const Triangles = (props: TriangleProps) => {
  const [w, h] = [500, 500];
  const planeSize = 50;
  const planeHeight = -40;
  const cZ = -160;
  const rng = seedrandom(props.seed);

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
  const rand = (n: number) =>
    Array.from({ length: n }, () => -c + rng() * 2 * c);
  const tri1 = triangleWithShadow([rand(3), rand(3), rand(3)], "#34379a");
  const tri2 = triangleWithShadow([rand(3), rand(3), rand(3)], "#340000");

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
  const theta = createMutable<Var>(variable(0));

  const onSlide = (n: number) => {
    theta.val = n;
  };

  const triangles = (s: string) => <Triangles seed={s} theta={theta} />;

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
      {triangles(seed())}
    </>
  );
};
