import {
  Input,
  Num,
  add,
  compile,
  cos,
  div,
  mul,
  scalar,
  sin,
  sub,
} from "@penrose/core";
import seedrandom from "seedrandom";
import { Show, createEffect, createResource } from "solid-js";
import { createMutable } from "solid-js/store";

const pairs = <T,>(a: T[]): [T, T][] => {
  const b: [T, T][] = [];
  for (let i = 0; i < a.length; i += 2) b.push([a[i], a[i + 1]]);
  return b;
};

export interface TriangleProps {
  seed?: string;
  theta: Input;
  onFinish?: () => void;
}

export const Triangles = (props: TriangleProps) => {
  const waiting: Promise<void>[] = [];

  const compilePoints = (
    points: [Num, Num][]
  ): (() => [number, number][] | undefined) => {
    const p = compile(points.flat());
    waiting.push(p.then(() => {}));
    const [f] = createResource(() => p);
    return () => {
      const g = f();
      if (g !== undefined) return pairs(g((x) => x.val));
    };
  };

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
  const ps = compilePoints(Qs.map(perspective));

  const plane = (
    <Show when={ps()}>
      {(points) => (
        <polygon
          points={points().map(toCanvas).join(" ")}
          fill={"#0003"}
          stroke={"#aaa"}
          stroke-width={0.5}
        ></polygon>
      )}
    </Show>
  );

  // triangles
  const triangleWithShadow = (qs: Input[][], fillColor: string) => {
    // triangle
    const [qi, qj, qk] = qs.map((p) => rotate(p, props.theta));
    const ps = compilePoints([qi, qj, qk].map(perspective));
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = compilePoints(rs.map(perspective));
    return (
      <>
        <Show when={ps()}>
          {(triangle) => (
            <polygon
              points={triangle().map(toCanvas).join(" ")}
              fill={fillColor}
              stroke={"#1b1f8a"}
              stroke-width={0.5}
            ></polygon>
          )}
        </Show>
        <Show when={ss()}>
          {(shadow) => (
            <polygon
              points={shadow().map(toCanvas).join(" ")}
              fill={"#0002"}
            ></polygon>
          )}
        </Show>
      </>
    );
  };

  const c = 0.9 * Math.min(planeSize, Math.abs(planeHeight));
  const inputs = (n: number) =>
    Array.from({ length: n }, () => scalar(-c + rng() * 2 * c));
  const tri1 = triangleWithShadow([inputs(3), inputs(3), inputs(3)], "#34379a");
  const tri2 = triangleWithShadow([inputs(3), inputs(3), inputs(3)], "#340000");

  createEffect(async () => {
    const f = props.onFinish;
    if (f) {
      await Promise.all(waiting);
      f();
    }
  });

  return (
    <svg version="1.2" xmlns="http://www.w3.org/2000/svg" width={w} height={h}>
      {plane}
      {tri1}
      {tri2}
    </svg>
  );
};

export const RotatingTriangles = () => {
  const theta = createMutable<Input>(scalar(0));

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
      <Triangles theta={theta} />
    </>
  );
};
