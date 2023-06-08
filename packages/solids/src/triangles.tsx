import {
  Num,
  add,
  addN,
  cos,
  div,
  eq,
  lt,
  measureText,
  mul,
  neg,
  problem,
  sin,
  sub,
  textBBox,
  variable,
} from "@penrose/core";
import { convexPolygonMinkowskiSDF } from "@penrose/core/dist/contrib/Minkowski.js";
import { BBox, corners } from "@penrose/core/dist/engine/BBox.js";
import { For, createEffect, createResource, createSignal, on } from "solid-js";
import { createMutable, createStore } from "solid-js/store";
import { bool, num, sample, signalBool, signalNum } from "./util.js";

export interface TriangleProps {
  seed: string;
  theta: Num;
  onFinish?: () => void;
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
  const ps = Qs.map(perspective).map((p) => p.map(signalNum));
  const plane = (
    <polygon
      points={ps.map(([x, y]) => toCanvas([num(x), num(y)])).join(" ")}
      fill={"#0003"}
      stroke={"#aaa"}
      stroke-width={0.5}
    ></polygon>
  );

  const waiting: Promise<void>[] = [];

  // triangles
  const triangleWithShadow = (
    qs: Num[][],
    fillColor: string,
    names: string[]
  ) => {
    // triangle
    const [qi, qj, qk] = qs.map((p) => rotate(p, props.theta));
    const ps = [qi, qj, qk].map(perspective).map((p) => p.map(signalNum));
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = rs.map(perspective).map((p) => p.map(signalNum));

    const vertices = () => ps.map(([x, y]) => [num(x), num(y)]);
    const labels = vertices().map(([x, y]) => [
      createMutable(variable(x)),
      createMutable(variable(y)),
    ]);
    const bboxes = labels.map(([x, y], i) =>
      textBBox(measureText(names[i], "italic bold Palatino"), x, y)
    );
    const labelSet = new Set(labels.flat());
    const [compiling] = createResource(() => {
      const prob = problem({
        constraints: bboxes.map((bbox) => {
          const textCorners = corners(bbox as BBox);
          return eq(
            convexPolygonMinkowskiSDF(
              ps,
              [
                textCorners.bottomLeft,
                textCorners.bottomRight,
                textCorners.topRight,
                textCorners.topLeft,
              ].map(([x, y]) => [neg(x), neg(y)]),
              0
            ),
            10
          );
        }),
      });
      waiting.push(prob.then(() => {}));
      return prob;
    });
    createEffect(
      on([vertices, compiling], ([ps, prob]) => {
        ps.forEach((p, i) => {
          labels[i][0].val = p[0];
          labels[i][1].val = p[1];
        });
        if (prob !== undefined) {
          const run = prob.start({ freeze: (x) => !labelSet.has(x) }).run({});
          for (const [v, x] of run.vals) v.val = x;
        }
      })
    );

    createEffect(async () => {
      const f = props.onFinish;
      if (f) {
        await Promise.all(waiting);
        f();
      }
    });

    return {
      z: addN([qi, qj, qk].map((q) => q[2])),
      shapes: (
        <>
          <polygon
            points={vertices()
              .map((v) => toCanvas(v))
              .join(" ")}
            fill={fillColor}
            stroke={"#1b1f8a"}
            stroke-width={0.5}
          ></polygon>
          <polygon
            points={ss.map(([x, y]) => toCanvas([num(x), num(y)])).join(" ")}
            fill={"#0002"}
          ></polygon>
          {labels.map(([x0, y0], i) => {
            const [x, y] = toCanvas([num(x0), num(y0)]);
            return (
              <text
                x={x}
                y={y}
                fill-color="black"
                font-style="italic"
                font-weight="bold"
                font-family="Palatino"
                stroke="white"
                stroke-width={3}
                paint-order="stroke"
              >
                {names[i]}
              </text>
            );
          })}
        </>
      ),
    };
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
  const tri1 = triangleWithShadow(qs1, "#34379a", ["A", "B", "C"]);
  const tri2 = triangleWithShadow(qs2, "#340000", ["D", "E", "F"]);

  const swap = signalBool(lt(tri1.z, tri2.z));

  return (
    <svg version="1.2" xmlns="http://www.w3.org/2000/svg" width={w} height={h}>
      {plane}
      <For
        each={
          bool(swap) ? [tri2.shapes, tri1.shapes] : [tri1.shapes, tri2.shapes]
        }
      >
        {(s) => s}
      </For>
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
