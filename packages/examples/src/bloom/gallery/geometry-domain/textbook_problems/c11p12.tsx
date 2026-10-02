/** @jsxImportSource @penrose/bloom */
// Gallery translation of: geometry-domain/textbook_problems/c11p12

import type { Circle, Diagram } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  constraints,
  objectives,
  ops,
  signedDistance,
  signedDistanceCircle,
} from "@penrose/bloom";

import { rgba } from "../../utils.js";

const SEED = "EdwardGuanaco94367";

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build, ensure, encourage, layer } = new DiagramBuilder(
    canvas(500, 500),
    variation,
  );

  const pointColor = rgba(66, 86, 100, 1);
  const background = (
    <rect
      center={[0, 0]}
      width={500}
      height={500}
      fill-color={rgba(246, 244, 242, 1)}
    />
  );

  const c = {
    icon: (
      <circle
        fill-color={rgba(0, 0, 0, 0)}
        stroke-color={pointColor}
        stroke-width={1.75}
      />
    ) as Circle,
  };

  const mkPoint = (label: string) => {
    const dot = (
      <circle
        r={4}
        fill-color={pointColor}
        stroke-color={pointColor}
        stroke-width={0}
      />
    ) as Circle;
    const text = (
      <equation
        string={label}
        font-size={"20px"}
        fill-color={pointColor}
        ensure-on-canvas
      />
    );
    ensure(constraints.equal(signedDistance(text, dot.center), 14));
    layer(background, dot);
    layer(background, text);
    layer(dot, text);
    return { dot, text };
  };

  const A = mkPoint("A");
  const B = mkPoint("B");
  const C = mkPoint("C");
  const D = mkPoint("D");
  const E = mkPoint("E");
  const F = {
    dot: (
      <circle
        center={c.icon.center}
        r={4}
        fill-color={pointColor}
        stroke-color={pointColor}
        stroke-width={0}
      />
    ) as Circle,
    text: (
      <equation
        string={"F"}
        font-size={"20px"}
        fill-color={pointColor}
        ensure-on-canvas
      />
    ),
  };
  ensure(constraints.equal(signedDistance(F.text, F.dot.center), 14));
  layer(F.dot, F.text);

  const b = mkPoint("b");
  const d = mkPoint("d");

  for (const point of [A, B, C, D, E]) {
    ensure(
      constraints.equal(
        signedDistanceCircle(c.icon.center, c.icon.r, point.dot.center),
        0,
      ),
    );
  }

  ensure(constraints.collinear(E.dot.center, F.dot.center, A.dot.center));
  ensure(constraints.collinear(F.dot.center, b.dot.center, B.dot.center));
  ensure(constraints.collinear(F.dot.center, d.dot.center, D.dot.center));

  const midEC = ops.vmul(0.5, ops.vadd(E.dot.center, C.dot.center));
  const midAC = ops.vmul(0.5, ops.vadd(A.dot.center, C.dot.center));

  ensure(constraints.collinear(F.dot.center, d.dot.center, midEC));
  ensure(constraints.collinear(F.dot.center, b.dot.center, midAC));
  ensure(constraints.perpendicular(E.dot.center, midEC, d.dot.center));
  ensure(constraints.perpendicular(A.dot.center, midAC, b.dot.center));

  for (const [start, end, dashed] of [
    [A.dot.center, C.dot.center, false],
    [E.dot.center, C.dot.center, false],
    [F.dot.center, B.dot.center, false],
    [F.dot.center, D.dot.center, false],
    [F.dot.center, C.dot.center, false],
    [F.dot.center, d.dot.center, false],
    [F.dot.center, b.dot.center, false],
    [E.dot.center, A.dot.center, false],
  ] as const) {
    const line = (
      <line
        start={start}
        end={end}
        stroke-color={pointColor}
        stroke-width={1.75}
        stroke-dasharray={dashed ? "5 5" : ""}
      />
    );
    for (const point of [
      A.dot,
      B.dot,
      C.dot,
      D.dot,
      E.dot,
      F.dot,
      b.dot,
      d.dot,
    ]) {
      layer(line, point);
    }
  }

  for (const p of [A.dot, B.dot, C.dot, D.dot, E.dot, F.dot, b.dot, d.dot]) {
    encourage(objectives.notTooClose(p, F.dot, 1));
  }

  return await build();
};
