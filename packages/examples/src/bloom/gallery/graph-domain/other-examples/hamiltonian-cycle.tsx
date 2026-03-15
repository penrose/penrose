/** @jsxImportSource @penrose/bloom */
// Gallery translation of: graph-domain/other-examples/hamiltonian-cycle

import type { Diagram, Vec2 } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  constraints,
  mul,
  normalize,
  objectives,
  ops,
  quadraticCurveFromPoints,
  rot90,
} from "@penrose/bloom";

import { rgba } from "../../utils.js";

const SEED = "PredictableWasp290";
const edgeDist = 100;
const repelDist = 150;

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const db = new DiagramBuilder(canvas(400, 400), variation);
  const { build, ensure, encourage, input, layer, type, forall } = db;

  const Vertex = type();

  const vertices = {
    a: Vertex(),
    b: Vertex(),
    c: Vertex(),
    d: Vertex(),
    e: Vertex(),
    f: Vertex(),
    g: Vertex(),
  } as const;

  for (const [label, vertex] of Object.entries(vertices)) {
    vertex.label = label;
  }

  const cycleEdges = [
    ["a", "b"],
    ["b", "c"],
    ["c", "d"],
    ["d", "e"],
    ["e", "f"],
    ["f", "g"],
    ["g", "a"],
  ] as const;

  const extraEdges = [
    ["a", "c"],
    ["a", "d"],
    ["b", "e"],
    ["b", "f"],
    ["c", "g"],
    ["d", "f"],
  ] as const;

  forall({ v: Vertex }, ({ v }) => {
    const highlighted = v === vertices.a;
    v.dot = (
      <circle
        r={5}
        fill-color={highlighted ? rgba(254, 74, 73, 1) : rgba(0, 0, 0, 1)}
      />
    );
    v.text = (
      <text
        string={v.label}
        fill-color={highlighted ? rgba(254, 74, 73, 1) : rgba(0, 0, 0, 1)}
        font-family={"serif"}
        font-size={"18px"}
        stroke-color={rgba(255, 255, 255, 1)}
        center={ops.vadd(v.dot.center, [10, 10])}
        stroke-width={4}
        paint-order="stroke"
      />
    );

    encourage(objectives.above(v.text, v.dot, 5));
    layer(v.dot, v.text);
  });

  forall({ u: Vertex, v: Vertex }, ({ u, v }) => {
    encourage(objectives.notTooClose(u.dot, v.dot, repelDist));
    ensure(constraints.disjoint(u.text, v.text, 5));
  });

  const addEdge = (
    from: keyof typeof vertices,
    to: keyof typeof vertices,
    highlighted: boolean,
  ) => {
    const u = vertices[from];
    const v = vertices[to];
    const tangent = normalize(ops.vsub(v.dot.center, u.dot.center));
    const normal = rot90(tangent as Vec2);
    const midpoint = ops.vmul(0.5, ops.vadd(u.dot.center, v.dot.center));
    const offset = input({ init: 0 });

    const edge = (
      <path
        d={quadraticCurveFromPoints("open", [
          u.dot.center,
          ops.vadd(midpoint, ops.vmul(offset, normal)),
          v.dot.center,
        ])}
        stroke-color={highlighted ? rgba(254, 74, 73, 1) : rgba(0, 0, 0, 1)}
        end-arrowhead={"straight"}
      />
    );

    encourage(
      objectives.lessThan(
        ops.vnorm(ops.vsub(u.dot.center, v.dot.center)),
        edgeDist,
      ),
    );
    encourage(objectives.minimal(mul(offset, offset)));
    layer(edge, u.dot);
    layer(edge, v.dot);
  };

  cycleEdges.forEach(([from, to]) => addEdge(from, to, true));
  extraEdges.forEach(([from, to]) => addEdge(from, to, false));

  return await build();
};
