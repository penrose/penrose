/** @jsxImportSource @penrose/bloom */
// Gallery translation of: graph-domain/other-examples/arpanet

import type { Diagram, Shape, Substance } from "@penrose/bloom";
import {
  DiagramBuilder,
  add,
  canvas,
  constraints,
  div,
  objectives,
  ops,
  shapeDistance,
} from "@penrose/bloom";

import { rgba } from "../../utils.js";

const SEED = "CascaraKudu41689";
const edgeDist = 100;
const repelDist = 150;

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const {
    build,
    ensure,
    encourage,
    layer,
    type,
    predicate,
    forall,
    forallWhere,
  } = new DiagramBuilder(canvas(600, 600), variation);

  const Vertex = type();
  const Edge = predicate();

  const labels = [
    "UCLA",
    "SRI",
    "UCSB",
    "Utah",
    "BBN",
    "MIT",
    "RAND",
    "SDC",
    "HARV",
    "LL",
    "CWRU",
    "CMU",
    "NASA",
  ] as const;

  const vertices = Object.fromEntries(
    labels.map((label) => {
      const vertex = Vertex();
      vertex.label = label;
      return [label, vertex];
    }),
  ) as Record<(typeof labels)[number], ReturnType<typeof Vertex>>;

  const edges = [
    ["UCLA", "SRI"],
    ["UCLA", "UCSB"],
    ["SRI", "UCSB"],
    ["SRI", "Utah"],
    ["UCSB", "Utah"],
    ["SRI", "BBN"],
    ["BBN", "MIT"],
    ["BBN", "RAND"],
    ["RAND", "SDC"],
    ["SDC", "UCSB"],
    ["BBN", "HARV"],
    ["HARV", "MIT"],
    ["MIT", "LL"],
    ["LL", "NASA"],
    ["NASA", "UCSB"],
    ["BBN", "CWRU"],
    ["CWRU", "CMU"],
  ] as const satisfies readonly (readonly [
    keyof typeof vertices,
    keyof typeof vertices,
  ])[];

  const edgeShapes = new Map<string, Shape>();
  const edgeKey = (u: Substance, v: Substance): string =>
    `${u.label}->${v.label}`;

  for (const [from, to] of edges) {
    Edge(vertices[from], vertices[to]);
  }

  const background = (
    <rect
      center={[0, 0]}
      width={600}
      height={600}
      fill-color={rgba(15, 37, 87, 1)}
    />
  );

  forall({ v: Vertex }, ({ v }) => {
    v.text = (
      <text
        string={v.label}
        fill-color={rgba(255, 255, 255, 1)}
        font-family={"Palatino"}
        font-size={"18px"}
        stroke-color={rgba(40, 85, 154, 1)}
        stroke-width={4}
        paint-order={"stroke"}
      />
    );
    v.dot2 = (
      <circle
        center={v.text.center}
        r={add(div(v.text.width, 2), 4)}
        fill-color={rgba(40, 85, 154, 1)}
        stroke-color={rgba(0, 0, 0, 0)}
      />
    );
    v.dot = (
      <circle
        center={v.text.center}
        r={add(div(v.text.width, 2), 4)}
        fill-color={rgba(0, 0, 0, 0)}
        stroke-color={rgba(255, 255, 255, 1)}
        stroke-width={2}
      />
    );

    layer(background, v.dot2);
    layer(v.dot2, v.text);
    layer(v.text, v.dot);
  });

  forallWhere(
    { u: Vertex, v: Vertex },
    ({ u, v }) => Edge.test(u, v),
    ({ u, v }) => {
      const edge = (
        <line
          start={u.dot.center}
          end={v.dot.center}
          stroke-color={rgba(255, 255, 255, 1)}
          stroke-width={2}
        />
      );

      edgeShapes.set(edgeKey(u, v), edge);

      encourage(
        objectives.lessThan(
          ops.vnorm(ops.vsub(u.dot.center, v.dot.center)),
          edgeDist,
        ),
      );
      encourage(objectives.greaterThan(shapeDistance(u.dot, v.dot), 0));
    },
  );

  forallWhere(
    { u: Vertex, v: Vertex },
    ({ u, v }) => Edge.test(u, v),
    ({ u, v }) => {
      const edge = edgeShapes.get(edgeKey(u, v));
      if (!edge) {
        return;
      }

      layer(background, edge);
      layer(edge, u.dot2);
      layer(edge, u.dot);
      layer(edge, v.dot2);
      layer(edge, v.dot);
    },
  );

  forallWhere(
    { u: Vertex, v: Vertex, w: Vertex },
    ({ u, v }) => Edge.test(u, v),
    ({ u, v, w }) => {
      const edge = edgeShapes.get(edgeKey(u, v));
      if (!edge) {
        return;
      }

      layer(edge, w.dot2);
    },
  );

  forall({ u: Vertex, v: Vertex }, ({ u, v }) => {
    if (u === v) {
      return;
    }

    encourage(objectives.notTooClose(u.dot, v.dot, repelDist));
    ensure(constraints.disjoint(u.text, v.text, 5));
  });

  return await build();
};

export default async (variation = SEED): Promise<string> => {
  const diagram = await buildDiagram(variation);
  const { svg } = await diagram.render();
  diagram.discard();
  return svg.outerHTML;
};
