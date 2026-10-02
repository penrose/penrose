/** @jsxImportSource @penrose/bloom */

import type { ProgramArgument, Proposition } from "../core/program.js";
import {
  polygonalPathAvoids,
  pointSetTopology as topology,
  type PlaneCoordinates,
} from "../domains/point-set-topology.js";
import { OscillatingSineGraphView } from "./oscillating-sine-curve.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { hatchedTopologyPolygon } from "./topology-bases.js";

type XY = [number, number];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const isProposition = (p: ProgramArgument): p is Proposition =>
  "predicate" in p;

/** A local interval witnesses the failure of a hypothesized separation/supremum. */
export function intervalConnectednessStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const assumptions = ctx.facts(topology.Hypothesis).map(([p]) => p);
    const separation = assumptions.find(
      (p) => p.predicate === topology.TopologicalSeparationOf,
    );
    const supremum = assumptions.find(
      (p) => p.predicate === topology.SupremumOf,
    );
    const interval =
      separation &&
      ctx
        .entities(topology.ClosedInterval)
        .find((i) => i === separation.args[0]);
    const a =
      supremum &&
      ctx.entities(topology.RealPoint).find((p) => p === supremum.args[0]);
    const tau =
      interval &&
      ctx.facts(topology.TopologyOn).find(([, i]) => i === interval)?.[0];
    if (
      !separation ||
      !supremum ||
      !interval ||
      !a ||
      !tau ||
      !ctx.test(topology.Connected, tau)
    )
      throw new Error(
        "Keep the assumed separation distinct from the connected interval",
      );
    const U = separation.args[1],
      V = separation.args[2];
    if (isProposition(U) || isProposition(V))
      throw new Error("The separation needs two sets");
    const N = ctx
      .entities(topology.OpenIntervalNeighborhood)
      .find((n) => ctx.test(topology.NeighborhoodOf, n, a));
    const middle =
      N &&
      ctx
        .entities(topology.ClosedInterval)
        .find((i) => i !== interval && ctx.test(topology.Subset, i, N));
    if (
      !N ||
      !middle ||
      !(
        N.a < middle.a &&
        middle.a < a.coordinate &&
        a.coordinate < middle.b &&
        middle.b < N.b
      )
    )
      throw new Error(
        "The witness must lie strictly inside the local open interval",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "11px",
    });
    const unit = 292;
    const at = (x: number, y = 0): XY => [
      unit * ((x - interval.a) / (interval.b - interval.a) - 0.5),
      y,
    ];
    const cut = 0.71;
    const Ux = at(cut)[0],
      left = at(interval.a)[0],
      right = at(interval.b)[0];
    const color = options.regionColor ?? [0.95, 0.41, 0.12, 0.14];
    hatchedTopologyPolygon(
      "connectedness.assumed-U",
      [
        [left, -3],
        [Ux - 3, -3],
        [Ux - 3, 3],
        [left, 3],
      ].map(([x, y]) => draw.xy([x, y])),
      -Math.PI / 4,
      color,
      { spacing: 2.8 },
    );
    hatchedTopologyPolygon(
      "connectedness.assumed-V",
      [
        [Ux + 3, -3],
        [right, -3],
        [right, 3],
        [Ux + 3, 3],
      ].map(([x, y]) => draw.xy([x, y])),
      Math.PI / 4,
      color,
      { spacing: 2.8 },
    );
    hatchedTopologyPolygon(
      "connectedness.closed-witness",
      [
        at(middle.a, -3),
        at(middle.b, -3),
        at(middle.b, 3),
        at(middle.a, 3),
      ].map(([x, y]) => draw.xy([x, y])),
      Math.PI / 4,
      [0.95, 0.41, 0.12, 0.28],
      { spacing: 1.8, strokeOpacity: 0.9 },
    );
    draw.line("connectedness.interval", [left, 0], [right, 0]);
    const parenthesis = (x: number, direction: -1 | 1, height = 6) =>
      draw.outline("connectedness.open-end", [
        ["M", x + direction * 3, height],
        [
          "C",
          x - direction * 4,
          height,
          x - direction * 4,
          -height,
          x + direction * 3,
          -height,
        ],
      ]);
    parenthesis(Ux - 3, -1);
    parenthesis(Ux + 3, 1);
    parenthesis(at(N.a)[0], 1);
    parenthesis(at(N.b)[0], -1);
    for (const x of [left, right, at(middle.a)[0], at(middle.b)[0]]) {
      draw.line("connectedness.closed-end", [x, -5], [x, 5]);
      const sign = x === right || x === at(middle.b)[0] ? -1 : 1;
      draw.line("connectedness.closed-end-top", [x, 5], [x + sign * 3, 5]);
      draw.line("connectedness.closed-end-bottom", [x, -5], [x + sign * 3, -5]);
    }
    draw.label(String(interval.a), [left - 6, 0]);
    draw.label(String(interval.b), [right + 6, 0]);
    draw.label(U.label, [left + 68, -13]);
    draw.label(V.label, [right - 38, 13]);
    draw.label(a.label, at(a.coordinate, -11));
    draw.label(middle.endpointNames?.[0] ?? String(middle.a), at(middle.a, 16));
    draw.label(middle.endpointNames?.[1] ?? String(middle.b), at(middle.b, 16));
  });
}

/** Shared segment/vertex rendering handles a punctured plane and an enclosing region. */
export function polygonalConnectednessStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const routes = ctx.entities(topology.PolygonalPath);
    if (routes.length !== 1)
      throw new Error("A polygonal view needs one ordered route");
    const route = routes[0];
    const relation = ctx
      .facts(topology.PolygonalPathBetween)
      .find(([p]) => p === route);
    if (
      !relation ||
      route.vertices.length < 2 ||
      route.vertices.some((p) => !p.every(Number.isFinite))
    )
      throw new Error(
        "The polygonal route needs endpoints and a containing subspace",
      );
    const vertices = route.vertices.map((coordinate) =>
      ctx
        .entities(topology.CoordinatePoint)
        .find(
          (p) =>
            p.coordinates.every((v, i) => v === coordinate[i]) &&
            ctx.test(topology.VertexOfPath, p, route),
        ),
    );
    if (
      vertices.some((p) => !p) ||
      vertices[0] !== relation[1] ||
      vertices.at(-1) !== relation[2]
    )
      throw new Error(
        "Every ordered vertex must preserve its incidence and endpoints",
      );
    for (let i = 1; i < vertices.length; i++) {
      const segment = ctx
        .facts(topology.SegmentBetween)
        .find(([, a, b]) => a === vertices[i - 1] && b === vertices[i])?.[0];
      if (
        !segment ||
        !ctx.test(topology.SegmentOfPath, segment, route) ||
        !ctx.test(topology.Subset, segment, relation[3])
      )
        throw new Error(
          "Every consecutive closed segment must lie in the containing subspace",
        );
    }
    const deleted = ctx
      .facts(topology.DeletedPointFrom)
      .find(([space]) => space === relation[3]);
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "12px",
    });
    let toWorld: (p: PlaneCoordinates) => XY;
    if (deleted) {
      if (!polygonalPathAvoids(route.vertices, deleted[2].coordinates))
        throw new Error("No segment may pass through the removed point");
      toWorld = ([x, y]) => [-26 + 82 * x, -23 + 82 * y];
      draw.line("polygonal.x-axis", [-119, -23], [123, -23]);
      draw.line("polygonal.y-axis", [-26, -87], [-26, 52]);
      draw.label("x", [129, -23]);
      draw.label("y", [-26, 58]);
      draw.dot("polygonal.removed-point", toWorld(deleted[2].coordinates));
      draw.label(deleted[2].label, [
        toWorld(deleted[2].coordinates)[0] + 10,
        toWorld(deleted[2].coordinates)[1],
      ]);
    } else {
      toWorld = ([x, y]) => [100 * x, 100 * y];
      <path
        d={draw.data([
          ["M", -74, 74],
          ["C", -92, 73, -119, 47, -115, 28],
          ["C", -113, 11, -96, 8, -105, -23],
          ["C", -109, -55, -93, -82, -72, -72],
          ["C", -52, -61, -24, -50, 2, -43],
          ["C", 30, -36, 23, -12, 20, 5],
          ["C", 14, 28, 8, 62, 20, 65],
          ["C", 30, 66, 37, 38, 41, 19],
          ["C", 48, -18, 57, -43, 75, -30],
          ["C", 93, -23, 121, -28, 119, -3],
          ["C", 117, 7, 104, 0, 101, 14],
          ["C", 99, 32, 99, 53, 79, 72],
          ["C", 63, 88, 23, 108, -1, 82],
          ["C", -17, 66, -11, 48, -10, 23],
          ["C", -6, -9, -23, -31, -46, -22],
          ["C", -72, -13, -53, 40, -57, 58],
          ["C", -59, 74, -67, 80, -74, 74],
          ["Z"],
        ])}
        fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.08]}
        stroke-color={INK}
        stroke-width={1}
        aria-label="polygonal enclosing region W"
      />;
      draw.label(relation[3].label, [12, -50]);
    }
    for (let i = 1; i < vertices.length; i++)
      draw.line(
        "polygonal.segment-" + i,
        toWorld(route.vertices[i - 1]),
        toWorld(route.vertices[i]),
      );
    for (const [i, point] of vertices.entries()) {
      const p = point!;
      const at = toWorld(p.coordinates);
      draw.dot("polygonal.vertex-" + i, at);
      if (!p.label) continue;
      if (!deleted && (i === 0 || i === vertices.length - 1)) {
        const direction = i === 0 ? 1 : -1;
        draw.line(
          "polygonal.endpoint-leader",
          [at[0], at[1] + 8],
          [at[0], at[1] + 20],
        );
        <polygon
          points={[
            draw.xy([at[0], at[1] + 4]),
            draw.xy([at[0] - 1.8, at[1] + 8]),
            draw.xy([at[0] + 1.8, at[1] + 8]),
          ]}
          fill-color={INK}
          stroke-width={0}
        />;
        draw.label(p.label, [at[0] + direction * 2, at[1] + 29]);
      } else {
        const offset: XY = deleted
          ? i === 1
            ? [8, 9]
            : [9, -3]
          : i === 1
          ? [-10, 0]
          : i === 3
          ? [3, 10]
          : [11, 0];
        draw.label(p.label, [at[0] + offset[0], at[1] + offset[1]]);
      }
    }
  });
}

/** The origin-adjoined analytic sine graph preserves connectedness without path connectedness. */
export function sineConnectednessStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const Y = ctx.entities(topology.OriginAdjoinedSineCurve)[0];
    const image =
      Y && ctx.facts(topology.SineCurveImageOf).find(([curve]) => curve === Y);
    const tau =
      Y && ctx.facts(topology.TopologyOn).find(([, space]) => space === Y)?.[0];
    if (
      !Y ||
      !image ||
      !tau ||
      !ctx.test(topology.Connected, tau) ||
      !ctx.test(topology.NotPathConnected, tau) ||
      image[1].frequency !== Y.frequency ||
      image[1].amplitude !== Y.amplitude ||
      image[2].coordinates.some((v) => v !== 0) ||
      !ctx.test(topology.Member, image[2], Y)
    )
      throw new Error(
        "Keep the positive sine graph and its included origin distinct",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "12px",
    });
    const origin: XY = [-88, 0];
    const xMax = (Y.frequency * 2) / (3 * Math.PI);
    draw.line("sine-connectedness.x-axis", [-99, 0], [103, 0]);
    draw.line("sine-connectedness.y-axis", [-88, -69], [-88, 77]);
    draw.line("sine-connectedness.upper-guide", [-88, 54], [103, 54]);
    draw.line("sine-connectedness.lower-guide", [-88, -54], [103, -54]);
    // The analytic window already bounds the fixed curve within the chart.
    <OscillatingSineGraphView
      data={Y}
      origin={origin}
      units={[190 / xMax, 54 / Y.amplitude]}
      xMin={Y.frequency / 500}
      xMax={xMax}
      phaseStep={Math.PI / 12}
      style={options}
      name="sine-connectedness.positive-graph"
      ensureOnCanvas={false}
    />;
    draw.dot("sine-connectedness.included-origin", origin);
    draw.label(image[2].label, [-107, -1]);
    draw.label("x", [109, 0]);
    draw.label("y", [-88, 83]);
  });
}
