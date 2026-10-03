import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  polygonalPathAvoids,
  pointSetTopology as topology,
  type PlaneCoordinates,
} from "../domains/point-set-topology.js";
import {
  intervalConnectednessStyle,
  polygonalConnectednessStyle,
  sineConnectednessStyle,
} from "../styles/connectedness.js";

/** Figure 9.1 depicts one branch of a contradiction proof, not an actual separation of I. */
export function intervalConnectednessSubstance(a = 0.5, rho = 0.14) {
  if (![a, rho].every(Number.isFinite) || !(0 < rho && rho < a && a + rho < 1))
    throw new Error("The local interval must lie strictly inside [0,1]");
  const s = topology.substance();
  const I = s.ClosedInterval({
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
    label: "[0,1]",
  });
  const tau = s.Topology({ label: "\\tau_I" });
  s.TopologyOn(tau, I);
  s.Connected(tau);
  const U = s.Set({ label: "U" }),
    V = s.Set({ label: "V" }),
    S = s.Set({ label: "S" });
  const u = s.RealPoint({ coordinate: a / 2, label: "u" });
  const supremum = s.RealPoint({ coordinate: a, label: "a" });
  s.Member(u, I);
  s.Member(supremum, I);
  s.InitialSegmentsIn(S, I, u, U);
  s.Hypothesis(s.TopologicalSeparationOf.expression(I, U, V, tau));
  s.Hypothesis(s.Member.expression(u, U));
  s.Hypothesis(s.SupremumOf.expression(supremum, S));
  s.Hypothesis(s.Member.expression(supremum, U));
  const N = s.OpenIntervalNeighborhood({
    a: a - rho,
    b: a + rho,
    leftClosed: false,
    rightClosed: false,
    label: "(a-\\rho,a+\\rho)",
  });
  const middle = s.ClosedInterval({
    a: a - rho / 2,
    b: a + rho / 2,
    leftClosed: true,
    rightClosed: true,
    label: "[a-\\rho/2,a+\\rho/2]",
    endpointNames: ["a-\\rho/2", "a+\\rho/2"],
  });
  s.NeighborhoodOf(N, supremum);
  s.Member(supremum, N);
  s.OpenIn(N, tau);
  s.Subset(N, I);
  s.Subset(middle, N);
  s.Hypothesis(s.Subset.expression(N, U));
  const next = s.RealPoint({ coordinate: a + rho / 2, label: "a+\\rho/2" });
  s.Member(next, middle);
  s.Hypothesis(s.Member.expression(next, S));
  return s.make();
}

/** Two closed segments connect Q to Q′ while avoiding the removed point P. */
export function puncturedPlanePathSubstance(
  points: readonly [
    PlaneCoordinates,
    PlaneCoordinates,
    PlaneCoordinates,
    PlaneCoordinates,
  ] = [
    [0.3, 0.24],
    [0.64, 0.85],
    [1.15, 0.6],
    [0.72, 0.35],
  ],
) {
  const [q, via, target, removed] = points;
  if (!polygonalPathAvoids([q, via, target], removed))
    throw new Error("The polygonal route must avoid the removed point");
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" });
  const Y = s.PuncturedPlane({ label: "R^2-\\{P\\}" });
  const tau = s.Topology({ label: "\\tau_D" }),
    relative = s.Topology({ label: "\\tau_Y" });
  const Q = s.CoordinatePoint({ label: "Q", coordinates: q });
  const Q2 = s.CoordinatePoint({ label: "Q''", coordinates: via });
  const Q1 = s.CoordinatePoint({ label: "Q'", coordinates: target });
  const P = s.CoordinatePoint({ label: "P", coordinates: removed });
  s.TopologyOn(tau, X);
  s.TopologyOn(relative, Y);
  s.Subset(Y, X);
  s.SubspaceTopologyOf(relative, Y, tau);
  s.DeletedPointFrom(Y, X, P);
  s.Member(P, X);
  s.Outside(P, Y);
  s.Connected(tau);
  s.Connected(relative);
  s.PathConnected(relative);
  s.PolygonallyConnected(relative);
  const route = s.PolygonalPath({
    vertices: [q, via, target],
    label: "QQ''\\cup Q''Q'",
  });
  const family = s.FiniteSetFamily();
  const first = s.LinearSegment({ endpoints: [q, via], label: "QQ''" });
  const last = s.LinearSegment({ endpoints: [via, target], label: "Q''Q'" });
  s.SegmentBetween(first, Q, Q2);
  s.SegmentBetween(last, Q2, Q1);
  for (const segment of [first, last]) {
    s.SegmentOfPath(segment, route);
    s.SetInFamily(segment, family);
    s.Subset(segment, Y);
    s.ConnectedIn(segment, relative);
  }
  s.UnionOf(route, family);
  s.ConnectedIn(route, relative);
  s.Subset(route, Y);
  s.PolygonalPathBetween(route, Q, Q1, Y);
  for (const p of [Q, Q2, Q1]) {
    s.Member(p, X);
    s.Member(p, Y);
    s.Member(p, route);
    s.VertexOfPath(p, route);
  }
  return s.make();
}

export const defaultRegionRoute: readonly PlaneCoordinates[] = [
  [-0.72, 0.56],
  [-0.69, -0.58],
  [0.02, -0.16],
  [-0.03, 0.71],
  [0.38, 0.64],
  [0.56, -0.07],
  [0.94, -0.03],
];

/** A finite sequence witnesses a polygonal path inside a subspace W. */
export function polygonalRegionPathSubstance(vertices = defaultRegionRoute) {
  if (vertices.length < 2 || vertices.some((p) => !p.every(Number.isFinite)))
    throw new Error("The polygonal path must have finite ordered vertices");
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" }),
    W = s.Subspace({ label: "W" });
  const tau = s.Topology({ label: "\\tau_D" }),
    relative = s.Topology({ label: "\\tau_W" });
  const route = s.PolygonalPath({
    vertices,
    label: "x_0x_1\\cup\\cdots\\cup x_{n-1}x_n",
  });
  s.TopologyOn(tau, X);
  s.TopologyOn(relative, W);
  s.Subset(W, X);
  s.SubspaceTopologyOf(relative, W, tau);
  const points = vertices.map((coordinates, i) =>
    s.CoordinatePoint({
      coordinates,
      label:
        i === 0
          ? "x_0=x"
          : i === vertices.length - 1
          ? "x_n=y"
          : i <= 3
          ? "x_" + i
          : "",
    }),
  );
  const family = s.FiniteSetFamily();
  for (const [i, p] of points.entries()) {
    s.Member(p, W);
    s.Member(p, route);
    s.VertexOfPath(p, route);
    if (i > 0) {
      const segment = s.LinearSegment({
        endpoints: [vertices[i - 1], vertices[i]],
        label: "x_" + (i - 1) + "x_" + i,
      });
      s.SegmentBetween(segment, points[i - 1], p);
      s.SegmentOfPath(segment, route);
      s.SetInFamily(segment, family);
      s.Subset(segment, W);
      s.ConnectedIn(segment, relative);
    }
  }
  s.UnionOf(route, family);
  s.Subset(route, W);
  s.ConnectedIn(route, relative);
  s.PolygonalPathBetween(route, points[0], points[points.length - 1], W);
  return s.make();
}

/** The origin-adjoined graph is connected but has no path from its origin to the positive branch. */
export function connectedSineCurveSubstance(frequency = 1, amplitude = 1) {
  if (
    ![frequency, amplitude].every(Number.isFinite) ||
    !(frequency > 0 && amplitude > 0)
  )
    throw new Error(
      "The oscillating curve parameters must be finite and positive",
    );
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" });
  const graph = s.OscillatingSineGraph({
    label:
      "y=" +
      (amplitude === 1 ? "" : String(amplitude)) +
      "\\sin(" +
      String(frequency) +
      "/x), x>0",
    frequency,
    amplitude,
  });
  const Y = s.OriginAdjoinedSineCurve({ label: "Y", frequency, amplitude });
  const origin = s.CoordinatePoint({ label: "(0,0)", coordinates: [0, 0] });
  const tau = s.Topology({ label: "\\tau_D" }),
    relative = s.Topology({ label: "\\tau_Y" });
  s.TopologyOn(tau, X);
  s.TopologyOn(relative, Y);
  s.Subset(Y, X);
  s.Subset(graph, Y);
  s.Member(origin, Y);
  s.Member(origin, X);
  s.SineCurveImageOf(Y, graph, origin);
  s.SubspaceTopologyOf(relative, Y, tau);
  s.ConnectedIn(graph, tau);
  s.Connected(relative);
  s.NotPathConnected(relative);
  return s.make();
}

export const buildIntervalConnectednessFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: intervalConnectednessSubstance(),
    sty: intervalConnectednessStyle(),
    canvas: canvas(326, 52),
    variation: "gemignani-9.1",
    ...renderOptions,
  });
export const buildPuncturedPlanePathFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: puncturedPlanePathSubstance(),
    sty: polygonalConnectednessStyle({ offset: [0, 10] }),
    canvas: canvas(270, 166),
    variation: "gemignani-9.2",
    ...renderOptions,
  });
export const buildPolygonalRegionPathFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: polygonalRegionPathSubstance(),
    sty: polygonalConnectednessStyle({ offset: [0, -9] }),
    canvas: canvas(252, 186),
    variation: "gemignani-9.3",
    ...renderOptions,
  });
export const buildConnectedSineCurveFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: connectedSineCurveSubstance(),
    sty: sineConnectednessStyle(),
    canvas: canvas(264, 182),
    variation: "gemignani-9.4",
    ...renderOptions,
  });
