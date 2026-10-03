import type { FigureRenderOptions } from "@penrose/bloom";
import {
  accumulatingComponentsStyle,
  canvas,
  connectedProductSlicesStyle,
  convexBoxStyle,
  diagram,
  directedIntersectionStyle,
  mutuallySeparatedDisksStyle,
  polygonalReachabilityStyle,
  reciprocalCircleRadius,
  spiralClosureStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** Two arbitrary interior points of a basic three-dimensional product neighborhood. */
export function convexBoxSegment() {
  const sub = topology.substance();
  const space = sub.Set({ label: "\\mathbb R^3" }),
    box = sub.EuclideanOpenBox({
      label: "V",
      bounds: [
        [0, 1],
        [0, 1],
        [0, 1],
      ],
    });
  const x = sub.EuclideanVector({ label: "x", coordinates: [0.19, 0.5, 0.49] }),
    y = sub.EuclideanVector({ label: "y", coordinates: [0.85, 0.5, 0.41] });
  const segment = sub.EuclideanLineSegment({
    label: "\\overline{xy}",
    endpoints: [x.coordinates, y.coordinates],
  });
  sub.Convex(box);
  sub.Subset(box, space);
  sub.Subset(segment, box);
  sub.Member(x, box);
  sub.Member(y, box);
  sub.EuclideanSegmentBetween(segment, x, y);
  return sub.make();
}

/** A route to a extends to b inside a convex product neighborhood V contained in connected open U. */
export function polygonalReachability() {
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    region = sub.OpenSubspace({ label: "U" });
  const tauPlane = sub.Topology({ label: "\\tau_D" }),
    tauU = sub.Topology({ label: "\\tau_U" });
  const vertices = [
    [1.85, 0.05],
    [1.14, -0.19],
    [0.95, 0.12],
    [0.48, 0.21],
    [0, 0],
  ] as const;
  const points = vertices.map((coordinates, i) =>
    sub.CoordinatePoint({
      label: i === 0 ? "u" : i === vertices.length - 1 ? "a" : `a_${i}`,
      coordinates,
    }),
  );
  const u = points[0],
    a = points[points.length - 1],
    b = sub.CoordinatePoint({ label: "b", coordinates: [-0.14, 0.08] });
  const reachable = sub.PolygonalReachabilityClass({ label: "A" });
  const box = sub.EuclideanOpenBox({
    label: "V",
    bounds: [
      [-0.2, 0.2],
      [-0.2, 0.2],
    ],
  });
  const original = sub.PolygonalPath({ label: "P(u,a)", vertices }),
    extended = sub.PolygonalPath({
      label: "P(u,b)",
      vertices: [...vertices, b.coordinates],
    });
  sub.TopologyOn(tauPlane, plane);
  sub.TopologyOn(tauU, region);
  sub.SubspaceTopologyOf(tauU, region, tauPlane);
  sub.OpenIn(region, tauPlane);
  sub.Subset(region, plane);
  sub.Connected(tauU);
  sub.PolygonallyConnected(tauU);
  sub.PolygonalReachableFrom(reachable, u, region);
  sub.Subset(reachable, region);
  sub.OpenIn(reachable, tauU);
  sub.Convex(box);
  sub.Subset(box, region);
  sub.OpenIn(box, tauPlane);
  sub.NeighborhoodOf(box, a);
  sub.PolygonalPathBetween(original, u, a, region);
  sub.PolygonalPathBetween(extended, u, b, region);
  sub.Subset(original, region);
  sub.Subset(extended, region);
  sub.ConnectedIn(original, tauPlane);
  sub.ConnectedIn(extended, tauPlane);
  const all = [...points, b];
  all.forEach((p, i) => {
    sub.Member(p, region);
    sub.Member(p, reachable);
    sub.VertexOfPath(p, extended);
    if (i < points.length) sub.VertexOfPath(p, original);
    if (i) {
      const part = sub.LinearSegment({
        label: `L_${i}`,
        endpoints: [all[i - 1].coordinates, p.coordinates],
      });
      sub.SegmentBetween(part, all[i - 1], p);
      sub.SegmentOfPath(part, extended);
      sub.Subset(part, region);
      if (i < points.length) sub.SegmentOfPath(part, original);
      else sub.Subset(part, box);
    }
  });
  sub.Member(a, box);
  sub.Member(b, box);
  return sub.make();
}

/** Open disks are mutually separated although their closures share the tangent point. */
export function mutuallySeparatedDisks() {
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    tau = sub.Topology({ label: "\\tau_D" });
  const s = sub.OpenDisk({ label: "S", center: [0, 0], radius: 1 }),
    t = sub.OpenDisk({ label: "T", center: [2, 0], radius: 1 });
  const closedS = sub.ClosedDisk({
      label: "\\operatorname{Cl}S",
      center: s.center,
      radius: 1,
    }),
    closedT = sub.ClosedDisk({
      label: "\\operatorname{Cl}T",
      center: t.center,
      radius: 1,
    });
  const contact = sub.CoordinatePoint({ label: "(1,0)", coordinates: [1, 0] }),
    meeting = sub.Singleton({ label: "\\{(1,0)\\}" });
  const union = sub.Subspace({ label: "S\\cup T" }),
    tauUnion = sub.Topology({ label: "\\tau_{S\\cup T}" }),
    pieces = sub.FiniteSetFamily({ label: "\\{S,T\\}" });
  sub.TopologyOn(tau, plane);
  sub.TopologyOn(tauUnion, union);
  sub.SubspaceTopologyOf(tauUnion, union, tau);
  sub.Subset(union, plane);
  sub.MutuallySeparated(s, t, tau);
  sub.Disjoint(s, t);
  sub.Disjoint(s, closedT);
  sub.Disjoint(closedS, t);
  sub.ClosureOf(closedS, s, tau);
  sub.ClosureOf(closedT, t, tau);
  sub.IntersectionOf(meeting, closedS, closedT);
  sub.SingletonOf(meeting, contact);
  sub.Member(contact, meeting);
  sub.Member(contact, closedS);
  sub.Member(contact, closedT);
  sub.Outside(contact, s);
  sub.Outside(contact, t);
  sub.SetInFamily(s, pieces);
  sub.SetInFamily(t, pieces);
  sub.UnionOf(union, pieces);
  sub.Disconnected(tauUnion);
  sub.ConnectedIn(s, tau);
  sub.ConnectedIn(t, tau);
  return sub.make();
}

/** The exact closure includes both the limiting circle and the omitted initial origin. */
export function reciprocalSpiralClosure() {
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    tau = sub.Topology({ label: "\\tau_D" });
  const curve = sub.ReciprocalPolarSpiral({
    label: "Y",
    limitRadius: 1,
    coefficient: 1,
  });
  const circle = sub.CircleBoundary({ label: "C", center: [0, 0], radius: 1 });
  const origin = sub.CoordinatePoint({ label: "0", coordinates: [0, 0] }),
    originSet = sub.Singleton({ label: "\\{0\\}" });
  const closure = sub.ClosedSubspace({ label: "\\operatorname{Cl}Y" }),
    parts = sub.FiniteSetFamily({ label: "\\{Y,C,\\{0\\}\\}" });
  const partial = sub.Subspace({ label: "Y\\cup C" }),
    partialParts = sub.FiniteSetFamily({ label: "\\{Y,C\\}" });
  const parameter = sub.HalfLine({
      label: "\\lambda>1",
      bound: 1,
      direction: "right",
    }),
    tauParameter = sub.Topology({ label: "\\tau_{(1,\\infty)}" }),
    tauCurve = sub.Topology({ label: "\\tau_Y" });
  const parametrization = sub.TopologicalMap({
    label: "g",
    formula: "g(\\lambda)=(1-1/\\lambda)(\\cos\\lambda,\\sin\\lambda)",
  });
  sub.TopologyOn(tau, plane);
  sub.TopologyOn(tauParameter, parameter);
  sub.TopologyOn(tauCurve, curve);
  sub.SubspaceTopologyOf(tauCurve, curve, tau);
  sub.Connected(tauParameter);
  sub.MapBetween(parametrization, parameter, curve);
  sub.ContinuousMap(parametrization, tauParameter, tauCurve);
  sub.Homeomorphism(parametrization, tauParameter, tauCurve);
  sub.ImageOf(curve, parametrization, parameter);
  sub.Connected(tauCurve);
  sub.ConnectedIn(curve, tau);
  sub.SpiralLimitCircleOf(circle, curve);
  sub.SpiralInitialLimitOf(origin, curve);
  sub.SingletonOf(originSet, origin);
  sub.Member(origin, originSet);
  sub.Outside(origin, curve);
  for (const part of [curve, circle, originSet]) {
    sub.SetInFamily(part, parts);
    sub.Subset(part, closure);
  }
  sub.UnionOf(closure, parts);
  sub.ClosureOf(closure, curve, tau);
  sub.Member(origin, closure);
  sub.ConnectedIn(closure, tau);
  sub.SetInFamily(curve, partialParts);
  sub.SetInFamily(circle, partialParts);
  sub.UnionOf(partial, partialParts);
  sub.Subset(curve, partial);
  sub.Subset(partial, closure);
  sub.ConnectedIn(partial, tau);
  return sub.make();
}

/** A two-factor coordinate chain under the hypothesized separation of a connected product. */
export function connectedProductSlices() {
  const sub = topology.substance();
  const real1 = sub.RealLine({ label: "X_1" }),
    real2 = sub.RealLine({ label: "X_2" }),
    plane = sub.ProductSet({ label: "X_1\\times X_2" });
  const t1 = sub.Topology({ label: "\\tau_1" }),
    t2 = sub.Topology({ label: "\\tau_2" }),
    tau = sub.Topology({ label: "\\tau_1\\times\\tau_2" });
  const u = sub.CoordinatePoint({
      label: "(u_1,u_2)",
      coordinates: [-0.7, 0.8],
    }),
    v = sub.CoordinatePoint({
      label: "(v_1,v_2)",
      coordinates: [1.075, -0.17],
    }),
    c = sub.CoordinatePoint({
      label: "(v_1,u_2)",
      coordinates: [v.coordinates[0], u.coordinates[1]],
    });
  const U = sub.OpenSet({ label: "U" }),
    V = sub.OpenSet({ label: "V" });
  const W = sub.EuclideanProductBox({
    label: "W_1\\times W_2",
    bounds: [
      [-0.85, -0.55],
      [0.65, 0.95],
    ],
  });
  const w1 = sub.OpenInterval({
      label: "W_1",
      a: -0.85,
      b: -0.55,
      leftClosed: false,
      rightClosed: false,
    }),
    w2 = sub.OpenInterval({
      label: "W_2",
      a: 0.65,
      b: 0.95,
      leftClosed: false,
      rightClosed: false,
    });
  const a1 = sub.AffineProductSubspace({
      label: "A_1",
      coefficients: [0, 1, u.coordinates[1]],
    }),
    a2 = sub.AffineProductSubspace({
      label: "A_2",
      coefficients: [1, 0, v.coordinates[0]],
    });
  const pu2 = sub.RealPoint({ label: "u_2", coordinate: u.coordinates[1] }),
    pv1 = sub.RealPoint({ label: "v_1", coordinate: v.coordinates[0] }),
    fixed2 = sub.Singleton({ label: "\\{u_2\\}" }),
    fixed1 = sub.Singleton({ label: "\\{v_1\\}" });
  sub.TopologyOn(t1, real1);
  sub.TopologyOn(t2, real2);
  sub.TopologyOn(tau, plane);
  sub.ProductOf(plane, real1, real2);
  sub.ProductTopologyOf(tau, t1, t2);
  sub.Connected(t1);
  sub.Connected(t2);
  sub.Connected(tau);
  sub.Hypothesis(sub.TopologicalSeparationOf.expression(plane, U, V, tau));
  sub.Member(u, U);
  sub.Member(v, V);
  sub.Member(u, plane);
  sub.Member(v, plane);
  sub.Member(c, plane);
  sub.NeighborhoodOf(W, u);
  sub.Member(u, W);
  sub.Subset(W, U);
  sub.ProductOf(W, w1, w2);
  sub.OpenIn(W, tau);
  sub.SingletonOf(fixed2, pu2);
  sub.SingletonOf(fixed1, pv1);
  sub.ProductOf(a1, real1, fixed2);
  sub.ProductOf(a2, fixed1, real2);
  sub.Member(u, a1);
  sub.Member(c, a1);
  sub.Member(c, a2);
  sub.Member(v, a2);
  sub.ConnectedIn(a1, tau);
  sub.ConnectedIn(a2, tau);
  return sub.make();
}

/** All C_n and both horizontal lines are components, but the line points cannot be split. */
export function accumulatingCircleComponents() {
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    space = sub.Subspace({ label: "Y" }),
    tau = sub.Topology({ label: "\\tau_Y" }),
    tauPlane = sub.Topology({ label: "\\tau_D" });
  const family = sub.ReciprocalRadiusCircleFamily({
      label: "\\{C_n:n\\ge1\\}",
      limitRadius: 1,
    }),
    circleUnion = sub.Set({ label: "\\bigcup_n C_n" });
  const upper = sub.AffineSubspace({ label: "y=1", coefficients: [0, 1, 1] }),
    lower = sub.AffineSubspace({ label: "y=-1", coefficients: [0, 1, -1] }),
    parts = sub.FiniteSetFamily({ label: "\\{\\bigcup C_n,L_+,L_-\\}" });
  const up = sub.CoordinatePoint({ label: "(0,1)", coordinates: [0, 1] }),
    down = sub.CoordinatePoint({ label: "(0,-1)", coordinates: [0, -1] });
  sub.TopologyOn(tauPlane, plane);
  sub.TopologyOn(tau, space);
  sub.SubspaceTopologyOf(tau, space, tauPlane);
  sub.Subset(space, plane);
  sub.UnionOf(circleUnion, family);
  for (const s of [circleUnion, upper, lower]) sub.SetInFamily(s, parts);
  sub.UnionOf(space, parts);
  sub.ComponentFamilyOf(family, space, tau);
  sub.ComponentOf(upper, space, tau);
  sub.ComponentOf(lower, space, tau);
  sub.Disconnected(tau);
  sub.NotCompact(tau);
  sub.Member(up, upper);
  sub.Member(down, lower);
  sub.Member(up, space);
  sub.Member(down, space);
  sub.CannotSplitBetween(space, up, down, tau);
  for (const n of [1, 2, 3, 4, 5, 6, 8, 12, 20]) {
    const index = sub.RealPoint({ label: String(n), coordinate: n }),
      circle = sub.CircleBoundary({
        label: `C_${n}`,
        center: [0, 0],
        radius: reciprocalCircleRadius(n),
      });
    sub.CircleIndexedIn(circle, index, family);
    sub.SetInFamily(circle, family);
    sub.ComponentOf(circle, space, tau);
    sub.Subset(circle, space);
    sub.Disjoint(circle, upper);
    sub.Disjoint(circle, lower);
  }
  const outer = sub.CircleBoundary({
      label: "C_\\infty",
      center: [0, 0],
      radius: 1,
    }),
    side = sub.CoordinatePoint({ label: "(1,0)", coordinates: [1, 0] });
  sub.Member(side, outer);
  sub.Outside(side, space);
  sub.Member(up, outer);
  sub.Member(down, outer);
  return sub.make();
}

/** The proposed split of B=intersection A_i is a contradiction hypothesis, not a true partition. */
export function directedClosedIntersection() {
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const family = sub.ClosedDirectedFamily({
      label: "\\{A_i:i\\in I\\}",
      order: "reverse-inclusion",
    }),
    b = sub.ClosedSubspace({ label: "B" }),
    ai = sub.ClosedSubspace({ label: "A_i" });
  const tauB = sub.Topology({ label: "\\tau_B" });
  const x = sub.Point({ label: "x" }),
    y = sub.Point({ label: "y" }),
    xi = sub.Point({ label: "x_i" }),
    z = sub.Point({ label: "z" });
  const u = sub.ClopenSubspace({ label: "U" }),
    v = sub.ClopenSubspace({ label: "V" }),
    g = sub.OpenSet({ label: "G" }),
    h = sub.OpenSet({ label: "H" });
  const union = sub.OpenSet({ label: "G\\cup H" }),
    neighborhoods = sub.FiniteSetFamily({ label: "\\{G,H\\}" }),
    net = sub.Net({ label: "(x_i)" });
  sub.TopologyOn(tau, space);
  sub.TopologyOn(tauB, b);
  sub.SubspaceTopologyOf(tauB, b, tau);
  sub.Compact(tau);
  sub.T2(tau);
  sub.T4(tau);
  sub.ClosedFamilyIn(family, space, tau);
  sub.IntersectionOfFamily(b, family);
  sub.FamilyCannotSplitBetween(family, x, y, tau);
  sub.CannotSplitBetween(b, x, y, tauB);
  sub.Subset(b, space);
  sub.ClosedIn(b, tau);
  sub.SetInFamily(ai, family);
  sub.Subset(ai, space);
  sub.ClosedIn(ai, tau);
  sub.Subset(b, ai);
  sub.Member(x, b);
  sub.Member(y, b);
  sub.Hypothesis(sub.SplitBetween.expression(b, x, y, u, v, tauB));
  sub.OpenIn(u, tauB);
  sub.OpenIn(v, tauB);
  sub.ClosedIn(u, tau);
  sub.ClosedIn(v, tau);
  sub.SetSeparation(u, v, g, h, tau);
  sub.Subset(u, g);
  sub.Subset(v, h);
  sub.Disjoint(g, h);
  sub.OpenIn(g, tau);
  sub.OpenIn(h, tau);
  sub.SetInFamily(g, neighborhoods);
  sub.SetInFamily(h, neighborhoods);
  sub.UnionOf(union, neighborhoods);
  sub.Member(xi, ai);
  sub.Outside(xi, union);
  sub.NetIndexedBy(net, family);
  sub.NetIn(net, space);
  sub.FamilyChoiceOutside(net, family, union);
  sub.NetLimitPoint(z, net, tau);
  sub.Member(z, b);
  return sub.make();
}

export const buildConvexBoxFigure = (renderOptions: FigureRenderOptions = {}) =>
  diagram({
    sub: convexBoxSegment(),
    sty: convexBoxStyle(),
    canvas: canvas(183, 193),
    variation: "gemignani-convex-product-box",
    ...renderOptions,
  });
export const buildPolygonalReachabilityFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: polygonalReachability(),
    sty: polygonalReachabilityStyle(),
    canvas: canvas(355, 187),
    variation: "gemignani-local-polygonal-extension",
    ...renderOptions,
  });
export const buildMutuallySeparatedDisksFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: mutuallySeparatedDisks(),
    sty: mutuallySeparatedDisksStyle(),
    canvas: canvas(346, 202),
    variation: "gemignani-mutually-separated-tangent-open-disks",
    ...renderOptions,
  });
export const buildSpiralClosureFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: reciprocalSpiralClosure(),
    sty: spiralClosureStyle(),
    canvas: canvas(225, 211),
    variation: "gemignani-connected-spiral-closure",
    ...renderOptions,
  });
export const buildConnectedProductSlicesFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: connectedProductSlices(),
    sty: connectedProductSlicesStyle(),
    canvas: canvas(344, 229),
    variation: "gemignani-connected-coordinate-chain",
    ...renderOptions,
  });
export const buildAccumulatingComponentsFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: accumulatingCircleComponents(),
    sty: accumulatingComponentsStyle(),
    canvas: canvas(345, 275),
    variation: "gemignani-unsplittable-line-components",
    ...renderOptions,
  });
export const buildDirectedClosedIntersectionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: directedClosedIntersection(),
    sty: directedIntersectionStyle(),
    canvas: canvas(354, 234),
    variation: "gemignani-directed-closed-family-intersection",
    ...renderOptions,
  });
