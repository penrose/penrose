import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  endpointCirclePoint,
  endpointIdentificationStyle,
  endpointIdentified,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** A whole quotient relation, with two representative classes drawn in the book. */
export function intervalEndpointIdentification(
  options: { bounds?: readonly [number, number]; representative?: number } = {},
) {
  const bounds = options.bounds ?? [0, 1],
    [a, b] = bounds;
  const t = options.representative ?? a + (b - a) / 2;
  endpointIdentified(bounds, t, t);
  if (!(t > a && t < b))
    throw new Error(
      "The singleton representative must be strictly inside the interval",
    );
  const sub = topology.substance();
  const interval = sub.ClosedInterval({
    label: `[${a},${b}]`,
    a,
    b,
    leftClosed: true,
    rightClosed: true,
  });
  const relation = sub.EndpointIdentification({
    label: "R",
    bounds,
    rule: "x~y iff x=y or both x and y are endpoints",
  });
  const quotient = sub.QuotientSpace({ label: `[${a},${b}]/R` });
  const zero = sub.CoordinatePoint({ label: String(a), coordinates: [a, 0] }),
    one = sub.CoordinatePoint({ label: String(b), coordinates: [b, 0] }),
    x = sub.CoordinatePoint({ label: "x", coordinates: [t, 0] });
  const endpointClass = sub.EquivalenceClass({ label: `\\{${a},${b}\\}` }),
    singleton = sub.EquivalenceClass({ label: "\\{x\\}" });
  const q = sub.QuotientMap({ label: "q" });
  const circle = sub.CircleBoundary({
    label: "S^1",
    center: [0, 0],
    radius: 1,
  });
  const phi = sub.CircleParameterization({
    label: "f",
    bounds,
    center: circle.center,
    radius: circle.radius,
    formula: "f(t)=(cos(π/2+2π(t-a)/(b-a)),sin(π/2+2π(t-a)/(b-a)))",
  });
  const induced = sub.TopologicalMap({ label: "\\bar f" });
  const endpointImage = sub.CoordinatePoint({
    label: `f(${a})=f(${b})`,
    coordinates: endpointCirclePoint(bounds, a),
  });
  const interiorImage = sub.CoordinatePoint({
    label: "f(x)",
    coordinates: endpointCirclePoint(bounds, t),
  });
  const sourceTopology = sub.Topology({ label: "\\tau" }),
    quotientTopology = sub.Topology({ label: "\\tau'" }),
    circleTopology = sub.Topology({ label: "\\tau_{S^1}" });
  sub.TopologyOn(sourceTopology, interval);
  sub.TopologyOn(quotientTopology, quotient);
  sub.TopologyOn(circleTopology, circle);
  sub.EquivalenceOn(relation, interval);
  sub.EquivalentUnder(zero, one, relation);
  sub.QuotientOf(quotient, interval, relation);
  for (const [point, cls, image] of [
    [zero, endpointClass, endpointImage],
    [one, endpointClass, endpointImage],
    [x, singleton, interiorImage],
  ] as const) {
    sub.Member(point, interval);
    sub.Member(point, cls);
    sub.ClassOf(cls, point, relation);
    sub.MapsTo(q, point, cls);
    sub.MapsTo(phi, point, image);
  }
  for (const cls of [endpointClass, singleton]) {
    sub.Member(cls, quotient);
    sub.ClassInQuotient(cls, quotient);
  }
  sub.Member(endpointImage, circle);
  sub.Member(interiorImage, circle);
  sub.MapBetween(q, interval, quotient);
  sub.IdentificationMap(q, interval, quotient, relation);
  sub.IdentificationTopology(quotientTopology, sourceTopology, q);
  sub.MapBetween(phi, interval, circle);
  sub.CircleParameterizes(phi, interval, circle);
  sub.MapBetween(induced, quotient, circle);
  sub.FactorsThrough(phi, q, induced);
  sub.MapsTo(induced, endpointClass, endpointImage);
  sub.MapsTo(induced, singleton, interiorImage);
  sub.ContinuousMap(q, sourceTopology, quotientTopology);
  sub.ContinuousMap(phi, sourceTopology, circleTopology);
  sub.Homeomorphism(induced, quotientTopology, circleTopology);
  return sub.make();
}

export const buildEndpointIdentificationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: intervalEndpointIdentification(),
    sty: endpointIdentificationStyle(),
    canvas: canvas(250, 330),
    variation: "gemignani-interval-endpoint-quotient",
    ...renderOptions,
  });
