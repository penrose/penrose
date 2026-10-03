import type { FigureRenderOptions, PlaneCoordinates } from "@penrose/bloom";
import {
  canvas,
  centralProjectToSegment,
  diagram,
  radialProjectToCircle,
  topologicalProjectionStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** The visible central projection construction; Example 12's page is absent. */
export function segmentCentralProjection(options: { fraction?: number } = {}) {
  const p: PlaneCoordinates = [0, 2],
    a: PlaneCoordinates = [-0.85, 0.08],
    b: PlaneCoordinates = [2.45, -0.2];
  const c: PlaneCoordinates = [
    p[0] + 0.265 * (b[0] - p[0]),
    p[1] + 0.265 * (b[1] - p[1]),
  ];
  const fraction = options.fraction ?? -a[0] / (c[0] - a[0]);
  if (!Number.isFinite(fraction) || fraction < 0 || fraction > 1)
    throw new Error("The marked fraction must lie in the closed segment");
  const point: PlaneCoordinates = [
    a[0] + fraction * (c[0] - a[0]),
    a[1] + fraction * (c[1] - a[1]),
  ];
  const image = centralProjectToSegment(p, point, [a, b]);
  const sub = topology.substance();
  const source = sub.LinearSegment({
    label: "\\text{Segment 1}",
    endpoints: [a, c],
  });
  const target = sub.LinearSegment({
    label: "\\text{Segment 2}",
    endpoints: [a, b],
  });
  const center = sub.CoordinatePoint({ label: "P", coordinates: p });
  const x = sub.CoordinatePoint({ label: "x", coordinates: point }),
    jx = sub.CoordinatePoint({ label: "j_P(x)", coordinates: image });
  const j = sub.CentralProjection({ label: "j_P", center: p });
  const sourceTopology = sub.Topology({ label: "\\tau_1" }),
    targetTopology = sub.Topology({ label: "\\tau_2" });
  sub.TopologyOn(sourceTopology, source);
  sub.TopologyOn(targetTopology, target);
  sub.MapBetween(j, source, target);
  sub.CentralProjectionBetween(j, source, target);
  sub.ProjectionCenter(j, center);
  sub.Member(x, source);
  sub.Member(jx, target);
  sub.MapsTo(j, x, jx);
  sub.ContinuousMap(j, sourceTopology, targetTopology);
  sub.Homeomorphism(j, sourceTopology, targetTopology);
  return sub.make();
}

/** The displayed map is triangle→circle; its inverse also matches the prose's direction. */
export function triangleCircleRadialProjection() {
  const center: PlaneCoordinates = [0, 0],
    radius = 1.16;
  const vertices: readonly [
    PlaneCoordinates,
    PlaneCoordinates,
    PlaneCoordinates,
  ] = [
    [-1.58, -0.82],
    [1.16, -0.6],
    [0.35, 1.43],
  ];
  const point: PlaneCoordinates = [
    (vertices[1][0] + vertices[2][0]) / 2,
    (vertices[1][1] + vertices[2][1]) / 2,
  ];
  const image = radialProjectToCircle(center, radius, point);
  const sub = topology.substance();
  const triangle = sub.TriangleBoundary({ label: "\\triangle", vertices }),
    circle = sub.CircleBoundary({ label: "S^1", center, radius });
  const x = sub.CoordinatePoint({ label: "x", coordinates: point }),
    jx = sub.CoordinatePoint({ label: "j(x)", coordinates: image });
  const j = sub.RadialProjection({ label: "j", center }),
    inverse = sub.TopologicalMap({ label: "j^{-1}" });
  const sourceTopology = sub.Topology({ label: "\\tau_{\\triangle}" }),
    targetTopology = sub.Topology({ label: "\\tau_{S^1}" });
  sub.TopologyOn(sourceTopology, triangle);
  sub.TopologyOn(targetTopology, circle);
  sub.MapBetween(j, triangle, circle);
  sub.MapBetween(inverse, circle, triangle);
  sub.InverseMaps(j, inverse);
  sub.RadialProjectionBetween(j, triangle, circle);
  sub.Member(x, triangle);
  sub.Member(jx, circle);
  sub.MapsTo(j, x, jx);
  sub.MapsTo(inverse, jx, x);
  sub.ContinuousMap(j, sourceTopology, targetTopology);
  sub.ContinuousMap(inverse, targetTopology, sourceTopology);
  sub.Homeomorphism(j, sourceTopology, targetTopology);
  sub.Homeomorphism(inverse, targetTopology, sourceTopology);
  return sub.make();
}

export const buildSegmentProjectionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: segmentCentralProjection(),
    sty: topologicalProjectionStyle(),
    canvas: canvas(340, 240),
    variation: "gemignani-segment-central-projection",
    ...renderOptions,
  });
export const buildTriangleCircleFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: triangleCircleRadialProjection(),
    sty: topologicalProjectionStyle(),
    canvas: canvas(280, 245),
    variation: "gemignani-triangle-circle-radial-map",
    ...renderOptions,
  });
