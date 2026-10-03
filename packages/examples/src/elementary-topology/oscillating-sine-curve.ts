import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  oscillatingSineCurveStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** f(-1)=(0,0), f(x)=(x,sin(1/x)) for x>0; only the origin is adjoined to the graph. */
export function originAdjoinedSineImage() {
  const sub = topology.substance();
  const real = sub.RealLine({ label: "R" }),
    plane = sub.EuclideanPlane({ label: "\\mathbb R^2" });
  const x = sub.Subspace({ label: "X" });
  const isolated = sub.RealPoint({ label: "-1", coordinate: -1 }),
    a = sub.ClopenSingleton({ label: "A" });
  const b = sub.ClopenHalfLine({ label: "B", bound: 0, direction: "right" });
  const pieces = sub.FiniteSetFamily({ label: "\\{A,B\\}" });
  const data = { frequency: 1, amplitude: 1 };
  const graph = sub.OscillatingSineGraph({
    label: "\\{(x,\\sin(1/x)):x>0\\}",
    ...data,
  });
  const y = sub.OriginAdjoinedSineCurve({ label: "Y", ...data });
  const f = sub.OscillatingSineMap({ label: "f", ...data });
  const origin = sub.CoordinatePoint({ label: "(0,0)", coordinates: [0, 0] });
  const originSet = sub.Singleton({ label: "\\{(0,0)\\}" });
  const imagePieces = sub.FiniteSetFamily({ label: "\\{f(A),f(B)\\}" });
  const tauR = sub.Topology({ label: "\\tau_R" }),
    tauX = sub.Topology({ label: "\\tau_X" }),
    tauPlane = sub.Topology({ label: "\\tau_D" }),
    tauY = sub.Topology({ label: "\\tau_Y" });
  sub.TopologyOn(tauR, real);
  sub.TopologyOn(tauX, x);
  sub.TopologyOn(tauPlane, plane);
  sub.TopologyOn(tauY, y);
  sub.Subset(x, real);
  sub.Subset(y, plane);
  sub.SubspaceTopologyOf(tauX, x, tauR);
  sub.SubspaceTopologyOf(tauY, y, tauPlane);
  sub.LocallyCompact(tauX);
  const closedSource = sub.ClosedRegion({ label: "\\{-1\\}\\cup[0,\\infty)" });
  const openSource = sub.OpenSet({ label: "R-\\{0\\}" });
  sub.ClosedIn(closedSource, tauR);
  sub.OpenIn(openSource, tauR);
  sub.IntersectionOf(x, closedSource, openSource);
  sub.LocallyClosedIn(x, real, tauR);
  sub.NotLocallyCompact(tauY);
  sub.SingletonOf(a, isolated);
  sub.Member(isolated, a);
  sub.Member(isolated, x);
  sub.SetInFamily(a, pieces);
  sub.SetInFamily(b, pieces);
  sub.UnionOf(x, pieces);
  sub.Disjoint(a, b);
  for (const part of [a, b]) {
    sub.Subset(part, x);
    sub.OpenIn(part, tauX);
    sub.ClosedIn(part, tauX);
  }
  sub.SingletonOf(originSet, origin);
  sub.SetInFamily(originSet, imagePieces);
  sub.SetInFamily(graph, imagePieces);
  sub.UnionOf(y, imagePieces);
  sub.Subset(graph, y);
  sub.Subset(originSet, y);
  sub.Outside(origin, graph);
  sub.Member(origin, originSet);
  sub.Member(origin, y);
  sub.SineCurveImageOf(y, graph, origin);
  sub.MapBetween(f, x, y);
  sub.SineGraphMapOn(f, b, graph);
  sub.MapsTo(f, isolated, origin);
  sub.ImageOf(originSet, f, a);
  sub.ImageOf(graph, f, b);
  sub.ImageOf(y, f, x);
  sub.ContinuousMap(f, tauX, tauY);
  sub.OneToOne(f);
  sub.Onto(f, y);
  sub.NotOpenMap(f, tauX, tauY);
  sub.FailsLocalCompactnessAt(origin, y, tauY);
  // The drawn endpoint markers are accumulation annotations, not members of the image.
  for (const height of [-1, 1]) {
    const annotation = sub.CoordinatePoint({
      label: `(0,${height})`,
      coordinates: [0, height],
    });
    sub.Member(annotation, plane);
    sub.Outside(annotation, y);
  }
  const sequence = sub.OscillationSequence({ label: "(z_n)", height: 0.5 });
  const ambientLimit = sub.CoordinatePoint({
    label: "(0,1/2)",
    coordinates: [0, 0.5],
  });
  sub.OscillationSequenceIn(sequence, graph);
  sub.NetIn(sequence, graph);
  sub.NetConvergesTo(sequence, ambientLimit, tauPlane);
  sub.Outside(ambientLimit, y);
  return sub.make();
}
export const buildOriginAdjoinedSineFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: originAdjoinedSineImage(),
    sty: oscillatingSineCurveStyle(),
    canvas: canvas(555, 195),
    variation: "gemignani-origin-adjoined-oscillating-sine-image",
    ...renderOptions,
  });
