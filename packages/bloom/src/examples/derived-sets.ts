import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  pointSetTopology as topology,
  type RealIntervalData,
} from "../domains/point-set-topology.js";
import {
  diskDerivedSetsStyle,
  intervalDerivedSetsStyle,
} from "../styles/derived-sets.js";

/** Usual Euclidean derived sets of an open disk; no drawing metadata. */
export function openDiskDerivedSubstance(radius = 1) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("Disk radius must be finite and positive");
  const sub = topology.substance();
  const plane = sub.Set({ label: "\\mathbb{R}^2" });
  const tau = sub.Topology({ label: "\\tau_D" });
  const geometry = { center: [0, 0] as const, radius };
  const a = sub.OpenDisk({ ...geometry, label: "A" });
  const cl = sub.ClosedDisk({ ...geometry, label: "\\operatorname{Cl} A" });
  const fr = sub.CircleBoundary({ ...geometry, label: "\\operatorname{Fr} A" });
  const ext = sub.DiskExterior({ ...geometry, label: "\\operatorname{Ext} A" });
  const endpoint = sub.CoordinatePoint({
    label: `(${radius},0)`,
    coordinates: [radius, 0],
  });
  sub.TopologyOn(tau, plane);
  sub.InteriorOf(a, a, tau);
  sub.ClosureOf(cl, a, tau);
  sub.FrontierOf(fr, a, tau);
  sub.ExteriorOf(ext, a, tau);
  sub.DerivedSetOf(cl, a, tau);
  sub.BoundaryPoint(endpoint, a);
  sub.Outside(endpoint, a);
  sub.Member(endpoint, cl);
  sub.Member(endpoint, fr);
  return sub.make();
}

/** Interior, closure, frontier, exterior and derived set in the usual real topology. */
export function intervalDerivedSubstance(
  data: RealIntervalData = { a: 0, b: 1, leftClosed: false, rightClosed: true },
) {
  if (!(data.a < data.b) || ![data.a, data.b].every(Number.isFinite))
    throw new Error("Interval endpoints must be finite and increasing");
  const sub = topology.substance();
  const line = sub.Set({ label: "\\mathbb{R}" });
  const tau = sub.Topology({ label: "\\tau_D" });
  const a = sub.RealInterval({ ...data, label: "A" });
  const op = sub.OpenInterval({
    ...data,
    label: "A^{\\circ}",
    leftClosed: false,
    rightClosed: false,
  });
  const cl = sub.ClosedInterval({
    ...data,
    label: "\\operatorname{Cl} A",
    leftClosed: true,
    rightClosed: true,
  });
  const fr = sub.EndpointPair({
    label: "\\operatorname{Fr} A",
    endpoints: [data.a, data.b],
  });
  const ext = sub.OpenSet({ label: "\\operatorname{Ext} A" });
  const rays = sub.FiniteSetFamily();
  const left = sub.HalfLine({ bound: data.a, direction: "left" });
  const right = sub.HalfLine({ bound: data.b, direction: "right" });
  sub.TopologyOn(tau, line);
  sub.InteriorOf(op, a, tau);
  sub.ClosureOf(cl, a, tau);
  sub.FrontierOf(fr, a, tau);
  sub.ExteriorOf(ext, a, tau);
  sub.DerivedSetOf(cl, a, tau);
  sub.SetInFamily(left, rays);
  sub.SetInFamily(right, rays);
  sub.UnionOf(ext, rays);
  return sub.make();
}

export const buildDiskDerivedSetsFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: openDiskDerivedSubstance(),
    sty: diskDerivedSetsStyle(),
    canvas: canvas(370, 350),
    variation: "gemignani-3.4",
    ...renderOptions,
  });
export const buildIntervalDerivedSetsFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: intervalDerivedSubstance(),
    sty: intervalDerivedSetsStyle(),
    canvas: canvas(610, 115),
    variation: "gemignani-3.5",
    ...renderOptions,
  });
