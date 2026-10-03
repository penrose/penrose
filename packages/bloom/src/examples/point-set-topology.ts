import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  diskBoundaryStyle,
  reciprocalSetDistanceStyle,
  separationStyle,
} from "../styles/point-set-topology.js";

/** Example 18: the open unit disk and the two boundary singletons. */
export function openDiskBoundarySubstance(radius = 1) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("Disk radius must be finite and positive");
  const sub = topology.substance();
  const y = sub.OpenDisk({ label: "Y", center: [0, 0], radius });
  const w = sub.Singleton({ label: "W" });
  const z = sub.Singleton({ label: "Z" });
  const right = sub.CoordinatePoint({ label: "w", coordinates: [radius, 0] });
  const left = sub.CoordinatePoint({ label: "z", coordinates: [-radius, 0] });
  for (const [singleton, p] of [
    [w, right],
    [z, left],
  ] as const) {
    sub.SingletonOf(singleton, p);
    sub.Member(p, singleton);
    sub.BoundaryPoint(p, y);
    sub.Outside(p, y);
    sub.Disjoint(singleton, y);
    sub.ZeroPointDistance(p, y);
    sub.ZeroSetDistance(singleton, y);
  }
  sub.Disjoint(w, z);
  return sub.make();
}

/** Proposition 12; abstract closed sets and neighborhoods carry no drawing fields. */
export function pointClosedSetSeparationSubstance() {
  const sub = topology.substance();
  const f = sub.ClosedRegion({ label: "F" });
  const x = sub.Point({ label: "x" });
  const y = sub.Point({ label: "y" });
  const u = sub.Neighborhood({ label: "U" });
  const uy = sub.Neighborhood({ label: "U_y" });
  const v = sub.NeighborhoodUnion({ label: "V" });
  sub.Outside(x, f);
  sub.Member(x, u);
  sub.Member(y, f);
  sub.NeighborhoodOf(u, x);
  sub.NeighborhoodOf(uy, y);
  sub.Subset(uy, v);
  sub.Subset(f, v);
  sub.Disjoint(u, v);
  sub.PointClosedSeparation(x, f, u, v);
  return sub.make();
}

/** Proposition 13; representative balls illustrate two infinite neighborhood unions. */
export function closedSetsSeparationSubstance() {
  const sub = topology.substance();
  const f = sub.ClosedRegion({ label: "F" });
  const fp = sub.ClosedRegion({ label: "F'" });
  const u = sub.NeighborhoodUnion({ label: "U" });
  const v = sub.NeighborhoodUnion({ label: "V" });
  const y = sub.Point({ label: "y" });
  const yp = sub.Point({ label: "y'" });
  const n = sub.Neighborhood({ label: "N(y,\\rho_y)" });
  const np = sub.Neighborhood({ label: "N(y',\\rho'_{y'})" });
  sub.Member(y, f);
  sub.Member(yp, fp);
  sub.NeighborhoodOf(n, y);
  sub.NeighborhoodOf(np, yp);
  sub.Subset(n, u);
  sub.Subset(np, v);
  sub.Subset(f, u);
  sub.Subset(fp, v);
  sub.Disjoint(f, fp);
  sub.Disjoint(u, v);
  sub.ClosedSetsSeparation(f, fp, u, v);
  return sub.make();
}

/** Example 19: both entire sets are closed, disjoint, and have infimum distance zero. */
export function reciprocalXAxisSubstance(coefficient = 1) {
  if (!(coefficient > 0) || !Number.isFinite(coefficient))
    throw new Error(
      "This first/third quadrant figure requires a finite positive coefficient",
    );
  const sub = topology.substance();
  const f = sub.ReciprocalGraph({ label: "F", coefficient });
  const axis = sub.AffineLine({ label: "F'", coefficients: [0, 1, 0] });
  sub.Disjoint(f, axis);
  sub.ZeroSetDistance(f, axis);
  return sub.make();
}

export const buildOpenDiskBoundaryFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: openDiskBoundarySubstance(),
    sty: diskBoundaryStyle(),
    canvas: canvas(360, 280),
    variation: "gemignani-2.17",
    ...renderOptions,
  });
export const buildPointClosedSeparationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: pointClosedSetSeparationSubstance(),
    sty: separationStyle(),
    canvas: canvas(350, 210),
    variation: "gemignani-2.18",
    ...renderOptions,
  });
export const buildClosedSetsSeparationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: closedSetsSeparationSubstance(),
    sty: separationStyle(),
    canvas: canvas(500, 280),
    variation: "gemignani-2.19",
    ...renderOptions,
  });
export const buildReciprocalXAxisFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: reciprocalXAxisSubstance(),
    sty: reciprocalSetDistanceStyle(),
    canvas: canvas(340, 350),
    variation: "gemignani-2.20",
    ...renderOptions,
  });
