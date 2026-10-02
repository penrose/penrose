import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { identificationSourceStyle } from "../styles/identification-sources.js";

/** Figure 4.4 collapses both complete opposite edges together into one class. */
export function oppositeEdgesCollapseSubstance() {
  const s = topology.substance();
  const region = s.ClosedRectangle({ label: "X", bounds: [0, 0, 1, 1] });
  const corners = [
    s.CoordinatePoint({ label: "A", coordinates: [0, 1] }),
    s.CoordinatePoint({ label: "B", coordinates: [0, 0] }),
    s.CoordinatePoint({ label: "C", coordinates: [1, 1] }),
    s.CoordinatePoint({ label: "D", coordinates: [1, 0] }),
  ];
  corners.forEach((p) => s.Member(p, region));
  const left = s.LinearSegment({
    label: "\\overline{AB}",
    endpoints: [
      [0, 1],
      [0, 0],
    ],
  });
  const right = s.LinearSegment({
    label: "\\overline{CD}",
    endpoints: [
      [1, 1],
      [1, 0],
    ],
  });
  const edges = s.ClosedSet({ label: "\\overline{AB}\\cup\\overline{CD}" });
  const family = s.FiniteSetFamily({
    label: "\\{\\overline{AB},\\overline{CD}\\}",
  });
  s.SetInFamily(left, family);
  s.SetInFamily(right, family);
  s.UnionOf(edges, family);
  s.Subset(left, region);
  s.Subset(right, region);
  s.Subset(edges, region);
  const relation = s.SubsetCollapseRelation({ label: "R", collapsed: edges });
  const quotient = s.QuotientSpace({ label: "X/R" });
  s.EquivalenceOn(relation, region);
  s.QuotientOf(quotient, region, relation);
  // One class contains both entire edges, not just corresponding edge pairs.
  const collapsed = s.EquivalenceClass({
    label: "\\overline{AB}\\cup\\overline{CD}",
  });
  corners.forEach((p) => {
    s.Member(p, collapsed);
    s.ClassOf(collapsed, p, relation);
  });
  s.ClassInQuotient(collapsed, quotient);
  return s.make();
}

/** Figure 4.5 identifies antipodal boundary points and leaves interior singletons. */
export function antipodalDiskSubstance(radius = 1) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("Disk radius must be finite and positive");
  const s = topology.substance();
  const disk = s.ClosedDisk({ label: "X", center: [0, 0], radius });
  const center = s.CoordinatePoint({ label: "", coordinates: [0, 0] });
  s.Member(center, disk);
  const relation = s.AntipodalBoundaryIdentification({
    label: "R",
    center: [0, 0],
    radius,
  });
  const quotient = s.QuotientSpace({ label: "X/R" });
  s.EquivalenceOn(relation, disk);
  s.QuotientOf(quotient, disk, relation);
  const singleton = s.EquivalenceClass({ label: "\\{(0,0)\\}" });
  s.ClassOf(singleton, center, relation);
  s.Member(center, singleton);
  s.ClassInQuotient(singleton, quotient);
  return s.make();
}

/** Figure 4.6's literal caption names one subset; no repaired partition is asserted. */
export function integerDifferencePlaneSubstance(period = 1) {
  if (!(period > 0) || !Number.isFinite(period))
    throw new Error("Period must be finite and positive");
  const s = topology.substance();
  const plane = s.EuclideanPlane({ label: "R^2" });
  const subset = s.IntegerDifferenceLocus({
    label: "\\{(x,y)\\mid x=y+m,\\ m\\in Z\\}",
    period,
  });
  s.Subset(subset, plane);
  return s.make();
}

/** Figure 4.7 collapses the entire boundary of C into one class. */
export function polygonBoundaryCollapseSubstance() {
  const vertices = [
    [0, 0],
    [0.15, 1],
    [0.55, 1.08],
    [1.45, 0.57],
    [0.67, -0.15],
  ] as const;
  const s = topology.substance();
  const polygon = s.ClosedPolygon({ label: "C", vertices });
  const boundary = s.PolygonBoundary({ label: "\\partial C", vertices });
  s.BoundaryOf(boundary, polygon);
  s.Subset(boundary, polygon);
  const relation = s.SubsetCollapseRelation({
    label: "R",
    collapsed: boundary,
  });
  const quotient = s.QuotientSpace({ label: "C/R" });
  s.EquivalenceOn(relation, polygon);
  s.QuotientOf(quotient, polygon, relation);
  return s.make();
}

export const buildOppositeEdgesCollapseFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: oppositeEdgesCollapseSubstance(),
    sty: identificationSourceStyle(),
    canvas: canvas(240, 165),
    variation: "gemignani-opposite-edges-collapse",
    ...renderOptions,
  });
export const buildAntipodalDiskFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: antipodalDiskSubstance(),
    sty: identificationSourceStyle({
      interactive: renderOptions.interactive
        ? {
            maxDistance: 6,
            ...(typeof renderOptions.interactive === "object"
              ? renderOptions.interactive
              : {}),
          }
        : undefined,
    }),
    canvas: canvas(166, 166),
    variation: "gemignani-antipodal-disk",
    ...renderOptions,
  });
export const buildIntegerDifferencePlaneFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: integerDifferencePlaneSubstance(),
    sty: identificationSourceStyle({
      frameWidth: 230,
      frameHeight: 152,
      offset: [-7, -8],
    }),
    canvas: canvas(260, 178),
    variation: "gemignani-integer-difference-plane",
    ...renderOptions,
  });
export const buildPolygonBoundaryCollapseFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: polygonBoundaryCollapseSubstance(),
    sty: identificationSourceStyle({ frameWidth: 228, frameHeight: 162 }),
    canvas: canvas(244, 176),
    variation: "gemignani-polygon-boundary-collapse",
    ...renderOptions,
  });
