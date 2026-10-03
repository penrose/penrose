import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { coordinateEmbeddingStyle } from "../styles/product-topology.js";
import { subspaceDerivedSetsStyle } from "../styles/subspaces.js";
import { realLineEmbeddingSubstance } from "./product-topology.js";

/** Example 6, section 4.2: an affine line has different interior and frontier in itself. */
export function subspaceDerivedSetsSubstance(
  fixedCoordinate = 0,
  neighborhoodRadius = 0.5,
) {
  const witnessHeight = fixedCoordinate + neighborhoodRadius / 2;
  if (
    ![fixedCoordinate, neighborhoodRadius, witnessHeight].every(
      Number.isFinite,
    ) ||
    !(neighborhoodRadius > 0) ||
    witnessHeight === fixedCoordinate
  )
    throw new Error(
      "The affine subspace requires a finite coordinate and positive neighborhood radius",
    );
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "X=R^2" });
  const line = sub.OpenAffineSubspace({
    label: "A=Y",
    coefficients: [0, 1, fixedCoordinate],
  });
  const empty = sub.EmptySet({ label: "\\phi" });
  const ambient = sub.Topology({ label: "\\tau_D" });
  const relative = sub.Topology({ label: "\\tau_Y" });
  const p = sub.CoordinatePoint({
    label: "p",
    coordinates: [0, fixedCoordinate],
  });
  const q = sub.CoordinatePoint({
    label: "q",
    coordinates: [0, witnessHeight],
  });
  const disk = sub.DiskNeighborhood({
    label: "N",
    center: p.coordinates,
    radius: neighborhoodRadius,
  });
  const relativeNeighborhood = sub.Neighborhood({ label: "N\\cap Y" });
  const family = sub.FiniteSetFamily();
  sub.TopologyOn(ambient, plane);
  sub.TopologyOn(relative, line);
  sub.SubspaceTopologyOf(relative, line, ambient);
  sub.Subset(line, plane);
  sub.Subset(empty, line);
  sub.OpenIn(line, relative);
  sub.ClosedIn(line, relative);
  sub.ClosedIn(line, ambient);
  sub.InteriorOf(empty, line, ambient);
  sub.FrontierOf(line, line, ambient);
  sub.ClosureOf(line, line, ambient);
  sub.InteriorOf(line, line, relative);
  sub.FrontierOf(empty, line, relative);
  sub.ClosureOf(line, line, relative);
  sub.Member(p, line);
  sub.Member(p, disk);
  sub.Member(q, disk);
  sub.Outside(q, line);
  sub.NeighborhoodOf(disk, p);
  sub.SetInFamily(disk, family);
  sub.SetInFamily(line, family);
  sub.FiniteIntersectionOf(relativeNeighborhood, family);
  sub.NeighborhoodOf(relativeNeighborhood, p);
  sub.OpenIn(relativeNeighborhood, relative);
  sub.Member(p, relativeNeighborhood);
  return sub.make();
}

export const buildSubspaceDerivedSetsIllustration = (
  fixedCoordinate = 0,
  neighborhoodRadius = 0.5,
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: subspaceDerivedSetsSubstance(fixedCoordinate, neighborhoodRadius),
    sty: subspaceDerivedSetsStyle(),
    canvas: canvas(640, 360),
    variation: "original-relative-derived-sets",
    ...renderOptions,
  });

/** An original illustration of Chapter 4 Proposition 20, not a numbered book figure. */
export function coordinateSliceSubstance(
  fixedCoordinate = 0.75,
  varyingCoordinate: 1 | 2 = 2,
) {
  return realLineEmbeddingSubstance(fixedCoordinate, varyingCoordinate);
}

/** Reuses Figure 4.10's coordinate-inclusion style with its ambient axes exposed. */
export const buildCoordinateSliceIllustration = (
  fixedCoordinate = 0.75,
  varyingCoordinate: 1 | 2 = 2,
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: coordinateSliceSubstance(fixedCoordinate, varyingCoordinate),
    sty: coordinateEmbeddingStyle({ showAmbientAxes: true }),
    canvas: canvas(400, 360),
    variation: "original-coordinate-slice",
    ...renderOptions,
  });
