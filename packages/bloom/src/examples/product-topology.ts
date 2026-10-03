import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  coordinateEmbed,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  coordinateEmbeddingStyle,
  productNeighborhoodStyle,
} from "../styles/product-topology.js";

/** A coordinate projection's inverse image of an open interval is an unbounded strip. */
export function productStripSubstance(a = 1, b = 2) {
  if (!(a < b) || ![a, b].every(Number.isFinite))
    throw new Error("Product intervals require finite increasing endpoints");
  const sub = topology.substance();
  const real = sub.RealLine({ label: "\\mathbb{R}" });
  const plane = sub.ProductSet({ label: "\\mathbb{R}^2" });
  const tauReal = sub.Topology({ label: "\\tau_D" });
  const tauProduct = sub.Topology({ label: "\\tau_{\\times}" });
  const family = sub.Subbasis({ label: "\\mathcal{S}" });
  const interval = sub.OpenInterval({
    a,
    b,
    leftClosed: false,
    rightClosed: false,
    endpointNames: ["a", "b"],
    label: "(a,b)",
  });
  const strip = sub.OpenProductSet({ label: "(a,b)\\times R" });
  const projection = sub.CoordinateProjection({ coordinate: 1, label: "p_1" });
  sub.ProductOf(plane, real, real);
  sub.ProductOf(strip, interval, real);
  sub.TopologyOn(tauReal, real);
  sub.TopologyOn(tauProduct, plane);
  sub.ProductTopologyOf(tauProduct, tauReal, tauReal);
  sub.ProjectionOf(projection, plane, real);
  sub.MapBetween(projection, plane, real);
  sub.ContinuousMap(projection, tauProduct, tauReal);
  sub.InverseImageOf(strip, projection, interval);
  sub.SubbasisFor(family, tauProduct);
  sub.SetInFamily(strip, family);
  sub.Subset(strip, plane);
  for (const [coordinate, name] of [
    [a, "a"],
    [b, "b"],
  ] as const) {
    const point = sub.CoordinatePoint({
      coordinates: [coordinate, 0],
      label: `(${name},0)`,
    });
    sub.BoundaryPoint(point, strip);
    sub.Outside(point, strip);
  }
  return sub.make();
}

/** One finite intersection of coordinate subbasis strips is an open product rectangle. */
export function productRectangleSubstance(a = 1.4, b = 2.15, c = 1, d = 1.7) {
  if (!(a < b && c < d) || ![a, b, c, d].every(Number.isFinite))
    throw new Error("Product intervals require finite increasing endpoints");
  const sub = topology.substance();
  const real = sub.RealLine({ label: "\\mathbb{R}" });
  const plane = sub.ProductSet({ label: "\\mathbb{R}^2" });
  const tauReal = sub.Topology({ label: "\\tau_D" });
  const tauProduct = sub.Topology({ label: "\\tau_{\\times}" });
  const tauMetric = sub.Topology({ label: "\\tau_{D_3}" });
  const basis = sub.Basis({ label: "\\mathcal{B}" });
  const subbasis = sub.Subbasis({ label: "\\mathcal{S}" });
  const family = sub.FiniteSetFamily();
  const xInterval = sub.OpenInterval({
    a,
    b,
    leftClosed: false,
    rightClosed: false,
    endpointNames: ["a", "b"],
    label: "(a,b)",
  });
  const yInterval = sub.OpenInterval({
    a: c,
    b: d,
    leftClosed: false,
    rightClosed: false,
    endpointNames: ["c", "d"],
    label: "(c,d)",
  });
  const vertical = sub.OpenProductSet({ label: "(a,b)\\times R" });
  const horizontal = sub.OpenProductSet({ label: "R\\times(c,d)" });
  const rectangle = sub.OpenProductSet({ label: "(a,b)\\times(c,d)" });
  sub.ProductOf(plane, real, real);
  sub.ProductOf(vertical, xInterval, real);
  sub.ProductOf(horizontal, real, yInterval);
  sub.ProductOf(rectangle, xInterval, yInterval);
  sub.TopologyOn(tauReal, real);
  sub.TopologyOn(tauProduct, plane);
  sub.TopologyOn(tauMetric, plane);
  sub.ProductTopologyOf(tauProduct, tauReal, tauReal);
  sub.EqualTopologies(tauProduct, tauMetric);
  sub.BasisFor(basis, tauProduct);
  sub.SubbasisFor(subbasis, tauProduct);
  sub.SetInFamily(rectangle, basis);
  sub.FiniteIntersectionOf(rectangle, family);
  sub.Subset(rectangle, vertical);
  sub.Subset(rectangle, horizontal);
  sub.Subset(rectangle, plane);
  for (const [strip, interval, coordinate] of [
    [vertical, xInterval, 1],
    [horizontal, yInterval, 2],
  ] as const) {
    const projection = sub.CoordinateProjection({
      coordinate,
      label: `p_${coordinate}`,
    });
    sub.ProjectionOf(projection, plane, real);
    sub.MapBetween(projection, plane, real);
    sub.ContinuousMap(projection, tauProduct, tauReal);
    sub.InverseImageOf(strip, projection, interval);
    sub.SetInFamily(strip, subbasis);
    sub.SetInFamily(strip, family);
    sub.Subset(strip, plane);
    for (const [value, name] of [
      [interval.a, interval.endpointNames![0]],
      [interval.b, interval.endpointNames![1]],
    ] as const) {
      const point = sub.CoordinatePoint({
        coordinates: coordinate === 1 ? [value, 0] : [0, value],
        label: coordinate === 1 ? `(${name},0)` : `(0,${name})`,
      });
      sub.BoundaryPoint(point, strip);
      sub.Outside(point, strip);
    }
  }
  return sub.make();
}

/** The coordinate inclusion is a homeomorphism onto its image subspace, not onto the plane. */
export function realLineEmbeddingSubstance(
  fixedCoordinate = 0,
  varyingCoordinate: 1 | 2 = 1,
) {
  if (!Number.isFinite(fixedCoordinate))
    throw new Error("The fixed coordinate must be finite");
  const sub = topology.substance();
  const real = sub.RealLine({ label: "R" });
  const plane = sub.ProductSet({ label: "R^2" });
  const image = sub.Subspace({
    label:
      varyingCoordinate === 1
        ? `R\\times\\{${fixedCoordinate}\\}`
        : `\\{${fixedCoordinate}\\}\\times R`,
  });
  const tauReal = sub.Topology({ label: "\\tau_D" });
  const tauPlane = sub.Topology({ label: "\\tau_{\\times}" });
  const tauImage = sub.Topology({ label: "\\tau_Y" });
  const inclusion = sub.CoordinateEmbedding({
    varyingCoordinate,
    fixedCoordinate,
    label: `q_${varyingCoordinate}`,
  });
  const corestriction = sub.TopologicalMap({
    formula: "coordinate inclusion with codomain Y",
  });
  const inverse = sub.CoordinateProjection({ coordinate: varyingCoordinate });
  const x = sub.RealPoint({ coordinate: 1, label: "x" });
  const point = sub.CoordinatePoint({
    coordinates: coordinateEmbed(inclusion, x.coordinate),
    label:
      varyingCoordinate === 1
        ? `(x,${fixedCoordinate})`
        : `(${fixedCoordinate},x)`,
  });
  sub.ProductOf(plane, real, real);
  sub.TopologyOn(tauReal, real);
  sub.TopologyOn(tauPlane, plane);
  sub.TopologyOn(tauImage, image);
  sub.ProductTopologyOf(tauPlane, tauReal, tauReal);
  sub.SubspaceTopologyOf(tauImage, image, tauPlane);
  sub.Subset(image, plane);
  sub.ImageOf(image, inclusion, real);
  sub.EmbeddingInto(inclusion, real, plane, image);
  sub.MapBetween(inclusion, real, plane);
  sub.ContinuousMap(inclusion, tauReal, tauPlane);
  sub.CorestrictionOf(corestriction, inclusion, image);
  sub.MapBetween(corestriction, real, image);
  sub.Homeomorphism(corestriction, tauReal, tauImage);
  sub.ContinuousMap(corestriction, tauReal, tauImage);
  sub.ProjectionOf(inverse, plane, real);
  sub.MapBetween(inverse, image, real);
  sub.ContinuousMap(inverse, tauImage, tauReal);
  sub.InverseMaps(corestriction, inverse);
  sub.MapsTo(inclusion, x, point);
  sub.MapsTo(corestriction, x, point);
  sub.MapsTo(inverse, point, x);
  sub.Member(x, real);
  sub.Member(point, image);
  sub.Member(point, plane);
  return sub.make();
}

export const buildProductStripFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: productStripSubstance(),
    sty: productNeighborhoodStyle(),
    canvas: canvas(370, 200),
    variation: "gemignani-4.8",
    ...renderOptions,
  });
export const buildProductRectangleFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: productRectangleSubstance(),
    sty: productNeighborhoodStyle(),
    canvas: canvas(365, 305),
    variation: "gemignani-4.9",
    ...renderOptions,
  });
export const buildRealLineEmbeddingFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: realLineEmbeddingSubstance(),
    sty: coordinateEmbeddingStyle(),
    canvas: canvas(310, 205),
    variation: "gemignani-4.10",
    ...renderOptions,
  });
