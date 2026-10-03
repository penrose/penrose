import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  halfPlaneIntersectionStyle,
  intervalSubbasisStyle,
  triangleDiskBasisStyle,
} from "../styles/topology-bases.js";

/** Two open rays generate a representative basis interval; a,b are symbolic names. */
export function intervalSubbasisSubstance(a = 0, b = 1) {
  if (!(a < b) || ![a, b].every(Number.isFinite))
    throw new Error("Interval endpoints must be finite and increasing");
  const sub = topology.substance();
  const line = sub.Set({ label: "\\mathbb{R}" });
  const tau = sub.Topology({ label: "\\tau" });
  const basis = sub.Basis({ label: "\\mathcal{B}" });
  const subbasis = sub.Subbasis({ label: "\\mathcal{S}" });
  const rays = sub.FiniteSetFamily();
  const left = sub.HalfLine({
    label: "\\{x\\mid x<b\\}",
    bound: b,
    direction: "left",
    boundName: "b",
  });
  const right = sub.HalfLine({
    label: "\\{x\\mid a<x\\}",
    bound: a,
    direction: "right",
    boundName: "a",
  });
  const interval = sub.OpenInterval({
    label: "\\{x\\mid a<x<b\\}",
    a,
    b,
    leftClosed: false,
    rightClosed: false,
    endpointNames: ["a", "b"],
  });
  sub.TopologyOn(tau, line);
  sub.BasisFor(basis, tau);
  sub.SubbasisFor(subbasis, tau);
  for (const ray of [left, right]) {
    sub.SetInFamily(ray, rays);
    sub.SetInFamily(ray, subbasis);
  }
  sub.SetInFamily(interval, basis);
  sub.FiniteIntersectionOf(interval, rays);
  return sub.make();
}

/** A D3 neighborhood is an intersection of four strict coordinate half-planes. */
export function halfPlaneSquareSubstance(radius = 1) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("Square radius must be finite and positive");
  const sub = topology.substance();
  const plane = sub.Set({ label: "\\mathbb{R}^2" });
  const tau = sub.Topology({ label: "\\tau_3" });
  const basis = sub.Basis({ label: "\\mathcal{B}_{D_3}" });
  const subbasis = sub.Subbasis({ label: "\\mathcal{S}" });
  const family = sub.FiniteSetFamily();
  const point = sub.CoordinatePoint({ label: "(x,y)", coordinates: [0, 0] });
  const square = sub.OpenSquare({
    label: "N_{D_3}((x,y),\\rho)",
    center: [0, 0],
    radius,
  });
  for (const [a, b] of [
    [1, 0],
    [-1, 0],
    [0, 1],
    [0, -1],
  ] as const) {
    const halfPlane = sub.OpenHalfPlane({ coefficients: [a, b, radius] });
    sub.SetInFamily(halfPlane, family);
    sub.SetInFamily(halfPlane, subbasis);
  }
  sub.TopologyOn(tau, plane);
  sub.BasisFor(basis, tau);
  sub.SubbasisFor(subbasis, tau);
  sub.SetInFamily(square, basis);
  sub.FiniteIntersectionOf(square, family);
  sub.Member(point, square);
  return sub.make();
}

/** A nested neighborhood chain illustrates the mutual local refinement of the bases. */
export function triangleDiskBasisSubstance(radius = 1) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("Neighborhood radius must be finite and positive");
  const sub = topology.substance();
  const plane = sub.Set({ label: "\\mathbb{R}^2" });
  const euclidean = sub.Topology({ label: "\\tau_D" });
  const triangular = sub.Topology({ label: "\\tau_{\\triangle}" });
  const disks = sub.Basis({ label: "\\mathcal{B}_D" });
  const triangles = sub.Basis({ label: "\\mathcal{B}_{\\triangle}" });
  const point = sub.CoordinatePoint({ label: "(x,y)", coordinates: [0, 0] });
  const small = sub.OpenDisk({
    label: "N_D((x,y),\\rho_1)",
    center: [0, 0],
    radius: radius * 0.32,
  });
  const large = sub.OpenDisk({
    label: "N_D((x,y),\\rho_2)",
    center: [0, 0],
    radius,
  });
  const triangle = sub.OpenTriangle({
    label: "T",
    vertices: [
      [-0.98 * radius, -0.09 * radius],
      [0.37 * radius, 0.9 * radius],
      [0.6 * radius, -0.78 * radius],
    ],
  });
  sub.TopologyOn(euclidean, plane);
  sub.TopologyOn(triangular, plane);
  sub.BasisFor(disks, euclidean);
  sub.BasisFor(triangles, triangular);
  sub.EqualTopologies(euclidean, triangular);
  sub.SetInFamily(small, disks);
  sub.SetInFamily(large, disks);
  sub.SetInFamily(triangle, triangles);
  sub.Member(point, small);
  sub.Member(point, triangle);
  sub.Member(point, large);
  sub.Subset(small, triangle);
  sub.Subset(triangle, large);
  return sub.make();
}

export const buildIntervalSubbasisFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: intervalSubbasisSubstance(),
    sty: intervalSubbasisStyle(),
    canvas: canvas(400, 100),
    variation: "gemignani-3.1",
    ...renderOptions,
  });
export const buildHalfPlaneSquareFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: halfPlaneSquareSubstance(),
    sty: halfPlaneIntersectionStyle(),
    canvas: canvas(360, 285),
    variation: "gemignani-3.2",
    ...renderOptions,
  });
export const buildTriangleDiskBasisFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: triangleDiskBasisSubstance(),
    sty: triangleDiskBasisStyle(),
    canvas: canvas(240, 240),
    variation: "gemignani-3.3",
    ...renderOptions,
  });
