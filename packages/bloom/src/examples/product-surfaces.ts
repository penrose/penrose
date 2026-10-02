import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { compactProductSurfaceStyle } from "../styles/product-surfaces.js";

/** Example 12: I×C, its two boundary circles, and a marked interval fiber. */
export function compactCylinderSubstance(fixedAngle = Math.PI) {
  if (!Number.isFinite(fixedAngle))
    throw new Error("The circle coordinate must be finite");
  const s = topology.substance();
  const I = s.ClosedInterval({
    label: "I",
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
  });
  const C = s.CircleBoundary({ label: "C", center: [0, 0], radius: 1 });
  const cylinder = s.ProductSet({ label: "I\\times C" });
  const tauI = s.Topology({ label: "\\tau_I" });
  const tauC = s.Topology({ label: "\\tau_C" });
  const tau = s.Topology({ label: "\\tau_{\\times}" });
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauC, C);
  s.TopologyOn(tau, cylinder);
  s.Compact(tauI);
  s.Compact(tauC);
  s.Compact(tau);
  s.ProductOf(cylinder, I, C);
  s.ProductTopologyOf(tau, tauI, tauC);
  const zero = s.RealPoint({ label: "0", coordinate: 0 });
  const one = s.RealPoint({ label: "1", coordinate: 1 });
  const zeroSet = s.Singleton({ label: "\\{0\\}" });
  const oneSet = s.Singleton({ label: "\\{1\\}" });
  const bottom = s.ProductSet({ label: "\\{0\\}\\times C" });
  const top = s.ProductSet({ label: "\\{1\\}\\times C" });
  s.SingletonOf(zeroSet, zero);
  s.SingletonOf(oneSet, one);
  s.Member(zero, zeroSet);
  s.Member(one, oneSet);
  s.Member(zero, I);
  s.Member(one, I);
  s.Subset(zeroSet, I);
  s.Subset(oneSet, I);
  s.ProductOf(bottom, zeroSet, C);
  s.ProductOf(top, oneSet, C);
  s.Subset(bottom, cylinder);
  s.Subset(top, cylinder);
  s.CompactIn(bottom, tau);
  s.CompactIn(top, tau);
  const x = s.CoordinatePoint({
    label: "x",
    coordinates: [Math.cos(fixedAngle), Math.sin(fixedAngle)],
  });
  const xSet = s.Singleton({ label: "\\{x\\}" });
  const fiber = s.ProductSet({ label: "I\\times\\{x\\}" });
  s.Member(x, C);
  s.Member(x, xSet);
  s.SingletonOf(xSet, x);
  s.Subset(xSet, C);
  s.ProductOf(fiber, I, xSet);
  s.Subset(fiber, cylinder);
  s.CompactIn(fiber, tau);
  return s.make();
}

/** Example 12: the cube I³ with its first-coordinate-zero and third-coordinate-one faces. */
export function compactCubeSubstance(a = 0, b = 1) {
  if (![a, b].every(Number.isFinite) || !(a < b))
    throw new Error(
      "The compact interval must have finite increasing endpoints",
    );
  const s = topology.substance();
  const I = s.ClosedInterval({
    label: "I",
    a,
    b,
    leftClosed: true,
    rightClosed: true,
  });
  const square = s.ProductSet({ label: "I\\times I" });
  const cube = s.ProductSet({ label: "I\\times I\\times I" });
  const tauI = s.Topology({ label: "\\tau_I" });
  const tauSquare = s.Topology({ label: "\\tau_{I^2}" });
  const tauCube = s.Topology({ label: "\\tau_{I^3}" });
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauSquare, square);
  s.TopologyOn(tauCube, cube);
  s.Compact(tauI);
  s.Compact(tauSquare);
  s.Compact(tauCube);
  s.ProductOf(square, I, I);
  s.ProductOf(cube, square, I);
  s.ProductTopologyOf(tauSquare, tauI, tauI);
  s.ProductTopologyOf(tauCube, tauSquare, tauI);
  const zero = s.RealPoint({ label: String(a), coordinate: a });
  const one = s.RealPoint({ label: String(b), coordinate: b });
  const zeroSet = s.Singleton({ label: "\\{" + a + "\\}" });
  const oneSet = s.Singleton({ label: "\\{" + b + "\\}" });
  s.SingletonOf(zeroSet, zero);
  s.SingletonOf(oneSet, one);
  s.Member(zero, zeroSet);
  s.Member(one, oneSet);
  s.Member(zero, I);
  s.Member(one, I);
  s.Subset(zeroSet, I);
  s.Subset(oneSet, I);
  const firstFaceBase = s.ProductSet({ label: zeroSet.label + "\\times I" });
  const firstFace = s.ProductSet({
    label: zeroSet.label + "\\times I\\times I",
  });
  const lastFace = s.ProductSet({ label: "I\\times I\\times" + oneSet.label });
  s.ProductOf(firstFaceBase, zeroSet, I);
  s.ProductOf(firstFace, firstFaceBase, I);
  s.ProductOf(lastFace, square, oneSet);
  s.Subset(firstFace, cube);
  s.Subset(lastFace, cube);
  s.CompactIn(firstFace, tauCube);
  s.CompactIn(lastFace, tauCube);
  return s.make();
}

export const buildCompactCylinderFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: compactCylinderSubstance(),
    sty: compactProductSurfaceStyle(),
    canvas: canvas(108, 148),
    variation: "gemignani-7.4",
    ...renderOptions,
  });

export const buildCompactCubeFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: compactCubeSubstance(),
    sty: compactProductSurfaceStyle({ offset: [0, -4] }),
    canvas: canvas(118, 130),
    variation: "gemignani-7.6",
    ...renderOptions,
  });
