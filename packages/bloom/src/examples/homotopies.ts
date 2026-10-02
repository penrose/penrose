import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  radialContractionValue,
  pointSetTopology as topology,
  type PlaneCoordinates,
} from "../domains/point-set-topology.js";
import {
  diskContractionStyle,
  homotopyFamilyStyle,
  radialHomotopyStyle,
} from "../styles/homotopies.js";

/** The closed unit disk, with the usual subspace topology, is contractible. */
export function contractibleDiskSubstance() {
  const s = topology.substance();
  const Y = s.ClosedDisk({ center: [0, 0], radius: 1, label: "Y" });
  const tau = s.Topology({ label: "\\tau_Y" });
  const origin = s.CoordinatePoint({ coordinates: [0, 0], label: "(0,0)" });
  s.TopologyOn(tau, Y);
  s.Contractible(tau);
  s.Member(origin, Y);
  return s.make();
}

/** A radial contraction sends p to r·p and the closed disk to a concentric disk. */
export function radialDiskContractionSubstance(
  factor = 0.5,
  point: PlaneCoordinates = [0.53, 0.53],
) {
  if (
    !Number.isFinite(factor) ||
    !(factor > 0 && factor < 1) ||
    !point.every(Number.isFinite) ||
    Math.hypot(...point) > 1
  )
    throw new Error(
      "The illustrated contraction needs 0<r<1 and a point in the closed unit disk",
    );
  const s = topology.substance();
  const Y = s.ClosedDisk({ center: [0, 0], radius: 1, label: "Y" });
  const tau = s.Topology({ label: "\\tau_Y" });
  const origin = s.CoordinatePoint({ coordinates: [0, 0], label: "(0,0)" });
  const parameter = s.RealPoint({
    coordinate: factor,
    label: factor === 0.5 ? "1/2" : String(factor),
  });
  const contraction = s.RadialContraction({
    center: Y.center,
    factor,
    label: "j_{" + parameter.label + "}",
    formula: "(x,y)↦(" + factor + "x," + factor + "y)",
  });
  const image = s.ClosedDisk({
    center: Y.center,
    radius: factor,
    label: contraction.label + "(Y)",
  });
  const p = s.CoordinatePoint({ coordinates: point, label: "(x,y)" });
  const q = s.CoordinatePoint({
    coordinates: radialContractionValue(point, Y.center, factor),
    label: contraction.label + "(x,y)",
  });
  const boundary = s.CoordinatePoint({ coordinates: [1, 0], label: "(1,0)" });
  const boundaryImage = s.CoordinatePoint({
    coordinates: [factor, 0],
    label: factor === 0.5 ? "(\\tfrac12,0)" : "(" + factor + ",0)",
  });
  s.TopologyOn(tau, Y);
  s.Contractible(tau);
  s.RadialContractionOf(contraction, Y, parameter);
  s.MapBetween(contraction, Y, Y);
  s.ContinuousMap(contraction, tau, tau);
  s.ImageOf(image, contraction, Y);
  s.Subset(image, Y);
  for (const v of [origin, p, q, boundary, boundaryImage]) s.Member(v, Y);
  s.Member(q, image);
  s.Member(boundaryImage, image);
  s.MapsTo(contraction, p, q);
  s.MapsTo(contraction, boundary, boundaryImage);
  s.MapsTo(contraction, origin, origin);
  return s.make();
}

/** A continuous radial family has identity at r=1 and the center-valued map at r=0. */
export function radialHomotopySubstance(parameter = 0.35, intermediate = 0.68) {
  if (!(0 < parameter && parameter < intermediate && intermediate < 1))
    throw new Error("The illustrated radial slices need 0<r<intermediate<1");
  const s = topology.substance();
  const Y = s.ClosedDisk({ center: [0, 0], radius: 1, label: "Y" });
  const I = s.ClosedInterval({
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
    label: "[0,1]",
  });
  const total = s.ProductSet({ label: "Y\\times[0,1]" });
  const tauY = s.Topology({ label: "\\tau_Y" }),
    tauI = s.Topology({ label: "\\tau_I" }),
    tauTotal = s.Topology({ label: "\\tau_{Y\\times I}" });
  const origin = s.CoordinatePoint({ coordinates: Y.center, label: "(0,0)" });
  const identity = s.TopologicalMap({ label: "j_1", formula: "(x,y)↦(x,y)" });
  const constant = s.TopologicalMap({ label: "j_0", formula: "(x,y)↦(0,0)" });
  const family = s.RadialHomotopy({
    center: Y.center,
    label: "j",
    formula: "((x,y),r)↦(rx,ry)",
  });
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauTotal, total);
  s.ProductOf(total, Y, I);
  s.ProductTopologyOf(tauTotal, tauY, tauI);
  s.Member(origin, Y);
  s.Contractible(tauY);
  s.IdentityOn(identity, Y);
  s.ConstantTo(constant, origin);
  s.MapBetween(identity, Y, Y);
  s.MapBetween(constant, Y, Y);
  s.MapBetween(family, total, Y);
  s.ContinuousMap(identity, tauY, tauY);
  s.ContinuousMap(constant, tauY, tauY);
  s.ContinuousMap(family, tauTotal, tauY);
  s.HomotopyBetween(family, identity, constant);
  s.Homotopic(identity, constant);
  const zero = s.RealPoint({ coordinate: 0, label: "0" });
  const one = s.RealPoint({ coordinate: 1, label: "1" });
  s.Member(zero, I);
  s.Member(one, I);
  s.SliceMapAt(constant, family, zero);
  s.SliceMapAt(identity, family, one);
  s.ImageOf(Y, identity, Y);
  const singleton = s.Singleton({ label: "\\{(0,0)\\}" });
  s.SingletonOf(singleton, origin);
  s.Subset(singleton, Y);
  s.ImageOf(singleton, constant, Y);
  for (const [value, label] of [
    [parameter, "r"],
    [intermediate, ""],
  ] as const) {
    const r = s.RealPoint({ coordinate: value, label });
    const map = s.RadialContraction({
      center: Y.center,
      factor: value,
      label: label ? "j_r" : "j_{" + value + "}",
    });
    const image = s.ClosedDisk({
      center: Y.center,
      radius: value,
      label: label ? "j_r(Y)" : "",
    });
    const rim = s.CoordinatePoint({
      coordinates: [value, 0],
      label: label ? "(r,0)" : "",
    });
    s.Member(r, I);
    s.RadialContractionOf(map, Y, r);
    s.SliceMapAt(map, family, r);
    s.MapBetween(map, Y, Y);
    s.ContinuousMap(map, tauY, tauY);
    s.ImageOf(image, map, Y);
    s.Subset(image, Y);
    s.Member(rim, image);
    s.Member(origin, image);
    s.MapsTo(map, origin, origin);
  }
  const rim = s.CoordinatePoint({ coordinates: [1, 0], label: "(1,0)" });
  s.Member(rim, Y);
  s.MapsTo(identity, rim, rim);
  return s.make();
}

/** H:X×[0,1]→Y restricts to f at time1 and g at time0. */
export function homotopyFamilySubstance(parameter = 0.5) {
  if (!Number.isFinite(parameter) || !(0 < parameter && parameter < 1))
    throw new Error("An intermediate homotopy slice needs 0<r<1");
  const s = topology.substance();
  const X = s.Set({ label: "X" }),
    Y = s.Set({ label: "Y" });
  const I = s.ClosedInterval({
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
    label: "[0,1]",
  });
  const total = s.ProductSet({ label: "X\\times[0,1]" });
  const tauX = s.Topology({ label: "\\tau_X" }),
    tauY = s.Topology({ label: "\\tau_Y" }),
    tauI = s.Topology({ label: "\\tau_I" }),
    tauTotal = s.Topology({ label: "\\tau_{X\\times I}" });
  s.TopologyOn(tauX, X);
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauTotal, total);
  s.ProductOf(total, X, I);
  s.ProductTopologyOf(tauTotal, tauX, tauI);
  const f = s.TopologicalMap({ label: "f" }),
    g = s.TopologicalMap({ label: "g" }),
    h = s.TopologicalMap({ label: "H_r" });
  const H = s.Homotopy({ label: "H" });
  for (const map of [f, g, h]) {
    s.MapBetween(map, X, Y);
    s.ContinuousMap(map, tauX, tauY);
  }
  s.MapBetween(H, total, Y);
  s.ContinuousMap(H, tauTotal, tauY);
  s.HomotopyBetween(H, f, g);
  s.Homotopic(f, g);
  for (const [value, parameterLabel, map, imageLabel] of [
    [0, "0", g, "g(X)"],
    [parameter, "r", h, "H(X\\times\\{r\\})"],
    [1, "1", f, "f(x)"],
  ] as const) {
    const r = s.RealPoint({ coordinate: value, label: parameterLabel });
    const singleton = s.Singleton({ label: "\\{" + parameterLabel + "\\}" });
    const slice = s.ProductSet({
      label: "X\\times\\{" + parameterLabel + "\\}",
    });
    const image = s.Set({ label: imageLabel });
    s.Member(r, I);
    s.SingletonOf(singleton, r);
    s.Subset(singleton, I);
    s.ProductOf(slice, X, singleton);
    s.Subset(slice, total);
    s.SliceMapAt(map, H, r);
    s.ImageOf(image, map, X);
    s.Subset(image, Y);
  }
  return s.make();
}

export const buildContractibleDiskFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: contractibleDiskSubstance(),
    sty: diskContractionStyle(),
    canvas: canvas(164, 164),
    variation: "gemignani-11.1",
    ...renderOptions,
  });
export const buildRadialDiskContractionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: radialDiskContractionSubstance(),
    sty: diskContractionStyle({ offset: [-3, 0] }),
    canvas: canvas(192, 196),
    variation: "gemignani-11.2",
    ...renderOptions,
  });
export const buildRadialHomotopyFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: radialHomotopySubstance(),
    sty: radialHomotopyStyle(),
    canvas: canvas(138, 154),
    variation: "gemignani-11.3",
    ...renderOptions,
  });
export const buildHomotopyFamilyFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: homotopyFamilySubstance(),
    sty: homotopyFamilyStyle({ offset: [-8, 0] }),
    canvas: canvas(248, 164),
    variation: "gemignani-11.4",
    ...renderOptions,
  });
