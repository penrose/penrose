import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  contractAndSlideValue,
  parabolicArcValue,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  contractAndSlideStyle,
  endpointExtensionStyle,
  fixedEndpointArcsStyle,
  pastedHomotopyStyle,
} from "../styles/homotopy-constructions.js";

/** A coherent family matching the graphic: identity at1, origin at1/2, horizontal constant at0. */
export function contractAndSlideSubstance(targetCoordinate = 0.5) {
  if (
    !Number.isFinite(targetCoordinate) ||
    !(targetCoordinate > 0 && targetCoordinate < 1)
  )
    throw new Error("The horizontal constant point must lie in the unit disk");
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
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauTotal, total);
  s.ProductOf(total, Y, I);
  s.ProductTopologyOf(tauTotal, tauY, tauI);
  const center = s.CoordinatePoint({ coordinates: Y.center, label: "(0,0)" });
  const target = s.CoordinatePoint({
    coordinates: [targetCoordinate, 0],
    label:
      targetCoordinate === 0.5
        ? "(\\tfrac12,0)"
        : "(" + targetCoordinate + ",0)",
  });
  const identity = s.TopologicalMap({ label: "i", formula: "p↦p" }),
    constant = s.TopologicalMap({
      label: "k",
      formula: "p↦(" + targetCoordinate + ",0)",
    });
  const H = s.ContractAndSlideHomotopy({
    center: Y.center,
    target: target.coordinates,
    breakpoint: 0.5,
    label: "H",
    formula: "r≤1/2:(1−2r)k; r≥1/2:(2r−1)p",
  });
  s.Member(center, Y);
  s.Member(target, Y);
  s.Contractible(tauY);
  s.IdentityOn(identity, Y);
  s.ConstantTo(constant, target);
  s.MapBetween(identity, Y, Y);
  s.MapBetween(constant, Y, Y);
  s.MapBetween(H, total, Y);
  s.ContinuousMap(identity, tauY, tauY);
  s.ContinuousMap(constant, tauY, tauY);
  s.ContinuousMap(H, tauTotal, tauY);
  s.HomotopyBetween(H, identity, constant);
  s.Homotopic(identity, constant);
  for (const time of [0, 0.5, 0.625, 0.75, 0.9, 1]) {
    const r = s.RealPoint({
      coordinate: time,
      label:
        time === 0.5 ? "\\tfrac12" : time === 0.75 ? "\\tfrac34" : String(time),
    });
    let map: typeof identity;
    let image: typeof Y | ReturnType<typeof s.Singleton>;
    if (time === 1) {
      map = identity;
      image = Y;
    } else if (time <= 0.5) {
      const point = time === 0 ? target : center;
      map = time === 0 ? constant : s.TopologicalMap({ label: "H_{1/2}" });
      image = s.Singleton({ label: "\\{" + point.label + "\\}" });
      s.ConstantTo(map, point);
      s.SingletonOf(image, point);
      s.MapsTo(map, center, point);
      if (time !== 0) {
        s.MapBetween(map, Y, Y);
        s.ContinuousMap(map, tauY, tauY);
      }
    } else {
      const factor = 2 * time - 1;
      const factorPoint = s.RealPoint({
        coordinate: factor,
        label: String(factor),
      });
      const contraction = s.RadialContraction({
        center: Y.center,
        factor,
        label: "H_{" + time + "}",
      });
      map = contraction;
      image = s.ClosedDisk({
        center: Y.center,
        radius: factor,
        label: "H(Y\\times\\{" + time + "\\})",
      });
      s.RadialContractionOf(contraction, Y, factorPoint);
      s.MapBetween(map, Y, Y);
      s.ContinuousMap(map, tauY, tauY);
    }
    s.Member(r, I);
    s.SliceMapAt(map, H, r);
    s.ImageOf(image, map, Y);
    s.Subset(image, Y);
    const singleton = s.Singleton({ label: "\\{" + r.label + "\\}" });
    const fiber = s.ProductSet({ label: "Y\\times\\{" + r.label + "\\}" });
    s.SingletonOf(singleton, r);
    s.Subset(singleton, I);
    s.ProductOf(fiber, Y, singleton);
    s.Subset(fiber, total);
  }
  const quarter = s.CoordinatePoint({
    coordinates: contractAndSlideValue([targetCoordinate, 0], 0.75),
    label: "",
  });
  s.Member(quarter, Y);
  return s.make();
}

/** The displayed family is relative to {0,1}; its endpoints cannot pass the puncture. */
export function fixedEndpointArcsSubstance(
  upperHeight = 0.55,
  lowerHeight = -0.55,
  punctureHeight = 0.04,
) {
  if (
    ![upperHeight, lowerHeight, punctureHeight].every(Number.isFinite) ||
    !(lowerHeight < punctureHeight && punctureHeight < upperHeight)
  )
    throw new Error(
      "The puncture must lie strictly between the two disjoint arc interiors",
    );
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" }),
    Y = s.PuncturedPlane({ label: "R^2-\\{P\\}" });
  const I = s.ClosedInterval({
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
    label: "[0,1]",
  });
  const boundary = s.EndpointPair({ endpoints: [0, 1], label: "\\{0,1\\}" });
  const product = s.ProductSet({ label: "[0,1]\\times[0,1]" });
  const tauI = s.Topology({ label: "\\tau_I" }),
    tauX = s.Topology({ label: "\\tau_D" }),
    tauY = s.Topology({ label: "\\tau_Y" }),
    tauProduct = s.Topology({ label: "\\tau_{I\\times I}" });
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauX, X);
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauProduct, product);
  s.Subset(Y, X);
  s.SubspaceTopologyOf(tauY, Y, tauX);
  s.ProductOf(product, I, I);
  s.ProductTopologyOf(tauProduct, tauI, tauI);
  s.Subset(boundary, I);
  s.ClosedIn(boundary, tauI);
  const endpoints = [
    [-0.7, -0.2],
    [0.7, 0.3],
  ] as const;
  const start = s.CoordinatePoint({
      coordinates: endpoints[0],
      label: "a_2(0)=a_1(0)",
    }),
    finish = s.CoordinatePoint({
      coordinates: endpoints[1],
      label: "a_1(1)=a_2(1)",
    });
  const P = s.CoordinatePoint({
    coordinates: parabolicArcValue({ endpoints, height: punctureHeight }, 0.5),
    label: "P",
  });
  s.Member(P, X);
  s.Outside(P, Y);
  s.DeletedPointFrom(Y, X, P);
  for (const p of [start, finish]) {
    s.Member(p, X);
    s.Member(p, Y);
  }
  const first = s.ParabolicArc({
      endpoints,
      height: upperHeight,
      label: "a_1",
    }),
    last = s.ParabolicArc({ endpoints, height: lowerHeight, label: "a_2" });
  const restrictedFirst = s.ParabolicArc({
      endpoints,
      height: upperHeight,
      label: "a_1",
    }),
    restrictedLast = s.ParabolicArc({
      endpoints,
      height: lowerHeight,
      label: "a_2",
    });
  for (const arc of [first, last]) {
    s.MapBetween(arc, I, X);
    s.ContinuousMap(arc, tauI, tauX);
    s.PathEndpointsOf(arc, start, finish);
    s.OneToOne(arc);
  }
  for (const arc of [restrictedFirst, restrictedLast]) {
    s.MapBetween(arc, I, Y);
    s.ContinuousMap(arc, tauI, tauY);
    s.PathEndpointsOf(arc, start, finish);
    s.OneToOne(arc);
  }
  s.CorestrictionOf(restrictedFirst, first, Y);
  s.CorestrictionOf(restrictedLast, last, Y);
  const H = s.Homotopy({
    label: "H",
    formula: "Chord(u)+4u(1−u)(r h₁+(1−r)h₂)n",
  });
  s.MapBetween(H, product, X);
  s.ContinuousMap(H, tauProduct, tauX);
  s.HomotopyBetween(H, first, last);
  s.RelativeHomotopyOn(H, boundary);
  s.RelativelyHomotopic(first, last, boundary, tauX);
  s.NotHomotopicRelativeTo(restrictedFirst, restrictedLast, boundary, tauY);
  return s.make();
}

function abstractFamilyBuilder() {
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
  const product = s.ProductSet({ label: "X\\times[0,1]" });
  const tauX = s.Topology({ label: "\\tau_X" }),
    tauY = s.Topology({ label: "\\tau_Y" }),
    tauI = s.Topology({ label: "\\tau_I" }),
    tauProduct = s.Topology({ label: "\\tau_{X\\times I}" });
  s.TopologyOn(tauX, X);
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauProduct, product);
  s.ProductOf(product, X, I);
  s.ProductTopologyOf(tauProduct, tauX, tauI);
  const f = s.TopologicalMap({ label: "f" }),
    g = s.TopologicalMap({ label: "g" }),
    k = s.TopologicalMap({ label: "k" });
  for (const map of [f, g, k]) {
    s.MapBetween(map, X, Y);
    s.ContinuousMap(map, tauX, tauY);
  }
  const H = s.Homotopy({ label: "H" });
  s.MapBetween(H, product, Y);
  s.ContinuousMap(H, tauProduct, tauY);
  return { s, X, Y, I, product, tauX, tauY, tauI, tauProduct, f, g, k, H };
}

/** Pasting H₁:f~g above time1/2 and H₂:g~k below it yields H:f~k. */
export function pastedHomotopySubstance(split = 0.5) {
  if (!Number.isFinite(split) || !(split > 0 && split < 1))
    throw new Error("Pasting needs an interior parameter split");
  const { s, X, Y, I, product, tauY, tauProduct, f, g, k, H } =
    abstractFamilyBuilder();
  const H1 = s.Homotopy({ label: "H_1" }),
    H2 = s.Homotopy({ label: "H_2" });
  for (const map of [H1, H2]) {
    s.MapBetween(map, product, Y);
    s.ContinuousMap(map, tauProduct, tauY);
  }
  const half = s.RealPoint({
    coordinate: split,
    label: split === 0.5 ? "\\tfrac12" : String(split),
  });
  s.Member(half, I);
  s.HomotopyBetween(H1, f, g);
  s.HomotopyBetween(H2, g, k);
  s.HomotopyBetween(H, f, k);
  s.HomotopyPastedFrom(H, H1, H2, half);
  s.Homotopic(f, g);
  s.Homotopic(g, k);
  s.Homotopic(f, k);
  for (const [time, map, imageLabel] of [
    [0, k, "k(X)=H(X\\times\\{0\\})"],
    [split, g, "g(X)=H(X\\times\\{" + half.label + "\\})"],
    [1, f, "f(X)=H(X\\times\\{1\\})"],
  ] as const) {
    const r =
      time === split
        ? half
        : s.RealPoint({ coordinate: time, label: String(time) });
    const singleton = s.Singleton({ label: "\\{" + r.label + "\\}" }),
      fiber = s.ProductSet({ label: "X\\times\\{" + r.label + "\\}" }),
      image = s.Set({ label: imageLabel });
    s.Member(r, I);
    s.SingletonOf(singleton, r);
    s.Subset(singleton, I);
    s.ProductOf(fiber, X, singleton);
    s.Subset(fiber, product);
    s.SliceMapAt(map, H, r);
    s.ImageOf(image, map, X);
    s.Subset(image, Y);
  }
  return s.make();
}

/** The endpoint-boundary map extends to H, with f at1 and g at0 as drawn. */
export function endpointExtensionSubstance() {
  const { s, X, Y, I, product, tauY, tauProduct, f, g, H } =
    abstractFamilyBuilder();
  const boundary = s.EndpointFibers({
    label: "X\\times\\{0\\}\\cup X\\times\\{1\\}",
  });
  const family = s.FiniteSetFamily();
  const h = s.TopologicalMap({
    label: "h",
    formula: "h(x,1)=f(x); h(x,0)=g(x)",
  });
  const tauBoundary = s.Topology({ label: "\\tau_{boundary}" });
  s.TopologyOn(tauBoundary, boundary);
  s.Subset(boundary, product);
  s.ClosedIn(boundary, tauProduct);
  s.EndpointFibersOf(boundary, product);
  s.MapBetween(h, boundary, Y);
  s.ContinuousMap(h, tauBoundary, tauY);
  s.ExtensionOf(H, h, boundary, Y);
  s.HomotopyBetween(H, f, g);
  s.Homotopic(f, g);
  for (const [time, map] of [
    [0, g],
    [1, f],
  ] as const) {
    const r = s.RealPoint({ coordinate: time, label: String(time) });
    const singleton = s.Singleton({ label: "\\{" + time + "\\}" }),
      fiber = s.ProductSet({ label: "X\\times\\{" + time + "\\}" }),
      image = s.Set({ label: map.label + "(X)" });
    s.Member(r, I);
    s.SingletonOf(singleton, r);
    s.Subset(singleton, I);
    s.ProductOf(fiber, X, singleton);
    s.Subset(fiber, product);
    s.SetInFamily(fiber, family);
    s.SliceMapAt(map, H, r);
    s.ImageOf(image, map, X);
    s.Subset(image, Y);
  }
  s.UnionOf(boundary, family);
  return s.make();
}

export const buildContractAndSlideFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: contractAndSlideSubstance(),
    sty: contractAndSlideStyle({
      sourceCylinderOrder: true,
      domainAnnotation: "Y\\times[0,0]",
      offset: [-7, 0],
    }),
    canvas: canvas(374, 176),
    variation: "gemignani-11.5",
    ...renderOptions,
  });
export const buildFixedEndpointArcsFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: fixedEndpointArcsSubstance(),
    sty: fixedEndpointArcsStyle({ offset: [20, 0] }),
    canvas: canvas(166, 132),
    variation: "gemignani-11.6",
    ...renderOptions,
  });
export const buildPastedHomotopyFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: pastedHomotopySubstance(),
    sty: pastedHomotopyStyle({ offset: [-58, 0] }),
    canvas: canvas(360, 182),
    variation: "gemignani-11.7",
    ...renderOptions,
  });
export const buildEndpointExtensionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: endpointExtensionSubstance(),
    sty: endpointExtensionStyle({ offset: [-3, 0] }),
    canvas: canvas(244, 146),
    variation: "gemignani-11.8",
    ...renderOptions,
  });
