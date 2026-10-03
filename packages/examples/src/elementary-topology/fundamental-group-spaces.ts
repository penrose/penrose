import type { FigureRenderOptions } from "@penrose/bloom";
import {
  basepointChangeStyle,
  canvas,
  contractiblePointEquivalenceStyle,
  diagram,
  planarGraphSpacesStyle,
  sphereLoopContractionStyle,
  pointSetTopology as topology,
  torusGeneratorStyle,
} from "@penrose/bloom";

/** A loop avoiding P contracts in the punctured sphere; the whole sphere does not contract. */
export function puncturedSphereLoop() {
  const s = topology.substance(),
    Y = s.Sphere({ label: "S^2", center: [0, 0, 0], radius: 1 }),
    tau = s.Topology({ label: "\\tau_{S^2}" }),
    puncture = s.PuncturedSphere({ label: "S^2\\setminus\\{P\\}" }),
    tauP = s.Topology({ label: "\\tau_P" });
  const P = s.EuclideanVector({ label: "P", coordinates: [0, 0, 1] }),
    base = s.EuclideanVector({ label: "y_0", coordinates: [-0.6, -0.8, 0] }),
    point = s.Singleton({ label: "\\{P\\}" });
  const loop = s.Loop({ label: "a" }),
    constant = s.ConstantLoop({ label: "k" }),
    image = s.Set({ label: "a([0,1])" }),
    unit = s.ClosedInterval({
      label: "[0,1]",
      a: 0,
      b: 1,
      leftClosed: true,
      rightClosed: true,
    }),
    H = s.Homotopy({ label: "H" });
  s.TopologyOn(tau, Y);
  s.SimplyConnected(tau);
  s.NotContractible(tau);
  s.SingletonOf(point, P);
  s.Member(P, Y);
  s.Member(base, Y);
  s.Member(base, puncture);
  s.SpherePunctureOf(puncture, Y, P);
  s.ComplementOf(puncture, point, Y);
  s.TopologyOn(tauP, puncture);
  s.SubspaceTopologyOf(tauP, puncture, tau);
  s.Contractible(tauP);
  s.ImageOf(image, loop, unit);
  s.Subset(image, puncture);
  s.Outside(P, image);
  s.LoopBasedAt(loop, base, tau);
  s.ConstantLoopAt(constant, base, tau);
  s.LoopBasedAt(constant, base, tau);
  s.NullHomotopic(loop, tau, base);
  s.HomotopyBetween(H, loop, constant);
  s.SphereLoopAvoids(loop, P, Y);
  s.SphereLoopContractionOf(H, loop, Y, P, base);
  const group = s.FundamentalGroup({ label: "\\pi_1(S^2,y_0)" }),
    trivial = s.TrivialGroup({ label: "\\{e\\}" }),
    identity = s.LoopHomotopyClass({ label: "|k|" });
  s.FundamentalGroupOf(group, tau, base);
  s.GroupIsomorphicTo(group, trivial);
  s.LoopClassOf(identity, constant, tau, base);
  s.ClassInFundamentalGroup(identity, group);
  s.IdentityElement(identity, group);
  return s.make();
}
/** The two unit-winding factor loops of C×C represent the two generators of Z⊕Z. */
export function torusFundamentalGenerators() {
  const s = topology.substance(),
    C = s.CircleBoundary({ label: "C", center: [0, 0], radius: 1 }),
    torus = s.ProductSet({ label: "C\\times C" }),
    tC = s.Topology({ label: "\\tau_C" }),
    tau = s.Topology({ label: "\\tau_{\\times}" }),
    factor = s.CoordinatePoint({ label: "p", coordinates: [1, 0] }),
    base = s.ProductPoint({ label: "y_0" });
  s.TopologyOn(tC, C);
  s.TopologyOn(tau, torus);
  s.ProductOf(torus, C, C);
  s.ProductTopologyOf(tau, tC, tC);
  s.Member(factor, C);
  s.ProductPairOf(base, factor, factor);
  s.Member(base, torus);
  const a = s.TorusFactorLoop({ label: "a", factor: "first", winding: 1 }),
    b = s.TorusFactorLoop({ label: "b", factor: "second", winding: 1 }),
    group = s.FundamentalGroup({ label: "\\pi_1(C\\times C,y_0)" }),
    z = s.InfiniteCyclicGroup({ label: "\\mathbb Z" }),
    sum = s.DirectSum({ label: "\\mathbb Z\\oplus\\mathbb Z" }),
    abelian = s.FreeAbelianGroup({ label: "\\mathbb Z^2", rank: 2 });
  s.LoopBasedAt(a, base, tau);
  s.LoopBasedAt(b, base, tau);
  s.FactorLoopOn(a, torus, base);
  s.FactorLoopOn(b, torus, base);
  s.FundamentalGroupOf(group, tau, base);
  s.DirectSumOf(sum, z, z);
  s.GroupIsomorphicTo(group, sum);
  s.GroupIsomorphicTo(group, abelian);
  for (const loop of [a, b]) {
    const c = s.LoopHomotopyClass({ label: `|${loop.label}|` });
    s.LoopClassOf(c, loop, tau, base);
    s.ClassInFundamentalGroup(c, group);
  }
  return s.make();
}
/** Transport a based loop along j from y1 to y0, then traverse the same arc backwards. */
export function transportedLoopBasepoint() {
  const s = topology.substance(),
    Y = s.Subspace({ label: "Y" }),
    tau = s.Topology({ label: "\\tau_Y" }),
    y0 = s.Point({ label: "y_0" }),
    y1 = s.Point({ label: "y_1" }),
    j = s.Arc({ label: "j" }),
    reverse = s.TopologicalPath({ label: "j^{-1}" }),
    a = s.Loop({ label: "a" }),
    transport = s.Loop({ label: "j\\#a\\#j^{-1}" });
  const g0 = s.FundamentalGroup({ label: "\\pi_1(Y,y_0)" }),
    g1 = s.FundamentalGroup({ label: "\\pi_1(Y,y_1)" }),
    iso = s.BasepointChangeIsomorphism({ label: "f" });
  s.TopologyOn(tau, Y);
  s.PathConnected(tau);
  s.Member(y0, Y);
  s.Member(y1, Y);
  s.PathEndpointsOf(j, y1, y0);
  s.PathEndpointsOf(reverse, y0, y1);
  s.ReversedPathOf(reverse, j);
  s.LoopBasedAt(a, y0, tau);
  s.LoopBasedAt(transport, y1, tau);
  s.BasepointConjugateOf(transport, a, j);
  s.FundamentalGroupOf(g0, tau, y0);
  s.FundamentalGroupOf(g1, tau, y1);
  s.BasepointChangeAlong(iso, j, g0, g1);
  s.GroupIsomorphismBetween(iso, g0, g1);
  return s.make();
}
/** Correct domains: g∘f is the constant map on X, while f∘g is the identity on singleton Y. */
export function contractibleSingletonEquivalence() {
  const s = topology.substance(),
    X = s.Subspace({ label: "X" }),
    Y = s.Singleton({ label: "\\{P\\}" }),
    tauX = s.Topology({ label: "\\tau_X" }),
    tauY = s.Topology({ label: "\\tau_Y" }),
    P = s.Point({ label: "P" }),
    x0 = s.Point({ label: "x_0" });
  const f = s.TopologicalMap({ label: "f", formula: "f(x)=P" }),
    g = s.TopologicalMap({ label: "g", formula: "g(P)=x_0" }),
    k = s.TopologicalMap({ label: "k", formula: "k(x)=x_0" }),
    ix = s.TopologicalMap({ label: "i_X" }),
    iy = s.TopologicalMap({ label: "i_Y" }),
    H = s.Homotopy({ label: "H" });
  s.TopologyOn(tauX, X);
  s.TopologyOn(tauY, Y);
  s.SingletonOf(Y, P);
  s.Member(P, Y);
  s.Member(x0, X);
  s.Contractible(tauX);
  s.Contractible(tauY);
  s.MapBetween(f, X, Y);
  s.MapBetween(g, Y, X);
  s.ConstantTo(f, P);
  s.ConstantTo(g, x0);
  s.MapsTo(g, P, x0);
  s.ContinuousMap(f, tauX, tauY);
  s.ContinuousMap(g, tauY, tauX);
  s.IdentityOn(ix, X);
  s.IdentityOn(iy, Y);
  s.ConstantTo(k, x0);
  s.MapBetween(k, X, X);
  s.CompositionOf(k, g, f);
  s.CompositionOf(iy, f, g);
  s.HomotopyBetween(H, ix, k);
  s.Homotopic(ix, k);
  s.HomotopyInverseMaps(f, g, tauX, tauY);
  s.HomotopyEquivalent(tauX, tauY);
  return s.make();
}
/** The eleven planar exercise spaces, with reusable finite graph spines. */
export function planarHomotopyExerciseSpaces() {
  const s = topology.substance();
  const subjects = [
    ...(["A", "B", "C", "D", "E", "R", "T", "8", "108"] as const).map(
      (symbol) => ({ kind: "glyph" as const, symbol, label: symbol }),
    ),
    { kind: "animal" as const, label: "animal" },
    { kind: "house" as const, label: "house" },
  ];
  const ranks: Record<string, number> = {
    A: 1,
    B: 2,
    C: 0,
    D: 1,
    E: 0,
    R: 1,
    T: 0,
    "8": 2,
    "108": 3,
    animal: 2,
    house: 2,
  };
  for (const item of subjects) {
    const X = s.PlanarDiagramSpace(item),
      tau = s.Topology({ label: `\\tau_{${item.label}}` }),
      rank = ranks[item.label],
      spine = s.FiniteGraphSpine({
        label: `G_{${item.label}}`,
        vertices: 1,
        edges: rank,
        components: 1,
      }),
      group = s.FundamentalGroup({ label: `\\pi_1(${item.label})` }),
      free = s.FreeGroup({ label: `F_${rank}`, rank }),
      base = s.Point({ label: `p_{${item.label}}` });
    s.TopologyOn(tau, X);
    s.Connected(tau);
    s.PathConnected(tau);
    s.GraphSpineOf(spine, X);
    s.Member(base, X);
    s.FundamentalGroupOf(group, tau, base);
    s.GroupIsomorphicTo(group, free);
    if (rank === 0) {
      s.Contractible(tau);
      s.SimplyConnected(tau);
    }
  }
  return s.make();
}
export const buildSphereLoopFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: puncturedSphereLoop(),
    sty: sphereLoopContractionStyle(),
    canvas: canvas(226, 238),
    variation: "ElementaryTopologyFigure11.19",
    ...renderOptions,
  });
export const buildTorusGeneratorsFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: torusFundamentalGenerators(),
    sty: torusGeneratorStyle(),
    canvas: canvas(210, 131),
    variation: "ElementaryTopologyFigure11.20",
    ...renderOptions,
  });
export const buildBasepointChangeFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: transportedLoopBasepoint(),
    sty: basepointChangeStyle(),
    canvas: canvas(235, 191),
    variation: "ElementaryTopologyFigure11.21",
    ...renderOptions,
  });
export const buildContractibleEquivalenceFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: contractibleSingletonEquivalence(),
    sty: contractiblePointEquivalenceStyle(),
    canvas: canvas(314, 169),
    variation: "ElementaryTopologyFigure11.23",
    ...renderOptions,
  });
export const buildPlanarHomotopyExercisesFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: planarHomotopyExerciseSpaces(),
    sty: planarGraphSpacesStyle({
      interactive: renderOptions.interactive
        ? typeof renderOptions.interactive === "object"
          ? renderOptions.interactive
          : {}
        : undefined,
    }),
    canvas: canvas(330, 118),
    variation: "ElementaryTopologyFigure11.24",
    ...renderOptions,
  });
