import { expect, test } from "vitest";
import { domain } from "../core/program.js";
import {
  createFiniteDirectedSet,
  createFiniteTopology,
  finiteClosure,
  finiteConnectedComponents,
  finiteDerivedSet,
  finiteExterior,
  finiteFrontier,
  finiteGeneratedFilter,
  finiteInterior,
  finiteMapIsContinuous,
  finiteNetConvergesTo,
  finiteSubspaceTopology,
  finiteTopologySeparation,
  isFiniteCoverRefinement,
} from "./finite-topology.js";
import { pointSetTopology as topology } from "./point-set-topology.js";
import {
  createFiniteFunction,
  declareElementarySetTheory,
  finitePowerSet,
} from "./set-theory.js";
import {
  affineRealContraction,
  contractionData,
  declareTopologicalVocabularyWithGroups,
} from "./topological-vocabulary.js";

test("Sierpiński topology distinguishes closure, derived set, interior, and T0 from T1", () => {
  const tau = createFiniteTopology([0, 1], [[], [1], [0, 1]]);
  expect(finiteClosure(tau, [1])).toEqual([0, 1]);
  expect(finiteDerivedSet(tau, [1])).toEqual([0]);
  expect(finiteDerivedSet(tau, [0])).toEqual([]);
  expect(finiteInterior(tau, [1])).toEqual([1]);
  expect(finiteInterior(tau, [0])).toEqual([]);
  expect(finiteFrontier(tau, [1])).toEqual([0]);
  expect(finiteExterior(tau, [0])).toEqual([1]);
  expect(finiteTopologySeparation(tau)).toEqual({
    T0: true,
    T1: false,
    T2: false,
  });
  expect(finiteConnectedComponents(tau)).toEqual([[0, 1]]);
  expect(finiteSubspaceTopology(tau, [0]).openSets).toEqual([[], [0]]);
  expect(() =>
    createFiniteTopology([0, 1, 2], [[], [0], [1], [0, 1, 2]]),
  ).toThrow(/unions/);
  expect(() => finiteClosure(tau, [2])).toThrow(/ambient/);
});

test("discrete and indiscrete spaces give opposite continuity behavior and connected components", () => {
  const X = [0, 1],
    discrete = createFiniteTopology(X, finitePowerSet(X)),
    indiscrete = createFiniteTopology(X, [[], X]);
  const identity = createFiniteFunction(X, X, (x) => x);
  expect(finiteMapIsContinuous(identity, discrete, indiscrete)).toBe(true);
  expect(finiteMapIsContinuous(identity, indiscrete, discrete)).toBe(false);
  expect(finiteTopologySeparation(discrete)).toEqual({
    T0: true,
    T1: true,
    T2: true,
  });
  expect(finiteConnectedComponents(discrete)).toEqual([[0], [1]]);
  expect(finiteConnectedComponents(indiscrete)).toEqual([[0, 1]]);
  expect(finiteClosure(discrete, [0])).toEqual([0]);
});

test("cover refinement is containment rather than a requirement to reuse cover members", () => {
  const X = [0, 1, 2];
  expect(
    isFiniteCoverRefinement(
      X,
      [[0], [1], [2]],
      [
        [0, 1],
        [1, 2],
      ],
    ),
  ).toBe(true);
  expect(
    isFiniteCoverRefinement(
      X,
      [[0, 2], [1]],
      [
        [0, 1],
        [1, 2],
      ],
    ),
  ).toBe(false);
  expect(
    isFiniteCoverRefinement(
      X,
      [[0], [1]],
      [
        [0, 1],
        [1, 2],
      ],
    ),
  ).toBe(false);
});

test("finite filters are principal, and an ultrafilter chooses every subset or its complement", () => {
  const X = [0, 1, 2],
    filter = finiteGeneratedFilter(X, [
      [0, 1],
      [0, 2],
    ]);
  expect(filter.kernel).toEqual([0]);
  expect(filter.isUltrafilter).toBe(true);
  for (const A of finitePowerSet(X)) {
    const complement = X.filter((x) => !A.includes(x));
    expect(filter.contains(A) !== filter.contains(complement)).toBe(true);
  }
  const ordinary = finiteGeneratedFilter(X, [[0, 1]]);
  expect(ordinary.isUltrafilter).toBe(false);
  expect(ordinary.contains([0, 1, 2])).toBe(true);
  expect(ordinary.contains([0])).toBe(false);
  expect(() => finiteGeneratedFilter(X, [[0], [1]])).toThrow(/empty/);
});

test("finite directed nets use whole tails and can have multiple limits in a non-Hausdorff space", () => {
  const order = createFiniteDirectedSet([0, 1, 2], (a, b) => a <= b);
  const net = createFiniteFunction(order.elements, [0, 1], (i) =>
    i === 2 ? 1 : 0,
  );
  const tau = createFiniteTopology([0, 1], [[], [1], [0, 1]]);
  expect(finiteNetConvergesTo(net, order, tau, 1)).toBe(true);
  expect(finiteNetConvergesTo(net, order, tau, 0)).toBe(true);
  const discrete = createFiniteTopology([0, 1], finitePowerSet([0, 1]));
  expect(finiteNetConvergesTo(net, order, discrete, 0)).toBe(false);
  expect(finiteNetConvergesTo(net, order, discrete, 1)).toBe(true);
  expect(() => createFiniteDirectedSet([0, 1], (a, b) => a === b)).toThrow(
    /upper bound/,
  );
});

test("affine contraction iterates satisfy the algebraic error identity, including alternating and constant maps", () => {
  for (const [a, b] of [
    [0.5, 1],
    [-0.75, 0.2],
    [0, -3],
  ]) {
    const f = affineRealContraction(a, b);
    const iterates = f.iterates(5, 25);
    expect(f.apply(f.fixedPoint)).toBeCloseTo(f.fixedPoint, 12);
    for (let n = 0; n < iterates.length; n++)
      expect(Math.abs(iterates[n] - f.fixedPoint)).toBeCloseTo(
        f.errorBound(5, n),
        12,
      );
    expect(f.data).toEqual({
      slope: a,
      intercept: b,
      lipschitzConstant: Math.abs(a),
    });
  }
  expect(() => contractionData(1)).toThrow(/0 ≤ k < 1/);
  expect(() => contractionData(-0.1)).toThrow();
  expect(() => affineRealContraction(-1, 0)).toThrow();
  expect(() => affineRealContraction(0.5, Infinity)).toThrow();
  expect(() => affineRealContraction(0.5, 1).errorBound(5, 2000)).toThrow(
    /underflows/,
  );
  expect(affineRealContraction(0.5, 1).errorBound(2, 2000)).toBe(0);
});

test("fundamental-group classes are set-valued group elements and induced maps retain homomorphism types", () => {
  const s = topology.substance(),
    tau = s.Topology(),
    p = s.Point({ label: "y_0" });
  const loop = s.Loop({ label: "a" }),
    cls = s.LoopHomotopyClass({ label: "|a|" }),
    G = s.FundamentalGroup({ label: "π_1(Y,y_0)" });
  const f = s.TopologicalMap({ label: "f" }),
    fStar = s.InducedHomomorphism({ label: "f_*" });
  s.LoopBasedAt(loop, p, tau);
  s.LoopClassOf(cls, loop, tau, p);
  s.FundamentalGroupOf(G, tau, p);
  s.Member(cls, G);
  s.ClassInFundamentalGroup(cls, G);
  s.GroupHomomorphismBetween(fStar, G, G);
  s.InducedHomomorphismOf(fStar, f, G, G);
  s.Hypothesis(s.SimplyConnected.expression(tau));
  const substance = s.make();
  expect(substance.propositions).toHaveLength(8);
  expect(
    substance.propositions.some(
      (fact) => fact.predicate === topology.SimplyConnected,
    ),
  ).toBe(false);
  expect(loop).not.toHaveProperty("coordinates");
  void (() => {
    // @ts-expect-error An unbased topological map is not a declared loop.
    s.LoopBasedAt(f, p, tau);
    // @ts-expect-error A loop is not a homotopy class/group element.
    s.ClassInFundamentalGroup(loop, G);
    // @ts-expect-error An ordinary function is not a group homomorphism.
    s.GroupHomomorphismBetween(f, G, G);
  });
});

test("full elementary set theory and topology compose with one shared algebra vocabulary", () => {
  const d = domain("combined-mathematics"),
    sets = declareElementarySetTheory(d);
  const RealPoint = d
    .type("RealPoint", sets.Point)
    .withData<{ value: number }>();
  const Topology = d.type("Topology"),
    TopologicalMap = d.type("TopologicalMap", sets.Function);
  const CountableSetFamily = d.type("CountableSetFamily", sets.SetFamily);
  const Basis = d.type("Basis", sets.SetFamily),
    OpenCover = d.type("OpenCover", sets.SetFamily);
  const NeighborhoodSystem = d.type("NeighborhoodSystem"),
    Neighborhood = d.type("Neighborhood", sets.Set);
  const Metric = d.type("Metric"),
    DirectedSet = d.type("DirectedSet"),
    Net = d.type("Net"),
    Filter = d.type("Filter"),
    FilterBase = d.type("FilterBase");
  const Homotopy = d.type("Homotopy", TopologicalMap),
    TopologicalPath = d.type("TopologicalPath", TopologicalMap);
  const base = {
    Set: sets.Set,
    Point: sets.Point,
    RealPoint,
    Topology,
    SetFamily: sets.SetFamily,
    CountableSetFamily,
    Basis,
    OpenCover,
    NeighborhoodSystem,
    Neighborhood,
    TopologicalMap,
    Metric,
    DirectedSet,
    Net,
    Filter,
    FilterBase,
    Homotopy,
    TopologicalPath,
  };
  const extra = declareTopologicalVocabularyWithGroups(d, base, sets);
  const combined = d.make({ ...sets, ...base, ...extra });
  const s = combined.substance(),
    tau = s.Topology(),
    p = s.Point(),
    G = s.FundamentalGroup(),
    fStar = s.InducedHomomorphism();
  s.FundamentalGroupOf(G, tau, p);
  s.GroupHomomorphismBetween(fStar, G, G);
  s.OneToOne(fStar);
  const example = affineRealContraction(0.5, 1),
    f = s.AffineRealContraction(example.data),
    sequence = s.IterationSequence(),
    x0 = s.RealIterationSample({ value: 5, index: 0 });
  s.IteratesOf(sequence, f, x0);
  s.IterationSampleOf(x0, sequence);
  expect(combined.Group).toBe(sets.Group);
  expect(s.make().propositions).toHaveLength(5);
  void (() => {
    // @ts-expect-error Groups are not maps despite sharing a Set parent.
    s.OneToOne(G);
  });
});
