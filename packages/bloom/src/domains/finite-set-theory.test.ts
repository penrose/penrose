import { expect, test } from "vitest";
import { domain } from "../core/program.js";
import {
  composeFiniteFunctions,
  createFiniteFunction,
  createFiniteGroup,
  createFinitePartialOrder,
  createFiniteRelation,
  declareElementarySetTheory,
  declareSetTheory,
  factorFiniteFunction,
  finiteCardinality,
  finiteCyclicGroup,
  finiteDirectSum,
  finiteEquivalenceClasses,
  finiteFunctionKernelRelation,
  finiteGroupKernel,
  finiteImage,
  finiteIntersection,
  finiteInverseRelation,
  finiteOrderBounds,
  finitePowerSet,
  finitePreimage,
  finiteQuotientGroup,
  finiteUnion,
  inverseFiniteFunction,
  isFiniteGroupHomomorphism,
  isFiniteGroupIsomorphism,
  isFiniteInjective,
  isFiniteNormalSubgroup,
  isFiniteSubgroup,
  isFiniteSurjective,
  restrictFiniteFunction,
  setTheory,
} from "./set-theory.js";

test("functions distinguish inverse image from an inverse function, and preserve preimage set laws", () => {
  const f = createFiniteFunction([0, 1, 2, 3], [0, 1], (x) => x % 2);
  expect(finitePreimage(f, [0])).toEqual([0, 2]);
  expect(isFiniteInjective(f)).toBe(false);
  expect(isFiniteSurjective(f)).toBe(true);
  expect(finiteInverseRelation(f)).toEqual([
    [0, 0],
    [1, 1],
    [0, 2],
    [1, 3],
  ]);
  expect(() => inverseFiniteFunction(f)).toThrow(/bijection/);
  const A = [0, 2],
    B = [1, 3];
  expect(finiteImage(f, finiteIntersection(A, B))).toEqual([]);
  expect(finiteIntersection(finiteImage(f, A), finiteImage(f, B))).toEqual([]);
  // Images need not preserve intersection: two distinct preimages can coincide.
  expect(finiteImage(f, finiteIntersection([0], [2]))).toEqual([]);
  expect(finiteIntersection(finiteImage(f, [0]), finiteImage(f, [2]))).toEqual([
    0,
  ]);
  expect(new Set(finitePreimage(f, finiteUnion([0], [1])))).toEqual(
    new Set(finiteUnion(finitePreimage(f, [0]), finitePreimage(f, [1]))),
  );
  expect(finitePreimage(f, finiteIntersection([0, 1], [1]))).toEqual(
    finiteIntersection(finitePreimage(f, [0, 1]), finitePreimage(f, [1])),
  );
  expect(() => finiteImage(f, [4])).toThrow(/ambient/);
  expect(() => createFiniteFunction([0], [1], () => 2)).toThrow(/codomain/);
});

test("composition follows f∘g order and bijective inversion, including empty functions", () => {
  const g = createFiniteFunction(
    [1, 2, 3],
    ["a", "b", "c"],
    (x) => ["a", "b", "c"][x - 1],
  );
  const f = createFiniteFunction(
    ["a", "b", "c"],
    [2, 4, 6],
    (x) => ({ a: 2, b: 4, c: 6 })[x as "a" | "b" | "c"],
  );
  const composite = composeFiniteFunctions(f, g),
    inverse = inverseFiniteFunction(composite);
  for (const x of composite.domain)
    expect(inverse.apply(composite.apply(x))).toBe(x);
  expect(restrictFiniteFunction(composite, [1, 3]).graph).toEqual([
    [1, 2],
    [3, 6],
  ]);
  expect(() =>
    composeFiniteFunctions(
      createFiniteFunction(["a"], [2], () => 2),
      g,
    ),
  ).toThrow(/ambient/);
  const empty = createFiniteFunction<number, string>(
    [],
    [],
    () => "unreachable",
  );
  expect(isFiniteInjective(empty)).toBe(true);
  expect(isFiniteSurjective(empty)).toBe(true);
  expect(inverseFiniteFunction(empty).graph).toEqual([]);
  const nan = createFiniteFunction([NaN, NaN, 0, -0], [NaN], () => NaN);
  expect(finiteCardinality(nan.domain)).toBe(2);
  expect(nan.apply(NaN)).toBeNaN();
});

test("a function kernel partitions its domain and the induced map commutes with the quotient projection", () => {
  const f = createFiniteFunction([0, 1, 2, 3, 4, 5], [0, 1], (x) => x % 2);
  const relation = finiteFunctionKernelRelation(f),
    factor = factorFiniteFunction(f, relation);
  expect(factor.classes).toEqual([
    [0, 2, 4],
    [1, 3, 5],
  ]);
  for (const x of f.domain)
    expect(factor.induced.apply(factor.projection.apply(x))).toBe(f.apply(x));
  expect(isFiniteInjective(factor.induced)).toBe(true);
  expect(isFiniteSurjective(factor.induced)).toBe(true);
  expect(Object.isFrozen(factor.classes[0])).toBe(true);
  const identity = createFiniteFunction(f.domain, f.domain, (x) => x);
  expect(() => factorFiniteFunction(identity, relation)).toThrow(/constant/);
  expect(() =>
    finiteEquivalenceClasses(
      createFiniteRelation(
        [0, 1, 2],
        (a, b) => a === b || Math.abs(a - b) === 1,
      ),
    ),
  ).toThrow(/transitive/);
});

test("bounds belong to the ambient order, while maximal and minimal elements belong to the subset", () => {
  const divisibility = createFinitePartialOrder(
    [1, 2, 3, 6],
    (a, b) => b % a === 0,
  );
  const result = finiteOrderBounds(divisibility, [2, 3]);
  expect(result.minimalElements).toEqual([2, 3]);
  expect(result.maximalElements).toEqual([2, 3]);
  expect(result.least).toEqual({ exists: false });
  expect(result.greatest).toEqual({ exists: false });
  expect(result.infimum).toEqual({ exists: true, value: 1 });
  expect(result.supremum).toEqual({ exists: true, value: 6 });
  expect(result.isChain).toBe(false);
  expect(finiteOrderBounds(divisibility, [1, 2, 6]).isChain).toBe(true);
  expect(finiteOrderBounds(divisibility, []).supremum).toEqual({
    exists: true,
    value: 1,
  });
  const noTop = createFinitePartialOrder([1, 2, 3], (a, b) => b % a === 0);
  expect(finiteOrderBounds(noTop, [2, 3]).supremum).toEqual({ exists: false });
  expect(() => createFinitePartialOrder([0, 1], () => true)).toThrow(
    /antisymmetric/,
  );
  expect(() =>
    createFinitePartialOrder([0, 1, 2], (a, b) => a === b || b === a + 1),
  ).toThrow(/transitive/);
});

test("power-set inclusion produces the finite Boolean order with union/intersection as bounds", () => {
  const subsets = finitePowerSet(["x", "y"]);
  const order = createFinitePartialOrder(subsets, (a, b) =>
    a.every((x) => b.includes(x)),
  );
  const singletons = subsets.filter((set) => set.length === 1);
  const result = finiteOrderBounds(order, singletons);
  expect(result.infimum).toEqual({ exists: true, value: [] });
  expect(result.supremum).toEqual({ exists: true, value: ["x", "y"] });
  expect(finiteCardinality(subsets)).toBe(4);
  expect(() => finitePowerSet(Array.from({ length: 17 }, (_, i) => i))).toThrow(
    /16/,
  );
});

test("the source four-element table is a Klein group and its direct-sum model is isomorphic", () => {
  const klein = createFiniteGroup(
    [1, 2, 3, 4],
    (a, b) => ((a - 1) ^ (b - 1)) + 1,
  );
  const product = finiteDirectSum(finiteCyclicGroup(2), finiteCyclicGroup(2));
  expect(klein.identity).toBe(1);
  for (const x of klein.elements) expect(klein.inverse(x)).toBe(x);
  const isomorphism = createFiniteFunction(
    klein.elements,
    product.elements,
    (x) =>
      product.elements.find(
        (pair) => pair[0] === (x - 1) >> 1 && pair[1] === ((x - 1) & 1),
      ) as readonly [number, number],
  );
  expect(isFiniteGroupIsomorphism(isomorphism, klein, product)).toBe(true);
  expect(() => createFiniteGroup([0, 1], (a) => a)).toThrow(/identity/);
  expect(() => createFiniteGroup([0, 1], (a, b) => a + b)).toThrow(/closed/);
  expect(() => createFiniteGroup([0, 1, 2], (a, b) => (a - b + 3) % 3)).toThrow(
    /associative/,
  );
});

test("a noninjective homomorphism has a subgroup kernel; a shifted map fails the group law", () => {
  const source = finiteCyclicGroup(6),
    target = finiteCyclicGroup(3);
  const f = createFiniteFunction(
    source.elements,
    target.elements,
    (x) => x % 3,
  );
  expect(isFiniteGroupHomomorphism(f, source, target)).toBe(true);
  const kernel = finiteGroupKernel(f, source, target);
  expect(kernel).toEqual([0, 3]);
  expect(isFiniteSubgroup(source, kernel)).toBe(true);
  expect(isFiniteSubgroup(source, [])).toBe(false);
  expect(isFiniteSubgroup(source, [0, 1])).toBe(false);
  const shifted = createFiniteFunction(
    source.elements,
    target.elements,
    (x) => (x + 1) % 3,
  );
  expect(isFiniteGroupHomomorphism(shifted, source, target)).toBe(false);
  expect(() => finiteGroupKernel(shifted, source, target)).toThrow(
    /homomorphism/,
  );
});

test("quotient groups require normality and preserve noncommutative composition order", () => {
  const permutations = [
    [0, 1, 2],
    [1, 0, 2],
    [2, 1, 0],
    [0, 2, 1],
    [1, 2, 0],
    [2, 0, 1],
  ];
  const lookup = new Map(permutations.map((p) => [p.join(","), p]));
  const S3 = createFiniteGroup<readonly number[]>(
    permutations,
    (a, b) => lookup.get(b.map((i) => a[i]).join(",")) as readonly number[],
  );
  expect(S3.operation(permutations[1], permutations[4])).not.toEqual(
    S3.operation(permutations[4], permutations[1]),
  );
  const H = [S3.identity, permutations[1]];
  expect(isFiniteSubgroup(S3, H)).toBe(true);
  expect(isFiniteNormalSubgroup(S3, H)).toBe(false);
  expect(() => finiteQuotientGroup(S3, H)).toThrow(/normal subgroup/);
  const Z6 = finiteCyclicGroup(6),
    Z3 = finiteCyclicGroup(3),
    quotient = finiteQuotientGroup(Z6, [0, 3]);
  expect(quotient.group.elements).toEqual([
    [0, 3],
    [1, 4],
    [2, 5],
  ]);
  expect(
    isFiniteGroupHomomorphism(quotient.projection, Z6, quotient.group),
  ).toBe(true);
  const induced = createFiniteFunction(
    quotient.group.elements,
    Z3.elements,
    (coset) => coset[0] % 3,
  );
  expect(isFiniteGroupIsomorphism(induced, quotient.group, Z3)).toBe(true);
});

test("the full vocabulary composes inside another domain and group maps retain function subtyping", () => {
  const builder = domain("algebra-with-topology");
  const base = declareSetTheory(builder),
    vocabulary = declareElementarySetTheory(builder, base);
  const Topology = builder.type("Topology");
  const TopologyOn = builder.predicate("TopologyOn", [Topology, base.Set]);
  const combined = builder.make({ ...vocabulary, Topology, TopologyOn });
  const s = combined.substance(),
    G = s.Group({ label: "G" }),
    H = s.Group({ label: "H" });
  const f = s.GroupIsomorphism({ label: "f" }),
    tau = s.Topology({ label: "τ" });
  s.GroupHomomorphismBetween(f, G, H);
  s.MapBetween(f, G, H);
  s.OneToOne(f);
  s.Onto(f);
  s.TopologyOn(tau, G);
  const substance = s.make();
  expect(substance.propositions).toHaveLength(5);
  expect(combined.Set).toBe(base.Set);
  expect(f).not.toHaveProperty("shape");
  expect(setTheory.Group.name).toBe("Group");
  // Static rejection checks; the function is deliberately never invoked.
  void (() => {
    // @ts-expect-error Sets are not mathematical functions.
    s.OneToOne(G);
    // @ts-expect-error An ordinary function is not declared as a group homomorphism.
    s.GroupHomomorphismBetween(s.Function(), G, H);
  });
});
