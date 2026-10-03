import {
  createFinitePartialOrder,
  createFiniteRelation,
  finiteDifference,
  finiteEquivalenceClasses,
  finiteIntersection,
  finitePowerSet,
  finitePreimage,
  finiteUnion,
  type FiniteFunction,
  type FiniteRelation,
} from "./finite-set-theory.js";

/** Actual finite topologies, rather than finite samples of an infinite space. */
export interface FiniteTopology<T> {
  readonly elements: readonly T[];
  readonly openSets: readonly (readonly T[])[];
  isOpen(subset: readonly T[]): boolean;
}

const normalizer = <T>(elements: readonly T[]) => {
  const ambient = Object.freeze(Array.from(new Set(elements)));
  const membership = new Set(ambient);
  const normalize = (subset: readonly T[]): readonly T[] => {
    const selected = new Set(subset);
    if (subset.some((value) => !membership.has(value)))
      throw new Error("A subset contains a value outside its ambient space");
    return Object.freeze(ambient.filter((value) => selected.has(value)));
  };
  const key = (subset: readonly T[]): string => {
    const selected = new Set(normalize(subset));
    return ambient.map((value) => (selected.has(value) ? "1" : "0")).join("");
  };
  return { ambient, normalize, key };
};

/**
 * Validate ∅, X, binary unions and intersections. Since the supplied family and
 * carrier are finite, binary union closure implies arbitrary union closure.
 * Object-valued points use identity, consistently with the finite set helpers.
 */
export function createFiniteTopology<T>(
  elements: readonly T[],
  openSets: readonly (readonly T[])[],
): FiniteTopology<T> {
  const { ambient, normalize, key } = normalizer(elements);
  const distinct = new Map(openSets.map((set) => [key(set), normalize(set)]));
  if (!distinct.has(key([])) || !distinct.has(key(ambient)))
    throw new Error(
      "A topology must contain both the empty set and the whole space",
    );
  const family = Object.freeze(Array.from(distinct.values()));
  for (const A of family)
    for (const B of family) {
      if (!distinct.has(key(finiteIntersection(A, B))))
        throw new Error(
          "The open family is not closed under finite intersections",
        );
      if (!distinct.has(key(finiteUnion(A, B))))
        throw new Error("The open family is not closed under unions");
    }
  return Object.freeze({
    elements: ambient,
    openSets: family,
    isOpen(subset: readonly T[]): boolean {
      return distinct.has(key(subset));
    },
  });
}

export function finiteInterior<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): readonly T[] {
  const { normalize } = normalizer(topology.elements),
    A = new Set(normalize(subset));
  return normalize(
    finiteUnion(...topology.openSets.filter((U) => U.every((x) => A.has(x)))),
  );
}
export function finiteClosure<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): readonly T[] {
  const { normalize } = normalizer(topology.elements),
    A = normalize(subset);
  return finiteDifference(
    topology.elements,
    finiteInterior(topology, finiteDifference(topology.elements, A)),
  );
}
/** A′ uses every neighborhood of x and removes x itself, including in non-T1 spaces. */
export function finiteDerivedSet<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): readonly T[] {
  const { normalize } = normalizer(topology.elements),
    A = normalize(subset);
  return Object.freeze(
    topology.elements.filter((x) => {
      const otherPoints = new Set(finiteDifference(A, [x]));
      return topology.openSets
        .filter((U) => new Set(U).has(x))
        .every((U) => U.some((y) => otherPoints.has(y)));
    }),
  );
}
export function finiteFrontier<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): readonly T[] {
  return finiteIntersection(
    finiteClosure(topology, subset),
    finiteClosure(topology, finiteDifference(topology.elements, subset)),
  );
}
export function finiteExterior<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): readonly T[] {
  const { normalize } = normalizer(topology.elements);
  return finiteInterior(
    topology,
    finiteDifference(topology.elements, normalize(subset)),
  );
}
export function finiteSubspaceTopology<T>(
  topology: FiniteTopology<T>,
  subset: readonly T[],
): FiniteTopology<T> {
  const { normalize } = normalizer(topology.elements),
    W = normalize(subset);
  return createFiniteTopology(
    W,
    topology.openSets.map((U) => finiteIntersection(U, W)),
  );
}
export function finiteTopologySeparation<T>(topology: FiniteTopology<T>) {
  const { elements: X, openSets } = topology;
  const neighborhoods = (x: T) => openSets.filter((U) => new Set(U).has(x));
  const pairs = X.flatMap((a) =>
    X.filter((b) => a !== b && !(a !== a && b !== b)).map(
      (b) => [a, b] as const,
    ),
  );
  const T0 = pairs.every(([a, b]) =>
    openSets.some((U) => new Set(U).has(a) !== new Set(U).has(b)),
  );
  const T1 = pairs.every(([a, b]) =>
    neighborhoods(a).some((U) => !new Set(U).has(b)),
  );
  const T2 = pairs.every(([a, b]) =>
    neighborhoods(a).some((U) =>
      neighborhoods(b).some((V) => finiteIntersection(U, V).length === 0),
    ),
  );
  return Object.freeze({ T0, T1, T2 });
}
/** In a finite space, points share a component iff no clopen set separates them. */
export function finiteConnectedComponents<T>(
  topology: FiniteTopology<T>,
): readonly (readonly T[])[] {
  const clopen = topology.openSets.filter((U) =>
    topology.isOpen(finiteDifference(topology.elements, U)),
  );
  return finiteEquivalenceClasses(
    createFiniteRelation(topology.elements, (a, b) =>
      clopen.every((U) => new Set(U).has(a) === new Set(U).has(b)),
    ),
  );
}
export function finiteMapIsContinuous<S, T>(
  f: FiniteFunction<S, T>,
  source: FiniteTopology<S>,
  target: FiniteTopology<T>,
): boolean {
  const checkDomain = normalizer(source.elements),
    checkCodomain = normalizer(target.elements);
  if (
    checkDomain.normalize(f.domain).length !== source.elements.length ||
    checkCodomain.normalize(f.codomain).length !== target.elements.length
  )
    throw new Error(
      "Continuity needs a function with the stated source and target spaces",
    );
  return target.openSets.every((U) => source.isOpen(finitePreimage(f, U)));
}

/** Both families must cover X; refinement is containment of each refining member. */
export function isFiniteCoverRefinement<T>(
  elements: readonly T[],
  refining: readonly (readonly T[])[],
  original: readonly (readonly T[])[],
): boolean {
  const { ambient, normalize } = normalizer(elements);
  const R = refining.map(normalize),
    C = original.map(normalize);
  if (
    finiteUnion(...R).length !== ambient.length ||
    finiteUnion(...C).length !== ambient.length
  )
    return false;
  return R.every((V) => C.some((U) => V.every((x) => new Set(U).has(x))));
}

export interface FiniteFilter<T> {
  readonly elements: readonly T[];
  readonly members: readonly (readonly T[])[];
  readonly kernel: readonly T[];
  readonly isUltrafilter: boolean;
  contains(subset: readonly T[]): boolean;
}
/**
 * Generate a proper filter on an actual finite carrier. Every such filter is
 * principal; it is an ultrafilter exactly when its kernel has one point. This
 * makes no assertion about free ultrafilters on infinite carriers (§6.8).
 */
export function finiteGeneratedFilter<T>(
  elements: readonly T[],
  generators: readonly (readonly T[])[],
): FiniteFilter<T> {
  const { ambient, normalize } = normalizer(elements);
  if (generators.length === 0)
    throw new Error("A filter base must be a nonempty family");
  const basis = generators.map(normalize);
  const kernel = finiteIntersection(basis[0], ...basis.slice(1));
  if (kernel.length === 0)
    throw new Error("A proper filter cannot contain the empty set");
  const contains = (subset: readonly T[]) => {
    const selected = new Set(normalize(subset));
    return kernel.every((x) => selected.has(x));
  };
  const members = Object.freeze(finitePowerSet(ambient).filter(contains));
  return Object.freeze({
    elements: ambient,
    members,
    kernel,
    isUltrafilter: kernel.length === 1,
    contains,
  });
}

/** Validate an actual nonempty finite directed partial order, not a truncated infinite index set. */
export function createFiniteDirectedSet<I>(
  elements: readonly I[],
  precedes: (a: I, b: I) => boolean,
): FiniteRelation<I> {
  const order = createFinitePartialOrder(elements, precedes);
  if (order.elements.length === 0)
    throw new Error("A directed index set must be nonempty");
  for (const a of order.elements)
    for (const b of order.elements)
      if (
        !order.elements.some((c) => order.relates(a, c) && order.relates(b, c))
      )
        throw new Error(
          "A directed order needs a common upper bound for each pair",
        );
  return order;
}
/** The neighborhood/tail definition of convergence, applied to a genuinely finite net. */
export function finiteNetConvergesTo<I, T>(
  net: FiniteFunction<I, T>,
  order: FiniteRelation<I>,
  topology: FiniteTopology<T>,
  point: T,
): boolean {
  const checked = createFiniteDirectedSet(order.elements, order.relates);
  const indices = normalizer(checked.elements),
    values = normalizer(topology.elements);
  if (
    indices.normalize(net.domain).length !== checked.elements.length ||
    values.normalize(net.codomain).length !== topology.elements.length
  )
    throw new Error(
      "A net must have the stated index set and space as domain and codomain",
    );
  values.normalize([point]);
  return topology.openSets
    .filter((U) => new Set(U).has(point))
    .every((U) => {
      const membership = new Set(U);
      return checked.elements.some((start) =>
        checked.elements.every(
          (i) => !checked.relates(start, i) || membership.has(net.apply(i)),
        ),
      );
    });
}
