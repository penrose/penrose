/**
 * Concrete finite models for Elementary Topology, Chapter 1.
 * Values use JavaScript Set/Map equality (SameValueZero): object values retain
 * identity, NaN equals NaN, and +0 equals -0. Results are frozen snapshots;
 * caller-owned values themselves are not cloned. Infinite facts remain explicit
 * Substance predicates rather than conclusions from a sampled finite prefix.
 */
const snapshot = <T>(values: Iterable<T>): readonly T[] =>
  Object.freeze(Array.from(new Set(values)));
const same = <T>(a: T, b: T): boolean => a === b || (a !== a && b !== b);
const checkedSubset = <T>(
  values: readonly T[],
  ambient: readonly T[],
): readonly T[] => {
  const membership = new Set(ambient);
  const result = snapshot(values);
  if (result.some((value) => !membership.has(value)))
    throw new Error("A subset contains a value outside its ambient set");
  return result;
};
const sameSet = <T>(a: readonly T[], b: readonly T[]) => {
  const membership = new Set(b);
  return (
    a.length === membership.size && a.every((value) => membership.has(value))
  );
};

export const finiteCardinality = <T>(values: readonly T[]): number =>
  new Set(values).size;
export const finiteUnion = <T>(
  ...sets: readonly (readonly T[])[]
): readonly T[] => snapshot(sets.flat());
export function finiteIntersection<T>(
  first: readonly T[],
  ...others: readonly (readonly T[])[]
): readonly T[] {
  const memberships = others.map((set) => new Set(set));
  return snapshot(
    first.filter((value) => memberships.every((set) => set.has(value))),
  );
}
export function finiteDifference<T>(
  left: readonly T[],
  right: readonly T[],
): readonly T[] {
  const membership = new Set(right);
  return snapshot(left.filter((value) => !membership.has(value)));
}
export function finiteCartesianProduct<S, T>(
  left: readonly S[],
  right: readonly T[],
): readonly (readonly [S, T])[] {
  const a = snapshot(left),
    b = snapshot(right);
  return Object.freeze(
    a.flatMap((s) => b.map((t) => Object.freeze([s, t] as const))),
  );
}
/** Enumerate all subsets; limited to 16 distinct values (65,536 subsets). */
export function finitePowerSet<T>(
  values: readonly T[],
): readonly (readonly T[])[] {
  const elements = snapshot(values);
  if (elements.length > 16)
    throw new Error(
      "A materialized power set supports at most 16 distinct values",
    );
  let subsets: readonly T[][] = [[]];
  for (const value of elements)
    subsets = [...subsets, ...subsets.map((set) => [...set, value])];
  return Object.freeze(subsets.map((set) => Object.freeze(set)));
}

export interface FiniteFunction<S, T> {
  readonly domain: readonly S[];
  readonly codomain: readonly T[];
  readonly graph: readonly (readonly [S, T])[];
  apply(value: S): T;
}

/** Snapshot each value f(s) once, rejecting values outside the stated codomain. */
export function createFiniteFunction<S, T>(
  domain: readonly S[],
  codomain: readonly T[],
  apply: (value: S) => T,
): FiniteFunction<S, T> {
  const source = snapshot(domain),
    target = snapshot(codomain);
  const membership = new Set(target),
    values = new Map<S, T>();
  for (const s of source) {
    const t = apply(s);
    if (!membership.has(t))
      throw new Error("A function value lies outside its stated codomain");
    values.set(s, t);
  }
  return Object.freeze({
    domain: source,
    codomain: target,
    graph: Object.freeze(
      Array.from(values, ([s, t]) => Object.freeze([s, t] as const)),
    ),
    apply(value: S): T {
      if (!values.has(value))
        throw new Error("A function argument lies outside its domain");
      return values.get(value) as T;
    },
  });
}

/** The book's f(A), with A explicitly contained in the domain of f. */
export function finiteImage<S, T>(
  f: FiniteFunction<S, T>,
  subset: readonly S[] = f.domain,
): readonly T[] {
  return snapshot(
    checkedSubset(subset, f.domain).map((value) => f.apply(value)),
  );
}
/** The book's f⁻¹(B); it does not require f to have an inverse function. */
export function finitePreimage<S, T>(
  f: FiniteFunction<S, T>,
  subset: readonly T[],
): readonly S[] {
  const membership = new Set(checkedSubset(subset, f.codomain));
  return snapshot(f.domain.filter((value) => membership.has(f.apply(value))));
}
export const isFiniteInjective = <S, T>(f: FiniteFunction<S, T>): boolean =>
  finiteImage(f).length === f.domain.length;
export const isFiniteSurjective = <S, T>(f: FiniteFunction<S, T>): boolean =>
  finiteImage(f).length === f.codomain.length;
export function restrictFiniteFunction<S, T>(
  f: FiniteFunction<S, T>,
  subset: readonly S[],
): FiniteFunction<S, T> {
  return createFiniteFunction(
    checkedSubset(subset, f.domain),
    f.codomain,
    f.apply,
  );
}
/** f∘g applies g first. The whole stated codomain of g must lie in dom(f). */
export function composeFiniteFunctions<S, T, U>(
  f: FiniteFunction<T, U>,
  g: FiniteFunction<S, T>,
): FiniteFunction<S, U> {
  checkedSubset(g.codomain, f.domain);
  return createFiniteFunction(g.domain, f.codomain, (s) => f.apply(g.apply(s)));
}
export function inverseFiniteFunction<S, T>(
  f: FiniteFunction<S, T>,
): FiniteFunction<T, S> {
  if (!isFiniteInjective(f) || !isFiniteSurjective(f))
    throw new Error(
      "An inverse function requires a bijection onto the stated codomain",
    );
  const inverse = new Map(f.graph.map(([s, t]) => [t, s] as const));
  return createFiniteFunction(f.codomain, f.domain, (t) => inverse.get(t) as S);
}

/** The inverse relation exists for any function, including many-to-one functions. */
export function finiteInverseRelation<S, T>(
  f: FiniteFunction<S, T>,
): readonly (readonly [T, S])[] {
  return Object.freeze(f.graph.map(([s, t]) => Object.freeze([t, s] as const)));
}

export interface FiniteRelation<T> {
  readonly elements: readonly T[];
  relates(left: T, right: T): boolean;
}
/** Snapshot the binary relation on S×S; callbacks run only during construction. */
export function createFiniteRelation<T>(
  elements: readonly T[],
  relates: (left: T, right: T) => boolean,
): FiniteRelation<T> {
  const source = snapshot(elements);
  const positions = new Map(
    source.map((value, index) => [value, index] as const),
  );
  const matrix = source.map((left) =>
    source.map((right) => !!relates(left, right)),
  );
  return Object.freeze({
    elements: source,
    relates(left: T, right: T): boolean {
      const a = positions.get(left),
        b = positions.get(right);
      if (a === undefined || b === undefined)
        throw new Error("Relation arguments must belong to its ambient set");
      return matrix[a][b];
    },
  });
}
const validateTransitivity = <T>(relation: FiniteRelation<T>) => {
  const { elements: S, relates: r } = relation;
  for (const a of S)
    for (const b of S)
      if (r(a, b))
        for (const c of S)
          if (r(b, c) && !r(a, c))
            throw new Error("The relation is not transitive");
};

/** E1–E3 in §1.2: reflexive, symmetric, transitive. */
export function finiteEquivalenceClasses<T>(
  relation: FiniteRelation<T>,
): readonly (readonly T[])[] {
  const { elements: S, relates: r } = relation;
  for (const a of S) {
    if (!r(a, a)) throw new Error("The relation is not reflexive");
    for (const b of S)
      if (r(a, b) !== r(b, a)) throw new Error("The relation is not symmetric");
  }
  validateTransitivity(relation);
  const remaining = new Set(S),
    classes: (readonly T[])[] = [];
  for (const a of S) {
    if (!remaining.has(a)) continue;
    const equivalenceClass = snapshot(S.filter((b) => r(a, b)));
    equivalenceClass.forEach((b) => remaining.delete(b));
    classes.push(equivalenceClass);
  }
  return Object.freeze(classes);
}
export function finiteQuotient<T>(relation: FiniteRelation<T>) {
  const classes = finiteEquivalenceClasses(relation);
  const representatives = new Map<T, readonly T[]>();
  classes.forEach((equivalenceClass) =>
    equivalenceClass.forEach((value) =>
      representatives.set(value, equivalenceClass),
    ),
  );
  const projection = createFiniteFunction(
    relation.elements,
    classes,
    (value) => representatives.get(value) as readonly T[],
  );
  return Object.freeze({ classes, projection });
}
export function finiteFunctionKernelRelation<S, T>(
  f: FiniteFunction<S, T>,
): FiniteRelation<S> {
  return createFiniteRelation(f.domain, (a, b) => same(f.apply(a), f.apply(b)));
}
/** The induced map on classes exists exactly when f is constant on every class. */
export function factorFiniteFunction<S, T>(
  f: FiniteFunction<S, T>,
  relation: FiniteRelation<S>,
) {
  if (!sameSet(f.domain, relation.elements))
    throw new Error(
      "A quotient relation must have the same ambient set as the function domain",
    );
  const quotient = finiteQuotient(relation);
  for (const equivalenceClass of quotient.classes)
    if (
      !equivalenceClass.every((value) =>
        same(f.apply(value), f.apply(equivalenceClass[0])),
      )
    )
      throw new Error("The function is not constant on an equivalence class");
  const induced = createFiniteFunction(
    quotient.classes,
    f.codomain,
    (equivalenceClass) => f.apply(equivalenceClass[0]),
  );
  return Object.freeze({ ...quotient, induced });
}

/** P1–P3 in §1.2: reflexive, antisymmetric, transitive. */
export function createFinitePartialOrder<T>(
  elements: readonly T[],
  precedes: (left: T, right: T) => boolean,
): FiniteRelation<T> {
  const order = createFiniteRelation(elements, precedes);
  for (const a of order.elements) {
    if (!order.relates(a, a)) throw new Error("The order is not reflexive");
    for (const b of order.elements)
      if (!same(a, b) && order.relates(a, b) && order.relates(b, a))
        throw new Error("The order is not antisymmetric");
  }
  validateTransitivity(order);
  return order;
}
export type FiniteChoice<T> =
  | { readonly exists: false }
  | { readonly exists: true; readonly value: T };
const choice = <T>(values: readonly T[]): FiniteChoice<T> =>
  Object.freeze(
    values.length === 1
      ? { exists: true as const, value: values[0] }
      : { exists: false as const },
  );
export function finiteOrderBounds<T>(
  order: FiniteRelation<T>,
  subset: readonly T[],
) {
  // Do not accept an arbitrary unverified relation as an order.
  const checked = createFinitePartialOrder(order.elements, order.relates);
  const W = checkedSubset(subset, checked.elements),
    r = checked.relates;
  const lowerBounds = snapshot(
    checked.elements.filter((a) => W.every((b) => r(a, b))),
  );
  const upperBounds = snapshot(
    checked.elements.filter((b) => W.every((a) => r(a, b))),
  );
  const least = (S: readonly T[]) => S.filter((a) => S.every((b) => r(a, b)));
  const greatest = (S: readonly T[]) =>
    S.filter((b) => S.every((a) => r(a, b)));
  return Object.freeze({
    lowerBounds,
    upperBounds,
    minimalElements: snapshot(
      W.filter((a) => W.every((b) => same(a, b) || !r(b, a))),
    ),
    maximalElements: snapshot(
      W.filter((a) => W.every((b) => same(a, b) || !r(a, b))),
    ),
    least: choice(least(W)),
    greatest: choice(greatest(W)),
    infimum: choice(greatest(lowerBounds)),
    supremum: choice(least(upperBounds)),
    isChain: W.every((a) => W.every((b) => r(a, b) || r(b, a))),
  });
}

export interface FiniteGroup<T> {
  readonly elements: readonly T[];
  readonly identity: T;
  operation(left: T, right: T): T;
  inverse(value: T): T;
}
/** Check closure, associativity, a two-sided identity, and two-sided inverses. */
export function createFiniteGroup<T>(
  elements: readonly T[],
  operation: (left: T, right: T) => T,
): FiniteGroup<T> {
  const source = snapshot(elements),
    membership = new Set(source);
  if (source.length === 0) throw new Error("A group must be nonempty");
  const table = new Map<T, Map<T, T>>();
  for (const a of source) {
    const row = new Map<T, T>();
    for (const b of source) {
      const result = operation(a, b);
      if (!membership.has(result))
        throw new Error("The operation is not closed on the group");
      row.set(b, result);
    }
    table.set(a, row);
  }
  const op = (a: T, b: T): T => {
    if (!membership.has(a) || !membership.has(b))
      throw new Error("Group operation arguments must belong to the group");
    return table.get(a)?.get(b) as T;
  };
  for (const a of source)
    for (const b of source)
      for (const c of source)
        if (!same(op(op(a, b), c), op(a, op(b, c))))
          throw new Error("The operation is not associative");
  const identities = source.filter((e) =>
    source.every((a) => same(op(e, a), a) && same(op(a, e), a)),
  );
  if (identities.length !== 1)
    throw new Error("The operation has no two-sided identity");
  const identity = identities[0],
    inverses = new Map<T, T>();
  for (const a of source) {
    const candidates = source.filter(
      (b) => same(op(a, b), identity) && same(op(b, a), identity),
    );
    if (candidates.length !== 1)
      throw new Error("A group element has no two-sided inverse");
    inverses.set(a, candidates[0]);
  }
  return Object.freeze({
    elements: source,
    identity,
    operation: op,
    inverse(value: T): T {
      if (!inverses.has(value))
        throw new Error("An inverse argument must belong to the group");
      return inverses.get(value) as T;
    },
  });
}
/** Z/nZ under addition; a concrete group, not the infinite group of integers. */
export function finiteCyclicGroup(order: number): FiniteGroup<number> {
  if (!Number.isSafeInteger(order) || order < 1 || order > 256)
    throw new Error("A materialized cyclic group needs an order from 1 to 256");
  return createFiniteGroup(
    Array.from({ length: order }, (_, i) => i),
    (a, b) => (a + b) % order,
  );
}
/** S⊕T in §1.4: the binary direct product, with componentwise operation. */
export function finiteDirectSum<S, T>(
  left: FiniteGroup<S>,
  right: FiniteGroup<T>,
): FiniteGroup<readonly [S, T]> {
  const pairs = finiteCartesianProduct(left.elements, right.elements);
  const lookup = new Map<S, Map<T, readonly [S, T]>>();
  pairs.forEach((pair) => {
    if (!lookup.has(pair[0])) lookup.set(pair[0], new Map());
    lookup.get(pair[0])?.set(pair[1], pair);
  });
  return createFiniteGroup(
    pairs,
    (a, b) =>
      lookup
        .get(left.operation(a[0], b[0]))
        ?.get(right.operation(a[1], b[1])) as readonly [S, T],
  );
}
export function isFiniteGroupHomomorphism<S, T>(
  f: FiniteFunction<S, T>,
  source: FiniteGroup<S>,
  target: FiniteGroup<T>,
): boolean {
  if (
    !sameSet(f.domain, source.elements) ||
    !sameSet(f.codomain, target.elements)
  )
    throw new Error(
      "A group homomorphism must have the stated groups as domain and codomain",
    );
  return source.elements.every((a) =>
    source.elements.every((b) =>
      same(
        f.apply(source.operation(a, b)),
        target.operation(f.apply(a), f.apply(b)),
      ),
    ),
  );
}
export function finiteGroupKernel<S, T>(
  f: FiniteFunction<S, T>,
  source: FiniteGroup<S>,
  target: FiniteGroup<T>,
): readonly S[] {
  if (!isFiniteGroupHomomorphism(f, source, target))
    throw new Error("The function is not a group homomorphism");
  return finitePreimage(f, [target.identity]);
}
export function isFiniteSubgroup<T>(
  group: FiniteGroup<T>,
  subset: readonly T[],
): boolean {
  const W = checkedSubset(subset, group.elements),
    membership = new Set(W);
  return (
    W.length > 0 &&
    membership.has(group.identity) &&
    W.every(
      (a) =>
        membership.has(group.inverse(a)) &&
        W.every((b) => membership.has(group.operation(a, b))),
    )
  );
}
export function isFiniteGroupIsomorphism<S, T>(
  f: FiniteFunction<S, T>,
  source: FiniteGroup<S>,
  target: FiniteGroup<T>,
): boolean {
  return (
    isFiniteGroupHomomorphism(f, source, target) &&
    isFiniteInjective(f) &&
    isFiniteSurjective(f)
  );
}

export function isFiniteNormalSubgroup<T>(
  group: FiniteGroup<T>,
  subset: readonly T[],
): boolean {
  if (!isFiniteSubgroup(group, subset)) return false;
  const membership = new Set(subset);
  return group.elements.every((g) =>
    subset.every((h) =>
      membership.has(group.operation(group.operation(g, h), group.inverse(g))),
    ),
  );
}
/** Quotient group of a finite group by a verified normal subgroup. */
export function finiteQuotientGroup<T>(
  group: FiniteGroup<T>,
  normalSubgroup: readonly T[],
) {
  if (!isFiniteNormalSubgroup(group, normalSubgroup))
    throw new Error("A quotient group requires a normal subgroup");
  const membership = new Set(normalSubgroup);
  const relation = createFiniteRelation(group.elements, (a, b) =>
    membership.has(group.operation(group.inverse(a), b)),
  );
  const quotient = finiteQuotient(relation);
  const result = createFiniteGroup(quotient.classes, (A, B) =>
    quotient.projection.apply(group.operation(A[0], B[0])),
  );
  return Object.freeze({ group: result, projection: quotient.projection });
}
