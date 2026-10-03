---
title: Reusable Domain, Substance, and TSX Style Programs
description: Define independent mathematical programs and reusable Penrose styles in TypeScript.
---

# A library of mathematical illustrations

Reuse the book’s mathematical vocabulary and visual rules to illustrate a new idea.

The TypeScript API keeps the three Penrose programs separate. A **domain**
declares the mathematical vocabulary. A **substance** creates objects and asserts
facts. A **style** gives those objects shapes and constraints. Reuse one substance
with different styles, or one style with different substances.

## Declare a domain

```ts
// sets.ts
import { domain } from "@penrose/bloom";

const d = domain("sets");
const Set = d.type("Set");
const Point = d.type("Point");
const Subset = d.predicate("Subset", [Set, Set]);
const Member = d.predicate("Member", [Point, Set]);
export const sets = d.make({ Set, Point, Subset, Member });
```

Declarations are independent of a diagram builder. TypeScript checks predicate
argument types; runtime checks also protect JavaScript callers. Subtypes are
declared with `d.type("OpenSet", Set)` and appear in selectors for their parents.
Use `declareSetTheory(d)` from `@penrose/bloom/domains/set-theory` to compose the
shared set vocabulary into a larger domain.

Types can carry mathematical data:

```ts
const Point = d
  .type("Point")
  .withData<{ coordinate: readonly [number, number] }>();
const MarkedPoint = d.type("MarkedPoint", Point).withData<{ note: string }>();
const Function = d.type("Function").withData<{
  source: EntityOf<typeof Set>;
  target: EntityOf<typeof Set>;
}>();
```

Import `EntityOf` from `@penrose/bloom`. Metadata is copied and frozen when an
entity is constructed. References to existing objects in the same substance keep
their semantic identity. References from another substance are rejected.

## Write a substance program

```ts
// example.ts
import { sets } from "./sets";

const s = sets.substance();
const A = s.Set({ label: "A" });
const B = s.Set({ label: "B" });
const x = s.Point({ label: "x" });
s.Subset(A, B);
s.Member(x, A);
export const sub = s.make();
```

`make()` closes an immutable snapshot. It contains mathematical objects and
propositions, with no shapes or builder state. Predicate calls assert facts and
deduplicate identical propositions. For nested propositions, use
`s.Subset.expression(A, B)` to form an expression without asserting it, then
pass it to a predicate declared with the `proposition` argument marker.
Predicates have no automatic logical inference.

## Reuse a TSX style

The library's Euler/Venn style turns set and membership facts into Penrose
constraints. The mathematical domain exports the same interface for every
substance program:

```ts
import { canvas, diagram, eulerVennStyle, setTheory } from "@penrose/bloom";

const s = setTheory.substance();
const A = s.Set({ label: "A" });
const B = s.Set({ label: "B" });
s.Subset(A, B);

const drawing = await diagram({
  sub: s.make(),
  sty: eulerVennStyle(),
  canvas: canvas(400, 300),
  variation: "nested-sets",
});
while (await drawing.optimizationStep()) {}
const { svg } = await drawing.render();
document.body.append(svg);
```

To write a style, add `/** @jsxImportSource @penrose/bloom */` to a `.tsx` module.
The style below captures its mathematical domain and defines reusable visual
behavior:

```tsx
/** @jsxImportSource @penrose/bloom */
import type { Circle, Equation } from "@penrose/bloom";
import { sets } from "./sets";

export const pointStyle = sets.style((ctx) => {
  const visual = ctx.view(sets.Point, (point) => ({
    dot: (<circle r={3} fill="black" ensure-on-canvas />) as Circle,
    label: (<equation>{point.label}</equation>) as Equation,
  }));
  ctx.forall({ point: sets.Point }, ({ point }) => {
    // Access the shapes associated with this mathematical point.
    const { dot, label } = visual.get(point);
    ctx.layer(dot, label);
  });
});
```

Style callbacks and TSX components are synchronous. Shapes are created eagerly
inside the active, scoped builder. Each `diagram()` assembly creates fresh visual
views, so parallel builds do not mutate a shared substance. `sty` can be an array
of compatible styles applied in order. Set the canvas in the assembly, or in a
style's options; conflicting style canvases require an explicit assembly canvas.

Use `ctx.facts(predicate)` to iterate directed facts, including reflexive facts;
`ctx.test(predicate, ...args)` checks an assertion. `ctx.forall` and
`ctx.forallWhere` use ordered assignments of distinct objects. `ctx.view` gives
typed per-object shape data without adding visual properties to the substance.

## Mathematical modules

The library supplies shape-free mathematical declarations for all 58 available
chapter sections and the appendix, plus a semantic route for all 49 entries in
the book's symbol index. Figure-specific subclasses add coordinates or formulas
when a visual policy needs them. A declaration is an author-supplied mathematical
fact; it is not a proof inferred from a rendered picture.
Coverage refers to the supplied source. Its 31 missing printed pagination
positions remain explicit gaps; the vocabulary does not reconstruct their content.

| Module                   | Mathematical vocabulary and helpers                                                                                                                                                                                                        |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `set-theory`             | Sets, relations, functions, partial/total/well orders, equivalence classes, cardinality, groups, rings and ideals. Re-exports validated finite set, map, order, quotient and group helpers.                                                |
| `metric-spaces`          | The book's plane metrics, strict metric neighborhoods, metric/function-space sequences, continuity, uniform distance and pointwise limits.                                                                                                 |
| `point-set-topology`     | Abstract topology, derived sets, bases, subspaces, quotients, indexed products, separation, nets, filters, covers, compactness, connectedness, completeness, homotopy, loops and fundamental groups.                                       |
| `topological-vocabulary` | Composable declarations for countability axioms, cover refinements, compactifications, continua, metrizability, contractions, loop operations, induced homomorphisms and retractions. Also validates affine contraction data and iterates. |
| `finite-topology`        | Validated actual finite topologies, derived sets, separation, components, continuity, cover refinement, generated filters and finite directed nets.                                                                                        |
| `metric-completeness`    | Geometric sequence terms and Cauchy witnesses, nested bisection bounds and Euclidean diameters.                                                                                                                                            |
| `metric-covers`          | A validated finite half-radius cover of a compact real interval and its Lebesgue-number witness.                                                                                                                                           |
| `loop-operations`        | Reusable parameter operations for based loop products, associativity, units and inverses.                                                                                                                                                  |
| `decimal-expansions`     | Composable integer/decimal declarations, distinct known prefixes, and the finite complemented diagonal. Included in `set-theory`.                                                                                                          |
| `plane-curves`           | Immutable joined cubic/circular loops, exact piecewise evaluation, linear based contractions, and conservative whole-loop disk bounds.                                                                                                     |
| `surface-topology`       | Stereographic sphere charts and loop contractions away from an omitted pole, torus factor curves, based conjugation, and finite graph cycle counts.                                                                                        |

Import these through `@penrose/bloom/domains/*`, reusable views through
`@penrose/bloom/styles/*`, and mathematical example factories through
`@penrose/bloom/examples/*`. Named root exports are also available for the
established modules.

### Compose declarations in one domain

The small `declareSetTheory(d)` factory retains the shared `Set`, `Point`,
`Subset`, `Member`, `Disjoint`, and `Intersecting` vocabulary used by the figure
libraries. Use `declareElementarySetTheory(d)` for the full Chapter 1 vocabulary:

```ts
import { domain, declareElementarySetTheory } from "@penrose/bloom";

const d = domain("my-algebra");
const vocabulary = declareElementarySetTheory(d);
const MarkedPoint = d.type("MarkedPoint", vocabulary.Point).withData<{
  note: string;
}>();
const algebra = d.make({ ...vocabulary, MarkedPoint });

const s = algebra.substance();
const G = s.Group({ label: "G" });
const H = s.Group({ label: "H" });
const f = s.GroupHomomorphism({ label: "f" });
s.GroupHomomorphismBetween(f, G, H);
s.MapBetween(f, G, H);
const sub = s.make();
```

`declareElementarySetTheory(d, existingSets)` can reuse an existing basic
vocabulary from that same builder. `declareGroupTheory(d, { Set, Point, Function
})` composes the algebra with an existing function type. Its group homomorphisms
retain function subtyping.

`declareTopologicalVocabulary(d, base)` adds the abstract later-chapter concepts
to an existing topology vocabulary. Its `base` supplies `Set`, `Point`,
`RealPoint`, `Topology`, `SetFamily`, `OpenCover`, `CountableSetFamily`, `Basis`,
`NeighborhoodSystem`, `Neighborhood`, `TopologicalMap`, `Metric`, `DirectedSet`,
`Net`, `Filter`, `FilterBase`, `Homotopy`, and `TopologicalPath`. If the domain
already declares algebra, use `declareTopologicalVocabularyWithGroups(d, base,
algebra)` to reuse those declarations. It creates no second copy of `Group` or
its homomorphisms. The standard `pointSetTopology` domain already performs this
composition; ordinary book programs can import it directly.

Declare types before `d.make()`. Declarations from independently sealed domains
are not interchangeable. Shared declarations are composed in the same builder;
then reusable Substance programs refer to that sealed domain. Types can have
multiple parents: a `LoopHomotopyClass` is both a set of loops and an element of
a `FundamentalGroup`.

### Use finite witnesses honestly

`declareDecimalExpansions(d, { Point })`, also included in the full elementary
set-theory factory, distinguishes abstract decimal expansions from their known
finite prefixes. `complementedDiagonalPrefix(rows)` checks the displayed 0/1
diagonal positions. These are base-ten decimals using only the digits 0 and 1;
the helper does not fill unseen digits or infer uncountability from four rows.
The reusable `decimalTableStyle` renders different such windows, including the
source table in §1.3.

`groupTableStyle` reads complete `GroupOperationOn` and `ProductValue` facts,
checks the whole finite group laws and recorded identities/inverses, and renders
the source table in §1.4. The same style accepts a different cyclic group table.
`groupKernelStyle` also validates every displayed operation and map value; the
two original kernel illustrations use $\mathbb Z_6\to\mathbb Z_3$ and
$\mathbb Z_8\to\mathbb Z_4$, with node dragging that preserves the immutable
group facts and keeps incident arrows attached.
In interactive mode the source tables move as whole constructions: their cells,
headings, continuation dots and grid lines share one native translation.

```ts
import {
  createFiniteFunction,
  finiteCyclicGroup,
  finiteGroupKernel,
  isFiniteGroupHomomorphism,
  isFiniteSubgroup,
} from "@penrose/bloom/domains/set-theory";

const G = finiteCyclicGroup(6);
const H = finiteCyclicGroup(3);
const f = createFiniteFunction(G.elements, H.elements, (x) => x % 3);
if (!isFiniteGroupHomomorphism(f, G, H)) throw new Error("Invalid group map");
const kernel = finiteGroupKernel(f, G, H); // [0, 3]
const isSubgroup = isFiniteSubgroup(G, kernel); // true
```

Finite functions snapshot their graphs, validate the stated codomain, and keep
preimage distinct from an inverse function. `inverseFiniteFunction` requires a
bijection. `finiteOrderBounds` distinguishes minimal/maximal elements of a
subset from its least/greatest elements and its bounds in the ambient order.
For instance, under divisibility on `{1,2,3,6}`, the subset `{2,3}` has two maximal
elements and no greatest element, while its supremum is `6`.

`createFiniteGroup` checks closure, associativity, a two-sided identity and
inverses. `finiteQuotientGroup` additionally checks normality;
`finiteDirectSum` implements the book's binary componentwise group product
`S⊕T`. Finite values use JavaScript Set/Map equality: object identity, `NaN`
equal to `NaN`, and `+0` equal to `-0`. Arrays and snapshots are frozen;
caller-owned point values themselves are not cloned.

`createFiniteTopology` validates all topology axioms on the actual finite
carrier. Its derived-set helper removes the tested point itself, including in
non-`T1` spaces. `finiteGeneratedFilter` constructs a proper filter on an actual
finite carrier; such filters are principal, and its `isUltrafilter` flag checks
whether the kernel has exactly one point. `finiteNetConvergesTo` checks every
neighborhood and whole directed tail of an actual finite net. None of these
finite results establishes a claim about a sampled infinite space or sequence.

`finiteBallLebesgueWitness([a,b], balls)` validates strict coverage by the open
half-radius balls of an actual compact real interval, and returns the positive
minimum half-radius. `halfBallContaining` selects the covering member for a
point of that interval. Touching open intervals are rejected when they leave
their shared endpoint uncovered.

`contractionData(k)` validates `0 ≤ k < 1` for typed contraction metadata.
`affineRealContraction(a,b)` derives the Lipschitz constant `|a|` and fixed point
`b/(1-a)` algebraically for `f(x)=ax+b`. It provides `.data` for Substance
metadata, `.iterates(initial,n)` and `.errorBound(initial,n)`. In real arithmetic
the latter is the exact identity `|a|^n |initial-fixedPoint|`. Numerical evaluation
has floating-point roundoff and rejects overflow or underflow of a nonzero
bound. The helper never infers a Lipschitz bound for an arbitrary callback from
samples.

`piecewisePlaneLoopData(segments)` validates finite cubic control points and
circular arcs, joins and closure, then copies and freezes the loop data.
`piecewisePlaneLoopValue` gives each segment an equal parameter interval.
`planeLoopDiskBound`, imported from `@penrose/bloom/domains/plane-curves`, uses
outward-rounded Bernstein bounds for complete cubic pieces and a conservative
whole-circle bound for arcs. `contained: true` certifies every parameter within
the stated tolerance. `false` can mean an unresolved subdivision bound, rather
than a point outside the disk. It does not infer containment from sampled points.
`planeLoopContractionValue` evaluates a linear based contraction; staying in a
chosen convex disk requires the whole-loop containment premise.

`trigonometricLoopValue` evaluates finite integer Fourier harmonics, whose
endpoints coincide. `inverseCancellationParameter` gives the exact retracing
parameter used by both the circle and figure-eight original illustrations.
The sphere helpers contract a based loop in an omitted-pole chart, not the
whole sphere. `finiteGraphCycleRank` computes `edges - vertices + components`
from supplied graph counts; callers establish the actual graph incidence and
component count independently.

General compactness, metrizability, infinite countability, completeness and
homotopy classes remain explicit mathematical facts supplied by authors.
Higher homotopy, homology and cohomology are named in the book's conclusion;
their declarations identify those background groups and degrees, without
inventing absent definitions or providing a group-computation algorithm.
Source gaps and inconsistent source proof steps remain documented in the
transcriptions. In particular, a homotopy relating induced maps must fix the
base point to justify equality of the induced homomorphisms.

### Every available section

The following map names the principal declarations for each section. The
underlying domains contain the additional relationships and helper functions
listed above. Mathematical notation in labels is the source's LaTeX; the API
does not parse arbitrary set-builder expressions or quantifiers.

| Section                                                                                                                      | Domain                              | Principal API                                                                                                                                                                       |
| ---------------------------------------------------------------------------------------------------------------------------- | ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [1.1 Sets and Functions](./chapter-01/sets-and-functions)                                                                    | `setTheory`                         | `Set`, `Point`, `Member`, `Subset`, `UnionOf`, `IntersectionOf`, `DifferenceOf`, `ProductOf`                                                                                        |
| [1.2 Orderings; Equivalence Relations](./chapter-01/orderings-equivalence-relations)                                         | `setTheory`                         | `PartialOrder`, `TotalOrder`, `WellOrder`, `Chain`, `OrderOn`, `Precedes`, `InducedOrderOn`, `UpperBoundOf`                                                                         |
| [1.3 Cardinality](./chapter-01/cardinality)                                                                                  | `setTheory`                         | `Cardinal`, `FiniteCardinal`, `CardinalityOf`, `SameCardinality`, `CardinalityAtMost`, `CardinalityStrictlyGreater`, `Finite`, `Infinite`                                           |
| [1.4 Groups](./chapter-01/groups)                                                                                            | `setTheory`                         | `Group`, `AbelianGroup`, `Subgroup`, `GroupHomomorphism`, `GroupIsomorphism`, `BinaryOperation`, `GroupOperationOn`, `ProductValue`                                                 |
| [2.1 The Notion of a Metric Space](./chapter-02/metric-space)                                                                | `metricSpaces` / `pointSetTopology` | `MetricSpace`, `Metric`, `MetricOn`, `MetricPlane`, `EuclideanMetric`, `EquivalentMetricsOn`                                                                                        |
| [2.2 Neighborhoods](./neighborhoods)                                                                                         | `metricSpaces` / `pointSetTopology` | `SpaceNeighborhood`, `SpaceNeighborhoodAt`, `Neighborhood`, `MetricNeighborhood`, `MetricNeighborhoodAt`                                                                            |
| [2.3 Open Sets](./chapter-02/open-sets)                                                                                      | `pointSetTopology`                  | `OpenSet`, `OpenIn`, `InteriorOf`                                                                                                                                                   |
| [2.4 Closed Sets](./chapter-02/closed-sets)                                                                                  | `pointSetTopology`                  | `ClosedSet`, `ClosedIn`, `ComplementOf`, `ClosureOf`                                                                                                                                |
| [2.5 Convergence of Sequences](./chapter-02/convergence-of-sequences)                                                        | `metricSpaces` / `pointSetTopology` | `ConvergesTo`, `TailOfSequence`, `TailInNeighborhood`, `RealSequence`, `SequenceConvergesTo`, `FunctionSequence`, `PointwiseConvergesTo`, `FailsToConvergeUniformlyTo`              |
| [2.6 Continuity](./chapter-02/continuity)                                                                                    | `metricSpaces` / `pointSetTopology` | `ContinuousAt`, `MapsNeighborhoodInto`, `ContinuousMap`, `UniformlyContinuousBetween`                                                                                               |
| [2.7 “Distance” Between Two Sets](./chapter-02/distance-between-sets)                                                        | `pointSetTopology`                  | `MetricValue`, `DistanceBetweenPoints`, `DistanceToSet`, `DistanceBetweenSets`, `ClosureOf`, `FrontierOf`, `ZeroSetDistance`, `ZeroPointDistance`                                   |
| [3.1 The Notion of a Topology](./chapter-03/topology)                                                                        | `pointSetTopology`                  | `Topology`, `TopologyOn`, `OpenIn`, `ClosedIn`, `Discrete`, `Indiscrete`, `CofiniteTopology`, `Ring`                                                                                |
| [3.2 Bases and Subbases](./chapter-03/bases-and-subbases)                                                                    | `pointSetTopology`                  | `Basis`, `Subbasis`, `BasisFor`, `SubbasisFor`, `SetInFamily`, `FiniteIntersectionOf`, `UnionOf`                                                                                    |
| [3.3 Open Neighborhood Systems](./chapter-03/open-neighborhood-systems)                                                      | `pointSetTopology`                  | `NeighborhoodSystem`, `NeighborhoodSystemAt`, `NeighborhoodInSystem`, `NeighborhoodOf`                                                                                              |
| [3.4 Finer and Coarser Topologies](./chapter-03/finer-and-coarser-topologies)                                                | `pointSetTopology`                  | `FinerTopologyThan`, `CoarserTopologyThan`, `EqualTopologies`                                                                                                                       |
| [3.5 Derived Sets](./chapter-03/derived-sets)                                                                                | `pointSetTopology`                  | `InteriorOf`, `ClosureOf`, `FrontierOf`, `ExteriorOf`, `DerivedSetOf`                                                                                                               |
| [3.6 More About Topologically Derived Sets](./chapter-03/topologically-derived-sets)                                         | `pointSetTopology`                  | `DenseIn`, `NowhereDenseIn`, `SomewhereDenseIn`, `DerivedSetOf`                                                                                                                     |
| [4.1 Subspaces](./chapter-04/subspaces)                                                                                      | `pointSetTopology`                  | `Subspace`, `OpenSubspace`, `ClosedSubspace`, `SubspaceTopologyOf`, `OpenIn`, `ClosedIn`                                                                                            |
| [4.2 The Topologically Derived Sets in Subspaces](./chapter-04/derived-sets-in-subspaces)                                    | `pointSetTopology`                  | `DerivedSetOf`, `ClosureOf`, `InteriorOf`, `FrontierOf`, `SubspaceTopologyOf`                                                                                                       |
| [4.3 Continuity](./chapter-04/continuity)                                                                                    | `pointSetTopology`                  | `TopologicalMap`, `MapBetween`, `ContinuousMap`, `MapsTo`, `TopologicalCompositionOf`, `TopologicalRestrictionOf`, `InclusionMap`, `InclusionOf`                                    |
| [4.4 Homeomorphisms](./chapter-04/homeomorphisms)                                                                            | `pointSetTopology`                  | `Homeomorphism`, `InverseMaps`, `CentralProjection`, `RadialProjection`                                                                                                             |
| [4.5 Identification Spaces](./chapter-04/identification-spaces)                                                              | `pointSetTopology`                  | `EquivalenceRelation`, `QuotientSpace`, `EquivalenceClass`, `QuotientMap`, `QuotientOf`, `ClassOf`, `IdentificationMap`, `IdentificationTopology`                                   |
| [4.6 Product Spaces](./chapter-04/product-spaces)                                                                            | `pointSetTopology`                  | `IndexedSetFamily`, `IndexedFamilyOver`, `IndexedSetAt`, `IndexedProduct`, `IndexedProductOf`, `IndexedTopologyFamily`, `IndexedProductTopologyOf`, `ProductCoordinateAt`           |
| [5.1 $T_0$- and $T_1$-Spaces](./chapter-05/t0-and-t1-spaces)                                                                 | `pointSetTopology`                  | `T0`, `T1`, `SingletonOf`                                                                                                                                                           |
| [5.2 $T_2$-Spaces](./chapter-05/t2-spaces)                                                                                   | `pointSetTopology`                  | `T2`, `HausdorffNeighborhoodPair`, `OrderRelation`, `OrderTopologyFor`                                                                                                              |
| [5.3 $T_3$- and Regular Spaces](./chapter-05/t3-and-regular-spaces)                                                          | `pointSetTopology`                  | `T3`, `Regular`, `PointClosedSetSeparation`, `NeighborhoodClosureWithin`                                                                                                            |
| [5.4 $T_4$- and Normal Spaces](./chapter-05/t4-and-normal-spaces)                                                            | `pointSetTopology`                  | `T4`, `Normal`, `NotNormal`, `ClosedSetsSeparation`, `CompletelyNormal`, `AbsoluteRetract`                                                                                          |
| [5.5 Normality and the Extension of Functions](./chapter-05/normality-and-extension)                                         | `pointSetTopology`                  | `DyadicOpenFamily`, `DyadicSeparationStep`, `DyadicRefinementStep`, `ExtensionOf`, `TopologicalPath`, `RelativeHomotopyOn`                                                          |
| [6.1 The Need for a Generalized Notion of Convergence](./chapter-06/generalized-convergence)                                 | `pointSetTopology`                  | `FunctionSpace`, `FunctionPoint`, `FunctionSpaceOf`, `PointwiseTopologyOn`, `PointwiseNeighborhood`, `FiniteControlNeighborhoodOf`, `FirstCountable`                                |
| [6.2 Nets](./chapter-06/nets)                                                                                                | `pointSetTopology`                  | `DirectedSet`, `DirectedOrder`, `DirectedIndex`, `DirectedOrderOn`, `IndexIn`, `IndexPrecedes`, `DirectedUpperBoundFor`, `Net`                                                      |
| [6.3 Subsequences and Subnets](./chapter-06/subsequences-and-subnets)                                                        | `pointSetTopology`                  | `Sequence`, `SubsequenceOf`, `SubnetOf`, `CofinalOrderMap`                                                                                                                          |
| [6.4 Convergence of Nets](./chapter-06/convergence-of-nets)                                                                  | `pointSetTopology`                  | `NetConvergesTo`, `EventuallyIn`, `OftenIn`, `NetLimitPoint`                                                                                                                        |
| [6.5 Limit Points](./chapter-06/limit-points)                                                                                | `pointSetTopology`                  | `NetLimitPoint`, `SubnetOf`, `SelectionNetOf`, `ChoosesFromIntersections`, `SelectedIntersectionValue`                                                                              |
| [6.6 Continuity and Convergence](./chapter-06/continuity-and-convergence)                                                    | `pointSetTopology`                  | `ContinuousMap`, `ImageNetOf`, `NetConvergesTo`, `SubspaceTopologyOf`                                                                                                               |
| [6.7 Filters](./chapter-06/filters)                                                                                          | `pointSetTopology`                  | `Filter`, `FilterBase`, `FilterOn`, `FilterBasisFor`, `FilterContains`, `FilterBaseContains`, `GeneratedFilterOf`, `FilterFinerThan`                                                |
| [6.8 Ultranets and Ultrafilters](./chapter-06/ultranets-and-ultrafilters)                                                    | `pointSetTopology`                  | `Ultranet`, `Ultrafilter`, `PrincipalFilterAt`, `CofiniteFilterOn`, `FreeFilter`                                                                                                    |
| [7.1 Open Covers and Refinements](./chapter-07/open-covers-and-refinements)                                                  | `pointSetTopology`                  | `SetFamily`, `OpenCover`, `CoverOf`, `OpenCoverOf`, `SubfamilyOf`, `Refines`                                                                                                        |
| [7.2 Countability Properties](./chapter-07/countability-properties)                                                          | `pointSetTopology`                  | `Lindelof`, `FirstCountable`, `SecondCountable`, `Separable`, `CountableBasis`, `CountableNeighborhoodSystem`, `CountableFamily`, `CountableDenseSubsetOf`                          |
| [7.3 Compactness](./chapter-07/compactness)                                                                                  | `pointSetTopology`                  | `Compact`, `CompactIn`, `FiniteFamily`, `FiniteIntersectionProperty`, `FamilyIntersectionIs`, `HasNoFiniteSubcover`                                                                 |
| [7.4 The Derived Spaces and Compactness. The Separation Axioms and Compactness](./chapter-07/derived-spaces-and-compactness) | `pointSetTopology`                  | `CompactIn`, `ImageOf`, `Homeomorphism`, `IndexedProductTopologyOf`, `ProjectionOf`                                                                                                 |
| [8.1 Compactness in $R^n$](./chapter-08/compactness-in-euclidean-space)                                                      | `pointSetTopology`                  | `EuclideanMetric`, `BoundedInMetric`, `BoundedFunction`, `TotallyBoundedIn`, `LebesgueNumber`, `LebesgueNumberFor`, `FiniteCoverReachOf`                                            |
| [8.2 Local Compactness](./chapter-08/local-compactness)                                                                      | `pointSetTopology`                  | `LocallyCompact`, `LocallyCompactIn`, `NotLocallyCompact`, `FailsLocalCompactnessAt`, `CompactNeighborhoodWithin`                                                                   |
| [8.3 Compactifications](./chapter-08/compactifications)                                                                      | `pointSetTopology`                  | `Compactification`, `OnePointCompactification`, `CompactificationOf`, `IdealPointOf`                                                                                                |
| [8.4 Sequential and Countable Compactness](./chapter-08/sequential-and-countable-compactness)                                | `pointSetTopology`                  | `CountablyCompact`, `SequentiallyCompact`, `AccumulationPointOf`, `Metacompact`, `Pseudocompact`                                                                                    |
| [9.1 The Notion of Connectedness](./chapter-09/notion-of-connectedness)                                                      | `pointSetTopology`                  | `Connected`, `ConnectedIn`, `Disconnected`, `TopologicalSeparationOf`, `TotallyDisconnected`                                                                                        |
| [9.2 Further Tests for Connectedness](./chapter-09/further-tests-for-connectedness)                                          | `pointSetTopology`                  | `PathConnected`, `PolygonallyConnected`, `Convex`, `PolygonalPath`, `EuclideanLineSegment`, `PolygonalReachabilityClass`, `PolygonalReachableFrom`                                  |
| [9.3 Connectedness and the Derived Spaces](./chapter-09/connectedness-and-derived-spaces)                                    | `pointSetTopology`                  | `Connected`, `ImageOf`, `ProductOf`, `ProductTopologyOf`, `MutuallySeparated`, `Disconnects`, `ClosedMap`                                                                           |
| [9.4 Components. Local Connectedness](./chapter-09/components-and-local-connectedness)                                       | `pointSetTopology`                  | `ComponentOf`, `ComponentFamilyOf`, `PathComponentOf`, `LocallyConnected`, `LocallyPathConnected`                                                                                   |
| [9.5 Connectedness and Compact $T_2$-Spaces](./chapter-09/connectedness-and-compact-t2-spaces)                               | `pointSetTopology`                  | `Continuum`, `IrreducibleAbout`, `IrreduciblyConnectedBetween`, `CutPoint`, `NoncutPoint`, `QuasiComponentOf`, `SubcontinuumOf`, `ClosedDirectedFamily`                             |
| [10.1 Metrizable Spaces](./chapter-10/metrizable-spaces)                                                                     | `pointSetTopology`                  | `Metrizable`, `Nonmetrizable`, `MetricInducesTopology`, `EuclideanMetric`, `HilbertCube`                                                                                            |
| [10.2 Cauchy Sequences](./chapter-10/cauchy-sequences)                                                                       | `pointSetTopology`                  | `RealSequence`, `CauchySequenceIn`, `SequenceConvergesTo`                                                                                                                           |
| [10.3 Complete Metric Spaces](./chapter-10/complete-metric-spaces)                                                           | `pointSetTopology`                  | `CompleteMetricSpace`, `Diameter`, `DiameterOf`, `DecreasingFamily`, `DiametersTendToZero`, `MetricCompletion`, `MetricCompletionOf`, `Isometry`                                    |
| [10.4 Baire Category Theorem](./chapter-10/baire-category-theorem)                                                           | `pointSetTopology`                  | `DenseOpenFamilyIn`, `NowhereDenseFamilyIn`, `FamilyUnionIs`, `FamilyIntersectionIs`, `BaireSpace`, `FirstCategoryIn`, `SecondCategoryIn`                                           |
| [10.5 Paracompactness. Complete Regularity](./chapter-10/paracompactness-and-complete-regularity)                            | `pointSetTopology`                  | `LocallyFiniteIn`, `PointFiniteIn`, `Refines`, `Paracompact`, `LocallyMetrizable`, `CompletelyRegular`, `Tychonoff`, `ContinuousFunctionSet`                                        |
| [11.1 Homotopic Functions](./chapter-11/homotopic-functions)                                                                 | `pointSetTopology`                  | `Homotopy`, `HomotopyBetween`, `Homotopic`, `SliceMapAt`, `FunctionSpace`, `FunctionSpaceOf`, `RelativeHomotopyOn`, `RelativelyHomotopic`                                           |
| [11.2 Loops](./chapter-11/loops)                                                                                             | `pointSetTopology`                  | `Loop`, `LoopSpace`, `LoopSpaceOf`, `LoopBasedAt`, `LoopInSpace`, `ConstantLoop`, `ConstantLoopAt`, `LoopHomotopyClass`                                                             |
| [11.3 The Fundamental Group](./chapter-11/fundamental-group)                                                                 | `pointSetTopology`                  | `FundamentalGroup`, `FundamentalGroupOf`, `ClassInFundamentalGroup`, `LoopClassProductOf`, `NullHomotopic`, `SimplyConnected`, `BasepointChangeIsomorphism`, `BasepointChangeAlong` |
| [11.4 The Fundamental Group and Continuous Functions](./chapter-11/fundamental-group-and-continuous-functions)               | `pointSetTopology`                  | `InducedHomomorphism`, `InducedHomomorphismOf`, `InducedLoopClassImage`, `HomotopyEquivalence`, `HomotopyEquivalent`, `HomotopyInverseMaps`, `RetractionMap`, `RetractsOnto`        |
| [A Appendix on Infinite Products](./back-matter/appendix-on-infinite-products)                                               | `pointSetTopology`                  | `IndexedSetFamily`, `IndexSet`, `IndexedProduct`, `ProductElement`, `ProductCoordinateAt`, `IndexedTopologyFamily`, `IndexedProductTopologyOf`, `IndexedCoordinateProjection`       |

### Source notation

This maps every entry in the [source symbol index](./back-matter/index-of-symbols)
to semantic declarations or helpers. Page numbers use the book's printed
pagination; `S-T` has no page reference in the source.

| Source notation                                                  | Printed pages | Semantic API                                                                                                                                           |
| ---------------------------------------------------------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| $\in$                                                            | 1             | `setTheory.Member` — element membership                                                                                                                |
| $\cup$                                                           | 1             | `setTheory.UnionOf / finiteUnion` — union                                                                                                              |
| $\cap$                                                           | 1             | `setTheory.IntersectionOf / finiteIntersection` — intersection                                                                                         |
| $\subset$                                                        | 1             | `setTheory.Subset` — inclusive subset; source ⊂ permits equality                                                                                       |
| $\{\mid\}$                                                       | 1             | `labeled Set + Member/relations` — set-builder notation; labels are LaTeX, not a set-builder parser                                                    |
| $\phi$                                                           | 1             | `setTheory.Empty` — empty set; source φ                                                                                                                |
| $S-T$                                                            |               | `setTheory.DifferenceOf / finiteDifference` — difference                                                                                               |
| $S\times T$                                                      | 2             | `setTheory.ProductOf / finiteCartesianProduct` — binary Cartesian product                                                                              |
| $f^{-1}$                                                         | 3             | `InverseRelationOf / finiteInverseRelation / finitePreimage / inverseFiniteFunction` — inverse relation, preimage, and bijective inverse kept distinct |
| $f\circ g$                                                       | 3             | `CompositionOf / TopologicalCompositionOf / composeFiniteFunctions` — composition; f∘g applies g first                                                 |
| $f\mid W$                                                        | 3             | `RestrictionOf / TopologicalRestrictionOf / restrictFiniteFunction` — restriction                                                                      |
| $\le$                                                            | 5             | `PartialOrder / Precedes / IndexPrecedes` — partial order                                                                                              |
| glb                                                              | 6             | `GreatestLowerBoundOf / finiteOrderBounds.infimum` — greatest lower bound in ambient order                                                             |
| lub                                                              | 6             | `LeastUpperBoundOf / finiteOrderBounds.supremum` — least upper bound in ambient order                                                                  |
| $f:S\to T$                                                       | 3             | `Function / MapBetween / MapsTo` — function with stated domain and codomain; the book calls codomain range                                             |
| $S\oplus T$                                                      | 14            | `DirectSum / DirectSumOf / finiteDirectSum` — the book's binary componentwise product group                                                            |
| $X,D$                                                            | 16            | `Metric / MetricOn` — metric space                                                                                                                     |
| $X,\tau$                                                         | 40            | `Topology / TopologyOn` — topological space                                                                                                            |
| $\max(x_1,\ldots,x_n)$ = maximum of the numbers $x_1,\ldots,x_n$ | 17            | `Math.max` — finite real maximum used in metric/helper formulas                                                                                        |
| $\min(x_1,\ldots,x_n)$ = minimum of the numbers $x_1,\ldots,x_n$ | 21            | `Math.min` — finite real minimum used in metric/helper formulas                                                                                        |
| $\lvert x-y\rvert$                                               | 16            | `planeDistance or absolute real difference` — absolute-value metric                                                                                    |
| $N(x,p)$                                                         | 21            | `MetricNeighborhood / MetricNeighborhoodAt / inNeighborhood` — strict metric neighborhood                                                              |
| $s_i\to y$                                                       | 26, 122       | `SequenceConvergesTo / NetConvergesTo` — convergence supplied as a fact, not inferred from finite samples                                              |
| $D(x,y)$                                                         | 16            | `DistanceBetweenPoints / planeDistance` — point distance                                                                                               |
| $D(x,A)$                                                         | 34            | `DistanceToSet / pointDiskDistance` — point-to-set infimum distance                                                                                    |
| $D(A,B)$                                                         | 34            | `DistanceBetweenSets / ZeroSetDistance` — set-to-set infimum distance; zero need not mean intersection                                                 |
| $\operatorname{Cl}$                                              | 37, 55        | `ClosureOf / finiteClosure` — closure                                                                                                                  |
| $\operatorname{Fr}$                                              | 38, 55        | `FrontierOf / finiteFrontier` — frontier                                                                                                               |
| $A^\circ$                                                        | 43, 55        | `InteriorOf / finiteInterior` — interior                                                                                                               |
| $\operatorname{Ext}$                                             | 56            | `ExteriorOf / finiteExterior` — exterior                                                                                                               |
| $A'$                                                             | 56            | `DerivedSetOf / finiteDerivedSet` — derived set; delete the point itself from the tested subset                                                        |
| $\mathop{\Large\times}_I S_i$                                    | 84, 261       | `IndexedProductOf / IndexedProductTopologyOf` — arbitrary indexed product; no runtime enumeration of an infinite product                               |
| $\mathfrak a\to x$                                               | 133           | `FilterConvergesTo` — filter convergence                                                                                                               |
| $\{S_i\},\ i\in I$                                               | 3             | `IndexedSetFamily / IndexedFamilyOver / IndexedSetAt` — indexed family of sets                                                                         |
| $\{s_n\},\ n\in N$                                               | 3             | `Sequence / RealSequence` — sequence indexed by positive integers                                                                                      |
| $\{s_i\},\ i\in I$                                               | 117           | `Net / NetIndexedBy / NetValueAt` — general directed net                                                                                               |
| $d(A)$                                                           | 218           | `DiameterOf / circleDiameter / rectangleDiameter` — diameter, potentially infinite in general                                                          |
| $f\sim g$                                                        | 236           | `Homotopic / HomotopyBetween` — homotopy                                                                                                               |
| $Y^X$                                                            | 236           | `FunctionSpace / FunctionSpaceOf` — space of functions                                                                                                 |
| $L(Y,y_0)$                                                       | 241           | `LoopSpace / LoopSpaceOf` — based loop space                                                                                                           |
| $f:[0,1],\{0,1\}\to Y,y_0$                                       | 241           | `Loop / LoopBasedAt / PathEndpointsOf` — both endpoints at the base point                                                                              |
| $\lvert a\rvert$                                                 | 241           | `LoopHomotopyClass / LoopClassOf` — relative endpoint homotopy class                                                                                   |
| $\pi_1(Y,y_0),\#$                                                | 242, 246      | `FundamentalGroup / FundamentalGroupOf / GroupOperationOn` — fundamental group and its group operation                                                 |
| $a_1\#a_2$                                                       | 241           | `LoopConcatenation / LoopProductOf` — concatenation of representatives                                                                                 |
| $\lvert a_1\rvert\#\lvert a_2\rvert$                             | 242           | `LoopClassProductOf` — well-defined class product supplied as a mathematical fact                                                                      |
| $a^{-1}$                                                         | 13, 246       | `InverseElement / InverseLoop / LoopInverseOf` — group inverse or parameter-reversed loop                                                              |
| $f_*$                                                            | 253           | `InducedHomomorphism / InducedHomomorphismOf` — based continuous map induces group homomorphism                                                        |
| $\bar f$                                                         | 253           | `InducedHomomorphism / InducedLoopClassImage` — induced map on homotopy classes                                                                        |
| $X\simeq Y$                                                      | 257           | `HomotopyEquivalent / HomotopyInverseMaps` — homotopy equivalence; homotopic composites to identities                                                  |

## Reuse a book program for another illustration

The two torus figures reuse the same mathematical factory and style. A compact
product with both factor fibers requires additional facts, including an ordered
pair distinct from its factor point:

```ts
import { canvas, diagram } from "@penrose/bloom";
import { circleProductSubstance } from "@penrose/bloom/examples/quotient-spaces";
import { circleProductStyle } from "@penrose/bloom/styles/quotient-spaces";

const sub = circleProductSubstance(0.8, {
  fixedLabel: "p",
  bothFibers: true,
  compact: true,
});
const drawing = await diagram({
  sub,
  sty: circleProductStyle(),
  canvas: canvas(216, 134),
  variation: "another-circle-product",
  interactive: { jitter: 0 },
});
while (await drawing.optimizationStep()) {}
const { svg } = await drawing.render();
document.body.append(svg);
```

The Substance contains circles, singleton factors, product sets, topologies and
their relationships. Radii, camera projection, shading and label positions belong
to the style. The paired-fiber view uses the book's schematic fiber glyphs; it
preserves their marked incidence while allowing any fixed circle coordinate.

The [further illustrations](./further-illustrations) also reuse the coordinate
embedding factory from Figure 4.10 with a nonzero fixed coordinate and the other
coordinate varying. Their source panels show the actual generic declarations and
the arguments used to instantiate them.

## Native interactive layouts

`diagram({ interactive: { jitter: 0 }, ... })` enables the built-in annotation
layout at its canonical positions. `interactive: true`, or
`interactive: { jitter: 4 }`, samples annotation displacement using the diagram's
`variation` seed. Whole-construction and node movement are explicit Style
policies: enable the Style's `interactive` option as well. The book build
factories forward their final `FigureRenderOptions` to those policies.

For example, the early-table factory enables one shared table translation:

```ts
import { buildFourElementGroupTable } from "@penrose/bloom/examples/early-tables";

const drawing = await buildFourElementGroupTable({
  interactive: { jitter: 0 },
  variation: "canonical-group-table",
});
```

Construct a new diagram with another seed and nonzero jitter to re-sample its
enabled layout policy. Zero jitter restores canonical placement.

The book reader reuses the site's Bloom `Renderer` and `useDiagram` widgets.
Dragging and keyboard arrow adjustments update Penrose inputs and run its
optimizer. Ordinary figures move labels with their white knockouts. Figure 4.5
moves its disk, hatching and center point together. The cover moves its boundary
and complete curve family together; the two source tables move all headings,
cells, dots and grid lines as one construction. Figure 11.24 moves each of its
eleven complete spaces, preserving holes, cycles and the joins in `108`. The
original finite group kernels move nodes and incident arrows together.
All these layout policies preserve immutable Substance facts and mathematical
parameters; dragging or re-sampling does not change a homotopy time.

Metric neighborhoods retain the book's `N(x, ρ)` convention and strict
`D(x,y) < ρ` inequality. `planeDistance` and `inNeighborhood` expose the same
metric mathematics used by the styles. The function collar uses vertical
differences in the uniform metric; it does not restrict the domain to continuous
functions. A function sampler supplies illustrative geometry when no formula is
specified in the source.

## Shape coordinates and SVG

Penrose's `center`, `start`, `end`, and point arrays use a centered canvas with
upward-positive y. Native SVG `cx`/`cy`, line endpoint props, and rectangle
`x`/`y` use top-left, downward-positive coordinates and are converted into
optimizer geometry. SVG paint values such as named, hexadecimal, and RGB colors
are parsed into Penrose colors. Gradient references remain SVG attributes.

Use Penrose `PathData` arrays for paths and vector arrays for polygon points.
Unsupported native coordinate transformations or relative units produce explicit
errors. Draggable coordinates may each be affine expressions of one input; fixed,
nonlinear, or ambiguous shared-coordinate drag definitions are rejected.

`defs`, gradients, clipping elements, and other raw SVG elements can be written
in TSX. Each rendered diagram namespaces their identifiers and local references
so multiple diagrams can share a page safely. Grouped children retain raw
attributes and interactive metadata.

## Reproduce the local book preview

The source PDF and scan pages are local inputs excluded from Git. Import the
audited user-supplied edition with Python containing `pypdf` and Poppler on PATH:

```bash
python scripts/elementary-topology/import-source.py /path/to/ElementaryTopologyGemignani.pdf
yarn workspace @penrose/bloom build
yarn workspace @penrose/examples build
node scripts/elementary-topology/render-book.mjs packages/docs-site/public/elementary-topology/figures
yarn workspace @penrose/docs-site dev
```

The importer validates the source checksum and page count, renders the supplied
scan pages through Poppler, and writes the local reader assets. HTML formulas
use the site's local KaTeX renderer. The figure manifest
records source pages, reusable modules, review status, and placement rectangles.

## Step through a construction

The animated illustrations use native Penrose diagrams at each chosen mathematical parameter. The same frame factories can be used in another reader, lesson, or exploration:

```ts
import { buildInverseCancellationFrameFigure } from "@penrose/bloom/examples/loop-retracing";
import { buildContractionIterationFrameFigure } from "@penrose/bloom/examples/contraction-iterates";
import { buildRadialContractionFrameFigure } from "@penrose/bloom/examples/homotopies";

const loop = await buildInverseCancellationFrameFigure("figure-eight", 0.5);
const iterates = await buildContractionIterationFrameFigure(-0.6, 0.8, 3, 8, 4);
const disk = await buildRadialContractionFrameFigure(0.5);
```

Loop and disk time runs from zero to one. The disk uses the book’s parameter `r = 1 - time`: its image changes from the full disk to the singleton origin. For contraction iterates, `currentStep` selects the visible prefix of a declared iteration sequence; the mathematical values and axes stay fixed. Each factory accepts the usual rendering options as its final argument. Flip an animated illustration to inspect its actual invocation and reusable programs.
