import {
  domain,
  type DomainProgramBuilder,
  type EntityOf,
  type TypeDeclaration,
} from "../core/program.js";
import { declareDecimalExpansions } from "./decimal-expansions.js";

/** Declare the mathematical vocabulary inside one domain's own type context. */
export function declareSetTheory<const D extends string>(
  declarations: DomainProgramBuilder<D>,
) {
  const Set = declarations.type("Set");
  const Point = declarations.type("Point");
  const Subset = declarations.predicate("Subset", [Set, Set]);
  const Member = declarations.predicate("Member", [Point, Set]);
  const Disjoint = declarations.predicate("Disjoint", [Set, Set]);
  const Intersecting = declarations.predicate("Intersecting", [Set, Set]);
  return { Set, Point, Subset, Member, Disjoint, Intersecting };
}

export type SetTheoryDeclarations<D extends string = string> = ReturnType<
  typeof declareSetTheory<D>
>;

/** Compose the book's group vocabulary with an existing set/function vocabulary. */
export function declareGroupTheory<
  const D extends string,
  const B extends {
    Set: TypeDeclaration;
    Point: TypeDeclaration;
    Function: TypeDeclaration;
  },
>(declarations: DomainProgramBuilder<D>, base: B) {
  // Preserve each caller's nominal parent type through the generic factory.
  const Set: B["Set"] = base.Set;
  const Point: B["Point"] = base.Point;
  const Function: B["Function"] = base.Function;
  const BinaryOperation = declarations.type("BinaryOperation", Function);
  const Group = declarations.type("Group", Set);
  const AbelianGroup = declarations.type("AbelianGroup", Group);
  const Subgroup = declarations.type("Subgroup", Group);
  const DirectSum = declarations.type("DirectSum", Group);
  const GroupHomomorphism = declarations.type("GroupHomomorphism", Function);
  const GroupIsomorphism = declarations.type(
    "GroupIsomorphism",
    GroupHomomorphism,
  );
  const TrivialGroup = declarations.type("TrivialGroup", Group);
  const FreeGroup = declarations
    .type("FreeGroup", Group)
    .withData<{ rank: number }>();
  const FreeAbelianGroup = declarations
    .type("FreeAbelianGroup", AbelianGroup)
    .withData<{ rank: number }>();
  const InfiniteCyclicGroup = declarations.type(
    "InfiniteCyclicGroup",
    AbelianGroup,
  );
  const FiniteCyclicGroup = declarations
    .type("FiniteCyclicGroup", AbelianGroup)
    .withData<{ order: number }>();
  const NormalSubgroup = declarations.type("NormalSubgroup", Subgroup);
  const QuotientGroup = declarations.type("QuotientGroup", Group);
  const CyclicGroupElement = declarations
    .type("CyclicGroupElement", Point)
    .withData<{ residue: number }>();
  // Rings and ideals are background objects in the book's topology examples.
  const Ring = declarations.type("Ring", Set);
  const Ideal = declarations.type("Ideal", Set);
  return {
    BinaryOperation,
    Group,
    AbelianGroup,
    Subgroup,
    DirectSum,
    GroupHomomorphism,
    GroupIsomorphism,
    TrivialGroup,
    FreeGroup,
    FreeAbelianGroup,
    InfiniteCyclicGroup,
    FiniteCyclicGroup,
    NormalSubgroup,
    QuotientGroup,
    CyclicGroupElement,
    Ring,
    Ideal,
    RingOperationsOn: declarations.predicate("RingOperationsOn", [
      BinaryOperation,
      BinaryOperation,
      Ring,
    ]),
    IdealOf: declarations.predicate("IdealOf", [Ideal, Ring]),
    GroupOperationOn: declarations.predicate("GroupOperationOn", [
      BinaryOperation,
      Group,
    ]),
    ProductValue: declarations.predicate("ProductValue", [
      BinaryOperation,
      Point,
      Point,
      Point,
    ]),
    IdentityElement: declarations.predicate("IdentityElement", [Point, Group]),
    InverseElement: declarations.predicate("InverseElement", [
      Point,
      Point,
      Group,
    ]),
    GroupHomomorphismBetween: declarations.predicate(
      "GroupHomomorphismBetween",
      [GroupHomomorphism, Group, Group],
    ),
    GroupIsomorphismBetween: declarations.predicate("GroupIsomorphismBetween", [
      GroupIsomorphism,
      Group,
      Group,
    ]),
    GroupIsomorphicTo: declarations.predicate("GroupIsomorphicTo", [
      Group,
      Group,
    ]),
    QuotientGroupOf: declarations.predicate("QuotientGroupOf", [
      QuotientGroup,
      Group,
      NormalSubgroup,
    ]),
    SubgroupOf: declarations.predicate("SubgroupOf", [Subgroup, Group]),
    KernelOf: declarations.predicate("KernelOf", [Subgroup, GroupHomomorphism]),
    // The book's S⊕T is a binary Cartesian product with componentwise operation.
    DirectSumOf: declarations.predicate("DirectSumOf", [
      DirectSum,
      Group,
      Group,
    ]),
  };
}

/**
 * Chapter 1 vocabulary, independent of geometry or any particular finite model.
 * Supply an existing basic vocabulary when composing this with another domain;
 * declarations and their parent types must belong to the same builder.
 *
 * Predicates record facts supplied by the author. They do not prove group laws,
 * countability, the axiom of choice, or statements about infinite sets. The
 * finite helpers re-exported below can check concrete finite examples.
 */
export function declareElementarySetTheory<const D extends string>(
  declarations: DomainProgramBuilder<D>,
  sets: SetTheoryDeclarations<D> = declareSetTheory(declarations),
) {
  const { Set, Point } = sets;
  const BinaryRelation = declarations.type("BinaryRelation", Set);
  const EquivalenceRelation = declarations.type(
    "EquivalenceRelation",
    BinaryRelation,
  );
  const PartialOrder = declarations.type("PartialOrder", BinaryRelation);
  const TotalOrder = declarations.type("TotalOrder", PartialOrder);
  const WellOrder = declarations.type("WellOrder", TotalOrder);
  const Chain = declarations.type("Chain", Set);
  // A function is a relation and can itself be an element of a function space.
  const Function = declarations.type("Function", BinaryRelation, Point);
  const decimals = declareDecimalExpansions(declarations, { Point });
  const ChoiceFunction = declarations.type("ChoiceFunction", Function);
  const Permutation = declarations.type("Permutation", Function);
  const groups = declareGroupTheory(declarations, { Set, Point, Function });
  const SetFamily = declarations.type("SetFamily");
  const Partition = declarations.type("Partition", SetFamily);
  const EquivalenceClass = declarations.type("EquivalenceClass", Set);
  const Cardinal = declarations.type("Cardinal");
  const FiniteCardinal = declarations
    .type("FiniteCardinal", Cardinal)
    .withData<{ value: number }>();
  return {
    ...sets,
    ...groups,
    ...decimals,
    BinaryRelation,
    EquivalenceRelation,
    PartialOrder,
    TotalOrder,
    WellOrder,
    Chain,
    Function,
    ChoiceFunction,
    Permutation,
    SetFamily,
    Partition,
    EquivalenceClass,
    Cardinal,
    FiniteCardinal,
    Empty: declarations.predicate("Empty", [Set]),
    EqualSets: declarations.predicate("EqualSets", [Set, Set]),
    UnionOf: declarations.predicate("UnionOf", [Set, Set, Set]),
    IntersectionOf: declarations.predicate("IntersectionOf", [Set, Set, Set]),
    DifferenceOf: declarations.predicate("DifferenceOf", [Set, Set, Set]),
    ProductOf: declarations.predicate("ProductOf", [Set, Set, Set]),
    ComplementOf: declarations.predicate("ComplementOf", [Set, Set, Set]),
    PowerSetOf: declarations.predicate("PowerSetOf", [Set, Set]),
    SetInFamily: declarations.predicate("SetInFamily", [Set, SetFamily]),
    FamilyUnionIs: declarations.predicate("FamilyUnionIs", [SetFamily, Set]),
    FamilyIntersectionIs: declarations.predicate("FamilyIntersectionIs", [
      SetFamily,
      Set,
    ]),
    RelationOn: declarations.predicate("RelationOn", [BinaryRelation, Set]),
    RelationBetween: declarations.predicate("RelationBetween", [
      BinaryRelation,
      Set,
      Set,
    ]),
    RelatedUnder: declarations.predicate("RelatedUnder", [
      BinaryRelation,
      Point,
      Point,
    ]),
    InverseRelationOf: declarations.predicate("InverseRelationOf", [
      BinaryRelation,
      BinaryRelation,
    ]),
    PermutationOn: declarations.predicate("PermutationOn", [Permutation, Set]),
    MapBetween: declarations.predicate("MapBetween", [Function, Set, Set]),
    MapsTo: declarations.predicate("MapsTo", [Function, Point, Point]),
    OneToOne: declarations.predicate("OneToOne", [Function]),
    Onto: declarations.predicate("Onto", [Function]),
    ImageOf: declarations.predicate("ImageOf", [Set, Function, Set]),
    InverseImageOf: declarations.predicate("InverseImageOf", [
      Set,
      Function,
      Set,
    ]),
    RestrictionOf: declarations.predicate("RestrictionOf", [
      Function,
      Function,
      Set,
    ]),
    CompositionOf: declarations.predicate("CompositionOf", [
      Function,
      Function,
      Function,
    ]),
    InverseOf: declarations.predicate("InverseOf", [Function, Function]),
    IdentityOn: declarations.predicate("IdentityOn", [Function, Set]),
    FunctionKernelRelationOf: declarations.predicate(
      "FunctionKernelRelationOf",
      [EquivalenceRelation, Function],
    ),
    OrderOn: declarations.predicate("OrderOn", [PartialOrder, Set]),
    Precedes: declarations.predicate("Precedes", [PartialOrder, Point, Point]),
    InducedOrderOn: declarations.predicate("InducedOrderOn", [
      PartialOrder,
      PartialOrder,
      Set,
    ]),
    ChainIn: declarations.predicate("ChainIn", [Chain, PartialOrder]),
    UpperBoundOf: declarations.predicate("UpperBoundOf", [
      Point,
      Set,
      PartialOrder,
    ]),
    LowerBoundOf: declarations.predicate("LowerBoundOf", [
      Point,
      Set,
      PartialOrder,
    ]),
    LeastUpperBoundOf: declarations.predicate("LeastUpperBoundOf", [
      Point,
      Set,
      PartialOrder,
    ]),
    GreatestLowerBoundOf: declarations.predicate("GreatestLowerBoundOf", [
      Point,
      Set,
      PartialOrder,
    ]),
    MaximalIn: declarations.predicate("MaximalIn", [Point, Set, PartialOrder]),
    MinimalIn: declarations.predicate("MinimalIn", [Point, Set, PartialOrder]),
    GreatestIn: declarations.predicate("GreatestIn", [
      Point,
      Set,
      PartialOrder,
    ]),
    LeastIn: declarations.predicate("LeastIn", [Point, Set, PartialOrder]),
    EquivalenceOn: declarations.predicate("EquivalenceOn", [
      EquivalenceRelation,
      Set,
    ]),
    EquivalentUnder: declarations.predicate("EquivalentUnder", [
      EquivalenceRelation,
      Point,
      Point,
    ]),
    ClassOf: declarations.predicate("ClassOf", [
      EquivalenceClass,
      Point,
      EquivalenceRelation,
    ]),
    PartitionOf: declarations.predicate("PartitionOf", [Partition, Set]),
    ClassInPartition: declarations.predicate("ClassInPartition", [
      EquivalenceClass,
      Partition,
    ]),
    PartitionBy: declarations.predicate("PartitionBy", [
      Partition,
      EquivalenceRelation,
    ]),
    QuotientSetOf: declarations.predicate("QuotientSetOf", [
      Set,
      Set,
      EquivalenceRelation,
    ]),
    InducedMapOnClasses: declarations.predicate("InducedMapOnClasses", [
      Function,
      Function,
      EquivalenceRelation,
    ]),
    Finite: declarations.predicate("Finite", [Set]),
    Infinite: declarations.predicate("Infinite", [Set]),
    Uncountable: declarations.predicate("Uncountable", [Set]),
    Countable: declarations.predicate("Countable", [Set]),
    CardinalityOf: declarations.predicate("CardinalityOf", [Cardinal, Set]),
    SameCardinality: declarations.predicate("SameCardinality", [Set, Set]),
    CardinalityAtMost: declarations.predicate("CardinalityAtMost", [
      Cardinal,
      Cardinal,
    ]),
    CardinalityStrictlyGreater: declarations.predicate(
      "CardinalityStrictlyGreater",
      [Cardinal, Cardinal],
    ),
    ChoiceFor: declarations.predicate("ChoiceFor", [ChoiceFunction, SetFamily]),
    Enumerates: declarations.predicate("Enumerates", [Function, Set]),
  };
}

export type ElementarySetTheoryDeclarations<D extends string = string> =
  ReturnType<typeof declareElementarySetTheory<D>>;

const declarations = domain("set-theory");
const sets = declareElementarySetTheory(declarations);
const CountableSet = declarations
  .type("CountableSet", sets.Set)
  .withData<{ index: number }>();
const CountableFamily = declarations.type("CountableFamily");
const IndexedElement = declarations
  .type("IndexedElement", sets.Point)
  .withData<{ index: number }>();
const ArrayEnumeration = declarations.type("ArrayEnumeration");
const FamilyMember = declarations.predicate("FamilyMember", [
  CountableSet,
  CountableFamily,
]);
const EnumeratesArray = declarations.predicate("EnumeratesArray", [
  ArrayEnumeration,
  CountableFamily,
]);

/** Shape-free set and point declarations, with directed mathematical facts. */
export const setTheory = declarations.make({
  ...sets,
  CountableSet,
  CountableFamily,
  IndexedElement,
  ArrayEnumeration,
  FamilyMember,
  EnumeratesArray,
});

export type MathematicalSet = EntityOf<typeof setTheory.Set>;
export type SetPoint = EntityOf<typeof setTheory.Point>;

/** Nonnegative rank for a finite-rank free or free abelian group declaration. */
export function freeGroupData(rank: number) {
  if (!Number.isSafeInteger(rank) || rank < 0)
    throw new Error("A group rank must be a nonnegative safe integer");
  return Object.freeze({ rank });
}

export * from "./decimal-expansions.js";
export * from "./finite-set-theory.js";

/**
 * The book's zigzag visits every pair of positive integers exactly once.
 * It enumerates array positions; equal values in different sets still require
 * duplicate removal when constructing a bijection with the union itself.
 */
export function diagonalEnumerationPrefix(
  count: number,
): readonly (readonly [number, number])[] {
  if (!Number.isSafeInteger(count) || count < 0 || count > 100000)
    throw new Error("An enumeration prefix needs 0–100000 positions");
  const positions: [number, number][] = [];
  for (let sum = 2; positions.length < count; sum++) {
    const rows = Array.from({ length: sum - 1 }, (_, i) => i + 1);
    if (sum % 2 === 0) rows.reverse();
    for (const row of rows) {
      positions.push([row, sum - row]);
      if (positions.length === count) break;
    }
  }
  return positions;
}
