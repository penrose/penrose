import {
  domain,
  type DomainProgramBuilder,
  type EntityOf,
} from "../core/program.js";

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

const declarations = domain("set-theory");
const sets = declareSetTheory(declarations);
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
