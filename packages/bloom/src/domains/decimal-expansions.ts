import type { DomainProgramBuilder, TypeDeclaration } from "../core/program.js";

/** Compose decimal notation without mistaking a known prefix for an infinite value. */
export function declareDecimalExpansions<
  const D extends string,
  const B extends { Point: TypeDeclaration },
>(declarations: DomainProgramBuilder<D>, base: B) {
  const Point: B["Point"] = base.Point;
  const PositiveInteger = declarations
    .type("PositiveInteger", Point)
    .withData<{ value: number }>();
  const DecimalExpansion = declarations.type("DecimalExpansion", Point);
  const ZeroOneDecimalExpansion = declarations.type(
    "ZeroOneDecimalExpansion",
    DecimalExpansion,
  );
  const DecimalPrefix = declarations
    .type("DecimalPrefix", Point)
    .withData<{ digits: readonly number[] }>();
  const ZeroOneDecimalPrefix = declarations
    .type("ZeroOneDecimalPrefix", DecimalPrefix)
    .withData<{ digits: readonly (0 | 1)[] }>();
  return {
    PositiveInteger,
    DecimalExpansion,
    ZeroOneDecimalExpansion,
    DecimalPrefix,
    ZeroOneDecimalPrefix,
    DecimalPrefixOf: declarations.predicate("DecimalPrefixOf", [
      DecimalPrefix,
      DecimalExpansion,
    ]),
  };
}

/** The known digits 1-a_nn in a finite window of the book's decimal argument. */
export function complementedDiagonalPrefix(
  rows: readonly (readonly (0 | 1)[])[],
): readonly (0 | 1)[] {
  if (rows.length > 10000)
    throw new Error("A displayed decimal window supports at most 10000 rows");
  if (
    Array.from(rows).some(
      (row, index) =>
        !Array.isArray(row) ||
        row.length <= index ||
        Array.from(row).some((digit) => digit !== 0 && digit !== 1),
    )
  )
    throw new Error(
      "Each row needs 0/1 digits including its diagonal position",
    );
  return Object.freeze(rows.map((row, i) => (row[i] === 0 ? 1 : 0)));
}
