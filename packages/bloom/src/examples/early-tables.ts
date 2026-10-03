import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  complementedDiagonalPrefix,
  finiteCyclicGroup,
  finiteDirectSum,
  setTheory,
  type FiniteGroup,
} from "../domains/set-theory.js";
import { decimalTableStyle } from "../styles/decimal-tables.js";
import { groupTableStyle } from "../styles/group-tables.js";

/** Exactly the visible prefixes in §1.3 Example 9, printed page 11. */
export const sourceCantorDecimalPrefixes: readonly (readonly (0 | 1)[])[] =
  Object.freeze(
    ["011010", "1110111", "101101", "0000000111"].map((row) =>
      Object.freeze(Array.from(row, (digit) => Number(digit) as 0 | 1)),
    ),
  );

/** Unseen digits and an assumed infinite bijection are never inferred from this window. */
export function decimalMapWindowSubstance(
  prefixes: readonly (readonly (0 | 1)[])[] = sourceCantorDecimalPrefixes,
) {
  if (!prefixes.length || prefixes.length > 16)
    throw new Error("A displayed decimal table supports 1–16 rows");
  complementedDiagonalPrefix(prefixes);
  const s = setTheory.substance();
  const N = s.Set({ label: "N" }),
    S = s.Set({ label: "S" });
  const f = s.Function({ label: "f" });
  s.MapBetween(f, N, S);
  s.Countable(N);
  prefixes.forEach((digits, i) => {
    const n = s.PositiveInteger({ value: i + 1, label: String(i + 1) });
    const value = s.ZeroOneDecimalExpansion({ label: `f(${i + 1})` });
    const prefix = s.ZeroOneDecimalPrefix({ digits });
    s.Member(n, N);
    s.Member(value, S);
    s.MapsTo(f, n, value);
    s.DecimalPrefixOf(prefix, value);
  });
  return s.make();
}

/** Every group product, identity and inverse remains mathematical Substance. */
export function finiteGroupTableSubstance<T>(
  model: FiniteGroup<T>,
  label: (value: T, index: number) => string,
  operationLabel = "\\#",
) {
  if (!model.elements.length || model.elements.length > 16)
    throw new Error("A displayed operation table supports 1–16 elements");
  const s = setTheory.substance(),
    G = s.Group({ label: "S" }),
    operation = s.BinaryOperation({ label: operationLabel });
  s.GroupOperationOn(operation, G);
  s.Finite(G);
  const points = model.elements.map((value, index) =>
    s.Point({ label: label(value, index) }),
  );
  const pointsByValue = new Map(
    model.elements.map((value, i) => [value, points[i]] as const),
  );
  const point = (value: T) => {
    const result = pointsByValue.get(value);
    if (!result) throw new Error("A finite table value is outside its group");
    return result;
  };
  model.elements.forEach((a, i) => {
    s.Member(points[i], G);
    s.InverseElement(points[i], point(model.inverse(a)), G);
    model.elements.forEach((b, j) =>
      s.ProductValue(
        operation,
        points[i],
        points[j],
        point(model.operation(a, b)),
      ),
    );
  });
  s.IdentityElement(point(model.identity), G);
  return s.make();
}

/** The printed table is the Klein four group, with the source ordering s1…s4. */
export function fourElementGroupTableSubstance() {
  const two = finiteCyclicGroup(2);
  return finiteGroupTableSubstance(
    finiteDirectSum(two, two),
    (_value, index) => `s_{${index + 1}}`,
  );
}

export const buildCantorDecimalTable = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: decimalMapWindowSubstance(),
    sty: decimalTableStyle({
      interactive: renderOptions.interactive
        ? typeof renderOptions.interactive === "object"
          ? renderOptions.interactive
          : { jitter: 0 }
        : false,
    }),
    canvas: canvas(270, 165),
    variation: "cantor-decimal-table",
    ...renderOptions,
  });

export const buildFourElementGroupTable = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: fourElementGroupTableSubstance(),
    sty: groupTableStyle({
      interactive: renderOptions.interactive
        ? typeof renderOptions.interactive === "object"
          ? renderOptions.interactive
          : { jitter: 0 }
        : false,
    }),
    canvas: canvas(250, 180),
    variation: "four-element-group-table",
    ...renderOptions,
  });
