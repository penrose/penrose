import type { FigureRenderOptions } from "../core/program.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { setTheory } from "../domains/set-theory.js";
import { countableEnumerationStyle } from "../styles/countable-enumeration.js";

/** Proposition 4's finite displayed window of an infinite countable family. */
export function countableFamilyEnumeration(rows = 5, columns = 6) {
  if (
    ![rows, columns].every((v) => Number.isSafeInteger(v) && v > 0 && v <= 20)
  )
    throw new Error("The displayed window needs 1–20 rows and columns");
  const s = setTheory.substance();
  const family = s.CountableFamily({ label: "\\{A_n\\}_{n\\in N}" });
  const enumeration = s.ArrayEnumeration({ label: "f" });
  s.EnumeratesArray(enumeration, family);
  for (let r = 1; r <= rows; r++) {
    const set = s.CountableSet({ label: `A_${r}`, index: r });
    s.FamilyMember(set, family);
    for (let c = 1; c <= columns; c++) {
      const digits = r < 10 && c < 10 ? `${r}${c}` : `${r},${c}`;
      const point = s.IndexedElement({ label: `a_{${digits}}`, index: c });
      s.Member(point, set);
    }
  }
  return s.make();
}

export const buildCountableEnumerationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: countableFamilyEnumeration(),
    sty: countableEnumerationStyle(),
    canvas: canvas(360, 230),
    variation: "gemignani-countable-union-enumeration",
    ...renderOptions,
  });
