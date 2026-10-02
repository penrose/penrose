/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { diagonalEnumerationPrefix, setTheory } from "../domains/set-theory.js";

export interface CountableEnumerationStyleOptions {
  columnSpacing?: number;
  rowSpacing?: number;
  fontSize?: string;
}

/** A finite window of countably many countable sets, with a zigzag traversal. */
export function countableEnumerationStyle(
  options: CountableEnumerationStyleOptions = {},
) {
  const dx = options.columnSpacing ?? 45,
    dy = options.rowSpacing ?? 34;
  if (![dx, dy].every((v) => Number.isFinite(v) && v > 18))
    throw new Error("An enumeration grid needs spacing greater than 18");
  return setTheory.style((ctx) => {
    const families = ctx.entities(setTheory.CountableFamily);
    if (families.length !== 1)
      throw new Error("An enumeration panel needs one countable family");
    const family = families[0];
    const rows = ctx
      .facts(setTheory.FamilyMember)
      .filter(([, f]) => f === family)
      .map(([set]) => set)
      .sort((a, b) => a.index - b.index);
    if (!rows.length || rows.some((row, i) => row.index !== i + 1))
      throw new Error(
        "Displayed family indices must start at one and be consecutive",
      );
    const indexed = ctx.entities(setTheory.IndexedElement);
    const members = rows.map((row) =>
      indexed
        .filter((point) => ctx.test(setTheory.Member, point, row))
        .sort((a, b) => a.index - b.index),
    );
    const columns = Math.max(...members.map((row) => row.length));
    if (
      !columns ||
      members.some(
        (row) =>
          row.length !== columns ||
          row.some((point, i) => point.index !== i + 1),
      )
    )
      throw new Error(
        "Displayed member indices must form a rectangular consecutive window",
      );
    const p = (row: number, column: number): [number, number] => [
      (column - (columns + 1) / 2) * dx + dx / 2,
      ((rows.length + 2) / 2 - row) * dy,
    ];
    const ink: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
    const label = (text: string, center: Vec2, name: string) => (
      <equation
        name={name}
        center={center}
        font-size={options.fontSize ?? "15px"}
        fill-color={ink}
      >
        {text}
      </equation>
    );
    rows.forEach((set, r) => {
      label(set.label, p(r + 1, 0), `enumeration.set-${r + 1}`);
      members[r].forEach((point, c) =>
        label(
          point.label,
          p(r + 1, c + 1),
          `enumeration.element-${r + 1}-${c + 1}`,
        ),
      );
      label(
        "\\cdots",
        p(r + 1, columns + 0.58),
        `enumeration.row-dots-${r + 1}`,
      );
    });
    for (let c = 0; c <= columns; c++)
      label("\\vdots", p(rows.length + 1, c), `enumeration.column-dots-${c}`);
    if (!ctx.facts(setTheory.EnumeratesArray).some(([, f]) => f === family))
      return;
    // Draw complete visible diagonals, including the final arrow into ellipses.
    const endSum = Math.min(columns + 1, rows.length + 2);
    const prefix = diagonalEnumerationPrefix((endSum * (endSum - 1)) / 2);
    for (let i = 1; i < prefix.length; i++) {
      const [r1, c1] = prefix[i - 1],
        [r2, c2] = prefix[i];
      if (
        r1 > rows.length ||
        c1 > columns ||
        r2 > rows.length + 1 ||
        c2 > columns
      )
        break;
      const a = p(r1, c1),
        b = p(r2, c2),
        length = Math.hypot(b[0] - a[0], b[1] - a[1]);
      const trim = (at: [number, number], sign: number): [number, number] => [
        at[0] + (sign * 12 * (b[0] - a[0])) / length,
        at[1] + (sign * 9 * (b[1] - a[1])) / length,
      ];
      <line
        name={`enumeration.step-${i}`}
        start={trim(a, 1)}
        end={trim(b, -1)}
        stroke-color={ink}
        stroke-width={0.7}
        end-arrowhead="straight"
        end-arrowhead-size={0.65}
      />;
    }
  });
}
