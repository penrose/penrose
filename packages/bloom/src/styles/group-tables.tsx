/** @jsxImportSource @penrose/bloom */
import type { InteractiveLayoutOptions } from "../core/builder.js";
import type { DomainProgram } from "../core/program.js";
import type { Equation, Line } from "../core/types.js";
import {
  createFiniteGroup,
  setTheory,
  type ElementarySetTheoryDeclarations,
} from "../domains/set-theory.js";
import { draggableTableConstruction } from "./table-construction.js";

export interface GroupTableStyleOptions {
  columnSpacing?: number;
  rowSpacing?: number;
  fontSize?: string;
  interactive?: false | InteractiveLayoutOptions;
}

/** A mathematical operation table; every cell comes from a ProductValue fact. */
export function groupTableStyleFor<
  const D extends string,
  T extends ElementarySetTheoryDeclarations<D>,
>(mathematics: DomainProgram<T>, options: GroupTableStyleOptions = {}) {
  const dx = options.columnSpacing ?? 43,
    dy = options.rowSpacing ?? 29;
  if (![dx, dy].every((x) => Number.isFinite(x) && x >= 24))
    throw new Error("A group table needs finite cell spacing of at least 24");
  return mathematics.style((ctx) => {
    const d = mathematics.definitions;
    const operations = ctx.facts(d.GroupOperationOn);
    if (operations.length !== 1)
      throw new Error("An operation table needs exactly one displayed group");
    const [operation, group] = operations[0];
    const elements = ctx
      .entities(d.Point)
      .filter((p) => ctx.test(d.Member, p, group));
    if (!elements.length || elements.length > 16)
      throw new Error(
        "A displayed finite operation table supports 1–16 elements",
      );
    const products = ctx
      .facts(d.ProductValue)
      .filter(([op]) => op === operation);
    if (products.length !== elements.length ** 2)
      throw new Error(
        "An operation table must contain one product for every pair",
      );
    const table = elements.map((a) =>
      elements.map((b) => {
        const results = products
          .filter(([, x, y]) => x === a && y === b)
          .map(([, , , z]) => z);
        if (results.length !== 1 || !elements.includes(results[0]))
          throw new Error("Each table cell needs one product inside the group");
        return results[0];
      }),
    );
    const identityFacts = ctx
      .facts(d.IdentityElement)
      .filter(([, G]) => G === group);
    if (identityFacts.length !== 1 || !elements.includes(identityFacts[0][0]))
      throw new Error("Record the unique identity of the displayed group");
    const identity = identityFacts[0][0];
    // This validates closure, every associative triple, and both-sided inverses.
    const checked = createFiniteGroup(
      elements,
      (a, b) => table[elements.indexOf(a)][elements.indexOf(b)],
    );
    if (checked.identity !== identity)
      throw new Error(
        "The recorded identity must agree with the operation table",
      );
    for (const a of elements) {
      const inverses = ctx
        .facts(d.InverseElement)
        .filter(([x, , G]) => x === a && G === group);
      if (inverses.length !== 1 || inverses[0][1] !== checked.inverse(a))
        throw new Error(
          "Recorded inverses must agree with the complete operation table",
        );
    }
    const count = elements.length + 1;
    const position = (r: number, c: number): [number, number] => [
      (c - (count - 1) / 2) * dx,
      ((count - 1) / 2 - r) * dy,
    ];
    const shapes: (Equation | Line)[] = [];
    const label = (value: string, r: number, c: number) => {
      const shape = (
        <equation
          name={`group-table.cell-${r}-${c}`}
          center={position(r, c)}
          font-size={options.fontSize ?? "17px"}
          fill-color={[0.08, 0.08, 0.08, 1]}
          data-tex={encodeURIComponent(value)}
        >
          {value}
        </equation>
      ) as Equation;
      shapes.push(shape);
      return shape;
    };
    label(operation.label, 0, 0);
    elements.forEach((a, i) => {
      label(a.label, 0, i + 1);
      label(a.label, i + 1, 0);
      table[i].forEach((result, j) => label(result.label, i + 1, j + 1));
    });
    const left = position(0, 0)[0] - dx / 2,
      right = position(0, count - 1)[0] + dx / 2,
      top = position(0, 0)[1] + dy / 2,
      bottom = position(count - 1, 0)[1] - dy / 2;
    // The source uses internal lines only; it has no rectangular outer border.
    for (let i = 1; i < count; i++) {
      const x = left + i * dx,
        y = top - i * dy;
      const column = (
        <line
          name={`group-table.column-${i}`}
          start={[x, top]}
          end={[x, bottom]}
          stroke-color={[0.08, 0.08, 0.08, 1]}
          stroke-width={0.8}
        />
      ) as Line;
      const row = (
        <line
          name={`group-table.row-${i}`}
          start={[left, y]}
          end={[right, y]}
          stroke-color={[0.08, 0.08, 0.08, 1]}
          stroke-width={0.8}
        />
      ) as Line;
      shapes.push(column, row);
    }
    if (options.interactive)
      draggableTableConstruction(
        ctx.builder,
        shapes,
        "group-table.construction",
        options.interactive,
      );
  });
}

export const groupTableStyle = (options: GroupTableStyleOptions = {}) =>
  groupTableStyleFor(setTheory, options);
