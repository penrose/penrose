/** @jsxImportSource @penrose/bloom */
import { add, div } from "@penrose/core";
import type { InteractiveLayoutOptions } from "../core/builder.js";
import type { DomainProgram } from "../core/program.js";
import type { Equation } from "../core/types.js";
import {
  complementedDiagonalPrefix,
  setTheory,
  type ElementarySetTheoryDeclarations,
} from "../domains/set-theory.js";
import { draggableTableConstruction } from "./table-construction.js";

export interface DecimalTableStyleOptions {
  rowSpacing?: number;
  integerColumn?: number;
  decimalLeft?: number;
  fontSize?: string;
  interactive?: false | InteractiveLayoutOptions;
}

/** A finite displayed window of a map into unending 0/1 decimal expansions. */
export function decimalTableStyleFor<
  const D extends string,
  T extends ElementarySetTheoryDeclarations<D>,
>(mathematics: DomainProgram<T>, options: DecimalTableStyleOptions = {}) {
  const dy = options.rowSpacing ?? 22,
    integerColumn = options.integerColumn ?? -65,
    decimalLeft = options.decimalLeft ?? -5;
  if (
    !Number.isFinite(dy) ||
    dy < 18 ||
    ![integerColumn, decimalLeft].every(Number.isFinite)
  )
    throw new Error(
      "A decimal table needs finite columns and row spacing of at least 18",
    );
  return mathematics.style((ctx) => {
    const d = mathematics.definitions;
    const maps = ctx.facts(d.MapsTo);
    if (!maps.length || new Set(maps.map(([f]) => f)).size !== 1)
      throw new Error("A decimal table needs the displayed values of one map");
    const integers = [...ctx.entities(d.PositiveInteger)].sort(
      (a, b) => a.value - b.value,
    );
    const expansions = ctx.entities(d.ZeroOneDecimalExpansion);
    if (!integers.length || integers.some((n, i) => n.value !== i + 1))
      throw new Error(
        "Displayed integer arguments must start at one and be consecutive",
      );
    const rows = integers.map((n) => {
      const values = maps
        .filter(([, x]) => x === n)
        .map(([, , y]) => expansions.find((v) => v === y));
      if (values.length !== 1 || !values[0])
        throw new Error(
          "Each integer needs exactly one displayed decimal expansion",
        );
      const prefixes = ctx
        .facts(d.DecimalPrefixOf)
        .filter(([, expansion]) => expansion === values[0])
        .map(([p]) =>
          ctx.entities(d.ZeroOneDecimalPrefix).find((v) => v === p),
        );
      if (prefixes.length !== 1 || !prefixes[0])
        throw new Error(
          "Record exactly one known 0/1 decimal prefix for each value",
        );
      return prefixes[0];
    });
    if (maps.length !== integers.length)
      throw new Error("Every displayed map value needs its integer argument");
    // This checks known digits only, never an infinite enumeration conclusion.
    complementedDiagonalPrefix(rows.map((row) => row.digits));
    const y = (r: number) => ((rows.length + 1) / 2 - r) * dy;
    const shapes: Equation[] = [];
    const label = (text: string, x: number, r: number, name: string) => {
      const shape = (
        <equation
          name={name}
          center={[x, y(r)]}
          font-size={options.fontSize ?? "17px"}
          fill-color={[0.08, 0.08, 0.08, 1]}
          data-tex={encodeURIComponent(text)}
        >
          {text}
        </equation>
      ) as Equation;
      shapes.push(shape);
      return shape;
    };
    label("n", integerColumn, 0, "decimal-table.integer-header");
    const decimalLabels = rows.map((prefix, i) => {
      label(
        String(integers[i].value),
        integerColumn,
        i + 1,
        `decimal-table.integer-${i + 1}`,
      );
      const value = label(
        `0.${prefix.digits.join("")}\\cdots`,
        0,
        i + 1,
        `decimal-table.value-${i + 1}`,
      );
      // Native equation metrics align the decimal points even for unequal prefixes.
      value.center[0] = add(decimalLeft, div(value.width, 2));
      return value;
    });
    const centerColumn = add(decimalLeft, div(decimalLabels[0].width, 2));
    const header = label("f(n)", 0, 0, "decimal-table.map-header");
    header.center[0] = centerColumn;
    label(
      "\\vdots",
      integerColumn,
      rows.length + 1,
      "decimal-table.integer-continuation",
    );
    const dots = label(
      "\\vdots",
      0,
      rows.length + 1,
      "decimal-table.value-continuation",
    );
    dots.center[0] = centerColumn;
    if (options.interactive)
      draggableTableConstruction(
        ctx.builder,
        shapes,
        "decimal-table.construction",
        options.interactive,
      );
  });
}

export const decimalTableStyle = (options: DecimalTableStyleOptions = {}) =>
  decimalTableStyleFor(setTheory, options);
