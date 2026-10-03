import { expect, test } from "vitest";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { diagonalEnumerationPrefix } from "../domains/set-theory.js";
import { countableFamilyEnumeration } from "../examples/countable-enumeration.js";
import { countableEnumerationStyle } from "./countable-enumeration.js";

test("zigzag agrees with the book and reaches every pair once", () => {
  expect(diagonalEnumerationPrefix(6)).toEqual([
    [1, 1],
    [1, 2],
    [2, 1],
    [3, 1],
    [2, 2],
    [1, 3],
  ]);
  const prefix = diagonalEnumerationPrefix(210);
  expect(new Set(prefix.map(([r, c]) => `${r},${c}`)).size).toBe(210);
  for (let r = 1; r < 20; r++)
    for (let c = 1; c <= 21 - r; c++) expect(prefix).toContainEqual([r, c]);
  expect(diagonalEnumerationPrefix(0)).toEqual([]);
  expect(() => diagonalEnumerationPrefix(1.5)).toThrow("positions");
});

test("one style renders different mathematical families through Penrose", async () => {
  const style = countableEnumerationStyle();
  for (const [rows, columns] of [
    [5, 6],
    [3, 4],
  ]) {
    const drawing = await diagram({
      sub: countableFamilyEnumeration(rows, columns),
      sty: style,
      canvas: canvas(360, 230),
    });
    const { svg } = await drawing.render();
    const names = Array.from(svg.querySelectorAll("title")).map(
      (title) => title.textContent ?? "",
    );
    expect(
      names.filter((name) => name.startsWith("enumeration.element-")),
    ).toHaveLength(rows * columns);
    expect(
      names.filter((name) => name.startsWith("enumeration.set-")),
    ).toHaveLength(rows);
    expect(svg.querySelectorAll("line").length).toBeGreaterThan(4);
    for (const arrow of Array.from(svg.querySelectorAll("line")))
      expect(arrow.getAttribute("marker-end")).toMatch(/^url\(#/);
    expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    drawing.discard();
  }
});
