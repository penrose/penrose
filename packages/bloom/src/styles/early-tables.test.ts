import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, domain } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  complementedDiagonalPrefix,
  declareDecimalExpansions,
  declareElementarySetTheory,
  finiteCyclicGroup,
  setTheory,
} from "../domains/set-theory.js";
import {
  buildCantorDecimalTable,
  buildFourElementGroupTable,
  decimalMapWindowSubstance,
  finiteGroupTableSubstance,
  fourElementGroupTableSubstance,
  sourceCantorDecimalPrefixes,
} from "../examples/early-tables.js";
import { decimalTableStyle } from "./decimal-tables.js";
import { groupTableStyle } from "./group-tables.js";

async function render(d: Diagram, filename?: string) {
  try {
    let finished = false;
    for (let i = 0; i < 1000; i++)
      if (!(await d.optimizationStep())) {
        finished = true;
        break;
      }
    expect(finished).toBe(true);
    const result = await d.render();
    const xml = new XMLSerializer().serializeToString(result.svg);
    expect(xml).not.toMatch(/NaN|Infinity|undefined/);
    expect(
      new DOMParser()
        .parseFromString(xml, "image/svg+xml")
        .querySelector("parsererror"),
    ).toBeNull();
    expect(result.svg.querySelectorAll("image")).toHaveLength(0);
    const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
    if (destination && filename) {
      mkdirSync(destination, { recursive: true });
      writeFileSync(join(destination, filename), xml);
    }
    return result;
  } finally {
    d.discard();
  }
}

test("Cantor's known decimal digits are faithful finite prefixes, and the diagonal changes every displayed row", () => {
  expect(sourceCantorDecimalPrefixes.map((row) => row.join(""))).toEqual([
    "011010",
    "1110111",
    "101101",
    "0000000111",
  ]);
  const complement = complementedDiagonalPrefix(sourceCantorDecimalPrefixes);
  expect(complement).toEqual([1, 0, 0, 1]);
  sourceCantorDecimalPrefixes.forEach((row, i) =>
    expect(complement[i]).not.toBe(row[i]),
  );
  expect(Object.isFrozen(complement)).toBe(true);
  expect(() => complementedDiagonalPrefix([[0], [1]])).toThrow("diagonal");
  expect(() =>
    Reflect.apply(complementedDiagonalPrefix, undefined, [[[2]]]),
  ).toThrow("0/1");
  expect(() => complementedDiagonalPrefix([new Array<0 | 1>(1)])).toThrow(
    "0/1",
  );
  const sub = decimalMapWindowSubstance();
  expect(
    sub.propositions.filter((p) => p.predicate === setTheory.DecimalPrefixOf),
  ).toHaveLength(4);
  for (const predicate of [
    setTheory.Onto,
    setTheory.OneToOne,
    setTheory.Enumerates,
    setTheory.Uncountable,
  ])
    expect(sub.propositions.some((p) => p.predicate === predicate)).toBe(false);
  const d = domain("composed-decimals"),
    Point = d.type("Point");
  const vocabulary = declareDecimalExpansions(d, { Point });
  const dom = d.make({ Point, ...vocabulary }),
    s = dom.substance();
  const expansion = s.ZeroOneDecimalExpansion(),
    prefix = s.ZeroOneDecimalPrefix({ digits: [0, 1] });
  s.DecimalPrefixOf(prefix, expansion);
  expect(s.make().propositions).toHaveLength(1);
});

test("the four-element source table has all sixteen exact products and self-inverses", () => {
  const sub = fourElementGroupTableSubstance();
  const products = sub.propositions.filter(
    (p) => p.predicate === setTheory.ProductValue,
  );
  const label = (entity: unknown) => (entity as { label: string }).label;
  const rows = Array.from({ length: 4 }, (_, r) =>
    Array.from({ length: 4 }, (_, c) => {
      const fact = products.find(
        (p) =>
          label(p.args[1]) === `s_{${r + 1}}` &&
          label(p.args[2]) === `s_{${c + 1}}`,
      )!;
      return label(fact.args[3]);
    }),
  );
  expect(rows).toEqual([
    ["s_{1}", "s_{2}", "s_{3}", "s_{4}"],
    ["s_{2}", "s_{1}", "s_{4}", "s_{3}"],
    ["s_{3}", "s_{4}", "s_{1}", "s_{2}"],
    ["s_{4}", "s_{3}", "s_{2}", "s_{1}"],
  ]);
  const inverse = sub.propositions.filter(
    (p) => p.predicate === setTheory.InverseElement,
  );
  expect(inverse).toHaveLength(4);
  inverse.forEach((p) => expect(p.args[0]).toBe(p.args[1]));
  const d = domain("composed-table");
  const algebra = d.make(declareElementarySetTheory(d));
  expect(algebra.GroupOperationOn.name).toBe("GroupOperationOn");
});

test("native tables preserve source layout and both styles apply to different mathematical programs", async () => {
  const decimals = await render(
    await buildCantorDecimalTable(),
    "figure-unnumbered-cantor-diagonal-table.svg",
  );
  expect(decimals.svg.querySelectorAll("line")).toHaveLength(0);
  const labels = Array.from(decimals.svg.querySelectorAll("[data-tex]"), (e) =>
    decodeURIComponent(e.getAttribute("data-tex")!),
  );
  for (const text of [
    "n",
    "f(n)",
    "0.011010\\cdots",
    "0.1110111\\cdots",
    "0.101101\\cdots",
    "0.0000000111\\cdots",
  ])
    expect(labels).toContain(text);
  const leftPositions = [1, 2, 3, 4].map((i) => {
    const element = decimals.nameElemMap.get(`decimal-table.value-${i}`)!;
    const translation = element
      .getAttribute("transform")!
      .match(/translate\(([^,]+),/)!;
    return Number(translation[1]);
  });
  leftPositions.forEach((x) => expect(x).toBeCloseTo(leftPositions[0], 8));
  const group = await render(
    await buildFourElementGroupTable(),
    "figure-unnumbered-four-element-group-table.svg",
  );
  expect(group.svg.querySelectorAll("line")).toHaveLength(8);
  expect(group.svg.querySelectorAll("rect")).toHaveLength(0);
  expect(group.svg.querySelectorAll("[data-tex]")).toHaveLength(25);
  await render(
    await diagram({
      sub: decimalMapWindowSubstance([
        [1, 0, 1],
        [0, 0, 1],
      ]),
      sty: decimalTableStyle(),
      canvas: canvas(270, 165),
    }),
  );
  const secondGroup = await render(
    await diagram({
      sub: finiteGroupTableSubstance(
        finiteCyclicGroup(3),
        (n) => `\\overline{${n}}`,
        "+",
      ),
      sty: groupTableStyle(),
      canvas: canvas(250, 180),
    }),
  );
  expect(secondGroup.svg.querySelectorAll("line")).toHaveLength(6);
  expect(secondGroup.svg.querySelectorAll("[data-tex]")).toHaveLength(16);
});

test("operation tables reject false group laws and incorrectly asserted inverses before rendering", async () => {
  const cyclic = finiteCyclicGroup(3);
  for (const [model, error] of [
    [
      { ...cyclic, operation: (a: number, b: number) => (a - b + 3) % 3 },
      "associative",
    ],
    [{ ...cyclic, inverse: () => 0 }, "inverses"],
  ] as const) {
    await expect(
      diagram({
        sub: finiteGroupTableSubstance(model, (n) => String(n)),
        sty: groupTableStyle(),
        canvas: canvas(250, 180),
      }),
    ).rejects.toThrow(error);
  }
});

test("native table construction dragging translates every cell and grid line together without changing mathematics", async () => {
  const cases = [
    {
      name: "decimal-table.construction",
      sub: decimalMapWindowSubstance,
      style: decimalTableStyle,
      build: buildCantorDecimalTable,
      width: 270,
      height: 165,
      labels: 12,
      lines: 0,
    },
    {
      name: "group-table.construction",
      sub: fourElementGroupTableSubstance,
      style: groupTableStyle,
      build: buildFourElementGroupTable,
      width: 250,
      height: 180,
      labels: 25,
      lines: 8,
    },
  ] as const;
  const positions = (names: Map<string, SVGElement>, handle: string) =>
    Array.from(names)
      .filter(([name]) => name !== handle)
      .map(([name, element]) => {
        const line =
          element.tagName === "line" ? element : element.querySelector("line");
        if (line)
          return {
            name,
            xy: ["x1", "y1", "x2", "y2"].map((a) =>
              Number(line.getAttribute(a)),
            ),
          };
        const translation = element
          .getAttribute("transform")!
          .match(/translate\(([^,]+),\s*([^)]+)\)/)!;
        return { name, xy: [Number(translation[1]), Number(translation[2])] };
      });
  for (const panel of cases) {
    const canonical = await render(await panel.build());
    const reference = positions(canonical.nameElemMap, panel.name);
    const sub = panel.sub(),
      semantics = JSON.stringify(sub);
    const d = await diagram({
      sub,
      sty: panel.style({ interactive: { jitter: 0 } }),
      canvas: canvas(panel.width, panel.height),
      interactive: { jitter: 0 },
    });
    try {
      while (await d.optimizationStep()) continue;
      const before = await d.render();
      expect(Array.from(d.getDraggingConstraints().keys())).toEqual([
        panel.name,
      ]);
      expect(positions(before.nameElemMap, panel.name)).toEqual(reference);
      expect(before.svg.querySelectorAll("[data-tex]")).toHaveLength(
        panel.labels,
      );
      expect(before.svg.querySelectorAll("line")).toHaveLength(panel.lines);
      const values = Array.from(
        before.svg.querySelectorAll("[data-tex]"),
        (e) => e.getAttribute("data-tex"),
      );
      const handle = before.nameElemMap.get(panel.name)!;
      expect(handle.getAttribute("fill-opacity")).toBe("0");
      expect(handle.getAttribute("pointer-events")).toBe("all");
      d.beginDrag(panel.name);
      d.translate(panel.name, 3, 2);
      d.endDrag(panel.name);
      const after = await d.render();
      const moved = positions(after.nameElemMap, panel.name);
      expect(moved).toHaveLength(panel.labels + panel.lines);
      moved.forEach((shape, index) => {
        expect(shape.name).toBe(reference[index].name);
        shape.xy.forEach((value, axis) =>
          expect(value).toBeCloseTo(
            reference[index].xy[axis] + (axis % 2 === 0 ? 3 : -2),
            8,
          ),
        );
      });
      expect(
        Array.from(after.svg.querySelectorAll("[data-tex]"), (e) =>
          e.getAttribute("data-tex"),
        ),
      ).toEqual(values);
      expect(JSON.stringify(sub)).toBe(semantics);
      expect(Object.isFrozen(sub)).toBe(true);
      sub.entities.forEach((entity) =>
        expect(Object.isFrozen(entity)).toBe(true),
      );
      sub.propositions.forEach((fact) =>
        expect(Object.isFrozen(fact)).toBe(true),
      );
    } finally {
      d.discard();
    }
    const samples: number[] = [];
    for (const seed of ["first", "second"]) {
      const sample = await panel.build({
        interactive: { jitter: 4 },
        variation: seed,
      });
      try {
        while (await sample.optimizationStep()) continue;
        expect(Array.from(sample.getDraggingConstraints().keys())).toEqual([
          panel.name,
        ]);
        const dx = sample.getInput(panel.name + ".layout.x"),
          dy = sample.getInput(panel.name + ".layout.y");
        expect(Math.abs(dx)).toBeLessThanOrEqual(4.00001);
        expect(Math.abs(dy)).toBeLessThanOrEqual(4.00001);
        samples.push(dx);
        const sampled = positions(
          (await sample.render()).nameElemMap,
          panel.name,
        );
        sampled.forEach((shape, index) =>
          shape.xy.forEach((value, axis) =>
            expect(value).toBeCloseTo(
              reference[index].xy[axis] + (axis % 2 === 0 ? dx : -dy),
              6,
            ),
          ),
        );
      } finally {
        sample.discard();
      }
    }
    expect(samples[0]).not.toBeCloseTo(samples[1], 4);
  }
});
