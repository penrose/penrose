import { expect, test } from "vitest";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  antipodalDiskEquivalent,
  inClosedRectangle,
  inIntegerDifferenceLocus,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  antipodalDiskSubstance,
  integerDifferencePlaneSubstance,
  oppositeEdgesCollapseSubstance,
  polygonBoundaryCollapseSubstance,
} from "../examples/identification-sources.js";
import { identificationSourceStyle } from "./identification-sources.js";

test("the two entire opposite edges belong to one collapsed class", () => {
  const sub = oppositeEdgesCollapseSubstance();
  const relation = sub.entities.find(
    (entity) => entity.label === "R",
  ) as EntityOf<typeof topology.SubsetCollapseRelation>;
  expect(sub.entities).toContain(relation.collapsed);
  expect(relation.collapsed.label).toBe("\\overline{AB}\\cup\\overline{CD}");
  expect(Object.isFrozen(relation.collapsed)).toBe(true);
  const classFacts = sub.propositions.filter(
    (fact) => fact.predicate === topology.ClassOf,
  );
  expect(classFacts).toHaveLength(4);
  expect(new Set(classFacts.map((fact) => fact.args[0])).size).toBe(1);
  expect(inClosedRectangle([0, 0, 1, 1], [0, 1])).toBe(true);
  expect(inClosedRectangle([0, 0, 1, 1], [-0.1, 0.5])).toBe(false);
});

test("antipodal identification affects the boundary and leaves interior singletons", () => {
  const disk = { center: [0, 0] as const, radius: 1 };
  expect(antipodalDiskEquivalent(disk, [1, 0], [-1, 0])).toBe(true);
  expect(antipodalDiskEquivalent(disk, [0.5, 0], [-0.5, 0])).toBe(false);
  expect(antipodalDiskEquivalent(disk, [0.5, 0], [0.5, 0])).toBe(true);
  expect(antipodalDiskEquivalent(disk, [2, 0], [2, 0])).toBe(false);
  expect(
    antipodalDiskEquivalent({ center: [2, 3], radius: 2 }, [4, 3], [0, 3]),
  ).toBe(true);
});

test("Figure 4.6's literal locus does not assert a repaired quotient partition", () => {
  expect(inIntegerDifferenceLocus(1, [-2, 3])).toBe(true);
  expect(inIntegerDifferenceLocus(1, [0.2, 0])).toBe(false);
  expect(inIntegerDifferenceLocus(2, [6, 2])).toBe(true);
  const sub = integerDifferencePlaneSubstance();
  expect(
    sub.propositions.some((fact) => fact.predicate === topology.QuotientOf),
  ).toBe(false);
});

test("one source-region style renders all four constructions as valid SVG", async () => {
  const style = identificationSourceStyle();
  for (const [sub, name] of [
    [oppositeEdgesCollapseSubstance(), "identification.rectangle"],
    [antipodalDiskSubstance(2), "identification.disk"],
    [integerDifferencePlaneSubstance(), "identification.plane-viewport"],
    [polygonBoundaryCollapseSubstance(), "identification.polygon"],
  ] as const) {
    const drawing = await diagram({
      sub,
      sty: style,
      canvas: canvas(275, 210),
    });
    try {
      const { svg } = await drawing.render();
      const xml = new XMLSerializer().serializeToString(svg);
      expect(
        new DOMParser()
          .parseFromString(xml, "image/svg+xml")
          .querySelector("parsererror"),
      ).toBeNull();
      expect(svg.querySelector(`[aria-label="${name}"]`)).not.toBeNull();
      expect(xml).not.toMatch(/NaN|undefined/);
    } finally {
      drawing.discard();
    }
  }
});
