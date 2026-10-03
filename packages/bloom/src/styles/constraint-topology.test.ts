import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { expect, test } from "vitest";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { measureConstraintTopology } from "../examples/constraint-topology-experiment.js";
import {
  constraintTopologySubstance,
  type ConstraintTopologyCase,
} from "../examples/constraint-topology.js";
import { constraintTopologyStyles } from "./constraint-topology.js";

function save(
  name: string,
  result: Awaited<ReturnType<typeof measureConstraintTopology>>,
) {
  const destination = process.env.PENROSE_CONSTRAINT_REVIEW_DIR;
  if (!destination) return;
  mkdirSync(destination, { recursive: true });
  writeFileSync(join(destination, `${name}.svg`), result.svg);
  writeFileSync(
    join(destination, `${name}.json`),
    JSON.stringify(result.metrics, null, 2),
  );
}

for (const example of [
  "book-separation",
  "book-nested",
  "neighborhood-triangle",
  "bipartite-map",
  "L-membership",
  "unhinted-composition",
] satisfies ConstraintTopologyCase[]) {
  test(`${example}: native geometry independently satisfies the mathematical facts`, async () => {
    const result = await measureConstraintTopology(example);
    save(example, result);
    console.log(
      JSON.stringify({
        example,
        calls: result.metrics.calls,
        time: result.metrics.optimizationMs,
        residual: result.metrics.maxViolation,
        semantic: result.metrics.semanticMaxViolation,
        status: result.metrics.status,
      }),
    );
    expect(result.metrics.optimizationFinished).toBe(true);
    expect(result.metrics.semanticMaxViolation).toBeLessThan(0.01);
    expect(result.metrics.maxViolation).toBeLessThan(0.01);
    expect(result.metrics.optimizedExplicitInputCount).toBe(
      result.metrics.geometryInputCount + result.metrics.labelInputCount,
    );
    expect(result.svg).not.toMatch(/NaN|Infinity|undefined/);
    const svg = new DOMParser().parseFromString(result.svg, "image/svg+xml");
    expect(svg.querySelector("parsererror")).toBeNull();
    expect(svg.querySelectorAll("image")).toHaveLength(0);
    expect(
      result.metrics.entities.every(
        (entity) => entity.fields.join() === "label",
      ),
    ).toBe(true);
    if (example === "neighborhood-triangle")
      expect(svg.querySelectorAll("[aria-label^='R:']")).toHaveLength(3);
    if (example === "bipartite-map")
      expect(svg.querySelectorAll("[aria-label^='f:']")).toHaveLength(4);
    if (example === "L-membership")
      expect(svg.querySelectorAll("polygon[aria-label='Set L']")).toHaveLength(
        1,
      );
  }, 120000);
}

test("contradictory subset and disjoint facts remain diagnosably infeasible", async () => {
  const result = await measureConstraintTopology("inconsistent");
  save("inconsistent", result);
  expect(result.metrics.feasible).toBe(false);
  expect(result.metrics.semanticMaxViolation).toBeGreaterThan(10);
  expect(
    result.metrics.residuals.filter(
      (c) => c.label?.startsWith("semantic:") && c.violation > 1,
    ).length,
  ).toBeGreaterThan(0);
}, 120000);

test("Substance is immutable and reusable; view composition has an intentional order", async () => {
  const sub = constraintTopologySubstance("neighborhood-triangle");
  const facts = [...sub.propositions];
  const styles = constraintTopologyStyles();
  await expect(
    diagram({ sub, sty: styles.relations, canvas: canvas(720, 540) }),
  ).rejects.toThrow("Compose the region style");
  expect(sub.propositions).toEqual(facts);
  expect(Object.isFrozen(sub)).toBe(true);
  sub.entities.forEach((entity) => expect(Object.isFrozen(entity)).toBe(true));
  expect(() => constraintTopologyStyles({ positionPrior: -1 })).toThrow(
    "positionPrior",
  );
});

for (const example of [
  "neighborhood-triangle",
  "bipartite-map",
  "L-membership",
] satisfies ConstraintTopologyCase[]) {
  test(`${example}: semantic constraints repair substantial perturbations with absolute geometry priors disabled`, async () => {
    const result = await measureConstraintTopology(example, {
      seed: "book",
      perturbation: 90,
      positionPrior: 0,
      sizePrior: 0,
    });
    save(`${example}-geometry-priors-off`, result);
    expect(result.metrics.initialMaxViolation).toBeGreaterThan(10);
    expect(result.metrics.geometryTravel.max).toBeGreaterThan(30);
    expect(result.metrics.optimizationFinished).toBe(true);
    expect(result.metrics.feasible).toBe(true);
    expect(result.metrics.semanticMaxViolation).toBeLessThan(0.001);
    expect(result.metrics.maxLabelAssociationDistance).toBeLessThan(24.01);
    expect(result.metrics.edgeIncidenceMaxDeviation).toBeLessThan(1e-8);
    expect(result.metrics.activeObjectiveCount).toBe(
      result.metrics.labelInputCount,
    );
  }, 120000);
}

test("the measurement distinguishes an unfinished optimizer from a converged feasible result", async () => {
  const result = await measureConstraintTopology(
    "neighborhood-triangle",
    { perturbation: 90 },
    1,
  );
  expect(result.metrics.optimizationFinished).toBe(false);
  expect(result.metrics.calls).toBe(1);
  expect(result.metrics.stopped).toBe(false);
});

test("the same region/graph style bundle assembles fresh native views concurrently", async () => {
  const sub = constraintTopologySubstance("book-separation"),
    styles = constraintTopologyStyles();
  const drawings = await Promise.all(
    [0, 1].map(() =>
      diagram({
        sub,
        sty: [styles.regions, styles.relations],
        canvas: canvas(720, 540),
        variation: "reuse",
      }),
    ),
  );
  try {
    const renders = await Promise.all(
      drawings.map(async (drawing) => {
        for (let i = 0; i < 300; i++)
          if (!(await drawing.optimizationStep())) break;
        expect(drawing.getConstraintDiagnostics().feasible).toBe(true);
        const { svg } = await drawing.render();
        return Array.from(svg.querySelectorAll("circle[aria-label]")).map(
          (circle) => [
            circle.getAttribute("aria-label"),
            circle.getAttribute("cx"),
            circle.getAttribute("cy"),
            circle.getAttribute("r"),
          ],
        );
      }),
    );
    expect(renders[0]).toHaveLength(4);
    expect(renders[0]).toEqual(renders[1]);
    expect(
      sub.entities.every((entity) => Object.keys(entity).join() === "label"),
    ).toBe(true);
  } finally {
    drawings.forEach((drawing) => drawing.discard());
  }
});
