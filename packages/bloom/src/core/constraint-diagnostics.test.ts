import type { PenroseState } from "@penrose/core";
import { describe, expect, test } from "vitest";
import { DiagramBuilder } from "./builder.js";
import * as constraints from "./constraints.js";
import type { Diagram } from "./diagram.js";
import * as objectives from "./objectives.js";
import { canvas } from "./utils.js";

async function finish(drawing: Diagram) {
  for (let i = 0; i < 2000; i++)
    if (!(await drawing.optimizationStep())) return;
  throw new Error("Diagnostic fixture exceeded its optimization budget");
}

describe("constraint diagnostics", () => {
  test("evaluates inactive terms while feasibility respects the current stage", async () => {
    const builder = new DiagramBuilder(canvas(100, 100));
    builder.ensure(2, undefined, "inactive contradiction");
    const drawing = await builder.build();
    try {
      // Bloom currently exposes one stage. Exercise the existing core mask
      // directly to verify truthful diagnostics for future staged assemblies.
      const state = Reflect.get(drawing, "state") as PenroseState;
      state.constraintSets.get("")!.constrMask[0] = false;
      const result = drawing.getConstraintDiagnostics();
      expect(result.feasible).toBe(true);
      expect(result.maxViolation).toBe(0);
      expect(result.constraints[0]).toMatchObject({
        active: false,
        value: 2,
        violation: 2,
      });
      expect(state.constraintSets.get("")!.constrMask[0]).toBe(false);
    } finally {
      drawing.discard();
    }
  });
  test("an explicit zero weight disables a term instead of restoring full strength", async () => {
    const builder = new DiagramBuilder(canvas(100, 100));
    const x = builder.input({ name: "x", init: 2 });
    builder.ensure(2, 0, "disabled contradiction");
    builder.encourage(objectives.equal(x, 8), 0);
    const drawing = await builder.build();
    try {
      await finish(drawing);
      expect(drawing.getConstraintDiagnostics().feasible).toBe(true);
      expect(drawing.getConstraintDiagnostics().constraints[0].value).toBe(0);
      expect(drawing.getInput("x")).toBe(2);
    } finally {
      drawing.discard();
    }
  });

  test("reports named contradictions even after the solver finishes", async () => {
    const builder = new DiagramBuilder(canvas(100, 100));
    const x = builder.input({ name: "x", init: 0 });
    builder.ensure(constraints.lessThan(x, -1), undefined, "x <= -1");
    builder.ensure(constraints.greaterThan(x, 1), undefined, "x >= 1");
    const drawing = await builder.build();
    try {
      await finish(drawing);
      const result = drawing.getConstraintDiagnostics();
      expect(result.optimizationFinished).toBe(true);
      expect(result.feasible).toBe(false);
      expect(result.maxViolation).toBeGreaterThan(0.99);
      expect(result.constraints.map((c) => c.label)).toEqual([
        "x <= -1",
        "x >= 1",
      ]);
      expect(result.constraints.every((c) => c.active)).toBe(true);
    } finally {
      drawing.discard();
    }
  });

  test("inspects live values without moving them and preserves weights", async () => {
    const builder = new DiagramBuilder(canvas(100, 100));
    const x = builder.input({ name: "x", init: 2 });
    builder.ensure(constraints.lessThan(x, 1), 3, "weighted upper bound");
    const drawing = await builder.build();
    try {
      const before = drawing.getInput("x");
      const initial = drawing.getConstraintDiagnostics();
      expect(initial.constraints[0].value).toBe(3);
      expect(initial.constraints[0].violation).toBe(3);
      expect(initial.feasible).toBe(false);
      expect(drawing.getInput("x")).toBe(before);
      await finish(drawing);
      expect(drawing.getConstraintDiagnostics().feasible).toBe(true);
      drawing.setInput("x", 2);
      expect(drawing.getConstraintDiagnostics().feasible).toBe(false);
      expect(drawing.getConstraintDiagnostics().optimizationFinished).toBe(
        false,
      );
    } finally {
      drawing.discard();
    }
  });

  test("identifies implicit canvas constraints by shape name", async () => {
    const builder = new DiagramBuilder(canvas(100, 100));
    builder.circle({ name: "off-page", center: [90, 0], r: 10 });
    const drawing = await builder.build();
    try {
      const diagnostic = drawing.getConstraintDiagnostics();
      expect(diagnostic.feasible).toBe(false);
      expect(diagnostic.constraints).toEqual([
        expect.objectContaining({ label: "onCanvas(off-page)", active: true }),
      ]);
      expect(diagnostic.constraints[0].violation).toBeGreaterThan(0);
      expect(() => drawing.getConstraintDiagnostics(-1)).toThrow(
        "finite and nonnegative",
      );
      expect(() => drawing.getConstraintDiagnostics(NaN)).toThrow(
        "finite and nonnegative",
      );
    } finally {
      drawing.discard();
    }
  });
});
