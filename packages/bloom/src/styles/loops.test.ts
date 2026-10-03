import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  associativeLoopParameter,
  concatenatedLoopParameter,
  inverseCancellationParameter,
  trigonometricLoopValue,
  unitLoopParameter,
} from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  associativeLoopSubstance,
  basedLoopFamilySubstance,
  basedLoopSubstance,
  buildAssociativeLoopFigure,
  buildBasedLoopFamilyFigure,
  buildBasedLoopFigure,
  buildConcatenationHomotopyFigure,
  buildInverseLoopFigure,
  buildLeftUnitLoopFigure,
  buildRightUnitLoopFigure,
  concatenationHomotopySubstance,
  inverseLoopSubstance,
  unitLoopSubstance,
} from "../examples/loops.js";
import {
  inverseLoopStyle,
  loopFamilyStyle,
  loopOperationStyle,
  loopStyle,
} from "./loops.js";

async function render(d: Diagram) {
  let converged = false;
  for (let n = 0; n < 3000; n++)
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  expect(converged).toBe(true);
  const result = await d.render(),
    xml = new XMLSerializer().serializeToString(result.svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return result;
}
describe("based-loop operations", () => {
  test("finite Fourier loops and every relative family keep both endpoint basepoints", () => {
    const sub = basedLoopFamilySubstance();
    const slices = sub.propositions.filter(
      (p) => p.predicate === topology.SliceMapAt,
    );
    expect(slices).toHaveLength(3);
    for (const p of slices) {
      const a = p.args[0] as EntityOf<typeof topology.TrigonometricLoop>;
      expect(trigonometricLoopValue(a, 1)).toEqual(
        trigonometricLoopValue(a, 0),
      );
      for (const coordinate of trigonometricLoopValue(a, 0))
        expect(coordinate).toBeCloseTo(0, 12);
    }
    const outer = slices.find(
      (p) =>
        (p.args[2] as EntityOf<typeof topology.RealPoint>).coordinate === 1,
    )!.args[0] as EntityOf<typeof topology.TrigonometricLoop>;
    const inner = slices.find(
      (p) =>
        (p.args[2] as EntityOf<typeof topology.RealPoint>).coordinate === 0,
    )!.args[0] as EntityOf<typeof topology.TrigonometricLoop>;
    for (const p of slices) {
      const r = (p.args[2] as EntityOf<typeof topology.RealPoint>).coordinate,
        a = p.args[0] as EntityOf<typeof topology.TrigonometricLoop>;
      for (const t of [0.1, 0.3, 0.5, 0.9])
        for (const c of [0, 1])
          expect(trigonometricLoopValue(a, t)[c]).toBeCloseTo(
            r * trigonometricLoopValue(outer, t)[c] +
              (1 - r) * trigonometricLoopValue(inner, t)[c],
            12,
          );
    }
    for (const program of [
      basedLoopSubstance(),
      sub,
      concatenationHomotopySubstance(),
      associativeLoopSubstance(),
      unitLoopSubstance("left"),
      unitLoopSubstance("right"),
      inverseLoopSubstance(),
    ]) {
      const based = program.propositions.filter(
        (p) => p.predicate === topology.LoopBasedAt,
      );
      expect(based.length).toBeGreaterThan(0);
      for (const p of based) {
        const ends = program.propositions.find(
          (f) =>
            f.predicate === topology.PathEndpointsOf && f.args[0] === p.args[0],
        )!;
        expect(ends.args[1]).toBe(p.args[1]);
        expect(ends.args[2]).toBe(p.args[1]);
      }
      for (const e of program.entities)
        expect(e).not.toHaveProperty("shapeType");
    }
  });
  test("concatenation and associativity rescale each interval continuously with matching seams", () => {
    expect(concatenatedLoopParameter(0)).toEqual({ loop: 0, time: 0 });
    expect(concatenatedLoopParameter(0.5)).toEqual({ loop: 0, time: 1 });
    expect(concatenatedLoopParameter(1)).toEqual({ loop: 1, time: 1 });
    for (const s of [0, 0.2, 0.5, 1]) {
      const first = (1 + s) / 4,
        second = (2 + s) / 4;
      expect(associativeLoopParameter(0, s)).toEqual({ loop: 0, time: 0 });
      expect(associativeLoopParameter(1, s)).toEqual({ loop: 2, time: 1 });
      expect(associativeLoopParameter(first, s)).toEqual({ loop: 0, time: 1 });
      expect(associativeLoopParameter(second, s).time).toBeCloseTo(1, 12);
      expect(associativeLoopParameter(first + 1e-9, s).loop).toBe(1);
      expect(associativeLoopParameter(first + 1e-9, s).time).toBeCloseTo(0, 7);
      expect(associativeLoopParameter(second + 1e-9, s).loop).toBe(2);
      expect(associativeLoopParameter(second + 1e-9, s).time).toBeCloseTo(0, 7);
    }
    expect(associativeLoopParameter(0.125, 0)).toEqual({ loop: 0, time: 0.5 });
    expect(associativeLoopParameter(0.75, 0)).toEqual({ loop: 2, time: 0.5 });
    expect(associativeLoopParameter(0.25, 1)).toEqual({ loop: 0, time: 0.5 });
    expect(associativeLoopParameter(0.875, 1)).toEqual({ loop: 2, time: 0.5 });
    expect(() => concatenatedLoopParameter(0.3, 0)).toThrow();
    expect(() => associativeLoopParameter(0.3, 1.2)).toThrow();
  });
  test("unit and inverse reparameterizations preserve the endpoints and realize their class relations", () => {
    for (const side of ["left", "right"] as const)
      for (const s of [0, 0.25, 0.5, 1]) {
        expect(unitLoopParameter(0, s, side)).toBe(0);
        expect(unitLoopParameter(1, s, side)).toBe(1);
        for (const r of [0, 0.25, 0.7, 1])
          if (s === 0) expect(unitLoopParameter(r, s, side)).toBe(r);
      }
    expect(unitLoopParameter(0.75, 1, "right")).toBe(1);
    expect(unitLoopParameter(0.25, 1, "left")).toBe(0);
    for (const s of [0, 0.3, 0.8, 1]) {
      expect(inverseCancellationParameter(0, s)).toBe(0);
      expect(inverseCancellationParameter(1, s)).toBe(0);
      expect(inverseCancellationParameter(0.5, s)).toBeCloseTo(1 - s, 12);
    }
    const sub = inverseLoopSubstance(),
      fact = sub.propositions.find(
        (p) => p.predicate === topology.LoopInverseOf,
      )!;
    const a = fact.args[1] as EntityOf<typeof topology.TrigonometricLoop>;
    for (const t of [0, 0.25, 0.5, 0.75, 1]) {
      const p = trigonometricLoopValue(a, t),
        inverse = trigonometricLoopValue(a, 1 - t);
      expect(Math.hypot(p[0], p[1] - 1)).toBeCloseTo(1, 12);
      expect(inverse[0]).toBeCloseTo(-p[0], 12);
      expect(inverse[1]).toBeCloseTo(p[1], 12);
    }
    for (const side of ["left", "right"] as const) {
      const p = unitLoopSubstance(side);
      expect(
        p.propositions.filter((f) => f.predicate === topology.IdentityElement),
      ).toHaveLength(1);
      expect(
        p.propositions.filter(
          (f) => f.predicate === topology.TopologicalEqualSets,
        ),
      ).toHaveLength(1);
    }
  });
  test("shared native operation styles render all cases and actual drag preserves their fixed constructions", async () => {
    const operation = loopOperationStyle(),
      family = loopFamilyStyle();
    for (const [sub, sty] of [
      [basedLoopSubstance(), loopStyle()],
      [basedLoopFamilySubstance(), family],
      [basedLoopFamilySubstance(0.45, 0.7), family],
      [concatenationHomotopySubstance(), operation],
      [associativeLoopSubstance(), operation],
      [unitLoopSubstance("left"), operation],
      [unitLoopSubstance("right"), operation],
      [inverseLoopSubstance(), inverseLoopStyle()],
    ] as const) {
      const d = await diagram({ sub, sty, canvas: canvas(280, 190) });
      try {
        const { svg } = await render(d);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        if (sty === family) {
          const square = svg.querySelector(
            '[aria-label="unit parameter square"]',
          )!;
          expect(square.closest("clipPath")).toBeNull();
          expect(Number(square.getAttribute("stroke-width"))).toBeGreaterThan(
            0,
          );
          expect(
            svg
              .querySelector('[aria-label="loop-family.outer"]')!
              .closest("clipPath"),
          ).toBeNull();
        }
      } finally {
        d.discard();
      }
    }
    for (const [factory, count] of [
      [buildBasedLoopFigure, 2],
      [buildBasedLoopFamilyFigure, 11],
      [buildConcatenationHomotopyFigure, 12],
      [buildAssociativeLoopFigure, 20],
      [buildRightUnitLoopFigure, 8],
      [buildLeftUnitLoopFigure, 8],
      [buildInverseLoopFigure, 4],
    ] as const) {
      const d = await factory({ interactive: { jitter: 0 } });
      try {
        const before = await render(d),
          handles = Array.from(d.getDraggingConstraints().keys());
        expect(handles).toHaveLength(count);
        const fixed = (svg: SVGSVGElement) =>
          Array.from(
            svg.querySelectorAll("path,line,circle,ellipse,polygon,rect"),
            (e) => new XMLSerializer().serializeToString(e),
          );
        const geometry = fixed(before.svg),
          h = handles[0];
        for (const name of handles) {
          expect(d.getInput(name + ".layout.x")).toBeCloseTo(0, 10);
          expect(d.getInput(name + ".layout.y")).toBeCloseTo(0, 10);
        }
        d.beginDrag(h);
        d.translate(h, 3, 1);
        d.endDrag(h);
        const after = await render(d);
        expect(d.getInput(h + ".layout.x")).toBeCloseTo(3, 4);
        expect(fixed(after.svg)).toEqual(geometry);
      } finally {
        d.discard();
      }
    }
  });
});
