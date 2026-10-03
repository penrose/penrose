import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  inverseCancellationParameter,
  trigonometricLoopValue,
} from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  buildInverseCancellationFigure,
  inverseCancellationSubstance,
} from "../examples/loop-retracing.js";
import { inverseCancellationStyle } from "./loop-retracing.js";
async function render(d: Diagram) {
  let converged = false;
  for (let n = 0; n < 3000; n++)
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  expect(converged).toBe(true);
  const out = await d.render(),
    xml = new XMLSerializer().serializeToString(out.svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return out;
}
describe("original loop retracing illustration", () => {
  test("a followed by its inverse contracts continuously while fixing the basepoint", () => {
    for (const kind of ["circle", "figure-eight"] as const) {
      const sub = inverseCancellationSubstance(kind),
        inverse = sub.propositions.find(
          (p) => p.predicate === topology.LoopInverseOf,
        )!;
      const a = inverse.args[1] as EntityOf<typeof topology.TrigonometricLoop>;
      const H = sub.propositions.find(
        (p) => p.predicate === topology.HomotopyBetween,
      )!;
      const constant = sub.propositions.find(
          (p) => p.predicate === topology.ConstantLoopAt,
        )!,
        y0 = constant.args[1] as EntityOf<typeof topology.CoordinatePoint>;
      expect(H.args[1]).toBe(constant.args[0]);
      expect(
        sub.propositions.some(
          (p) =>
            p.predicate === topology.LoopProductOf &&
            p.args[0] === H.args[2] &&
            p.args[1] === a &&
            p.args[2] === inverse.args[0],
        ),
      ).toBe(true);
      for (const s of [0, 0.13, 1 / 3, 2 / 3, 0.9, 1]) {
        expect(
          trigonometricLoopValue(a, inverseCancellationParameter(0, s)),
        ).toEqual(y0.coordinates);
        expect(
          trigonometricLoopValue(a, inverseCancellationParameter(1, s)),
        ).toEqual(y0.coordinates);
        for (const r of [0, 0.2, 0.5, 0.7, 1]) {
          expect(inverseCancellationParameter(r, s)).toBeCloseTo(
            inverseCancellationParameter(1 - r, s),
            12,
          );
          if (s === 1)
            expect(
              trigonometricLoopValue(a, inverseCancellationParameter(r, s)),
            ).toEqual(y0.coordinates);
        }
        const left = trigonometricLoopValue(
            a,
            inverseCancellationParameter(0.5 - 1e-8, s),
          ),
          right = trigonometricLoopValue(
            a,
            inverseCancellationParameter(0.5 + 1e-8, s),
          );
        for (const c of [0, 1]) expect(left[c]).toBeCloseTo(right[c], 12);
      }
      expect(
        sub.propositions.filter((p) => p.predicate === topology.SliceMapAt),
      ).toHaveLength(4);
      expect(
        sub.propositions.filter(
          (p) => p.predicate === topology.RelativeHomotopyOn,
        ),
      ).toHaveLength(1);
      expect(
        sub.propositions.filter(
          (p) => p.predicate === topology.TopologicalEqualSets,
        ),
      ).toHaveLength(1);
      for (const e of sub.entities) expect(e).not.toHaveProperty("shapeType");
    }
  });
  test("the same native style draws two different loop programs and every sampled retracing path returns to its start", async () => {
    const style = inverseCancellationStyle();
    for (const kind of ["circle", "figure-eight"] as const) {
      const d = await diagram({
        sub: inverseCancellationSubstance(kind),
        sty: style,
        canvas: canvas(512, 164),
      });
      try {
        const { svg } = await render(d);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        expect(
          svg.querySelectorAll('path[aria-label^="retracing.reference-"]'),
        ).toHaveLength(4);
        const slices = svg.querySelectorAll(
          'path[aria-label^="retracing.slice-"]',
        );
        expect(slices).toHaveLength(3);
        for (const path of Array.from(slices)) {
          const numbers = path
            .getAttribute("d")!
            .match(/[-+]?(?:\d*\.\d+|\d+)(?:[eE][-+]?\d+)?/g)!
            .map(Number);
          expect(numbers).toHaveLength(386);
          expect(numbers[0]).toBeCloseTo(numbers[numbers.length - 2], 10);
          expect(numbers[1]).toBeCloseTo(numbers[numbers.length - 1], 10);
        }
        expect(
          svg.querySelectorAll('[aria-label="outward traversal"]'),
        ).toHaveLength(3);
        expect(
          svg.querySelectorAll('[aria-label="return traversal"]'),
        ).toHaveLength(3);
        expect(
          svg.querySelectorAll('[aria-label^="fixed retracing basepoint "]'),
        ).toHaveLength(4);
      } finally {
        d.discard();
      }
    }
  });
  test("native label drag and seeded layout leave the retracing construction fixed", async () => {
    for (const kind of ["circle", "figure-eight"] as const) {
      const d = await buildInverseCancellationFigure(kind, {
        interactive: { jitter: 0 },
      });
      try {
        const before = await render(d),
          handles = Array.from(d.getDraggingConstraints().keys());
        expect(handles).toHaveLength(10);
        const fixed = (svg: SVGSVGElement) =>
          Array.from(svg.querySelectorAll("path,circle,polygon"), (e) =>
            new XMLSerializer().serializeToString(e),
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
        expect(fixed((await render(d)).svg)).toEqual(geometry);
        expect(d.getInput(h + ".layout.x")).toBeCloseTo(3, 4);
      } finally {
        d.discard();
      }
      const sampled = await buildInverseCancellationFigure(kind, {
        interactive: true,
        variation: "retracing-test",
      });
      try {
        await render(sampled);
        expect(sampled.getDraggingConstraints().size).toBe(10);
      } finally {
        sampled.discard();
      }
    }
  });
});
