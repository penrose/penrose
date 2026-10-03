import { expect, test } from "vitest";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { affineRealContraction } from "../domains/topological-vocabulary.js";
import { affineContractionIteratesSubstance } from "../examples/contraction-iterates.js";
import { contractionIterationStyle } from "./contraction-iterates.js";

test("the contraction illustration has true consecutive iterates and an algebraic all-n error bound", () => {
  for (const [a, b, y] of [
    [0.5, 1, -2],
    [-0.6, 0.8, 3],
    [0, 2, -1],
  ] as const) {
    const f = affineRealContraction(a, b),
      sub = affineContractionIteratesSubstance(a, b, y, 8);
    const samples = sub.entities.filter((e) => "index" in e) as EntityOf<
      typeof topology.RealIterationSample
    >[];
    expect(samples).toHaveLength(9);
    for (let n = 1; n < samples.length; n++) {
      expect(samples[n].coordinate).toBeCloseTo(
        f.apply(samples[n - 1].coordinate),
        12,
      );
      expect(Math.abs(samples[n].coordinate - f.fixedPoint)).toBeCloseTo(
        f.errorBound(y, n),
        12,
      );
    }
    // The closed formula proves convergence and the Cauchy bound for arbitrarily large n,m.
    for (const n of [0, 1, 8, 40])
      for (const m of [n, n + 1, n + 17]) {
        const sn = f.fixedPoint + a ** n * (y - f.fixedPoint),
          sm = f.fixedPoint + a ** m * (y - f.fixedPoint);
        expect(Math.abs(sn - sm)).toBeLessThanOrEqual(
          f.errorBound(y, n) + f.errorBound(y, m) + 1e-14,
        );
      }
    expect(
      sub.propositions.filter(
        (p) => p.predicate === topology.CompleteMetricSpace,
      ),
    ).toHaveLength(1);
    for (const e of sub.entities) expect(e).not.toHaveProperty("shapeType");
  }
});

test("one native iteration style supports monotone, alternating and constant maps with draggable annotations", async () => {
  const sty = contractionIterationStyle();
  for (const [a, b, y] of [
    [0.5, 1, -2],
    [-0.6, 0.8, 3],
    [0, 2, -1],
  ] as const) {
    const d = await diagram({
      sub: affineContractionIteratesSubstance(a, b, y, 8),
      sty,
      canvas: canvas(700, 330),
      interactive: { jitter: 0 },
    });
    try {
      let done = false;
      for (let i = 0; i < 3000; i++)
        if (!(await d.optimizationStep())) {
          done = true;
          break;
        }
      expect(done).toBe(true);
      const { svg } = await d.render();
      const xml = new XMLSerializer().serializeToString(svg);
      expect(xml).not.toMatch(/NaN|Infinity|undefined/);
      expect(
        new DOMParser()
          .parseFromString(xml, "image/svg+xml")
          .querySelector("parsererror"),
      ).toBeNull();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      const handles = [...d.getDraggingConstraints().keys()];
      expect(handles.length).toBeGreaterThan(5);
      const h = handles[0];
      d.beginDrag(h);
      d.translate(h, 3, 2);
      d.endDrag(h);
      expect(d.getInput(h + ".layout.x")).toBeCloseTo(3, 8);
      expect(d.getInput(h + ".layout.y")).toBeCloseTo(2, 8);
    } finally {
      d.discard();
    }
  }
});
