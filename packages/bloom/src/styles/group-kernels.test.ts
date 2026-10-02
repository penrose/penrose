import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { expect, test } from "vitest";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  setTheory as algebra,
  finiteCyclicGroup,
  isFiniteSubgroup,
} from "../domains/set-theory.js";
import {
  buildCyclicGroupKernelFigure,
  cyclicGroupKernelSubstance,
} from "../examples/group-kernels.js";
import { groupKernelStyle } from "./group-kernels.js";

test("the complete finite map preserves every product and has exactly its stated subgroup kernel", () => {
  for (const [n, m] of [
    [6, 3],
    [8, 4],
  ] as const) {
    const sub = cyclicGroupKernelSubstance(n, m);
    const facts = sub.propositions;
    const [f, G, H] = facts.find(
      (p) => p.predicate === algebra.GroupHomomorphismBetween,
    )!.args;
    const [K] = facts.find((p) => p.predicate === algebra.KernelOf)!.args;
    const belongs = (p: unknown, group: unknown) =>
      facts.some(
        (q) =>
          q.predicate === algebra.Member &&
          q.args[0] === p &&
          q.args[1] === group,
      );
    const points = sub.entities.filter(
      (p) => "residue" in p && belongs(p, G),
    ) as EntityOf<typeof algebra.CyclicGroupElement>[];
    const maps = facts.filter((p) => p.predicate === algebra.MapsTo);
    const values = new Map(maps.map((p) => [p.args[1], p.args[2]] as const));
    const residue = (p: unknown) =>
      (p as EntityOf<typeof algebra.CyclicGroupElement>).residue;
    expect(maps).toHaveLength(n);
    for (const a of points)
      for (const b of points) {
        const sum = points.find(
          (p) => p.residue === (a.residue + b.residue) % n,
        )!;
        expect(residue(values.get(sum))).toBe(
          (residue(values.get(a)) + residue(values.get(b))) % m,
        );
      }
    const kernel = points.filter((p) => belongs(p, K));
    expect(kernel.map((p) => p.residue)).toEqual([0, m]);
    for (const p of points)
      expect(belongs(p, K)).toBe(residue(values.get(p)) === 0);
    expect(
      isFiniteSubgroup(
        finiteCyclicGroup(n),
        kernel.map((p) => p.residue),
      ),
    ).toBe(true);
    expect(
      facts.filter((p) => p.predicate === algebra.ProductValue),
    ).toHaveLength(n * n + m * m + 4);
    expect(
      facts.some(
        (p) =>
          p.predicate === algebra.SubgroupOf &&
          p.args[0] === K &&
          p.args[1] === G,
      ),
    ).toBe(true);
    expect(
      facts.some(
        (p) =>
          p.predicate === algebra.GroupHomomorphismBetween &&
          p.args[0] === f &&
          p.args[2] === H,
      ),
    ).toBe(true);
    for (const entity of sub.entities)
      expect(entity).not.toHaveProperty("shapeType");
  }
  expect(() => cyclicGroupKernelSubstance(6, 4)).toThrow("divides");
});

test("one native style renders both programs and dragged nodes keep map arrows and kernel shading attached", async () => {
  const sty = groupKernelStyle({ interactive: { jitter: 0 } });
  for (const [n, m] of [
    [6, 3],
    [8, 4],
  ] as const) {
    const d = await diagram({
      sub: cyclicGroupKernelSubstance(n, m),
      sty,
      canvas: canvas(520, n * 27 + 145),
      interactive: false,
    });
    try {
      let finished = false;
      for (let step = 0; step < 1000; step++)
        if (!(await d.optimizationStep())) {
          finished = true;
          break;
        }
      expect(finished).toBe(true);
      const { svg, nameElemMap } = await d.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(n + m);
      expect(svg.querySelectorAll("line")).toHaveLength(n);
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      const xml = new XMLSerializer().serializeToString(svg);
      expect(xml).not.toMatch(/NaN|Infinity|undefined/);
      expect(
        new DOMParser()
          .parseFromString(xml, "image/svg+xml")
          .querySelector("parsererror"),
      ).toBeNull();
      const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
      if (destination) {
        mkdirSync(destination, { recursive: true });
        writeFileSync(
          join(destination, `original-group-kernel-${n}-${m}.svg`),
          xml,
        );
      }
      const name = "kernel.source-0";
      expect(d.getDraggingConstraints().size).toBe(n + m);
      const lineIn = (element: SVGElement) =>
        element.tagName === "line" ? element : element.querySelector("line")!;
      const before = lineIn(nameElemMap.get("kernel.map-0")!);
      const beforeStart = ["x1", "y1"].map((a) =>
        Number(before.getAttribute(a)),
      );
      const band = nameElemMap.get("kernel.highlight")!;
      const bandTop = Number(band.getAttribute("y"));
      const bandHeight = Number(band.getAttribute("height"));
      d.beginDrag(name);
      d.translate(name, 3, 2);
      d.endDrag(name);
      const { svg: afterSvg, nameElemMap: afterNames } = await d.render();
      const after = lineIn(afterNames.get("kernel.map-0")!);
      expect(Number(after.getAttribute("x1"))).toBeCloseTo(
        beforeStart[0] + 3,
        8,
      );
      expect(Number(after.getAttribute("y1"))).toBeCloseTo(
        beforeStart[1] - 2,
        8,
      );
      expect(
        Number(afterNames.get("kernel.highlight")!.getAttribute("y")),
      ).toBeCloseTo(bandTop - 2, 8);
      expect(
        Number(afterNames.get("kernel.highlight")!.getAttribute("height")),
      ).toBeCloseTo(bandHeight + 2, 8);
      expect(afterSvg.outerHTML).not.toMatch(/NaN|Infinity/);
    } finally {
      d.discard();
    }
  }
  const withBuilderOptions = await buildCyclicGroupKernelFigure(6, 3, {
    interactive: true,
  });
  // The builder also makes the five independent heading/formula annotations draggable.
  expect(withBuilderOptions.getDraggingConstraints().size).toBe(14);
  withBuilderOptions.discard();
});
