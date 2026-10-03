import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { coordinateEmbeddingStyle } from "../styles/product-topology.js";
import { subspaceDerivedSetsStyle } from "../styles/subspaces.js";
import {
  buildCoordinateSliceIllustration,
  buildSubspaceDerivedSetsIllustration,
  coordinateSliceSubstance,
  subspaceDerivedSetsSubstance,
} from "./original-subspaces.js";

async function render(drawing: Diagram) {
  try {
    for (let i = 0; i < 3000; i++) {
      if (!(await drawing.optimizationStep())) break;
      if (i === 2999) throw new Error("Original illustration did not converge");
    }
    const { svg } = await drawing.render();
    const xml = new XMLSerializer().serializeToString(svg);
    expect(xml).not.toMatch(/NaN|undefined|Infinity/);
    expect(
      new DOMParser()
        .parseFromString(xml, "image/svg+xml")
        .querySelector("parsererror"),
    ).toBeNull();
    return svg;
  } finally {
    drawing.discard();
  }
}

describe("original illustrations reuse mathematical topology libraries", () => {
  test("distinguishes ambient from relative interior and frontier without drawing data in Substance", () => {
    const sub = subspaceDerivedSetsSubstance();
    const derived = (
      predicate: typeof topology.InteriorOf | typeof topology.FrontierOf,
    ) =>
      sub.propositions
        .filter((p) => p.predicate === predicate)
        .map((p) => p.args.map((e) => ("label" in e ? e.label : "")));
    expect(derived(topology.InteriorOf)).toEqual([
      ["\\phi", "A=Y", "\\tau_D"],
      ["A=Y", "A=Y", "\\tau_Y"],
    ]);
    expect(derived(topology.FrontierOf)).toEqual([
      ["A=Y", "A=Y", "\\tau_D"],
      ["\\phi", "A=Y", "\\tau_Y"],
    ]);
    for (const entity of sub.entities) {
      expect(entity).not.toHaveProperty("shapeType");
      expect(entity).not.toHaveProperty("icon");
      expect(entity).not.toHaveProperty("fillColor");
    }
    const disk = sub.entities.find((e) => e.label === "N") as EntityOf<
      typeof topology.DiskNeighborhood
    >;
    const witness = sub.entities.find((e) => e.label === "q") as EntityOf<
      typeof topology.CoordinatePoint
    >;
    expect(
      Math.hypot(...witness.coordinates.map((v, i) => v - disk.center[i])),
    ).toBeLessThan(disk.radius);
    expect(witness.coordinates[1]).not.toBe(disk.center[1]);
    expect(() => subspaceDerivedSetsSubstance(0, 0)).toThrow();
  });

  test("reuses the subspace comparison style with distinct affine subspaces", async () => {
    const shared = subspaceDerivedSetsStyle();
    for (const [height, radius] of [
      [0, 0.5],
      [0.3, 0.4],
    ]) {
      const svg = await render(
        await diagram({
          sub: subspaceDerivedSetsSubstance(height, radius),
          sty: shared,
          canvas: canvas(640, 360),
        }),
      );
      const disk = svg.querySelector(
        '[aria-label="ambient disk neighborhood"]',
      )!;
      const point = svg.querySelector('[aria-label="subspace.ambient-point"]')!;
      const witness = svg.querySelector(
        '[aria-label="subspace.off-line-witness"]',
      )!;
      expect(Number(disk.getAttribute("r"))).toBeCloseTo(radius * 80, 10);
      expect(Number(disk.getAttribute("cy"))).toBeCloseTo(
        180 - height * 80,
        10,
      );
      expect(point.getAttribute("cy")).toBe(disk.getAttribute("cy"));
      expect(witness.getAttribute("cy")).not.toBe(point.getAttribute("cy"));
      expect(
        svg.querySelectorAll(
          '[aria-label="excluded relative neighborhood endpoint"]',
        ),
      ).toHaveLength(2);
      const labels = Array.from(svg.querySelectorAll("[data-tex]"), (e) =>
        decodeURIComponent(e.getAttribute("data-tex")!),
      );
      expect(labels).toContain("A_X^{\\circ}=\\phi");
      expect(labels).toContain("A_Y^{\\circ}=A");
      expect(labels).toContain("\\operatorname{Fr}_Y A=\\phi");
    }
    const interactive = await render(
      await buildSubspaceDerivedSetsIllustration(0, 0.5, { interactive: true }),
    );
    expect(
      interactive.querySelectorAll("[data-bloom-drag]").length,
    ).toBeGreaterThan(0);
  });

  test("draws nonzero coordinate slices with the existing embedding style", async () => {
    const shared = coordinateEmbeddingStyle({ showAmbientAxes: true });
    for (const [fixed, varying] of [
      [0.75, 2],
      [-0.5, 1],
    ] as const) {
      const sub = coordinateSliceSubstance(fixed, varying);
      expect(
        sub.propositions.filter((p) => p.predicate === topology.Homeomorphism),
      ).toHaveLength(1);
      expect(
        sub.propositions.filter((p) => p.predicate === topology.InverseMaps),
      ).toHaveLength(1);
      const svg = await render(
        await diagram({ sub, sty: shared, canvas: canvas(400, 360) }),
      );
      const point = svg.querySelector('[aria-label="embedding.image-point"]')!;
      expect(
        Number(point.getAttribute(varying === 2 ? "cx" : "cy")),
      ).toBeCloseTo(
        varying === 2 ? 200 - 14 + fixed * 84 : 180 - fixed * 84,
        10,
      );
      expect(svg.querySelectorAll("circle")).toHaveLength(1);
      const slice = svg.querySelector(
        '[aria-label="embedding.highlighted-image-slice"] line',
      )!;
      expect(Number(slice.getAttribute(varying === 2 ? "x1" : "y1"))).toBe(
        Number(point.getAttribute(varying === 2 ? "cx" : "cy")),
      );
      expect(Number(slice.getAttribute(varying === 2 ? "x2" : "y2"))).toBe(
        Number(point.getAttribute(varying === 2 ? "cx" : "cy")),
      );
    }
    const svg = await render(
      await buildCoordinateSliceIllustration(0.75, 2, { interactive: true }),
    );
    expect(svg.querySelectorAll("[data-bloom-drag]").length).toBeGreaterThan(0);
  });
});
