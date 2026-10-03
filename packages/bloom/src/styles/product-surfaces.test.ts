import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  buildCompactCubeFigure,
  buildCompactCylinderFigure,
  compactCubeSubstance,
  compactCylinderSubstance,
} from "../examples/product-surfaces.js";
import {
  compactProductSurfaceStyle,
  cylinderSurface,
} from "./product-surfaces.js";

async function optimize(d: Diagram) {
  for (let n = 0; n < 3000; n++) if (!(await d.optimizationStep())) return;
  throw new Error("Product surface did not converge");
}
async function render(d: Diagram) {
  await optimize(d);
  const result = await d.render();
  const xml = new XMLSerializer().serializeToString(result.svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return result;
}

describe("compact interval products", () => {
  test("declares compact factors and exact endpoint fibers without drawing fields", () => {
    for (const sub of [compactCylinderSubstance(), compactCubeSubstance()]) {
      expect(
        sub.propositions.filter((p) => p.predicate === topology.Compact),
      ).toHaveLength(3);
      for (const e of sub.entities) {
        expect(e).not.toHaveProperty("shapeType");
        expect(e).not.toHaveProperty("icon");
        expect(e).not.toHaveProperty("fillColor");
      }
    }
    const cylinder = compactCylinderSubstance();
    expect(
      cylinder.propositions
        .filter((p) => p.predicate === topology.ProductOf)
        .map((p) => p.args.map((e) => ("label" in e ? e.label : ""))),
    ).toEqual([
      ["I\\times C", "I", "C"],
      ["\\{0\\}\\times C", "\\{0\\}", "C"],
      ["\\{1\\}\\times C", "\\{1\\}", "C"],
      ["I\\times\\{x\\}", "I", "\\{x\\}"],
    ]);
    const cube = compactCubeSubstance();
    const products = cube.propositions.filter(
      (p) => p.predicate === topology.ProductOf,
    );
    expect(products[0].args[1]).toBe(products[0].args[2]);
    expect(
      products.map((p) => p.args.map((e) => ("label" in e ? e.label : ""))),
    ).toEqual([
      ["I\\times I", "I", "I"],
      ["I\\times I\\times I", "I\\times I", "I"],
      ["\\{0\\}\\times I", "\\{0\\}", "I"],
      ["\\{0\\}\\times I\\times I", "\\{0\\}\\times I", "I"],
      ["I\\times I\\times\\{1\\}", "I\\times I", "\\{1\\}"],
    ]);
    const sample = cylinderSurface(3, 5)(0.7, 0.2);
    expect(Math.hypot(sample.position[0], sample.position[1])).toBeCloseTo(
      3,
      10,
    );
    expect(sample.position[2]).toBeCloseTo(-1.5, 10);
    expect(Math.hypot(...sample.normal)).toBeCloseTo(1, 10);
    expect(() => compactCubeSubstance(1, 0)).toThrow();
  });

  test("the same mathematical style renders cylinder and cube substances through Penrose", async () => {
    const shared = compactProductSurfaceStyle();
    for (const angle of [Math.PI, 2.6]) {
      const d = await diagram({
        sub: compactCylinderSubstance(angle),
        sty: shared,
        canvas: canvas(134, 180),
      });
      try {
        const { svg } = await render(d);
        expect(svg.querySelectorAll("ellipse")).toHaveLength(2);
        expect(svg.querySelectorAll("polygon")).toHaveLength(48);
        const top = svg.querySelector(
          'ellipse[aria-label="cylinder.top-circle"]',
        )!;
        const point = svg.querySelector(
          'circle[aria-label="cylinder.marked-circle-point"]',
        )!;
        const dx =
          (Number(point.getAttribute("cx")) - Number(top.getAttribute("cx"))) /
          Number(top.getAttribute("rx"));
        const dy =
          (Number(point.getAttribute("cy")) - Number(top.getAttribute("cy"))) /
          Number(top.getAttribute("ry"));
        expect(dx * dx + dy * dy).toBeCloseTo(1, 10);
      } finally {
        d.discard();
      }
    }
    for (const bounds of [
      [0, 1],
      [-1, 2],
    ]) {
      const d = await diagram({
        sub: compactCubeSubstance(...bounds),
        sty: shared,
        canvas: canvas(140, 152),
      });
      try {
        const { svg } = await render(d);
        expect(
          svg.querySelectorAll(
            'polygon[aria-label="cube.first-coordinate-face"], polygon[aria-label="cube.last-coordinate-face"]',
          ),
        ).toHaveLength(2);
        expect(svg.querySelectorAll("clipPath")).toHaveLength(3);
        expect(
          svg.querySelector(
            '[aria-label="cube first-coordinate-face arrow pointing left"]',
          ),
        ).not.toBeNull();
        const wireframe = svg.querySelector(
          'path[aria-label="cube twelve edges"]',
        )!;
        expect(wireframe.getAttribute("d")!.match(/M/g)).toHaveLength(12);
        const labels = Array.from(svg.querySelectorAll("[data-tex]"), (e) =>
          decodeURIComponent(e.getAttribute("data-tex")!),
        );
        expect(labels).toContain("I\\times I\\times\\{" + bounds[1] + "\\}");
        expect(labels).toContain("\\{" + bounds[0] + "\\}\\times I\\times I");
      } finally {
        d.discard();
      }
    }
  });

  test("native annotation dragging keeps boundary geometry fixed and moves its knockout", async () => {
    const cylinder = await buildCompactCylinderFigure({ interactive: true });
    const cube = await buildCompactCubeFigure({ interactive: { jitter: 0 } });
    try {
      const first = await render(cylinder);
      expect(cylinder.getDraggingConstraints().size).toBe(6);
      expect(first.svg.querySelectorAll("ellipse")).toHaveLength(2);
      const before = await render(cube);
      expect(cube.getDraggingConstraints().size).toBe(4);
      const handle = Array.from(cube.getDraggingConstraints().keys()).find(
        (name) =>
          decodeURIComponent(
            before.nameElemMap.get(name)?.getAttribute("data-tex") ?? "",
          ) === "I\\times I\\times\\{1\\}",
      )!;
      expect(handle).toBeTruthy();
      const white = Array.from(before.svg.querySelectorAll("rect")).find(
        (e) => Number(e.getAttribute("y")) < 50,
      )!;
      const whiteBefore = Number(white.getAttribute("x"));
      const wireBefore = before.svg
        .querySelector('path[aria-label="cube twelve edges"]')!
        .getAttribute("d");
      cube.beginDrag(handle);
      cube.translate(handle, 4, 2);
      cube.endDrag(handle);
      const after = await render(cube);
      expect(cube.getInput(handle + ".layout.x")).toBeCloseTo(4, 4);
      expect(
        after.svg
          .querySelector('path[aria-label="cube twelve edges"]')!
          .getAttribute("d"),
      ).toBe(wireBefore);
      const whiteAfter = Array.from(after.svg.querySelectorAll("rect")).find(
        (e) => Number(e.getAttribute("y")) < 50,
      )!;
      expect(Number(whiteAfter.getAttribute("x"))).toBeCloseTo(
        whiteBefore + 4,
        4,
      );
    } finally {
      cylinder.discard();
      cube.discard();
    }
  });
});
