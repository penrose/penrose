import { describe, expect, test, vi } from "vitest";
import { buildDiskDerivedSetsFigure } from "../examples/derived-sets.js";
import { buildAntipodalDiskFigure } from "../examples/identification-sources.js";
import { buildCircleProductFigure } from "../examples/quotient-spaces.js";
import { DiagramBuilder } from "./builder.js";
import type { Diagram } from "./diagram.js";
import { canvas } from "./utils.js";

async function optimize(drawing: Diagram) {
  for (let i = 0; i < 3000; i++) {
    if (!(await drawing.optimizationStep())) return;
  }
  throw new Error("Interactive layout did not converge");
}

describe("native interactive figure layouts", () => {
  test("an unlabeled disk can move while preserving its marked center and hatching", async () => {
    const drawing = await buildAntipodalDiskFigure({
      interactive: { jitter: 0 },
    });
    try {
      await optimize(drawing);
      const before = await drawing.render();
      const hatchBefore = before.nameElemMap.get(
        "identification.disk.hatching",
      )!.outerHTML;
      drawing.beginDrag("identification.disk");
      drawing.translate("identification.disk", 4, 2);
      drawing.endDrag("identification.disk");
      await optimize(drawing);
      const { nameElemMap } = await drawing.render();
      const disk = nameElemMap.get("identification.disk")!;
      const point = nameElemMap.get("identification.disk-point")!;
      expect(disk.getAttribute("r")).toBe("76");
      expect(disk.getAttribute("cx")).toBe(point.getAttribute("cx"));
      expect(disk.getAttribute("cy")).toBe(point.getAttribute("cy"));
      expect(Number(disk.getAttribute("cx"))).toBeCloseTo(87, 4);
      expect(
        nameElemMap.get("identification.disk.hatching")!.outerHTML,
      ).not.toBe(hatchBefore);
      expect(disk.getAttribute("data-bloom-drag")).toBe("true");
    } finally {
      drawing.discard();
    }
  });
  test("a dense native surface compiles and drags without unused shape inputs inflating the energy", async () => {
    const drawing = await buildCircleProductFigure({
      interactive: { jitter: 0 },
    });
    try {
      await optimize(drawing);
      const before = await drawing.render();
      expect(before.svg.querySelectorAll("polygon")).toHaveLength(1536);
      const handle = drawing.getDraggingConstraints().keys().next().value!;
      expect(handle).toBeTruthy();
      drawing.beginDrag(handle);
      drawing.translate(handle, 3, 2);
      drawing.endDrag(handle);
      await optimize(drawing);
      expect(drawing.getInput(`${handle}.layout.x`)).toBeCloseTo(3, 4);
      const after = await drawing.render();
      expect(after.svg.querySelectorAll("polygon")).toHaveLength(1536);
      expect(after.svg.outerHTML).not.toMatch(/NaN|Infinity/);
    } finally {
      drawing.discard();
    }
  });
  test("retains canonical geometry and deterministically samples annotation layouts", async () => {
    const plain = await buildDiskDerivedSetsFigure();
    const zero = await buildDiskDerivedSetsFigure({
      interactive: { jitter: 0 },
    });
    const one = await buildDiskDerivedSetsFigure({
      variation: "label-seed-one",
      interactive: true,
    });
    const repeat = await buildDiskDerivedSetsFigure({
      variation: "label-seed-one",
      interactive: true,
    });
    const two = await buildDiskDerivedSetsFigure({
      variation: "label-seed-two",
      interactive: true,
    });
    try {
      for (const d of [plain, zero, one, repeat, two]) await optimize(d);
      expect(plain.getDraggingConstraints().size).toBe(0);
      expect(one.getDraggingConstraints().size).toBeGreaterThan(0);
      const canonical = await plain.render();
      const untouched = await zero.render();
      for (const name of zero.getDraggingConstraints().keys()) {
        expect(zero.getInput(`${name}.layout.x`)).toBe(0);
        expect(zero.getInput(`${name}.layout.y`)).toBe(0);
        expect(untouched.nameElemMap.get(name)!.getAttribute("transform")).toBe(
          canonical.nameElemMap.get(name)!.getAttribute("transform"),
        );
      }
      const name = one.getDraggingConstraints().keys().next().value!;
      expect(one.getInput(`${name}.layout.x`)).toBeCloseTo(
        repeat.getInput(`${name}.layout.x`),
        6,
      );
      expect(one.getInput(`${name}.layout.x`)).not.toBeCloseTo(
        two.getInput(`${name}.layout.x`),
        3,
      );
      for (const drawing of [plain, zero, one, repeat, two]) {
        const { svg } = await drawing.render();
        const disk = svg.querySelector(
          'circle[aria-label="derived-disk.interior"]',
        )!;
        const point = svg.querySelector(
          'circle[aria-label="derived-disk.boundary-point"]',
        )!;
        expect([
          disk.getAttribute("cx"),
          disk.getAttribute("cy"),
          disk.getAttribute("r"),
        ]).toEqual(["161", "175", "100"]);
        expect([point.getAttribute("cx"), point.getAttribute("cy")]).toEqual([
          "261",
          "175",
        ]);
      }
      const before = one.getInput(`${name}.layout.x`);
      one.beginDrag(name);
      one.translate(name, 5, 3);
      one.endDrag(name);
      await optimize(one);
      expect(one.getInput(`${name}.layout.x`)).toBeGreaterThan(before + 4);
      const { svg } = await one.render();
      expect(svg.querySelectorAll('[data-bloom-drag="true"]')).toHaveLength(
        one.getDraggingConstraints().size,
      );
    } finally {
      for (const d of [plain, zero, one, repeat, two]) d.discard();
    }
  });

  test("a native drag group moves its geometric companions without changing relative coordinates", async () => {
    const builder = new DiagramBuilder(canvas(200, 200), "group-seed", 1000);
    const handle = builder.circle({ name: "point", center: [0, 0], r: 4 });
    const companion = builder.circle({ name: "disk", center: [20, 0], r: 12 });
    builder.draggableGroup(handle, [companion]);
    const drawing = await builder.build();
    try {
      await optimize(drawing);
      drawing.beginDrag("point");
      drawing.translate("point", 8, 4);
      drawing.endDrag("point");
      await optimize(drawing);
      const { nameElemMap } = await drawing.render();
      const p = nameElemMap.get("point")!,
        disk = nameElemMap.get("disk")!;
      expect(
        Number(disk.getAttribute("cx")) - Number(p.getAttribute("cx")),
      ).toBeCloseTo(20, 10);
      expect(
        Number(disk.getAttribute("cy")) - Number(p.getAttribute("cy")),
      ).toBeCloseTo(0, 10);
      expect(disk.getAttribute("r")).toBe("12");
    } finally {
      drawing.discard();
    }
  });

  test("the interactive SVG retains keyboard handles across optimization frame updates", async () => {
    vi.stubGlobal("requestAnimationFrame", (callback: () => void) =>
      setTimeout(callback, 0),
    );
    const builder = new DiagramBuilder(canvas(180, 120), "keyboard-seed", 1000);
    const label = builder.equation({
      name: "label",
      center: [0, 0],
      string: "A",
    });
    builder.draggableGroup(label);
    const drawing = await builder.build();
    const host = drawing.getInteractiveElement();
    const eventually = async (check: () => boolean) => {
      for (let i = 0; i < 100; i++) {
        if (check()) return;
        await new Promise((resolve) => setTimeout(resolve, 10));
      }
      throw new Error("Interactive SVG frame did not update");
    };
    try {
      document.body.appendChild(host);
      await eventually(() => !!host.querySelector('[data-bloom-drag="true"]'));
      const handle = host.querySelector('[data-bloom-drag="true"]')!;
      const before = handle.getAttribute("transform");
      const startingX = drawing.getInput("label.layout.x");
      handle.dispatchEvent(
        new KeyboardEvent("keydown", { key: "ArrowRight", bubbles: true }),
      );
      await eventually(() => handle.getAttribute("transform") !== before);
      expect(drawing.getInput("label.layout.x")).toBeGreaterThan(startingX + 1);
      expect(host.querySelector('[data-bloom-drag="true"]')).toBe(handle);
      expect(handle.getAttribute("tabindex")).toBe("0");
      expect(handle.getAttribute("aria-label")).toBe("Drag A");
      expect(handle.children[0].localName).toBe("title");
    } finally {
      drawing.discard();
      host.remove();
      vi.unstubAllGlobals();
    }
  });
});
