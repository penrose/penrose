import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  diskDerivedMembership,
  inOpenDisk,
  inRealInterval,
  intervalDerivedMembership,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildDiskDerivedSetsFigure,
  buildIntervalDerivedSetsFigure,
  intervalDerivedSubstance,
  openDiskDerivedSubstance,
} from "../examples/derived-sets.js";
import {
  diskDerivedSetsStyle,
  intervalDerivedSetsStyle,
} from "./derived-sets.js";

async function render(drawing: Diagram, id?: string) {
  try {
    for (let i = 0; i < 2000; i++) {
      if (!(await drawing.optimizationStep())) break;
      if (i === 1999)
        throw new Error("Derived-set drawing did not finish optimizing");
    }
    const { svg } = await drawing.render();
    expect(svg.outerHTML).not.toMatch(/NaN|undefined|Infinity/);
    const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
    if (destination && id) {
      mkdirSync(destination, { recursive: true });
      writeFileSync(
        join(destination, `figure-${id}.svg`),
        new XMLSerializer().serializeToString(svg),
      );
    }
    return svg;
  } finally {
    drawing.discard();
  }
}

describe("Chapter 3 topologically derived sets", () => {
  test("preserves the strict disk and half-open interval, including all five derived sets", () => {
    const disk = { center: [0, 0] as const, radius: 1 };
    expect(inOpenDisk(disk, [1, 0])).toBe(false);
    expect(diskDerivedMembership(disk, [1, 0])).toEqual({
      interior: false,
      closure: true,
      frontier: true,
      exterior: false,
      derived: true,
    });
    expect(diskDerivedMembership(disk, [0, 0])).toEqual({
      interior: true,
      closure: true,
      frontier: false,
      exterior: false,
      derived: true,
    });
    expect(diskDerivedMembership(disk, [2, 0])).toEqual({
      interior: false,
      closure: false,
      frontier: false,
      exterior: true,
      derived: false,
    });
    const interval = { a: 0, b: 1, leftClosed: false, rightClosed: true };
    expect(inRealInterval(interval, 0)).toBe(false);
    expect(inRealInterval(interval, 1)).toBe(true);
    for (const point of [0, 1])
      expect(intervalDerivedMembership(interval, point)).toEqual({
        interior: false,
        closure: true,
        frontier: true,
        exterior: false,
        derived: true,
      });
    expect(intervalDerivedMembership(interval, 0.5)).toEqual({
      interior: true,
      closure: true,
      frontier: false,
      exterior: false,
      derived: true,
    });
    expect(intervalDerivedMembership(interval, -1).exterior).toBe(true);
    for (const sub of [
      openDiskDerivedSubstance(),
      intervalDerivedSubstance(),
    ]) {
      expect(
        sub.propositions.filter((p) => p.predicate === topology.InteriorOf),
      ).toHaveLength(1);
      for (const entity of sub.entities)
        expect(entity).not.toHaveProperty("icon");
    }
  });

  test("renders the original single-panel compositions and their boundary conventions", async () => {
    const disk = await render(await buildDiskDerivedSetsFigure(), "3.4");
    const area = disk.querySelector(
      'circle[aria-label="derived-disk.interior"]',
    )!;
    const endpoint = disk.querySelector(
      'circle[aria-label="derived-disk.boundary-point"]',
    )!;
    expect(
      Math.hypot(
        Number(endpoint.getAttribute("cx")) - Number(area.getAttribute("cx")),
        Number(endpoint.getAttribute("cy")) - Number(area.getAttribute("cy")),
      ),
    ).toBe(Number(area.getAttribute("r")));
    expect(disk.querySelectorAll("circle")).toHaveLength(2);
    const diskLabels = Array.from(disk.querySelectorAll("[data-tex]")).map(
      (a) => decodeURIComponent(a.getAttribute("data-tex")!),
    );
    expect(diskLabels).toContain("A=A^{\\circ}");
    expect(diskLabels).toContain(
      "\\operatorname{Fr} A=\\{(x,y)\\mid x^2+y^2=1\\}",
    );
    expect(diskLabels).toContain(
      "\\operatorname{Cl} A=\\{(x,y)\\mid x^2+y^2\\leq1\\}",
    );
    const interval = await render(
      await buildIntervalDerivedSetsFigure(),
      "3.5",
    );
    for (const [set, left, right] of [
      ["A", "false", "true"],
      ["interior", "false", "false"],
      ["closure", "true", "true"],
    ] as const) {
      expect(
        interval
          .querySelector(`[aria-label="${set} left endpoint"]`)!
          .getAttribute("data-included"),
      ).toBe(left);
      expect(
        interval
          .querySelector(`[aria-label="${set} right endpoint"]`)!
          .getAttribute("data-included"),
      ).toBe(right);
    }
    const intervalLabels = Array.from(
      interval.querySelectorAll("[data-tex]"),
    ).map((a) => decodeURIComponent(a.getAttribute("data-tex")!));
    expect(intervalLabels).toContain("A=\\{x\\mid 0<x\\leq1\\}");
    expect(intervalLabels).toContain("A^{\\circ}=(0,1)");
    expect(intervalLabels).toContain("\\operatorname{Cl} A=[0,1]");
  });

  test("reuses the derived-set styles on different intervals and disk radii", async () => {
    const sub = intervalDerivedSubstance({
      a: -2,
      b: 5,
      leftClosed: true,
      rightClosed: false,
    });
    const svg = await render(
      await diagram({
        sub,
        sty: intervalDerivedSetsStyle(),
        canvas: canvas(610, 115),
      }),
    );
    expect(
      svg
        .querySelector('[aria-label="A left endpoint"]')!
        .getAttribute("data-included"),
    ).toBe("true");
    expect(
      svg
        .querySelector('[aria-label="A right endpoint"]')!
        .getAttribute("data-included"),
    ).toBe("false");
    const disk = await render(
      await diagram({
        sub: openDiskDerivedSubstance(0.75),
        sty: diskDerivedSetsStyle(),
        canvas: canvas(370, 350),
      }),
    );
    expect(
      disk
        .querySelector('circle[aria-label="derived-disk.interior"]')!
        .getAttribute("r"),
    ).toBe("75");
    expect(
      Array.from(disk.querySelectorAll("[data-tex]")).map((a) =>
        decodeURIComponent(a.getAttribute("data-tex")!),
      ),
    ).toContain("\\operatorname{Cl} A=\\{(x,y)\\mid x^2+y^2\\leq0.5625\\}");
  });
});
