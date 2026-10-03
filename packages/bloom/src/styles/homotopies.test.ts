import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  radialContractionValue,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildContractibleDiskFigure,
  buildHomotopyFamilyFigure,
  buildRadialDiskContractionFigure,
  buildRadialHomotopyFigure,
  contractibleDiskSubstance,
  homotopyFamilySubstance,
  radialDiskContractionSubstance,
  radialHomotopySubstance,
} from "../examples/homotopies.js";
import {
  diskContractionStyle,
  homotopyFamilyStyle,
  radialHomotopyStyle,
} from "./homotopies.js";

async function render(d: Diagram) {
  let converged = false;
  for (let n = 0; n < 3000; n++)
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  expect(converged).toBe(true);
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

describe("homotopy families", () => {
  test("a radial family preserves its mathematical center, endpoint maps and disk image", () => {
    expect(radialContractionValue([0.6, 0.8], [0, 0], 0)).toEqual([0, 0]);
    expect(radialContractionValue([0.6, 0.8], [0, 0], 1)).toEqual([0.6, 0.8]);
    expect(radialContractionValue([3, 4], [1, 2], 0.5)).toEqual([2, 3]);
    expect(() => radialContractionValue([0, 0], [0, 0], -0.1)).toThrow();
    const sub = radialDiskContractionSubstance();
    const relation = sub.propositions.find(
      (p) => p.predicate === topology.RadialContractionOf,
    )!;
    const map = relation.args[0] as EntityOf<typeof topology.RadialContraction>;
    const Y = relation.args[1] as EntityOf<typeof topology.ClosedDisk>;
    expect(Y.radius).toBe(1);
    for (const fact of sub.propositions.filter(
      (p) => p.predicate === topology.MapsTo,
    )) {
      const p = fact.args[1] as EntityOf<typeof topology.CoordinatePoint>,
        q = fact.args[2] as EntityOf<typeof topology.CoordinatePoint>;
      expect(q.coordinates).toEqual(
        radialContractionValue(p.coordinates, map.center, map.factor),
      );
      expect(Math.hypot(...q.coordinates)).toBeLessThanOrEqual(
        Y.radius * map.factor,
      );
    }
    const family = radialHomotopySubstance();
    const endpoints = family.propositions.find(
      (p) => p.predicate === topology.HomotopyBetween,
    )!;
    const slices = family.propositions.filter(
      (p) => p.predicate === topology.SliceMapAt,
    );
    for (const [r, mapIndex] of [
      [0, 2],
      [1, 1],
    ])
      expect(
        slices.find(
          (p) =>
            (p.args[2] as EntityOf<typeof topology.RealPoint>).coordinate === r,
        )?.args[0],
      ).toBe(endpoints.args[mapIndex]);
    expect(
      family.propositions.some(
        (p) =>
          p.predicate === topology.IdentityOn &&
          p.args[0] === endpoints.args[1],
      ),
    ).toBe(true);
    expect(
      family.propositions.some(
        (p) =>
          p.predicate === topology.ConstantTo &&
          p.args[0] === endpoints.args[2],
      ),
    ).toBe(true);
    for (const entity of family.entities) {
      expect(entity).not.toHaveProperty("shapeType");
      expect(entity).not.toHaveProperty("fillColor");
    }
    expect(() => radialDiskContractionSubstance(0.5, [1, 1])).toThrow();
    expect(() => radialHomotopySubstance(0.8, 0.6)).toThrow();
  });

  test("general endpoint slices are restrictions of the same continuous family", () => {
    const sub = homotopyFamilySubstance();
    const endpoints = sub.propositions.find(
      (p) => p.predicate === topology.HomotopyBetween,
    )!;
    const slices = sub.propositions.filter(
      (p) => p.predicate === topology.SliceMapAt,
    );
    expect(slices).toHaveLength(3);
    for (const p of slices) {
      expect(p.args[1]).toBe(endpoints.args[0]);
      const time = p.args[2] as EntityOf<typeof topology.RealPoint>;
      if (time.coordinate === 1) expect(p.args[0]).toBe(endpoints.args[1]);
      if (time.coordinate === 0) expect(p.args[0]).toBe(endpoints.args[2]);
    }
    expect(
      sub.propositions.filter((p) => p.predicate === topology.ContinuousMap),
    ).toHaveLength(4);
    expect(
      sub.propositions.filter((p) => p.predicate === topology.ImageOf),
    ).toHaveLength(3);
    expect(() => homotopyFamilySubstance(1)).toThrow();
  });

  test("shared disk, cone and abstract-family styles optimize distinct substances natively", async () => {
    const disk = diskContractionStyle(),
      cone = radialHomotopyStyle(),
      family = homotopyFamilyStyle();
    const cases = [
      [contractibleDiskSubstance(), disk],
      [radialDiskContractionSubstance(), disk],
      [radialDiskContractionSubstance(0.4, [0.7, 0.3]), disk],
      [radialHomotopySubstance(), cone],
      [radialHomotopySubstance(0.25, 0.6), cone],
      [homotopyFamilySubstance(), family],
      [homotopyFamilySubstance(0.3), family],
    ] as const;
    for (const [sub, sty] of cases) {
      const d = await diagram({ sub, sty, canvas: canvas(310, 230) });
      try {
        const { svg } = await render(d);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        if (
          sty === disk &&
          sub.propositions.some(
            (p) => p.predicate === topology.RadialContractionOf,
          )
        ) {
          const relation = sub.propositions.find(
            (p) => p.predicate === topology.RadialContractionOf,
          )!;
          const factor = (
            relation.args[0] as EntityOf<typeof topology.RadialContraction>
          ).factor;
          const original = svg.querySelector(
              'circle[aria-label="closed source disk"]',
            )!,
            image = svg.querySelector(
              'circle[aria-label="concentric radial image disk"]',
            )!;
          expect(
            Number(image.getAttribute("r")) /
              Number(original.getAttribute("r")),
          ).toBeCloseTo(factor, 12);
          const p = svg.querySelector(
              'circle[aria-label="disk-contraction.source-point"]',
            )!,
            q = svg.querySelector(
              'circle[aria-label="disk-contraction.image-point"]',
            )!;
          for (const [coordinate, center] of [
            ["cx", "cx"],
            ["cy", "cy"],
          ])
            expect(
              Number(q.getAttribute(coordinate)) -
                Number(original.getAttribute(center)),
            ).toBeCloseTo(
              factor *
                (Number(p.getAttribute(coordinate)) -
                  Number(original.getAttribute(center))),
              10,
            );
        }
        if (sty === cone) {
          const ellipses = Array.from(
            svg.querySelectorAll(
              'ellipse[aria-label^="radial-homotopy.slice-"]',
            ),
          );
          expect(ellipses).toHaveLength(3);
          for (const e of ellipses) {
            const r = Number(e.getAttribute("aria-label")!.split("slice-")[1]);
            expect(Number(e.getAttribute("rx"))).toBeCloseTo(43 * r, 10);
            expect(Number(e.getAttribute("ry"))).toBeCloseTo(17 * r, 10);
          }
        }
      } finally {
        d.discard();
      }
    }
  });

  test("native annotation dragging keeps contraction and family geometry fixed", async () => {
    for (const [factory, expectedHandles] of [
      [buildContractibleDiskFigure, 4],
      [buildRadialDiskContractionFigure, 7],
      [buildRadialHomotopyFigure, 9],
      [buildHomotopyFamilyFigure, 9],
    ] as const) {
      const d = await factory({ interactive: { jitter: 0 } });
      try {
        const before = await render(d),
          handles = Array.from(d.getDraggingConstraints().keys());
        expect(handles).toHaveLength(expectedHandles);
        for (const h of handles) {
          expect(d.getInput(h + ".layout.x")).toBeCloseTo(0, 10);
          expect(d.getInput(h + ".layout.y")).toBeCloseTo(0, 10);
        }
        const fixed = (svg: SVGSVGElement) =>
          Array.from(
            svg.querySelectorAll("path,line,circle,ellipse,polygon"),
            (e) => new XMLSerializer().serializeToString(e),
          );
        const geometry = fixed(before.svg),
          handle = handles[0];
        d.beginDrag(handle);
        d.translate(handle, 3, 1);
        d.endDrag(handle);
        const after = await render(d);
        expect(d.getInput(handle + ".layout.x")).toBeCloseTo(3, 4);
        expect(fixed(after.svg)).toEqual(geometry);
      } finally {
        d.discard();
      }
    }
  });
});
