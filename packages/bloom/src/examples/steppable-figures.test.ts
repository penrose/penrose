import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import type { EntityOf } from "../core/program.js";
import {
  inverseCancellationParameter,
  trigonometricLoopValue,
} from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  affineContractionIteratesSubstance,
  buildContractionIteratesIllustration,
  buildContractionIterationFrameFigure,
} from "./contraction-iterates.js";
import {
  buildRadialContractionFrameFigure,
  radialDiskContractionSliceSubstance,
} from "./homotopies.js";
import {
  buildInverseCancellationFrameFigure,
  inverseCancellationSubstance,
} from "./loop-retracing.js";

async function render(d: Diagram) {
  let converged = false;
  for (let i = 0; i < 3000; i++)
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  expect(converged).toBe(true);
  const { svg } = await d.render();
  const xml = new XMLSerializer().serializeToString(svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(svg.querySelectorAll("image")).toHaveLength(0);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return svg;
}

const numbers = (path: Element) =>
  path
    .getAttribute("d")!
    .match(/[-+]?(?:\d*\.\d+|\d+)(?:[eE][-+]?\d+)?/g)!
    .map(Number);
const named = (svg: SVGSVGElement, name: string) => {
  const explicit = svg.querySelector(`[aria-label="${name}"]`);
  if (explicit) return explicit;
  const title = Array.from(svg.querySelectorAll("title")).find(
    (node) => node.textContent === name,
  );
  const parent = title?.parentElement;
  return parent?.tagName === "g"
    ? parent.querySelector("path,line,circle,ellipse,polygon")!
    : parent!;
};
const markup = (e: Element) => new XMLSerializer().serializeToString(e);

describe("native steppable mathematical illustrations", () => {
  test("each selected homotopy time declares exactly its true slice, with the book's endpoint convention", () => {
    for (const family of ["circle", "figure-eight"] as const)
      for (let frame = 0; frame <= 12; frame++) {
        const time = frame / 12;
        const sub = inverseCancellationSubstance(family, time);
        const homotopy = sub.propositions.find(
          (p) => p.predicate === topology.HomotopyBetween,
        )!;
        const constant = sub.propositions.find(
          (p) => p.predicate === topology.ConstantLoopAt,
        )!;
        const product = sub.propositions.find(
          (p) => p.predicate === topology.LoopProductOf,
        )!;
        const slices = sub.propositions.filter(
          (p) => p.predicate === topology.SliceMapAt,
        );
        expect(slices).toHaveLength(1);
        expect(homotopy.args[1]).toBe(constant.args[0]);
        expect(homotopy.args[2]).toBe(product.args[0]);
        expect(slices[0].args[1]).toBe(homotopy.args[0]);
        expect(
          (slices[0].args[2] as EntityOf<typeof topology.RealPoint>).coordinate,
        ).toBe(time);
        if (time === 0) expect(slices[0].args[0]).toBe(product.args[0]);
        if (time === 1) expect(slices[0].args[0]).toBe(constant.args[0]);
        const a = product.args[1] as EntityOf<
          typeof topology.TrigonometricLoop
        >;
        for (const r of [0, 0.2, 0.5, 0.7, 1]) {
          const parameter = inverseCancellationParameter(r, time);
          expect(parameter).toBeCloseTo(
            2 * Math.min(r, 1 - r) * (1 - time),
            12,
          );
          if (time === 1 || r === 0 || r === 1)
            expect(trigonometricLoopValue(a, parameter)).toEqual(
              (constant.args[1] as EntityOf<typeof topology.CoordinatePoint>)
                .coordinates,
            );
        }
      }
    expect(
      inverseCancellationSubstance().propositions.filter(
        (p) => p.predicate === topology.SliceMapAt,
      ),
    ).toHaveLength(4);
  });

  test("rendered loop frames are genuine retracing paths on a fixed native chart, including the constant endpoint", async () => {
    for (const family of ["circle", "figure-eight"] as const) {
      const sub = inverseCancellationSubstance(family, 0);
      const a = sub.propositions.find(
        (p) => p.predicate === topology.LoopInverseOf,
      )!.args[1] as EntityOf<typeof topology.TrigonometricLoop>;
      const reference = Array.from({ length: 193 }, (_, i) =>
        trigonometricLoopValue(a, i / 192),
      );
      const xMin = Math.min(...reference.map(([x]) => x)),
        xMax = Math.max(...reference.map(([x]) => x)),
        yMin = Math.min(...reference.map(([, y]) => y)),
        yMax = Math.max(...reference.map(([, y]) => y));
      const unit = 80 / Math.max(xMax - xMin, yMax - yMin);
      const origin = [
        (-unit * (xMin + xMax)) / 2,
        6 - (unit * (yMin + yMax)) / 2,
      ];
      let referenceMarkup: string | undefined;
      for (const time of [0, 0.5, 1]) {
        const d = await buildInverseCancellationFrameFigure(family, time);
        try {
          const svg = await render(d);
          const background = markup(named(svg, "retracing.reference-0"));
          referenceMarkup ??= background;
          expect(background).toBe(referenceMarkup);
          expect(
            svg.querySelectorAll('[aria-label^="fixed retracing basepoint "]'),
          ).toHaveLength(1);
          const slice = svg.querySelector(
            'path[aria-label="retracing.slice-0"]',
          );
          if (time === 1) expect(slice).toBeNull();
          else {
            const coordinates = numbers(slice!);
            expect(coordinates).toHaveLength(386);
            for (let i = 0; i <= 192; i++) {
              const [x, y] = trigonometricLoopValue(
                a,
                2 * Math.min(i / 192, 1 - i / 192) * (1 - time),
              );
              expect(coordinates[2 * i]).toBeCloseTo(
                128 + origin[0] + unit * x,
                8,
              );
              expect(coordinates[2 * i + 1]).toBeCloseTo(
                88 - origin[1] - unit * y,
                8,
              );
            }
          }
        } finally {
          d.discard();
        }
      }
    }
  });

  test("iteration frames show exact prefixes and geometric errors without rescaling the axes", async () => {
    for (const [slope, intercept, initial] of [
      [0.5, 1, -2],
      [-0.6, 0.8, 3],
    ] as const) {
      const steps = 8;
      const fixed = intercept / (1 - slope);
      const values: number[] = [initial];
      for (let n = 0; n < steps; n++)
        values.push(slope * values[n] + intercept);
      const sub = affineContractionIteratesSubstance(
        slope,
        intercept,
        initial,
        steps,
      );
      const samples = sub.entities.filter((e) => "index" in e) as EntityOf<
        typeof topology.RealIterationSample
      >[];
      expect(samples.map((p) => p.coordinate)).toEqual(values);
      const low = Math.min(0, fixed, ...values),
        high = Math.max(0, fixed, ...values),
        min = low - 0.15 * (high - low),
        max = high + 0.15 * (high - low);
      const screenX = (x: number) =>
        350 - 315 + (260 * (x - min)) / (max - min);
      const screenY = (y: number) =>
        165 + 110 - (260 * (y - min)) / (max - min);
      let axes: string[] | undefined;
      let finalCobweb: string | undefined;
      for (const currentStep of [0, 1, 4, 8]) {
        const d = await buildContractionIterationFrameFigure(
          slope,
          intercept,
          initial,
          steps,
          currentStep,
          {
            interactive: currentStep === 4 ? { jitter: 0 } : false,
          },
        );
        try {
          const svg = await render(d);
          const frameAxes = [
            "contraction.axis-x",
            "contraction.axis-y",
            "contraction.error-axis-x",
            "contraction.error-axis-y",
          ].map((name) => markup(named(svg, name)));
          axes ??= frameAxes;
          expect(frameAxes).toEqual(axes);
          const cobweb = named(svg, "contraction.cobweb");
          if (currentStep === 0) expect(cobweb).toBeFalsy();
          const coords = currentStep === 0 ? [] : numbers(cobweb);
          if (currentStep > 0) {
            expect(coords).toHaveLength(2 + 4 * currentStep);
            expect(coords[0]).toBeCloseTo(screenX(initial), 8);
            expect(coords[1]).toBeCloseTo(screenY(0), 8);
          }
          for (let n = 1; n <= currentStep; n++) {
            const at = 2 + 4 * (n - 1);
            expect(coords[at]).toBeCloseTo(screenX(values[n - 1]), 8);
            expect(coords[at + 1]).toBeCloseTo(screenY(values[n]), 8);
            expect(coords[at + 2]).toBeCloseTo(screenX(values[n]), 8);
            expect(coords[at + 3]).toBeCloseTo(screenY(values[n]), 8);
          }
          expect(
            Array.from(svg.querySelectorAll("circle > title")).filter(
              (node) => node.textContent?.startsWith("contraction.error-"),
            ),
          ).toHaveLength(currentStep + 1);
          for (let n = 0; n <= currentStep; n++) {
            const dot = named(svg, "contraction.error-" + n);
            expect(Number(dot.getAttribute("cx"))).toBeCloseTo(
              350 + 45 + (270 * n) / steps,
              8,
            );
            expect(Number(dot.getAttribute("cy"))).toBeCloseTo(
              165 + 45 - 115 * Math.abs(slope) ** n,
              8,
            );
          }
          const current = named(svg, "current iterate s_" + currentStep);
          expect(Number(current.getAttribute("cx"))).toBeCloseTo(
            screenX(values[currentStep]),
            8,
          );
          expect(Number(current.getAttribute("cy"))).toBeCloseTo(
            screenY(currentStep === 0 ? 0 : values[currentStep]),
            8,
          );
          if (currentStep === 4)
            expect(d.getDraggingConstraints().size).toBeGreaterThan(5);
          if (currentStep === steps) finalCobweb = markup(cobweb);
        } finally {
          d.discard();
        }
      }
      const original = await buildContractionIteratesIllustration(
        slope,
        intercept,
        initial,
        steps,
      );
      try {
        expect(
          markup(named(await render(original), "contraction.cobweb")),
        ).toBe(finalCobweb);
      } finally {
        original.discard();
      }
    }
  });

  test("radial slices assert their continuous family and exact identity/singleton endpoints", () => {
    for (let frame = 0; frame <= 12; frame++) {
      const factor = 1 - frame / 12;
      const sub = radialDiskContractionSliceSubstance(factor, [0.6, 0.8]);
      const slice = sub.propositions.find(
        (p) => p.predicate === topology.SliceMapAt,
      )!;
      const endpoints = sub.propositions.find(
        (p) => p.predicate === topology.HomotopyBetween,
      )!;
      const map = slice.args[0] as EntityOf<typeof topology.RadialContraction>;
      expect(map.factor).toBe(factor);
      expect(slice.args[1]).toBe(endpoints.args[0]);
      expect(
        (slice.args[2] as EntityOf<typeof topology.RealPoint>).coordinate,
      ).toBe(factor);
      for (const relation of sub.propositions.filter(
        (p) => p.predicate === topology.MapsTo,
      )) {
        const p = relation.args[1] as EntityOf<typeof topology.CoordinatePoint>,
          q = relation.args[2] as EntityOf<typeof topology.CoordinatePoint>;
        expect(q.coordinates).toEqual(p.coordinates.map((x) => factor * x));
        expect(Math.hypot(...q.coordinates)).toBeLessThanOrEqual(
          factor + 1e-14,
        );
      }
      const image = sub.propositions.find(
        (p) => p.predicate === topology.ImageOf,
      )!.args[0];
      if (factor === 0) {
        expect(slice.args[0]).toBe(endpoints.args[2]);
        expect(
          sub.propositions.some(
            (p) => p.predicate === topology.SingletonOf && p.args[0] === image,
          ),
        ).toBe(true);
        expect(image).not.toHaveProperty("radius");
      } else
        expect((image as EntityOf<typeof topology.ClosedDisk>).radius).toBe(
          factor,
        );
      if (factor === 1) expect(slice.args[0]).toBe(endpoints.args[1]);
      expect(
        sub.propositions.some(
          (p) =>
            p.predicate === topology.IdentityOn &&
            p.args[0] === endpoints.args[1],
        ),
      ).toBe(true);
      expect(
        sub.propositions.some(
          (p) =>
            p.predicate === topology.ConstantTo &&
            p.args[0] === endpoints.args[2],
        ),
      ).toBe(true);
    }
  });

  test("native radial frames preserve the source disk while moving the image point and shrinking the image exactly", async () => {
    for (const point of [
      [0.53, 0.53],
      [-0.3, 0.8],
    ] as const) {
      let sourceMarkup: string | undefined;
      for (const [time, parameterLabel] of [
        [0, "r=1"],
        [1 / 12, "r\\approx 0.9167"],
        [0.25, "r=0.75"],
        [0.5, "r=1/2"],
        [1, "r=0"],
      ] as const) {
        const d = await buildRadialContractionFrameFigure(time, point, {
          interactive: { jitter: 0 },
        });
        try {
          const svg = await render(d);
          expect(
            Array.from(svg.querySelectorAll("[aria-label]")).some(
              (node) => node.getAttribute("aria-label") === parameterLabel,
            ),
          ).toBe(true);
          const source = named(svg, "closed source disk");
          sourceMarkup ??= markup(source);
          expect(markup(source)).toBe(sourceMarkup);
          const image = svg.querySelector(
            'circle[aria-label="concentric radial image disk"]',
          );
          if (time === 1) {
            expect(image).toBeNull();
            expect(named(svg, "radial image singleton")).not.toBeNull();
          } else
            expect(Number(image!.getAttribute("r"))).toBeCloseTo(
              Number(source.getAttribute("r")) * (1 - time),
              10,
            );
          const p = named(svg, "disk-contraction.source-point"),
            q = named(svg, "disk-contraction.image-point");
          for (const axis of ["cx", "cy"]) {
            const center = Number(source.getAttribute(axis));
            expect(Number(q.getAttribute(axis)) - center).toBeCloseTo(
              (1 - time) * (Number(p.getAttribute(axis)) - center),
              10,
            );
          }
          expect(d.getDraggingConstraints().size).toBeGreaterThan(3);
        } finally {
          d.discard();
        }
      }
    }
  });

  test("frame factories reject nonmathematical times and nonexistent iteration indices", async () => {
    for (const time of [-1, 1.1, NaN, Infinity]) {
      expect(() => inverseCancellationSubstance("circle", time)).toThrow(
        /time/,
      );
      expect(() => buildInverseCancellationFrameFigure("circle", time)).toThrow(
        /time/,
      );
      expect(() => buildRadialContractionFrameFigure(time)).toThrow(/time/);
    }
    for (const step of [-1, 0.5, 9, NaN, Infinity])
      await expect(
        buildContractionIterationFrameFigure(0.5, 1, -2, 8, step),
      ).rejects.toThrow(/iteration index/);
    for (const steps of [0, 21, 0.5])
      expect(() =>
        buildContractionIterationFrameFigure(0.5, 1, -2, steps, 0),
      ).toThrow(/one to twenty/);
    expect(() => radialDiskContractionSliceSubstance(0.5, [1, 1])).toThrow(
      /unit disk/,
    );
  });
});
