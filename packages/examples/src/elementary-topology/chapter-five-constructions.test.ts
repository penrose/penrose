// @vitest-environment jsdom

import {
  canvas,
  diagram,
  dyadicRefinementValues,
  dyadicValue,
  illustrativeLowerPath,
  illustrativeUpperPath,
  inIntervalProduct,
  interpolateBoundaryPaths,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildDyadicRefinementFigure,
  buildDyadicSeparationFigure,
  dyadicRefinementNeighborhoods,
  dyadicSeparatedClosedSets,
} from "./dyadic-neighborhoods.js";
import {
  buildLowerLimitDiagonalFigure,
  buildLowerLimitNeighborhoodFigure,
  lowerLimitBasicNeighborhood,
  lowerLimitDiagonalSingleton,
} from "./lower-limit-topology.js";
import {
  boundaryPathExtension,
  buildBoundaryPathExtensionFigure,
} from "./path-extensions.js";

describe("lower-limit products and their discrete antidiagonal", () => {
  test("the basic rectangle includes its left/bottom edges and excludes its top/right edges", async () => {
    const result = await diagram({
      sub: lowerLimitBasicNeighborhood(),
      canvas: canvas(325, 211),
      sty: topology.style((ctx) => {
        const rectangle = ctx.entities(topology.LowerLimitPlaneNeighborhood)[0];
        const [, xSet, ySet] = ctx
          .facts(topology.ProductOf)
          .find(([s]) => s === rectangle)!;
        const intervals = ctx.entities(topology.LowerLimitInterval);
        const x = intervals.find((i) => i === xSet)!,
          y = intervals.find((i) => i === ySet)!;
        const middleX = (x.a + x.b) / 2,
          middleY = (y.a + y.b) / 2;
        for (const point of [
          [x.a, y.a],
          [x.a, middleY],
          [middleX, y.a],
        ] as const)
          expect(inIntervalProduct(x, y, point)).toBe(true);
        for (const point of [
          [x.b, y.a],
          [x.a, y.b],
          [middleX, y.b],
          [x.b, middleY],
        ] as const)
          expect(inIntervalProduct(x, y, point)).toBe(false);
        const tau = ctx
          .facts(topology.OpenIn)
          .find(([s]) => s === rectangle)![1];
        expect(ctx.test(topology.ClosedIn, rectangle, tau)).toBe(true);
        expect(ctx.test(topology.ClosureOf, rectangle, rectangle, tau)).toBe(
          true,
        );
        expect(ctx.test(topology.Regular, tau)).toBe(true);
        expect(ctx.test(topology.NotNormal, tau)).toBe(true);
      }),
    });
    result.discard();
  });

  test("x+y=0 meets the northeast rectangle only at its southwest corner", async () => {
    const result = await diagram({
      sub: lowerLimitDiagonalSingleton(),
      canvas: canvas(331, 241),
      sty: topology.style((ctx) => {
        const rectangle = ctx.entities(topology.LowerLimitPlaneNeighborhood)[0];
        const [, xSet, ySet] = ctx
          .facts(topology.ProductOf)
          .find(([s]) => s === rectangle)!;
        const intervals = ctx.entities(topology.LowerLimitInterval);
        const x = intervals.find((i) => i === xSet)!,
          y = intervals.find((i) => i === ySet)!;
        for (let n = -100; n <= 100; n++) {
          const delta = n / 200;
          expect(inIntervalProduct(x, y, [x.a + delta, y.a - delta])).toBe(
            n === 0,
          );
        }
        const [singleton, , line] = ctx.facts(topology.IntersectionOf)[0];
        const point = ctx.entities(topology.CoordinatePoint)[0];
        expect(
          ctx
            .facts(topology.SingletonOf)
            .some(([s, p]) => s === singleton && p === point),
        ).toBe(true);
        const tau = ctx
          .facts(topology.TopologyOn)
          .find(([, s]) => s === line)![0];
        expect(ctx.test(topology.Discrete, tau)).toBe(true);
        expect(
          ctx
            .facts(topology.SubspaceTopologyOf)
            .some(([t, s]) => t === tau && s === line),
        ).toBe(true);
      }),
    });
    result.discard();
  });
});

describe("dyadic Urysohn neighborhoods", () => {
  test("refinement uses exact binary indices, including the corrected middle exponent", () => {
    expect(dyadicRefinementValues(3, 2)).toEqual([0.25, 0.375, 0.5]);
    expect(dyadicValue(0, 0)).toBe(0);
    expect(dyadicValue(2 ** 52, 52)).toBe(1);
    for (const args of [
      [-1, 2],
      [5, 2],
      [1.5, 2],
      [1, 53],
      [1, -1],
    ] as const)
      expect(() => dyadicValue(args[0], args[1])).toThrow();
    expect(() => dyadicRefinementValues(2, 2)).toThrow();
    expect(() => dyadicRefinementNeighborhoods({ numerator: 0 })).toThrow();
  });

  test("A and B remain disjoint closed sets and Cl U(q) stays inside the next U(q prime)", async () => {
    const result = await diagram({
      sub: dyadicSeparatedClosedSets(),
      canvas: canvas(318, 221),
      sty: topology.style((ctx) => {
        const [family, a, b, tau] = ctx.facts(topology.DyadicFamilyFor)[0];
        expect(ctx.test(topology.T4, tau)).toBe(true);
        expect(ctx.test(topology.ClosedIn, a, tau)).toBe(true);
        expect(ctx.test(topology.ClosedIn, b, tau)).toBe(true);
        expect(ctx.test(topology.Disjoint, a, b)).toBe(true);
        const [f, u, closure, next] = ctx.facts(
          topology.DyadicSeparationStep,
        )[0];
        expect(f).toBe(family);
        expect(ctx.test(topology.ClosureOf, closure, u, tau)).toBe(true);
        expect(ctx.test(topology.Subset, closure, next)).toBe(true);
        for (const open of [u, next]) {
          expect(ctx.test(topology.Subset, a, open)).toBe(true);
          expect(ctx.test(topology.Disjoint, b, open)).toBe(true);
          expect(ctx.test(topology.OpenIn, open, tau)).toBe(true);
        }
      }),
    });
    result.discard();
  });

  test("the inserted index and both closure inclusions follow the induction prose", async () => {
    const result = await diagram({
      sub: dyadicRefinementNeighborhoods(),
      canvas: canvas(345, 293),
      sty: topology.style((ctx) => {
        const [, previous, clPrevious, middle, clMiddle, next] = ctx.facts(
          topology.DyadicRefinementStep,
        )[0];
        const [, , , tau] = ctx.facts(topology.DyadicFamilyFor)[0];
        expect(ctx.test(topology.ClosureOf, clPrevious, previous, tau)).toBe(
          true,
        );
        expect(ctx.test(topology.Subset, clPrevious, middle)).toBe(true);
        expect(ctx.test(topology.ClosureOf, clMiddle, middle, tau)).toBe(true);
        expect(ctx.test(topology.Subset, clMiddle, next)).toBe(true);
        const values = [previous, middle, next].map(
          (open) =>
            ctx.facts(topology.DyadicIndexOf).find(([s]) => s === open)![1]
              .coordinate,
        );
        expect(values).toEqual([0.25, 0.375, 0.5]);
        expect(middle.label).toContain("2^{k+1}");
      }),
    });
    result.discard();
  });
});

describe("continuous extension of two boundary paths", () => {
  test("every lower/upper boundary sample agrees exactly with its named path", () => {
    const extension = interpolateBoundaryPaths(
      illustrativeLowerPath,
      illustrativeUpperPath,
      (s, t) => [s + t, s - t],
    );
    for (let n = 0; n <= 100; n++) {
      const s = n / 100;
      expect(extension(s, 0)).toEqual(illustrativeLowerPath(s));
      expect(extension(s, 1)).toEqual(illustrativeUpperPath(s));
      expect(extension(s, 0.37).every(Number.isFinite)).toBe(true);
    }
    expect(() => extension(-0.01, 0.5)).toThrow("closed unit square");
    expect(() => extension(0.5, 1.01)).toThrow("closed unit square");
    expect(() =>
      interpolateBoundaryPaths(
        () => [NaN, 0],
        () => [0, 1],
      )(0, 0),
    ).toThrow("finite");
  });

  test("the illustrative extension has an unfolded patch interior", () => {
    const extension = interpolateBoundaryPaths(
      illustrativeLowerPath,
      illustrativeUpperPath,
      (s) => [0.52 - 0.26 * s, 0],
    );
    const h = 1e-5;
    for (let i = 1; i < 20; i++)
      for (let j = 1; j < 20; j++) {
        const s = i / 20,
          t = j / 20;
        const left = extension(s - h, t),
          right = extension(s + h, t),
          bottom = extension(s, t - h),
          top = extension(s, t + h);
        const jacobian =
          ((right[0] - left[0]) * (top[1] - bottom[1]) -
            (right[1] - left[1]) * (top[0] - bottom[0])) /
          (4 * h * h);
        expect(jacobian).toBeGreaterThan(0.25);
      }
  });

  test("the closed domain is the two horizontal edges rather than the whole square boundary", async () => {
    const result = await diagram({
      sub: boundaryPathExtension(),
      canvas: canvas(337, 198),
      sty: topology.style((ctx) => {
        const [extended, original, boundary, square] = ctx.facts(
          topology.ExtensionOf,
        )[0];
        const pair = ctx.entities(topology.HorizontalBoundaryPair)[0];
        expect(pair).toBe(boundary);
        const squareEntity = ctx.entities(topology.ClosedProductRectangle)[0];
        expect(squareEntity).toBe(square);
        expect(ctx.test(topology.HorizontalEdgesOf, pair, squareEntity)).toBe(
          true,
        );
        expect(
          ctx.entities(topology.LinearSegment).map((s) => s.endpoints),
        ).toEqual([
          [
            [0, 0],
            [1, 0],
          ],
          [
            [0, 1],
            [1, 1],
          ],
        ]);
        const [, lower, upper] = ctx.facts(topology.BoundaryPathsOf)[0];
        expect(lower).not.toBe(upper);
        expect(ctx.facts(topology.ContinuousMap).map(([map]) => map)).toContain(
          original,
        );
        expect(ctx.facts(topology.ContinuousMap).map(([map]) => map)).toContain(
          extended,
        );
        expect(ctx.facts(topology.ClosedIn).some(([s]) => s === boundary)).toBe(
          true,
        );
      }),
    });
    result.discard();
  });
});

describe("native Penrose renderings of Figures 5.9–5.12 and 5.14", () => {
  test("both lower-limit panels retain the half-open hooks and distinct antidiagonal behavior", async () => {
    for (const [build, diagonal] of [
      [buildLowerLimitNeighborhoodFigure, false],
      [buildLowerLimitDiagonalFigure, true],
    ] as const) {
      const result = await build();
      try {
        const { svg } = await result.render();
        expect(
          svg.querySelector('path[aria-label="lower-limit.open-top"]'),
        ).not.toBeNull();
        expect(
          svg.querySelector('path[aria-label="lower-limit.open-right"]'),
        ).not.toBeNull();
        expect(
          Array.from(svg.querySelectorAll("title")).some(
            (title) => title.textContent === "lower-limit.antidiagonal",
          ),
        ).toBe(diagonal);
        expect(svg.querySelectorAll("circle")).toHaveLength(diagonal ? 0 : 1);
        expect(svg.querySelectorAll("clipPath").length).toBeGreaterThan(0);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      } finally {
        result.discard();
      }
    }
  });

  test("the separated sets remain unfilled outlines while refinement uses native clipped hatch layers", async () => {
    const pair = await buildDyadicSeparationFigure();
    const refinement = await buildDyadicRefinementFigure();
    try {
      const { svg: pairSvg } = await pair.render();
      expect(
        pairSvg.querySelector('path[aria-label="dyadic.b"]'),
      ).not.toBeNull();
      expect(pairSvg.querySelectorAll("clipPath")).toHaveLength(0);
      const { svg } = await refinement.render();
      expect(
        svg.querySelector(
          `[data-tex="${encodeURIComponent(
            "U\\left(\\frac{n}{2^k}\\right)",
          )}"]`,
        ),
      ).not.toBeNull();
      expect(svg.querySelectorAll("clipPath")).toHaveLength(3);
      expect(svg.querySelectorAll("line").length).toBeGreaterThan(100);
      expect(
        svg.querySelector('path[aria-label="dyadic.refinement-a"]'),
      ).toBeNull();
      expect(svg.querySelector('[data-tex="A"]')).not.toBeNull();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      pair.discard();
      refinement.discard();
    }
  });

  test("the source square and its warped image are distinct native clipped regions", async () => {
    const result = await buildBoundaryPathExtensionFigure();
    try {
      const { svg } = await result.render();
      expect(
        svg.querySelector('path[aria-label="extension.square-boundary"]'),
      ).not.toBeNull();
      const image = svg.querySelector(
        'path[aria-label="extension.image-boundary"]',
      );
      expect(
        (image?.getAttribute("d") ?? "").match(/L/g)?.length,
      ).toBeGreaterThan(100);
      expect(svg.querySelectorAll("clipPath")).toHaveLength(2);
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });
});
