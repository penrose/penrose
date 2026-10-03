// @vitest-environment jsdom

import {
  canvas,
  compactHausdorffStyle,
  diagram,
  extendFiniteCoverReach,
  inOpenDisk,
  inRealInterval,
  separatedCoverRadii,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildCompactHausdorffFigure,
  buildFiniteCoverReachFigure,
  buildSeparatedMetricCoverFigure,
  compactSubsetHausdorffSeparation,
  finiteCoverSupremumWitness,
  separatedMetricCover,
} from "./covering-properties.js";

/** Check the rendered cubic contours, independent of the declared topology facts. */
function containsRenderedPoint(
  path: Element,
  point: readonly [number, number],
) {
  const vertices: [number, number][] = [];
  let previous: [number, number] = [0, 0];
  for (const command of (path.getAttribute("d") ?? "").match(
    /[MLCZ][^MLCZ]*/g,
  ) ?? []) {
    const coordinates = (
      command.slice(1).match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
    ).map(Number);
    if (command[0] === "M" || command[0] === "L") {
      previous = [coordinates[0], coordinates[1]];
      vertices.push(previous);
    } else if (command[0] === "C") {
      const start = previous;
      for (let i = 1; i <= 30; i++) {
        const t = i / 30,
          q = 1 - t;
        vertices.push(
          [0, 1].map(
            (axis) =>
              q ** 3 * start[axis] +
              3 * q * q * t * coordinates[axis] +
              3 * q * t * t * coordinates[axis + 2] +
              t ** 3 * coordinates[axis + 4],
          ) as [number, number],
        );
      }
      previous = [coordinates[4], coordinates[5]];
    }
  }
  let inside = false;
  for (let i = 0, j = vertices.length - 1; i < vertices.length; j = i++) {
    const a = vertices[i],
      b = vertices[j];
    if (
      a[1] > point[1] !== b[1] > point[1] &&
      point[0] < ((b[0] - a[0]) * (point[1] - a[1])) / (b[1] - a[1]) + a[0]
    )
      inside = !inside;
  }
  return inside;
}

describe("separated metric covering families", () => {
  test("the half and quarter radii preserve strict half-ball separation", () => {
    for (const p of [1e-6, 1, 3, 100]) {
      const { half, quarter } = separatedCoverRadii(p);
      expect(half).toBe(p / 2);
      expect(quarter).toBe(p / 4);
      const left = { center: [0, 0] as const, radius: half },
        right = { center: [p, 0] as const, radius: half };
      for (let i = 0; i <= 100; i++) {
        const point = [(p * i) / 100, 0] as const;
        expect(inOpenDisk(left, point) && inOpenDisk(right, point)).toBe(false);
      }
      expect(inOpenDisk(left, [half, 0])).toBe(false);
    }
    for (const p of [0, -1, NaN, Infinity, Number.MIN_VALUE])
      expect(() => separatedCoverRadii(p)).toThrow();
  });

  test("generic E is not replaced by the finite drawn grid", async () => {
    const result = await diagram({
      sub: separatedMetricCover(),
      canvas: canvas(219, 213),
      sty: topology.style((ctx) => {
        const centers = ctx.entities(topology.MaximalSeparatedSubset)[0];
        expect("points" in centers).toBe(false);
        expect(ctx.entities(topology.CoordinatePoint)).toHaveLength(0);
        expect(ctx.test(topology.Countable, centers)).toBe(true);
        const [cover, half, e, v] = ctx.facts(
          topology.IndispensableCenteredCover,
        )[0];
        expect(e).toBe(centers);
        expect(half.radius).toBe(centers.separation / 2);
        const [closures, quarter, tau] = ctx.facts(
          topology.ClosuresOfFamily,
        )[0];
        expect(quarter.radius).toBe(centers.separation / 4);
        expect(closures.radius).toBe(quarter.radius);
        const union = ctx
          .facts(topology.UnionOf)
          .find(([, family]) => family === closures)![0];
        expect(
          ctx
            .facts(topology.ComplementOf)
            .some(([c, u]) => c === v && u === union),
        ).toBe(true);
        expect(ctx.test(topology.OpenIn, v, tau)).toBe(true);
        expect(ctx.test(topology.FamilyIncludedIn, half, cover)).toBe(true);
        expect(ctx.test(topology.Lindelof, tau)).toBe(true);
      }),
    });
    result.discard();
  });
});

describe("finite-cover reach past an assumed supremum", () => {
  test("an arbitrary open interval around interior u yields a covered point strictly beyond u", () => {
    for (const u of [0.01, 0.3, 0.55, 0.99]) {
      const interval = {
        a: u - 0.001,
        b: u + 0.003,
        leftClosed: false,
        rightClosed: false,
      };
      const next = extendFiniteCoverReach(u, interval);
      expect(next).toBeGreaterThan(u);
      expect(next).toBeLessThanOrEqual(1);
      expect(inRealInterval(interval, next)).toBe(true);
    }
    for (const u of [0, 1, NaN])
      expect(() =>
        extendFiniteCoverReach(u, {
          a: -1,
          b: 2,
          leftClosed: false,
          rightClosed: false,
        }),
      ).toThrow();
    expect(() =>
      finiteCoverSupremumWitness({ interval: [0.56, 0.7] }),
    ).toThrow();
  });

  test("the snapshot declares a contradiction hypothesis and a genuine finite cover extension", async () => {
    const result = await diagram({
      sub: finiteCoverSupremumWitness(),
      canvas: canvas(339, 63),
      sty: topology.style((ctx) => {
        const [u, reach] = ctx.facts(topology.AssumedSupremumOf)[0];
        const [, original, unit] = ctx.facts(topology.FiniteCoverReachOf)[0];
        const interval = ctx.entities(topology.OpenIntervalNeighborhood)[0];
        const initial = ctx.facts(topology.InitialIntervalAt)[0][0];
        expect(initial.rightClosed).toBe(false);
        expect(initial.b).toBe(u.coordinate);
        expect(ctx.test(topology.Member, u, reach)).toBe(true);
        expect(ctx.test(topology.Member, u, interval)).toBe(true);
        const next = ctx.entities(topology.RealPoint).find((p) => p !== u)!;
        expect(next.coordinate).toBeGreaterThan(u.coordinate);
        expect(ctx.test(topology.Member, next, reach)).toBe(true);
        const covers = ctx.entities(topology.FiniteOpenCover);
        expect(covers).toHaveLength(2);
        for (const cover of covers)
          expect(ctx.test(topology.SubfamilyOf, cover, original)).toBe(true);
        const extended = ctx
          .entities(topology.ClosedInterval)
          .find((i) => i !== unit)!;
        expect(extended.b).toBe(next.coordinate);
        expect(
          ctx.facts(topology.OpenCoverOf).some(([, set]) => set === extended),
        ).toBe(true);
      }),
    });
    result.discard();
  });
});

describe("compact subsets in a Hausdorff space", () => {
  test("compact A starts as a generic subset, without assuming the closedness being proved", async () => {
    const result = await diagram({
      sub: compactSubsetHausdorffSeparation(),
      canvas: canvas(478, 198),
      sty: topology.style((ctx) => {
        const [a, tau] = ctx.facts(topology.CompactIn)[0];
        expect(ctx.entities(topology.ClosedSet)).toHaveLength(0);
        expect(ctx.test(topology.T2, tau)).toBe(true);
        const cover = ctx.entities(topology.FiniteOpenCover)[0];
        expect(ctx.test(topology.OpenCoverOf, cover, a, tau)).toBe(true);
        const [union] = ctx.facts(topology.UnionOf)[0];
        const [intersection, family] = ctx.facts(
          topology.FiniteIntersectionOf,
        )[0];
        expect(ctx.test(topology.Subset, a, union)).toBe(true);
        expect(ctx.test(topology.Disjoint, intersection, union)).toBe(true);
        expect(ctx.facts(topology.HausdorffNeighborhoodPair)).toHaveLength(6);
        for (const [x, y, u, v] of ctx.facts(
          topology.HausdorffNeighborhoodPair,
        )) {
          expect(ctx.test(topology.Outside, x, a)).toBe(true);
          expect(ctx.test(topology.Member, y, a)).toBe(true);
          expect(ctx.test(topology.Disjoint, u, v)).toBe(true);
          expect(ctx.test(topology.SetInFamily, v, cover)).toBe(true);
          expect(ctx.test(topology.SetInFamily, u, family)).toBe(true);
          expect(ctx.test(topology.Subset, intersection, u)).toBe(true);
        }
      }),
    });
    result.discard();
  });

  test("the separation style rejects a compact subset without Hausdorff witnesses", async () => {
    const sub = topology.substance();
    const a = sub.Set({ label: "A" }),
      tau = sub.Topology({ label: "tau" });
    sub.CompactIn(a, tau);
    await expect(
      diagram({
        sub: sub.make(),
        sty: compactHausdorffStyle(),
        canvas: canvas(478, 198),
      }),
    ).rejects.toThrow("Hausdorff");
  });
});

describe("actual native renderings of Figures 7.1–7.3", () => {
  test("quarter-ball closures retain nine illustrative centers and no extra half-ball outlines", async () => {
    const result = await buildSeparatedMetricCoverFigure();
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(18);
      expect(svg.querySelector('[data-tex="V"]')).not.toBeNull();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("the unit-interval panel retains the endpoint and proposed-supremum dots and the open witness", async () => {
    const result = await buildFiniteCoverReachFigure();
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(3);
      expect(
        svg.querySelector('path[aria-label="cover.supremum-neighborhood"]'),
      ).not.toBeNull();
      expect(svg.querySelector('[data-tex="u"]')).not.toBeNull();
      expect(svg.querySelector('[data-tex="v"]')).toBeNull();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("the outside neighborhood hatching is clipped by every depicted member of the intersection", async () => {
    const result = await buildCompactHausdorffFigure();
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(3);
      expect(svg.querySelectorAll("clipPath")).toHaveLength(9);
      const outside = svg.querySelector(
        'circle[aria-label="compact.outside-point"]',
      )!;
      const x = [
        Number(outside.getAttribute("cx")),
        Number(outside.getAttribute("cy")),
      ] as const;
      for (let i = 0; i < 4; i++)
        expect(
          containsRenderedPoint(
            svg.querySelector(
              `path[aria-label="compact.outside-neighborhood-${i}"]`,
            )!,
            x,
          ),
        ).toBe(true);
      for (const name of ["first", "last"]) {
        const dot = svg.querySelector(
          `circle[aria-label="compact.${name}-point"]`,
        )!;
        expect(
          containsRenderedPoint(
            svg.querySelector(
              `path[aria-label="compact.${name}-neighborhood"]`,
            )!,
            [Number(dot.getAttribute("cx")), Number(dot.getAttribute("cy"))],
          ),
        ).toBe(true);
      }
      expect(
        svg.querySelector('path[aria-label="compact.subset-a"]'),
      ).not.toBeNull();
      expect(svg.querySelector('[data-tex="U"]')).not.toBeNull();
      expect(svg.querySelector('[data-tex="V"]')).not.toBeNull();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });
});
