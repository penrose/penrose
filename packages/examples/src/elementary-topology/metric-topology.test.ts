// @vitest-environment jsdom

import {
  canvas,
  closedIntervalComplementStyle,
  diagram,
  inNeighborhood,
  interiorNeighborhoodRadius,
  intervalComplementRadius,
  metricSpaces,
  neighborhoodInclusionStyle,
  planeDistance,
  reciprocalConvergenceThreshold,
  reciprocalHeightTerm,
  schematicConvergentTerm,
  sequenceConvergenceStyle,
  sequenceMetricComparisonStyle,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import { closedIntervalComplementNeighborhood } from "./closed-interval.js";
import { openBallInteriorNeighborhood } from "./neighborhood-inclusion.js";
import {
  convergentSequenceNeighborhood,
  reciprocalSequenceNeighborhoods,
} from "./sequence-convergence.js";

describe("Figures 2.8–2.11 mathematical content", () => {
  test("the smaller open ball is tangent inside the outer ball and satisfies the triangle bound", () => {
    const x = [0, 0] as const;
    const w = [-0.52, -0.63] as const;
    const q = interiorNeighborhoodRadius("euclidean", x, w, 1);
    expect(planeDistance("euclidean", x, w) + q).toBeCloseTo(1, 14);
    for (let angle = 0; angle < 2 * Math.PI; angle += Math.PI / 20) {
      const z = [
        w[0] + q * 0.999 * Math.cos(angle),
        w[1] + q * 0.999 * Math.sin(angle),
      ] as const;
      expect(inNeighborhood("euclidean", w, q, z)).toBe(true);
      expect(inNeighborhood("euclidean", x, 1, z)).toBe(true);
    }
    expect(() => interiorNeighborhoodRadius("euclidean", x, [1, 0], 1)).toThrow(
      "inside",
    );
    expect(() => openBallInteriorNeighborhood({ interior: [2, 0] })).toThrow(
      "inside",
    );
  });

  test("an exterior point's open interval excludes both endpoints of the closed interval", () => {
    for (const x of [-0.6, 1.4]) {
      const rho = intervalComplementRadius(x, [0, 1]);
      expect(rho).toBeCloseTo(x < 0 ? -x : x - 1);
      for (const z of [0, 0.5, 1])
        expect(Math.abs(z - x)).toBeGreaterThanOrEqual(rho);
    }
    expect(() => closedIntervalComplementNeighborhood({ position: 0 })).toThrow(
      "outside",
    );
    expect(() => intervalComplementRadius(-1, [2, 1])).toThrow(
      "finite closed interval",
    );
  });

  test("the schematic has a finite exceptional prefix and a convergent tail inside the ball", () => {
    const cutoff = 6;
    expect(
      Math.hypot(...schematicConvergentTerm(cutoff, cutoff)),
    ).toBeGreaterThan(1);
    for (let n = cutoff + 1; n <= 100; n++) {
      const term = schematicConvergentTerm(n, cutoff);
      expect(inNeighborhood("euclidean", [0, 0], 1, term)).toBe(true);
    }
    expect(Math.hypot(...schematicConvergentTerm(100, cutoff))).toBeLessThan(
      1e-8,
    );
    expect(() => convergentSequenceNeighborhood({ cutoff: 1.5 })).toThrow(
      "integer cutoff",
    );
  });

  test("the reciprocal sequence converges under exactly the three illustrated plane metrics", () => {
    for (const rho of [0.45, 0.5, 0.02, 2]) {
      const threshold = reciprocalConvergenceThreshold(rho);
      expect(threshold).toBeGreaterThan(1 / rho);
      for (let n = threshold + 1; n < threshold + 20; n++) {
        const point = reciprocalHeightTerm(n);
        for (const metric of ["euclidean", "taxicab", "supremum"] as const) {
          expect(planeDistance(metric, [1, 0], point)).toBe(1 / n);
          expect(inNeighborhood(metric, [1, 0], rho, point)).toBe(true);
        }
        expect(inNeighborhood("discrete", [1, 0], 0.5, point)).toBe(false);
      }
    }
    expect(() => reciprocalHeightTerm(0)).toThrow("positive integers");
    expect(() => reciprocalSequenceNeighborhoods({ rho: 0 })).toThrow(
      "threshold",
    );
  });

  test("declared tail membership is checked against every drawn sample", async () => {
    await expect(
      diagram({
        sub: convergentSequenceNeighborhood(),
        sty: sequenceConvergenceStyle({ sampleTerm: () => [2, 0] }),
      }),
    ).rejects.toThrow("Drawn tail terms");
    await expect(
      diagram({
        sub: reciprocalSequenceNeighborhoods(),
        sty: sequenceMetricComparisonStyle({ sampleTerm: () => [1, 2] }),
      }),
    ).rejects.toThrow("contradict");
  });

  test("false inclusion facts cannot render a mathematically inconsistent inner radius", async () => {
    const sub = metricSpaces.substance();
    const plane = sub.MetricPlane({ metric: "euclidean" });
    const x = sub.MetricPoint({ label: "x", point: [0, 0] });
    const w = sub.MetricPoint({ label: "w", point: [0.5, 0] });
    const outer = sub.Neighborhood({ point: x.point, rho: 1 });
    const inner = sub.Neighborhood({ point: w.point, rho: 0.8 });
    sub.InSpace(inner, plane);
    sub.InSpace(outer, plane);
    sub.NeighborhoodAt(inner, w);
    sub.NeighborhoodAt(outer, x);
    sub.InsideNeighborhood(w, outer);
    sub.NeighborhoodContainedIn(inner, outer);
    await expect(
      diagram({ sub: sub.make(), sty: neighborhoodInclusionStyle() }),
    ).rejects.toThrow("ρ−D(x,w)");
  });
});

describe("Figures 2.8–2.11 actual Penrose SVG rendering", () => {
  test("the nested neighborhoods and both cubic distance braces survive SVG lowering", async () => {
    const result = await diagram({
      sub: openBallInteriorNeighborhood(),
      sty: neighborhoodInclusionStyle(),
      canvas: canvas(340, 330),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(4);
      const braces = Array.from(svg.querySelectorAll("path")).filter(
        (path) => path.getAttribute("d")?.includes("C"),
      );
      expect(braces.length).toBeGreaterThanOrEqual(2);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("closed endpoints, open parentheses, hatches and the radius brace render on the number line", async () => {
    const result = await diagram({
      sub: closedIntervalComplementNeighborhood(),
      sty: closedIntervalComplementStyle(),
      canvas: canvas(330, 110),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("rect")).toHaveLength(2);
      expect(svg.querySelectorAll("polyline")).toHaveLength(2);
      expect(svg.querySelectorAll("line").length).toBeGreaterThan(40);
      expect(
        Array.from(svg.querySelectorAll("path")).some(
          (path) => path.getAttribute("d")?.includes("C"),
        ),
      ).toBe(true);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("a symbolic convergent sequence renders a finite prefix, its limit and a circular neighborhood", async () => {
    const result = await diagram({
      sub: convergentSequenceNeighborhood(),
      sty: sequenceConvergenceStyle({ count: 22 }),
      canvas: canvas(300, 270),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(24);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("one reusable snapshot renders circle, diamond and square neighborhoods repeatedly", async () => {
    const sub = reciprocalSequenceNeighborhoods();
    const sty = sequenceMetricComparisonStyle({ count: 24 });
    const results = await Promise.all([
      diagram({ sub, sty, canvas: canvas(320, 300), variation: "repeat" }),
      diagram({ sub, sty, canvas: canvas(320, 300), variation: "repeat" }),
    ]);
    try {
      for (const result of results) {
        const { svg } = await result.render();
        expect(svg.querySelectorAll("rect")).toHaveLength(1);
        expect(svg.querySelectorAll("polygon")).toHaveLength(1);
        expect(svg.querySelectorAll("circle")).toHaveLength(26);
        const terms = Array.from(svg.querySelectorAll("circle")).filter(
          (circle) => circle.getAttribute("r") === "1.8",
        );
        expect(
          new Set(terms.map((circle) => circle.getAttribute("cx"))).size,
        ).toBe(1);
        expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      }
    } finally {
      results.forEach((result) => result.discard());
    }
  });
});
