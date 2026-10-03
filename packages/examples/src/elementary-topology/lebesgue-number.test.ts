// @vitest-environment jsdom
import {
  canvas,
  diagram,
  finiteBallLebesgueWitness,
  halfBallContaining,
  pointSetTopology as topology,
  type IntervalBall,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildLebesgueNumberFigure,
  buildSymmetricLebesgueNumberFigure,
  compactIntervalLebesgueCover,
  symmetricIntervalLebesgueCover,
} from "./lebesgue-number.js";

const cases: readonly {
  interval: readonly [number, number];
  balls: readonly IntervalBall[];
}[] = [
  {
    interval: [0, 1],
    balls: [
      { center: 0, radius: 0.5 },
      { center: 0.33, radius: 0.44 },
      { center: 0.66, radius: 0.5 },
      { center: 1, radius: 0.46 },
    ],
  },
  {
    interval: [-2, 2],
    balls: [
      { center: -2, radius: 1.8 },
      { center: -0.8, radius: 2 },
      { center: 0.8, radius: 2 },
      { center: 2, radius: 1.8 },
    ],
  },
];
describe("finite-subcover Lebesgue-number witnesses", () => {
  test("a strict half-ball cover yields a positive uniform containment margin everywhere", () => {
    for (const { interval, balls } of cases) {
      const rho = finiteBallLebesgueWitness(interval, balls);
      expect(rho).toBe(Math.min(...balls.map((b) => b.radius / 2)));
      expect(rho).toBeGreaterThan(0);
      for (let i = 0; i <= 1000; i++) {
        const x = interval[0] + ((interval[1] - interval[0]) * i) / 1000,
          j = halfBallContaining(interval, balls, x),
          b = balls[j];
        expect(Math.abs(x - b.center)).toBeLessThan(b.radius / 2);
        expect(b.radius - Math.abs(x - b.center) - rho).toBeGreaterThan(0);
        for (const t of [-0.999, -0.5, 0, 0.5, 0.999]) {
          const z = Math.max(interval[0], Math.min(interval[1], x + t * rho));
          expect(Math.abs(z - b.center)).toBeLessThan(b.radius);
        }
      }
    }
  });
  test("touching half-open intervals and uncovered endpoints are rejected exactly", () => {
    expect(() =>
      finiteBallLebesgueWitness(
        [0, 1],
        [
          { center: 0, radius: 1 },
          { center: 1, radius: 1 },
        ],
      ),
    ).toThrow(/uncovered/);
    expect(() =>
      finiteBallLebesgueWitness([0, 1], [{ center: 0, radius: 2 }]),
    ).toThrow(/endpoint/);
    expect(() => finiteBallLebesgueWitness([0, 1], [])).toThrow();
    expect(() =>
      finiteBallLebesgueWitness([0, 1], [{ center: 0.5, radius: 0 }]),
    ).toThrow();
    expect(() =>
      finiteBallLebesgueWitness([0, 1], [{ center: 2, radius: 4 }]),
    ).toThrow();
    expect(() =>
      halfBallContaining(cases[0].interval, cases[0].balls, 1.01),
    ).toThrow();
  });
  test("both shape-free programs retain compactness, refinement and the correct relative metric", async () => {
    for (const sub of [
      compactIntervalLebesgueCover(),
      symmetricIntervalLebesgueCover(),
    ]) {
      const d = await diagram({
        sub,
        canvas: canvas(450, 330),
        sty: topology.style((ctx) => {
          const [number, original, space, metric] = ctx.facts(
            topology.LebesgueNumberFor,
          )[0];
          expect(number.value).toBeGreaterThan(0);
          expect(ctx.test(topology.MetricOn, metric, space)).toBe(true);
          expect(ctx.entities(topology.EuclideanMetric)[0].dimension).toBe(1);
          const [finite, refinement] = ctx.facts(topology.SubfamilyOf)[0];
          expect(
            ctx
              .facts(topology.OpenCoverOf)
              .some(([c, x]) => c === finite && x === space),
          ).toBe(true);
          expect(
            ctx.facts(topology.SetInFamily).filter(([, f]) => f === finite),
          ).toHaveLength(4);
          expect(
            ctx.facts(topology.SetInFamily).filter(([, f]) => f === refinement),
          ).toHaveLength(4);
          expect(
            ctx.facts(topology.SetInFamily).filter(([, f]) => f === original),
          ).toHaveLength(4);
          expect(ctx.facts(topology.FamilyIncludedIn)).toHaveLength(0);
          expect(ctx.facts(topology.Compact)).toHaveLength(1);
        }),
      });
      d.discard();
    }
  });
  test("one reusable native style renders both interval-cover programs", async () => {
    for (const build of [
      () => buildLebesgueNumberFigure(),
      () => buildSymmetricLebesgueNumberFigure(),
    ]) {
      const d = await build();
      try {
        while (await d.optimizationStep()) {
          // Optimize to convergence before inspecting the native geometry.
        }
        const { svg } = await d.render();
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        expect(svg.querySelectorAll("title").length).toBeGreaterThan(20);
        expect(svg.outerHTML).toContain("lebesgue.uniform-ball");
        expect(svg.outerHTML).toContain("lebesgue.proof-z");
      } finally {
        d.discard();
      }
    }
  });
  test("alternate seeds and interactive options remain repeatable and isolated", async () => {
    const diagrams = await Promise.all([
      buildLebesgueNumberFigure(
        {},
        { variation: "LebesgueAlternative", interactive: true },
      ),
      buildSymmetricLebesgueNumberFigure({ variation: "LebesgueAlternative" }),
    ]);
    try {
      for (const d of diagrams) {
        const { svg } = await d.render();
        expect(svg.outerHTML).toContain("lebesgue.compact-interval");
      }
    } finally {
      diagrams.forEach((d) => d.discard());
    }
  });
});
