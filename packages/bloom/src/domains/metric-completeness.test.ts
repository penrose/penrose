import { expect, test } from "vitest";
import {
  circleDiameter,
  geometricCauchyIndex,
  geometricSequenceBound,
  geometricSequenceTerm,
  nestedBisectionBounds,
  rectangleDiameter,
} from "./metric-completeness.js";
import { pointSetTopology as topology } from "./point-set-topology.js";

test("the infinite geometric tail has a uniform Cauchy witness despite an arbitrary finite prefix", () => {
  for (const ratio of [-0.8, 0, 0.97]) {
    const sequence = { prefix: [-120, 40], initial: 3, limit: 0.44, ratio };
    expect(geometricSequenceTerm(sequence, 1)).toBe(-120);
    expect(geometricSequenceTerm(sequence, 3)).toBe(3);
    const bound = geometricSequenceBound(sequence);
    for (let n = 1; n < 250; n++)
      expect(Math.abs(geometricSequenceTerm(sequence, n))).toBeLessThanOrEqual(
        bound,
      );
    for (const epsilon of [0.2, 0.0001]) {
      const M = geometricCauchyIndex(sequence, epsilon);
      for (const k of [M + 1, M + 2, M + 17])
        for (const m of [M + 1, M + 4, M + 500])
          expect(
            Math.abs(
              geometricSequenceTerm(sequence, k) -
                geometricSequenceTerm(sequence, m),
            ),
          ).toBeLessThan(epsilon);
    }
  }
  expect(() =>
    geometricSequenceTerm({ prefix: [], initial: 1, limit: 0, ratio: 1 }, 1),
  ).toThrow();
});

test("nested bisection keeps its anchor, halves its width, and rejects exhausted precision", () => {
  let parent: readonly [number, number] = [-1, 1];
  for (let n = 1; n <= 14; n++) {
    const child = nestedBisectionBounds([-1, 1], 0.44, n);
    expect(child[0]).toBeGreaterThanOrEqual(parent[0]);
    expect(child[1]).toBeLessThanOrEqual(parent[1]);
    expect(child[1] - child[0]).toBeCloseTo(2 / 2 ** n, 14);
    expect(child[0]).toBeLessThanOrEqual(0.44);
    expect(child[1]).toBeGreaterThanOrEqual(0.44);
    parent = child;
  }
  expect(nestedBisectionBounds([0, 1], 0.5, 1)).toEqual([0, 0.5]);
  expect(() => nestedBisectionBounds([0, 1], 2, 1)).toThrow();
  expect(() => nestedBisectionBounds([0, 1], 0.44, 80)).toThrow(/precision/);
});

test("Euclidean diameters are determined by radius and displacement, independent of translation", () => {
  expect(circleDiameter(1)).toBe(2);
  expect(rectangleDiameter([-2, -1, 2, 1])).toBeCloseTo(Math.sqrt(20), 12);
  expect(rectangleDiameter([5, 7, 9, 9])).toBeCloseTo(Math.sqrt(20), 12);
  expect(() => rectangleDiameter([1, 0, 0, 1])).toThrow();
});

test("abstract metric neighborhoods and temporary cover hypotheses have no drawing data or false cover assertions", () => {
  const s = topology.substance(),
    X = s.Set({ label: "X" }),
    D = s.Metric({ label: "D" }),
    tau = s.Topology();
  const b = s.Point({ label: "b" }),
    N = s.MetricNeighborhood({ radius: 0.4, label: "N(b,p)" });
  const family = s.CountableSetFamily({ label: "\\{A_n\\}" });
  s.MetricOn(D, X);
  s.MetricInducesTopology(D, tau);
  s.CompleteMetricSpace(X, D);
  s.MetricNeighborhoodAt(N, b, D);
  s.NeighborhoodOf(N, b);
  s.OpenIn(N, tau);
  s.Hypothesis(s.FamilyUnionIs.expression(family, X));
  const sub = s.make();
  expect(N).not.toHaveProperty("center");
  expect(b).not.toHaveProperty("coordinates");
  expect(
    sub.propositions.some((p) => p.predicate === topology.FamilyUnionIs),
  ).toBe(false);
  expect(
    sub.propositions.filter((p) => p.predicate === topology.Hypothesis),
  ).toHaveLength(1);
});
