import { expect, test } from "vitest";
import {
  partitionMesh,
  partitionRefines,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildIntervalPartitionFigure,
  buildNonHausdorffNetFigure,
  intervalPartitionSubstance,
  nonHausdorffSelectionNetSubstance,
} from "../examples/convergence.js";

test("finite refinement retains endpoints and cannot increase mesh", () => {
  for (const coarser of [
    [0, 1],
    [-2, -1, 3],
    [0, 0.125, 0.75, 1],
  ]) {
    const finer = [
      ...coarser,
      ...coarser.slice(1).map((p, i) => (p + coarser[i]) / 2),
    ].sort((a, b) => a - b);
    expect(partitionRefines(finer, coarser)).toBe(true);
    expect(partitionMesh(finer)).toBeLessThanOrEqual(partitionMesh(coarser));
  }
  expect(partitionRefines([0, 0.4, 1], [0, 0.5, 1])).toBe(false);
  expect(partitionRefines([0, 0.5, 2], [0, 1])).toBe(false);
  for (const invalid of [
    [],
    [1],
    [0, 0, 1],
    [0, 2, 1],
    [0, Infinity],
    [-Number.MAX_VALUE, Number.MAX_VALUE],
  ])
    expect(() => partitionMesh(invalid)).toThrow();
});

test("a finite partition is one member of a general directed family with the source order explicit", () => {
  const sub = intervalPartitionSubstance([-2, -1, 0, 3]);
  const partition = sub.propositions.find(
    (p) => p.predicate === topology.PartitionOf,
  )!.args[0];
  const family = sub.propositions.find(
    (p) => p.predicate === topology.PartitionsOfInterval,
  )!.args[0];
  expect(family).toHaveProperty("order", "finer-first");
  expect(partition).toHaveProperty("points", [-2, -1, 0, 3]);
  expect(Object.isFrozen(partition)).toBe(true);
  expect(partition).not.toHaveProperty("center");
});

test("the all-neighborhood selector and the finite depicted value remain distinct", () => {
  const sub = nonHausdorffSelectionNetSubstance();
  const limits = sub.propositions.filter(
    (p) => p.predicate === topology.NetConvergesTo,
  );
  expect(limits).toHaveLength(2);
  expect(limits[0].args[0]).toBe(limits[1].args[0]);
  expect(limits[0].args[1]).not.toBe(limits[1].args[1]);
  expect(limits[0].args[2]).toBe(limits[1].args[2]);
  const selected = sub.propositions.find(
    (p) => p.predicate === topology.SelectedIntersectionValue,
  )!.args;
  for (const neighborhood of selected.slice(1, 3))
    expect(
      sub.propositions.some(
        (p) =>
          p.predicate === topology.Member &&
          p.args[0] === selected[3] &&
          p.args[1] === neighborhood,
      ),
    ).toBe(true);
  expect(
    sub.propositions.filter(
      (p) => p.predicate === topology.ChoosesFromIntersections,
    ),
  ).toHaveLength(1);
  expect(sub.entities.every((e) => !("coordinates" in e))).toBe(true);
});

test("both source views render native geometry with draggable annotations when requested", async () => {
  for (const [factory, circles] of [
    [buildIntervalPartitionFigure, 7],
    [buildNonHausdorffNetFigure, 3],
  ] as const) {
    const drawing = await factory({ interactive: { jitter: 0 } });
    try {
      for (let i = 0; await drawing.optimizationStep(); i++)
        if (i > 1000) throw new Error("Convergence diagram did not optimize");
      const { svg } = await drawing.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(circles);
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|Infinity|undefined/);
      expect(drawing.getDraggingConstraints().size).toBeGreaterThan(0);
      expect(
        new DOMParser()
          .parseFromString(
            new XMLSerializer().serializeToString(svg),
            "image/svg+xml",
          )
          .querySelector("parsererror"),
      ).toBeNull();
    } finally {
      drawing.discard();
    }
  }
});
