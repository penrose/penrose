// @vitest-environment jsdom

import {
  canvas,
  diagram,
  inDeletedReciprocalNeighborhood,
  isReciprocalSequenceTerm,
  reciprocalNeighborhoodOverlap,
  reciprocalSequenceTerm,
  schematicReciprocalCoordinate,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildClosureNeighborhoodFigure,
  buildDeletedReciprocalFigure,
  buildSetSeparationFigure,
  buildTopologicalPointSeparationFigure,
  deletedReciprocalCounterexample,
  disjointSetNeighborhoods,
  nestedClosureNeighborhoods,
  topologicalPointClosedSeparation,
} from "./separation-axioms.js";

describe("the book's separation definitions and infinite counterexample", () => {
  test("generic set separation does not assume closed A and B", async () => {
    const result = await diagram({
      sub: disjointSetNeighborhoods(),
      canvas: canvas(350, 180),
      sty: topology.style((ctx) => {
        expect(ctx.entities(topology.ClosedSet)).toHaveLength(0);
        const [a, b, u, v, tau] = ctx.facts(topology.SetSeparation)[0];
        expect(ctx.test(topology.Subset, a, u)).toBe(true);
        expect(ctx.test(topology.Subset, b, v)).toBe(true);
        expect(ctx.test(topology.Disjoint, u, v)).toBe(true);
        expect(ctx.test(topology.OpenIn, u, tau)).toBe(true);
        expect(ctx.test(topology.OpenIn, v, tau)).toBe(true);
      }),
    });
    result.discard();
  });

  test("the author's T3 witness asserts neither T1 nor Regular", async () => {
    const result = await diagram({
      sub: topologicalPointClosedSeparation(),
      canvas: canvas(310, 185),
      sty: topology.style((ctx) => {
        expect(ctx.facts(topology.T3)).toHaveLength(1);
        expect(ctx.facts(topology.T1)).toHaveLength(0);
        expect(ctx.facts(topology.Regular)).toHaveLength(0);
        expect(ctx.entities(topology.CoordinatePoint)).toHaveLength(0);
        const [x, f, u, v, tau] = ctx.facts(
          topology.PointClosedSetSeparation,
        )[0];
        expect(ctx.test(topology.Outside, x, f)).toBe(true);
        expect(ctx.test(topology.ClosedIn, f, tau)).toBe(true);
        expect(ctx.test(topology.Member, x, u)).toBe(true);
        expect(ctx.test(topology.Subset, f, v)).toBe(true);
        expect(ctx.test(topology.Disjoint, u, v)).toBe(true);
      }),
    });
    result.discard();
  });

  test("the closure witness retains the entire proof inclusion chain and complements", async () => {
    const result = await diagram({
      sub: nestedClosureNeighborhoods(),
      canvas: canvas(275, 210),
      sty: topology.style((ctx) => {
        const [v, clV, u, x, tau] = ctx.facts(
          topology.NeighborhoodClosureWithin,
        )[0];
        const complements = ctx.facts(topology.ComplementOf);
        const xU = complements.find(([, s]) => s === u)?.[0];
        const [xW, w] = complements.find(([, s]) => s !== u)!;
        expect(ctx.test(topology.ClosureOf, clV, v, tau)).toBe(true);
        expect(ctx.test(topology.Subset, v, clV)).toBe(true);
        expect(ctx.test(topology.Subset, clV, xW)).toBe(true);
        expect(ctx.test(topology.Subset, xW, u)).toBe(true);
        expect(xU).toBeDefined();
        expect(ctx.test(topology.Subset, xU!, w)).toBe(true);
        expect(ctx.test(topology.Disjoint, v, w)).toBe(true);
        expect(ctx.test(topology.Member, x, v)).toBe(true);
      }),
    });
    result.discard();
  });

  test("zero belongs to the deleted neighborhood while every named reciprocal term is excluded", () => {
    expect(inDeletedReciprocalNeighborhood(0.27, 1, 0)).toBe(true);
    for (let n = 1; n <= 300; n++) {
      const term = reciprocalSequenceTerm(1, n);
      expect(isReciprocalSequenceTerm(1, term)).toBe(true);
      expect(inDeletedReciprocalNeighborhood(0.27, 1, term)).toBe(false);
    }
    expect(inDeletedReciprocalNeighborhood(0.27, 1, 0.27)).toBe(false);
    expect(inDeletedReciprocalNeighborhood(0.27, 1, -0.27)).toBe(false);
    expect(inDeletedReciprocalNeighborhood(0.27, 1, -0.13)).toBe(true);
    expect(() => reciprocalSequenceTerm(1, 0)).toThrow("positive safe integer");
  });

  test("an arbitrary positive local interval around a sufficiently small term intersects V outside F", () => {
    for (const epsilon of [1e-8, 1e-6, 0.018, 0.5, 10]) {
      const witness = reciprocalNeighborhoodOverlap(0.27, 1, 6, epsilon);
      expect(witness.point).toBeGreaterThan(1 / 7);
      expect(witness.point).toBeLessThan(1 / 6);
      expect(witness.point).toBeGreaterThan(witness.interval[0]);
      expect(witness.point).toBeLessThan(witness.interval[1]);
      expect(inDeletedReciprocalNeighborhood(0.27, 1, witness.point)).toBe(
        true,
      );
      expect(isReciprocalSequenceTerm(1, witness.point)).toBe(false);
    }
    expect(() => reciprocalNeighborhoodOverlap(0.1, 1, 6, 0.02)).toThrow(
      "inside",
    );
    expect(() => deletedReciprocalCounterexample({ radius: 0 })).toThrow();
  });

  test("the counterexample declares infinite F, its relative closedness, and a valid overlap witness", async () => {
    const result = await diagram({
      sub: deletedReciprocalCounterexample(),
      canvas: canvas(340, 75),
      sty: topology.style((ctx) => {
        const [v, zero, f, tau] = ctx.facts(
          topology.DeletedReciprocalNeighborhoodOf,
        )[0];
        expect(f.coefficient).toBe(1);
        expect("terms" in f).toBe(false);
        expect(ctx.test(topology.ClosedIn, f, tau)).toBe(true);
        expect(ctx.test(topology.T2, tau)).toBe(true);
        expect(ctx.test(topology.NotT3, tau)).toBe(true);
        expect(ctx.test(topology.T3, tau)).toBe(false);
        expect(ctx.test(topology.Outside, zero, f)).toBe(true);
        const [witness, u] = ctx.facts(topology.IntersectionWitness)[0];
        const point = ctx
          .entities(topology.RealPoint)
          .find((p) => p === witness)!;
        expect(
          inDeletedReciprocalNeighborhood(
            v.radius,
            f.coefficient,
            point.coordinate,
          ),
        ).toBe(true);
        expect(ctx.test(topology.Member, point, u)).toBe(true);
        expect(ctx.test(topology.Member, point, v)).toBe(true);
      }),
    });
    result.discard();
  });

  test("the source's unscaled coordinate chart preserves order and the three exterior terms", () => {
    let previous = -Infinity;
    for (let i = -100; i <= 1000; i++) {
      const position = schematicReciprocalCoordinate(i / 1000);
      expect(position).toBeGreaterThan(previous);
      previous = position;
    }
    const boundary = schematicReciprocalCoordinate(0.27);
    expect(
      [1, 0.5, 1 / 3].every((x) => schematicReciprocalCoordinate(x) > boundary),
    ).toBe(true);
    expect(schematicReciprocalCoordinate(0.25)).toBeLessThan(boundary);
  });
});

describe("actual Penrose rendering of Figures 5.1–5.4", () => {
  test("the reusable factory accepts native interactive label layout and a caller seed", async () => {
    const result = await buildSetSeparationFigure({
      variation: "chapter-five-layout-test",
      interactive: true,
    });
    try {
      expect(result.getDraggingConstraints().size).toBe(4);
      const { svg } = await result.render();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("generic set and point separation reuse one native TSX policy", async () => {
    for (const [build, regions, points] of [
      [buildSetSeparationFigure, ["separation.set-a", "separation.set-b"], 0],
      [buildTopologicalPointSeparationFigure, ["separation.closed-f"], 1],
    ] as const) {
      const result = await build();
      try {
        const { svg } = await result.render();
        for (const region of regions)
          expect(
            svg.querySelector(`path[aria-label="${region}"]`),
          ).not.toBeNull();
        expect(svg.querySelectorAll("circle")).toHaveLength(points);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      } finally {
        result.discard();
      }
    }
  });

  test("W is a native path with a true unfilled complement hole", async () => {
    const result = await buildClosureNeighborhoodFigure();
    try {
      const { svg } = await result.render();
      const path = svg.querySelector('path[aria-label="closure.open-w"]');
      const commands = path?.getAttribute("d") ?? "";
      expect(commands.match(/M/g)).toHaveLength(2);
      expect(commands.match(/Z/g)).toHaveLength(2);
      expect(
        svg.querySelector('path[aria-label="closure.open-v"]'),
      ).not.toBeNull();
      expect(svg.querySelectorAll("circle")).toHaveLength(1);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("the deleted neighborhood shows a finite representative prefix, not a raster wrapper", async () => {
    const result = await buildDeletedReciprocalFigure();
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(33);
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(
        svg.querySelector('path[aria-label="deleted.zero-neighborhood"]'),
      ).not.toBeNull();
      expect(
        svg.querySelector('path[aria-label="deleted.u-component"]'),
      ).not.toBeNull();
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });
});
