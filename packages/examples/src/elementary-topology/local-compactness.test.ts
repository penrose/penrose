// @vitest-environment jsdom

import {
  canvas,
  diagram,
  inOpenDisk,
  originAdjoinedSineMapValue,
  rationalRayGapWitness,
  sampleSineCurve,
  sineAccumulationTerm,
  sineCurveValue,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildClosedBoundedSetFigure,
  closedBoundedSet,
} from "./bounded-sets.js";
import {
  buildCompactNeighborhoodFigure,
  buildLocallyClosedSubspaceFigure,
  buildRationalNeighborhoodFigure,
  euclideanCompactNeighborhood,
  locallyClosedSubspace,
  rationalNoncompactNeighborhood,
} from "./local-compactness.js";
import {
  buildOriginAdjoinedSineFigure,
  originAdjoinedSineImage,
} from "./oscillating-sine-curve.js";

describe("closed bounded sets and local compactness", () => {
  test("a closed bounded set is contained in the open box and its compact product closure", async () => {
    const d = await diagram({
      sub: closedBoundedSet({ radius: 3 }),
      canvas: canvas(350, 200),
      sty: topology.style((ctx) => {
        const [a, open] = ctx.facts(topology.BoundedBy)[0];
        const box = ctx.entities(topology.ClosedProductRectangle)[0];
        const [interval] = ctx.entities(topology.ClosedInterval);
        const tau = ctx.facts(topology.TopologyOn)[0][0];
        expect(box.bounds).toEqual([-3, -3, 3, 3]);
        expect(ctx.test(topology.Subset, a, open)).toBe(true);
        expect(ctx.test(topology.CompactIn, a, tau)).toBe(true);
        expect(ctx.test(topology.ClosureOf, box, open, tau)).toBe(true);
        expect(ctx.test(topology.ProductOf, box, interval, interval)).toBe(
          true,
        );
        expect(ctx.entities(topology.ClosedRegion)).toHaveLength(1);
      }),
    });
    d.discard();
    for (const radius of [0, -1, NaN, Infinity])
      expect(() => closedBoundedSet({ radius })).toThrow();
  });

  test("the half-radius compact disk retains interior, closure and the full-radius containment chain", async () => {
    const d = await diagram({
      sub: euclideanCompactNeighborhood({ radius: 4 }),
      canvas: canvas(294, 246),
      sty: topology.style((ctx) => {
        const [point, u, inner, closed, tau] = ctx.facts(
          topology.CompactNeighborhoodWithin,
        )[0];
        const disk = ctx.entities(topology.ClosedDisk)[0];
        const outer = ctx
          .entities(topology.DiskNeighborhood)
          .find((n) => n !== inner)!;
        expect(disk.radius).toBe(2);
        expect(outer.radius).toBe(4);
        expect(ctx.test(topology.InteriorOf, inner, disk, tau)).toBe(true);
        expect(ctx.test(topology.ClosureOf, disk, inner, tau)).toBe(true);
        expect(ctx.test(topology.Subset, inner, closed)).toBe(true);
        expect(ctx.test(topology.Subset, closed, outer)).toBe(true);
        expect(ctx.test(topology.Subset, outer, u)).toBe(true);
        expect(ctx.test(topology.NotCompact, tau)).toBe(true);
        expect(ctx.test(topology.LocallyCompact, tau)).toBe(true);
        const x = ctx
          .entities(topology.CoordinatePoint)
          .find((p) => p === point)!;
        expect(inOpenDisk(outer, x.coordinates)).toBe(true);
        for (let i = 0; i < 100; i++) {
          const angle = (i * Math.PI) / 50;
          expect(
            inOpenDisk(outer, [
              disk.center[0] + disk.radius * Math.cos(angle),
              disk.center[1] + disk.radius * Math.sin(angle),
            ]),
          ).toBe(true);
        }
      }),
    });
    d.discard();
    expect(() =>
      euclideanCompactNeighborhood({ radius: Number.MIN_VALUE }),
    ).toThrow();
  });

  test("the rational example asserts assumed compactness, not a true compact neighborhood", async () => {
    const d = await diagram({
      sub: rationalNoncompactNeighborhood(),
      canvas: canvas(351, 106),
      sty: topology.style((ctx) => {
        const [cover, t, a, q, tau] = ctx.facts(topology.RationalRayCoverAt)[0];
        expect(t.expression).toBe("\\sqrt2/2");
        expect(ctx.test(topology.Outside, t, q)).toBe(true);
        expect(ctx.test(topology.AssumedCompactIn, a, tau)).toBe(true);
        expect(ctx.test(topology.CompactIn, a, tau)).toBe(false);
        expect(ctx.test(topology.HasNoFiniteSubcover, cover, a, tau)).toBe(
          true,
        );
        expect(ctx.test(topology.NotLocallyCompact, tau)).toBe(true);
        const x = ctx.entities(topology.DyadicRational)[0];
        const [relative, interval] = ctx.facts(topology.IntersectionOf)[0];
        expect(ctx.test(topology.Member, x, relative)).toBe(true);
        const ambient = ctx
          .entities(topology.OpenInterval)
          .find((i) => i === interval)!;
        expect(ctx.test(topology.IrrationalInInterval, t, ambient)).toBe(true);
        expect(x.coordinate).toBe(x.numerator / 2 ** x.exponent);
      }),
    });
    d.discard();
  });

  test("any finite selection of irrational-cut rays misses a rational in (a,b)", () => {
    const cut = Math.SQRT2 / 2;
    for (const bases of [
      [],
      [0.2, 0.4],
      [0.8, 0.9],
      [0.1, 0.6, 0.75, 0.9],
      [0.707, 0.708],
    ]) {
      const witness = rationalRayGapWitness(cut, [0, 1], bases);
      expect(witness.coordinate).toBe(witness.numerator / witness.denominator);
      expect(witness.coordinate).toBeGreaterThan(witness.gap[0]);
      expect(witness.coordinate).toBeLessThan(witness.gap[1]);
      for (const q of bases)
        expect(q < cut ? witness.coordinate < q : witness.coordinate > q).toBe(
          false,
        );
    }
    expect(() => rationalRayGapWitness(cut, [0, 0.5], [])).toThrow();
    expect(() => rationalRayGapWitness(cut, [0, 1], [cut])).toThrow();
  });

  test("the relative compact closure and ambient closure use distinct topologies and the same entities", async () => {
    const d = await diagram({
      sub: locallyClosedSubspace(),
      canvas: canvas(239, 218),
      sty: topology.style((ctx) => {
        const [y, space, tau] = ctx.facts(topology.LocallyClosedIn)[0];
        const tauY = ctx
          .facts(topology.SubspaceTopologyOf)
          .find(([, s]) => s === y)![0];
        const [relative, subspace, ambient] = ctx
          .facts(topology.IntersectionOf)
          .find(([s]) => s !== y)!;
        expect(subspace).toBe(y);
        const closure = ctx
          .facts(topology.ClosureOf)
          .find(([, s, t]) => s === relative && t === tauY)![0];
        expect(ctx.test(topology.CompactIn, closure, tauY)).toBe(true);
        expect(ctx.test(topology.ClosedIn, closure, tau)).toBe(true);
        expect(ctx.test(topology.ClosureOf, closure, relative, tau)).toBe(true);
        expect(
          ctx.test(
            topology.OpenIn,
            ctx.entities(topology.Neighborhood).find((n) => n === ambient)!,
            tau,
          ),
        ).toBe(true);
        expect(ctx.test(topology.T2, tau)).toBe(true);
        expect(
          ctx.facts(topology.TopologyOn).some(([, s]) => s === space),
        ).toBe(true);
        const [, globalOpen, closureY] = ctx
          .facts(topology.IntersectionOf)
          .find(([s]) => s === y)!;
        expect(
          ctx.test(
            topology.ClosureOf,
            ctx.entities(topology.ClosedSubspace).find((s) => s === closureY)!,
            y,
            tau,
          ),
        ).toBe(true);
        expect(
          ctx.entities(topology.OpenSet).some((s) => s === globalOpen),
        ).toBe(true);
      }),
    });
    d.discard();
  });
});

describe("origin-adjoined oscillating sine image", () => {
  test("the map has only the isolated -1 branch and the positive graph branch", () => {
    expect(originAdjoinedSineMapValue(-1)).toEqual([0, 0]);
    for (const x of [0.001, 0.05, 0.5, 1, 10])
      expect(originAdjoinedSineMapValue(x)).toEqual([x, Math.sin(1 / x)]);
    for (const x of [-2, -0.5, 0, NaN, Infinity])
      expect(() => originAdjoinedSineMapValue(x)).toThrow();
    expect(() => sineCurveValue(0.1, 0)).toThrow();
  });

  test("a fixed nonzero height yields genuine graph points tending to a missing vertical-axis point", () => {
    for (const height of [-1, -0.5, 0.25, 1]) {
      let previous = Infinity;
      for (const n of [1, 2, 10, 100]) {
        const [x, y] = sineAccumulationTerm(height, n);
        expect(x).toBeGreaterThan(0);
        expect(x).toBeLessThan(previous);
        previous = x;
        expect(y).toBe(height);
        expect(Math.sin(1 / x)).toBeCloseTo(height, 10);
        expect(Math.hypot(x, y)).toBeGreaterThan(0);
      }
    }
    for (const height of [0, 2, NaN])
      expect(() => sineAccumulationTerm(height, 1)).toThrow();
  });
  test("arbitrarily small origin balls admit tails with a nonzero missing ambient limit", () => {
    for (const rho of [0.001, 0.01, 0.1, 1, 2]) {
      const height = Math.min(rho / 2, 0.5);
      const n = Math.ceil(1 / rho);
      const point = sineAccumulationTerm(height, n);
      expect(Math.hypot(...point)).toBeLessThan(rho);
      expect(point[1]).toBeGreaterThan(0);
      expect(Math.sin(1 / point[0])).toBeCloseTo(height, 9);
      // The limit (0,height) is neither a positive-x graph point nor the adjoined origin.
      expect(height).not.toBe(0);
    }
  });

  test("phase sampling resolves oscillation and never adds a vertical segment or the origin", () => {
    const points = sampleSineCurve({ frequency: 1, amplitude: 1 });
    expect(points.length).toBeGreaterThan(2000);
    expect(points[0][0]).toBeCloseTo(0.0015);
    expect(points[points.length - 1][0]).toBeCloseTo(0.17);
    for (let i = 0; i < points.length; i++) {
      const [x, y] = points[i];
      expect(x).toBeGreaterThan(0);
      expect(y).toBeCloseTo(Math.sin(1 / x), 12);
      if (i) {
        expect(x).toBeGreaterThan(points[i - 1][0]);
        expect(1 / points[i - 1][0] - 1 / x).toBeLessThanOrEqual(
          Math.PI / 12 + 1e-10,
        );
      }
    }
    expect(() =>
      sampleSineCurve({ frequency: 1, amplitude: 1 }, { xMin: 0 }),
    ).toThrow();
    expect(() =>
      sampleSineCurve({ frequency: 1, amplitude: 1 }, { xMin: 1e-12 }),
    ).toThrow();
  });

  test("the image is a continuous bijection but not an open map; annotations remain outside Y", async () => {
    const d = await diagram({
      sub: originAdjoinedSineImage(),
      canvas: canvas(555, 195),
      sty: topology.style((ctx) => {
        const f = ctx.entities(topology.OscillatingSineMap)[0];
        const [y, graph, origin] = ctx.facts(topology.SineCurveImageOf)[0];
        const tauX = ctx.facts(topology.ContinuousMap)[0][1],
          tauY = ctx.facts(topology.ContinuousMap)[0][2];
        expect(ctx.test(topology.OneToOne, f)).toBe(true);
        expect(ctx.test(topology.Onto, f, y)).toBe(true);
        expect(ctx.test(topology.NotOpenMap, f, tauX, tauY)).toBe(true);
        expect(ctx.test(topology.Member, origin, y)).toBe(true);
        expect(ctx.test(topology.Outside, origin, graph)).toBe(true);
        expect(
          ctx.test(topology.FailsLocalCompactnessAt, origin, y, tauY),
        ).toBe(true);
        for (const p of ctx
          .entities(topology.CoordinatePoint)
          .filter((p) => p !== origin && p.coordinates[0] === 0)) {
          expect(ctx.test(topology.Outside, p, y)).toBe(true);
          expect(ctx.test(topology.Member, p, y)).toBe(false);
        }
        const [source] = ctx.entities(topology.ClopenSingleton);
        const [positive] = ctx.entities(topology.ClopenHalfLine);
        expect(ctx.test(topology.OpenIn, source, tauX)).toBe(true);
        expect(ctx.test(topology.ClosedIn, source, tauX)).toBe(true);
        expect(ctx.test(topology.OpenIn, positive, tauX)).toBe(true);
        expect(ctx.test(topology.ClosedIn, positive, tauX)).toBe(true);
      }),
    });
    d.discard();
  });
});

test("five source views render native shapes and labels, with no embedded image wrapper", async () => {
  for (const [build, title, kind] of [
    [buildClosedBoundedSetFigure, "bounded.compact-box", "path"],
    [
      buildCompactNeighborhoodFigure,
      "local.compact-half-radius-disk",
      "circle",
    ],
    [buildRationalNeighborhoodFigure, "rational.interior-brace", "path"],
    [buildLocallyClosedSubspaceFigure, "local.ambient-space", "ellipse"],
    [buildOriginAdjoinedSineFigure, "sine.positive-graph", "path"],
  ] as const) {
    const d = await build();
    try {
      const { svg } = await d.render();
      expect(svg.querySelector("image")).toBeNull();
      const target = Array.from(svg.querySelectorAll("title")).find(
        (t) => t.textContent === title,
      )?.parentElement;
      const rendered = target?.matches(kind)
        ? target
        : target?.querySelector(kind);
      expect(rendered).not.toBeNull();
      expect(svg.querySelectorAll("[data-tex]").length).toBeGreaterThan(2);
      if (build === buildOriginAdjoinedSineFigure) {
        const graph = rendered!;
        expect(
          (graph.getAttribute("d")!.match(/L/g) ?? []).length,
        ).toBeGreaterThan(2000);
      }
    } finally {
      d.discard();
    }
  }
});

test("the canonical rational scene is repeatable, and final render options enable seeded interactivity", async () => {
  const [left, right] = await Promise.all([
    buildRationalNeighborhoodFigure(),
    buildRationalNeighborhoodFigure(),
  ]);
  const interactive = await buildCompactNeighborhoodFigure({
    variation: "compact-neighborhood-interactive",
    interactive: true,
  });
  try {
    expect((await left.render()).svg.outerHTML).toBe(
      (await right.render()).svg.outerHTML,
    );
    expect(interactive.getDraggingConstraints().size).toBeGreaterThan(0);
  } finally {
    left.discard();
    right.discard();
    interactive.discard();
  }
});

/** Inspect actual emitted geometry rather than trusting declared membership facts. */
function renderedShape(
  svg: SVGSVGElement,
  title: string,
  kind: string,
): Element {
  const parent = Array.from(svg.querySelectorAll("title")).find(
    (t) => t.textContent === title,
  )?.parentElement;
  const shape = parent?.matches(kind) ? parent : parent?.querySelector(kind);
  if (!shape) throw new Error(`Missing native ${kind} ${title}`);
  return shape;
}
test("rendered compact neighborhoods and relative subspace neighborhoods contain their marked points", async () => {
  const d = await buildCompactNeighborhoodFigure(),
    relative = await buildLocallyClosedSubspaceFigure();
  try {
    const { svg } = await d.render();
    const disk = renderedShape(svg, "local.compact-half-radius-disk", "circle"),
      outer = renderedShape(svg, "local.full-radius-neighborhood", "circle"),
      point = renderedShape(svg, "local.point", "circle");
    const num = (e: Element, k: string) => Number(e.getAttribute(k));
    expect(num(disk, "r") * 2).toBe(num(outer, "r"));
    expect(num(disk, "cx")).toBe(num(outer, "cx"));
    expect(num(disk, "cy")).toBe(num(outer, "cy"));
    expect(
      Math.hypot(
        num(point, "cx") - num(disk, "cx"),
        num(point, "cy") - num(disk, "cy"),
      ),
    ).toBeLessThan(num(disk, "r"));
    const target = (await relative.render()).svg;
    const v = renderedShape(
        target,
        "local.ambient-open-neighborhood",
        "circle",
      ),
      y = renderedShape(target, "local.subspace-point", "circle");
    expect(
      Math.hypot(num(y, "cx") - num(v, "cx"), num(y, "cy") - num(v, "cy")),
    ).toBeLessThan(num(v, "r"));
    const removed = renderedShape(target, "local.removed-complement", "path");
    const values = (
      removed
        .getAttribute("d")!
        .match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
    ).map(Number);
    const top = Math.min(...values.filter((_, i) => i % 2 === 1));
    expect(num(y, "cy")).toBeLessThan(top);
  } finally {
    d.discard();
    relative.discard();
  }
});

test("the emitted sine paths stay strictly to the right of the origin and inside amplitude guides", async () => {
  const d = await buildOriginAdjoinedSineFigure();
  try {
    const { svg } = await d.render();
    const origin = renderedShape(svg, "sine.isolated-origin-image", "circle");
    const ox = Number(origin.getAttribute("cx")),
      oy = Number(origin.getAttribute("cy"));
    for (const title of [
      "sine.positive-graph",
      "sine.resolved-positive-graph",
    ]) {
      const path = renderedShape(svg, title, "path");
      expect(path.getAttribute("fill-opacity")).toBe("0");
      expect(path.getAttribute("d")).not.toContain("Z");
      const p = (
        path
          .getAttribute("d")!
          .match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
      ).map(Number);
      for (let i = 0; i < p.length; i += 2) {
        expect(p[i]).toBeGreaterThan(ox);
        expect(Math.abs(p[i + 1] - oy)).toBeLessThanOrEqual(56.5 + 1e-8);
      }
    }
  } finally {
    d.discard();
  }
});
