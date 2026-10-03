// @vitest-environment jsdom

import {
  canvas,
  diagram,
  inEuclideanOpenBox,
  inOpenDisk,
  interpolateEuclideanSegment,
  reciprocalCircleRadius,
  reciprocalSpiralPoint,
  sampleReciprocalSpiral,
  spiralDisplayRadius,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  accumulatingCircleComponents,
  buildAccumulatingComponentsFigure,
  buildConnectedProductSlicesFigure,
  buildConvexBoxFigure,
  buildDirectedClosedIntersectionFigure,
  buildMutuallySeparatedDisksFigure,
  buildPolygonalReachabilityFigure,
  buildSpiralClosureFigure,
  connectedProductSlices,
  convexBoxSegment,
  directedClosedIntersection,
  mutuallySeparatedDisks,
  polygonalReachability,
  reciprocalSpiralClosure,
} from "./connectedness-constructions.js";

describe("convex product neighborhoods and polygonal reachability", () => {
  test("interior segment interpolation stays strictly in a box in any finite dimension", () => {
    for (const dimension of [1, 2, 3, 5]) {
      const bounds = Array.from({ length: dimension }, () => [-2, 3] as const),
        a = Array.from(
          { length: dimension },
          (_, i) => -1 + i / (dimension + 1),
        ),
        b = Array.from(
          { length: dimension },
          (_, i) => 2 - i / (dimension + 1),
        );
      for (let i = 0; i <= 100; i++)
        expect(
          inEuclideanOpenBox(
            bounds,
            interpolateEuclideanSegment(a, b, i / 100),
          ),
        ).toBe(true);
    }
    expect(() => interpolateEuclideanSegment([0], [0, 1], 0.5)).toThrow();
    expect(() => inEuclideanOpenBox([[0, 0]], [0])).toThrow();
    expect(inEuclideanOpenBox([[0, 1]], [1])).toBe(false);
  });
  test("the source cube program uses genuine three-dimensional points and a complete interior segment", async () => {
    const d = await diagram({
      sub: convexBoxSegment(),
      canvas: canvas(183, 193),
      sty: topology.style((ctx) => {
        const box = ctx.entities(topology.EuclideanOpenBox)[0],
          [segment, x, y] = ctx.facts(topology.EuclideanSegmentBetween)[0];
        expect(x.coordinates).toHaveLength(3);
        expect(y.coordinates).toHaveLength(3);
        expect(ctx.test(topology.Convex, box)).toBe(true);
        expect(ctx.test(topology.Subset, segment, box)).toBe(true);
        expect(segment.endpoints[0]).toEqual(x.coordinates);
        expect(Object.isFrozen(box.bounds)).toBe(true);
      }),
    });
    d.discard();
  });
  test("the local segment extends the original path and lies entirely in the convex neighborhood", async () => {
    const d = await diagram({
      sub: polygonalReachability(),
      canvas: canvas(355, 187),
      sty: topology.style((ctx) => {
        const [reachable, u, region] = ctx.facts(
            topology.PolygonalReachableFrom,
          )[0],
          box = ctx.entities(topology.EuclideanOpenBox)[0];
        const paths = [...ctx.facts(topology.PolygonalPathBetween)].sort(
          ([a], [b]) => a.vertices.length - b.vertices.length,
        );
        const [original, , a] = paths[0],
          [extended, , b] = paths[1];
        expect(paths.every(([, p, , set]) => p === u && set === region)).toBe(
          true,
        );
        expect(extended.vertices.slice(0, -1)).toEqual(original.vertices);
        for (let i = 0; i <= 100; i++)
          expect(
            inEuclideanOpenBox(
              box.bounds,
              interpolateEuclideanSegment(
                a.coordinates,
                b.coordinates,
                i / 100,
              ),
            ),
          ).toBe(true);
        expect(ctx.test(topology.Member, b, reachable)).toBe(true);
      }),
    });
    d.discard();
  });
});

describe("mutual separation versus touching closures", () => {
  test("the contact point belongs to both closures and neither open disk", async () => {
    const d = await diagram({
      sub: mutuallySeparatedDisks(),
      canvas: canvas(346, 202),
      sty: topology.style((ctx) => {
        const [s, t, tau] = ctx.facts(topology.MutuallySeparated)[0],
          disks = ctx.entities(topology.OpenDisk),
          contact = ctx.entities(topology.CoordinatePoint)[0];
        expect(contact.coordinates).toEqual([1, 0]);
        for (const disk of disks) {
          expect(inOpenDisk(disk, contact.coordinates)).toBe(false);
          expect(ctx.test(topology.Outside, contact, disk)).toBe(true);
        }
        expect(ctx.test(topology.Disjoint, s, t)).toBe(true);
        expect(ctx.facts(topology.ClosureOf)).toHaveLength(2);
        expect(ctx.facts(topology.IntersectionOf)).toHaveLength(1);
        expect(ctx.facts(topology.Disconnected)).toHaveLength(1);
        expect(tau).toBe(ctx.facts(topology.TopologyOn)[0][0]);
      }),
    });
    d.discard();
  });
});

describe("connected spiral and its exact closure", () => {
  test("the reciprocal polar curve has both the origin and the unit circle as endpoint limits", () => {
    for (const t of [1.001, 1.01, 2, 10, 100]) {
      const p = reciprocalSpiralPoint(t);
      expect(Math.hypot(...p)).toBeCloseTo(1 - 1 / t, 12);
      expect(Math.hypot(...p)).toBeGreaterThan(0);
      expect(Math.hypot(...p)).toBeLessThan(1);
    }
    expect(Math.hypot(...reciprocalSpiralPoint(1 + 1e-8))).toBeLessThan(1e-7);
    expect(reciprocalSpiralPoint(2 * Math.PI * 1000)[0]).toBeCloseTo(1, 3);
    expect(() => reciprocalSpiralPoint(1)).toThrow();
    expect(() => reciprocalSpiralPoint(2, 0)).toThrow();
  });
  test("the source's omitted origin is retained in the mathematical closure", async () => {
    const d = await diagram({
      sub: reciprocalSpiralClosure(),
      canvas: canvas(225, 211),
      sty: topology.style((ctx) => {
        const [origin, curve] = ctx.facts(topology.SpiralInitialLimitOf)[0];
        const [closure, , tau] = ctx.facts(topology.ClosureOf)[0];
        expect(ctx.test(topology.Member, origin, closure)).toBe(true);
        expect(ctx.test(topology.Outside, origin, curve)).toBe(true);
        expect(ctx.test(topology.ConnectedIn, curve, tau)).toBe(true);
        const [, family] = ctx
          .facts(topology.UnionOf)
          .find(([set]) => set === closure)!;
        expect(
          ctx.facts(topology.SetInFamily).filter(([, f]) => f === family),
        ).toHaveLength(3);
      }),
    });
    d.discard();
  });
  test("the native spiral sampler stays inside the limit circle and excludes the origin", () => {
    const p = sampleReciprocalSpiral(1, 1);
    expect(p.length).toBeGreaterThan(300);
    for (const point of p) {
      expect(Math.hypot(...point)).toBeGreaterThan(0);
      expect(Math.hypot(...point)).toBeLessThan(1);
    }
    expect(() => sampleReciprocalSpiral(1, 1, 0)).toThrow();
  });
  test("the source's radial display chart preserves limits and strict radial order, with an identity metric option", () => {
    for (const limit of [0.5, 1, 3]) {
      expect(spiralDisplayRadius(0, limit)).toBe(0);
      expect(spiralDisplayRadius(limit, limit)).toBe(limit);
      let previous = 0;
      for (let i = 1; i <= 100; i++) {
        const r = (limit * i) / 100,
          display = spiralDisplayRadius(r, limit);
        expect(display).toBeGreaterThan(previous);
        expect(display).toBeLessThanOrEqual(limit);
        expect(spiralDisplayRadius(r, limit, 1)).toBeCloseTo(r, 12);
        previous = display;
      }
      for (const lambda of [1 + 1e-8, 2 * Math.PI * 10000]) {
        const r = Math.hypot(...reciprocalSpiralPoint(lambda, limit, limit));
        const display = spiralDisplayRadius(r, limit);
        expect(display).toBeGreaterThan(0);
        expect(display).toBeLessThan(limit);
        if (lambda < 2) expect(display).toBeLessThan(1e-20);
        else expect(display).toBeCloseTo(limit, 3);
      }
    }
    expect(() => spiralDisplayRadius(-1, 1)).toThrow();
    expect(() => spiralDisplayRadius(1, 1, 0)).toThrow();
  });
});

test("connected product programs keep the alleged separation nested and use intersecting coordinate fibers", async () => {
  const d = await diagram({
    sub: connectedProductSlices(),
    canvas: canvas(344, 229),
    sty: topology.style((ctx) => {
      expect(ctx.facts(topology.TopologicalSeparationOf)).toHaveLength(0);
      expect(
        ctx
          .facts(topology.Hypothesis)
          .some(([p]) => p.predicate === topology.TopologicalSeparationOf),
      ).toBe(true);
      const slices = ctx.entities(topology.AffineSubspace),
        c = ctx
          .entities(topology.CoordinatePoint)
          .find((p) => p.label === "(v_1,u_2)")!;
      expect(slices).toHaveLength(2);
      for (const slice of slices) {
        const [a, b, value] = slice.coefficients;
        expect(a * c.coordinates[0] + b * c.coordinates[1]).toBe(value);
        expect(ctx.test(topology.Member, c, slice)).toBe(true);
      }
      expect(ctx.facts(topology.ConnectedIn)).toHaveLength(2);
    }),
  });
  d.discard();
});

describe("components and unsplittable points", () => {
  test("the infinite circle family includes the zero-radius singleton and approaches both lines", () => {
    expect(reciprocalCircleRadius(1)).toBe(0);
    for (const rho of [0.1, 0.01, 0.001]) {
      const r = reciprocalCircleRadius(Math.ceil(2 / rho));
      expect(r).toBeLessThan(1);
      expect(1 - r).toBeLessThan(rho);
    }
    expect(() => reciprocalCircleRadius(0)).toThrow();
    expect(() => reciprocalCircleRadius(2.5)).toThrow();
  });
  test("the example is disconnected while the two line points cannot be split", async () => {
    const d = await diagram({
      sub: accumulatingCircleComponents(),
      canvas: canvas(345, 275),
      sty: topology.style((ctx) => {
        const [space, up, down, tau] = ctx.facts(
          topology.CannotSplitBetween,
        )[0];
        expect(ctx.test(topology.Disconnected, tau)).toBe(true);
        expect(ctx.test(topology.Connected, tau)).toBe(false);
        const lines = ctx.entities(topology.AffineSubspace);
        expect(ctx.test(topology.Member, up, lines[0])).toBe(true);
        expect(ctx.test(topology.Member, down, lines[1])).toBe(true);
        expect(
          lines.every((line) =>
            ctx.test(topology.ComponentOf, line, space, tau),
          ),
        ).toBe(true);
        const circles = ctx.facts(topology.CircleIndexedIn);
        expect(circles.find(([, i]) => i.coordinate === 1)![0].radius).toBe(0);
        const side = ctx
          .entities(topology.CoordinatePoint)
          .find((p) => p.label === "(1,0)")!;
        expect(ctx.test(topology.Outside, side, space)).toBe(true);
      }),
    });
    d.discard();
  });
});

test("a directed intersection program preserves compact Hausdorff premises and an assumed split only", async () => {
  const d = await diagram({
    sub: directedClosedIntersection(),
    canvas: canvas(354, 234),
    sty: topology.style((ctx) => {
      const [b, family] = ctx.facts(topology.IntersectionOfFamily)[0];
      expect(family).toBe(ctx.entities(topology.ClosedDirectedFamily)[0]);
      expect(ctx.entities(topology.ClosedDirectedFamily)[0].order).toBe(
        "reverse-inclusion",
      );
      expect(ctx.facts(topology.SplitBetween)).toHaveLength(0);
      const hypothesis = ctx
        .facts(topology.Hypothesis)
        .find(([p]) => p.predicate === topology.SplitBetween)![0];
      expect(hypothesis.args[0]).toBe(b);
      const [u, v, g, h, tau] = ctx.facts(topology.SetSeparation)[0];
      expect(ctx.test(topology.Compact, tau)).toBe(true);
      expect(ctx.test(topology.T2, tau)).toBe(true);
      expect(ctx.test(topology.Disjoint, g, h)).toBe(true);
      expect(ctx.test(topology.Subset, u, g)).toBe(true);
      expect(ctx.test(topology.Subset, v, h)).toBe(true);
      expect(ctx.facts(topology.FamilyChoiceOutside)).toHaveLength(1);
      expect(ctx.facts(topology.NetLimitPoint)).toHaveLength(1);
    }),
  });
  d.discard();
});

function shape(svg: SVGSVGElement, title: string, tag: string): Element {
  const p = Array.from(svg.querySelectorAll("title")).find(
    (t) => t.textContent === title,
  )?.parentElement;
  const e = p?.matches(tag) ? p : p?.querySelector(tag);
  if (!e) throw new Error(`Missing ${title}`);
  return e;
}
function inside(path: Element, point: readonly [number, number]) {
  const vertices: [number, number][] = [];
  let previous: [number, number] = [0, 0];
  for (const c of (path.getAttribute("d") ?? "").match(/[MLCZ][^MLCZ]*/g) ??
    []) {
    const p = (
      c.slice(1).match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
    ).map(Number);
    if (c[0] === "M" || c[0] === "L") {
      previous = [p[0], p[1]];
      vertices.push(previous);
    } else if (c[0] === "C") {
      const a = previous;
      for (let i = 1; i <= 30; i++) {
        const t = i / 30,
          q = 1 - t;
        vertices.push(
          [0, 1].map(
            (j) =>
              q ** 3 * a[j] +
              3 * q * q * t * p[j] +
              3 * q * t * t * p[j + 2] +
              t ** 3 * p[j + 4],
          ) as [number, number],
        );
      }
      previous = [p[4], p[5]];
    }
  }
  let result = false;
  for (let i = 0, j = vertices.length - 1; i < vertices.length; j = i++) {
    const a = vertices[i],
      b = vertices[j];
    if (
      a[1] > point[1] !== b[1] > point[1] &&
      point[0] < ((b[0] - a[0]) * (point[1] - a[1])) / (b[1] - a[1]) + a[0]
    )
      result = !result;
  }
  return result;
}
test("seven available source figures render native shapes and retain their mathematical labels", async () => {
  for (const build of [
    buildConvexBoxFigure,
    buildPolygonalReachabilityFigure,
    buildMutuallySeparatedDisksFigure,
    buildSpiralClosureFigure,
    buildConnectedProductSlicesFigure,
    buildAccumulatingComponentsFigure,
    buildDirectedClosedIntersectionFigure,
  ]) {
    const d = await build();
    try {
      const { svg } = await d.render();
      expect(svg.querySelector("image")).toBeNull();
      expect(svg.querySelectorAll("[data-tex]").length).toBeGreaterThan(0);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      d.discard();
    }
  }
});
test("actual source route vertices lie inside U and the directed selection lies outside G and H", async () => {
  const path = await buildPolygonalReachabilityFigure(),
    intersection = await buildDirectedClosedIntersectionFigure();
  try {
    const svg = (await path.render()).svg,
      contour = shape(svg, "polygonal.open-region", "path");
    for (let i = 0; i < 6; i++) {
      const dot = shape(svg, `polygonal.vertex-${i}`, "circle");
      expect(
        inside(contour, [
          Number(dot.getAttribute("cx")),
          Number(dot.getAttribute("cy")),
        ]),
      ).toBe(true);
    }
    const target = (await intersection.render()).svg,
      xi = shape(target, "intersection.outside-selection", "circle"),
      p: [number, number] = [
        Number(xi.getAttribute("cx")),
        Number(xi.getAttribute("cy")),
      ];
    expect(inside(shape(target, "intersection.family-member", "path"), p)).toBe(
      true,
    );
    expect(inside(shape(target, "intersection.normal-g", "path"), p)).toBe(
      false,
    );
    expect(inside(shape(target, "intersection.normal-h", "path"), p)).toBe(
      false,
    );
  } finally {
    path.discard();
    intersection.discard();
  }
});
test("repeated source builds are deterministic and final options enable native interactive labels", async () => {
  const [a, b] = await Promise.all([
      buildConvexBoxFigure(),
      buildConvexBoxFigure(),
    ]),
    interactive = await buildMutuallySeparatedDisksFigure({
      interactive: true,
      variation: "separated-disks-interactive",
    });
  try {
    expect((await a.render()).svg.outerHTML).toBe(
      (await b.render()).svg.outerHTML,
    );
    expect(interactive.getDraggingConstraints().size).toBeGreaterThan(0);
  } finally {
    a.discard();
    b.discard();
    interactive.discard();
  }
});
