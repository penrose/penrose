// @vitest-environment jsdom

import type { PlaneCoordinates } from "@penrose/bloom";
import {
  canvas,
  centralProjectToSegment,
  diagram,
  endpointCirclePoint,
  endpointIdentificationStyle,
  endpointIdentified,
  radialProjectToCircle,
  radialProjectToTriangle,
  topologicalProjectionStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import { intervalEndpointIdentification } from "./endpoint-identification.js";
import {
  segmentCentralProjection,
  triangleCircleRadialProjection,
} from "./topological-projections.js";

const close = (a: PlaneCoordinates, b: PlaneCoordinates) =>
  expect(Math.hypot(a[0] - b[0], a[1] - b[1])).toBeLessThan(1e-10);

describe("Chapter 4 projection and quotient mathematics", () => {
  test("the perspective map carries both endpoints bijectively and has a continuous ordered inverse", () => {
    const p = [0, 2] as const,
      a = [-0.85, 0.08] as const,
      b = [2.45, -0.2] as const,
      c = [0.64925, 1.417] as const;
    let previous = -1;
    for (let i = 0; i <= 100; i++) {
      const t = i / 100,
        x: PlaneCoordinates = [
          a[0] + t * (c[0] - a[0]),
          a[1] + t * (c[1] - a[1]),
        ];
      const image = centralProjectToSegment(p, x, [a, b]);
      const fraction = (image[0] - a[0]) / (b[0] - a[0]);
      expect(fraction).toBeGreaterThan(previous);
      previous = fraction;
      close(centralProjectToSegment(p, image, [a, c]), x);
      expect(
        (image[0] - p[0]) * (x[1] - p[1]) - (image[1] - p[1]) * (x[0] - p[0]),
      ).toBeCloseTo(0, 10);
    }
    close(centralProjectToSegment(p, a, [a, b]), a);
    close(centralProjectToSegment(p, c, [a, b]), b);
    expect(() => centralProjectToSegment(p, p, [a, b])).toThrow("transversely");
  });

  test("radial maps are inverse correspondences around every direction, including triangle vertices", () => {
    const center = [0, 0] as const,
      vertices = [
        [-1.58, -0.82],
        [1.16, -0.6],
        [0.35, 1.43],
      ] as const;
    for (let angle = 0; angle < Math.PI * 2; angle += Math.PI / 40) {
      const circle: PlaneCoordinates = [
        1.16 * Math.cos(angle),
        1.16 * Math.sin(angle),
      ];
      const triangle = radialProjectToTriangle(center, vertices, circle);
      const projected = radialProjectToCircle(center, 1.16, triangle);
      close(projected, circle);
      expect(Math.hypot(...projected)).toBeCloseTo(1.16, 12);
    }
    for (const vertex of vertices)
      close(
        radialProjectToTriangle(
          center,
          vertices,
          radialProjectToCircle(center, 1.16, vertex),
        ),
        vertex,
      );
    expect(() => radialProjectToTriangle([2, 2], vertices, [1, 0])).toThrow(
      "strictly inside",
    );
    expect(() => radialProjectToCircle(center, 1, center)).toThrow("differ");
  });

  test("the endpoint equivalence is reflexive, symmetric and transitive while interior classes stay singleton", () => {
    const values = [0, 0.25, 0.5, 0.75, 1];
    for (const x of values) {
      expect(endpointIdentified([0, 1], x, x)).toBe(true);
      for (const y of values) {
        expect(endpointIdentified([0, 1], x, y)).toBe(
          endpointIdentified([0, 1], y, x),
        );
        for (const z of values)
          if (
            endpointIdentified([0, 1], x, y) &&
            endpointIdentified([0, 1], y, z)
          )
            expect(endpointIdentified([0, 1], x, z)).toBe(true);
      }
    }
    expect(endpointIdentified([0, 1], 0, 1)).toBe(true);
    expect(endpointIdentified([0, 1], 0.25, 0.75)).toBe(false);
    expect(endpointIdentified([0, 1], 0, 0.5)).toBe(false);
    expect(() => endpointIdentified([0, 1], -1, 0)).toThrow("closed interval");
  });

  test("the circle realization is constant precisely on the endpoint class and sends the midpoint to the bottom", () => {
    expect(endpointCirclePoint([0, 1], 0)).toEqual(
      endpointCirclePoint([0, 1], 1),
    );
    close(endpointCirclePoint([0, 1], 0), [0, 1]);
    close(endpointCirclePoint([0, 1], 0.5), [0, -1]);
    const images = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9].map((t) =>
      endpointCirclePoint([0, 1], t),
    );
    for (let i = 0; i < images.length; i++) {
      expect(Math.hypot(...images[i])).toBeCloseTo(1, 12);
      for (let j = i + 1; j < images.length; j++)
        expect(
          Math.hypot(images[i][0] - images[j][0], images[i][1] - images[j][1]),
        ).toBeGreaterThan(0.1);
    }
    expect(() => intervalEndpointIdentification({ representative: 1 })).toThrow(
      "strictly inside",
    );
  });

  test("quotient classes reuse set membership and act as points under the induced map", async () => {
    const inspect = topology.style((ctx) => {
      const [quotient, interval, relation] = ctx.facts(topology.QuotientOf)[0];
      const classes = ctx.entities(topology.EquivalenceClass);
      expect(classes).toHaveLength(2);
      for (const cls of classes) {
        expect(ctx.entities(topology.Set)).toContain(cls);
        expect(ctx.entities(topology.Point)).toContain(cls);
        expect(ctx.test(topology.Member, cls, quotient)).toBe(true);
        expect("coordinates" in cls).toBe(false);
      }
      const [q, , target, r] = ctx.facts(topology.IdentificationMap)[0];
      expect(target).toBe(quotient);
      expect(r).toBe(relation);
      const endpointPoints = ctx.facts(topology.EquivalentUnder)[0];
      const images = endpointPoints
        .slice(0, 2)
        .map(
          (p) =>
            ctx
              .facts(topology.MapsTo)
              .find(([map, point]) => map === q && point === p)?.[2],
        );
      expect(images[0]).toBe(images[1]);
      expect(ctx.test(topology.MapBetween, q, interval, quotient)).toBe(true);
      expect(ctx.facts(topology.FactorsThrough)).toHaveLength(1);
      expect(ctx.facts(topology.IdentificationTopology)).toHaveLength(1);
    });
    const result = await diagram({
      sub: intervalEndpointIdentification(),
      sty: [inspect, endpointIdentificationStyle()],
      canvas: canvas(250, 330),
    });
    result.discard();
  });
});

describe("Chapter 4 actual Penrose SVG rendering", () => {
  test("one reusable projection style renders the segment construction", async () => {
    const result = await diagram({
      sub: segmentCentralProjection(),
      sty: topologicalProjectionStyle(),
      canvas: canvas(340, 240),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("line")).toHaveLength(4);
      expect(svg.querySelectorAll("circle")).toHaveLength(3);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("the same projection style renders triangle-to-circle correspondence", async () => {
    const result = await diagram({
      sub: triangleCircleRadialProjection(),
      sty: topologicalProjectionStyle(),
      canvas: canvas(280, 245),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("polygon")).toHaveLength(1);
      expect(svg.querySelectorAll("circle")).toHaveLength(4);
      expect(svg.querySelectorAll("line")).toHaveLength(1);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("endpoint identification renders both stacked panels and highlights one quotient point", async () => {
    const result = await diagram({
      sub: intervalEndpointIdentification(),
      sty: endpointIdentificationStyle(),
      canvas: canvas(250, 330),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("ellipse")).toHaveLength(3);
      expect(svg.querySelectorAll("circle")).toHaveLength(6);
      const circle = Array.from(svg.querySelectorAll("circle")).find(
        (c) => Number(c.getAttribute("r")) > 50,
      );
      const interval = Array.from(svg.querySelectorAll("line")).find(
        (l) => l.getAttribute("x1") === "25" && l.getAttribute("x2") === "225",
      );
      expect(Number(interval?.getAttribute("y1"))).toBeLessThan(
        Number(circle?.getAttribute("cy")),
      );
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });
});
