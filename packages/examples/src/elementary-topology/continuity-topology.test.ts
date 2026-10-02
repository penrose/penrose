// @vitest-environment jsdom

import {
  canvas,
  diagram,
  inNeighborhood,
  inProjectionInverseImage,
  limitUniquenessStyle,
  metricContinuityStyle,
  metricSpaces,
  projectionPreimageStyle,
  separatedLimitRadius,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import { distinctProposedLimitNeighborhoods } from "./limit-uniqueness.js";
import { continuousMapNeighborhoods } from "./metric-continuity.js";
import { coordinateProjectionNeighborhood } from "./projection-preimage.js";

describe("Figures 2.14–2.16 mathematical content", () => {
  test("the tangent boundary is excluded from both disjoint open neighborhoods", () => {
    const left = [-1, -0.4] as const,
      right = [1, 0.4] as const;
    const rho = separatedLimitRadius("euclidean", left, right);
    expect(inNeighborhood("euclidean", left, rho, [0, 0])).toBe(false);
    expect(inNeighborhood("euclidean", right, rho, [0, 0])).toBe(false);
    for (let angle = 0; angle < Math.PI * 2; angle += Math.PI / 30) {
      const p = [
        left[0] + rho * 0.999 * Math.cos(angle),
        left[1] + rho * 0.999 * Math.sin(angle),
      ] as const;
      expect(inNeighborhood("euclidean", left, rho, p)).toBe(true);
      expect(inNeighborhood("euclidean", right, rho, p)).toBe(false);
    }
    expect(() =>
      distinctProposedLimitNeighborhoods({ first: [0, 0], second: [0, 0] }),
    ).toThrow("distinct");
  });

  test("the uniqueness example asserts proposed limits and no contradictory actual convergence", async () => {
    const assertions = metricSpaces.style((ctx) => {
      expect(ctx.facts(metricSpaces.ProposedLimitOf)).toHaveLength(2);
      expect(ctx.facts(metricSpaces.ConvergesTo)).toHaveLength(0);
      expect(ctx.facts(metricSpaces.DistinctMetricPoints)).toHaveLength(1);
      expect(ctx.facts(metricSpaces.DisjointNeighborhoods)).toHaveLength(1);
    });
    const result = await diagram({
      sub: distinctProposedLimitNeighborhoods(),
      sty: [assertions, limitUniquenessStyle()],
      canvas: canvas(370, 285),
    });
    result.discard();
  });

  test("a generic continuity witness contains no drawing coordinates or blob geometry", async () => {
    const assertions = metricSpaces.style((ctx) => {
      const points = ctx.entities(metricSpaces.SpacePoint);
      expect(points).toHaveLength(4);
      for (const point of points) expect("point" in point).toBe(false);
      expect(
        ctx
          .entities(metricSpaces.MetricSpace)
          .map((space) => space.metricLabel),
      ).toEqual(["D", "D'"]);
      const [f, source, target] = ctx.facts(metricSpaces.MapBetweenSpaces)[0];
      const [map, sourceBall, targetBall] = ctx.facts(
        metricSpaces.MapsNeighborhoodInto,
      )[0];
      expect(map).toBe(f);
      expect(
        ctx.test(metricSpaces.SpaceNeighborhoodInSpace, sourceBall, source),
      ).toBe(true);
      expect(
        ctx.test(metricSpaces.SpaceNeighborhoodInSpace, targetBall, target),
      ).toBe(true);
      expect(ctx.facts(metricSpaces.MapsPoint)).toHaveLength(2);
      expect(ctx.facts(metricSpaces.ContinuousAt)).toHaveLength(1);
    });
    const result = await diagram({
      sub: continuousMapNeighborhoods(),
      sty: [assertions, metricContinuityStyle()],
      canvas: canvas(570, 260),
    });
    result.discard();
    expect(() => continuousMapNeighborhoods({ sourceRadius: 0 })).toThrow(
      "positive radii",
    );
  });

  test("the projection inverse image uses a strict x inequality and permits arbitrarily distant y", () => {
    for (const y of [-1e100, -1000, 0, 1000, 1e100]) {
      expect(inProjectionInverseImage([2, y], 2, 1)).toBe(true);
      expect(inProjectionInverseImage([1.01, y], 2, 1)).toBe(true);
      expect(inProjectionInverseImage([1, y], 2, 1)).toBe(false);
      expect(inProjectionInverseImage([3, y], 2, 1)).toBe(false);
      expect(inProjectionInverseImage([3.01, y], 2, 1)).toBe(false);
    }
    expect(() => coordinateProjectionNeighborhood({ rho: -1 })).toThrow(
      "positive radius",
    );
    expect(() => inProjectionInverseImage([2, NaN], 2, 1)).toThrow(
      "finite data",
    );
  });

  test("incomplete continuity or projection facts are rejected before assembly", async () => {
    const sub = metricSpaces.substance();
    const f = sub.MetricMap({ label: "f" });
    const source = sub.MetricSpace({ label: "X" }),
      target = sub.MetricSpace({ label: "Y" });
    const ball = sub.SpaceNeighborhood({ rho: 1 });
    sub.MapBetweenSpaces(f, source, target);
    sub.MapsNeighborhoodInto(f, ball, ball);
    await expect(
      diagram({ sub: sub.make(), sty: metricContinuityStyle() }),
    ).rejects.toThrow("unique spaces and centers");
    await expect(
      diagram({
        sub: metricSpaces.substance().make(),
        sty: projectionPreimageStyle(),
      }),
    ).rejects.toThrow("one neighborhood inverse image");
  });
});

describe("Figures 2.14–2.16 actual Penrose rendering", () => {
  test("the uniqueness proof renders tangent circles, a dashed separation, and the schematic sequence", async () => {
    const result = await diagram({
      sub: distinctProposedLimitNeighborhoods(),
      sty: limitUniquenessStyle(),
      canvas: canvas(370, 285),
    });
    try {
      const { svg } = await result.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(11);
      const circles = Array.from(svg.querySelectorAll("circle")).filter(
        (circle) => Number(circle.getAttribute("r")) > 50,
      );
      const centers = circles.map((circle) => [
        Number(circle.getAttribute("cx")),
        Number(circle.getAttribute("cy")),
      ]);
      expect(
        Math.hypot(
          centers[0][0] - centers[1][0],
          centers[0][1] - centers[1][1],
        ),
      ).toBeCloseTo(2 * Number(circles[0].getAttribute("r")), 8);
      expect(svg.querySelector('line[stroke-dasharray="3 3"]')).not.toBeNull();
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("the continuity witness renders the two original-shaped cubic blobs and labeled neighborhoods", async () => {
    const result = await diagram({
      sub: continuousMapNeighborhoods(),
      sty: metricContinuityStyle(),
      canvas: canvas(570, 260),
    });
    try {
      const { svg } = await result.render();
      const blobs = Array.from(svg.querySelectorAll("path")).filter(
        (path) => path.getAttribute("stroke-width") === "1.1",
      );
      expect(blobs).toHaveLength(2);
      for (const blob of blobs) {
        expect(blob.getAttribute("d")).toContain("C");
        expect(blob.getAttribute("d")).toContain("Z");
      }
      expect(svg.querySelectorAll("circle")).toHaveLength(6);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      result.discard();
    }
  });

  test("changing the visible strip height leaves its infinite Substance inverse image intact", async () => {
    const sub = coordinateProjectionNeighborhood();
    const inspect = metricSpaces.style((ctx) => {
      const strip = ctx.entities(metricSpaces.VerticalOpenStrip)[0];
      expect(strip.center).toBe(2);
      expect(strip.halfWidth).toBe(1);
      expect("height" in strip).toBe(false);
      expect("yBounds" in strip).toBe(false);
    });
    for (const height of [180, 240]) {
      const result = await diagram({
        sub,
        sty: [inspect, projectionPreimageStyle({ clipHeight: height })],
        canvas: canvas(350, 300),
      });
      try {
        const { svg } = await result.render();
        const strip = Array.from(svg.querySelectorAll("rect")).find(
          (rect) => rect.getAttribute("width") === "130",
        );
        expect(strip?.getAttribute("height")).toBe(String(height));
        expect(strip?.getAttribute("stroke-width")).toBe("0");
        expect(svg.querySelectorAll("circle")).toHaveLength(2);
        expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      } finally {
        result.discard();
      }
    }
  });
});
