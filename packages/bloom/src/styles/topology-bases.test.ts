import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { planeDistance } from "../domains/metric-spaces.js";
import {
  inOpenHalfPlane,
  inOpenTriangle,
  inRealInterval,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildHalfPlaneSquareFigure,
  buildIntervalSubbasisFigure,
  buildTriangleDiskBasisFigure,
  halfPlaneSquareSubstance,
  intervalSubbasisSubstance,
  triangleDiskBasisSubstance,
} from "../examples/topology-bases.js";
import {
  halfPlaneIntersectionStyle,
  intervalSubbasisStyle,
} from "./topology-bases.js";

async function render(drawing: Diagram, id?: string) {
  try {
    for (let i = 0; i < 2000; i++) {
      if (!(await drawing.optimizationStep())) break;
      if (i === 1999)
        throw new Error("Basis diagram did not finish optimizing");
    }
    const { svg } = await drawing.render();
    expect(svg.outerHTML).not.toMatch(/NaN|undefined|Infinity/);
    const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
    if (destination && id) {
      mkdirSync(destination, { recursive: true });
      writeFileSync(
        join(destination, `figure-${id}.svg`),
        new XMLSerializer().serializeToString(svg),
      );
    }
    return svg;
  } finally {
    drawing.discard();
  }
}

const coords = (element: Element) =>
  element
    .getAttribute("points")!
    .split(/[\s,]+/)
    .map(Number)
    .reduce<[number, number][]>((pairs, n, i, values) => {
      if (i % 2 === 0) pairs.push([n, values[i + 1]]);
      return pairs;
    }, []);
const inside = ([x, y]: [number, number], polygon: [number, number][]) => {
  let contained = false;
  for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
    const [xi, yi] = polygon[i],
      [xj, yj] = polygon[j];
    if (yi > y !== yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi)
      contained = !contained;
  }
  return contained;
};

describe("Chapter 3 bases and subbases", () => {
  test("uses strict intersections and the correct triangle/circle basis comparison", () => {
    const sub = intervalSubbasisSubstance(-2, 5);
    const interval = sub.entities.find((a) => "leftClosed" in a)!;
    if (
      !(
        "a" in interval &&
        "b" in interval &&
        "leftClosed" in interval &&
        "rightClosed" in interval
      )
    )
      throw new Error("Missing interval data");
    for (const x of [-3, -2, 0, 5, 6])
      expect(
        inRealInterval(
          interval as {
            a: number;
            b: number;
            leftClosed: boolean;
            rightClosed: boolean;
          },
          x,
        ),
      ).toBe(-2 < x && x < 5);
    const planes = [
      [1, 0, 1],
      [-1, 0, 1],
      [0, 1, 1],
      [0, -1, 1],
    ] as const;
    for (const p of [
      [0, 0],
      [0.99, -0.99],
      [1, 0],
      [-1, 0],
      [0, 1],
      [0, -1],
      [2, 2],
    ] as const)
      expect(planes.every((plane) => inOpenHalfPlane(plane, p))).toBe(
        planeDistance("supremum", [0, 0], p) < 1,
      );
    const vertices = [
      [-0.98, -0.09],
      [0.37, 0.9],
      [0.6, -0.78],
    ] as const;
    expect(inOpenTriangle(vertices, [0, 0])).toBe(true);
    expect(inOpenTriangle(vertices, vertices[0])).toBe(false);
    expect(inOpenTriangle(vertices, [2, 2])).toBe(false);
    for (const substance of [
      sub,
      halfPlaneSquareSubstance(),
      triangleDiskBasisSubstance(),
    ])
      for (const entity of substance.entities)
        expect(entity).not.toHaveProperty("icon");
    expect(
      triangleDiskBasisSubstance().propositions.some(
        (p) => p.predicate === topology.EqualTopologies,
      ),
    ).toBe(true);
  });

  test("renders three source panels and checks the nested neighborhoods geometrically", async () => {
    const ray = await render(await buildIntervalSubbasisFigure(), "3.1");
    expect(
      Array.from(ray.querySelectorAll("[data-tex]")).map((a) =>
        decodeURIComponent(a.getAttribute("data-tex")!),
      ),
    ).toContain("\\{x\\mid a<x<b\\}");
    const halfPlanes = await render(await buildHalfPlaneSquareFigure(), "3.2");
    expect(halfPlanes.querySelectorAll("clipPath")).toHaveLength(4);
    const shapes = Array.from(
      halfPlanes.querySelectorAll("polygon[aria-label]"),
    ).map(coords);
    expect(shapes).toHaveLength(4);
    for (const [x, y] of [
      [0, 0],
      [0.75, 0.75],
      [1.5, 0],
      [0, -1.5],
    ] as const)
      expect(
        shapes.every((p) => inside([180 + x * 46, 142.5 - y * 46], p)),
      ).toBe(Math.abs(x) < 1 && Math.abs(y) < 1);
    const basis = await render(await buildTriangleDiskBasisFigure(), "3.3");
    const outer = basis.querySelector('circle[aria-label="basis.outer-disk"]')!;
    const inner = basis.querySelector('circle[aria-label="basis.inner-disk"]')!;
    const triangle = coords(
      basis.querySelector('polygon[aria-label="basis.triangle"]')!,
    );
    const cx = Number(outer.getAttribute("cx")),
      cy = Number(outer.getAttribute("cy")),
      r = Number(outer.getAttribute("r"));
    expect(r).toBe(114);
    for (const [x, y] of triangle)
      expect(Math.hypot(x - cx, y - cy)).toBeLessThan(r);
    const ix = Number(inner.getAttribute("cx")),
      iy = Number(inner.getAttribute("cy")),
      ir = Number(inner.getAttribute("r"));
    for (let i = 0; i < 128; i++) {
      const angle = (i * Math.PI) / 64;
      expect(
        inside(
          [ix + ir * Math.cos(angle), iy + ir * Math.sin(angle)],
          triangle,
        ),
      ).toBe(true);
    }
  });

  test("reuses basis styles on other substance programs", async () => {
    const squareStyle = halfPlaneIntersectionStyle();
    const small = await render(
      await diagram({
        sub: halfPlaneSquareSubstance(0.75),
        sty: squareStyle,
        canvas: canvas(360, 285),
      }),
    );
    const polygons = Array.from(
      small.querySelectorAll("polygon[aria-label]"),
    ).map(coords);
    expect(polygons.every((p) => inside([180 + 0.7 * 46, 142.5], p))).toBe(
      true,
    );
    expect(polygons.every((p) => inside([180 + 0.8 * 46, 142.5], p))).toBe(
      false,
    );
    const rays = await render(
      await diagram({
        sub: intervalSubbasisSubstance(-2, 5),
        sty: intervalSubbasisStyle(),
        canvas: canvas(400, 100),
      }),
    );
    expect(rays.querySelectorAll("line")).toHaveLength(3);
  });
});

const checkTypes = () => {
  const sub = topology.substance();
  const a = sub.OpenInterval({
    a: 0,
    b: 1,
    leftClosed: false,
    rightClosed: false,
  });
  sub.SetInFamily(a, sub.Basis());
  // @ts-expect-error A point cannot be a family member set.
  sub.SetInFamily(sub.Point(), sub.Basis());
  // @ts-expect-error An open interval cannot include an endpoint.
  sub.OpenInterval({ a: 0, b: 1, leftClosed: true, rightClosed: false });
};
void checkTypes;
