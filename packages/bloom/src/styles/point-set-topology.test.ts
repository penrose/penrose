import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  inOpenDisk,
  pointDiskDistance,
  reciprocalDistanceToXAxis,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildClosedSetsSeparationFigure,
  buildOpenDiskBoundaryFigure,
  buildPointClosedSeparationFigure,
  buildReciprocalXAxisFigure,
  closedSetsSeparationSubstance,
  openDiskBoundarySubstance,
  pointClosedSetSeparationSubstance,
  reciprocalXAxisSubstance,
} from "../examples/point-set-topology.js";
import { diskBoundaryStyle, separationStyle } from "./point-set-topology.js";
import { eulerVennStyleFor } from "./set-theory.js";

const circle = (svg: SVGElement, name: string) => {
  const element = svg.querySelector(`circle[aria-label="${name}"]`);
  if (!element) throw new Error(`Missing circle ${name}`);
  return {
    x: Number(element.getAttribute("cx")),
    y: Number(element.getAttribute("cy")),
    r: Number(element.getAttribute("r")),
  };
};

/** Sample the rendered cubic outline, independently of the style's PathData. */
const boundary = (svg: SVGElement, name: string) => {
  const element = svg.querySelector(`path[aria-label="${name}"]`);
  if (!element) throw new Error(`Missing outline ${name}`);
  const tokens = element
    .getAttribute("d")!
    .match(/[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?|[MCZ]/g)!;
  const result: [number, number][] = [];
  let at: [number, number] = [0, 0];
  let i = 0;
  while (i < tokens.length) {
    const command = tokens[i++];
    if (command === "M") {
      at = [Number(tokens[i++]), Number(tokens[i++])];
      result.push(at);
    } else if (command === "C") {
      const coordinates = tokens.slice(i, i + 6).map(Number);
      i += 6;
      const start = at;
      for (let step = 1; step <= 40; step++) {
        const t = step / 40;
        const u = 1 - t;
        at = [0, 1].map(
          (axis) =>
            u ** 3 * start[axis] +
            3 * u * u * t * coordinates[axis] +
            3 * u * t * t * coordinates[axis + 2] +
            t ** 3 * coordinates[axis + 4],
        ) as [number, number];
        result.push(at);
      }
    } else if (command !== "Z")
      throw new Error(`Unsupported outline command ${command}`);
  }
  return result;
};

const inside = ([x, y]: [number, number], polygon: [number, number][]) => {
  let contained = false;
  for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
    const [xi, yi] = polygon[i];
    const [xj, yj] = polygon[j];
    if (yi > y !== yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi)
      contained = !contained;
  }
  return contained;
};

async function renderDrawing(drawing: Diagram, id?: string) {
  try {
    for (let i = 0; i < 2000; i++) {
      if (!(await drawing.optimizationStep())) break;
      if (i === 1999)
        throw new Error("Topology figure did not finish optimizing");
    }
    const { svg } = await drawing.render();
    expect(svg.outerHTML).not.toMatch(/NaN|undefined|Infinity/);
    const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
    if (destination && id) {
      mkdirSync(destination, { recursive: true });
      writeFileSync(join(destination, `figure-${id}.svg`), svg.outerHTML);
    }
    return svg;
  } finally {
    drawing.discard();
  }
}

describe("Gemignani Figures 2.17–2.20", () => {
  test("distinguishes boundary membership, zero infima, and the triangle-inequality counterexample", () => {
    const disk = { center: [0, 0] as const, radius: 1 };
    expect(inOpenDisk(disk, [0.999, 0])).toBe(true);
    expect(inOpenDisk(disk, [1, 0])).toBe(false);
    expect(inOpenDisk(disk, [-1, 0])).toBe(false);
    expect(pointDiskDistance(disk, [1, 0])).toBe(0);
    expect(pointDiskDistance(disk, [-1, 0])).toBe(0);
    expect(Math.hypot(1 - -1, 0)).toBeGreaterThan(
      pointDiskDistance(disk, [1, 0]) + pointDiskDistance(disk, [-1, 0]),
    );
    for (const epsilon of [0.1, 0.01, 0.0001]) {
      const x = 2 / epsilon;
      expect(reciprocalDistanceToXAxis(1, x)).toBeGreaterThan(0);
      expect(reciprocalDistanceToXAxis(1, x)).toBeLessThan(epsilon);
    }
    expect(() => reciprocalDistanceToXAxis(1, 0)).toThrow("nonzero");
    for (const sub of [
      openDiskBoundarySubstance(),
      pointClosedSetSeparationSubstance(),
      closedSetsSeparationSubstance(),
      reciprocalXAxisSubstance(),
    ]) {
      for (const entity of sub.entities) {
        expect(entity).not.toHaveProperty("icon");
        expect(entity).not.toHaveProperty("fillColor");
      }
    }
    expect(eulerVennStyleFor(topology).domain).toBe(topology);
  });

  test("renders the open unit disk and reuses its style for another mathematical radius", async () => {
    const svg = await renderDrawing(
      await buildOpenDiskBoundaryFigure(),
      "2.17",
    );
    const disk = circle(svg, "open-disk");
    const w = circle(svg, "singleton.W");
    const z = circle(svg, "singleton.Z");
    for (const p of [w, z])
      expect(Math.hypot(p.x - disk.x, p.y - disk.y)).toBeCloseTo(disk.r, 8);
    expect(disk.r).toBe(80);
    expect(w.x - z.x).toBe(160);
    const sty = diskBoundaryStyle();
    const half = await renderDrawing(
      await diagram({
        sub: openDiskBoundarySubstance(0.5),
        sty,
        canvas: canvas(360, 280),
      }),
    );
    expect(circle(half, "open-disk").r).toBe(40);
    expect(circle(half, "singleton.W").x - circle(half, "singleton.Z").x).toBe(
      80,
    );
    expect(half.querySelectorAll("circle")).toHaveLength(
      svg.querySelectorAll("circle").length,
    );
  });

  test("reuses one separation style for both point and closed-pair constructions", async () => {
    const point = await renderDrawing(
      await buildPointClosedSeparationFigure(),
      "2.18",
    );
    const pairs = await renderDrawing(
      await buildClosedSetsSeparationFigure(),
      "2.19",
    );
    for (const [svg, first, second] of [
      [point, "separation.neighborhood-y", "separation.neighborhood-x"],
      [pairs, "separation.neighborhood-left", "separation.neighborhood-right"],
    ] as const) {
      const a = circle(svg, first);
      const b = circle(svg, second);
      expect(Math.hypot(a.x - b.x, a.y - b.y)).toBeGreaterThan(a.r + b.r);
    }
    expect(pairs.querySelectorAll("clipPath")).toHaveLength(2);
    expect(
      point.querySelector('path[aria-label="Closed set F"]'),
    ).not.toBeNull();
    expect(
      pairs.querySelector('path[aria-label="Closed set F\'"]'),
    ).not.toBeNull();
    const leftUnion = boundary(pairs, "separation.union-left");
    const rightUnion = boundary(pairs, "separation.union-right");
    expect(Math.max(...leftUnion.map(([x]) => x))).toBeLessThan(
      Math.min(...rightUnion.map(([x]) => x)),
    );
    for (const [closed, union, ball] of [
      ["Closed set F", leftUnion, "separation.neighborhood-left"],
      ["Closed set F'", rightUnion, "separation.neighborhood-right"],
    ] as const) {
      for (const p of boundary(pairs, closed))
        expect(inside(p, union)).toBe(true);
      const disk = circle(pairs, ball);
      for (let i = 0; i < 128; i++) {
        const angle = (2 * Math.PI * i) / 128;
        expect(
          inside(
            [
              disk.x + disk.r * Math.cos(angle),
              disk.y + disk.r * Math.sin(angle),
            ],
            union,
          ),
        ).toBe(true);
      }
    }
    const xNeighborhood = circle(point, "separation.neighborhood-x");
    for (const [x, y] of boundary(point, "Closed set F"))
      expect(
        Math.hypot(x - xNeighborhood.x, y - xNeighborhood.y),
      ).toBeGreaterThan(xNeighborhood.r);
    const sharedStyle = separationStyle({ scale: 0.7 });
    for (const sub of [
      pointClosedSetSeparationSubstance(),
      closedSetsSeparationSubstance(),
    ]) {
      const svg = await renderDrawing(
        await diagram({ sub, sty: sharedStyle, canvas: canvas(500, 280) }),
      );
      expect(svg.querySelectorAll("circle")).toHaveLength(4);
    }
  });

  test("renders both reciprocal branches with xy = 1 and the disjoint x-axis", async () => {
    const svg = await renderDrawing(await buildReciprocalXAxisFigure(), "2.20");
    const axis = svg.querySelector('[aria-label="Line F\'"] line');
    if (!axis) throw new Error("Missing x-axis");
    const originX = 340 / 2 - 79;
    const originY = Number(axis.getAttribute("y1"));
    const branches = Array.from(svg.querySelectorAll("polyline"));
    expect(branches).toHaveLength(2);
    for (const branch of branches) {
      const coordinates = branch
        .getAttribute("points")!
        .trim()
        .split(/[\s,]+/)
        .map(Number);
      expect(coordinates.length).toBe(322);
      for (let i = 0; i < coordinates.length; i += 2) {
        const x = (coordinates[i] - originX) / 51;
        const y = (originY - coordinates[i + 1]) / 51;
        expect(x).not.toBe(0);
        expect(y).not.toBe(0);
        expect(x * y).toBeCloseTo(1, 8);
      }
    }
  });
});

const checkTopologyTypes = () => {
  const sub = topology.substance();
  const disk = sub.OpenDisk({ center: [0, 0], radius: 1 });
  const f = sub.ClosedRegion();
  const p = sub.Point();
  sub.BoundaryPoint(p, disk);
  sub.PointClosedSeparation(
    p,
    // @ts-expect-error Closed-set separation requires a closed set.
    disk,
    sub.Neighborhood(),
    sub.NeighborhoodUnion(),
  );
  // @ts-expect-error Sets cannot substitute for points.
  sub.Member(f, disk);
  // @ts-expect-error Mathematical sets have no TSX shape fields.
  disk.icon;
};
void checkTopologyTypes;
