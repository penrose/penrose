import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  pointOnClosedSegment,
  polygonalPathAvoids,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildConnectedSineCurveFigure,
  buildIntervalConnectednessFigure,
  buildPolygonalRegionPathFigure,
  buildPuncturedPlanePathFigure,
  connectedSineCurveSubstance,
  defaultRegionRoute,
  intervalConnectednessSubstance,
  polygonalRegionPathSubstance,
  puncturedPlanePathSubstance,
} from "../examples/connectedness.js";
import {
  intervalConnectednessStyle,
  polygonalConnectednessStyle,
  sineConnectednessStyle,
} from "./connectedness.js";
import { sampleSineCurve } from "./oscillating-sine-curve.js";

async function render(d: Diagram) {
  let converged = false;
  for (let n = 0; n < 3000; n++) {
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  }
  expect(converged).toBe(true);
  const result = await d.render();
  const xml = new XMLSerializer().serializeToString(result.svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return result;
}

describe("connectedness programs", () => {
  test("the connected interval keeps contradiction assumptions nested and its witness strictly inside the neighborhood", () => {
    const sub = intervalConnectednessSubstance();
    expect(
      sub.propositions.some((p) => p.predicate === topology.Connected),
    ).toBe(true);
    expect(
      sub.propositions.some(
        (p) => p.predicate === topology.TopologicalSeparationOf,
      ),
    ).toBe(false);
    expect(
      sub.propositions.some((p) => p.predicate === topology.SupremumOf),
    ).toBe(false);
    const hypotheses = sub.propositions.filter(
      (p) => p.predicate === topology.Hypothesis,
    );
    expect(hypotheses).toHaveLength(6);
    const local = sub.entities.find(
      (e) => e.label === "(a-\\rho,a+\\rho)",
    ) as EntityOf<typeof topology.OpenIntervalNeighborhood>;
    const middle = sub.entities.find(
      (e) => e.label === "[a-\\rho/2,a+\\rho/2]",
    ) as EntityOf<typeof topology.ClosedInterval>;
    const a = sub.entities.find((e) => e.label === "a") as EntityOf<
      typeof topology.RealPoint
    >;
    expect(local.leftClosed || local.rightClosed).toBe(false);
    expect(middle.leftClosed && middle.rightClosed).toBe(true);
    expect(local.a).toBeLessThan(middle.a);
    expect(middle.a).toBeLessThan(a.coordinate);
    expect(a.coordinate).toBeLessThan(middle.b);
    expect(middle.b).toBeLessThan(local.b);
    expect(() => intervalConnectednessSubstance(0.95, 0.1)).toThrow();
    for (const entity of sub.entities) {
      expect(entity).not.toHaveProperty("shapeType");
      expect(entity).not.toHaveProperty("fillColor");
    }
  });

  test("ordered closed segments avoid the puncture and a single region path does not claim global connectedness", () => {
    const sub = puncturedPlanePathSubstance();
    const segments = sub.propositions.filter(
      (p) => p.predicate === topology.SegmentBetween,
    );
    expect(segments).toHaveLength(2);
    expect(segments[0].args[2]).toBe(segments[1].args[1]);
    const deleted = sub.propositions.find(
      (p) => p.predicate === topology.DeletedPointFrom,
    )!;
    const removed = deleted.args[2] as EntityOf<
      typeof topology.CoordinatePoint
    >;
    const path = sub.propositions.find(
      (p) => p.predicate === topology.PolygonalPathBetween,
    )!.args[0] as EntityOf<typeof topology.PolygonalPath>;
    expect(polygonalPathAvoids(path.vertices, removed.coordinates)).toBe(true);
    expect(pointOnClosedSegment([0.5, 0.5], [0, 0], [1, 1])).toBe(true);
    expect(() =>
      puncturedPlanePathSubstance([
        [0, 0],
        [1, 1],
        [2, 0],
        [0.5, 0.5],
      ]),
    ).toThrow();
    const region = polygonalRegionPathSubstance();
    expect(
      region.propositions.filter(
        (p) => p.predicate === topology.SegmentBetween,
      ),
    ).toHaveLength(6);
    expect(
      region.propositions.some(
        (p) =>
          p.predicate === topology.PolygonallyConnected ||
          p.predicate === topology.Connected,
      ),
    ).toBe(false);
    expect(
      region.propositions.some((p) => p.predicate === topology.ConnectedIn),
    ).toBe(true);
  });

  test("one style instance renders distinct mathematical substances with actual Penrose optimization", async () => {
    const interval = intervalConnectednessStyle();
    const polygonal = polygonalConnectednessStyle({ offset: [0, -5] });
    const sine = sineConnectednessStyle();
    const variants = [
      [intervalConnectednessSubstance(), interval, canvas(350, 80)],
      [intervalConnectednessSubstance(0.45, 0.1), interval, canvas(350, 80)],
      [puncturedPlanePathSubstance(), polygonal, canvas(300, 200)],
      [
        puncturedPlanePathSubstance([
          [0.28, 0.24],
          [0.66, 0.8],
          [1.12, 0.61],
          [0.74, 0.36],
        ]),
        polygonal,
        canvas(300, 200),
      ],
      [polygonalRegionPathSubstance(), polygonal, canvas(300, 220)],
      [
        polygonalRegionPathSubstance(
          defaultRegionRoute.map(([x, y]) => [x + 0.01, y]),
        ),
        polygonal,
        canvas(300, 220),
      ],
      [connectedSineCurveSubstance(), sine, canvas(280, 200)],
      [connectedSineCurveSubstance(2, 0.7), sine, canvas(280, 200)],
    ] as const;
    for (const [sub, sty, size] of variants) {
      const d = await diagram({ sub, sty, canvas: size });
      try {
        const { svg } = await render(d);
        expect(svg.querySelectorAll("image")).toHaveLength(0);
        if (sty === sine) {
          const graph = svg.querySelector(
            'path[aria-label="positive oscillating sine graph, x>0"]',
          )!;
          expect(graph.getAttribute("d")!.match(/M/g)).toHaveLength(1);
          const origin = svg.querySelector(
            'circle[aria-label="sine-connectedness.included-origin"]',
          )!;
          expect(origin).not.toBeNull();
          const data = sub.entities.find((e) => e.label === "Y") as EntityOf<
            typeof topology.OriginAdjoinedSineCurve
          >;
          for (const [x, y] of sampleSineCurve(data, {
            xMin: data.frequency / 500,
            xMax: (data.frequency * 2) / (3 * Math.PI),
          })) {
            expect(x).toBeGreaterThan(0);
            expect(y).toBeCloseTo(
              data.amplitude * Math.sin(data.frequency / x),
              12,
            );
            expect(Math.abs(y)).toBeLessThanOrEqual(data.amplitude);
          }
          expect(
            sub.propositions.some(
              (p) => p.predicate === topology.NotPathConnected,
            ),
          ).toBe(true);
        }
      } finally {
        d.discard();
      }
    }
  });

  test("native label dragging preserves mathematical geometry and zero-jitter reviewed positions", async () => {
    for (const factory of [
      buildIntervalConnectednessFigure,
      buildPuncturedPlanePathFigure,
      buildPolygonalRegionPathFigure,
      buildConnectedSineCurveFigure,
    ]) {
      const d = await factory({ interactive: { jitter: 0 } });
      try {
        const before = await render(d);
        const handles = Array.from(d.getDraggingConstraints().keys());
        expect(handles.length).toBeGreaterThan(0);
        for (const h of handles) {
          expect(d.getInput(h + ".layout.x")).toBeCloseTo(0, 10);
          expect(d.getInput(h + ".layout.y")).toBeCloseTo(0, 10);
        }
        const geometry = (svg: SVGSVGElement) =>
          Array.from(
            svg.querySelectorAll("path,line,circle,polygon"),
            (element) => new XMLSerializer().serializeToString(element),
          );
        const fixed = geometry(before.svg);
        const h = handles[0];
        d.beginDrag(h);
        d.translate(h, 3, 1);
        d.endDrag(h);
        const after = await render(d);
        expect(d.getInput(h + ".layout.x")).toBeCloseTo(3, 4);
        expect(d.getInput(h + ".layout.y")).toBeCloseTo(1, 4);
        expect(geometry(after.svg)).toEqual(fixed);
      } finally {
        d.discard();
      }
    }
  });
});
