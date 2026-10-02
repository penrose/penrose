import { expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { geometricSequenceTerm } from "../domains/metric-completeness.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  baireDenseIntersectionSubstance,
  baireNestedBallsSubstance,
  buildBaireDenseIntersectionFigure,
  buildBaireNestedBallsFigure,
} from "../examples/baire-category.js";
import {
  buildBoundedCauchySequenceFigure,
  buildCircleDiameterFigure,
  buildNestedBisectionFigure,
  buildRectangleDiameterFigure,
  cauchyBisectionSubstance,
  circleDiameterSubstance,
  decreasingCauchySequence,
  rectangleDiameterSubstance,
} from "../examples/metric-completeness.js";
import { baireCategoryStyle } from "./baire-category.js";
import { cauchyBisectionStyle, diameterStyle } from "./metric-completeness.js";

async function render(d: Diagram) {
  let done = false;
  for (let i = 0; i < 3000; i++)
    if (!(await d.optimizationStep())) {
      done = true;
      break;
    }
  expect(done).toBe(true);
  const { svg } = await d.render(),
    xml = new XMLSerializer().serializeToString(svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  expect(svg.querySelectorAll("image")).toHaveLength(0);
  return svg;
}

test("the real sequence is uniformly bounded and every selected interval has infinitely many tail terms", () => {
  for (const sub of [
    cauchyBisectionSubstance(),
    cauchyBisectionSubstance(decreasingCauchySequence, 4),
  ]) {
    const sn = sub.entities.find((e) => e.label === "\\{s_n\\}") as EntityOf<
      typeof topology.EventuallyGeometricSequence
    >;
    const family = sub.entities.find(
      (e) => e.label === "\\{[a_n,b_n]\\}",
    ) as EntityOf<typeof topology.BisectionFamily>;
    for (let n = 1; n < 200; n++) {
      const value = geometricSequenceTerm(sn, n);
      expect(value).toBeGreaterThan(family.initialBounds[0]);
      expect(value).toBeLessThan(family.initialBounds[1]);
    }
    const selected = sub.propositions
      .filter((p) => p.predicate === topology.BisectionIntervalIn)
      .map((p) => p.args[0] as EntityOf<typeof topology.BisectionInterval>);
    for (const interval of selected) {
      expect(interval.a).toBeLessThan(sn.limit);
      expect(interval.b).toBeGreaterThan(sn.limit);
      for (const n of [100, 200, 1000]) {
        const value = geometricSequenceTerm(sn, n);
        expect(value).toBeGreaterThan(interval.a);
        expect(value).toBeLessThan(interval.b);
      }
    }
    for (const p of sub.propositions.filter(
      (p) => p.predicate === topology.HalfOf,
    )) {
      const [child, parent] = p.args as readonly EntityOf<
        typeof topology.BisectionInterval
      >[];
      expect(child.b - child.a).toBeCloseTo((parent.b - parent.a) / 2, 14);
    }
    for (const e of sub.entities) {
      expect(e).not.toHaveProperty("shapeType");
      expect(e).not.toHaveProperty("fillColor");
    }
  }
});

test("diameter witnesses preserve a circle boundary and translated rectangles", () => {
  const circle = circleDiameterSubstance(1.4, [2, 3]);
  const d = circle.propositions.find(
    (p) => p.predicate === topology.DiameterOf,
  )!;
  expect((d.args[0] as EntityOf<typeof topology.Diameter>).value).toBe(2.8);
  const points = circle.propositions
    .filter((p) => p.predicate === topology.Member && p.args[1] === d.args[1])
    .map((p) => p.args[0] as EntityOf<typeof topology.CoordinatePoint>);
  expect(
    Math.hypot(
      points[0].coordinates[0] - points[1].coordinates[0],
      points[0].coordinates[1] - points[1].coordinates[1],
    ),
  ).toBeCloseTo(2.8, 12);
  for (const bounds of [
    [-2, -1, 2, 1],
    [3, 4, 7, 6],
  ] as const) {
    const sub = rectangleDiameterSubstance(bounds),
      diagonal = sub.propositions.find(
        (p) => p.predicate === topology.SegmentBetween,
      )!.args[0] as EntityOf<typeof topology.LinearSegment>;
    expect(
      Math.hypot(
        diagonal.endpoints[1][0] - diagonal.endpoints[0][0],
        diagonal.endpoints[1][1] - diagonal.endpoints[0][1],
      ),
    ).toBeCloseTo(Math.sqrt(20), 12);
  }
});

test("Baire proof hypotheses do not become false cover or somewhere-density assertions", () => {
  const nested = baireNestedBallsSubstance();
  expect(
    nested.propositions.some((p) => p.predicate === topology.FamilyUnionIs),
  ).toBe(false);
  expect(
    nested.propositions.filter((p) => p.predicate === topology.Hypothesis),
  ).toHaveLength(1);
  const closures = nested.propositions.filter(
    (p) =>
      p.predicate === topology.ClosureOf &&
      (
        p.args[1] as EntityOf<typeof topology.MetricNeighborhood>
      ).label.startsWith("B_"),
  );
  expect(closures).toHaveLength(3);
  for (const [i, p] of closures.entries()) {
    const c = p.args[0] as EntityOf<typeof topology.MetricBallClosure>;
    expect(c.radius).toBeLessThan(1 / (i + 1) / 2);
    expect(c).not.toHaveProperty("center");
    expect(p.args[1]).not.toHaveProperty("center");
  }
  const dense = baireDenseIntersectionSubstance();
  expect(
    dense.propositions.some((p) => p.predicate === topology.SomewhereDenseIn),
  ).toBe(false);
  expect(
    dense.propositions.filter((p) => p.predicate === topology.Hypothesis),
  ).toHaveLength(3);
  const relative = dense.propositions.find(
    (p) => p.predicate === topology.SubspaceTopologyOf,
  )!;
  expect(
    (relative.args[1] as EntityOf<typeof topology.MetricBallClosure>).label,
  ).toBe("T=\\operatorname{Cl}N(x,p/2)");
  for (const point of dense.entities.filter((e) =>
    ["x", "t", "z"].includes(e.label),
  ))
    expect(point).not.toHaveProperty("coordinates");
  // Closed metric balls and open-ball closures remain distinct declarations.
  const s = topology.substance(),
    X = s.Set(),
    D = s.Metric(),
    x = s.Point();
  const closed = s.ClosedMetricBall({ radius: 1 }),
    closure = s.MetricBallClosure({ radius: 1 });
  s.MetricOn(D, X);
  s.ClosedBallAt(closed, x, D);
  expect(closed).not.toBe(closure);
  expect(() => baireNestedBallsSubstance([0.8, 0.6, 0.1])).toThrow();
});

test("shared styles render distinct mathematical programs through native optimization", async () => {
  const sequence = cauchyBisectionStyle(),
    diameter = diameterStyle(),
    baire = baireCategoryStyle();
  const variants = [
    [cauchyBisectionSubstance(), sequence],
    [cauchyBisectionSubstance(decreasingCauchySequence, 4), sequence],
    [circleDiameterSubstance(), diameter],
    [circleDiameterSubstance(0.7), diameter],
    [rectangleDiameterSubstance(), diameter],
    [rectangleDiameterSubstance([-1.5, -0.7, 1.5, 0.7]), diameter],
    [baireNestedBallsSubstance(), baire],
    [baireNestedBallsSubstance([0.7, 0.2, 0.04]), baire],
    [baireDenseIntersectionSubstance(), baire],
    [baireDenseIntersectionSubstance(2, 0.3, 0.06), baire],
  ] as const;
  for (const [sub, sty] of variants) {
    const d = await diagram({ sub, sty, canvas: canvas(360, 350) });
    try {
      const svg = await render(d);
      if (sub === variants[2][0])
        expect(
          svg
            .querySelector('circle[aria-label="diameter.circle"]')
            ?.getAttribute("fill-opacity"),
        ).toBe("0");
    } finally {
      d.discard();
    }
  }
});

test("all six reviewed figures support canonical and sampled native annotation dragging", async () => {
  for (const factory of [
    buildBoundedCauchySequenceFigure,
    buildNestedBisectionFigure,
    buildCircleDiameterFigure,
    buildRectangleDiameterFigure,
    buildBaireNestedBallsFigure,
    buildBaireDenseIntersectionFigure,
  ]) {
    for (const jitter of [0, 4]) {
      const d = await factory({
        interactive: { jitter },
        variation: "completeness-drag",
      });
      try {
        const before = await render(d),
          handles = [...d.getDraggingConstraints().keys()];
        expect(handles.length).toBeGreaterThan(0);
        if (jitter === 0)
          for (const h of handles) {
            expect(d.getInput(h + ".layout.x")).toBeCloseTo(0, 9);
            expect(d.getInput(h + ".layout.y")).toBeCloseTo(0, 9);
          }
        const h = handles[0],
          x = d.getInput(h + ".layout.x"),
          y = d.getInput(h + ".layout.y");
        const geometry = (svg: SVGSVGElement) =>
          Array.from(svg.querySelectorAll("path,line,circle,polygon")).map(
            (e) => new XMLSerializer().serializeToString(e),
          );
        const fixed = geometry(before);
        d.beginDrag(h);
        d.translate(h, 3, 1);
        expect(d.getInput(h + ".layout.x")).toBeCloseTo(x + 3, 9);
        expect(d.getInput(h + ".layout.y")).toBeCloseTo(y + 1, 9);
        d.endDrag(h);
        const after = await render(d);
        // Sampled layouts retain collision constraints, so releasing a label
        // on top of a point can relax its position. Canonical layout is exact.
        if (jitter === 0) {
          expect(d.getInput(h + ".layout.x")).toBeCloseTo(x + 3, 4);
          expect(d.getInput(h + ".layout.y")).toBeCloseTo(y + 1, 4);
        } else {
          expect(Math.abs(d.getInput(h + ".layout.x"))).toBeLessThanOrEqual(
            16.01,
          );
          expect(Math.abs(d.getInput(h + ".layout.y"))).toBeLessThanOrEqual(
            16.01,
          );
        }
        expect(geometry(after)).toEqual(fixed);
      } finally {
        d.discard();
      }
    }
  }
});
