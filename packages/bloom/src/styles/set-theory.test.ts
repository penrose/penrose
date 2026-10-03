import { afterEach, describe, expect, test, vi } from "vitest";
import * as constraints from "../core/constraints.js";
import type { Diagram } from "../core/diagram.js";
import { diagram, domain } from "../core/program.js";
import { ShapeType } from "../core/types.js";
import { canvas } from "../core/utils.js";
import { declareSetTheory, setTheory } from "../domains/set-theory.js";
import { eulerVennStyle, eulerVennStyleFor } from "./set-theory.js";

afterEach(() => {
  vi.restoreAllMocks();
});

const optimize = async (drawing: Diagram) => {
  for (let step = 0; step < 2000; step++) {
    if (!(await drawing.optimizationStep())) return;
  }
  throw new Error("Set diagram did not finish optimizing");
};

const geometry = (svg: SVGElement, kind: "Set" | "Point", label: string) => {
  const circle = Array.from(svg.querySelectorAll("circle")).find(
    (element) => element.getAttribute("aria-label") === `${kind} ${label}`,
  );
  expect(circle, `${kind} ${label}`).toBeDefined();
  return {
    x: Number(circle!.getAttribute("cx")),
    y: Number(circle!.getAttribute("cy")),
    r: Number(circle!.getAttribute("r")),
  };
};
const distance = (
  a: ReturnType<typeof geometry>,
  b: ReturnType<typeof geometry>,
) => Math.hypot(a.x - b.x, a.y - b.y);

describe("reusable set theory programs", () => {
  test("keeps signatures and ownership independent of visual programs", () => {
    const sub = setTheory.substance();
    const a = sub.Set({ label: "A" });
    const x = sub.Point({ label: "x" });
    sub.Member(x, a);
    sub.Subset(a, a);
    expect(a).not.toHaveProperty("icon");
    expect(x).not.toHaveProperty("center");
    expect(() => Reflect.apply(sub.Subset, undefined, [x, a])).toThrow(
      "Expected Set, got Point",
    );
    expect(() => Reflect.apply(sub.Member, undefined, [x])).toThrow(
      "expects 2 arguments",
    );

    const topologyBuilder = domain("point-set-topology");
    const sets = declareSetTheory(topologyBuilder);
    const OpenSet = topologyBuilder.type("OpenSet", sets.Set);
    const topology = topologyBuilder.make({ ...sets, OpenSet });
    const another = topology.substance();
    const u = another.OpenSet({ label: "U" });
    const p = another.Point({ label: "p" });
    another.Member(p, u);
    expect(eulerVennStyleFor(topology).domain).toBe(topology);
    expect(() => Reflect.apply(sub.Subset, undefined, [a, u])).toThrow(
      "this substance",
    );
    expect(() => topologyBuilder.type("AfterSealing")).toThrow(
      "after domain.make()",
    );
  });

  test("optimizes two shape-free substances with one shared TSX style", async () => {
    const style = eulerVennStyle();

    // Reverse insertion order tests the direction of the subset fact. A
    // reflexive fact must not request a circle to strictly contain itself.
    const nested = setTheory.substance();
    const b = nested.Set({ label: "B" });
    const a = nested.Set({ label: "A" });
    const x = nested.Point({ label: "x" });
    nested.Subset(b, a);
    nested.Subset(a, a);
    nested.Member(x, b);
    const first = await diagram({
      sub: nested.make(),
      sty: style,
      canvas: canvas(500, 360),
      variation: "nested-sets",
    });
    try {
      await optimize(first);
      const { svg } = await first.render();
      const outer = geometry(svg, "Set", "A");
      const inner = geometry(svg, "Set", "B");
      const dot = geometry(svg, "Point", "x");
      expect(distance(outer, inner) + inner.r + 6).toBeLessThanOrEqual(
        outer.r + 0.25,
      );
      expect(distance(dot, inner) + dot.r + 6).toBeLessThanOrEqual(
        inner.r + 0.25,
      );
      expect(svg.querySelectorAll("circle")).toHaveLength(3);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      expect(a).not.toHaveProperty("icon");
    } finally {
      first.discard();
    }

    const disjoint = vi.spyOn(constraints, "disjoint");
    const overlapping = vi.spyOn(constraints, "overlapping");
    const relations = setTheory.substance();
    const left = relations.Set({ label: "L" });
    const right = relations.Set({ label: "R" });
    const separate = relations.Set({ label: "S" });
    const p = relations.Point({ label: "p" });
    relations.Intersecting(left, right);
    relations.Intersecting(right, left);
    relations.Intersecting(left, left);
    relations.Disjoint(left, separate);
    relations.Disjoint(separate, left);
    relations.Disjoint(right, separate);
    relations.Disjoint(separate, right);
    relations.Member(p, left);
    relations.Member(p, right);
    const substance = relations.make();
    const second = await diagram({
      sub: substance,
      sty: style,
      canvas: canvas(500, 360),
      variation: "intersecting-and-disjoint-sets",
    });
    try {
      expect(
        disjoint.mock.calls.filter(
          ([firstShape, secondShape]) =>
            firstShape.shapeType === ShapeType.Circle &&
            secondShape.shapeType === ShapeType.Circle,
        ),
      ).toHaveLength(2);
      expect(overlapping).toHaveBeenCalledTimes(1);
      expect(substance.propositions).toHaveLength(9);
      await optimize(second);
      const { svg } = await second.render();
      const l = geometry(svg, "Set", "L");
      const r = geometry(svg, "Set", "R");
      const s = geometry(svg, "Set", "S");
      const dot = geometry(svg, "Point", "p");
      expect(distance(l, r)).toBeLessThanOrEqual(l.r + r.r - 6 + 0.25);
      for (const region of [l, r]) {
        expect(distance(region, s)).toBeGreaterThanOrEqual(
          region.r + s.r + 6 - 0.25,
        );
        expect(distance(dot, region) + dot.r + 6).toBeLessThanOrEqual(
          region.r + 0.25,
        );
      }
      expect(svg.querySelectorAll("circle")).toHaveLength(4);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    } finally {
      second.discard();
    }
  }, 30000);

  test("rejects relations the circle representation cannot satisfy", async () => {
    const sub = setTheory.substance();
    const a = sub.Set({ label: "A" });
    sub.Disjoint(a, a);
    await expect(
      diagram({ sub: sub.make(), sty: eulerVennStyle() }),
    ).rejects.toThrow("empty-set representation");
    expect(() => eulerVennStyle({ preferredRadius: -1 })).toThrow(
      "finite positive preferredRadius",
    );
  });
});

/** Compile-time checks preserve the mathematical signatures and domain brands. */
const checkSetTheoryTypes = () => {
  const sub = setTheory.substance();
  const a = sub.Set();
  const x = sub.Point();
  sub.Member(x, a);
  // @ts-expect-error A set is not a point.
  sub.Member(a, a);
  // @ts-expect-error A point is not a set.
  sub.Subset(x, a);
  // @ts-expect-error Predicate arity is retained.
  sub.Member(x);
  // @ts-expect-error Style fields are absent from mathematical entities.
  a.icon;
  const builder = domain("another-set-domain");
  const foreign = builder.make(declareSetTheory(builder)).substance();
  // @ts-expect-error Composed domains retain their own descriptor brands.
  sub.Subset(foreign.Set(), a);
};
void checkSetTheoryTypes;
