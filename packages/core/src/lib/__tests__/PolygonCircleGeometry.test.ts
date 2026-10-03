import { describe, expect, test } from "vitest";
import { genGradient, problem, variable } from "../../engine/Autodiff.js";
import { add, squared, sub } from "../../engine/AutodiffFunctions.js";
import { Circle, makeCircle } from "../../shapes/Circle.js";
import { Polygon, makePolygon } from "../../shapes/Polygon.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import * as ad from "../../types/ad.js";
import { floatV, ptListV, vectorV } from "../../utils/Util.js";
import { disjoint, overlapping } from "../Constraints.js";
import { shapeDistance } from "../Queries.js";
import { numOf } from "../Utils.js";

const context = simpleContext("simple polygon circle distance"),
  canvas = makeCanvas(20, 20);
const vertices: [number, number][] = [
  [0, 0],
  [4, 0],
  [4, 1],
  [1, 1],
  [1, 4],
  [0, 4],
];
const polygon = (scale: ad.Num = 1, points: ad.Pt2[] = vertices) =>
  makePolygon(context, canvas, {
    points: ptListV(points),
    scale: floatV(scale),
    strokeWidth: floatV(0),
  });
const circle = (center: ad.Pt2, radius: ad.Num = 0.2) =>
  makeCircle(context, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(0),
  });

const checkBoth = (p: Polygon<ad.Num>, c: Circle<ad.Num>, expected: number) => {
  for (const pair of [
    [p, c],
    [c, p],
  ] as const) {
    const result = shapeDistance(pair[0], pair[1]);
    expect(result.warnings).toHaveLength(0);
    expect(numOf(result.value)).toBeCloseTo(expected, 10);
    expect(numOf(disjoint(pair[0], pair[1], 0).value)).toBeCloseTo(
      -expected,
      10,
    );
    expect(numOf(overlapping(pair[0], pair[1], 0).value)).toBeCloseTo(
      expected,
      10,
    );
  }
};

describe("simple filled Polygon versus Circle clearance", () => {
  test("distinguishes the unfilled concave notch from the filled arms", () => {
    checkBoth(polygon(), circle([2, 2]), 0.8);
    checkBoth(polygon(), circle([3, 0.4]), -0.6);
    checkBoth(polygon(), circle([4.2, 0.5]), 0);
    checkBoth(polygon(), circle([4.1, 0.5]), -0.1);
  });

  test("uses Euclidean clearance at a concave corner and honors optional margins", () => {
    const p = polygon(),
      c = circle([1.3, 1.4]);
    checkBoth(p, c, 0.1);
    expect(numOf(disjoint(p, c, 0.15).value)).toBeCloseTo(0.05, 10);
    expect(numOf(overlapping(p, c, 0.15).value)).toBeCloseTo(0.25, 10);
    // The nearest outer boundary from this point is the concave corner.
    checkBoth(polygon(), circle([0.8, 0.8], 0.2), -Math.sqrt(0.08) - 0.2);
  });

  test.each([2, -2, 0])(
    "keeps scale %p consistent for both argument orders",
    (scale) => {
      const c = circle(scale === 0 ? [3, 4] : [2 * scale, 2 * scale]);
      checkBoth(polygon(scale), c, scale === 0 ? 4.8 : Math.abs(scale) - 0.2);
    },
  );

  test("supports reversed vertex order and duplicate closing/adjacent points", () => {
    for (const points of [
      [...vertices].reverse(),
      [...vertices, vertices[0]],
      [vertices[0], ...vertices],
    ]) {
      checkBoth(polygon(1, points), circle([2, 2]), 0.8);
    }
  });

  test("optimization does not push a valid notch placement out of the polygon's bounding box", async () => {
    const x = variable(2),
      y = variable(2);
    const objective = add(squared(sub(x, 2)), squared(sub(y, 2)));
    const constraint = disjoint(polygon(), circle([x, y]), 0.1).value;
    const run = (await problem({ objective, constraints: [constraint] }))
      .start({})
      .run({});
    expect(run.converged).toBe(true);
    expect(run.vals.get(x)).toBeCloseTo(2, 5);
    expect(run.vals.get(y)).toBeCloseTo(2, 5);
  });

  test("retains live scale, circle coordinates and radius gradients", async () => {
    const s = variable(1.2),
      x = variable(3),
      y = variable(2.4),
      r = variable(0.2);
    const expression = shapeDistance(polygon(s), circle([x, y], r)).value;
    const fn = await genGradient([s, x, y, r], [expression], []),
      gradient = new Float64Array(4);
    const masks = {
      inputMask: [true, true, true, true],
      objMask: [true],
      constrMask: [],
    };
    const input = [1.2, 3, 2.4, 0.2];
    const at = (values: number[]) =>
      fn(masks, new Float64Array(values), 1, gradient).phi;
    expect(at(input)).toBeCloseTo(1, 10);
    const analytic = Array.from(gradient);
    for (let i = 0; i < input.length; i++) {
      const a = [...input],
        b = [...input],
        h = 1e-5;
      a[i] -= h;
      b[i] += h;
      expect(analytic[i]).toBeCloseTo((at(b) - at(a)) / (2 * h), 5);
    }
    [-1, 0, 1, -1].forEach((expected, i) =>
      expect(analytic[i]).toBeCloseTo(expected, 12),
    );
  });
});
