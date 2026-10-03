import { describe, expect, test } from "vitest";
import { genGradient, ops, problem, variable } from "../../engine/Autodiff.js";
import { add, max, mul, squared, sub } from "../../engine/AutodiffFunctions.js";
import { makeCircle } from "../../shapes/Circle.js";
import { makeRectangle } from "../../shapes/Rectangle.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import * as ad from "../../types/ad.js";
import { floatV, vectorV } from "../../utils/Util.js";
import {
  contains,
  containsCircleRect,
  containsPolyCircle,
  containsPolyPoint,
} from "../Constraints.js";
import { signedDistanceLine, signedDistancePolygon } from "../Functions.js";
import { numOf } from "../Utils.js";

// A simple, nonconvex neighborhood: the union of two rectangular arms.
const elbow: ad.Pt2[] = [
  [0, 0],
  [4, 0],
  [4, 1],
  [1, 1],
  [1, 4],
  [0, 4],
];

const gradientMatches = async (
  initial: number[],
  expression: (xs: ad.Var[]) => ad.Num,
) => {
  const inputs = initial.map(variable);
  const fn = await genGradient(inputs, [expression(inputs)], []);
  const masks = {
    inputMask: initial.map(() => true),
    objMask: [true],
    constrMask: [],
  };
  const gradient = new Float64Array(initial.length);
  const at = (xs: number[]) => fn(masks, new Float64Array(xs), 1, gradient).phi;
  expect(Number.isFinite(at(initial))).toBe(true);
  const analytic = Array.from(gradient);
  for (let i = 0; i < initial.length; i++) {
    const step = 1e-5;
    const before = [...initial],
      after = [...initial];
    before[i] -= step;
    after[i] += step;
    const numerical = (at(after) - at(before)) / (2 * step);
    expect(Number.isFinite(analytic[i])).toBe(true);
    expect(analytic[i]).toBeCloseTo(numerical, 5);
  }
};

describe("simple polygon containment", () => {
  test("contains points and disks in either arm, but excludes the notch", () => {
    expect(numOf(containsPolyPoint(elbow, [3, 0.5], 0))).toBeLessThanOrEqual(0);
    expect(numOf(containsPolyPoint(elbow, [0.5, 3], 0))).toBeLessThanOrEqual(0);
    expect(numOf(containsPolyPoint(elbow, [2, 2], 0))).toBeGreaterThan(0);
    expect(
      numOf(containsPolyCircle(elbow, [3, 0.5], 0.3, 0.1)),
    ).toBeLessThanOrEqual(0);
    expect(numOf(containsPolyCircle(elbow, [3, 0.5], 0.6, 0))).toBeGreaterThan(
      0,
    );
  });

  test("optimization retains a feasible point near its preferred position in a concave arm", async () => {
    const x = variable(3),
      y = variable(0.4);
    const containment = containsPolyPoint(elbow, [x, y], 0.1);
    const preference = add(squared(sub(x, 3)), squared(sub(y, 0.4)));
    // This is the same fixed-weight problem used to reproduce the old
    // partition-intersection bug, which incorrectly pushed x from 3 to ~1.
    const objective = add(preference, mul(1000, squared(max(0, containment))));
    const run = (await problem({ objective })).start({}).run({});
    expect(run.converged).toBe(true);
    const position = [run.vals.get(x)!, run.vals.get(y)!];
    expect(position[0]).toBeCloseTo(3, 3);
    expect(position[1]).toBeCloseTo(0.4, 3);
    expect(
      numOf(containsPolyPoint(elbow, position as ad.Pt2, 0.1)),
    ).toBeLessThanOrEqual(1e-6);
  });

  test("optimization moves an outside disk into a feasible arm with positive clearance", async () => {
    const x = variable(3),
      y = variable(2);
    const preference = add(squared(sub(x, 3)), squared(sub(y, 0.4)));
    const constraint = containsPolyCircle(elbow, [x, y], 0.2, 0.1);
    const run = (
      await problem({ objective: preference, constraints: [constraint] })
    )
      .start({})
      .run({});
    expect(run.converged).toBe(true);
    expect(run.vals.get(x)).toBeCloseTo(3, 2);
    expect(run.vals.get(y)).toBeCloseTo(0.4, 2);
    expect(
      numOf(
        containsPolyCircle(
          elbow,
          [run.vals.get(x)!, run.vals.get(y)!],
          0.2,
          0.1,
        ),
      ),
    ).toBeLessThanOrEqual(1e-5);
  });

  test("respects orientation, repeated closing points, boundaries and the concave corner", () => {
    for (const points of [
      elbow,
      [...elbow].reverse(),
      [...elbow, elbow[0]],
      [elbow[0], elbow[0], ...elbow.slice(1)],
    ]) {
      expect(numOf(containsPolyPoint(points, [3, 0.5], 0))).toBeCloseTo(
        -0.5,
        12,
      );
      expect(numOf(containsPolyPoint(points, [1, 1], 0))).toBeCloseTo(0, 12);
      expect(numOf(containsPolyPoint(points, [1, 1], 0.1))).toBeCloseTo(
        0.1,
        12,
      );
      expect(numOf(containsPolyPoint(points, [2, 2], 0))).toBeCloseTo(1, 12);
      // The disk crosses no outer boundary even though it straddles an
      // internal convex-partition edge near the concave corner.
      expect(
        numOf(containsPolyCircle(points, [0.8, 0.8], 0.25, 0)),
      ).toBeLessThan(0);
      expect(
        numOf(containsPolyCircle(points, [0.8, 0.8], 0.3, 0)),
      ).toBeGreaterThan(0);
    }
  });

  test.each([1e-6, 1, 1e6])(
    "has geometrically scaled distances at scale %p",
    (scale) => {
      const points = elbow.map(
        ([x, y]) => [Number(x) * scale, Number(y) * scale] as ad.Pt2,
      );
      expect(
        numOf(
          containsPolyPoint(points, [3 * scale, 0.4 * scale], 0.1 * scale),
        ) / scale,
      ).toBeCloseTo(-0.3, 10);
      expect(
        numOf(containsPolyPoint(points, [2 * scale, 2 * scale], 0.1 * scale)) /
          scale,
      ).toBeCloseTo(1.1, 10);
    },
  );

  test("keeps gradients through point, padding, polygon translation, scale and rotation", async () => {
    await gradientMatches([3, 0.4, 0.1], ([x, y, padding]) =>
      containsPolyPoint(elbow, [x, y], padding),
    );
    await gradientMatches(
      [0.2, -0.1, 1.1, 13, 0.05, 0.1],
      ([dx, dy, scale, rotation, radius, padding]) => {
        const polygon = elbow.map(
          (p) =>
            ops.vadd(ops.vmul(scale, ops.vrot(p, rotation)), [
              dx,
              dy,
            ]) as ad.Pt2,
        );
        return containsPolyCircle(polygon, [2.8, 0.2], radius, padding);
      },
    );
    // A live formerly-concave vertex can become convex after graph creation.
    // No fixed convex decomposition is carried into this evaluation.
    const vertexX = variable(1),
      vertexY = variable(1);
    const polygon: ad.Pt2[] = [
      [0, 0],
      [4, 0],
      [4, 1],
      [vertexX, vertexY],
      [1, 4],
      [0, 4],
    ];
    const fn = await genGradient(
      [vertexX, vertexY],
      [containsPolyPoint(polygon, [2, 2], 0)],
      [],
    );
    const gradient = new Float64Array(2);
    const masks = { inputMask: [true, true], objMask: [true], constrMask: [] };
    expect(
      fn(masks, new Float64Array([1, 1]), 1, gradient).phi,
    ).toBeGreaterThan(0);
    expect(fn(masks, new Float64Array([4, 4]), 1, gradient).phi).toBeLessThan(
      0,
    );
    expect(Array.from(gradient).every(Number.isFinite)).toBe(true);
  });

  test("boundary and vertex evaluations retain finite AD values and gradients", async () => {
    const x = variable(0),
      y = variable(0);
    const fn = await genGradient(
      [x, y],
      [containsPolyPoint(elbow, [x, y], 0)],
      [],
    );
    const gradient = new Float64Array(2),
      masks = { inputMask: [true, true], objMask: [true], constrMask: [] };
    for (const point of [
      [3, 0],
      [1, 1],
      [4, 1],
      [0, 0],
    ]) {
      expect(fn(masks, new Float64Array(point), 1, gradient).phi).toBeCloseTo(
        0,
        12,
      );
      expect(Array.from(gradient).every(Number.isFinite)).toBe(true);
    }
  });
});

describe("circle contains rectangle", () => {
  test("uses the actual corners and honors clearance", () => {
    const corners: ad.Pt2[] = [
      [3, 4],
      [-3, 4],
      [-3, -4],
      [3, -4],
    ];
    expect(numOf(containsCircleRect([0, 0], 5, corners, 0))).toBeCloseTo(0);
    expect(numOf(containsCircleRect([0, 0], 5, corners, 0.25))).toBeCloseTo(
      0.25,
    );
    expect(numOf(containsCircleRect([0, 0], 5.5, corners, 0.25))).toBeCloseTo(
      -0.25,
    );
  });

  test("native shape dispatch uses rotated rectangle corners rather than its larger AABB", () => {
    const context = simpleContext("rotated containment"),
      canvas = makeCanvas(20, 20);
    const circle = makeCircle(context, canvas, {
      center: vectorV([0, 0]),
      r: floatV(5),
    });
    const rectangle = makeRectangle(context, canvas, {
      center: vectorV([0, 0]),
      width: floatV(6),
      height: floatV(8),
      rotation: floatV(45),
      strokeWidth: floatV(0),
    });
    expect(numOf(contains(circle, rectangle, 0).value)).toBeCloseTo(0, 12);
    expect(numOf(contains(circle, rectangle, 0.1).value)).toBeCloseTo(0.1, 12);
    expect(contains(circle, rectangle, 0).warnings).toHaveLength(0);
  });

  test("differentiates center, radius, corner coordinates and padding", async () => {
    await gradientMatches(
      [0.3, -0.5, 4.6, 3.2, 3.7, 0.1],
      ([cx, cy, r, x, y, padding]) =>
        containsCircleRect(
          [cx, cy],
          r,
          [
            [x, y],
            [-3, 4],
            [-3, -4],
            [3, -4],
          ],
          padding,
        ),
    );
  });

  test("rejects a malformed rectangle and supports a collapsed one", () => {
    expect(() => containsCircleRect([0, 0], 1, [[0, 0]], 0)).toThrow("four");
    expect(
      numOf(
        containsCircleRect(
          [0, 0],
          1,
          [
            [0, 0],
            [0, 0],
            [0, 0],
            [0, 0],
          ],
          0,
        ),
      ),
    ).toBe(-1);
  });
});

describe("collapsed segment distances", () => {
  test("treats a zero-length segment and a collapsed polygon as a point", async () => {
    expect(numOf(signedDistanceLine([1, 2], [1, 2], [4, 6]))).toBe(5);
    expect(
      numOf(
        signedDistancePolygon(
          [
            [1, 2],
            [1, 2],
            [1, 2],
          ],
          [4, 6],
        ),
      ),
    ).toBe(5);
    expect(numOf(signedDistancePolygon([[1, 2]], [1, 2]))).toBe(0);
    expect(() => signedDistancePolygon([], [0, 0])).toThrow(
      "at least one point",
    );
    await gradientMatches([4, 6], ([x, y]) =>
      signedDistanceLine([1, 2], [1, 2], [x, y]),
    );
    await gradientMatches([4, 6], ([x, y]) =>
      containsPolyPoint(
        [
          [1, 2],
          [1, 2],
          [1, 2],
        ],
        [x, y],
        0.1,
      ),
    );
  });

  test("a live segment can collapse without NaN values or gradients", async () => {
    const endpoint = variable(2),
      x = variable(3),
      y = variable(4);
    const fn = await genGradient(
      [endpoint, x, y],
      [signedDistanceLine([0, 0], [endpoint, 0], [x, y])],
      [],
    );
    const gradient = new Float64Array(3),
      masks = {
        inputMask: [true, true, true],
        objMask: [true],
        constrMask: [],
      };
    expect(fn(masks, new Float64Array([0, 3, 4]), 1, gradient).phi).toBe(5);
    expect(Array.from(gradient).every(Number.isFinite)).toBe(true);
    expect(gradient[1]).toBeCloseTo(0.6, 12);
    expect(gradient[2]).toBeCloseTo(0.8, 12);
    expect(fn(masks, new Float64Array([2, 3, 4]), 1, gradient).phi).toBeCloseTo(
      Math.sqrt(17),
      12,
    );
  });
});
