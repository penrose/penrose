// @vitest-environment jsdom
import { describe, expect, test } from "vitest";
import { compile, genGradient, variable } from "../../engine/Autodiff.js";
import RenderPolygon from "../../renderer/Polygon.js";
import RenderPolyline from "../../renderer/Polyline.js";
import { makeCircle } from "../../shapes/Circle.js";
import { Polygon, makePolygon } from "../../shapes/Polygon.js";
import { Polyline, makePolyline } from "../../shapes/Polyline.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import * as ad from "../../types/ad.js";
import { black, floatV, noPaint, ptListV, vectorV } from "../../utils/Util.js";
import { contains, disjoint } from "../Constraints.js";
import { bboxFromShape, polygonLikePoints } from "../Queries.js";
import { numOf } from "../Utils.js";

const points: [number, number][] = [
  [1, 1],
  [3, 1],
  [3, 2],
  [1, 2],
];
const context = simpleContext("scaled native geometry"),
  canvas = makeCanvas(100, 80);
const polygon = (scale: ad.Num) =>
  makePolygon(context, canvas, {
    points: ptListV(points),
    scale: floatV(scale),
    strokeWidth: floatV(3),
    strokeColor: black(),
    fillColor: noPaint(),
  });
const polyline = (scale: ad.Num) =>
  makePolyline(context, canvas, {
    points: ptListV(points.slice(0, 2)),
    scale: floatV(scale),
    strokeWidth: floatV(3),
    strokeColor: black(),
    fillColor: noPaint(),
  });
const props = {
  canvasSize: [100, 80] as [number, number],
  namespace: "scale-test",
  variation: "scale-test",
  labels: new Map(),
  texLabels: false,
  pathResolver: async () => undefined,
};

describe("Polygon and Polyline scale in Penrose coordinates", () => {
  test.each([1, 2, -2, 0])(
    "rendered points, query vertices and bbox agree at scale %p",
    (scale) => {
      for (const shape of [polygon(scale), polyline(scale)]) {
        // All rendering properties in these fixed fixtures contain numbers.
        const rendered =
          shape.shapeType === "Polygon"
            ? RenderPolygon(shape as Polygon<number>, props)
            : RenderPolyline(shape as Polyline<number>, props);
        const expected = shape.points.contents.map(([x, y]) => [
          50 + scale * Number(x),
          40 - scale * Number(y),
        ]);
        expect(rendered.getAttribute("points")).toBe(expected.toString());
        expect(rendered.getAttribute("transform")).toBe(
          scale === 1 ? "scale(1)" : null,
        );
        const query = polygonLikePoints(shape).map((p) => p.map(numOf));
        expect(query).toEqual(
          shape.points.contents.map((p) => p.map((x) => scale * Number(x))),
        );
        const box = bboxFromShape(shape);
        expect(numOf(box.width)).toBe(
          Math.max(...query.map((p) => p[0])) -
            Math.min(...query.map((p) => p[0])),
        );
        expect(numOf(box.height)).toBe(
          Math.max(...query.map((p) => p[1])) -
            Math.min(...query.map((p) => p[1])),
        );
        expect(box.center.map(numOf)).toEqual([
          (Math.max(...query.map((p) => p[0])) +
            Math.min(...query.map((p) => p[0]))) /
            2,
          (Math.max(...query.map((p) => p[1])) +
            Math.min(...query.map((p) => p[1]))) /
            2,
        ]);
        // Native scale changes geometry. Stroke width remains a screen-space
        // width and is excluded from polygon query/BBox geometry as before.
        expect(rendered.getAttribute("stroke-width")).toBe("3");
        expect(rendered.getAttribute("scale")).toBeNull();
      }
    },
  );

  test.each([2, -2])(
    "generic containment and disjoint dispatch use scale %p",
    (scale) => {
      const region = polygon(scale);
      const inside = makeCircle(context, canvas, {
        center: vectorV([2 * scale, 1.5 * scale]),
        r: floatV(0.2),
      });
      const outside = makeCircle(context, canvas, {
        center: vectorV([4 * scale, 1.5 * scale]),
        r: floatV(0.2),
      });
      expect(numOf(contains(region, inside, 0.1).value)).toBeLessThan(0);
      expect(numOf(contains(region, outside, 0.1).value)).toBeGreaterThan(0);
      const second = makePolygon(context, canvas, {
        points: ptListV([
          [2 * scale, 1.2 * scale],
          [2.1 * scale, 1.2 * scale],
          [2.1 * scale, 1.3 * scale],
        ]),
        scale: floatV(1),
      });
      expect(numOf(disjoint(region, second, 0).value)).toBeGreaterThan(0);
      const lineCircle = makeCircle(context, canvas, {
        center: vectorV([2 * scale, scale]),
        r: floatV(0.2),
      });
      expect(
        numOf(disjoint(polyline(scale), lineCircle, 0).value),
      ).toBeGreaterThan(0);
    },
  );

  test("zero scale collapses containment and polyline distance to the origin", () => {
    const atOrigin = makeCircle(context, canvas, {
      center: vectorV([0, 0]),
      r: floatV(0),
    });
    const away = makeCircle(context, canvas, {
      center: vectorV([1, 0]),
      r: floatV(0.1),
    });
    expect(numOf(contains(polygon(0), atOrigin, 0).value)).toBe(0);
    expect(numOf(contains(polygon(0), away, 0).value)).toBeCloseTo(1.1, 12);
    expect(numOf(disjoint(polyline(0), away, 0).value)).toBeLessThan(0);
    expect(numOf(disjoint(polyline(0), atOrigin, 0).value)).toBeCloseTo(0, 12);
  });

  test("keeps both scale and vertex positions live in the compiled containment graph", async () => {
    const scale = variable(2),
      vertexX = variable(3);
    const shape = makePolygon(context, canvas, {
      points: ptListV([
        [1, 1],
        [vertexX, 1],
        [vertexX, 2],
        [1, 2],
      ]),
      scale: floatV(scale),
    });
    const probe = makeCircle(context, canvas, {
      center: vectorV([5.5, 3]),
      r: floatV(0.1),
    });
    const residual = contains(shape, probe, 0).value;
    const graph = await compile([residual]);
    const at = (s: number, x: number) =>
      graph((v) => (v === scale ? s : v === vertexX ? x : v.val))[0];
    expect(at(2, 3)).toBeLessThan(0);
    expect(at(1, 3)).toBeGreaterThan(0);
    expect(at(2, 2)).toBeGreaterThan(0);
    const fn = await genGradient([scale, vertexX], [residual], []),
      gradient = new Float64Array(2);
    fn(
      { inputMask: [true, true], objMask: [true], constrMask: [] },
      new Float64Array([2, 3]),
      1,
      gradient,
    );
    expect(gradient[0]).toBeCloseTo(-3, 10);
    expect(gradient[1]).toBeCloseTo(-2, 10);
    const h = 1e-5;
    expect(gradient[0]).toBeCloseTo((at(2 + h, 3) - at(2 - h, 3)) / (2 * h), 5);
    expect(gradient[1]).toBeCloseTo((at(2, 3 + h) - at(2, 3 - h)) / (2 * h), 5);
  });
});
