import { afterEach, describe, expect, test, vi } from "vitest";
import { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { metricSpaces } from "../domains/metric-spaces.js";
import { metricComparisonStyle } from "./metric-comparison.js";

afterEach(() => {
  vi.restoreAllMocks();
});

function substance(
  innerRadius = 1 / Math.SQRT2,
  middleCenter: readonly [number, number] = [0, 0],
) {
  const sub = metricSpaces.substance();
  const e = sub.MetricPlane({ metric: "euclidean" });
  const t = sub.MetricPlane({ metric: "taxicab" });
  const inner = sub.Neighborhood({ point: [0, 0], rho: innerRadius });
  const middle = sub.Neighborhood({ point: middleCenter, rho: 1 });
  const outer = sub.Neighborhood({ point: [0, 0], rho: 1 });
  sub.InSpace(inner, e);
  sub.InSpace(middle, t);
  sub.InSpace(outer, e);
  sub.NeighborhoodContainedIn(inner, middle);
  sub.NeighborhoodContainedIn(middle, outer);
  return sub.make();
}

describe("Figure 2.5 metric comparison", () => {
  test("the Euclidean circle is tangent to the taxicab diamond at its side midpoint", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    await diagram({
      sub: substance(),
      sty: metricComparisonStyle({ unit: 80 }),
    });
    const shapes = create.mock.calls[0][0].shapes;
    const outer = shapes.find((s) => s.name.contents === "comparison.outer");
    const inner = shapes.find((s) => s.name.contents === "comparison.inner");
    const diamond = shapes.find(
      (s) => s.name.contents === "comparison.taxicab",
    );
    const radius = shapes.find((s) => s.name.contents === "comparison.radius");
    if (
      outer?.shapeType !== "Circle" ||
      inner?.shapeType !== "Circle" ||
      diamond?.shapeType !== "Polygon" ||
      radius?.shapeType !== "Line"
    )
      throw new Error("Missing comparison geometry");
    expect(outer.r.contents).toBe(80);
    expect(inner.r.contents).toBeCloseTo(80 / Math.SQRT2);
    expect(diamond.points.contents).toEqual([
      [0, 80],
      [80, 0],
      [0, -80],
      [-80, 0],
    ]);
    expect(radius.end.contents[0]).toBeCloseTo(40);
    expect(radius.end.contents[1]).toBeCloseTo(40);
    expect(diamond.strokeDasharray.contents).toBe("5 4");
  });

  test("rejects false containment and nonconcentric facts", async () => {
    await expect(
      diagram({ sub: substance(0.8), sty: metricComparisonStyle() }),
    ).rejects.toThrow("containment is false");
    await expect(
      diagram({ sub: substance(0.5, [1, 0]), sty: metricComparisonStyle() }),
    ).rejects.toThrow("concentric");
  });

  test("renders circles, a dashed diamond, coordinate labels and a radius guide", async () => {
    const result = await diagram({
      sub: substance(),
      sty: metricComparisonStyle(),
      canvas: canvas(360, 300),
    });
    const { svg } = await result.render();
    expect(svg.querySelectorAll("circle")).toHaveLength(6);
    expect(svg.querySelectorAll("polygon")).toHaveLength(1);
    expect(svg.querySelectorAll("line")).toHaveLength(3);
    expect(svg.querySelector("polygon")?.getAttribute("stroke-dasharray")).toBe(
      "5 4",
    );
    expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    result.discard();
  });
});
