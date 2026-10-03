import { afterEach, describe, expect, test, vi } from "vitest";
import { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { metricSpaces } from "../domains/metric-spaces.js";
import { functionNeighborhoodStyle } from "./function-neighborhoods.js";

afterEach(() => {
  vi.restoreAllMocks();
});

function substance(rho = 0.04) {
  const sub = metricSpaces.substance();
  const space = sub.FunctionSpace({
    metric: "uniform",
    domain: [0, 1],
    codomain: [0, 1],
  });
  const f = sub.ScalarFunction({ label: "f" });
  const ball = sub.FunctionNeighborhood({ rho });
  sub.FunctionInSpace(f, space);
  sub.FunctionNeighborhoodOf(ball, f);
  return sub.make();
}

describe("Figure 2.6 uniform function neighborhood", () => {
  test("collar thickness is a constant vertical distance from the graph", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    await diagram({
      sub: substance(0.1),
      sty: functionNeighborhoodStyle({
        sample: (x) => 0.2 + x / 2,
        height: 200,
        samples: 12,
      }),
    });
    const shapes = create.mock.calls[0][0].shapes;
    const graph = shapes.find((s) => s.name.contents === "function.graph");
    const upper = shapes.find((s) => s.name.contents === "function.upper");
    const lower = shapes.find((s) => s.name.contents === "function.lower");
    if (
      graph?.shapeType !== "Polyline" ||
      upper?.shapeType !== "Polyline" ||
      lower?.shapeType !== "Polyline"
    )
      throw new Error("Missing collar curves");
    graph.points.contents.forEach(([x, y], i) => {
      expect(upper.points.contents[i][0]).toBe(x);
      expect(lower.points.contents[i][0]).toBe(x);
      expect(Number(upper.points.contents[i][1]) - Number(y)).toBeCloseTo(20);
      expect(Number(y) - Number(lower.points.contents[i][1])).toBeCloseTo(20);
    });
    const labels = shapes.flatMap((s) =>
      s.shapeType === "Equation" ? [s.string.contents] : [],
    );
    expect(labels).toContain("(x,f(x)+\\rho)");
    expect(labels).toContain("(x,f(x))");
    expect(labels).toContain("(x,f(x)-\\rho)");
  });

  test("rejects invalid radii, function samples, and marked arguments", async () => {
    await expect(
      diagram({ sub: substance(0), sty: functionNeighborhoodStyle() }),
    ).rejects.toThrow("radius");
    await expect(
      diagram({
        sub: substance(),
        sty: functionNeighborhoodStyle({ sample: () => 2 }),
      }),
    ).rejects.toThrow("codomain");
    await expect(
      diagram({
        sub: substance(),
        sty: functionNeighborhoodStyle({ markAt: 2 }),
      }),
    ).rejects.toThrow("domain");
  });

  test("permits collar boundaries outside the codomain, while retaining the graph in it", async () => {
    vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    await expect(
      diagram({
        sub: substance(0.2),
        sty: functionNeighborhoodStyle({ sample: () => 0.05 }),
      }),
    ).resolves.toBeDefined();
  });

  test("renders an illustrative graph and collar with explicit function values", async () => {
    const result = await diagram({
      sub: substance(),
      sty: functionNeighborhoodStyle(),
      canvas: canvas(340, 300),
    });
    const { svg } = await result.render();
    expect(svg.querySelectorAll("polyline")).toHaveLength(3);
    expect(svg.querySelectorAll("polygon")).toHaveLength(1);
    expect(svg.querySelectorAll("circle")).toHaveLength(5);
    expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
    result.discard();
  });
});
