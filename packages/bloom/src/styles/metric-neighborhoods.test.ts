import { describe, expect, test } from "vitest";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  inNeighborhood,
  metricSpaces,
  planeDistance,
  type PlaneMetric,
} from "../domains/metric-spaces.js";
import { metricNeighborhoodStyle } from "./metric-neighborhoods.js";

function substance(metric: PlaneMetric, rho = 1) {
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({ metric });
  const ball = sub.Neighborhood({ point: [0, 0], rho });
  sub.InSpace(ball, plane);
  return sub.make();
}

describe("Gemignani's metric neighborhoods", () => {
  test("distinguishes the four metrics on the same two points", () => {
    expect(planeDistance("euclidean", [0, 0], [3, 4])).toBe(5);
    expect(planeDistance("taxicab", [0, 0], [3, 4])).toBe(7);
    expect(planeDistance("supremum", [0, 0], [3, 4])).toBe(4);
    expect(planeDistance("discrete", [0, 0], [3, 4])).toBe(1);
  });

  test("uses open balls and preserves the discrete singleton", () => {
    expect(inNeighborhood("euclidean", [0, 0], 1, [1, 0])).toBe(false);
    expect(inNeighborhood("taxicab", [0, 0], 1, [0.5, 0.5])).toBe(false);
    expect(inNeighborhood("supremum", [0, 0], 1, [0.5, 0.5])).toBe(true);
    expect(inNeighborhood("discrete", [0, 0], 1, [0, 0])).toBe(true);
    expect(inNeighborhood("discrete", [0, 0], 1, [0.001, 0])).toBe(false);
    expect(inNeighborhood("discrete", [0, 0], 1.01, [100, 100])).toBe(true);
  });

  test.each([
    ["euclidean", "circle"],
    ["taxicab", "polygon"],
    ["discrete", "circle"],
    ["supremum", "rect"],
  ] as const)(
    "renders the %s neighborhood with the shared style",
    async (metric, tag) => {
      const sub = substance(metric);
      const sty = metricNeighborhoodStyle();
      const first = await diagram({ sub, sty, canvas: canvas(300, 280) });
      const { svg } = await first.render();
      expect(svg.querySelector(tag)).not.toBeNull();
      expect(svg.querySelectorAll("line")).toHaveLength(2);
      expect(svg.outerHTML).not.toMatch(/NaN|undefined/);
      first.discard();
      // Rendering a second instance must not retain shapes from the first build.
      const second = await diagram({ sub, sty, canvas: canvas(300, 280) });
      const again = await second.render();
      expect(again.svg.querySelectorAll(tag)).toHaveLength(
        svg.querySelectorAll(tag).length,
      );
      second.discard();
    },
  );

  test("rejects a false finite drawing of the whole discrete plane", async () => {
    await expect(
      diagram({
        sub: substance("discrete", 2),
        sty: metricNeighborhoodStyle(),
        canvas: canvas(300, 280),
      }),
    ).rejects.toThrow("entire plane");
  });
});
