import { curvature } from "contrib/Functions";

describe("curvature", () => {
  it.each([
    [
      [1, 2],
      [2, 2],
      [3, 2],
    ],
    [
      [1, 1],
      [2, 2],
      [3, 3],
    ],
    [
      [-1, 1],
      [0, 0],
      [7, -7],
    ],
    [
      [0.1, 0.1],
      [0.2, 0.3],
      [0.6, 0.9],
    ],
  ])("line segment", (p1: number[], p2: number[], p3: number[]) => {
    const result = curvature([p1[0], p1[1]], [p2[0], p2[1]], [p3[0], p3[1]]);
    expect(result).toBeCloseTo(0, 4);
  });

  it.each([
    [[1, 0], [0, 0], [0, 1], -Math.PI / 2],
    [[0, 1], [0, 0], [1, 0], Math.PI / 2],
    [[2, 1], [0, 0], [-2, 1], -Math.PI / 2],
    [[3, 4], [3, 3], [4, 3], Math.PI / 2],
    [[7, 0], [0, 0], [-2, 1], Math.PI / 6],
  ])(
    "curved segment",
    (p1: number[], p2: number[], p3: number[], expected: number) => {
      const result = curvature([p1[0], p1[1]], [p2[0], p2[1]], [p3[0], p3[1]]);
      expect(result).toBeCloseTo(0, 4);
    }
  );
});
