import { equivalued } from "contrib/CurveConstraints";
import { numOf } from "contrib/Utils";

describe("equivalued", () => {
  it.each([[[2]], [[3, 3, 3, 3, 3]], [[-5, -5, -5]], [[-5, -5, -5]]])(
    "same numbers",
    (values: number[]) => {
      const result = equivalued(values);
      expect(numOf(result)).toBeCloseTo(0, 4);
    }
  );

  it.each([
    [[3, 1], 2],
    [[-1, 0, 1], 2],
    [[-5, -1], 4],
  ])("different numbers", (values: number[], expected: number) => {
    const result = equivalued(values);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});
