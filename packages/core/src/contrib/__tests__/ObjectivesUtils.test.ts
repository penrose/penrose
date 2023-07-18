import { describe, expect, test } from "vitest";
import { makeCircle } from "../../shapes/Circle.js";
import { makeRectangle } from "../../shapes/Rectangle.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import { inDirection } from "../ObjectivesUtils.js";
import { numOf, toPt } from "../Utils.js";

describe("inDirection", () => {
  const context = simpleContext("ObjectivesUtils.test");
  const canvas = makeCanvas(800, 700);
  const testShape = makeCircle(context, canvas, {
    center: { tag: "VectorV", contents: [0, 2] },
  });
  const testRefShape = makeRectangle(context, canvas, {
    center: { tag: "VectorV", contents: [1, 1] },
  });
  test("without padding", async () => {
    let result = inDirection(
      toPt(testShape.center.contents),
      toPt(testRefShape.center.contents),
      [-1, 0],
      0,
    );
    expect(numOf(result)).toBeCloseTo(1);
  });

  test("with padding (not in the direction)", async () => {
    let result = inDirection(
      toPt(testShape.center.contents),
      toPt(testRefShape.center.contents),
      [-1, 0],
      1,
    );
    expect(numOf(result)).toBeCloseTo(0);
  });

  test("with padding (in the direction)", async () => {
    let result = inDirection(
      toPt(testShape.center.contents),
      toPt(testRefShape.center.contents),
      [-1, 0],
      -2,
    );
    expect(numOf(result)).toBeCloseTo(9);
  });
});
