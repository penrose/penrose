import { makeCircle } from "../../shapes/Circle";
import { makeRectangle } from "../../shapes/Rectangle";
import { makeCanvas, simpleContext } from "../../shapes/Samplers";
import { inDirection } from "../ObjectivesUtils";
import { numOf } from "../Utils";

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
    let result = inDirection(testShape, testRefShape, [-1, 0], 0);
    expect(numOf(result)).toEqual(1);
  });

  test("with padding (not in the direction)", async () => {
    let result = inDirection(testShape, testRefShape, [-1, 0], 1);
    expect(numOf(result)).toEqual(0);
  });

  test("with padding (in the direction)", async () => {
    let result = inDirection(testShape, testRefShape, [-1, 0], -2);
    expect(numOf(result)).toEqual(9);
  });
});
