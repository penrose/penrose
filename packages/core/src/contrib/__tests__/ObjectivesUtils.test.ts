import { ready } from "@penrose/optimizer";
import { inDirection } from "../ObjectivesUtils";
import { numOf } from "../Utils";

await ready;

const testShape = { center: { contents: [0, 2] } };
const testRefShape = { center: { contents: [1, 1] } };

describe("inDirection", () => {
  test("without padding", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [-1, 0],
      0
    );
    expect(numOf(result)).toEqual(1);
  });

  test("with padding (not in the direction)", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [-1, 0],
      1
    );
    expect(numOf(result)).toEqual(0);
  });

  test("with padding (in the direction)", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [-1, 0],
      -2
    );
    expect(numOf(result)).toEqual(9);
  });
});
