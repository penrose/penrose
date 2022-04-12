import { inDirection } from "contrib/ObjectivesUtils";
import { constOf, numOf } from "engine/Autodiff";

const testShape = { center: { contents: [constOf(0), constOf(2)] } };
const testRefShape = { center: { contents: [constOf(1), constOf(1)] } };

describe("inDirection", () => {
  test("without padding", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [constOf(-1.0), constOf(0.0)],
      constOf(0)
    );
    expect(numOf(result)).toEqual(1);
  });

  test("with padding (not in the direction)", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [constOf(-1.0), constOf(0.0)],
      constOf(1)
    );
    expect(numOf(result)).toEqual(0);
  });

  test("with padding (in the direction)", async () => {
    let result = inDirection(
      ["testShape", testShape],
      ["testRefShape", testRefShape],
      [constOf(-1.0), constOf(0.0)],
      constOf(-2)
    );
    expect(numOf(result)).toEqual(9);
  });
});
