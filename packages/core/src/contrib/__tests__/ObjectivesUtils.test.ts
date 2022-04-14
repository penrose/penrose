import { inDirection } from "contrib/ObjectivesUtils";
import { genCode, makeGraph } from "engine/Autodiff";
import { VarAD } from "types/ad";

const testShape = { center: { contents: [0, 2] } };
const testRefShape = { center: { contents: [1, 1] } };

const numOf = (x: VarAD) => {
  const g = makeGraph({ primary: 0, secondary: [x] });
  const f = genCode(g);
  const [y] = f([]).secondary; // no inputs, so, empty array
  return y;
};

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
