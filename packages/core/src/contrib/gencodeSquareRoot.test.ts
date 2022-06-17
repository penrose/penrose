import { genCode, input, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import {
  Context,
  InputFactory,
  makeCanvas,
  simpleContext,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const makeContextNum = (pt: number): { context: Context; p: ad.Input } => {
  const rng = seedrandom("sdf");
  const makeInput: InputFactory = (meta) => {
    const x = input({
      key: 0,
      val: "pending" in meta ? meta.pending : meta.sampler(rng),
    });
    return x;
  };
  return { context: { makeInput }, p: makeInput({ sampler: () => pt }) };
};

const compareSqrt = (context: Context, input: ad.Input, expected: number) => {
  const result = compDict.sqrt(context, input);
  const g = secondaryGraph([result.contents]);
  const f = genCode(g);
  const [x] = f([input.val]).secondary;
  expect(x).toBeCloseTo(expected);
};

export const testSqrt = (input: number, expected: number) => {
  const p = makeContextNum(input).p;
  const context = simpleContext("sqrt");
  compareSqrt(context, p, expected);
};

describe("closest point", () => {
  test("sqrt", () => {
    testSqrt(-2, NaN);
  });
});
