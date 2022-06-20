import { compDict, sdEllipse } from "contrib/Functions";
import { fnShapeDef, sampleCircle } from "contrib/sdfConfig";
import { genCode, input, primaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { Context, InputFactory, makeCanvas } from "shapes/Samplers";
import * as ad from "types/ad";
import { Shape } from "types/shapes";
import { FloatV } from "types/value";
import { black, floatV, vectorV } from "utils/Util";

const canvas = makeCanvas(800, 700);

const makeContext = (pt: number[]): { context: Context; p: ad.Input[] } => {
  const rng = seedrandom("sdf");
  const inputs: ad.Input[] = [];
  const makeInput: InputFactory = (meta) => {
    const x = input({
      key: inputs.length,
      val: "pending" in meta ? meta.pending : meta.sampler(rng),
    });
    inputs.push(x);
    return x;
  };
  for (const coord of pt) {
    makeInput({ sampler: () => coord });
  }
  return { context: { makeInput }, p: [...inputs] };
};

const compareDistance = (
  context: Context,
  shapeType: string,
  shape: Shape,
  p: ad.Input[],
  expected: number
) => {
  const result = getResult(context, shapeType, shape, p);
  const g = primaryGraph(result.contents);
  const f = genCode(g);
  const randArgs: number[] = [];
  for (let i = 0; i < sampleCircle.args.length; i++) {
    const curArg = sampleCircle.args[i];
    if (curArg.tag === "VectorV") {
      randArgs.push[
        Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
          curArg.contents[0].min
      ];
      randArgs.push[
        Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
          curArg.contents[1].min
      ];
    } else {
      console.log(curArg.tag);
      randArgs.concat(shapeRecursion(curArg));
    } // else {
    // throw Error("wrong value passed in");
    //}
  }
  const { primary: dist, gradient } = f([...randArgs]);
  expect(dist).toBeCloseTo(expected);
};

const shapeRecursion = (shapeContents: fnShapeDef): number[] => {
  const retVal: number[] = [];
  for (let i = 0; i < shapeContents.contents.length; i++) {
    const curArg = shapeContents.contents[i];
    if (curArg.tag === "VectorV") {
      retVal.push[
        Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
          curArg.contents[0].min
      ];
      retVal.push[
        Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
          curArg.contents[1].min
      ];
    } else if (curArg.tag === "FloatV") {
      retVal.push[Math.random() * (curArg.max - curArg.min) + curArg.min];
    } else {
      throw Error("wrong value passed in");
    }
  }
  return retVal;
};

const getResult = (
  context: Context,
  shapeType: string,
  s: any,
  p: ad.Input[]
): FloatV<ad.Num> => {
  if (shapeType === "Ellipse") {
    return {
      tag: "FloatV",
      contents: sdEllipse(s, p),
    };
  } else {
    const result = compDict.signedDistance(context, [shapeType, s], p);
    return result;
  }
};

const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: number[],
  expected: number
) => {
  const { context, p } = makeContext(pt);
  const shape = makeCircle(context, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareDistance(context, "Circle", shape, p, expected);
};

describe("sdf", () => {
  test("circle", () => {
    testCircle([0, 0], 3, 0, [0, 0], -3);
  });
});
