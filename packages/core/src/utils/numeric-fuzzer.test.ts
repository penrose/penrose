import { compDict, sdEllipse } from "contrib/Functions";
import { fnShapeDef, sampleCircle } from "contrib/sdfConfig";
import { genCode, input, primaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { Context, InputFactory, makeCanvas } from "shapes/Samplers";
import * as ad from "types/ad";
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
  shape: any,
  p: ad.Input[],
  expected: number
) => {
  const result = getResult(context, shapeType, shape, p);
  const g = primaryGraph(result.contents);
  const f = genCode(g);
  const randArgs: number[] = [
    p[0].val,
    p[1].val,
    shape.r.contents.val,
    shape.center.contents[0].val,
    shape.center.contents[1].val,
  ];
  console.log(randArgs);
  for (let i = 0; i < sampleCircle.args.length; i++) {
    const curArg = sampleCircle.args[i];
    if (curArg.tag === "VectorV") {
      randArgs[randArgs.indexOf(p[0].val)] =
        Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
        curArg.contents[0].min;
      randArgs[randArgs.indexOf(p[1].val)] =
        Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
        curArg.contents[1].min;
    } else {
      for (let i = 0; i < curArg.contents.length; i++) {
        const curArgNested = curArg.contents[i];
        if (curArgNested.tag === "VectorV") {
          randArgs[randArgs.indexOf(shape.center.contents[0].val)] =
            Math.random() *
              (curArgNested.contents[0].max - curArgNested.contents[0].min) +
            curArgNested.contents[0].min;
          randArgs[randArgs.indexOf(shape.center.contents[1].val)] =
            Math.random() *
              (curArgNested.contents[1].max - curArgNested.contents[1].min) +
            curArgNested.contents[1].min;
        } else if (curArgNested.tag === "FloatV") {
          randArgs[randArgs.indexOf(shape.center.contents.val)] =
            Math.random() * (curArgNested.max - curArgNested.min) +
            curArgNested.min;
        } else {
          throw Error("wrong value passed in");
        }
      }
    } // else {
    // throw Error("wrong value passed in");
    //}
  }
  console.log([...randArgs]);
  const { primary: dist, gradient } = f([...randArgs]);
  expect(dist).toBeCloseTo(expected);
};

const shapeRecursion = (shapeContents: fnShapeDef): number[] => {
  const retVal: number[] = [];
  for (let i = 0; i < shapeContents.contents.length; i++) {
    const curArg = shapeContents.contents[i];
    if (curArg.tag === "VectorV") {
      retVal.push(
        Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
          curArg.contents[0].min
      );
      retVal.push(
        Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
          curArg.contents[1].min
      );
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
