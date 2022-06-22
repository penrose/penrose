import { compDict, sdEllipse } from "contrib/Functions";
import { genCode, input, primaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import {
  Context,
  InputFactory,
  makeCanvas,
  simpleContext,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { FloatV } from "types/value";
import { black, floatV, vectorV } from "utils/Util";

const canvas = makeCanvas(800, 700);

/**
 * This function turns types of function properties from concrete val types to input types.
 * Works for numeric types, unsure for other types.
 * @param pt
 * @returns
 */
const makeInputArray = (pt: number[]): ad.Input[] => {
  const inputs: ad.Input[] = [];
  const makeInput: InputFactory = (meta) => {
    const rng = seedrandom("sdf");
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
  return [...inputs];
};

// const makeContext = (pt: number[]): { context: Context; p: ad.Input[] } => {
//   const rng = seedrandom("sdf");
//   const inputs: ad.Input[] = [];
//   const makeInput: InputFactory = (meta) => {
//     const x = input({
//       key: inputs.length,
//       val: "pending" in meta ? meta.pending : meta.sampler(rng),
//     });
//     inputs.push(x);
//     return x;
//   };
//   for (const coord of pt) {
//     makeInput({ sampler: () => coord });
//   }
//   return { context: { makeInput }, p: [...inputs] };
// };

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
  ]; //Currently manually passing in all values
  console.log(randArgs);

  console.log([...randArgs]);
  const { primary: dist, gradient } = f([...randArgs]);
  expect(dist).toBeCloseTo(expected);
};

// const shapeRecursion = (shapeContents: fnShapeDef): number[] => {
//   const retVal: number[] = [];
//   for (let i = 0; i < shapeContents.contents.length; i++) {
//     const curArg = shapeContents.contents[i];
//     if (curArg.tag === "VectorV") {
//       retVal.push(
//         Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
//           curArg.contents[0].min
//       );
//       retVal.push(
//         Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
//           curArg.contents[1].min
//       );
//     } else if (curArg.tag === "FloatV") {
//       retVal.push[Math.random() * (curArg.max - curArg.min) + curArg.min];
//     } else {
//       throw Error("wrong value passed in");
//     }
//   }
//   return retVal;
// };

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

export const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: [number, number],
  expected: number
) => {
  const p = makeInputArray([pt[0], pt[1], center[0], center[1], radius]); // Here I am specifying all parameters (center, r, point) must be input types. Eventually this should be automated.
  const context = simpleContext("circle");
  const shape = makeCircle(context, canvas, {
    center: vectorV([p[2], p[3]]),
    r: floatV(p[4]),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareDistance(context, "Circle", shape, [p[0], p[1]], expected);
};

describe("sdf", () => {
  test("circle", () => {
    testCircle([0, 0], 3, 0, [1, 1], 2);
  });
});
