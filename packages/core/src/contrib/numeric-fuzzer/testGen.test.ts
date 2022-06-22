// export function wrapReturn((args: number[]) => ( number | number[] ))
import * as fs from "fs";
import { foo } from "./gencodeOutput";
import { fnDef, sampleCircle } from "./sdfConfig";

export function testGen(
  fn: fnDef,
  fn2Test: (arg: number[]) => number | number[],
  numTests: number
): any[] {
  const ret = [];
  for (let i = 0; i < numTests; i++) {
    const inputArgs = [];
    for (let i = 0; i < fn.args.length; i++) {
      //Main part of randomizing inputs
      const curArg = fn.args[i];
      switch (curArg.tag) {
        case "VectorRand":
          inputArgs.push(
            Math.random() * (curArg.contents[0].max - curArg.contents[0].min) +
              curArg.contents[0].min
          );
          inputArgs.push(
            Math.random() * (curArg.contents[1].max - curArg.contents[1].min) +
              curArg.contents[1].min
          );
          break;
        case "FloatRand":
          inputArgs.push(
            Math.random() * (curArg.max - curArg.min) + curArg.min
          );
          break;
        case "VectorV":
          inputArgs.push(curArg.contents[0].contents);
          inputArgs.push(curArg.contents[1].contents);
          break;
        default:
          inputArgs.push(curArg.contents);
          break;
      }
    }

    const retFlt = fn2Test(inputArgs);
    if (typeof retFlt === "number") {
      const retJSON = {
        input: inputArgs,
        output: retFlt,
        passed: oracleNum(retFlt),
      };
    } else {
      const retJSON = {
        input: inputArgs,
        output: retFlt,
        passed: oracleArr(retFlt),
      };

      ret.push(JSON.stringify(retJSON));
    }
  }
  return ret;
}

export function oracleArr(arg: number[]): boolean {
  if (isNaN(arg[0]) || isNaN(arg[1])) {
    return false;
  }
  return true;
}

export function oracleNum(arg: number): boolean {
  if (isNaN(arg)) {
    return false;
  }
  return true;
}

export function fuzz(): void {
  fs.writeFileSync(
    "fuzzedGen.txt",
    testGen(sampleCircle, foo, 1000000).join("\n")
  );
}

describe("sdf", () => {
  test("circle", () => {
    fuzz();
  });
});
/**
 * Shape code
 */
// else if (curArg.tag === "Shape") {
//     for (let i = 0; i < curArg.contents.length; i++) {
//       const curArgNested = curArg.contents[i];
//       if (curArgNested.tag === "VectorV") {
//         defaultArgs[defaultArgs.indexOf(shape.center.contents[0].val)] =
//           Math.random() *
//             (curArgNested.contents[0].max - curArgNested.contents[0].min) +
//           curArgNested.contents[0].min;
//         defaultArgs[defaultArgs.indexOf(shape.center.contents[1].val)] =
//           Math.random() *
//             (curArgNested.contents[1].max - curArgNested.contents[1].min) +
//           curArgNested.contents[1].min;
//       } else if (curArgNested.tag === "FloatV") {
//         defaultArgs[defaultArgs.indexOf(shape.r.contents.val)] =
//           Math.random() * (curArgNested.max - curArgNested.min) +
//           curArgNested.min;
//       } else {
//         throw Error("wrong value passed in");
//       }
//     }
//   }
