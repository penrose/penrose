import { tsAnalyzePropertyAccess, TsPropertyAccess } from "analysis/Typescript";
import { compDict } from "contrib/Functions";
import { atan2 } from "engine/AutodiffFunctions";
import { Context } from "shapes/Samplers";
import * as ad from "types/ad";
import { FloatV } from "types/value";

// ------------------------------- Functions for Testing ----------------------------- //

/**
 * A function where searching for _context yeields no hits.
 *
 * @param _context
 * @param v
 * @returns
 */
const testTsAnalyzePropertyAccessIntraNoHits = (
  _context: Context,
  v: ad.Num[]
): FloatV<ad.Num> => {
  return { tag: "FloatV", contents: atan2(v[1], v[0]) };
};

/**
 * A function where the property access is in the return statement.
 *
 * @param s
 * @returns
 */
const testTsAnalyzePropertyAccessRetVal1 = (s: any): FloatV<ad.Num> => {
  const x = 5;
  const y = x + 3;
  return {
    tag: "FloatV",
    contents: s.r.contents,
  };
};

/**
 * A function where the property access is in the return statement.
 *
 * @param s
 * @returns
 */
const testTsAnalyzePropertyAccessRetVal2 = (s: any): ad.Num => {
  const [x, y] = [5, 3];
  const z = x + 3;
  return s.r.contents;
};

/**
 * A function where the property access is indirectly through another variable.
 *
 * @param _context
 * @param s
 */
const testTsAnalyzePropertyAccessAssign = (_context: Context, s: any): void => {
  const pi = compDict.MathPI(_context);
  const y: any = s;
  const e = compDict.MathE(_context);
  const z = s.r;
  const e2 = compDict.MathE(_context);
};

/**
 * A function where the property access is by way of creating an object
 *
 * @param s
 */
const testTsAnalyzePropertyAccessObj = (s: any): void => {
  const { r } = s;
  const x = r;
};

/**
 * A function where the property access is via LVal assignment.
 *
 * @param s
 */
const testTsAnalyzePropertyLValueProp = (s: any): void => {
  s.r = 1;
  s.r = 2;
};

/**
 * A function where the property access is by way of calling a function
 *
 * @param s
 */
const testTsAnalyzePropertyAccessFn = (s: any): void => {
  const r = (): { s: boolean } => {
    return { s: true };
  };
  const x = r().s;
};

/**
 * A function where the property access is within an inner function.
 *
 * @param s
 */
const testTsAnalyzePropertyInnerFn = (s: any): { x: any } => {
  const inner = (): { x: boolean } => {
    return { x: s.r };
  };
  return inner();
};

/**
 * A function where the property access is within an inner function.
 *
 * @param s
 */
const testTsAnalyzePropertyInnerFnDef = (s: any): { x: any } => {
  function inner(): { x: boolean } {
    return { x: s.r };
  }
  return inner();
};

/**
 * A function where the property assignment is by way of calling a function
 *
 * @param s
 */
const testTsAnalyzePropertyAssignFn = (s: any): void => {
  const inner = (): { s: any } => {
    return { s: true };
  };
  inner().s = s.r;
};

/**
 * A function with inner recursion
 *
 * @param s
 */
const testTsAnalyzePropertyRecursion = (s: any): number => {
  const inner = (x: number, z: any): number => {
    if (x < 1) return z.r;
    else return inner(x - 1, z);
  };
  return inner(100, s);
};

/**
 * A property access within an anonymous function
 *
 * @param s
 */
const testTsAnalyzePropertyAnonFn = (s: any): void => {
  [1, 2, 3].forEach((e) => (e === 2 ? s.r : 0));
};

/**
 * A function with no arguments.
 */
const testTsAnalyzePropertyAccessNoArgErr = (): void => {
  const x = 5;
};

/**
 * A function where a variable of interest is assigned to multiple times.
 *
 * @param s1
 * @param s2
 */
const testTsAnalyzePropertyAccessMultiAssignErr = (s1: any, s2: any): void => {
  s2 = s1;
  const x = s2.r;
};

/**
 * A function where the property access is via array.
 *
 * @param s
 */
const testTsAnalyzePropertyAccessArrayErr = (s: any): void => {
  const x = [s]; // Exception
  const y = x[0].r;
};

/**
 * A function where the property access is via an object
 *
 * @param s
 */
const testTsAnalyzePropertyAccessObjErr = (s: any): void => {
  const s2 = { shape: s }; // Exception
  const x = s2.shape.r;
};

/**
 * A function where the variable of interest is assigned to.
 *
 * @param s
 */
const testTsAnalyzePropertyLValueErr = (s: any): void => {
  s = 1; // Exception
};

// ---------------------------------- Helper Functions ------------------------------- //

/**
 * Tests a function analysis for deep equality.
 *
 * @param fn Function to analyze
 * @param arg Argument name or offset
 * @param expected Expected result
 */
const testFnEq = (
  fn: Function,
  arg: string | number,
  expected: TsPropertyAccess[]
): void => {
  test(fn.name, async () => {
    expect(
      tsAnalyzePropertyAccess("fn", arg, fn.toString())
        .toArray()
        .sort(sortTsPropertyAccess)
    ).toEqual(expected.sort(sortTsPropertyAccess));
  });
};

/**
 * Tests a function analysis to ensure it returns an exception.
 *
 * @param fn Function to analyze
 * @param arg Argument name or offset
 */
const testFnException = (fn: Function, arg: string | number): void => {
  test(fn.name, async () => {
    expect(() =>
      tsAnalyzePropertyAccess("fn", arg, fn.toString())
    ).toThrowError();
  });
};

/**
 * Sorts two TsPropertyAccess objects.  Suitable for passing to Array.sort().
 *
 * @param a Left-hand TsPropertyAccess
 * @param b Right-hand TsPropertyAccess
 * @returns 0 if a===b, -1 if a<b, 1 if a>b
 */
const sortTsPropertyAccess = (
  a: TsPropertyAccess,
  b: TsPropertyAccess
): number => {
  if (a.fnName < b.fnName) return -1;
  if (a.fnName > b.fnName) return 1;
  if (a.propName < b.propName) return -1;
  if (a.propName > b.propName) return 1;
  if (a.varName < b.varName) return -1;
  if (a.varName > b.varName) return 1;
  return 0;
};

// ---------------------------------- The Actual Tests ------------------------------- //

/**
 * Tests for the TypeScript Property Analysis
 */
describe("TS Property Analysis", () => {
  testFnEq(testTsAnalyzePropertyAccessIntraNoHits, "_context", []);
  testFnEq(testTsAnalyzePropertyAccessFn, "s", []);
  testFnEq(testTsAnalyzePropertyAssignFn, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  /* !!! Not working yet !!!
  testFnEq(testTsAnalyzePropertyInnerFn, "s", [
    { fnName: "inner", varName: "z", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyInnerFnDef, "s", [
    { fnName: "inner", varName: "z", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyRecursion, "s", [
    { fnName: "inner", varName: "z", propName: "r" },
  ]);
  */
  testFnEq(testTsAnalyzePropertyAnonFn, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnEq(compDict.midpointOffset, "s1", [
    { fnName: "linePts", varName: "_a", propName: "start" },
    { fnName: "linePts", varName: "_a", propName: "end" },
  ]);
  testFnEq(compDict.signedDistance, "s", [
    { fnName: "fn", varName: "s", propName: "center" },
    { fnName: "fn", varName: "s", propName: "width" },
    { fnName: "fn", varName: "s", propName: "height" },
    { fnName: "fn", varName: "s", propName: "r" },
    { fnName: "fn", varName: "s", propName: "points" },
    { fnName: "fn", varName: "s", propName: "shapeType" },
    { fnName: "sdLine", varName: "s", propName: "start" },
    { fnName: "sdLine", varName: "s", propName: "end" },
    { fnName: "sdPolyline", varName: "s", propName: "points" },
  ]);
  testFnEq(testTsAnalyzePropertyAccessRetVal1, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyAccessRetVal2, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyAccessAssign, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyAccessObj, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnEq(testTsAnalyzePropertyLValueProp, "s", [
    { fnName: "fn", varName: "s", propName: "r" },
  ]);
  testFnException(testTsAnalyzePropertyAccessNoArgErr, "s");
  testFnException(testTsAnalyzePropertyAccessMultiAssignErr, "s1");
  testFnException(testTsAnalyzePropertyAccessMultiAssignErr, "s2");
  testFnException(testTsAnalyzePropertyAccessArrayErr, "s");
  testFnException(testTsAnalyzePropertyAccessObjErr, "s");
  testFnException(testTsAnalyzePropertyLValueErr, "s");
});
