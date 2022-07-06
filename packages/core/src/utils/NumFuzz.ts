import {
  genCode,
  getLastGenCodeFn,
  input,
  primaryGraph,
} from "engine/Autodiff";
import * as fs from "fs";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeEquation } from "shapes/Equation";
import { makeImage } from "shapes/Image";
import { makeLine } from "shapes/Line";
import { makePath } from "shapes/Path";
import { makePolygon } from "shapes/Polygon";
import { makePolyline } from "shapes/Polyline";
import { makeRectangle } from "shapes/Rectangle";
import { Canvas, Context, makeCanvas, simpleContext } from "shapes/Samplers";
import { Properties, Shape } from "shapes/Shapes";
import { makeText } from "shapes/Text";
import * as ad from "types/ad";
import { Color, Value } from "types/value";
import { floatV, vectorV } from "./Util";

// ------------------------------------- Fuzzer ------------------------------------- //

/**
 * Using a set of provided FuzzOptions, sets up a fuzzing environment
 * and returns it to the client.  Before calling fuzz(), call this function
 * to set up the fuzzing environment.
 *
 * @param fo Describes the fuzzing environment to setup
 * @returns Fuzzing environment
 */
export const fuzzSetup = (fo: FuzzOptions): FuzzEnv => {
  const context = simpleContext(fo.variation || "");
  const fe = { ...fo, context }; // Fuzzing environment

  const nonShapeV: { [k: string]: ad.Num[] } = {}; // Non-shape Vector Inputs
  const nonShape: { [k: string]: ad.Num } = {}; // Non-shape Numeric Inputs
  const shape = {}; // Shape Property Inputs (retain the wrapper)
  const inputs: FuzzInput[] = []; // All inputs

  // Count and keep track of the varying input values. This is at
  // most a two-layer structure, so just iterate straight across
  // while keeping track of shape and non-shape inputs.
  //
  // For shape inputs, we need to retain the FloatV/VectorV wrappers,
  // but we do not want or retain them for non-shape inputs.
  for (const arg of fo.fnCfg.args) {
    switch (arg.tag) {
      case "VectorRand": // Random Vector of Floats --> Input[] | VectorV
        if (arg.shapeProp)
          shape[arg.name] = vectorV(
            arg.contents.map((e) => makeInput(e, inputs)) // wrapper
          );
        else
          nonShapeV[arg.name] = arg.contents.map((e) => makeInput(e, inputs)); // nop wrapper
        break;

      case "FloatRand": // Random Float --> Input | FloatV
        if (arg.shapeProp) {
          shape[arg.name] = floatV(makeInput(arg, inputs)); // wrapper
        } else {
          nonShape[arg.name] = makeInput(arg, inputs); // no wrapper
        }
        break;

      case "VectorV": // VectorV --> Num[] | VectorV
        if (arg.shapeProp) {
          shape[arg.name] = arg; // wrapper
        } else {
          nonShapeV[arg.name] = arg.contents; // no wrapper
        }
        break;

      case "FloatV": // FloatV --> Num
        if (arg.shapeProp) {
          shape[arg.name] = arg; // wrapper
        } else {
          nonShape[arg.name] = arg.contents; // no wrapper
        }
        break;

      case "Context": // Context --> Noop
        break;

      default:
        throw new Error(`Unexpected arg tag: ${JSON.stringify(arg)}`);
    }
  }

  // Now handle shape inputs by creating the shape object
  if (fo.fnCfg.shapeType) {
    // Create the shape
    const theShape: Shape = getShapeMaker(fo.fnCfg.shapeType)(
      context,
      makeCanvas(800, 700),
      {} // empty for now so we can flatten any input nodes
    );

    // Traverse the shape and flatten out any "standard" input nodes we
    // just don't care about right now. Not configured means: we don't care.
    //
    // We need to do this so we provide the same number of inputs to the
    // function that we specify. Otherwise, we might be providing inputs
    // to properties other than the ones we expect.
    const valStack: (Value<ad.Num> | Color<ad.Num>)[] = [];
    for (const k in theShape) {
      if (typeof theShape[k] !== "object") continue;
      valStack.push(theShape[k]); // init the stack
    }
    while (valStack.length) {
      const thisVal = valStack.pop();
      if (!thisVal || !("contents" in thisVal)) continue;

      if (typeof thisVal.contents === "object") {
        // Flatten object inputs
        if ("tag" in thisVal.contents) {
          switch (thisVal.contents.tag) {
            case "Input": {
              thisVal.contents = thisVal.contents.val; // Flatten
              break;
            }
            case "NONE": // fallthrough
            case "RGBA": // fallthrough
            case "HSVA": {
              valStack.push(thisVal.contents); // traverse
              break;
            }
          } // switch: contents.tag
        } else if (Array.isArray(thisVal.contents)) {
          // Flatten array inputs
          for (const k in thisVal.contents) {
            const subVal = thisVal.contents[k];
            if (
              typeof subVal === "object" &&
              "tag" in subVal &&
              subVal.tag === "Input"
            ) {
              thisVal.contents[k] = subVal.val; // Flatten
            }
          }
        } // else if: contents is array
      } // if: contents is object
    } // while: valStack.length

    // Add the configured input nodes we DO care about to the shape
    for (const k in shape) {
      theShape[k] = shape[k];
    }

    // Return the shape
    return {
      ...fe,
      nonShape,
      nonShapeV,
      inputs,
      shape: theShape,
    };
  } else {
    return { ...fe, nonShape, nonShapeV, inputs };
  }
};

/**
 * Fuzzes a computation function using a provided Fuzzing environment.
 *
 * @param comp Computation function output to fuzz
 * @param fe Fuzzing Environment
 * @returns Results of Fuzzing
 */
export const fuzz = (comp: ad.Num, fe: FuzzEnv): FuzzTestResult[] => {
  const ret: FuzzTestResult[] = []; // Results of fuzzing: one record per test
  const fn = genCode(fe.graphFn(comp)); // Generate code for the function
  console.log(`inner fn under test: ${getLastGenCodeFn().inner.toString()}`); // !!!

  // Generate the inputs
  for (let i = 0; i < fe.numTests; i++) {
    // Randomize inputs
    const inputArgs: number[] = [];
    fe.inputs.forEach((e) =>
      inputArgs.push(Math.random() * (e.arg.max - e.arg.min) + e.arg.min)
    );

    // Call the function and check the output against the oracle
    const retFlt = fn(inputArgs);
    if (typeof retFlt.primary === "number") {
      ret.push({
        input: inputArgs,
        output: retFlt,
        passed: [
          ...retFlt.gradient,
          ...retFlt.secondary,
          retFlt.primary,
        ].every((e) => fe.oracleFn(e)), // Oracle check
      });
    } else {
      throw new Error(
        "I'm a numeric fuzzer; I can only fuzz with numeric values."
      );
    }
  } // for: numTests

  // Write to an output file if requested
  if (fe.outputFile) {
    fs.writeFileSync(
      fe.outputFile,
      ret.map((e) => JSON.stringify(e)).join("\n")
    );
  }

  return ret;
};

/**
 * Returns true if every number in a vector is real. Infinity, null, NaN are not real.
 *
 * @param arg vector of numbers to check
 * @returns true if all numbers in vector are real; false, otherwise.
 */
export const isRealV = (arg: number[]): boolean => {
  return arg.every((e) => isReal(e));
};

/**
 * Returns true if number is real. Infinity, null, NaN are not real.
 *
 * @param arg number to check
 * @returns true if number is real; false, otherwise.
 */
export const isReal = (arg: number | null): boolean => {
  return arg !== null && !isNaN(arg) && arg !== Infinity && arg !== -Infinity;
};

/**
 * Creates a new input value and returns it.  Also updates the accumulated
 * list of inputs, which associates each input with its configuration (arg).
 *
 * @param arg argument configuration
 * @param inputs array of accumulated inputs
 * @returns new input value
 */
export const makeInput = (arg: FnNumRand, inputs: FuzzInput[]): ad.Input => {
  const ret = input({
    key: inputs.length,
    val: 0, // will be replaced by fuzzer
  });
  inputs.push({
    tag: "FuzzInput",
    arg: arg,
    input: ret,
  });
  return ret;
};

/**
 * Gets the appropriate shape maker function for the given shape type string.
 * TODO: Consier moving this to shapes/Shapes
 *
 * @param shapeType Name of shape type
 * @returns shape maker function
 */
export const getShapeMaker = (
  shapeType:
    | "Circle"
    | "Ellipse"
    | "Equation"
    | "Image"
    | "Line"
    | "Path"
    | "Polygon"
    | "Polyline"
    | "Rectangle"
    | "Text"
): ShapeMakerFn => {
  return {
    Circle: makeCircle,
    Ellipse: makeEllipse,
    Equation: makeEquation,
    Image: makeImage,
    Line: makeLine,
    Path: makePath,
    Polygon: makePolygon,
    Polyline: makePolyline,
    Rectangle: makeRectangle,
    Text: makeText,
  }[shapeType];
};

/**
 * Describes the common shape of all Shape maker functions
 * TODO: Consier moving this to shapes/Shapes
 */
export type ShapeMakerFn = (
  context: Context,
  canvas: Canvas,
  properties: Properties
) => Shape;

// ------------------------------ Function Definitions ------------------------------ //

/**
 * Describes a Penrose computation function
 */
export type FnDef = {
  fnName: string;
  shapeType?:
    | "Circle"
    | "Ellipse"
    | "Equation"
    | "Image"
    | "Line"
    | "Path"
    | "Polygon"
    | "Polyline"
    | "Rectangle"
    | "Text";
  args: FnArg[];
};

/**
 * Describes an argument to a Penrose computation function
 */
export type FnArg = FnContext | FnVector | FnNum | FnVectorRand | FnNumRand;

/**
 * Base argument properties
 */
type FnBase = {
  name: string; // Name of the argument
  shapeProp?: boolean; // Whether this argument is a shape property
};

/**
 * Non-randomized VectorV argument
 */
export type FnVector = FnBase & {
  tag: "VectorV";
  contents: number[]; //fnNum[];
};

/**
 * Randomized VectorV argument
 */
export type FnVectorRand = FnBase & {
  tag: "VectorRand";
  contents: FnNumRand[];
};

/**
 * Context argument (placeholder)
 */
export type FnContext = {
  tag: "Context";
  name: string;
};

/**
 * Non-randomized Number argument
 */
export type FnNum = FnBase & {
  tag: "FloatV";
  contents: number;
};

/**
 * Randomized Number argument
 */
export type FnNumRand = FnBase & {
  tag: "FloatRand";
  min: number;
  max: number;
};

// ---------------------------------- Fuzzer Types ---------------------------------- //

/**
 * Describes the results of a Fuzzing test: input, output, and whether the test passed
 */
export type FuzzTestResult = {
  input: number[]; // input to function
  output: ad.Outputs<number>; // output from function
  passed: boolean; // true if output matches oracle; false, otherwise
};

/**
 * Fuzzing options for a given computation function
 */
export type FuzzOptions = {
  fnCfg: FnDef; // Function configuration
  variation?: string; // Variation / seed (optional)
  graphFn: typeof primaryGraph; // Function for generating the computation graph. TODO: Should also accept secondaryGraph
  numTests: number; // Number of fuzzing tests to execute
  oracleFn: typeof isReal; // The oracle function TODO: Create type for function shape
  outputFile?: string; // File to write output to
};

/**
 * A fuzzing environment suitable for executing a fuzzing session
 */
export type FuzzEnv = FuzzOptions & {
  context: Context; // Context
  shape?: Shape; // Shape object parameter (optional)
  nonShape: { [key: string]: ad.Num }; // Non-shape parameters
  nonShapeV: { [key: string]: ad.Num[] }; // Non-shape vector parameters
  inputs: FuzzInput[]; // Accumulated inputs
};

/**
 * A single input to a computation function that may vary with each iteration
 * of the fuzzer.  Associates the input with its correspongin configuration (arg).
 */
export type FuzzInput = {
  tag: "FuzzInput";
  input: ad.Input; // The actual ad.Input
  arg: FnNumRand; // Configuration of the input (i.e., min and max)
};
