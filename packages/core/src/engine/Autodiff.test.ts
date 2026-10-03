import _ from "lodash";
import seedrandom from "seedrandom";
import { describe, expect, test } from "vitest";
import * as ad from "../types/ad.js";
import { eqList, randList } from "../utils/Util.js";
import {
  compile,
  fns,
  genGradient,
  logAD,
  problem,
  variable,
} from "./Autodiff.js";
import {
  add,
  div,
  ifCond,
  lt,
  max,
  mul,
  sin,
  squared,
  sub,
} from "./AutodiffFunctions.js";

describe("problem tests", () => {
  const f = (x: ad.Num, y: ad.Num) => add(squared(sub(x, y)), squared(x));

  test("no freeze", async () => {
    const x = variable(1);
    const y = variable(2);
    const p = await problem({ objective: f(x, y) });
    const run = p.start({}).run({});
    expect(run.converged).toBe(true);
    for (const [z, a] of run.vals) z.val = a;
    expect(x.val).toBeCloseTo(0);
    expect(y.val).toBeCloseTo(0);
  });

  test("freeze", async () => {
    const x = variable(1);
    const y = variable(2);
    const p = await problem({ objective: f(x, y) });
    const run = p.start({ freeze: (z) => z === x }).run({});
    expect(run.converged).toBe(true);
    for (const [z, a] of run.vals) z.val = a;
    expect(x.val).toBe(1);
    expect(y.val).toBeCloseTo(1);
  });
});

describe("compile tests", () => {
  test("no outputs", async () => {
    const f = await compile([]);
    const v = f((x) => x.val);
    expect(v).toEqual([]);
  });

  test("simple", async () => {
    const x = variable(3);
    const y = variable(4);
    const f = await compile([add(x, y), mul(x, y)]);
    const v = f((x) => x.val);
    expect(v).toEqual([7, 12]);
  });
});

describe("sparse energy inputs", () => {
  test("preserves state indices and masks when most inputs are unused", async () => {
    const inputs = Array.from({ length: 50000 }, (_, i) => variable(i));
    const xIndex = 7,
      yIndex = 49991;
    const x = inputs[xIndex],
      y = inputs[yIndex];
    const f = await genGradient(
      inputs,
      [add(squared(x), mul(x, y))],
      [sub(y, 1)],
    );
    const values = new Float64Array(inputs.map((v) => v.val));
    values[xIndex] = 2;
    values[yIndex] = 4;
    const gradient = new Float64Array(inputs.length).fill(NaN);
    const inputMask = inputs.map(() => true);
    inputMask[xIndex] = false;
    const result = f(
      { inputMask, objMask: [true], constrMask: [true] },
      values,
      3,
      gradient,
    );
    expect(result.phi).toBe(39);
    expect(result.objectives).toEqual([12]);
    expect(result.constraints).toEqual([3]);
    expect(gradient[xIndex]).toBe(0);
    expect(gradient[yIndex]).toBe(20);
    expect(gradient.filter((v) => v !== 0)).toEqual(new Float64Array([20]));
    inputMask[xIndex] = true;
    const objectiveOnly = f(
      { inputMask, objMask: [true], constrMask: [false] },
      values,
      3,
      gradient,
    );
    expect(objectiveOnly.phi).toBe(12);
    expect(gradient[xIndex]).toBe(8);
    expect(gradient[yIndex]).toBe(2);
    const constraintOnly = f(
      { inputMask, objMask: [false], constrMask: [true] },
      values,
      3,
      gradient,
    );
    expect(constraintOnly.phi).toBe(27);
    expect(gradient[xIndex]).toBe(0);
    expect(gradient[yIndex]).toBe(18);
  });

  test("supports constant energy without reachable variables", async () => {
    const f = await genGradient([variable(2)], [5], [-1]);
    const gradient = new Float64Array([NaN]);
    const result = f(
      { inputMask: [true], objMask: [true], constrMask: [true] },
      new Float64Array([9]),
      3,
      gradient,
    );
    expect(result.phi).toBe(5);
    expect(gradient).toEqual(new Float64Array([0]));
  });
});

// df/f[x] with finite differences about xi
export const _gradFiniteDiff = (f: (args: number[]) => number) => {
  return (xs: number[]): number[] => {
    const EPSG = 10e-5;

    // Scalar estimate (in 1D)
    // const dfxi = (f, x) => (f(x + EPSG / 2.) - f(x - EPSG / 2.)) / EPSG;

    const xsDiff = xs.map((e, i) => {
      const xsLeft = [...xs];
      xsLeft[i] = xsLeft[i] - EPSG / 2;
      const xsRight = [...xs];
      xsRight[i] = xsRight[i] + EPSG / 2;
      return (f(xsRight) - f(xsLeft)) / EPSG;
    });

    return xsDiff;
  };
};

const NUM_SAMPLES = 5; // Number of samples to evaluate gradient tests at

describe("symbolic differentiation tests", () => {
  test("grad finite diff", () => testGradFiniteDiff());
  test("graph 0", () => testGradSymbolic(0, gradGraph0()));
  test("graph 1", () => testGradSymbolic(1, gradGraph1()));
  test("graph 2", () => testGradSymbolic(2, gradGraph2()));
  test("graph 3", () => testGradSymbolic(3, gradGraph3()));
  test("graph 4", () => testGradSymbolic(4, gradGraph4()));
  test("graph 5", () => testGradSymbolic(5, gradGraph5()));
  test("graph 6", () => testGradSymbolic(6, gradGraph6()));
  test("graph 7", () => testGradSymbolic(7, gradGraph7()));
});

//#region Functions for testing numeric and symbolic gradients

const assert = (b: boolean, s: any[]) => {
  const res = b ? "passed" : "failed";
  logAD.trace("Assertion", res, ": ", ...s);
  return b;
};

const testGradFiniteDiff = () => {
  const rng = seedrandom("testGradFiniteDiff");

  // Only tests with hardcoded functions
  const f = (ys: number[]) => _.sum(_.map(ys, (e: number) => e * e));
  const df = (ys: number[]) => _.map(ys, (e: number) => 2 * e);

  const testResults = [];

  for (let i = 0; i < NUM_SAMPLES; i++) {
    const xs = randList(rng, 4);
    const gradEstRes = _gradFiniteDiff(f)(xs);
    const expectedRes = df(xs);
    const testRes = eqList(gradEstRes, expectedRes);
    testResults.push(testRes);
  }

  testResults.forEach((result) => {
    expect(result).toBeTruthy();
  });
};

interface GradGraph {
  inputs: ad.Var[];
  output: ad.Num;
}

const gradGraph1 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = variable(-5);
  const x1 = variable(6);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = sin(a);
  const z = mul(b, c);
  return { inputs: [x0, x1], output: z };
};

// Test addition of consts to graph (`c`)
const gradGraph2 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = variable(-5);
  const x1 = variable(6);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = add(a, 3);
  const z = mul(b, c);
  return { inputs: [x0, x1], output: z };
};

// Test vars w/ no grad
const gradGraph3 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = variable(100);
  const x1 = variable(-100);
  const head = squared(x0);
  return { inputs: [x0, x1], output: head };
};

// Test toPenalty
const gradGraph4 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = variable(100);
  const head = fns.toPenalty(x0);
  return { inputs: [x0], output: head };
};

// Test ifCond
const gradGraph5 = (): GradGraph => {
  logAD.info("test ifCond");

  // Build energy/gradient graph
  const x0 = variable(100);
  const x1 = variable(-100);
  const head = ifCond(lt(x0, 33), squared(x1), squared(x0));
  return { inputs: [x0, x1], output: head };
};

// Test max
const gradGraph6 = (): GradGraph => {
  logAD.info("test max");

  // Build energy/gradient graph
  const x0 = variable(100);
  const head = max(squared(x0), 0);
  return { inputs: [x0], output: head };
};

// Test div
// TODO < Test all ops automatically
const gradGraph7 = (): GradGraph => {
  logAD.info("test div");

  // Build energy graph
  const x0 = variable(100);
  const x1 = variable(-100);
  const head = div(x0, x1);
  return { inputs: [x0, x1], output: head };
};

const makeFunc = async (
  g: GradGraph,
): Promise<(xs: number[]) => { output: number; gradient: number[] }> => {
  const f = await genGradient(g.inputs, [g.output], []);
  return (xs: number[]) => {
    const masks = {
      inputMask: g.inputs.map(() => true),
      objMask: [true],
      constrMask: [],
    };
    const grad = new Float64Array(g.inputs.length);
    const { phi } = f(masks, new Float64Array(xs), 0, grad);
    return { output: phi, gradient: Array.from(grad) };
  };
};

// Compile the gradient and check it against numeric gradients
// TODO: Currently the tests will "fail" if the magnitude is greater than `eqList`'s sensitivity. Fix this.
const testGradSymbolic = async (
  testNum: number,
  graph: GradGraph,
): Promise<void> => {
  const rng = seedrandom(`testGradSymbolic graph ${testNum}`);

  // Synthesize energy and gradient code
  const f0 = await makeFunc(graph);

  const f = (xs: number[]) => f0(xs).output;
  const gradGen = (xs: number[]) => f0(xs).gradient;

  // Test the gradient at several points via evaluation
  const gradEst = _gradFiniteDiff(f);
  const testResults = [];

  for (let i = 0; i < NUM_SAMPLES; i++) {
    const xsTest = randList(rng, graph.inputs.length);
    const gradEstRes = gradEst(xsTest);
    const gradGenRes = gradGen(xsTest);

    const testRes = eqList(gradEstRes, gradGenRes);
    testResults.push(testRes);
  }

  testResults.forEach((result) => {
    expect(result).toBeTruthy();
  });
};

const gradGraph0 = (): GradGraph => {
  // Build energy/gradient graph

  // f(x) = x^2, where x is 100
  // Result: (2 * 100) * 1 <-- this comes from the (new) parent node, dx/dx = 1
  const ref = variable(100);
  const head = squared(ref);

  // Print results
  logAD.trace(
    "computational graphs for test 1 (input, output, gradient)",
    ref,
    head,
  );

  return { inputs: [ref], output: head };
};

//#endregion
