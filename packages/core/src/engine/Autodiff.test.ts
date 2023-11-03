import _ from "lodash";
import seedrandom from "seedrandom";
import { describe, expect, test } from "vitest";
import { numsOf } from "../lib/Utils.js";
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
  eq,
  ifCond,
  lt,
  max,
  min,
  mul,
  neg,
  polyRoots,
  sin,
  sqrt,
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
  test("no outputs", () => {
    const f = compile([]);
    const v = f((x) => x.val);
    expect(v).toEqual([]);
  });

  test("simple", () => {
    const x = variable(3);
    const y = variable(4);
    const f = compile([add(x, y), mul(x, y)]);
    const v = f((x) => x.val);
    expect(v).toEqual([7, 12]);
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

const makeFunc = (
  g: GradGraph,
): ((xs: number[]) => { output: number; gradient: number[] }) => {
  const f = genGradient(g.inputs, [g.output], []);
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
  const f0 = makeFunc(graph);

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

describe("polyRoots tests", () => {
  test("degree 1", () => {
    const x = 42;
    const v = variable(x);
    const [z] = polyRoots([v]);
    const f = makeFunc({ inputs: [v], output: z });
    expect(f([v.val])).toEqual({ output: -x, gradient: [-1] });
  });

  type F = (v: ad.Num, w: ad.Num) => ad.Num;

  // check that `polyRoots` gives the same answer as just doing symbolic
  // differentiation on the quadratic formula
  const testQuadratic = (f1: F, f2: F) => {
    const x1 = Math.PI;
    const x2 = Math.E;

    const a = 1;
    const b = variable(-(x1 + x2));
    const c = variable(x1 * x2);

    const closedForm = makeFunc({
      inputs: [b, c],
      // c + bx + ax²
      output: div(
        f1(neg(b), sqrt(sub(squared(b), mul(4, mul(a, c))))),
        mul(2, a),
      ),
    });

    const [r1, r2] = polyRoots([c, b]); // c + bx + x²; recall that a = 1
    const implicit = makeFunc({ inputs: [b, c], output: f2(r1, r2) });

    const received = implicit([b.val, c.val]);
    const expected = closedForm([b.val, c.val]);

    expect(received.output).toBeCloseTo(expected.output);

    const inputs = new Set([b, c]);
    expect(received.gradient[0]).toBeCloseTo(expected.gradient[0]);
    expect(received.gradient[1]).toBeCloseTo(expected.gradient[1]);
  };

  test("quadratic formula min root", () => {
    testQuadratic(sub, min);
  });

  test("quadratic formula max root", () => {
    testQuadratic(add, max);
  });

  test("cubic with only one real root", () => {
    const inputs = [variable(8), variable(0), variable(0)];
    const [c0, c1, c2] = inputs;

    const roots = polyRoots([c0, c1, c2]);
    const [r1, r2, r3] = roots;

    // get the first real root we can find
    const z = ifCond(eq(r1, r1), r1, ifCond(eq(r2, r2), r2, r3));

    const f = makeFunc({ inputs, output: z });

    const { output, gradient } = f([c0.val, c1.val, c2.val]);
    const secondary = numsOf(roots);

    expect(secondary.filter(Number.isNaN).length).toBe(2);
    const realRoots = secondary.filter((x) => !Number.isNaN(x));
    expect(realRoots.length).toBe(1);
    const [x] = realRoots;
    expect(x).toBeCloseTo(-2);

    expect(output).toBeCloseTo(-2);

    expect(gradient[0]).toBeCloseTo(-1 / 12);
    expect(gradient[1]).toBeCloseTo(1 / 6);
    expect(gradient[2]).toBeCloseTo(-1 / 3);
  });

  test("quintic", () => {
    const [c0, c1, c2, c3, c4] = [
      variable(-120),
      variable(274),
      variable(-225),
      variable(85),
      variable(-15),
    ];
    const roots = numsOf(polyRoots([c0, c1, c2, c3, c4]));
    roots.sort((a, b) => a - b);
    expect(roots[0]).toBeCloseTo(1);
    expect(roots[1]).toBeCloseTo(2);
    expect(roots[2]).toBeCloseTo(3);
    expect(roots[3]).toBeCloseTo(4);
    expect(roots[4]).toBeCloseTo(5);
  });
});
