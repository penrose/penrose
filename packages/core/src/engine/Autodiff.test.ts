import {
  clearVisitedNodes,
  constOf,
  fns,
  gvarOf,
  logAD,
  markInput,
  variableAD,
  varOf,
  _genCode,
  _genEnergyFn,
  _gradADSymbolic,
  _gradAllSymbolic,
} from "engine/Autodiff";
import * as _ from "lodash";
import seedrandom from "seedrandom";
import { GradGraphs } from "types/ad";
import { eqList, randList } from "utils/Util";
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
} from "./AutodiffFunctions";

// df/f[x] with finite differences about xi
const _gradFiniteDiff = (f: (args: number[]) => number) => {
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

describe("clearVisitedNodeInput tests", () => {
  test("clears one node graph", () => {
    const var1 = varOf(1);
    var1.nodeVisited = true;
    clearVisitedNodes([var1]);
    expect(var1.nodeVisited).toEqual(false);
  });
  test("clears addition of two numbers graph", () => {
    const var1 = varOf(1);
    const var2 = varOf(2);
    const addVar = add(var1, var2);
    addVar.nodeVisited = true;
    var1.nodeVisited = true;
    clearVisitedNodes([addVar]);
    expect(var1.nodeVisited).toEqual(false);
    expect(var2.nodeVisited).toEqual(false);
    expect(addVar.nodeVisited).toEqual(false);
  });
});

describe("symbolic differentiation tests", () => {
  test("grad finite diff", () => {
    testGradFiniteDiff();
  });
  test("graph 0", () => {
    testGradSymbolic(0, gradGraph0());
  });
  test("graph 1", () => {
    testGradSymbolic(1, gradGraph1());
  });

  test("graph 2", () => {
    testGradSymbolic(2, gradGraph2());
  });

  test("graph 3", () => {
    testGradSymbolic(3, gradGraph3());
  });
  test("graph 4", () => {
    testGradSymbolic(4, gradGraph4());
  });

  test("graph 5", () => {
    testGradSymbolic(5, gradGraph5());
  });

  test("graph 6", () => {
    testGradSymbolic(6, gradGraph6());
  });

  test("graph 7", () => {
    testGradSymbolic(7, gradGraph7());
  });
});

/// TESTING CODE FROM HERE OUT
// ----- Functions for testing numeric and symbolic gradients

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

// See codegen-results.md for description
const gradGraph1 = (): GradGraphs => {
  // Build energy graph
  const x0 = markInput(variableAD(-5.0), 0);
  const x1 = markInput(variableAD(6.0), 1);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = sin(a);
  // const c = add(a, variableAD(3.0)); // const?
  const z = mul(b, c);

  // Build gradient graph
  z.gradNode = gvarOf(1.0);
  const dx0 = _gradADSymbolic(x0);
  const dx1 = _gradADSymbolic(x1);

  return {
    inputs: [x0, x1],
    energyOutput: z,
    gradOutputs: [dx0, dx1],
    weight: undefined,
  };
};

// Test addition of consts to graph (`c`)
const gradGraph2 = (): GradGraphs => {
  // Build energy graph
  const x0 = markInput(variableAD(-5.0), 0);
  const x1 = markInput(variableAD(6.0), 1);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = add(a, variableAD(3.0));
  const z = mul(b, c);

  // Build gradient graph
  z.gradNode = gvarOf(1.0);
  const dx0 = _gradADSymbolic(x0);
  const dx1 = _gradADSymbolic(x1);

  return {
    inputs: [x0, x1],
    energyOutput: z,
    gradOutputs: [dx0, dx1],
    weight: undefined,
  };
};

// Test vars w/ no grad
const gradGraph3 = (): GradGraphs => {
  // Build energy graph

  const x0 = markInput(variableAD(100.0), 0);
  const x1 = markInput(variableAD(-100.0), 0);
  const inputs = [x0, x1];
  const head = squared(x0);

  // Build gradient graph
  const dxs = _gradAllSymbolic(head, inputs);

  return {
    inputs,
    energyOutput: head,
    gradOutputs: dxs,
    weight: undefined,
  };
};

// Test toPenalty
const gradGraph4 = (): GradGraphs => {
  // Build energy graph

  const x0 = markInput(variableAD(100.0), 0);
  const inputs = [x0];
  const head = fns.toPenalty(x0);

  // Build gradient graph
  const dxs = _gradAllSymbolic(head, inputs);

  return {
    inputs,
    energyOutput: head,
    gradOutputs: dxs,
    weight: undefined,
  };
};

// Test ifCond
const gradGraph5 = (): GradGraphs => {
  // Build energy graph
  logAD.info("test ifCond");
  const [tru, fals] = [constOf(500), constOf(-500)];

  const x0 = markInput(variableAD(100.0), 0);
  const x1 = markInput(variableAD(-100.0), 0);
  const inputs = [x0, x1];

  const head = ifCond(lt(x0, constOf(33)), squared(x1), squared(x0));

  // Build gradient graph
  const dxs = _gradAllSymbolic(head, inputs);

  return {
    inputs,
    energyOutput: head,
    gradOutputs: dxs,
    weight: undefined,
  };
};

// Test max
const gradGraph6 = (): GradGraphs => {
  // Build energy graph
  logAD.info("test max");

  const x0 = markInput(variableAD(100.0), 0);
  const inputs = [x0];
  const head = max(squared(x0), constOf(0));

  // Build gradient graph
  const dxs = _gradAllSymbolic(head, inputs);

  return {
    inputs,
    energyOutput: head,
    gradOutputs: dxs,
    weight: undefined,
  };
};

// Test div
// TODO < Test all ops automatically
const gradGraph7 = (): GradGraphs => {
  // Build energy graph
  logAD.info("test div");

  const x0 = markInput(variableAD(100.0), 0);
  const x1 = markInput(variableAD(-100.0), 0);
  const inputs = [x0, x1];
  const head = div(x0, x1);

  // Build gradient graph
  const dxs = _gradAllSymbolic(head, inputs);

  return {
    inputs,
    energyOutput: head,
    gradOutputs: dxs,
    weight: undefined,
  };
};

// Given a graph with schema: { inputs: VarAD[], output: VarAD, gradOutputs: VarAD }
// Compile the gradient and check it against numeric gradients
// TODO: Currently the tests will "fail" if the magnitude is greater than `eqList`'s sensitivity. Fix this.
const testGradSymbolic = (testNum: number, graphs: GradGraphs): void => {
  const rng = seedrandom(`testGradSymbolic graph ${testNum}`);

  // Synthesize energy and gradient code
  const f0 = _genEnergyFn(graphs.inputs, graphs.energyOutput, graphs.weight);
  const gradGen0 = _genCode(
    graphs.inputs,
    graphs.gradOutputs,
    "grad",
    graphs.weight
  );

  const weight = 1; // TODO: Test with several weights
  let f;
  let gradGen;

  if (graphs.weight !== undefined) {
    // Partially apply with weight
    f = f0(weight);
    gradGen = gradGen0(weight);
  } else {
    f = f0;
    gradGen = gradGen0;
  }

  // Test the gradient at several points via evaluation
  const gradEst = _gradFiniteDiff(f);
  const testResults = [];

  for (let i = 0; i < NUM_SAMPLES; i++) {
    const xsTest = randList(rng, graphs.inputs.length);
    const energyRes = f(xsTest);
    const gradEstRes = gradEst(xsTest);
    const gradGenRes = gradGen(xsTest);

    const testRes = eqList(gradEstRes, gradGenRes);
    testResults.push(testRes);
  }

  testResults.forEach((result) => {
    expect(result).toBeTruthy();
  });
};

const gradGraph0 = (): GradGraphs => {
  // Build energy graph

  // f(x) = x^2, where x is 100
  // Result: (2 * 100) * 1 <-- this comes from the (new) parent node, dx/dx = 1
  const ref = markInput(variableAD(100.0), 0); // TODO: Should use makeADInputVars
  const head = squared(ref);

  // Build gradient graph
  head.gradNode = gvarOf(1.0);
  const dRef = _gradADSymbolic(ref);

  // Print results
  logAD.trace(
    "computational graphs for test 1 (input, output, gradient)",
    ref,
    head,
    dRef
  );

  return {
    inputs: [ref],
    energyOutput: head,
    gradOutputs: [dRef],
    weight: undefined,
  };
};
