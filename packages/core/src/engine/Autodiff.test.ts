import * as _ from "lodash";
import { all, randList, eqList } from "utils/OtherUtils";
import { GradGraphs } from "types/ad";
import {
  clearVisitedNodes,
  gvarOf,
  logAD,
  markInput,
  NUM_SAMPLES,
  variableAD,
  varOf,
  constOf,
  _gradADSymbolic,
  _genCode,
  _gradAllSymbolic,
  _gradFiniteDiff,
  _genEnergyFn,
  fns,
} from "engine/Autodiff";
import {
  acos,
  add,
  addN,
  atan2,
  cos,
  div,
  ifCond,
  lt,
  max,
  mul,
  neg,
  sin,
  sqrt,
  squared,
  sub,
} from "./AutodiffFunctions";

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

/// TESTING CODE FROM HERE OUT
// ----- Functions for testing numeric and symbolic gradients

const assert = (b: boolean, s: any[]) => {
  const res = b ? "passed" : "failed";
  logAD.trace("Assertion", res, ": ", ...s);
  return b;
};

const testGradFiniteDiff = () => {
  // Only tests with hardcoded functions
  const f = (ys: number[]) => _.sum(_.map(ys, (e: number) => e * e));
  const df = (ys: number[]) => _.map(ys, (e: number) => 2 * e);

  const testResults = [];

  for (let i = 0; i < NUM_SAMPLES; i++) {
    const xs = randList(4);
    const gradEstRes = _gradFiniteDiff(f)(xs);
    const expectedRes = df(xs);
    const testRes = assert(eqList(gradEstRes, expectedRes), [
      "test grad finite diff (grad res, expected res)",
      gradEstRes,
      expectedRes,
    ]);
    testResults.push(testRes);
  }

  const testOverall = assert(all(testResults), [
    "all tests passed? test results:",
    testResults,
  ]);
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
  z.gradNode = { tag: "Just", contents: gvarOf(1.0) };
  const dx0 = _gradADSymbolic(x0);
  const dx1 = _gradADSymbolic(x1);

  return {
    inputs: [x0, x1],
    energyOutput: z,
    gradOutputs: [dx0, dx1],
    weight: { tag: "Nothing" },
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
  z.gradNode = { tag: "Just", contents: gvarOf(1.0) };
  const dx0 = _gradADSymbolic(x0);
  const dx1 = _gradADSymbolic(x1);

  return {
    inputs: [x0, x1],
    energyOutput: z,
    gradOutputs: [dx0, dx1],
    weight: { tag: "Nothing" },
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
    weight: { tag: "Nothing" },
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
    weight: { tag: "Nothing" },
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
    weight: { tag: "Nothing" },
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
    weight: { tag: "Nothing" },
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
    weight: { tag: "Nothing" },
  };
};

export const testGradSymbolicAll = (): void => {
  logAD.trace("testing symbolic gradients");

  testGradFiniteDiff();

  const graphs: GradGraphs[] = [
    gradGraph0(),
    gradGraph1(),
    gradGraph2(),
    gradGraph3(),
    gradGraph4(),
    gradGraph5(),
    gradGraph6(),
    gradGraph7(),
  ];

  const testResults = graphs.map((graph, i) => testGradSymbolic(i, graph));

  logAD.trace(`All grad symbolic tests passed?: ${all(testResults)}`);
};

// Given a graph with schema: { inputs: VarAD[], output: VarAD, gradOutputs: VarAD }
// Compile the gradient and check it against numeric gradients
// TODO: Currently the tests will "fail" if the magnitude is greater than `eqList`'s sensitivity. Fix this.
const testGradSymbolic = (testNum: number, graphs: GradGraphs): boolean => {
  logAD.trace(`======= START TEST GRAD SYMBOLIC ${testNum} ======`);

  logAD.trace("head node (energy  output)", graphs.energyOutput);
  logAD.trace("inputs", graphs.inputs);
  logAD.trace("grad node(s)", graphs.gradOutputs);

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
  logAD.trace("testGradSymbolic has weight?", graphs.weight);

  if (graphs.weight.tag === "Just") {
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
    const xsTest = randList(graphs.inputs.length);
    const energyRes = f(xsTest);
    const gradEstRes = gradEst(xsTest);
    const gradGenRes = gradGen(xsTest);

    logAD.trace("----");
    logAD.trace("test", i);
    logAD.trace("energy at x", xsTest, "=", energyRes);
    logAD.trace("estimated gradient at", xsTest, "=", gradEstRes);
    logAD.trace("analytic gradient at", xsTest, "=", gradGenRes);

    const testRes = assert(eqList(gradEstRes, gradGenRes), [
      "estimated, analytic gradients:",
      gradEstRes,
      gradGenRes,
    ]);
    testResults.push(testRes);
  }

  const testOverall = assert(all(testResults), [
    "all tests passed? test results:",
    testResults,
  ]);

  // TODO: Visualize both of them
  logAD.trace(`======= DONE WITH TEST GRAD SYMBOLIC ${testNum} ======`);

  return testOverall;
};

const gradGraph0 = (): GradGraphs => {
  // Build energy graph

  // f(x) = x^2, where x is 100
  // Result: (2 * 100) * 1 <-- this comes from the (new) parent node, dx/dx = 1
  const ref = markInput(variableAD(100.0), 0); // TODO: Should use makeADInputVars
  const head = squared(ref);

  // Build gradient graph
  head.gradNode = { tag: "Just", contents: gvarOf(1.0) };
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
    weight: { tag: "Nothing" },
  };
};
