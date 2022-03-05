import {
  addGradient,
  compile,
  fns,
  input,
  logAD,
  makeGraph,
} from "engine/Autodiff";
import * as _ from "lodash";
import seedrandom from "seedrandom";
import * as ad from "types/ad";
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

describe("makeGraph tests", () => {
  test("no expression swell", () => {
    // https://arxiv.org/pdf/1904.02990.pdf Figure 2
    const x1 = input("x1");
    const t1 = mul(x1, x1);
    const t2 = mul(t1, t1);
    const f = mul(t2, t2);

    const { graph } = makeGraph([f]);
    expect(graph.nodeCount()).toBe(4);
    expect(graph.edgeCount()).toBe(6);
  });
});

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

const gradGraph1 = (): SymbolicTest => {
  // Build energy graph
  const x0 = input("x0");
  const x1 = input("x1");
  const inputs = new Map([
    ["x0", -5],
    ["x1", 6],
  ]);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = sin(a);
  const z = mul(b, c);
  const graph = makeGraph([z]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test addition of consts to graph (`c`)
const gradGraph2 = (): SymbolicTest => {
  // Build energy graph
  const x0 = input("x0");
  const x1 = input("x1");
  const inputs = new Map([
    ["x0", -5],
    ["x1", 6],
  ]);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = add(a, 3);
  const z = mul(b, c);
  const graph = makeGraph([z]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test vars w/ no grad
const gradGraph3 = (): SymbolicTest => {
  // Build energy graph
  const x0 = input("x0");
  const x1 = input("x1");
  const inputs = new Map([
    ["x0", 100],
    ["x1", -100],
  ]);
  const head = squared(x0);
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test toPenalty
const gradGraph4 = (): SymbolicTest => {
  // Build energy graph
  const x0 = input("x0");
  const inputs = new Map([["x0", 100]]);
  const head = fns.toPenalty(x0);
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test ifCond
const gradGraph5 = (): SymbolicTest => {
  logAD.info("test ifCond");

  // Build energy graph
  const x0 = input("x0");
  const x1 = input("x1");
  const inputs = new Map([
    ["x0", 100],
    ["x1", -100],
  ]);
  const head = ifCond(lt(x0, 33), squared(x1), squared(x0));
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test max
const gradGraph6 = (): SymbolicTest => {
  logAD.info("test max");

  // Build energy graph
  const x0 = input("x0");
  const inputs = new Map([["x0", 100]]);
  const head = max(squared(x0), 0);
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

// Test div
// TODO < Test all ops automatically
const gradGraph7 = (): SymbolicTest => {
  logAD.info("test div");

  // Build energy graph
  const x0 = input("x0");
  const x1 = input("x1");
  const inputs = new Map([
    ["x0", 100],
    ["x1", -100],
  ]);
  const head = div(x0, x1);
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  return { graph, inputs };
};

interface SymbolicTest {
  graph: ad.Graph;
  inputs: Map<string, number>;
}

// Compile the gradient and check it against numeric gradients
// TODO: Currently the tests will "fail" if the magnitude is greater than `eqList`'s sensitivity. Fix this.
const testGradSymbolic = (testNum: number, test: SymbolicTest): void => {
  const rng = seedrandom(`testGradSymbolic graph ${testNum}`);

  // Synthesize energy and gradient code
  const f = compile(test.graph);

  const weight = 1; // TODO: Test with several weights

  // Test the gradient at several points via evaluation
  const gradEst = _gradFiniteDiff(f);
  const testResults = [];

  for (let i = 0; i < NUM_SAMPLES; i++) {
    const xsTest = randList(rng, test.inputs.size);
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

const gradGraph0 = (): SymbolicTest => {
  // Build energy graph

  // f(x) = x^2, where x is 100
  // Result: (2 * 100) * 1 <-- this comes from the (new) parent node, dx/dx = 1
  const ref = input("ref");
  const inputs = new Map([["ref", 100]]);
  const head = squared(ref);
  const graph = makeGraph([head]);

  // Build gradient graph
  addGradient(graph, 0);

  // Print results
  logAD.trace(
    "computational graphs for test 1 (input, output, gradient)",
    ref,
    head
  );

  return { graph, inputs };
};
