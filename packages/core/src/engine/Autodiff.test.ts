import _ from "lodash";
import seedrandom from "seedrandom";
import { describe, expect, test } from "vitest";
import { numsOf } from "../contrib/Utils.js";
import * as ad from "../types/ad.js";
import { eqList, randList } from "../utils/Util.js";
import {
  compile,
  fns,
  genCode,
  genCodeSync,
  input,
  logAD,
  makeGraph,
  primaryGraph,
  secondaryGraph,
} from "./Autodiff.js";
import {
  add,
  addN,
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

describe("makeGraph tests", () => {
  test("secondary outputs", () => {
    // the values 0 don't matter for this test
    const x = input(0);
    const y = input(0);
    const sum = add(x, y);
    const product = mul(x, y);
    const difference = sub(x, y);

    const { graph, gradient, secondary } = secondaryGraph([
      sum,
      product,
      difference,
    ]);

    expect(gradient.size).toBe(0); // no inputs reachable from primary output
    expect(secondary.length).toBe(3);

    const nodes: ad.Node[] = secondary.map((id) => graph.node(id));
    expect(nodes[0]).toEqual({ tag: "Binary", binop: "+" });
    expect(nodes[1]).toEqual({ tag: "Binary", binop: "*" });
    expect(nodes[2]).toEqual({ tag: "Binary", binop: "-" });
  });

  test("no expression swell", () => {
    // https://arxiv.org/pdf/1904.02990.pdf Figure 2
    const x1 = input(0); // 0, doesn't matter for this test
    const t1 = mul(x1, x1);
    const t2 = mul(t1, t1);
    const f = mul(t2, t2);

    const { graph } = secondaryGraph([f]);

    // x1, t1, t2, f, and the constant primary node 1
    expect(graph.nodeCount()).toBe(5);
  });
});

describe("genCode tests", () => {
  test("zero addends", async () => {
    const f = await genCode();
    expect(f((x) => x.val)).toEqual({
      gradient: new Map(),
      primary: 0,
      secondary: [],
    });
  });

  test("zero addends sync", () => {
    const f = genCodeSync();
    expect(f((x) => x.val)).toEqual({
      gradient: new Map(),
      primary: 0,
      secondary: [],
    });
  });

  test("multiple addends", () => {
    const x = input(2);
    const g = primaryGraph(x);
    const f = genCodeSync(g, g, g);
    expect(f((x) => x.val)).toEqual({
      gradient: new Map([[x, 3]]),
      primary: 6,
      secondary: [],
    });
  });

  test("multiple graphs with secondary outputs", () => {
    const v1 = [5];
    const v2 = [];
    v2[1] = 8;
    const f = genCodeSync(secondaryGraph(v1), secondaryGraph(v2));
    expect(f((x) => x.val).secondary).toEqual([5, 8]);
  });

  test("secondary outputs must not conflict", () => {
    const g1 = secondaryGraph([5]);
    const g2 = secondaryGraph([8]);
    expect(() => genCodeSync(g1, g2)).toThrow(
      "secondary output 0 is present in 2 graphs"
    );
  });

  test("mask", () => {
    const primary = input(13);
    const v1 = [5];
    const v2 = [];
    v2[1] = 8;
    const f = genCodeSync(
      makeGraph({ primary, secondary: v2 }),
      makeGraph({ primary, secondary: v1 })
    );
    expect(f((x) => x.val, [true, false])).toEqual({
      gradient: new Map([[primary, 1]]),
      primary: 13,
      secondary: [0, 8],
    });
  });
});

describe("compile tests", () => {
  test("no outputs", async () => {
    const f = await compile([]);
    const v = f((x) => x.val);
    expect(v).toEqual([]);
  });

  test("simple", async () => {
    const x = input(3);
    const y = input(4);
    const f = await compile([add(x, y), mul(x, y)]);
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

  test("graph 8", () => {
    testGradSymbolic(8, gradGraph8());
  });
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
  inputs: ad.Input[];
  output: ad.Num;
}

const gradGraph1 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = input(-5);
  const x1 = input(6);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = sin(a);
  const z = mul(b, c);
  return { inputs: [x0, x1], output: z };
};

// Test addition of consts to graph (`c`)
const gradGraph2 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = input(-5);
  const x1 = input(6);
  const a = sub(x0, x1);
  const b = squared(a);
  const c = add(a, 3);
  const z = mul(b, c);
  return { inputs: [x0, x1], output: z };
};

// Test vars w/ no grad
const gradGraph3 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = input(100);
  const x1 = input(-100);
  const head = squared(x0);
  return { inputs: [x0, x1], output: head };
};

// Test toPenalty
const gradGraph4 = (): GradGraph => {
  // Build energy/gradient graph
  const x0 = input(100);
  const head = fns.toPenalty(x0);
  return { inputs: [x0], output: head };
};

// Test ifCond
const gradGraph5 = (): GradGraph => {
  logAD.info("test ifCond");

  // Build energy/gradient graph
  const x0 = input(100);
  const x1 = input(-100);
  const head = ifCond(lt(x0, 33), squared(x1), squared(x0));
  return { inputs: [x0, x1], output: head };
};

// Test max
const gradGraph6 = (): GradGraph => {
  logAD.info("test max");

  // Build energy/gradient graph
  const x0 = input(100);
  const head = max(squared(x0), 0);
  return { inputs: [x0], output: head };
};

// Test div
// TODO < Test all ops automatically
const gradGraph7 = (): GradGraph => {
  logAD.info("test div");

  // Build energy graph
  const x0 = input(100);
  const x1 = input(-100);
  const head = div(x0, x1);
  return { inputs: [x0, x1], output: head };
};

// Test polyRoots
const gradGraph8 = (): GradGraph => {
  logAD.info("test polyRoots");

  // Build energy/gradient graph
  const x0 = input(0);
  const x1 = input(0);
  const x2 = input(0);
  const x3 = input(0);
  const x4 = input(0);
  const roots = polyRoots([x0, x1, x2, x3, x4]);
  const head = addN(roots.map((r) => ifCond(eq(r, r), r, 0)));
  return { inputs: [x0, x1, x2, x3, x4], output: head };
};

const makeFunc = (
  g: GradGraph
): ((xs: number[]) => { output: number; gradient: number[] }) => {
  const indices = new Map(g.inputs.map((x, i) => [x, i]));
  const f = genCodeSync(primaryGraph(g.output));
  return (xs: number[]) => {
    const outputs = f((x) => xs[indices.get(x)!]);
    const gradient = xs.map(() => 0);
    for (const [v, x] of outputs.gradient) gradient[indices.get(v)!] = x;
    return { output: outputs.primary, gradient };
  };
};

// Compile the gradient and check it against numeric gradients
// TODO: Currently the tests will "fail" if the magnitude is greater than `eqList`'s sensitivity. Fix this.
const testGradSymbolic = (testNum: number, graph: GradGraph): void => {
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
  const ref = input(100);
  const head = squared(ref);

  // Print results
  logAD.trace(
    "computational graphs for test 1 (input, output, gradient)",
    ref,
    head
  );

  return { inputs: [ref], output: head };
};

//#endregion

describe("polyRoots tests", () => {
  test("degree 1", () => {
    const x = 42;
    const v = input(x);
    const [z] = polyRoots([v]);
    const g = primaryGraph(z);
    const f = genCodeSync(g);
    expect(f((v) => v.val)).toEqual({
      gradient: new Map([[v, -1]]),
      primary: -x,
      secondary: [],
    });
  });

  type F = (v: ad.Num, w: ad.Num) => ad.Num;

  // check that `polyRoots` gives the same answer as just doing symbolic
  // differentiation on the quadratic formula
  const testQuadratic = (f1: F, f2: F) => {
    const x1 = Math.PI;
    const x2 = Math.E;

    const a = 1;
    const b = input(-(x1 + x2));
    const c = input(x1 * x2);

    const closedForm = genCodeSync(
      primaryGraph(
        // c + bx + ax²
        div(f1(neg(b), sqrt(sub(squared(b), mul(4, mul(a, c))))), mul(2, a))
      )
    );

    const [r1, r2] = polyRoots([c, b]); // c + bx + x²; recall that a = 1
    const implicit = genCodeSync(primaryGraph(f2(r1, r2)));

    const received = implicit((x) => x.val);
    const expected = closedForm((x) => x.val);

    expect(received.primary).toBeCloseTo(expected.primary);

    const inputs = new Set([b, c]);
    expect(new Set(received.gradient.keys())).toEqual(inputs);
    expect(new Set(expected.gradient.keys())).toEqual(inputs);
    expect(received.gradient.get(b)).toBeCloseTo(expected.gradient.get(b)!);
    expect(received.gradient.get(c)).toBeCloseTo(expected.gradient.get(c)!);
  };

  test("quadratic formula min root", () => {
    testQuadratic(sub, min);
  });

  test("quadratic formula max root", () => {
    testQuadratic(add, max);
  });

  test("cubic with only one real root", () => {
    const [c0, c1, c2] = [input(8), input(0), input(0)];
    const [r1, r2, r3] = polyRoots([c0, c1, c2]);

    // get the first real root we can find
    const z = ifCond(eq(r1, r1), r1, ifCond(eq(r2, r2), r2, r3));

    const f = genCodeSync(makeGraph({ primary: z, secondary: [r1, r2, r3] }));

    const { gradient, primary, secondary } = f((v) => v.val);

    expect(secondary.filter(Number.isNaN).length).toBe(2);
    const realRoots = secondary.filter((x) => !Number.isNaN(x));
    expect(realRoots.length).toBe(1);
    const [x] = realRoots;
    expect(x).toBeCloseTo(-2);

    expect(primary).toBeCloseTo(-2);

    expect(new Set(gradient.keys())).toEqual(new Set([c0, c1, c2]));
    expect(gradient.get(c0)).toBeCloseTo(-1 / 12);
    expect(gradient.get(c1)).toBeCloseTo(1 / 6);
    expect(gradient.get(c2)).toBeCloseTo(-1 / 3);
  });

  test("quintic", () => {
    const [c0, c1, c2, c3, c4] = [
      input(-120),
      input(274),
      input(-225),
      input(85),
      input(-15),
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
