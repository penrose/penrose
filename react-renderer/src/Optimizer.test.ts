import {
  minimize,
  gradF,
  tfStr,
  tfsStr,
  tfVar,
  constrDict,
  dist,
  center,
} from "./Optimizer";
import * as tf from "@tensorflow/tfjs";

const fn = (...args: tf.Scalar[]) =>
  args.reduce((res, n) => res.add(n.square()), tf.scalar(0));
const state = [tfVar(100), tfVar(25), tfVar(0)];

// const fn = (x, y) => x.sub(y).square();
// const state = [tfVar(20), tfVar(5)];

// describe("minimizing actual Penrose example", () => {
// });

describe("minimize a simple function", () => {
  it("logical comparison of tensors that returns a js value", () => {
    // expect(tf.lessStrict(tfVar(20), tfVar(100))).toBe(true);
  });
  it("minimize a simple state with L2 norm", () => {
    const { energy, normGrad, i } = minimize(fn, gradF(fn) as any, state, []);
    console.log(
      "converged after",
      i,
      "steps with energy",
      tfStr(energy),
      "and grad norm",
      normGrad
    );
    console.log("state (varyingMap): ", tfsStr(state));
  });

  it("evaluate a single energy function f(x)", () => {
    console.log(fn(...state).print());
  });
});

describe("contraint functions test", () => {
  it("tests opt function contains", () => {
    const fn = constrDict["contains"];
    const c1: [string, any] = [
      "Circle",
      { x: { contents: 0 }, y: { contents: 0 }, r: { contents: 10 } },
    ];
    const c2: [string, any] = [
      "Circle",
      { x: { contents: 0 }, y: { contents: 0 }, r: { contents: 5 } },
    ];
    expect(tfStr(dist(center(c1[1]), center(c2[1])))).toEqual(0);
    expect(tfStr(fn(c1, c2, 0))).toEqual(-5);
  });
});
