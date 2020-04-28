import {
  minimize,
  gradF,
  tfStr,
  tfsStr,
  differentiable,
  constrDict,
  dist,
  center,
  evalEnergyOn,
  stepUntilConvergence,
} from "../Optimizer";
import * as tf from "@tensorflow/tfjs";
import * as stateJSON from "./venn-opt-initial.json";
import { decodeState } from "../Evaluator";
import { scalar, variableGrads } from "@tensorflow/tfjs";

const fn = (...args: tf.Tensor[]): tf.Scalar =>
  args.reduce((res, n) => res.add(n.square()), tf.scalar(0)).asScalar();
const state = [differentiable(100), differentiable(25), differentiable(0)];

// const fn = (x, y) => x.sub(y).square();
// const state = [tfVar(20), tfVar(5)];

// describe("minimizing actual Penrose example", () => {
// });
describe("Whole optimizer pipeline tests", () => {
  const vennState = decodeState(stateJSON.contents);
  it("evaluates the energy and its gradient of an initial state", () => {
    const f = evalEnergyOn(vennState);
    const xs = vennState.varyingValues.map(differentiable);

    console.log("Overall energy of the state is:");
    f(...xs).print();
    console.log("Gradients: ");
    // variableGrads(f, xs);
    const gf = gradF(f);
    tf.stack(gf(xs)).print();
  });
  it("steps the initial state until convergence", () => {
    stepUntilConvergence(vennState);
  });
});

describe("minimize a simple function", () => {
  it("logical comparison of tensors that returns a js value", () => {
    // expect(tf.lessStrict(tfVar(20), tfVar(100))).toBe(true);
  });
  it("evaluate a single energy function f(x)", () => {
    console.log("The value of the function is: ", fn(...state).toString());
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
});

describe("contraint functions test", () => {
  it("tests opt function contains", () => {
    const contains = constrDict.contains;
    const c1: [string, any] = [
      "Circle",
      {
        x: { contents: scalar(0) },
        y: { contents: scalar(0) },
        r: { contents: scalar(10) },
      },
    ];
    const c2: [string, any] = [
      "Circle",
      {
        x: { contents: scalar(0) },
        y: { contents: scalar(0) },
        r: { contents: scalar(5) },
      },
    ];
    expect(tfStr(dist(center(c1[1]), center(c2[1])))).toEqual(0);
    expect(tfStr(contains(c1, c2, scalar(0)))).toEqual(-5);
  });
});
