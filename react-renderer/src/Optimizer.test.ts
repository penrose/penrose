import { minimize, gradF, tfStr, tfsStr, tfVar } from "./Optimizer";
import * as tf from "@tensorflow/tfjs";

const fn = (...args: any[]) =>
  args.reduce((res, n) => res.add(n.square()), tf.scalar(0));
const state = [tfVar(100), tfVar(25), tfVar(0)];

// const fn = (x, y) => x.sub(y).square();
// const state = [tfVar(20), tfVar(5)];

describe("minimize a simple function", () => {
  it("minimize a simple state with L2 norm", () => {
    // expect(tf.lessStrict(tfVar(20), tfVar(100))).toBe(true);
  });
  it("minimize a simple state with L2 norm", () => {
    const { energy, norm_grad, i } = minimize(fn, tf.grads(fn), state, []);
    console.log(
      "converged after",
      i,
      "steps with energy",
      tfStr(energy),
      "and grad norm",
      norm_grad
    );
    console.log("state (varyingMap): ", tfsStr(state));
  });

  it("evaluate a single energy function f(x)", () => {
    console.log(fn(...state).print());
  });
});
