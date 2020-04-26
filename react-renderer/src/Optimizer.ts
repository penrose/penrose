import * as tf from "@tensorflow/tfjs";

export const tfStr = x => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map(e => tfStr(e));
export const tfVar = e => tf.scalar(e).variable();
const learningRate = 0.5; // TODO Try different learning rates
const optimizer2 = tf.train.adam(learningRate);

export const gradF: any = tf.grads;
// Use included tf.js optimizer to minimize f over xs (note: xs is mutable)
export const minimize = (
  f: (arg0: any) => tf.Scalar,
  gradf: any,
  xs: any,
  names: any
) => {
  // optimization hyperparameters
  const EPS = 1e-3;
  const MAX_STEPS = 1000;

  let energy;
  let gradfx;
  let norm_grad = Number.MAX_SAFE_INTEGER;
  let i = 0;

  // console.log("xs0", tfsStr(xs));
  // console.log("f'(xs0)", tfsStr(gradf(xs)));

  // TODO profile this
  while (norm_grad > EPS && i < MAX_STEPS) {
    // TODO: use/revert spread, also doesn't work with varList=xs and compGraph1
    energy = optimizer2.minimize(() => f(...xs), true);
    gradfx = gradf(xs);
    norm_grad = tf
      .stack(gradfx)
      .norm()
      .dataSync()[0]; // not sure how to compare a tensor to a scalar
    // TODO: use tf logical operator

    // note: this printing could tank the performance
    // vals = xs.map(v => v.dataSync()[0]);
    // console.log("i=", i);
    // console.log("state", tups2obj(names, vals));
    // console.log(`f(xs): ${energy}`);
    // console.log("f'(xs)", tfsStr(gradfx));
    // console.log("||f'(xs)||", norm_grad);
    // console.log("cond", norm_grad > EPS, i < MAX_STEPS);
    i++;
  }

  return { energy, norm_grad, i };
};
