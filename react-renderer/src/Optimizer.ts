import * as tf from "@tensorflow/tfjs";
import {
  Tensor,
  stack,
  scalar,
  Variable,
  Scalar,
  maximum,
} from "@tensorflow/tfjs";
import {
  argValue,
  evalTranslation,
  insertVaryings,
  genVaryMap,
  evalFns,
} from "./Evaluator";
import { zip } from "lodash";
import { constrDict, objDict } from "./Constraints";

// HACK: constant constraint weight
const constraintWeight = 10e4;

const toPenalty = (x: Tensor): Tensor => {
  return tf.pow(tf.maximum(x, tf.scalar(0)), tf.scalar(2));
};

export const evalEnergyOn = (state: State) => {
  // NOTE: this will greatly improve the performance of the optmizer
  // TODO: move this decl to somewhere else
  tf.setBackend("cpu");
  const { objFns, constrFns, translation, varyingPaths } = state;
  // TODO: types
  const applyFn = (f: FnDone<Tensor>, dict: any) => {
    if (dict[f.name]) {
      return dict[f.name](...f.args.map(argValue));
    } else {
      throw new Error(
        `constraint or objective ${f.name} not found in dirctionary`
      );
    }
  };
  return (...varyingValuesTF: Variable[]): Scalar => {
    // construct a new varying map
    const varyingMap = genVaryMap(varyingPaths, varyingValuesTF) as VaryMap<
      Variable
    >;

    const objEvaled = evalFns(objFns, translation, varyingMap);
    const constrEvaled = evalFns(constrFns, translation, varyingMap);

    const objEngs: Tensor[] = objEvaled.map((o) => applyFn(o, objDict));
    const constrEngs: Tensor[] = constrEvaled.map((c) =>
      toPenalty(applyFn(c, constrDict))
    );

    // Printing to check for NaNs
    // console.log(
    //   zip(
    //     constrEngs.map((e) => e.toString()),
    //     constrEvaled.map((c) => c.name)
    //   ),
    //   varyingMap
    // );

    const objEng: Tensor =
      objEngs.length === 0 ? differentiable(0) : stack(objEngs).sum();
    const constrEng: Tensor =
      constrEngs.length === 0 ? differentiable(0) : stack(constrEngs).sum();
    const overallEng = objEng.add(
      constrEng.mul(scalar(constraintWeight * state.paramsr.weight))
    );

    // NOTE: the current version of tfjs requires all input variables to have gradients (i.e. actually involved when computing the overall energy). See https://github.com/tensorflow/tfjs-core/blob/8c2d9e05643988fa7f4575c30a5ad3e732d189b2/tfjs-core/src/engine.ts#L726
    // HACK: therefore, we try to work around it by using all varying values without affecting the value and gradients of the energy function
    const dummyVal = stack(varyingValuesTF).sum();
    return overallEng.add(dummyVal.mul(scalar(0)));

    // DEBUG: manually constructed comp graph and gradient tests
    // const [r1, x1, y1, r2, x2, y2] = varyingValuesTF;
    // const contains = dist(stack([x1, y1]), stack([x2, y2]));
    // const limit = scalar(20);
    // const minsize1 = stack([r1, limit.neg()]).sum();
    // const minsize2 = stack([r2, limit.neg()]).sum();
    // const limit2 = scalar(Math.max(...canvasSize) / 6);
    // const maxSize1 = stack([r1.neg(), limit2]).sum();
    // // const maxSize2 = stack([r1.neg(), limit2]).sum();
    // const eng = stack([contains, minsize1, minsize2]).sum();
    // return eng
    //   .asScalar()
    //   .add(dummyVal)
    //   .sub(dummyVal);
  };
};

export const step = (state: State, steps: number) => {
  const f = evalEnergyOn(state);
  const fgrad = gradF(f);
  // const xs = state.varyingValues.map(differentiable);
  const xs = state.varyingState; // NOTE: use cached varying values
  // NOTE: minimize will mutates xs
  const { energy } = minimize(f, fgrad, xs, steps);
  // insert the resulting variables back into the translation for rendering
  const varyingValues = xs.map((x) => scalarValue(x));
  const trans = insertVaryings(
    state.translation,
    zip(state.varyingPaths, varyingValues) as [Path, number][]
  );
  const newState = { ...state, translation: trans, varyingValues };
  if (scalarValue(energy) > 10) {
    // const newState = { ...state, varyingState: xs };
    newState.paramsr.optStatus.tag = "UnconstrainedRunning";
    console.log(`Took ${steps} steps. Current energy`, scalarValue(energy));
    // return newState;
  } else {
    // const varyingValues = xs.map((x) => tfStr(x));
    // const trans = insertVaryings(
    //   state.translation,
    //   zip(state.varyingPaths, varyingValues) as [Path, number][]
    // );
    // const newState = { ...state, translation: trans, varyingValues };
    newState.paramsr.optStatus.tag = "EPConverged";
    console.log("Converged with energy", scalarValue(energy));
    // return evalTranslation(newState);
  }
  // return the state with a new set of shapes
  return evalTranslation(newState);
};

////////////////////////////////////////////////////////////////////////////////
// All TFjs related functions

// TODO: types
export const scalarValue = (x: Scalar): number => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map((e) => scalarValue(e));
export const differentiable = (e: number): Variable => tf.scalar(e).variable();
// const learningRate = 0.5; // TODO Try different learning rates
const learningRate = 50; // TODO Try different learning rates
// const optimizer = tf.train.adam(learningRate);
const optimizer = tf.train.adam(learningRate, 0.9, 0.999);

export const gradF = (fn: any) => tf.grads(fn);

/**
 * Use included tf.js optimizer to minimize f over xs (note: xs is mutable)
 *
 * @param {(...arg: tf.Tensor[]) => tf.Tensor} f overall energy function
 * @param {(...arg: tf.Tensor[]) => tf.Tensor[]} gradf gradient function
 * @param {tf.Tensor[]} xs varying state
 * @param {*} names // TODO: what is this
 * @returns // TODO: document
 */
export const minimize = (
  f: (...arg: Variable[]) => Scalar,
  gradf: (arg: Tensor[]) => Tensor[],
  xs: Variable[],
  maxSteps = 100
): {
  energy: Scalar;
  normGrad: number;
  i: number;
} => {
  // optimization hyperparameters
  const EPS = 1e-3;

  let energy; // to be returned
  // let normGrad = Number.MAX_SAFE_INTEGER;
  const normGrad = Number.MAX_SAFE_INTEGER;
  let i = 0;

  // console.log("xs0", tfsStr(xs));
  // console.log("f'(xs0)", tfsStr(gradf(xs)));

  // TODO profile this
  while (i < maxSteps) {
    // while (normGrad > EPS && i < MAX_STEPS) {
    // TODO: use/revert spread, also doesn't work with varList=xs and compGraph1
    energy = optimizer.minimize(() => f(...xs) as any, true);
    // const gradfx = gradf(xs);
    // normGrad = tf
    //   .stack(gradfx)
    //   .norm()
    //   .dataSync()[0]; // not sure how to compare a tensor to a scalar
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

  return { energy: energy as Scalar, normGrad, i };
};
