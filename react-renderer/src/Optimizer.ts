import * as tf from "@tensorflow/tfjs";
import { Tensor1D, Scalar, stack, scalar } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import { argValue, evalFn, evalTranslation, insertVaryings } from "./Evaluator";
import { zip } from "lodash";
import { insertPending } from "./PropagateUpdate";

// HACK: constant constraint weight
const constraintWeight = 10e4;

export const objDict = {};
export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(Math.max(...canvasSize) / 6);
    switch (shapeType) {
      case "Circle":
        return tf.stack([props.r.contents.sub(limit)]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },
  minSize: ([shapeType, props]: [string, any]) => {
    const limit = 20;
    switch (shapeType) {
      case "Circle":
        return tf.stack([limit, props.r.contents.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: Scalar
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = offset
        ? tf.stack([s1.r.contents, s2.r.contents.neg(), offset.neg()])
        : tf.stack([s1.r.contents, s2.r.contents.neg()]);
      return d.sub(o.sum());
    } else throw new Error(`${[t1, t2]} not supported for contains`);
  },
  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = tf.stack([s1.r.contents, s2.r.contents, 10]);
      return o.sum().sub(d);
    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },
};

export const center = (props: any): Tensor1D =>
  tf.stack([props.x.contents, props.y.contents]) as Tensor1D; // HACK: need to annotate the types of x and y to be Scalar
export const dist = (p1: Tensor1D, p2: Tensor1D) => p1.sub(p2).norm(); // NOTE: or tf.squaredDifference

// TODO: use it
const getConstraint = (name: string) => {
  if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
  // TODO: types for args
  return (...args: any[]) => toPenalty(constrDict[name]);
};

const toPenalty = (x: tf.Scalar): tf.Scalar => {
  return tf.pow(tf.maximum(x, tf.scalar(0)), tf.scalar(2));
};

export const evalEnergyOn = (state: State) => {
  const { objFns, constrFns, translation, varyingPaths } = state;
  const toNumber = tfStr;
  // TODO: types
  const applyFn = (f: FnDone<Scalar>, dict: any) => {
    if (dict[f.name]) {
      return dict[f.name](...f.args.map(argValue));
    } else {
      throw new Error(
        `constraint or objective ${f.name} not found in dirctionary`
      );
    }
  };

  return (...varyingValuesTF: Scalar[]): Scalar => {
    // HACK: convert the new varying values to normal js values first, probably need to let eval fn return shapes with tf vars?
    const varyingMap = zip(varyingPaths, varyingValuesTF) as VaryMap<Scalar>;
    const evalFns = (fns: Fn[]): FnDone<Scalar>[] =>
      fns.map((f) => evalFn(f, translation, varyingMap));
    const objEvaled = evalFns(objFns);
    const constrEvaled = evalFns(constrFns);
    const objEngs: Scalar[] = objEvaled.map((o) => applyFn(o, objDict));
    const constrEngs: Scalar[] = constrEvaled.map(
      (c) => toPenalty(applyFn(c, constrDict))
      // (sum, c) => applyFn(c, constrDict).add(sum),
    );

    // Printing to check for NaNs
    console.log(
      zip(
        constrEngs.map((e) => e.toString()),
        constrEvaled.map((c) => c.name)
      )
    );

    const objEng: Scalar =
      objEngs.length === 0 ? scalar(0) : stack(objEngs).sum();
    const constrEng: Scalar =
      constrEngs.length === 0 ? scalar(0) : stack(constrEngs).sum();
    // return constrEng;
    return objEng.add(
      constrEng
        .mul(tf.scalar(constraintWeight * state.paramsr.weight))
        .asScalar()
    );
  };
};

export const stepUntilConvergence = (state: State) => {
  const f = evalEnergyOn(state);
  const fgrad = gradF(f);
  const xs = state.varyingValues.map(tfVar);
  minimize(f, fgrad as any, xs, []);
  const trans = insertVaryings(
    state.translation,
    zip(state.varyingPaths, xs.map((x) => tfStr(x)) as number[]) as VaryMap
  );
  const newState = {
    ...state,
    translation: trans,
    varyingValues: xs.map(tfStr),
  };
  return evalTranslation(newState);
};

////////////////////////////////////////////////////////////////////////////////
// All TFjs related functions

// TODO: types
export const tfStr = (x: any) => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map((e) => tfStr(e));
export const tfVar = (e: number) => tf.scalar(e).variable();
// const learningRate = 0.5; // TODO Try different learning rates
const learningRate = 2; // TODO Try different learning rates
const optimizer2 = tf.train.adam(learningRate);

export const gradF = (fn: any) => tf.grads(fn);

/**
 * Use included tf.js optimizer to minimize f over xs (note: xs is mutable)
 *
 * @param {(...arg: tf.Scalar[]) => tf.Scalar} f overall energy function
 * @param {(...arg: tf.Scalar[]) => tf.Scalar[]} gradf gradient function
 * @param {tf.Scalar[]} xs varying state
 * @param {*} names // TODO: what is this
 * @returns // TODO: document
 */
export const minimize = (
  f: (...arg: tf.Scalar[]) => tf.Scalar,
  gradf: (arg: tf.Scalar[]) => tf.Scalar[],
  xs: tf.Scalar[],
  names: any
) => {
  // optimization hyperparameters
  const EPS = 1e-3;
  const MAX_STEPS = 1000;

  let energy; // to be returned
  let normGrad = Number.MAX_SAFE_INTEGER;
  let i = 0;

  // console.log("xs0", tfsStr(xs));
  // console.log("f'(xs0)", tfsStr(gradf(xs)));

  // TODO profile this
  while (normGrad > EPS && i < MAX_STEPS) {
    // TODO: use/revert spread, also doesn't work with varList=xs and compGraph1
    energy = optimizer2.minimize(() => f(...xs), true);
    const gradfx = gradf(xs);
    normGrad = tf
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
  console.log(
    "converged after",
    i,
    "steps with energy",
    tfStr(energy),
    "and grad norm",
    normGrad
  );
  console.log("state (varyingMap): ", tfsStr(xs));

  return { energy, normGrad, i };
};
