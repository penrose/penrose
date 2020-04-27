import * as tf from "@tensorflow/tfjs";
import { Tensor1D } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import { argValue, evalFn, evalTranslation, insertVaryings } from "./Evaluator";
import { zip } from "lodash";
import { insertPending } from "./PropagateUpdate";

// HACK: constant constraint weight
const constraintWeight = 10e4;

export const objDict = {};
export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = Math.max(...canvasSize);
    switch (shapeType) {
      case "Circle":
        return tf.tensor1d([props.r.contents - limit / 6]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },
  minSize: ([shapeType, props]: [string, any]) => {
    const limit = 20;
    switch (shapeType) {
      case "Circle":
        return tf.tensor1d([limit, -props.r.contents]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: number
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = offset
        ? tf.tensor1d([s1.r.contents, -s2.r.contents, -offset])
        : tf.tensor1d([s1.r.contents, -s2.r.contents]);
      return d.sub(o.sum());
    } else throw new Error(`${[t1, t2]} not supported for contains`);
  },
  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = tf.tensor1d([s1.r.contents, s2.r.contents, 10]);
      return o.sum().sub(d);
    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },
};

export const center = (props: any): Tensor1D =>
  tf.tensor1d([props.x.contents, props.y.contents]);
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

export const evalEnergyOn = (state: State) => (
  ...varyingValuesTF: tf.Scalar[]
): tf.Scalar => {
  const { objFns, constrFns, translation, varyingPaths } = state;
  // HACK: convert the new varying values to normal js values first, probably need to let eval fn return shapes with tf vars?
  const toNumber = tfStr;
  const varyingValues = varyingValuesTF.map(toNumber);
  const varyingMap = zip(varyingPaths, varyingValues);
  const evalFns = (fns: Fn[]): FnDone[] =>
    fns.map((f) => evalFn(f, translation, varyingMap as VaryMap));
  const objEvaled = evalFns(objFns);
  const constrEvaled = evalFns(constrFns);
  // TODO: types
  const applyFn = (f: FnDone, dict: any) => {
    if (dict[f.name]) {
      return dict[f.name](...f.args.map(argValue));
    } else {
      throw new Error(
        `constraint or objective ${f.name} not found in dirctionary`
      );
    }
  };
  const objEng: tf.Scalar = objEvaled.reduce(
    (sum, o) => applyFn(o, objDict).add(sum),
    tf.scalar(0)
  );
  const constrEng: tf.Scalar = constrEvaled.reduce(
    (sum, c) => toPenalty(applyFn(c, constrDict)).add(sum),
    // (sum, c) => applyFn(c, constrDict).add(sum),
    tf.scalar(0)
  );
  return objEng.add(
    constrEng.mul(tf.scalar(constraintWeight * state.paramsr.weight))
  );
};

export const stepUntilConvergence = (state: State) => {
  const f = evalEnergyOn(state);
  const fgrad = gradF(f);
  const xs = state.varyingValues.map(tfVar);
  minimize(f, fgrad as any, xs, []);
  const trans = insertVaryings(
    state.translation,
    xs.map((x) => tfStr(x))
  );
  const newState = {
    ...state,
    translation: trans,
  };
  return evalTranslation(newState);
};

////////////////////////////////////////////////////////////////////////////////
// All TFjs related functions

// TODO: types
export const tfStr = (x: any) => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map((e) => tfStr(e));
export const tfVar = (e: number) => tf.scalar(e).variable();
const learningRate = 0.5; // TODO Try different learning rates
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

  return { energy, normGrad, i };
};
