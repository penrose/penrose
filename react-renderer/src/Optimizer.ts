import * as tf from "@tensorflow/tfjs";
import { Tensor, stack, scalar, Variable, Scalar } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import {
  argValue,
  evalTranslation,
  insertVaryings,
  genVaryMap,
  evalFns,
} from "./Evaluator";
import { zip } from "lodash";
import { insertPending } from "./PropagateUpdate";
import { ENGINE_METHOD_PKEY_ASN1_METHS } from "constants";

// HACK: constant constraint weight
const constraintWeight = 10e4;

export const objDict = {};
export const constrDict = {
  min1: (n: Tensor) => n.square(),
  minAll: (...numbers: Tensor[]) =>
    stack(numbers)
      .square()
      .sum(),
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(Math.max(...canvasSize) / 6);
    switch (shapeType) {
      case "Circle":
        return tf.stack([props.r.contents, limit.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },
  minSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(20);
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
    offset: Tensor
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      // const o = s1.r.contents.sub(s2.r.contents);
      const o = offset
        ? s1.r.contents.sub(s2.r.contents).sub(offset)
        : s1.r.contents.sub(s2.r.contents);
      return d.sub(o);
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

export const center = (props: any): Tensor =>
  tf.stack([props.x.contents, props.y.contents]); // HACK: need to annotate the types of x and y to be Tensor
export const dist = (p1: Tensor, p2: Tensor) => p1.sub(p2).norm(); // NOTE: or tf.squaredDifference

// TODO: use it
const getConstraint = (name: string) => {
  if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
  // TODO: types for args
  return (...args: any[]) => toPenalty(constrDict[name]);
};

const toPenalty = (x: Tensor): Tensor => {
  return tf.pow(tf.maximum(x, tf.scalar(0)), tf.scalar(2));
};

export const evalEnergyOn = (state: State) => {
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
    return overallEng.sub(dummyVal).add(dummyVal);

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
  const xs = state.varyingValues.map(differentiable);
  // TODO: mutates xs??
  const { energy } = minimize(f, fgrad as any, xs, steps);
  const trans = insertVaryings(
    state.translation,
    zip(
      state.varyingPaths,
      xs.map((x) => tfStr(x))
    ) as [Path, number][]
  );
  const newState = {
    ...state,
    translation: trans,
    varyingValues: xs.map(tfStr), // TODO: need this?
  };
  if (tfStr(energy) > 10) {
    newState.paramsr.optStatus.tag = "UnconstrainedRunning";
    console.log("Stepping... current energy", tfStr(energy));
  } else {
    newState.paramsr.optStatus.tag = "EPConverged";
    console.log("converged with energy", tfStr(energy));
  }
  return evalTranslation(newState);
};

////////////////////////////////////////////////////////////////////////////////
// All TFjs related functions

// TODO: types
export const tfStr = (x: any) => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map((e) => tfStr(e));
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
) => {
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

  return { energy, normGrad, i };
};
