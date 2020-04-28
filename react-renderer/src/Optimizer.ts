import * as tf from "@tensorflow/tfjs";
import {
  Tensor1D,
  Tensor,
  stack,
  scalar,
  Variable,
  Scalar,
} from "@tensorflow/tfjs-node";
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

// HACK: constant constraint weight
const constraintWeight = 10e4;

export const objDict = {};
export const constrDict = {
  min1: (number: Tensor) => number.square(),
  minAll: (...numbers: Tensor[]) =>
    stack(numbers)
      .square()
      .sum(),
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
    const limit = scalar(20);
    switch (shapeType) {
      case "Circle":
        // return tf.stack([limit, props.r.contents.neg()]).sum();
        return props.r.contents.square();
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
  tf.stack([props.x.contents, props.y.contents]) as Tensor1D; // HACK: need to annotate the types of x and y to be Tensor
export const dist = (p1: Tensor1D, p2: Tensor1D) => p1.sub(p2).norm(); // NOTE: or tf.squaredDifference

// TODO: use it
const getConstraint = (name: string) => {
  if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
  // TODO: types for args
  return (...args: any[]) => toPenalty(constrDict[name]);
};

const toPenalty = (x: tf.Tensor): tf.Tensor => {
  return tf.pow(tf.maximum(x, tf.scalar(0)), tf.scalar(2));
};

export const evalEnergyOn = (state: State) => {
  const { objFns, constrFns, translation, varyingPaths } = state;
  // TODO: types
  const applyFn = (f: FnDone<Tensor>, dict: any) => {
    if (dict[f.name]) {
      // return dict[f.name](...f.args.map(argValue));
      return dict[f.name](argValue(f.args[0]));
    } else {
      throw new Error(
        `constraint or objective ${f.name} not found in dirctionary`
      );
    }
  };

  return (...varyingValuesTF: Variable[]): Tensor => {
    // construct a new varying map
    const varyingMap = genVaryMap(varyingPaths, varyingValuesTF) as VaryMap<
      Variable
    >;
    const objEvaled = evalFns(objFns, translation, varyingMap);
    const constrEvaled = evalFns(constrFns, translation, varyingMap);

    // const objEngs: Tensor[] = objEvaled.map((o) => applyFn(o, objDict));
    // const constrEngs: Tensor[] = constrEvaled.map(
    //   (c) => toPenalty(applyFn(c, constrDict))
    //   // (sum, c) => applyFn(c, constrDict).add(sum),
    // );

    // // Printing to check for NaNs
    // console.log(
    //   zip(
    //     constrEngs.map((e) => e.toString()),
    //     constrEvaled.map((c) => c.name)
    //   )
    // );

    // const objEng: Tensor =
    //   objEngs.length === 0 ? tfVar(0) : stack(objEngs).sum();
    // const constrEng: Tensor =
    //   constrEngs.length === 0 ? scalar(0) : stack(constrEngs).sum();

    return varyingValuesTF[0].square(); // TODO: FAIL
    // return stack(varyingValuesTF).sum(); // TODO: OKAY
    // return constrDict["minAll"](...varyingMap.values()); // TODO: OKAY
    // return constrDict["min1"](varyingValuesTF[3]);
    // return applyFn(constrEvaled[0], constrDict);

    // A single obj with a prop that's __one of__ the tfVars retrieved from a dict
    // const varyingMap2 = zip(
    //   varyingPaths.map((p) => JSON.stringify(p)),
    //   varyingValuesTF
    // );
    // const gpi = ["Circle", { r: varyingValuesTF[0] }];
    // const gpi = ["Circle", { r: varyingMap.get(varyingPaths[0]) }];
    // const gpi = ["Circle", { r: varyingMap2[JSON.stringify(varyingPaths[0])] }];
    // Another obj with a props that's the entire varying state
    // const gpi2 = ["Circle", { r: varyingValuesTF[0] }];

    // Just square the property
    // const func = ([_, props]: any) => props.r.square();
    // Square and add everybody
    // const func2 = ([_, props]: any) =>
    //   stack(props.r)
    //     .square()
    //     .sum();
    // return func(gpi2); // Does __not__ work
    // return func2(gpi2); // Works

    // testing a single constraint
    // const { name, args } = constrEvaled[0]; // minsize
    // console.log(args[0].contents);
    // return constrDict[name](args[0].contents);

    // return objEng.add(
    //   constrEng
    //     .mul(tf.scalar(constraintWeight * state.paramsr.weight))
    //     .asScalar()
    // );
  };
};

export const stepUntilConvergence = (state: State) => {
  const f = evalEnergyOn(state);
  const fgrad = gradF(f);
  const xs = state.varyingValues.map(differentiable);
  // TODO: mutates xs??
  minimize(f, fgrad as any, xs, []);
  const trans = insertVaryings(
    state.translation,
    genVaryMap(
      state.varyingPaths,
      xs.map((x) => tfStr(x)) as number[]
    ) as VaryMap
  );
  const newState = {
    ...state,
    translation: trans,
    varyingValues: xs.map(tfStr), // TODO: need this?
  };
  return evalTranslation(newState);
};

////////////////////////////////////////////////////////////////////////////////
// All TFjs related functions

// TODO: types
export const tfStr = (x: any) => x.dataSync()[0];
export const tfsStr = (xs: any[]) => xs.map((e) => tfStr(e));
export const differentiable = (e: number) => tf.scalar(e).variable();
// const learningRate = 0.5; // TODO Try different learning rates
const learningRate = 2; // TODO Try different learning rates
const optimizer2 = tf.train.adam(learningRate);

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
  gradf: (arg: tf.Tensor[]) => tf.Tensor[],
  xs: tf.Tensor[],
  names: any
) => {
  // optimization hyperparameters
  const EPS = 1e-3;
  const MAX_STEPS = 100; // TODO: revert

  let energy; // to be returned
  let normGrad = Number.MAX_SAFE_INTEGER;
  let i = 0;

  // console.log("xs0", tfsStr(xs));
  // console.log("f'(xs0)", tfsStr(gradf(xs)));

  // TODO profile this
  while (normGrad > EPS && i < MAX_STEPS) {
    // TODO: use/revert spread, also doesn't work with varList=xs and compGraph1
    energy = optimizer2.minimize(() => f(...xs) as any, true);
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
