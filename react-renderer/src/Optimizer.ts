import * as tf from "@tensorflow/tfjs";
import {
  Tensor,
  stack,
  scalar,
  Variable,
  Scalar,
  // maximum,
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

////////////////////////////////////////////////////////////////////////////////
// Globals

// growth factor for constraint weights
const weightGrowthFactor = 10;
// weight for constraints
const constraintWeight = 10e4; // HACK: constant constraint weight
// Intial weight for constraints
const initConstraintWeight = 10e-3;
// learning rate for the tfjs optimizer
const learningRate = 50;
const optimizer = tf.train.adam(learningRate, 0.9, 0.999);
// EP method convergence criteria
const epStop = 1e-3;

////////////////////////////////////////////////////////////////////////////////

const toPenalty = (x: Tensor): Tensor => {
  return tf.pow(tf.maximum(x, tf.scalar(0)), tf.scalar(2));
};

// TODO. change to `(norm (x -. x') <= epStop) || (abs (fx - fx') <= epStop)`
// where epStop is 10**-3
// and move this to unconstrainedStopCond
const epConverged = (normGrad: Scalar): boolean => {
  // energy heuristic
  // return scalarValue(energy) > 10;
  // via @hypotext
  return scalarValue(normGrad) < epStop;
  // return normGrad.less(scalar(epStop)).dataSync()[0];
  // .then((data) => data);
};

const applyFn = (f: FnDone<Tensor>, dict: any) => {
  if (dict[f.name]) {
    return dict[f.name](...f.args.map(argValue));
  } else {
    throw new Error(
      `constraint or objective ${f.name} not found in dirctionary`
    );
  }
};

/**
 * Given a `State`, take n steps by evaluating the overall objective function
 *
 * @param {State} state
 * @param {number} steps
 * @returns
 */
// TODO. Annotate the return type: a new (copied?) state with the varyingState and opt params set?

export const stepEP = (state: State, steps: number, evaluate = true) => {
  // TODO. Maybe factor this back out into `State -> (VaryingState, OptParams)`?
  const { optStatus, weight } = state.params;
  let newState = { ...state };
  let xs: Variable[] = []; // guaranteed to be assigned
  switch (optStatus.tag) {
    case "NewIter": {
      // Collect the overall objective and varying values
      const overallObjective = evalEnergyOn(state); // TODO. Why is this being generated here?
      const newParams: Params = {
        ...state.params, // TODO. does this make a copy?
        weight: initConstraintWeight,
        optStatus: {
          tag: "UnconstrainedRunning",
          contents: state.varyingValues.map(differentiable),
        }, // TODO. set `bfgsInfo: defaultBfgsParams`
      };
      return { ...state, params: newParams, overallObjective };
    }
    case "UnconstrainedRunning": {
      // NOTE: use cached varying values
      // TODO. we should be using `varyingValues` below in place of `xs`, not the `xs` from optStatus
      // (basically use the last UO state, not last EP state)
      xs = optStatus.contents;
      const f = state.overallObjective;
      const fgrad = gradF(f);
      // NOTE: minimize will mutate xs
      const { energy, normGrad } = minimize(f, fgrad, xs, steps);
      // TODO: we could mutate the state, but is this what we want?

      // TODO. you have to save the previous state (and previous energy) to do the EP convergence check. What is the right way to do this? Copy tfvars into scalars?
      // TODO. test unconstrained convergence instead, and remove the (!) and swap the two cases
      // (TODO. `varyingValues` is updated `state` after each step by putting it into `newState` and passing it to `evalTranslation`, which returns another state)

      // TODO. these below are both doing the equivalent of returning `vstate'`
      if (!epConverged(normGrad)) { // UO didn't converge; keep running UO
        newState.params.optStatus = {
          tag: "UnconstrainedRunning",
          contents: xs,
        };
        console.log(`Took ${steps} steps. Current energy`, scalarValue(energy));
      } else {
        newState.params.optStatus = {
          tag: "UnconstrainedConverged",
          contents: xs,
        }; // TODO. reset bfgs params to default
        console.log("Unconstrained converged with energy", scalarValue(energy));
      }
      break;
    }
    case "UnconstrainedConverged": {
      xs = optStatus.contents;
      const f = state.overallObjective;
      const fgrad = gradF(f);
      // NOTE: minimize will mutate xs
      const { energy, normGrad } = minimize(f, fgrad, xs, steps);

      // TODO. Do EP convergence check on the last EP state (and its energy), and last UO state (xs) (and its energy) 
      // TODO. Make a diagram to clarify vocabulary
      if (epConverged(normGrad)) {
        newState.params.optStatus.tag = "EPConverged";
        console.log("EP converged with energy", scalarValue(energy));
      } else {
        // if EP has not converged, increase weight and continue
        // TODO. the point is that, for the next round, the last converged UO state becomes both the last EP state and the initial state for the next round -- starting with a harsher penalty. but these should be separate
        newState.params = {
          ...newState.params,
          optStatus: { tag: "UnconstrainedRunning", contents: xs }, // TODO: use which state again?
          weight: weightGrowthFactor * weight,
        };
      }
      break;
    }
    case "EPConverged": // do nothing if converged
      return state;
  }
  // return the state with a new set of shapes
  if (evaluate) {
    const varyingValues = xs.map((x) => scalarValue(x as Scalar));
    newState.translation = insertVaryings(
      state.translation,
      zip(state.varyingPaths, varyingValues) as [Path, number][]
    );
    newState.varyingValues = varyingValues;
    newState = evalTranslation(newState);
  }
  return newState;
};

/**
 * Generate an energy function from the current state
 *
 * @param {State} state
 * @returns a function that takes in a list of `Variable`s and return a `Scalar`
 */
export const evalEnergyOn = (state: State) => {
  // NOTE: this will greatly improve the performance of the optmizer
  // TODO: move this decl to somewhere else
  tf.setBackend("cpu");
  const { objFns, constrFns, translation, varyingPaths } = state;
  // TODO: types
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
    const objEng: Tensor =
      objEngs.length === 0 ? differentiable(0) : stack(objEngs).sum();
    const constrEng: Tensor =
      constrEngs.length === 0 ? differentiable(0) : stack(constrEngs).sum();
    const overallEng = objEng.add(
      constrEng.mul(scalar(constraintWeight * state.params.weight))
    );

    // NOTE: the current version of tfjs requires all input variables to have gradients (i.e. actually involved when computing the overall energy). See https://github.com/tensorflow/tfjs-core/blob/8c2d9e05643988fa7f4575c30a5ad3e732d189b2/tfjs-core/src/engine.ts#L726
    // HACK: therefore, we try to work around it by using all varying values without affecting the value and gradients of the energy function
    const dummyVal = stack(varyingValuesTF).sum();
    return overallEng.add(dummyVal.mul(scalar(0)));
  };
};

export const step = (state: State, steps: number) => {
  const f = evalEnergyOn(state);
  const fgrad = gradF(f);
  const xs = state.varyingValues.map(differentiable);
  // const xs = state.varyingState; // NOTE: use cached varying values
  // NOTE: minimize will mutates xs
  const { energy } = minimize(f, fgrad, xs, steps);
  // insert the resulting variables back into the translation for rendering
  // NOTE: this is a synchronous operation on all varying values; may block
  const varyingValues = xs.map((x) => scalarValue(x as Scalar));
  const trans = insertVaryings(
    state.translation,
    zip(state.varyingPaths, varyingValues) as [Path, number][]
  );
  const newState = { ...state, translation: trans, varyingValues };
  if (scalarValue(energy) > 10) {
    // const newState = { ...state, varyingState: xs };
    newState.params.optStatus.tag = "UnconstrainedRunning";
    console.log(`Took ${steps} steps. Current energy`, scalarValue(energy));
    // return newState;
  } else {
    // const varyingValues = xs.map((x) => tfStr(x));
    // const trans = insertVaryings(
    //   state.translation,
    //   zip(state.varyingPaths, varyingValues) as [Path, number][]
    // );
    // const newState = { ...state, translation: trans, varyingValues };
    newState.params.optStatus.tag = "EPConverged";
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
  normGrad: Scalar;
  i: number;
} => {
  // values to be returned
  let energy;
  let i = 0;
  while (i < maxSteps) {
    energy = optimizer.minimize(() => f(...xs) as any, true);

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
  // find the current
  const gradfx = gradf(xs);
  const normGrad = tf.stack(gradfx).norm();
  return { energy: energy as Scalar, normGrad: normGrad as Scalar, i };
};
