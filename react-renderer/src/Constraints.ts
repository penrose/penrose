import { Tensor, stack, scalar, maximum, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

export const objDict = {
  equal: (x: DiffVar, y: DiffVar) => squaredDifference(x, y),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    distsq(center(s1), center(s2)),

  // Generic repel function for two GPIs with centers
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;
    // 1 / (d^2(cx, cy) + eps)
    return distsq(center(s1), center(s2)).add(epsd).reciprocal().mul(repelWeight);
  },

  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): DiffVar => {
    const spacing = scalar(1.1); // arbitrary

    if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, center(text1), center(text2),
        [spacing.mul(text1.h.contents),
        text2.h.contents.mul(spacing).mul(scalar(1.0)).neg()]);
    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

};

export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(Math.max(...canvasSize) / 6);
    switch (shapeType) {
      case "Circle":
        return stack([props.r.contents, limit.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },

  minSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(20);
    switch (shapeType) {
      case "Circle":
        return stack([limit, props.r.contents.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },

  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: DiffVar
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      // const o = s1.r.contents.sub(s2.r.contents);
      const o = offset
        ? s1.r.contents.sub(s2.r.contents).sub(offset)
        : s1.r.contents.sub(s2.r.contents);
      return d.sub(o);
    } else if (t1 === "Circle" && t2 === "Text") {
      const d = dist(center(s1), center(s2));
      const textR = maximum(s2.w.contents, s2.h.contents);
      return d.sub(s1.r.contents).add(textR);
    } else throw new Error(`${[t1, t2]} not supported for contains`);
  },

  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = stack([s1.r.contents, s2.r.contents, 10]);
      return o.sum().sub(d);
    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },

  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // s1 is smaller than s2
    const offset = scalar(0.4).mul(s2.r.contents); // take 0.4 as param
    return s1.r.contents.sub(s2.r.contents).sub(offset);
  },

  outsideOf: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Text" && t2 === "Circle") {
      const textR = maximum(s1.w.contents, s1.h.contents);
      const d = dist(center(s1), center(s2));
      return s2.r.contents
        .add(textR)
        .add(scalar(padding))
        .sub(d);
    } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  },

  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      return looseIntersect(center(s1), s1.r.contents,
        center(s2), s2.r.contents, padding);
    } else throw new Error(`${[t1, t2]} not supported for overlapping`);
  },

};

// -------- Helpers for writing objectives

const typesAre = (inputs: string[], expected: string[]) =>
  (inputs.length === expected.length) && _.zip(inputs, expected).map(([i, e]) => i === e);

// -------- (Hidden) helpers for objective/constraints/computations

const centerArrow2 = (arr: any, center1: DiffVar, center2: DiffVar, [o1, o2]: DiffVar[]): DiffVar => {
  const vec = center2.sub(center1); // direction the arrow should point to
  const dir = normalize(vec);

  let start = center1;
  let end = center2;

  // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality
  if (norm(vec).greater(o1.add(abs(o2)))) {
    start = center1.add(o1.mul(dir));
    end = center2.add(o2.mul(dir));
  }

  const fromPt = stack([arr.startX.contents, arr.startY.contents]);
  const toPt = stack([arr.endX.contents, arr.endY.contents]);

  return distsq(fromPt, start).add(distsq(toPt, end));
}


// -------- Utils for objective/constraints/computations

const sc = (x: any): number => x.dataSync()[0];
const scs = (xs: any[]) => xs.map((e) => sc(e));

export const zero: DiffVar = scalar(0);

// to prevent 1/0 (infinity). put it in the denominator
export const epsd: DiffVar = scalar(10e-10);

export const looseIntersect = (center1: DiffVar, r1: DiffVar, center2: DiffVar, r2: DiffVar, padding: number) =>
  dist(center1, center2).sub(r1.add(r2).sub(scalar(padding)));
// dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

export const center = (props: any): VecAD | Tensor => {
  const [x, y] = [props.x.contents, props.y.contents];

  if (props.x.contents.tag) {
    return { tag: "VecAD", contents: [x, y] } as VecAD;
  }

  return stack([props.x.contents, props.y.contents]);
};

export const dist = (p1: DiffVar, p2: DiffVar): DiffVar => p1.sub(p2).norm();

// Be careful not to use element-wise operations. This should return a scalar.
// Apparently typescript can't check a return type of `DiffVar<Rank.R0>`?
export const distsq = (p1: Tensor | VecAD, p2: Tensor | VecAD): DiffVar => {

  if ("tag" in p1 && "tag" in p2) { // both are VecADs
    const [v1, v2] = [p1.contents, p2.contents];
    const dv = ops.vsub(v1, v2);
    const res = ops.vnormsq(dv);
    return res;
  } else if (!("tag" in p1) && !("tag" in p2)) { // Need this check, otherwise Typescript can't figure out they are both tensors
    console.error("both tensors");
    const dp = p1.sub(p2);
    return dp.dot(dp);
  }

  throw Error("p1 and p2 not the same type");
};

// with epsilon to avoid NaNs
export const normalize = (v: DiffVar): DiffVar => v.div(v.norm().add(epsd));

// TODO: use it
// const getConstraint = (name: string) => {
//   if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
//   // TODO: types for args
//   return (...args: any[]) => toPenalty(constrDict[name]);
// };

// -----------------

// Reverse-mode AD
// Implementation adapted from https://rufflewind.com/2016-12-30/reverse-mode-automatic-differentiation and https://github.com/Rufflewind/revad/blob/eb3978b3ccdfa8189f3ff59d1ecee71f51c33fd7/revad.py

// TODO: Are there specific things that need to be done for consts, not vars?
// NOTE: VARIABLES ARE MUTATED DURING AD CALCULATION

// ----- Core AD code

export const variableAD = (x: number, vname = ""): VarAD => {
  const nameVal = vname ? vname : String(x);

  return {
    tag: "custom",
    name: nameVal,
    val: x,
    parents: [],
    gradVal: { tag: "Nothing" }
  };
};

// TODO: Do we need to "flush" the cached vals and reseed after computing the grad once? 

// This should only be applied to a leaf node (TODO: fix API) 
// It moves forward from the leaf in question, then recursively arrives at the seed node (one output parent), caching each gradient value for an intermediate note
// (However, it doesn't calculate gradient for any other leaf nodes, though they do use the cached intermediate values)

//            (ds/ds = 1)
//                s
//               ^ ^
//              /   \
//               ...
//        z1 = ...  z2 = ...
//              ^   ^
//  dz1/dv = ... \ / dz2/dv = ...
//               (v) = ...
//          
//  ds/dv = ds/sz1 * dz1/dv + ds/dz2 * dz2/dv + ...
// The recursive parts are ds/dzi (the parents further up)

// grad(v) means ds/dv (s is the single output seed)

export const gradAD = (v: VarAD): number => {
  // Already computed/cached the gradient
  if (v.gradVal.tag === "Just") {
    return v.gradVal.contents;
  }

  // TODO: What's the most efficient way to do this recursion?
  // parent.sensitivity = dzi/dv (in expression above)
  // grad(parent.node) = ds/dzi
  const res = _.sum(v.parents.map(parent => parent.sensitivity * gradAD(parent.node)));

  // Note we both set the gradVal and return it
  v.gradVal = { tag: "Just", contents: res };

  return res;
};

// ----- Ops (extensible)
// NOTE: These all update the graph and return new variables that should be used to build the ops

// NOTE: For the resulting var `z`, z's parents and grad are uninitialized

// TODO: Factor out op helper to deal with graph-building boilerplate

// --- Binary ops

//                (+) (z := v + w)     -- parent
//               ^   ^
// dz/dv = 1    /     \    dz/dw = 1   -- sensitivities
//             v       w               -- children

// TODO: Put these in ops dict

export const add = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val + w.val, "+");
  v.parents.push({ node: z, sensitivity: 1.0 });
  w.parents.push({ node: z, sensitivity: 1.0 });
  return z;
};

export const mul = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val * w.val, "*");
  v.parents.push({ node: z, sensitivity: w.val });
  w.parents.push({ node: z, sensitivity: v.val });
  return z;
};

const sub = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val - w.val, "-");
  v.parents.push({ node: z, sensitivity: 1.0 });
  w.parents.push({ node: z, sensitivity: -1.0 });
  return z;
};

// --- Unary ops

const sin = (v: VarAD): VarAD => {
  const z = variableAD(Math.sin(v.val), "sin");
  v.parents.push({ node: z, sensitivity: Math.cos(v.val) });
  return z;
};

const neg = (v: VarAD): VarAD => {
  const z = variableAD(-v.val, "- (unary)");
  v.parents.push({ node: z, sensitivity: -1.0 });
  return z;
};

// TODO: rename to `square` after tf.js dependency is removed
const squared = (v: VarAD): VarAD => {
  const z = variableAD(v.val * v.val, "^2");
  v.parents.push({ node: z, sensitivity: 2.0 * v.val });
  return z;
};

// ----- Helper functions

const fromJust = (n: MaybeVal<number>): number => {
  if (n.tag === "Just") {
    return n.contents;
  }

  console.error("expected value in fromJust but got Nothing");
  return 0;
}

const assert = (b: boolean, s: any[]) => {
  const res = b ? "passed" : "failed";
  console.assert(b);
  console.log("Assertion", res, ...s);
}

const close = (x: number, y: number) => {
  const EPS = 1e-15;
  console.log("x, y", x, y); // TODO make the assert better
  return Math.abs(x - y) < EPS;
};

// ----- Tests

const testAD1 = () => {
  console.log("Testing z := x + y");
  const x = variableAD(0.5);
  const y = variableAD(4.2);
  const z = add(x, y);

  z.gradVal = { tag: "Just", contents: 1.0 }; // seed: dz/d(x_i) (there's only one output)
  const dx = gradAD(x);
  const dy = gradAD(y);
  console.log("z, x, y", z, x, y);
  console.log("dx, dy", dx, dy);

  const vals = [z, x, y];
  assert(close(z.val, x.val + y.val), ["z = x + y", vals]);
  assert(close(fromJust(x.gradVal), 1.0), ["dz/dx = 1", vals]);
  assert(close(fromJust(y.gradVal), 1.0), ["dz/dy = 1", vals]);
};

// From https://github.com/Rufflewind/revad/blob/eb3978b3ccdfa8189f3ff59d1ecee71f51c33fd7/revad.py
const testAD2 = () => {
  console.log("Testing z := (x * y) + sin(x)");
  const x = variableAD(0.5);
  const y = variableAD(4.2);
  const z = add(mul(x, y), sin(x)); // x * y + sin(x)

  z.gradVal = { tag: "Just", contents: 1.0 };
  const dx = gradAD(x);
  const dy = gradAD(y);
  console.log("z, x, y", z, x, y);
  console.log("dx, dy", dx, dy);

  const vals = [z, x, y, dx, dy];
  assert(close(z.val, (x.val * y.val) + Math.sin(x.val)), ["z := (x * y) + sin(x)", vals]);
  assert(close(fromJust(x.gradVal), (y.val + Math.cos(x.val))), ["dz/dx", vals]);
  assert(close(fromJust(y.gradVal), x.val), ["dz/dy", vals]);
};

// TODO: Test for numerical instability

const testAD3 = () => {
  console.log("Testing z := (x - y)^2"); // x^2 - 2xy + y^2
  const x = variableAD(0.5);
  const y = variableAD(4.2);
  const z = squared(sub(x, y));

  z.gradVal = { tag: "Just", contents: 1.0 };
  const dx = gradAD(x);
  const dy = gradAD(y);
  console.log("z, x, y", z, x, y);
  console.log("dx, dy", dx, dy);

  const vals = [z, x, y, dx, dy];
  assert(close(z.val, Math.pow(x.val - y.val, 2.0)), ["z := (x - y)^2", vals]);
  assert(close(fromJust(x.gradVal), (2.0 * x.val - 2.0 * y.val)), ["dz/dx", vals]);
  assert(close(fromJust(y.gradVal), (2.0 * y.val - 2.0 * x.val)), ["dz/dy", vals]);
};

// Helpers for programmatic testing

// TODO: Probably the right thing to do is make the objectives/constraints/computations agnostic to the choice of number, autodiff library, and representation -- i.e. make it swappable with a flag -- esp since most energy evaluations don't need a gradient (so don't use vars for that)

// TODO: Another next step is to make the evaluator work with these vars instead, so the system can use the existing infra for programmatically composing an energy fn (which doesn't seem to matter for speed, since it's just the bare ops)

// TODO: Use type like in evalExprs: varyingVars?: VaryMap<number | Tensor>`

// Functions for new kinds of vars (TODO: get rid of tensors everywhere)

// TODO: Write a variation on evalEnergyOn that works on State and IVarAD, then write a variation on stepEP
// --> requires variation on evalFns, evalFn, evalExprs, evalExpr, compDict?, 
// --> requires variation on evalFn
// Remove Tensor and Scalar types from types.d.ts

// NOTE: Only when you apply a special operation, it mutates the variable(s) (IVarAD reference(s)) to add a reference to its parent (the result of the op) to both. That means the rest of the code doesn't matter, only the ops do (except for keeping the objfn's form constant between iterations). 
// You just need to hold onto the references to your vars

// TODO: You will need to zero grads on all the bottom nodes (varying vars) because they will still have the parent refs and grad vals attached (Unless that's done automatically by the grad function)

// NOTE: `evalFn` calls `evalExpr` with `autodiff = true`. It makes everything (base vals) differentiable when encountered

// TODO: How to modify this grad code to deal with non-variable constants?? I guess it depends on how the code handles constants (are there special ops for "c * x" or do you convert "c" into a variable and not take the gradient with respect to it?)

const objDict2 = {};

const constrDict2 = {};

// Note that these ops MUST use the custom var ops for grads
export const ops = {
  vsub: (v1: VarAD[], v2: VarAD[]): VarAD[] => {
    const res = _.zipWith(v1, v2, sub);
    return res;
  },

  vnormsq: (v: VarAD[]): VarAD => {
    const res = v.map(e => squared(e));
    return _.reduce(res, add, variableAD(0.0)); // TODO: Will this one (var(0)) have its memory freed?        
    // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
  },

  // Note: if you want to compute a normsq, use that instead, it generates a smaller computational graph
  vdot: (v1: VarAD[], v2: VarAD[]): VarAD => {
    const res = _.zipWith(v1, v2, mul);
    return _.reduce(res, add, variableAD(0.0));
  },

  vsum: (v: VarAD[]): VarAD => {
    return _.reduce(v, add, variableAD(0.0));
  }

};

// Returns true if x is a VarAD, false if x is a Tensor
export const isCustom = (x: DiffVar): boolean => {
  return x.tag;
};

export const energyAndGradAD = (f: (...arg: DiffVar[]) => DiffVar, xs: number[], xsVarsInit: DiffVar[]) => {
  // NOTE: mutates xsVars
  // console.log("energy and grad with vars", xs);

  // Questions/assumptions: TODO 
  // Who calls the energy?
  // 1) Who sets the seed??
  // 2) What if someone has already called energy? (vars will already exist) -- I guess just re-evaluates and makes a new comp graph. Does the memory get cleared right?
  // 3) what if someone has already called the grad? (vals will be cached)
  // Who zeroes the variables? 
  // Who zeroes the gradients?

  // TODO: Why is the grad twice the right value when using xsVarsInit? Each var seems to have 3 parents when it should just have one--maybe due to extra calls

  // For now, just make the vars from scratch
  const xsCopy = [...xs];
  const xsVars = xsCopy.map(x => variableAD(x));

  const z = f(...xsVars);
  z.gradVal = { tag: "Just", contents: 1.0 }; // just in case, but it's also auto-set in `f`
  const energyZ = z.val;

  // This will take the grad of all of them, mutating xsVars to store grad values (OR lookup if already cached -- TODO note this!)
  const dxs = xsVars.map(gradAD);
  // console.log("xsVars with grads", xsVars);
  const gradxs = xsVars.map((x: DiffVar) => fromJust(x.gradVal));

  return { energyVal: energyZ, gradVal: gradxs };
};

export const energyAndGradADHardcoded = (state: number[]) => {
  // TODO: You probably want to hold onto the vars at the top level

  const stateCopy = [...state];
  const xs = stateCopy.map(x => variableAD(x));

  // This `z` expression will do two things:
  // 1) Evaluate the expression (forward), resulting in the value of z
  // 2) Dynamically build the computational graph: mutate the `xs` and automatically create anonymous intermediate nodes (parents) that the leaf nodes `xs` hold references to

  // TODO: Use an n-ary add to save intermediate nodes!

  const z =
    add(
      add(
        squared(sub(xs[2], xs[0])),
        squared(sub(xs[3], xs[1]))
      ),
      add(
        squared(sub(xs[6], xs[4])),
        squared(sub(xs[7], xs[5]))
      )
    );

  z.gradVal = { tag: "Just", contents: 1.0 };
  // This will evaluate the energy
  const energyZ = z.val;
  // console.log("energyZ", energyZ);

  // This will take the grad of all of them, TODO note cache / mutability
  const dxs = xs.map(gradAD);
  const gradxs = xs.map(x => fromJust(x.gradVal));

  // console.log("xs", xs);
  // console.log("gradxs", gradxs);

  // TODO: check correctness against analytic energy/gradient

  // TODO: Need to free the memory of this comp graph on later runs?
  return { energyVal: energyZ, gradVal: gradxs };
};

// Main

export const testReverseAD = () => {
  console.log("testing reverse AD");

  // TODO: Generalize these tests to be parametrized/fuzzed etc
  // TODO: Have nodes store their names/ops?
  testAD1();
  testAD2();
  testAD3();
};

// ------ Hardcoded energy and its grad (for testing only)

// FNS: ["sameCenter(A.text, A.shape)", "sameCenter(B.text, B.shape)"] => [f(s1, s2), f(s3, s4)]
// VARS: [
// "A.shape.x" (0), "A.shape.y" (1), 
// "A.text.x" (2), "A.text.y" (3), 
// "B.shape.x" (4), "B.shape.y" (5), 
// "B.text.x" (6), "B.text.y" (7)]

// GRAD:
// (In general, df(s1, s2)/d(s1x) = d((a-b)^2)/da = 2(a-b) = basically a-b
// [ A.shape.x - A.text.x, A.shape.y - A.text.y,
//   A.text.x - A.shape.x, A.text.y - A.shape.y, 
//   B.shape.x - B.text.x, B.shape.y - B.text.y,
//   B.text.x - B.shape.x, B.text.y - B.shape.y ]
// [ xs[0] - xs[2], xs[1] - xs[3],
//   xs[2] - xs[0], xs[3] - xs[1],
//   xs[4] - xs[6], xs[5] - xs[7],
//   xs[6] - xs[4], xs[7] - xs[5] ] 
// Pretty sure you could implement this as a matrix

// 1) Inlined energy
// distsq(center(A.text), center(A.shape)) + distsq(center(B.text), center(B.shape))
// distsq([zs[2], zs[3]], [zs[0], zs[1]]) + distsq([zs[6], zs[7]], [zs[4], zs[5]])
// (zs[2] - zs[0])^2 + (zs[3] - zs[1])^2 + (zs[6] - zs[4])^2 + (zs[7] - zs[5])^2
export const energyHardcoded = (zs: number[]) => {
  const res1 = zs[2] - zs[0];
  const res2 = zs[3] - zs[1];
  const res3 = zs[6] - zs[4];
  const res4 = zs[7] - zs[5];
  return res1 * res1 + res2 * res2 + res3 * res3 + res4 * res4;
};

// 2) Inlined gradient
export const gradfHardcoded = (zs: number[]) =>
  [zs[0] - zs[2], zs[1] - zs[3],
  zs[2] - zs[0], zs[3] - zs[1],
  zs[4] - zs[6], zs[5] - zs[7],
  zs[6] - zs[4], zs[7] - zs[5]].map(x => x * 2.0);

export const normList = (xs: number[]) =>
  Math.sqrt(_.sum(xs.map(e => e * e)));
