import { Tensor, Scalar, Rank, stack, scalar, maximum, tensor, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

export const objDict = {
  equal: (x: Tensor, y: Tensor) => squaredDifference(x, y),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return distsq(center(s1), center(s2));
  },

  // Generic repel function for two GPIs with centers
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;
    // 1 / (d^2(cx, cy) + eps)
    return distsq(center(s1), center(s2)).add(epsd).reciprocal().mul(repelWeight);
  },

  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): Tensor => {
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
    offset: Tensor
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

const centerArrow2 = (arr: any, center1: Tensor, center2: Tensor, [o1, o2]: Tensor[]): Tensor => {
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

export const zero: Tensor = scalar(0);

// to prevent 1/0 (infinity). put it in the denominator
export const epsd: Tensor = scalar(10e-10);

export const looseIntersect = (center1: Tensor, r1: Tensor, center2: Tensor, r2: Tensor, padding: number) =>
  dist(center1, center2).sub(r1.add(r2).sub(scalar(padding)));
// dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

export const center = (props: any): Tensor =>
  stack([props.x.contents, props.y.contents]); // HACK: need to annotate the types of x and y to be Tensor

export const dist = (p1: Tensor, p2: Tensor): Tensor => p1.sub(p2).norm();

// Be careful not to use element-wise operations. This should return a scalar.
// Apparently typescript can't check a return type of `Tensor<Rank.R0>`?
export const distsq = (p1: Tensor, p2: Tensor): Tensor => {
  const dp = p1.sub(p2);
  // console.log("p1, p2", p1, p2, p1.arraySync(), p2.arraySync(), dp.arraySync());
  return dp.dot(dp);
};

// with epsilon to avoid NaNs
export const normalize = (v: Tensor): Tensor => v.div(v.norm().add(epsd));

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

const variable = (x: number): VarAD => {
  return {
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

const grad = (v: VarAD): number => {
  // Already computed/cached the gradient
  if (v.gradVal.tag === "Just") {
    return v.gradVal.contents;
  }

  // TODO: What's the most efficient way to do this recursion?
  // parent.differential = dzi/dv (in expression above)
  // grad(parent.node) = ds/dzi
  const res = _.sum(v.parents.map(parent => parent.differential * grad(parent.node)));

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
// dz/dv = 1    /     \    dz/dw = 1   -- differentials
//             v       w               -- children

const add = (v: VarAD, w: VarAD): VarAD => {
  const z = variable(v.val + w.val);
  v.parents.push({ node: z, differential: 1.0 });
  w.parents.push({ node: z, differential: 1.0 });
  return z;
};

const mul = (v: VarAD, w: VarAD): VarAD => {
  const z = variable(v.val * w.val);
  v.parents.push({ node: z, differential: w.val });
  w.parents.push({ node: z, differential: v.val });
  return z;
};

const sub = (v: VarAD, w: VarAD): VarAD => {
  const z = variable(v.val - w.val);
  v.parents.push({ node: z, differential: 1.0 });
  w.parents.push({ node: z, differential: -1.0 });
  return z;
};

// --- Unary ops

const sin = (v: VarAD): VarAD => {
  const z = variable(Math.sin(v.val));
  v.parents.push({ node: z, differential: Math.cos(v.val) });
  return z;
};

const neg = (v: VarAD): VarAD => {
  const z = variable(-v.val);
  v.parents.push({ node: z, differential: -1.0 });
  return z;
};

// TODO: rename to `square` after tf.js dependency is removed
const squared = (v: VarAD): VarAD => {
  const z = variable(v.val * v.val);
  v.parents.push({ node: z, differential: 2.0 * v.val });
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
  const x = variable(0.5);
  const y = variable(4.2);
  const z = add(x, y);

  z.gradVal = { tag: "Just", contents: 1.0 }; // seed: dz/d(x_i) (there's only one output)
  const dx = grad(x);
  const dy = grad(y);
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
  const x = variable(0.5);
  const y = variable(4.2);
  const z = add(mul(x, y), sin(x)); // x * y + sin(x)

  z.gradVal = { tag: "Just", contents: 1.0 };
  const dx = grad(x);
  const dy = grad(y);
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
  const x = variable(0.5);
  const y = variable(4.2);
  const z = squared(sub(x, y));

  z.gradVal = { tag: "Just", contents: 1.0 };
  const dx = grad(x);
  const dy = grad(y);
  console.log("z, x, y", z, x, y);
  console.log("dx, dy", dx, dy);

  const vals = [z, x, y, dx, dy];
  assert(close(z.val, Math.pow(x.val - y.val, 2.0)), ["z := (x - y)^2", vals]);
  assert(close(fromJust(x.gradVal), (2.0 * x.val - 2.0 * y.val)), ["dz/dx", vals]);
  assert(close(fromJust(y.gradVal), (2.0 * y.val - 2.0 * x.val)), ["dz/dy", vals]);
};

// Helpers for programmatic testing

export const energyAndGradAD = (state: number[]) => {
  const stateCopy = [...state];
  const xs = stateCopy.map(x => variable(x));

  // This `z` expression will do two things:
  // 1) Evaluate the expression (forward), resulting in the value of z
  // 2) Dynamically build the computational graph: mutate the `xs` and automatically create anonymous intermediate nodes (parents) that the leaf nodes `xs` hold references to

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
  const dxs = xs.map(grad);
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
