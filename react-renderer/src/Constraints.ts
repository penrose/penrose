import { Tensor, stack, scalar, maximum, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

const TOL = 1e-4;
const DEBUG_ENERGY = false;

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

  containsOld: (
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

  // TODO: Had to rename due to needing to match funciton names in backend
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: DiffVar
  ) => {

    if (t1 === "Circle" && t2 === "Circle") {
      console.error("circle, circle", s1, s2, offset);

      const d = ops.vdist(centerList(s1), centerList(s2));
      const o = offset
        ? sub(sub(s1.r.contents, s2.r.contents), offset)
        : sub(s1.r.contents, s2.r.contents);
      const res = sub(d, o);

      console.error("contains circle circle res", res, res.val);

      return res;

    } else if (t1 === "Circle" && t2 === "Text") {
      // Note: The print/debug output will be compiled out in the computational graph! (So it will not display)
      // Note: The shapes' properties are still floats, so each time it's used, it's compiled to a NEW var here
      // (TODO: One question is whether they should be shared root variables?)

      // The problem is that: when a GPI is passed in here, is one of its properties varying? If so, was it looked up from varyingMap? Looks like it gets looked up in varyingMap first; if so, then non-varying properties should be var-ified beforehand so the objective function doesn't have to deal with var-ifying constants
      // TODO: Check the gradients for 'contains' as well (automatically, e.g. manual diff?)

      console.error("circle, text", s1, s2);

      const d = ops.vdist(centerList(s1), centerList(s2));
      const textR = max((s2.w.contents), s2.h.contents);
      const res = add(sub(d, s1.r.contents), textR);
      console.log("contains2 circle text res", res);

      return res;

    } else throw new Error(`${[t1, t2]} not supported for contains2`);

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

export const centerList = (props: any): VarAD[] => {
  return [props.x.contents, props.y.contents];
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

export const varOf = (x: number, vname = "", metadata = ""): VarAD => variableAD(x, vname, metadata);

export const variableAD = (x: number, vname = "", metadata = ""): VarAD => {
  const opName = vname ? vname : String(x);

  return {
    tag: "custom",
    metadata,
    op: opName,
    isInput: false,
    val: x,
    valDone: true,
    parents: [],
    children: [],
    gradVal: { tag: "Nothing" },
    index: -1
  };
};

const markInput = (v: VarAD, i: number) => {
  v.isInput = true;
  v.index = i;
  return v;
};

const inputVarAD = (x: number, i: number, vname = ""): VarAD => {
  return markInput(variableAD(x), i);
};

// Copies the input numbers and returns a new list of vars marked as inputs
export const makeADInputVars = (xs: number[]): VarAD[] => {
  const xsCopy = [...xs];
  const xsVars = xsCopy.map((x, i) => markInput(variableAD(x), i));
  // Need to mark these so we know what's special when generating the function code
  return xsVars;
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

const gradAD = (v: VarAD): number => {
  // console.log("grad", v.op);

  // Already computed/cached the gradient
  if (v.gradVal.tag === "Just") {
    // console.log("return cached", v.gradVal.contents);
    return v.gradVal.contents;
  }

  // TODO: What's the most efficient way to do this recursion?
  // parent.sensitivity = dzi/dv (in expression above)
  // grad(parent.node) = ds/dzi

  // console.log("sum parents", v.parents, v.parents.map(parent => parent.sensitivity));
  const res = _.sum(v.parents.map(parent => parent.sensitivityFn("unit") * gradAD(parent.node)));

  // console.log("return calculated", res);
  // Note we both set the gradVal and return it
  v.gradVal = { tag: "Just", contents: res };

  return res;
};

// (Don't use this, you probably want energyAndGradDynamic)
// Computes the gradient for each (mutable) variable, and sets the results in the computational graph.
// The variables passed in should be all the leaves of the graph.
// Returns a vector of the same length
const gradAll = (energyGraph: VarAD, xsVars: VarAD[]): number[] => {
  energyGraph.gradVal = { tag: "Just", contents: 1.0 };
  const dxs = xsVars.map(gradAD); // Computes it per variable, mutating the graph to set cached results and reuse them
  const gradxs = xsVars.map((x: DiffVar) => fromJust(x.gradVal));
  return gradxs;
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
// NOTE: The names of these ops matter for opMap, don't change them

export const add = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val + w.val, "+");
  v.parents.push({ node: z, sensitivity: 1.0, sensitivityFn: () => 1.0 });
  w.parents.push({ node: z, sensitivity: 1.0, sensitivityFn: () => 1.0 });

  // TODO: Check if the sensitivities are right
  z.children.push({ node: v, sensitivity: 1.0, sensitivityFn: () => 1.0 });
  z.children.push({ node: w, sensitivity: 1.0, sensitivityFn: () => 1.0 });

  return z;
};

export const mul = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val * w.val, "*");
  v.parents.push({ node: z, sensitivity: w.val, sensitivityFn: () => w.val, });
  w.parents.push({ node: z, sensitivity: v.val, sensitivityFn: () => v.val, });

  z.children.push({ node: v, sensitivity: w.val, sensitivityFn: () => w.val, });
  z.children.push({ node: w, sensitivity: v.val, sensitivityFn: () => v.val, });

  return z;
};

const sub = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(v.val - w.val, "-");
  v.parents.push({ node: z, sensitivity: 1.0, sensitivityFn: () => 1.0, });
  w.parents.push({ node: z, sensitivity: -1.0, sensitivityFn: () => -1.0, });

  z.children.push({ node: v, sensitivity: 1.0, sensitivityFn: () => 1.0, });
  z.children.push({ node: w, sensitivity: -1.0, sensitivityFn: () => -1.0, });

  return z;
};

const max = (v: VarAD, w: VarAD): VarAD => {
  const z = variableAD(Math.max(v.val, w.val), "max");

  const vFn = (arg: "unit"): number => v.val > w.val ? 1.0 : 0.0;
  const wFn = (arg: "unit"): number => v.val > w.val ? 0.0 : 1.0;

  // NOTE: this adds a conditional to the computational graph itself, so the sensitivities change based on the input values
  // Note also the closure attached to each sensitivityFn, which has references to v and w (which have references to their values)
  v.parents.push({ node: z, sensitivity: vFn("unit"), sensitivityFn: vFn, });
  w.parents.push({ node: z, sensitivity: wFn("unit"), sensitivityFn: wFn, });

  z.children.push({ node: v, sensitivity: vFn("unit"), sensitivityFn: vFn, });
  z.children.push({ node: w, sensitivity: wFn("unit"), sensitivityFn: wFn, });

  return z;
};

// --- Unary ops

const sin = (v: VarAD): VarAD => {
  const z = variableAD(Math.sin(v.val), "sin");
  v.parents.push({ node: z, sensitivity: Math.cos(v.val), sensitivityFn: () => Math.cos(v.val), });

  // TODO: cache values
  z.children.push({ node: v, sensitivity: Math.cos(v.val), sensitivityFn: () => Math.cos(v.val), });

  return z;
};

const neg = (v: VarAD): VarAD => {
  const z = variableAD(-v.val, "- (unary)");
  v.parents.push({ node: z, sensitivity: -1.0, sensitivityFn: () => -1.0, });

  z.children.push({ node: v, sensitivity: -1.0, sensitivityFn: () => -1.0, });

  return z;
};

// TODO: rename to `square` after tf.js dependency is removed
const squared = (v: VarAD): VarAD => {
  const z = variableAD(v.val * v.val, "squared");
  v.parents.push({ node: z, sensitivity: 2.0 * v.val, sensitivityFn: () => 2.0 * v.val, });

  z.children.push({ node: v, sensitivity: 2.0 * v.val, sensitivityFn: () => 2.0 * v.val, });

  return z;
};

const sqrt = (v: VarAD): VarAD => {
  // NOTE: Watch out for negative numbers in sqrt
  // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
  // TODO: rename all the other fns to dz_dv
  const z = variableAD(Math.sqrt(v.val), "sqrt");
  const EPSD = 10e-6;

  const dzDv = (arg: "unit"): number => {
    if (v.val <= 0) { throw Error(`non-positive arg ${v.val} in sqrt`); }
    return 1.0 / (2.0 * Math.sqrt(v.val + EPSD))
  };

  v.parents.push({ node: z, sensitivity: dzDv("unit"), sensitivityFn: dzDv, });

  z.children.push({ node: v, sensitivity: dzDv("unit"), sensitivityFn: dzDv, });

  return z;
};

// ADDING A NEW OP: (TODO: document furtehr)
// Add its definition above
// Add it to the opMap
// Add its js mapping (code) to traverseGraph

const opMap = {
  "+": (x: number, y: number): number => x + y,
  "*": (x: number, y: number): number => x * y,
  "-": (x: number, y: number): number => x - y,
  "max": (x: number, y: number): number => Math.max(x, y),
  "sin": (x: number): number => Math.sin(x),
  "- (unary)": (x: number): number => -x,
  "squared": (x: number): number => x * x,
  "sqrt": (x: number): number => {
    if (x <= 0) { throw Error(`non-positive arg ${x} in sqrt`); }
    return Math.sqrt(x);
  },
}

// ----- Codegen

// Traverses the computational graph of ops obtained by interpreting the energy function, and generates code corresponding to just the ops (in plain js), which is then turned into an evaluable js function via the Function constructor

// Example of constructing an n-ary function by calling the Function constructor: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/Function

// const args = ["x0", "x1", "x2"];
// const inputs = [0, 1, 2];
// const f = new Function(...args, 'return x0 + x1 + x2');
// console.log(f(...inputs));

// (Returns `3`)

const genEnergyFn = (z: IVarAD): any => {

  // Initial parameters for generating var names
  // Start with 0, children name themselves with the passed-in index (so you know the child's name) and pass their counter back up. Parents do the work of incrementing
  const res = traverseGraph(0, z);
  const returnStmt = `return ${res.output};`;

  const progStr = res.prog.concat([returnStmt]).join("\n");
  // Have to sort the inputs to match the order of the initial variable list of xs
  const progInputs = _.sortBy(res.inputs, e => e.index).map(e => e.name);

  // console.error("progInputs", progInputs);
  // console.error("progStr", progStr);

  const f = new Function(...progInputs, progStr);
  console.log('generated f', f)
  const g = (xs: number[]) => f(...xs); // So you can call the function without spread

  return g;
};

const traverseGraph = (i: number, z: IVarAD): any => {
  const c = "x";

  // Parents do the work of incrementing
  if (z.children.length === 0) {
    const leafName = c + String(i);
    // Distinguish between inputs and constants

    if (z.isInput) { // Just return self name for function binding
      return {
        counter: i,
        prog: [],
        inputs: [{ name: leafName, index: z.index }],
        output: []
      };
    }

    // Otherwise bind const in body
    const stmt = `const ${leafName} = ${z.op};`

    return {
      counter: i,
      prog: [stmt],
      inputs: [],
      output: leafName
    };

  } else if (z.children.length === 1) { // Unary op
    const child = z.children[0].node;
    const res = traverseGraph(i, child);

    const childName = c + String(res.counter);
    const parCounter = res.counter + 1;
    const parName = c + String(parCounter);

    const op = z.op;
    let stmt;

    if (z.op === "squared") {
      stmt = `const ${parName} = Math.pow(${childName}, 2);`;
    } else if (z.op === "sqrt") {
      stmt = `const ${parName} = Math.sqrt(${childName}, 2);`;
    } else {
      stmt = `const ${parName} = (${op})(${childName});`;
    }

    return {
      counter: parCounter,
      prog: res.prog.concat([stmt]),
      inputs: res.inputs,
      output: parName
    };

  } else if (z.children.length === 2) { // Binary op
    const child0 = z.children[0].node;
    const child1 = z.children[1].node;

    const res0 = traverseGraph(i, child0);
    const childName0 = c + String(res0.counter);

    const nextCounter = res0.counter + 1;

    const res1 = traverseGraph(nextCounter, child1);
    const childName1 = c + String(res1.counter);

    const parCounter = res1.counter + 1;
    const parName = c + String(parCounter);

    const op = z.op;
    let stmt;
    if (op === "max") {
      stmt = `const ${parName} = Math.max(${childName0}, ${childName1});`;
    } else {
      stmt = `const ${parName} = ${childName0} ${op} ${childName1};`;
    }
    // TODO: Add the rest of the ops to codegen

    // Array efficiency?
    return {
      counter: parCounter,
      prog: res0.prog.concat(res1.prog).concat([stmt]),
      inputs: res0.inputs.concat(res1.inputs),
      output: parName
    };

  } else {
    throw Error("Ops that are not nullary, unary, or binary are not supported");
  }
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

  vnorm: (v: VarAD[]): VarAD => {
    const res = ops.vnormsq(v);
    return sqrt(res);
  },

  vdist: (v: VarAD[], w: VarAD[]): VarAD => {
    return ops.vnorm(ops.vsub(v, w));
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

export const fns = {

  toPenalty: (x: VarAD): VarAD => {
    return squared(max(x, variableAD(0.0)));
  }

};

// Returns true if x is a VarAD, false if x is a Tensor
export const isCustom = (x: DiffVar): boolean => {
  return x.tag;
};

export const eqNum = (x: number, y: number): boolean => {
  return Math.abs(x - y) < TOL;
};

export const eqList = (xs: number[], ys: number[]): boolean => {
  if (xs == null || ys == null) return false;
  if (xs.length !== ys.length) return false;

  //   _.every(_.zip(xs, ys), e => eqNum(e[0], e[1]));

  // let xys = _.zip(xs, ys);
  // return xys?.every(e => e ? Math.abs(e[1] - e[0]) < TOL : false) ?? false;
  // Typescript won't pass this code no matter how many undefined-esque checks I put in??

  for (let i = 0; i < xs.length; i++) {
    if (!eqNum(xs[i], ys[i])) return false;
  }

  return true;
};

export const repeatList = (e: any, n: number): any[] => {
  const xs = [];
  for (let i = 0; i < n; i++) {
    xs.push(e);
  }
  return xs;
};

export const randList = (n: number): number[] => {
  return repeatList(0, n).map(e => Math.random());
};

// Mutates z (top node) to clear all vals and gradients of its children
// NOTE that this will zero all the nodes in the graph, including the leaves (such as the stepEP parameters)
const clearGraphTopDown = (z: VarAD) => {
  z.val = 0;
  z.valDone = false; // This is necessary so we can cache energy values in comp graph
  z.gradVal = { tag: "Nothing" };
  z.children.forEach(e => clearGraphTopDown(e.node));
}

const clearGraphBottomUp = (xs: VarAD[]) => {
  xs.forEach(x => {
    x.val = 0;
    x.valDone = false; // This is necessary so we can cache energy values in comp graph
    x.gradVal = { tag: "Nothing" };
    clearGraphBottomUp(x.parents.map(p => p.node));
  });
};

// Mutates xsVars (leaf nodes) to set their values to the inputs in xs (and name them accordingly by value)
// NOTE: the xsVars should already have been set as inputs via makeAdInputVars
// NOTE: implicitly, the orders of the values need to match the order of variables
const setInputs = (xsVars: VarAD[], xs: number[]) => {
  xsVars.forEach((v, i) => {
    const val = xs[i];
    v.val = val;
    v.op = String(val);
  });
};

// Mutates graph (defined by z, the head) to evaluate the comp graph from top down, setting all values in children (intermediate node). Returns energy.
// We have to do this in the graph, not the compiled energy, because we need the values of the intermediate nodes to compute the gradient.
const evalEnergyOnGraph = (z: VarAD) => {
  const zFn = opMap[z.op];

  // Catch leaf nodes first, or nodes whose values have already been computed and set
  // TODO: Make this code more generic/neater over the # children
  if (z.valDone || !z.children || !z.children.length) {
    if (DEBUG_ENERGY) {
      console.log("z.result", z.val);
    }
    return z.val;
  }

  // TODO: Fix how leaf nodes are stored as numbers, not strings (for the second check)
  // TODO: Check that leaf nodes (numbers) don't have children (this also fails if the leaf val is 0...)
  if (!zFn && !Number(z.op)) throw Error(`invalid op ${z.op}`);

  if (z.children.length === 1) {
    const childVal = evalEnergyOnGraph(z.children[0].node);
    const res = zFn(childVal);
    z.val = res;
    z.valDone = true;

    if (DEBUG_ENERGY) {
      console.log("z result:", z.op, childVal, "=", z.val);
    }
    return z.val;
  } else if (z.children.length === 2) {
    const childVal0 = evalEnergyOnGraph(z.children[0].node);
    const childVal1 = evalEnergyOnGraph(z.children[1].node);
    const res = zFn(childVal0, childVal1);
    z.val = res;
    z.valDone = true;

    if (DEBUG_ENERGY) {
      console.log("z result:", z.op, childVal0, childVal1, "=", z.val);
    }
    return z.val;
  } else throw Error(`invalid # children: ${z.children.length}`);
};

const setWeights = (info: WeightInfo) => {
  info.constrWeightNode.val = info.constrWeight;
  info.constrWeightNode.op = String(info.constrWeight);

  info.epWeightNode.val = info.epWeight;
  info.epWeightNode.op = String(info.epWeight);
};

// Given an energyGraph of f, clears the graph and returns the energy and gradient of f at xs (by walking the graph and mutating values)
// The returned energyGraph will have intermediate values set
export const energyAndGradDynamic = (xs: number[], xsVars: VarAD[], energyGraph: VarAD, weightInfo: WeightInfo, debug = false) => {

  // Zero xsvars vals, gradients, and caching setting
  clearGraphBottomUp(xsVars);

  // Set the weight nodes to have the right weight values (may have been updated at some point during the opt)
  setWeights(weightInfo);

  // Set the leaves of the graph to have the new input values
  setInputs(xsVars, xs);

  // Evaluate energy at the new xs, setting the values of the intermediate nodes
  const energyVal = evalEnergyOnGraph(energyGraph);
  // (NOTE: It's only necessary to evaluate the energy on the graph first if you're taking the gradient afterward)
  // (If you just want the energy, you can use the compiled energy function)

  // Evaluate gradient of f at xs on the energy graph
  const gradVal = gradAll(energyGraph, xsVars);

  if (DEBUG_ENERGY) {
    console.error("generated energy function", genEnergyFn(energyGraph), genEnergyFn(energyGraph)(xs));
  }

  if (debug) {
    console.log("====== Test results for energyAndGradDynamic (vs. hardcoded energy) ======");
    // TODO: This needs to change when we build a more general energy
    const testResult = energyAndGradADHardcoded(xs);

    // TEST: Check correctness of energy vs. the hardcoded sum-of-squares energy
    console.log("correct energy?", eqNum(energyVal, testResult.energyVal));
    console.log("custom energy val", energyVal);
    console.log("hardcoded (golden) energy val", testResult.energyVal);

    // TEST: Check correctness of gradient vs. the gradient of hardcoded sum-of-squares energy
    console.log("correct gradient?", eqList(gradVal, testResult.gradVal));
    console.log("custom grad val", gradVal);
    console.log("hardcoded (golden) grad val", testResult.gradVal);
    console.log("xsVars with grads (backward)", xsVars);
  }

  // Return the energy and grad on the input, as well as updated energy graph
  return {
    energyVal,
    gradVal,
    energyGraph
  };
};

export const energyAndGradAD = (f: (...arg: DiffVar[]) => DiffVar, xs: number[], xsVarsInit: DiffVar[]) => {
  // NOTE: mutates xsVars
  console.log("energy and grad NEW with vars", xs, xsVarsInit);

  const xsToUse = randList(xs.length); // TODO: use xs; this is just for testing
  const xsVars = makeADInputVars(xsToUse);

  // ---- FORWARD
  // TEST A
  // This makes a NEW computational graph by interpreting `f` on xsVars
  const z = f(...xsVars);

  // TODO: Make proper unit tests
  // TEST B
  // const [v0, v1] = [inputVarAD(1.0, 0), inputVarAD(2.0, 1)];
  // const z = sub(v0, v1);

  // const dxs01 = [v0, v1].map(gradAD);
  // console.log("z", z);
  // console.log("xsVars with grads (backward one)", dxs01);
  // throw Error("after dxs01");

  // TEST C
  // const z = add(
  //   squared(sub(inputVarAD(1.0, 0), inputVarAD(2.0, 1))),
  //   squared(sub(inputVarAD(3.0, 2), inputVarAD(4.0, 3))),
  // );

  z.gradVal = { tag: "Just", contents: 1.0 }; // just in case, but it's also auto-set in `f`
  const energyZ = z.val;

  console.log("xsVars with ops (forward only)", xsVars);

  // ----- TRAVERSE/PRINT GRAPH AS CODE 
  // from bottom up. (No grads, just energy)
  console.log("z (with children)", z);
  console.log("traverse graph");
  const newF = genEnergyFn(z);

  console.log("normal f result", z.val);
  // TODO: Use g?
  // const xsIn = [1.0, 2.0, 3.0, 4.0];
  console.log("generated f result", newF, xs, newF(xsToUse));

  // ---- BACKWARD
  // This will take the grad of all of them, mutating xsVars to store grad values (OR lookup if already cached -- TODO note this!)
  const dxs = xsVars.map(gradAD);
  console.log("xsVars with grads (backward)", xsVars);

  const gradxs = xsVars.map((x: DiffVar) => fromJust(x.gradVal));
  console.log("xsVars grad values", gradxs);

  const testResult = energyAndGradADHardcoded(xsToUse);

  console.log("correct gradient?", eqList(gradxs, testResult.gradVal));
  console.log("custom grad val", gradxs);
  console.log("hardcoded (golden) grad val", testResult.gradVal);

  // ------------- ROUND 2
  console.error("ROUND 2");

  // Zero xsvars vals and gradients
  clearGraphBottomUp(xsVars);
  console.log("cleared", z);

  // Evaluate energy with a different xsvars/vals setting (with z=1)
  const xs2 = randList(xs.length);
  setInputs(xsVars, xs2);
  console.log("set inputs", xsVars, xs2);

  const energyVal2 = evalEnergyOnGraph(z);
  // It's only necessary to evaluate the energy on the graph first if you're taking the gradient afterward
  // If you just want the energy, you can use the compiled energy function

  // Check correctness of energy
  const testResult2 = energyAndGradADHardcoded(xs2);
  console.log("NEW correct energy?", eqNum(energyVal2, testResult2.energyVal));
  console.log("NEW custom energy val", energyVal2);
  console.log("NEW hardcoded (golden) energy val", testResult2.energyVal);

  // Evaluate gradient
  z.gradVal = { tag: "Just", contents: 1.0 };
  const dxs2 = xsVars.map(gradAD);
  console.log("NEW xsVars with grads (backward)", xsVars);
  const gradxs2 = xsVars.map((x: DiffVar) => fromJust(x.gradVal));
  console.log("xsVars grad values", gradxs);

  // Check correctness of gradient
  console.log("NEW correct gradient?", eqList(gradxs2, testResult2.gradVal));
  console.log("NEW custom grad val", gradxs2);
  console.log("NEW hardcoded (golden) grad val", testResult2.gradVal);

  throw Error("after backward of general xs");

  return { energyVal: energyZ, gradVal: gradxs };
};

// API:
// Interpret energy: xs -> list of xs vars
// Compile energy: the var result (z) -> function ([x] -> x)
// Grad (f, xs): takes the list of xs vars, (which carry f) as well as a list of scalars
//   zeroes the whole graph's sensitivities, vals, gradVals --- I guess this should be stored as a function?
//   evaluates the computational graph on the values
//     TODO: I guess then the sensitivity has to be stored as an op to be applied anyway! So then you evaluate the computational graph and then transform it into the gradient expression graph?
//   computes the gradient on the computational graph for each var
// What gets evaluated more, the energy or the grad?
// Don't I still have to port the line search, etc to this new format?
// Is there some way to instead construct the computational graph for the gradient instead, and then compile it?

export const energyAndGradADOld = (f: (...arg: DiffVar[]) => DiffVar, xs: number[], xsVarsInit: DiffVar[]) => {
  // NOTE: mutates xsVars
  console.log("energy and grad with vars", xs, xsVarsInit);

  // Questions/assumptions: TODO 
  // Who calls the energy?
  // 1) Who sets the seed??
  // 2) What if someone has already called energy? (vars will already exist) -- I guess just re-evaluates and makes a new comp graph. Does the memory get cleared right?
  // 3) what if someone has already called the grad? (vals will be cached)
  // Who zeroes the variables? 
  // Who zeroes the gradients?

  // TODO: Why is the grad twice the right value when using xsVarsInit? Each var seems to have 3 parents when it should just have one--maybe due to extra calls

  // For now, just make the vars from scratch
  // const xsCopy = [...xs];
  // const xsVars = xsCopy.map(x => variableAD(x));
  const xsVars = makeADInputVars(xs);

  // ---- FORWARD
  // const z = f(...xsVars);

  // TODO: Make proper unit tests
  const [v0, v1] = [inputVarAD(1.0, 0), inputVarAD(2.0, 1)];
  const z = sub(v0, v1);

  // const z = add(
  //   squared(sub(inputVarAD(1.0, 0), inputVarAD(2.0, 1))),
  //   squared(sub(inputVarAD(3.0, 2), inputVarAD(4.0, 3))),
  // );

  z.gradVal = { tag: "Just", contents: 1.0 }; // just in case, but it's also auto-set in `f`
  const energyZ = z.val;

  const dxs01 = [v0, v1].map(gradAD);
  console.log("z", z);
  console.log("xsVars with grads (backward one)", dxs01);

  console.log("xsVars with ops (forward only)", xsVars);
  // Note that chrome seems to print by reference, so if the grad is calculated, this will actually show it unless you throw the error directly after
  // throw Error("test");

  // ----- TRAVERSE/PRINT GRAPH AS CODE 
  // from bottom up. (No grads, just energy)
  // Depth-first or breadth-first?

  // The graph is built from top-down though. Should we also have nodes store their children?
  console.log("z (with children)", z);

  // Example:
  //       Z (+)
  //      / \
  //     v   v
  //  X (^2)  Y (-)
  //     \   / \
  //      v v   v
  //       A    B
  // That generates the code:
  // TODO: name vars correctly in example
  // Z := X + Y
  // X := A^2
  // Y := A - B
  // A := 5.0
  // B := 2.4

  // TODO: What does this *gradient expression graph* look like? How/when is it created? (On evaluating the existing computational graph?) What information is needed to create it? What information does it need to provide? (To compile it into gradient code)

  // TODO: Should the generated gradient code be interleaved with the generated energy code?
  // Basically what should be generated is the unrolled version of gradAD, right? How long is that function?
  // TODO: Problem: How to do the caching of gradient values??
  // Grad Z(A, B) = [dZ/dA, dZ/dB]
  // Z = X + Y = A^2 + (A - B) ==> Grad Z(A, B) = [2A + 1, -1]

  console.log("traverse graph");
  const newF = genEnergyFn(z);

  console.log("normal f result", z.val);
  // TODO: Use g?
  // const xsIn = [1.0, 2.0, 3.0, 4.0];
  console.log("generated f result", newF, xs, newF(xs));

  // -------------- PERF

  // const t0 = performance.now();

  // let fRes;
  // for (let i = 0; i < 10000000; i++) {
  //   fRes = newF(xs);
  // }

  // const t1 = performance.now();
  // console.error("Call to fns took " + (t1 - t0) + " milliseconds.")

  // 10 000 000 calls / 24,281 ms = 411.8 calls/ms = 411845 calls/s
  // = ~500k calls/s (for a small energy function)
  // (Earlier we could do maybe 5k calls/second? since we did 5k steps/s)

  // ---- BACKWARD
  // This will take the grad of all of them, mutating xsVars to store grad values (OR lookup if already cached -- TODO note this!)
  const dxs = xsVars.map(gradAD);
  console.log("xsVars with grads (backward)", xsVars);

  const gradxs = xsVars.map((x: DiffVar) => fromJust(x.gradVal));
  console.log("xsVars grad values", gradxs);

  throw Error("test");

  return { energyVal: energyZ, gradVal: gradxs };
};

const energyAndGradADHardcoded = (state: number[]) => {
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
