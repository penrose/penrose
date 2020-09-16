import { Tensor, stack, scalar, maximum, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

// Logging flags
const DEBUG_ENERGY = false;
const DEBUG_GRADIENT = true;
const DEBUG_GRADIENT_UNIT_TESTS = false;
const PRINT_TEST_RESULTS = true;

// Consts
const TOL = 1e-3;
const NUM_SAMPLES = 5; // Number of samples to evaluate gradient tests at
const RAND_RANGE = 100;
const EPS_DENOM = 10e-6; // Avoid divide-by-zero in denominator

export const objDict = {
    // (x - y)^2
    equal: (x: VarAD, y: VarAD) => squared(sub(x, y)),

    equalOld: (x: DiffVar, y: DiffVar) => squaredDifference(x, y),

    above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
        // (getY top - getY bottom - offset) ^ 2
        squared(
            sub(sub(top.y.contents, bottom.y.contents),
                varOf(offset))),

    aboveOld: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
        // (getY top - getY bottom - offset) ^ 2
        square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),

    sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
        // TODO: Remove after web-perf done
        // sin(s1.x.contents),
        // squared(s1.x.contents), 
        distsq(center(s1), center(s2)),

    repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
        // TODO: find this out programmatically
        const repelWeight = 10e6;

        console.log("shapes", s1, s2);

        // TODO: this only works for shapes with a center (x,y)
      
        // 1 / (d^2(cx, cy) + eps)
        return mul(inverse(ops.vdistsq(centerList(s1), centerList(s2))), varOf(repelWeight));
    },

    atDist: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset: any) => {
        // Place the latter at a distance from the center of the point
        // TODO: Account for the size/radius of the initial point, rather than just the center

        if (t2 === "Text") {
            // TODO: What type is the offset?

            // Get polygon of text (box)
            // TODO: Make this a GPI property
            // TODO: Port the matrix stuff in `textPolygonFn` / `textPolygonFn2` in Shapes.hs

            // If the point is inside the box, push it outside w/ `noIntersect`

            // If the point is outside the box, try to get the distance from the point to equal the desired distance

        } else {
            throw Error(`unsupported shapes for 'atDist': ${t1}, ${t2}`);
        }
    },

    // Generic repel function for two GPIs with centers
    repelOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
        // TODO: find this out programmatically
        const repelWeight = 10e6;
        // 1 / (d^2(cx, cy) + eps)
        return distsq(center(s1), center(s2)).add(epsd).reciprocal().mul(repelWeight);
    },

    centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): DiffVar => {
        const spacing = varOf(1.1); // arbitrary

        if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
            // HACK: Arbitrarily pick the height of the text
            // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
            return centerArrow2(arr, centerList(text1), centerList(text2),
                [mul(spacing, (text1.h.contents)),
                neg(mul(text2.h.contents, spacing))]);

        } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
    },

  below: ([t1, bottom]: [string, any], [t2, top]: [string, any], offset = 100) =>
    square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),
  // can this be made more efficient (code-wise) by calling "above" and swapping arguments? - stella

    centerArrowOld: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): DiffVar => {
        const spacing = scalar(1.1); // arbitrary

        if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
            // HACK: Arbitrarily pick the height of the text
            // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
            return centerArrow2Old(arr, center(text1), center(text2),
                [spacing.mul(text1.h.contents),
                text2.h.contents.mul(spacing).mul(scalar(1.0)).neg()]);
        } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
    },

  centerLabel: ([t1, arr]: [string, any], [t2, text1]: [string, any], w: number): Tensor => {
    if (typesAre([t1, t2], ["Arrow", "Text"])) {
      const mx = arr.startX.contents.add(arr.endX.contents).div(scalar(2.0));
      const my = arr.startY.contents.add(arr.endY.contents).div(scalar(2.0));
      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = mx.sub(text1.x.contents).square();
      const rh = my.add(text1.h.contents.mul(scalar(1.1))).sub(text1.y.contents).square();
      return lh.add(rh).mul(w);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },

};

export const constrDict = {
    maxSize: ([shapeType, props]: [string, any]) => {
        const limit = Math.max(...canvasSize) / 6;
        switch (shapeType) {
            case "Circle":
                return sub(props.r.contents, varOf(limit));
            default:
                // HACK: report errors systematically
                throw new Error(`${shapeType} doesn't have a maxSize`);
        }
    },

    maxSizeOld: ([shapeType, props]: [string, any]) => {
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
        const limit = 20;
        switch (shapeType) {
            case "Circle":
                return sub(varOf(limit), props.r.contents);
            default:
                // HACK: report errors systematically
                throw new Error(`${shapeType} doesn't have a minSize`);
        }
    },

    minSizeOld: ([shapeType, props]: [string, any]) => {
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
            // console.error("contains circle circle res", res, res.val);

            return res;

        } else if (t1 === "Circle" && t2 === "Text") {
            // Note: The print/debug output will be compiled out in the computational graph! (So it will not display)
            // Note: The shapes' properties are still floats, so each time it's used, it's compiled to a NEW var here
            // (TODO: One question is whether they should be shared root variables?)

            // The problem is that: when a GPI is passed in here, is one of its properties varying? If so, was it looked up from varyingMap? Looks like it gets looked up in varyingMap first; if so, then non-varying properties should be var-ified beforehand so the objective function doesn't have to deal with var-ifying constants
            // TODO: Check the gradients for 'contains' as well (automatically, e.g. manual diff?)
            const d = ops.vdist(centerList(s1), centerList(s2));
            const textR = max((s2.w.contents), s2.h.contents);
            const res = add(sub(d, s1.r.contents), textR);

            return res;
        } else if (t1 === "Rectangle" && t2 === "Circle") {
          // contains [GPI r@("Rectangle", _), GPI c@("Circle", _), Val (FloatV padding)] =
          // -- HACK: reusing test impl, revert later
          //    let r_l = min (getNum r "w") (getNum r "h") / 2
          //        diff = r_l - getNum c "r"
          //    in dist (getX r, getY r) (getX c, getY c) - diff + padding

          // TODO: `rL` is probably a hack for dimensions
          const rL = min(s1.w.contents, div(s1.h.contents, varOf(2.0)));
          const diff = sub(rL, s2.r.contents);
          const d = ops.vdist(centerList(s1), centerList(s2));
          return add(sub(d, diff), offset);

        } else if (t1 === "Rectangle" && t2 === "Text") {
          // contains [GPI r@("Rectangle", _), GPI l@("Text", _), Val (FloatV padding)] =
          // TODO: implement precisely, max (w, h)? How about diagonal case?
          // dist (getX l, getY l) (getX r, getY r) - getNum r "w" / 2 +
          //   getNum l "w" / 2 + padding

          const a1 = ops.vdist(centerList(s1), centerList(s2));
          const a2 = div(s1.w.contents, varOf(2.0));
          const a3 = div(s2.w.contents, varOf(2.0));
          return add(add(sub(a1, a2), a3), offset);

        } else throw new Error(`${[t1, t2]} not supported for contains2`);

    },

    disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        if (t1 === "Circle" && t2 === "Circle") {
            const d = ops.vdist(centerList(s1), centerList(s2));
            const o = [s1.r.contents, s2.r.contents, varOf(10.0)];
            return sub(addN(o), d);
        } else throw new Error(`${[t1, t2]} not supported for disjoint`);
    },

    disjointOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        if (t1 === "Circle" && t2 === "Circle") {
            const d = dist(center(s1), center(s2));
            const o = stack([s1.r.contents, s2.r.contents, 10]);
            return o.sum().sub(d);
        } else throw new Error(`${[t1, t2]} not supported for disjoint`);
    },

    smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        // s1 is smaller than s2
        const offset = mul(varOf(0.4), s2.r.contents);
        return sub(sub(s1.r.contents, s2.r.contents), offset);
    },

    smallerThanOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
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
            const textR = max(s1.w.contents, s1.h.contents);
            const d = ops.vdist(centerList(s1), centerList(s2));
            return sub(add(add(s2.r.contents, textR),
                varOf(padding)),
                d);
        } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
    },

    outsideOfOld: (
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

  tangentTo: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any]
  ) => {
    // Inner tangency -- assuming circle1 contains circle2
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      // Should we bring back the polygon code?
      // ||c_a - c_b|| - (r1 - r2)
      // Outer tangency would be `||c_a - c_b|| - (r1 + r2)`
      return d.sub(r1.sub(r2));
    } else throw new Error(`${[t1, t2]} not supported for tangentTo`);
  },
};

// -------- Helpers for writing objectives

const typesAre = (inputs: string[], expected: string[]) =>
    (inputs.length === expected.length) && _.zip(inputs, expected).map(([i, e]) => i === e);

// -------- (Hidden) helpers for objective/constraints/computations

const centerArrow2 = (arr: any, center1: VarAD[], center2: VarAD[], [o1, o2]: VarAD[]): VarAD => {
    const vec = ops.vsub(center2, center1); // direction the arrow should point to
    const dir = ops.vnormalize(vec);

    let start = center1;
    let end = center2;

    // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality

    // TODO: add abs
    if (gt(ops.vnorm(vec), add(o1, absVal(o2)))) {
        start = ops.vadd(center1, ops.vmul(o1, dir));
        end = ops.vadd(center2, ops.vmul(o2, dir));
    }

    const fromPt = [arr.startX.contents, arr.startY.contents];
    const toPt = [arr.endX.contents, arr.endY.contents];

    return add(ops.vdistsq(fromPt, start), ops.vdistsq(toPt, end));
}

const centerArrow2Old = (arr: any, center1: DiffVar, center2: DiffVar, [o1, o2]: DiffVar[]): DiffVar => {
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

// TODO: Phase out tensor version of `distsq` and `center`
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

// Grad var, level 1
export const gvarOf = (x: number, vname = "", metadata = ""): VarAD => variableAD(x, vname, metadata, false);

export const varOf = (x: number, vname = "", metadata = ""): VarAD => variableAD(x, vname, metadata);

export const numOf = (x: VarAD): number => x.val;

export const variableAD = (x: number, vname = "", metadata = "", isCompNode = true): VarAD => {
    const opName = vname ? vname : String(x);

    return {
        tag: "custom",
        metadata,
        op: opName,
        isInput: false,
        val: x,
        isCompNode,
        valDone: true,
        parents: [],
        children: [],
        parentsGrad: [],
        childrenGrad: [],
        gradVal: { tag: "Nothing" },
        gradNode: { tag: "Nothing" },
        index: -100,

        nodeVisited: false,
        name: "",
    };
};

export const markInput = (v: VarAD, i: number) => {
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
    throw Error("Don't use old gradAD");
    // Already computed/cached the gradient
    // if (v.gradVal.tag === "Just") {
    //   return v.gradVal.contents;
    // }

    // // parent.sensitivity = dzi/dv (in expression above)
    // // grad(parent.node) = ds/dzi

    // const res = _.sum(v.parents.map(parent => parent.sensitivityFn("unit") * gradAD(parent.node)));

    // // Note we both set the gradVal and return it
    // v.gradVal = { tag: "Just", contents: res };

    // return res;
};

const gradADSymbolic = (v: VarAD): VarAD => {
    // Already computed/cached the gradient
    if (v.gradNode.tag === "Just") {
        return v.gradNode.contents;
    }

    // Build subgraph
    let res;
    if (v.parents.length === 0) {
        // node has no parents, so setting grad to 0 (it doesn't influence the output)
        res = gvarOf(0, "0", "no gradient");
    } else { // normal reverse-mode AD chain rule
        // The result is built via pointers to subgraphs that are already built in child nodes of the original comp graph
        res = addN(v.parents.map(parent =>
            mul(fromJust2(parent.sensitivityNode), gradADSymbolic(parent.node), false)),
            false);
    }

    // Mark node as done
    v.gradNode = { tag: "Just", contents: res };

    // Result is a gradient
    res.isCompNode = false;

    // Note that it does not return v
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

// df/f[x] with finite differences about xi
const gradFiniteDiff = (f: (args: number[]) => number) => {
    return (xs: number[]): number[] => {
        const EPSG = 10e-5;

        // Scalar estimate (in 1D)
        // const dfxi = (f, x) => (f(x + EPSG / 2.) - f(x - EPSG / 2.)) / EPSG;

        const xsDiff = xs.map((e, i) => {
            const xsLeft = [...xs];
            xsLeft[i] = xsLeft[i] - EPSG / 2.;
            const xsRight = [...xs];
            xsRight[i] = xsRight[i] + EPSG / 2.;
            return (f(xsRight) - f(xsLeft)) / EPSG;
        });

        return xsDiff;
    };
};


const gradAllSymbolic = (energyGraph: VarAD, xsVars: VarAD[]): VarAD[] => {
    // TODO: Test the below (fully)
    energyGraph.gradNode = { tag: "Just", contents: variableAD(1.0) };
    const dxs = xsVars.map(gradADSymbolic); // Computes it per variable, mutating the graph to set cached results and reuse them
    const gradxs = xsVars.map((x: DiffVar) => fromJust2(x.gradNode));
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

// TODO: regexp-replace ` sensitivityNode: .* })` => ` })`
// The point of making the sensitivity nodes here is that when the gradient is computed, each child needs to know what its partial derivative was, which depends on its position (e.g. either the first or second arg in x * y has a different sensitivity). This can't be looked up in, say, a dict
// You have to build it inline bc it involves references to the variables

const just = (v: VarAD): MaybeVal<VarAD> => {
    return { tag: "Just", contents: v };
};

const none: MaybeVal<VarAD> = { tag: "Nothing" };

const check = (isCompNode: boolean, sensitivityNode: VarAD): MaybeVal<VarAD> => {
    return isCompNode ? just(sensitivityNode) : none;
};

export const add = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(v.val + w.val, "+");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(gvarOf(1.0)) });
        w.parents.push({ node: z, sensitivityNode: just(gvarOf(1.0)) });

        z.children.push({ node: v, sensitivityNode: just(gvarOf(1.0)) });
        z.children.push({ node: w, sensitivityNode: just(gvarOf(1.0)) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

export const addN = (xs: VarAD[], isCompNode = true): VarAD => { // N-way add
    // TODO: Do argument list length checking for other ops generically
    if (xs.length === 0) {
        console.error("node", xs);
        throw Error("argument list to addN is empty; expected 1+ elements");
    } else if (xs.length === 1) {
        return xs[0];
    } else if (xs.length === 2) {
        return add(xs[0], xs[1], isCompNode);
    } else {
        const z = variableAD(_.sum(_.map(xs, x => x.val)), "+ list");
        z.isCompNode = isCompNode;

        if (isCompNode) {
            for (const x of xs) {
                x.parents.push({ node: z, sensitivityNode: just(gvarOf(1.0)) });
                z.children.push({ node: x, sensitivityNode: just(gvarOf(1.0)) });
            }
        } else {
            for (const x of xs) {
                x.parentsGrad.push({ node: z, sensitivityNode: none });
                z.childrenGrad.push({ node: x, sensitivityNode: none });
            }
        }

        return z;
    }
};

export const mul = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(v.val * w.val, "*");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(w) });
        w.parents.push({ node: z, sensitivityNode: just(v) });

        z.children.push({ node: v, sensitivityNode: just(w) });
        z.children.push({ node: w, sensitivityNode: just(v) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

const sub = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(v.val - w.val, "-");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(gvarOf(1.0)) });
        w.parents.push({ node: z, sensitivityNode: just(gvarOf(-1.0)) });

        z.children.push({ node: v, sensitivityNode: just(gvarOf(1.0)) });
        z.children.push({ node: w, sensitivityNode: just(gvarOf(-1.0)) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

const div = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(v.val / w.val, "/");
    z.isCompNode = isCompNode;

    // grad(v/w) = [1/w, -v/w^2]
    if (isCompNode) {
        const vnode = just(div(gvarOf(1.0), w, false));
        const wnode = just(neg(div(v, squared(w, false), false), false));

        v.parents.push({ node: z, sensitivityNode: vnode });
        w.parents.push({ node: z, sensitivityNode: wnode });

        z.children.push({ node: v, sensitivityNode: vnode });
        z.children.push({ node: w, sensitivityNode: wnode });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

const max = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(Math.max(v.val, w.val), "max");
    z.isCompNode = isCompNode;

    // const vFn = (arg: "unit"): number => v.val > w.val ? 1.0 : 0.0;
    // const wFn = (arg: "unit"): number => v.val > w.val ? 0.0 : 1.0;

    const vNode = ifCond(gt(v, w, false), gvarOf(1.0), gvarOf(0.0), false);
    const wNode = ifCond(gt(v, w, false), gvarOf(0.0), gvarOf(1.0), false);
    // NOTE: this adds a conditional to the computational graph itself, so the sensitivities change based on the input values
    // Note also the closure attached to each sensitivityFn, which has references to v and w (which have references to their values)

    // TODO: Come back to this. Should the graph then have an evaluable If node?
    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(vNode) });
        w.parents.push({ node: z, sensitivityNode: just(wNode) });

        z.children.push({ node: v, sensitivityNode: just(vNode) });
        z.children.push({ node: w, sensitivityNode: just(wNode) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

const min = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(Math.min(v.val, w.val), "min");
    z.isCompNode = isCompNode;

    // const vFn = (arg: "unit"): number =< v.val < w.val ? 1.0 : 0.0;
    // const wFn = (arg: "unit"): number =< v.val < w.val ? 0.0 : 1.0;

    const vNode = ifCond(lt(v, w, false), gvarOf(1.0), gvarOf(0.0), false);
    const wNode = ifCond(lt(v, w, false), gvarOf(0.0), gvarOf(1.0), false);
    // NOTE: this adds a conditional to the computational graph itself, so the sensitivities change based on the input values
    // Note also the closure attached to each sensitivityFn, which has references to v and w (which have references to their values)

    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(vNode) });
        w.parents.push({ node: z, sensitivityNode: just(wNode) });

        z.children.push({ node: v, sensitivityNode: just(vNode) });
        z.children.push({ node: w, sensitivityNode: just(wNode) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });
    }

    return z;
};

// --- Unary ops

const sin = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(Math.sin(v.val), "sin");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        const node = just(cos(v, false));
        v.parents.push({ node: z, sensitivityNode: node });

        z.children.push({ node: v, sensitivityNode: node });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

const cos = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(Math.cos(v.val), "cos");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        const node = just(neg(sin(v, false), false));
        v.parents.push({ node: z, sensitivityNode: node });

        z.children.push({ node: v, sensitivityNode: node });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

const neg = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(-v.val, "- (unary)");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        v.parents.push({ node: z, sensitivityNode: just(gvarOf(-1.0)) });

        z.children.push({ node: v, sensitivityNode: just(gvarOf(-1.0)) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

// TODO: rename to `square` after tf.js dependency is removed
const squared = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(v.val * v.val, "squared");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        const node = just(mul(gvarOf(2.0), v, false));
        v.parents.push({ node: z, sensitivityNode: node });

        z.children.push({ node: v, sensitivityNode: node });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

const sqrt = (v: VarAD, isCompNode = true): VarAD => {
    // NOTE: Watch out for negative numbers in sqrt
    // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
    // TODO: rename all the other fns to dz_dv
    const z = variableAD(Math.sqrt(v.val), "sqrt");
    z.isCompNode = isCompNode;

    const dzDv = (arg: "unit"): number => {
        if (v.val < 0) { console.error(`negative arg ${v.val} in sqrt`); }
        return 1.0 / (2.0 * Math.sqrt(Math.max(0, v.val) + EPS_DENOM))
    };

    // TODO: How to do the checks in this graph? I guess sqrt should have a special evaluation/gradient rule?

    // It's important to only construct gradNode if this is a compnode, otherwise it will make recursive calls to the function ops and blow the stack
    if (isCompNode) {
        const gradNode = div(gvarOf(1.0), mul(gvarOf(2.0), sqrt(v, false), false), false);
        v.parents.push({ node: z, sensitivityNode: just(gradNode) });
        z.children.push({ node: v, sensitivityNode: just(gradNode) });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

// TODO: Avoid numerical instability
const inverse = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(1 / (v.val + EPS_DENOM), "inverse");
    z.isCompNode = isCompNode;

    // -1/(x^2 + epsilon) -- This takes care of the divide-by-zero gradient problem
    if (isCompNode) {
        const node = just(
            neg(
                inverse(
                    add(
                        squared(v, false),
                        gvarOf(EPS_DENOM), false),
                    false),
                false)
        );
        v.parents.push({ node: z, sensitivityNode: node });

        z.children.push({ node: v, sensitivityNode: node });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};

const absVal = (v: VarAD, isCompNode = true): VarAD => {
    const z = variableAD(Math.abs(v.val), "abs");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        // x / (|x| + epsilon)
        const node = just(
            div(v,
                add(
                    absVal(v, false),
                    gvarOf(EPS_DENOM),
                    false),
                false)
        );
        v.parents.push({ node: z, sensitivityNode: node });

        z.children.push({ node: v, sensitivityNode: node });
    } else {
        v.parentsGrad.push({ node: z, sensitivityNode: none });

        z.childrenGrad.push({ node: v, sensitivityNode: none });
    }

    return z;
};
// ------- Discontinuous / noGrad ops

const noGrad: VarAD = gvarOf(1.0, "noGrad");

const gt = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    // returns a boolean, which is converted to number
    // TODO: check that this all is right

    const z = variableAD(v.val > w.val ? 1.0 : 0.0, "gt");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        z.children.push({ node: v, sensitivityNode: just(noGrad) });
        z.children.push({ node: w, sensitivityNode: just(noGrad) });

        v.parents.push({ node: z, sensitivityNode: just(noGrad) });
        w.parents.push({ node: z, sensitivityNode: just(noGrad) });
    } else {
        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });

        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });
    }

    return z;
};

const lt = (v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    // returns a boolean, which is converted to number
    const z = variableAD(v.val < w.val ? 1.0 : 0.0, "lt");
    z.isCompNode = isCompNode;

    if (isCompNode) {
        z.children.push({ node: v, sensitivityNode: just(noGrad) });
        z.children.push({ node: w, sensitivityNode: just(noGrad) });

        v.parents.push({ node: z, sensitivityNode: just(noGrad) });
        w.parents.push({ node: z, sensitivityNode: just(noGrad) });
    } else {
        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });

        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });
    }

    return z;
};

const ifCond = (cond: VarAD, v: VarAD, w: VarAD, isCompNode = true): VarAD => {
    // TODO: check that this all is right
    // When the computation graph is evaluated, depending on whether cond is nonnegative, either v or w is evaluated (and returned?)

    const z = variableAD(0.0, "ifCond"); // No value?
    z.isCompNode = isCompNode;

    if (isCompNode) {
        z.children.push({ node: cond, sensitivityNode: just(noGrad) });
        z.children.push({ node: v, sensitivityNode: just(noGrad) });
        z.children.push({ node: w, sensitivityNode: just(noGrad) });

        cond.parents.push({ node: z, sensitivityNode: just(noGrad) });
        v.parents.push({ node: z, sensitivityNode: just(noGrad) });
        w.parents.push({ node: z, sensitivityNode: just(noGrad) });
    } else {
        z.childrenGrad.push({ node: cond, sensitivityNode: none });
        z.childrenGrad.push({ node: v, sensitivityNode: none });
        z.childrenGrad.push({ node: w, sensitivityNode: none });

        cond.parentsGrad.push({ node: z, sensitivityNode: none });
        v.parentsGrad.push({ node: z, sensitivityNode: none });
        w.parentsGrad.push({ node: z, sensitivityNode: none });
    }

    return z;
};

// ADDING A NEW OP: (TODO: document further)
// Add its definition above
// Add it to the opMap
// Add its js mapping (code) to traverseGraph

const opMap = {
    "+": {
        fn: (x: number, y: number): number => x + y,
        gradGraph: variableAD(1.0),
    },
    "+ list": {
        fn: (xs: number[]): number => _.sum(xs),
        gradGraph: variableAD(1.0),
    },
    "*": {
        fn: (x: number, y: number): number => x * y,
    },
    "-": {
        fn: (x: number, y: number): number => x - y,
    },
    "/": {
        fn: (x: number, y: number): number => x / y,
    },
    "max": {
        fn: (x: number, y: number): number => Math.max(x, y),
    },
    "min": {
        fn: (x: number, y: number): number => Math.min(x, y),
    },
    "sin": {
        fn: (x: number): number => Math.sin(x),
    },
    "cos": {
        fn: (x: number): number => Math.cos(x),
    },
    "- (unary)": {
        fn: (x: number): number => -x,
    },
    "squared": {
        fn: (x: number): number => x * x,
    },
    "sqrt": {
        fn: (x: number): number => {
            if (x < 0) { console.error(`negative arg ${x} in sqrt`); }
            return Math.sqrt(Math.max(0, x));
        },
    },
    "inverse": {
        fn: (x: number): number => {
            return 1 / (x + EPS_DENOM);
        },
    },
    "abs": {
        fn: (x: number): number => {
            return x / Math.abs(x + EPS_DENOM);
        },
    },
    // Note that these functions treat booleans as numbers: 1.0 = True, 0.0 = False
    "gt": {
        fn: (x: number, y: number): number => x > y ? 1.0 : 0.0,
    },
    "lt": {
        fn: (x: number, y: number): number => x < y ? 1.0 : 0.0,
    },
    "ifCond": {
        fn: (cond: number, x: number, y: number): number => (cond > 0.0) ? x : y,
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

// Wrapper since energy only has one output

const noWeight: MaybeVal<VarAD> = { tag: "Nothing" };

const genEnergyFn = (xs: VarAD[], z: IVarAD, weight: MaybeVal<VarAD>): any => genCode(xs, [z], "energy", weight);

// Generate code for multi-output function, given its computational graph and a setting for its outputs
// NOTE: Modifies the input computational graph `outputs` to set and clear visited nodes
const genCode = (inputs: VarAD[], outputs: IVarAD[], setting: string, weightNode: MaybeVal<VarAD>): any => {
    let counter = 0;
    let progInputs: string[] = [];
    let progStmts: string[] = [];
    let progOutputs: string[] = [];

    console.log("genCode inputs, outputs, weightNode, setting", inputs, outputs, weightNode, setting);

    let inputsNew;
    console.log("has weight?", weightNode.tag === "Just");
    if (weightNode.tag === "Nothing") {
        inputsNew = inputs;
    } else {
        inputsNew = [weightNode.contents].concat(inputs);
    }

    // Just traverse + name the inputs first, then work backward from the outputs
    // The inputs are the EP weight + original xsVars (if it's energy) or just the xsVars (if it's gradient)
    for (const x of inputsNew) {
        const res = traverseGraph(counter, x);

        const resInputsSorted = _.sortBy(res.inputs, e => e.index).map(e => e.name);
        progInputs = progInputs.concat(resInputsSorted);
        progStmts = progStmts.concat(res.prog);
        progOutputs = progOutputs.concat(res.output);

        // For any code generated for the next output, start on fresh index
        counter = res.counter + 1;
    }

    // For each output, traverse the graph and combine the results sequentially
    for (const z of outputs) {
        const res = traverseGraph(counter, z);

        // const resInputsSorted = _.sortBy(res.inputs, e => e.index).map(e => e.name);
        // progInputs = progInputs.concat(resInputsSorted); // TODO: Is this the right order?

        progStmts = progStmts.concat(res.prog);
        progOutputs = progOutputs.concat(res.output);

        // For any code generated for the next output, start on fresh index
        counter = res.counter + 1;

        // console.error("output node traversed", z);
        // console.error("res inputs", resInputsSorted);
        // console.error("res stmts", res.prog);
        // console.error("res output", res.output);
    }

    let returnStmt: string = "";

    if (setting === "energy") { // Return single scalar
        if (!progOutputs || !progOutputs[0]) { throw Error("not enough energy outputs -- need exactly 1"); }
        returnStmt = `return ${progOutputs[0]};`;
    } else if (setting === "grad") { // Return list of scalars
        const outputNamesStr = progOutputs.join(", ");
        returnStmt = `return [${outputNamesStr}];`;
    }

    const progStr = progStmts.concat([returnStmt]).join("\n");
    console.error("progInputs", "progStr", progInputs, progStr);

    const f = new Function(...progInputs, progStr);
    console.log('generated f\n', f)

    let g;
    if (weightNode.tag === "Nothing") {
        // So you can call the function without spread
        // hasWeight is for "normal" functions that aren't wrapped in the EP cycle (such as the symbolic gradient unit tests)
        g = (xs: number[]) => f(...xs);
    } else {
        // Curry the function so it can be partially applied with the EP weight later, without regenerating the function
        g = (weight: number[]) => {
            return (xs: number[]) => {
                const xs2 = [weight].concat(xs);
                return f(...xs2);
            };
        };
    }
    console.log("overall function generated (g):", g);

    for (const x of inputsNew) {
        clearVisitedNodesInput(x);
    }

    for (const z of outputs) {
        clearVisitedNodesOutput(z);
    }

    return g;
};

// NOTE: Mutates z to store that the node was visited, and what its name is
// `i` is the counter, the initial parameter for generating var names
// `i` starts with 0 for the frst call, children name themselves with the passed-in index (so you know the child's name) and pass their counter back up. Parents do the work of incrementing
const traverseGraph = (i: number, z: IVarAD): any => {
    const c = "x";
    const childType = z.isCompNode ? "children" : "childrenGrad";

    // If this node was already visited, return its name (cached), and counter should not increment
    if (z.nodeVisited) {
        return {
            counter: i,
            prog: [],
            inputs: [],
            output: [],
            references: [z.name]
        };
    }

    // Parents do the work of incrementing
    if (z[childType].length === 0) {
        const leafName = c + String(i);

        // Mark node as visited, with its name as reference for its computed/cached value
        z.nodeVisited = true;
        z.name = leafName;

        // Distinguish between inputs and constants
        if (z.isInput) { // Just return self name for function binding
            return {
                counter: i,
                prog: [],
                inputs: [{ name: leafName, index: z.index }],
                output: [],
                references: []
            };
        }

        let stmt;
        // Otherwise bind const in body
        if (z.op === "noGrad") {
            stmt = `const ${leafName} = 1.0;`;
        } else {
            stmt = `const ${leafName} = ${z.op};`;
        }

        return {
            counter: i,
            prog: [stmt],
            inputs: [],
            output: leafName,
            references: []
        };

    } else if (z[childType].length === 1) { // Unary op
        // debugger;

        const child = z[childType][0].node;
        const res = traverseGraph(i, child);

        let childName;
        let parCounter;
        if (res.references[0]) {
            // Just refer to child if the node was already visited
            // And don't increment counter, since we just looked up a reference, didn't make a new child node
            childName = res.references[0];
            parCounter = res.counter;
        } else {
            childName = c + String(res.counter);
            parCounter = res.counter + 1;
        }

        const parName = c + String(parCounter);

        // Mark node as visited with name as reference
        z.nodeVisited = true;
        z.name = parName;

        const op = z.op;
        let stmt;

        if (z.op === "squared") {
            stmt = `const ${parName} = Math.pow(${childName}, 2);`;
        } else if (z.op === "sqrt") {
            stmt = `const ${parName} = Math.sqrt(${childName});`;
        } else if (z.op === "sin") {
            stmt = `const ${parName} = Math.sin(${childName});`;
        } else if (z.op === "cos") {
            stmt = `const ${parName} = Math.cos(${childName});`;
        } else if (z.op === "+ list") {
            // TODO: Get rid of unary +
            stmt = `const ${parName} = ${childName};`;
        } else if (z.op === "inverse") {
            stmt = `const ${parName} = 1.0 / (${childName} + ${EPS_DENOM});`;
        } else if (z.op === "- (unary)") {
            stmt = `const ${parName} = -${childName};`;
        } else if (z.op === "abs") {
            stmt = `const ${parName} = Math.abs(${childName});`;
        } else {
            stmt = `const ${parName} = (${op})(${childName});`;
        }

        return {
            counter: parCounter,
            prog: res.prog.concat([stmt]),
            inputs: res.inputs,
            output: parName,
            references: []
        };

    } else if (z[childType].length === 2) { // Binary op
        // TODO: refactor repeated code below into the for loop as in ternary
        const child0 = z[childType][0].node;
        const child1 = z[childType][1].node;

        const res0 = traverseGraph(i, child0);
        let childName0;
        let nextCounter;
        if (res0.references[0]) {
            childName0 = res0.references[0];
            nextCounter = res0.counter;
        } else {
            childName0 = c + String(res0.counter);
            nextCounter = res0.counter + 1;
        }

        const res1 = traverseGraph(nextCounter, child1);
        let childName1;
        let parCounter;
        if (res1.references[0]) {
            // Just refer to child if the node was already visited
            childName1 = res1.references[0];
            parCounter = res1.counter;
        } else {
            childName1 = c + String(res1.counter);
            parCounter = res1.counter + 1;
        }

        const parName = c + String(parCounter);

        // Mark node as visited with name as reference
        z.nodeVisited = true;
        z.name = parName;

        const op = z.op;
        let stmt;
        if (op === "max") {
            stmt = `const ${parName} = Math.max(${childName0}, ${childName1});`;
        } else if (op === "min") {
            stmt = `const ${parName} = Math.min(${childName0}, ${childName1});`;
        } else if (z.op === "gt") {
            stmt = `const ${parName} = ${childName0} > ${childName1};`;
        } else if (z.op === "lt") {
            stmt = `const ${parName} = ${childName0} < ${childName1};`;
        } else if (z.op === "+ list") {
            stmt = `const ${parName} = ${childName0} + ${childName1};`;
        } else {
            stmt = `const ${parName} = ${childName0} ${op} ${childName1};`;
        }
        // TODO: Add the rest of the ops to codegen

        // Array efficiency?
        return {
            counter: parCounter,
            prog: res0.prog.concat(res1.prog).concat([stmt]),
            inputs: res0.inputs.concat(res1.inputs),
            output: parName,
            references: []
        };

    } else { // N-ary node
        const childNodes = z[childType].map(e => e.node);

        const childNames = [];
        let prog: string[] = [];
        let inputs: string[] = [];
        let counter = i;

        // Evaluate each child and get its generated code, inputs, and name first
        for (const childNode of childNodes) {
            const res = traverseGraph(counter, childNode);
            prog = prog.concat(res.prog);
            inputs = inputs.concat(res.inputs);

            // Child was already visited; don't generate code again, just a reference
            if (res.references[0]) {
                childNames.push(res.references[0]);
                counter = res.counter;
            } else {
                childNames.push(c + String(res.counter));
                counter = res.counter + 1;
            }
        }

        const parName = c + String(counter);

        // Mark node as visited with name as reference
        // TODO: factor out these 2 lines from all cases
        z.nodeVisited = true;
        z.name = parName;

        const op = z.op;
        let stmt;

        // TODO: Deal with ifCond nodes (ternary)
        // (eval c; eval d; eval e; const xNUM = c ? d : e;)
        // This doesn't short-circuit -- it actually evaluates both branches of the `if` first

        if (op === "ifCond") {
            if (childNames.length !== 3) {
                console.error("args", childNames);
                throw Error("expected three args to if cond");
            }

            stmt = `const ${parName} = ${childNames[0]} ? ${childNames[1]} : ${childNames[2]};`;
        } else if (op === "+ list") {
            const childList = "[".concat(childNames.join(", ")).concat("]");
            stmt = `const ${parName} = ${childList}.reduce((x, y) => x + y);`;
        } else {
            console.error("node", z, z.op);
            throw Error("unknown n-ary operation");
        }

        return {
            counter,
            prog: prog.concat([stmt]),
            inputs,
            output: parName,
            references: []
        };
    }
};

// ----- Helper functions

// TODO: Make this parametric
const fromJust = (n: MaybeVal<number>): number => {
    if (n.tag === "Just") {
        return n.contents;
    }

    throw Error("expected value in fromJust but got Nothing");
}

const fromJust2 = (n: MaybeVal<VarAD>): VarAD => {
    if (n.tag === "Just") {
        return n.contents;
    }

    throw Error("expected value in fromJust2 but got Nothing");
}

const assert = (b: boolean, s: any[]) => {
    const res = b ? "passed" : "failed";
    if (PRINT_TEST_RESULTS) {
        console.assert(b);
        console.log("Assertion", res, ": ", ...s);
    }
    return b;
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
// Note that these ops are hardcoded to assume they are not applied to grad nodes
export const ops = {
    vadd: (v1: VarAD[], v2: VarAD[]): VarAD[] => {
        const res = _.zipWith(v1, v2, add);
        return res;
    },

    vsub: (v1: VarAD[], v2: VarAD[]): VarAD[] => {
        const res = _.zipWith(v1, v2, sub);
        return res;
    },

    vnormsq: (v: VarAD[]): VarAD => {
        const res = v.map(e => squared(e));
        return _.reduce(res, (x, y) => add(x, y, true), variableAD(0.0)); // TODO: Will this one (var(0)) have its memory freed?        
        // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
    },

    vnorm: (v: VarAD[]): VarAD => {
        const res = ops.vnormsq(v);
        return sqrt(res);
    },

    vmul: (c: VarAD, v: VarAD[]): VarAD[] => {
        return v.map(e => mul(c, e));
    },

    vdiv: (v: VarAD[], c: VarAD): VarAD[] => {
        return v.map(e => div(e, c));
    },

    vnormalize: (v: VarAD[]): VarAD[] => {
        // TODO: Need to account for divide by zero?
        const vsize = add(ops.vnorm(v), varOf(EPS_DENOM));
        return ops.vdiv(v, vsize);
    },

    vdist: (v: VarAD[], w: VarAD[]): VarAD => {
        return ops.vnorm(ops.vsub(v, w));
    },

    vdistsq: (v: VarAD[], w: VarAD[]): VarAD => {
        return ops.vnormsq(ops.vsub(v, w));
    },

    // Note: if you want to compute a normsq, use that instead, it generates a smaller computational graph
    vdot: (v1: VarAD[], v2: VarAD[]): VarAD => {
        const res = _.zipWith(v1, v2, mul);
        return _.reduce(res, (x, y) => add(x, y, true), variableAD(0.0));
    },

    vsum: (v: VarAD[]): VarAD => {
        return _.reduce(v, (x, y) => add(x, y, true), variableAD(0.0));
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
    return repeatList(0, n).map(e => RAND_RANGE * (Math.random() - 0.5));
};

// Use this function after synthesizing an energy function, if you want to synthesize the gradient as well, since they both rely on mutating the computational graph to mark the visited nodes and their generated names
// Top-down
const clearVisitedNodesOutput = (z: VarAD) => {
    z.nodeVisited = false;
    // z.name = "";
    z.children.forEach(e => clearVisitedNodesOutput(e.node));
    z.childrenGrad.forEach(e => clearVisitedNodesOutput(e.node));
    // NOTE: This does NOT clear it for z.childrenGrad
}

// Bottom-up
const clearVisitedNodesInput = (x: VarAD) => {
    x.nodeVisited = false;
    // x.name = "";
    x.parents.forEach(e => clearVisitedNodesInput(e.node));
    x.parentsGrad.forEach(e => clearVisitedNodesInput(e.node));
}

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
    // Catch leaf nodes first, or nodes whose values have already been computed and set
    // TODO: Make this code more generic/neater over the # children
    if (z.valDone || !z.children || !z.children.length) {
        if (DEBUG_ENERGY) {
            console.log("z.result", z.val);
        }
        return z.val;
    }

    const zFn = opMap[z.op].fn;

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

// Given an energyGraph of f, clears the graph and returns the compiled energy and gradient of f as functions
// xsVars are the leaves, energyGraph is the topmost parent of the computational graph
export const energyAndGradCompiled = (xs: number[], xsVars: VarAD[], energyGraph: VarAD, weightInfo: WeightInfo, debug = false) => {

    // Zero xsvars vals, gradients, and caching setting
    clearGraphBottomUp(xsVars);
    clearVisitedNodesOutput(energyGraph); // TODO: Do we need this line?

    // Set the weight nodes to have the right weight values (may have been updated at some point during the opt)
    // TODO: Figure out what to do with this and compilation
    setWeights(weightInfo);

    // Set the leaves of the graph to have the new input values
    setInputs(xsVars, xs);

    // Build symbolic gradient of f at xs on the energy graph
    // Note that this does NOT include the weight (i.e. is called on `xsVars`, not `xsVarsWithWeight`! Because the EP weight is not a degree of freedom
    const gradGraph = gradAllSymbolic(energyGraph, xsVars);

    const graphs: GradGraphs = {
        inputs: xsVars,
        energyOutput: energyGraph,
        gradOutputs: gradGraph,
        weight: { tag: "Just", contents: weightInfo.epWeightNode }
    };

    // Synthesize energy and gradient code
    const f0 = genEnergyFn(graphs.inputs, graphs.energyOutput, graphs.weight);
    const gradGen = genCode(graphs.inputs, graphs.gradOutputs, "grad", graphs.weight);
    // TODO: This is called twice (and evaluated; why do the inputs disappear the second time...?

    // Evaluate energy and gradient at the point
    // const energyVal = f0(xs);
    // const gradVal = gradGen(xs);

    if (DEBUG_GRADIENT_UNIT_TESTS) {
        console.log("Running gradient unit tests", graphs);
        testGradSymbolicAll();
        // throw Error("done with gradient unit tests");
    }

    if (DEBUG_GRADIENT) {
        console.log("Testing real gradient on these graphs", graphs);
        testGradSymbolic(0, graphs);
        // throw Error("done with testGradSymbolic");
    }

    // Return the energy and grad on the input, as well as updated energy graph
    return {
        graphs,
        f: f0,
        gradf: gradGen
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
    const newF = genEnergyFn(xsVars, z, noWeight);

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
    const newF = genEnergyFn(xsVars, z, noWeight);

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

export const testReverseAD = () => {
    console.log("testing reverse AD");

    // TODO: Generalize these tests to be parametrized/fuzzed etc
    // TODO: Have nodes store their names/ops?
    testAD1();
    testAD2();
    testAD3();
};

// ----- Functions for testing numeric and symbolic gradients

const testGradFiniteDiff = () => {
    // Only tests with hardcoded functions
    const f = (ys: number[]) => _.sum(_.map(ys, (e: number) => e * e));
    const df = (ys: number[]) => _.map(ys, (e: number) => 2 * e);

    const testResults = [];

    for (let i = 0; i < NUM_SAMPLES; i++) {
        const xs = randList(4);
        const gradEstRes = gradFiniteDiff(f)(xs);
        const expectedRes = df(xs);
        const testRes = assert(eqList(gradEstRes, expectedRes),
            ["test grad finite diff (grad res, expected res)", gradEstRes, expectedRes]);
        testResults.push(testRes);
    }

    const testOverall = assert(all(testResults),
        ["all tests passed? test results:", testResults]);
};

// Given a graph with schema: { inputs: VarAD[], output: VarAD, gradOutputs: VarAD }
// Compile the gradient and check it against numeric gradients
// TODO: Encode the type of `graphs`
const testGradSymbolic = (testNum: number, graphs: GradGraphs): boolean => {
    console.log(`======= START TEST GRAD SYMBOLIC ${testNum} ======`);
    // Synthesize energy and gradient code
    const f0 = genEnergyFn(graphs.inputs, graphs.energyOutput, graphs.weight);
    const gradGen0 = genCode(graphs.inputs, graphs.gradOutputs, "grad", graphs.weight);

    const weight = 1; // TODO: Test with several weights
    let f;
    let gradGen;
    console.log("testGradSymbolic has weight?", graphs.weight);

    if (graphs.weight.tag === "Just") { // Partially apply with weight
        f = f0(weight);
        gradGen = gradGen0(weight);
    } else {
        f = f0;
        gradGen = gradGen0;
    }

    // Test the gradient at several points via evaluation
    const gradEst = gradFiniteDiff(f);
    const testResults = [];

    for (let i = 0; i < NUM_SAMPLES; i++) {
        const xsTest = randList(graphs.inputs.length);
        const energyRes = f(xsTest);
        const gradEstRes = gradEst(xsTest);
        const gradGenRes = gradGen(xsTest);

        console.log("----");
        console.log("test", i);
        console.log("energy at x", xsTest, "=", energyRes);
        console.log("estimated gradient at", xsTest, "=", gradEstRes);
        console.log("analytic gradient at", xsTest, "=", gradGenRes);

        const testRes = assert(eqList(gradEstRes, gradGenRes), ["estimated, analytic gradients:", gradEstRes, gradGenRes]);
        testResults.push(testRes);
    }

    const testOverall = assert(all(testResults),
        ["all tests passed? test results:", testResults]);

    // TODO: Visualize both of them
    console.log(`======= DONE WITH TEST GRAD SYMBOLIC ${testNum} ======`);

    return testOverall;
};

const gradGraph0 = (): GradGraphs => {
    // Build energy graph

    // f(x) = x^2, where x is 100
    // Result: (2 * 100) * 1 <-- this comes from the (new) parent node, dx/dx = 1
    const ref = markInput(variableAD(100.0), 0); // TODO: Should use makeADInputVars
    const head = squared(ref);

    // Build gradient graph
    head.gradNode = { tag: "Just", contents: gvarOf(1.0) };
    const dRef = gradADSymbolic(ref);
    // TODO: The graph does contain circular references, e.g. dx0 may refer to x0 which refers to its gradNode, dx0. So maybe delete the gradNode property? Why is it needed?

    // Print results
    console.log("computational graphs for test 1 (input, output, gradient)", ref, head, dRef);

    // dRef = ref.gradNode.contents
    return {
        inputs: [ref],
        energyOutput: head,
        gradOutputs: [dRef],
        weight: { tag: "Nothing" }
    };
}

// See codegen-results.md for description
const gradGraph1 = (): GradGraphs => {
    // Build energy graph
    const x0 = markInput(variableAD(-5.0), 0);
    const x1 = markInput(variableAD(6.0), 1);
    const a = sub(x0, x1);
    const b = squared(a);
    const c = sin(a);
    // const c = add(a, variableAD(3.0)); // const?
    const z = mul(b, c);

    // Build gradient graph
    z.gradNode = { tag: "Just", contents: gvarOf(1.0) };
    const dx0 = gradADSymbolic(x0);
    const dx1 = gradADSymbolic(x1);

    return {
        inputs: [x0, x1],
        energyOutput: z,
        gradOutputs: [dx0, dx1],
        weight: { tag: "Nothing" }
    };
}

// Test addition of consts to graph (`c`)
const gradGraph2 = (): GradGraphs => {
    // Build energy graph
    const x0 = markInput(variableAD(-5.0), 0);
    const x1 = markInput(variableAD(6.0), 1);
    const a = sub(x0, x1);
    const b = squared(a);
    const c = add(a, variableAD(3.0));
    const z = mul(b, c);

    // Build gradient graph
    z.gradNode = { tag: "Just", contents: gvarOf(1.0) };
    const dx0 = gradADSymbolic(x0);
    const dx1 = gradADSymbolic(x1);

    return {
        inputs: [x0, x1],
        energyOutput: z,
        gradOutputs: [dx0, dx1],
        weight: { tag: "Nothing" }
    };
}

// Test vars w/ no grad
const gradGraph3 = (): GradGraphs => {
    // Build energy graph

    const x0 = markInput(variableAD(100.0), 0);
    const x1 = markInput(variableAD(-100.0), 0);
    const inputs = [x0, x1];
    const head = squared(x0);

    // Build gradient graph
    const dxs = gradAllSymbolic(head, inputs);

    return {
        inputs,
        energyOutput: head,
        gradOutputs: dxs,
        weight: { tag: "Nothing" }
    };
}

// Test toPenalty
const gradGraph4 = (): GradGraphs => {
    // Build energy graph

    const x0 = markInput(variableAD(100.0), 0);
    const inputs = [x0];
    const head = fns.toPenalty(x0);

    // Build gradient graph
    const dxs = gradAllSymbolic(head, inputs);

    return {
        inputs,
        energyOutput: head,
        gradOutputs: dxs,
        weight: { tag: "Nothing" }
    };
}

export const testGradSymbolicAll = () => {
    console.log("testing symbolic gradients");

    testGradFiniteDiff();

    const graphs: GradGraphs[] = [
        gradGraph0(),
        gradGraph1(),
        gradGraph2(),
        gradGraph3(),
        gradGraph4()
    ];

    const testResults = graphs.map((graph, i) => testGradSymbolic(i, graph));

    console.error(`All grad symbolic tests passed?: ${all(testResults)}`);
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

// TODO: Move these utils elsewhere

export function repeat<T>(i: number, x: T) {
    const xs = [];

    for (let j = 0; j < i; j++) {
        xs.push(x);
    };

    return xs;
};

export const all = (xs: boolean[]) =>
    xs.reduce((prev, curr) => prev && curr, true);

