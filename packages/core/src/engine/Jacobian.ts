import { numbered } from "compiler/Style";
import eig from "eigen";
import _ from "lodash";
import { VarAD } from "types/ad";
import { State } from "types/state";
import { Path } from "types/style";
import { differentiable } from "./Autodiff";
import { evalShapes, insertVaryings } from "./Evaluator";
import { genFns } from "./Optimizer";

// internal typing
type Matrix = number[][];

// toggle on to run Jacobian.test.ts
export const TESTING = false;

// returns nspacevectors as rows of the output matrix
export const getNullspaceBasisVectors = (jacobian: Matrix): Matrix => {
  const J = new eig.Matrix(jacobian);

  // induced matrix, we use to find nullspace vecs
  const A = J.transpose().matMul(J);

  const svd = eig.Decompositions.svd(A, false);

  const numberedSingularValues = numbered(eigObjToMatrix(svd.sv));

  const eps = 10e-6;

  const zeroSingularValues = numberedSingularValues.filter(([[sv], index]) => {
    return sv < eps && sv > -eps;
  });

  const zeroSingularValueIndexes = zeroSingularValues.map(([[sv], index]) => {
    return index;
  });

  // either V or U works here, as A is a square matrix
  // transpose svd.V, since I want columns of V
  const evecsAsRows = eigObjToMatrix(svd.V.transpose());

  const nullspaceEvecs = evecsAsRows.filter((evec, index) => {
    return zeroSingularValueIndexes.includes(index);
  });

  if (!TESTING) {
    // eig.GC.flush(); // can't flush if testing; needs to reference matrix objs
  }

  return nullspaceEvecs;
};

export const eigObjToMatrix = (obj: any): Matrix => {
  let m: Matrix = [];
  for (let i = 0; i < obj.rows(); i++) {
    let row: number[] = [];
    for (let j = 0; j < obj.cols(); j++) {
      row.push(obj.get(i, j));
    }
    m.push(row);
  }
  return m;
};

export const getConstrFnGradientList = (s: State): Matrix => {
  // : Matrix
  const { objFnCache, constrFnCache } = s.params;
  if (!constrFnCache) {
    const newState = genFns(s);
    return getConstrFnGradientList(newState);
  }

  // variable list
  const xs = s.varyingValues;

  // keys (fn names) don't matter
  // console.log(Object.keys(constrFnCache));

  const gradientObjs = Object.values(constrFnCache);

  const res = gradientObjs.map((gradientObj) => {
    return gradientObj.gradf(xs);
  });

  // console.log(res);

  return res;
};

export const putNullspaceBasisVectorsInState = (state: State): State => {
  // jacobian stuff
  const j = getConstrFnGradientList(state);
  if (j.length > 0 && j[0].length > 0) {
    // can't run SVD on empty matrices
    const nspvcs = getNullspaceBasisVectors(j);
    let s = {
      ...state,
      params: {
        ...state.params,
        nullspaceVectors: nspvcs,
      },
    };

    return s; // put the nspacevecs in the state
  }
  return state; // don't modify
};

// update state
export const addVecToVaryingVals = (
  s: State,
  nullSpaceVec: number[],
  scalingFactor: number = 10
): State => {
  const xs = s.varyingValues;
  const v1 = new eig.Matrix(xs);
  const v2 = new eig.Matrix(nullSpaceVec);
  const sum = v1.matAdd(v2.mul(scalingFactor));

  // update the rest of state
  const varyingValues = eigObjToMatrix(sum).flat();

  s.translation = insertVaryings(
    s.translation,
    _.zip(s.varyingPaths, varyingValues.map(differentiable)) as [Path, VarAD][]
  );

  s.varyingValues = varyingValues;
  s = evalShapes(s);
  return s;
};
