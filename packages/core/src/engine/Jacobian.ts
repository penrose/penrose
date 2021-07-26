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

// returns nspacevectors as rows of the output matrix

// SVD gives more vectors than QR?
// ....

export const getNullspaceBasisVectorsSVD = (jacobian: Matrix): Matrix => {
  const J = new eig.Matrix(jacobian);

  // induced matrix, we use to find nullspace vecs
  const A = J.transpose().matMul(J);
  // try J^T J vs just J?

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

  /*
  evecsAsRows.map(
    (vec, index) => {
      const v = new eig.Matrix(vec);
      const res = J.matMul(v);
      res.print(`svd ${index}`)
    }
  ) */

  const nullspaceEvecs = evecsAsRows.filter((evec, index) => {
    return zeroSingularValueIndexes.includes(index);
  });
  return nullspaceEvecs;
};

export const getNullspaceBasisVectorsQR = (jacobian: Matrix): Matrix => {
  const J = new eig.Matrix(jacobian);

  // J.print('J')

  const qr = eig.Decompositions.qr(J.transpose());
  // how do I figure out which ones are the nullspace vectors?

  // I want the cols, as rows to be easily parseable
  const qCols = eigObjToMatrix(qr.Q.transpose());

  // qr.Q.print('q');
  // qr.R.print('r');

  // console.log(J.rows())
  // console.log(J.cols())

  // debugging

  /*
  qCols.map(
    (vec, index) => {
      const v = new eig.Matrix(vec);
      const res = J.matMul(v);
      res.print(`qr ${index}`)
    }
  )
  */

  // return qCols;

  return qCols.filter((vec, index) => {
    const v = new eig.Matrix(vec);
    const res = J.matMul(v);
    const resNorm = Math.sqrt(res.dot(res));
    return resNorm < 10e-6;
  });

  // get columns m+1 to n.
  // if J is mxn, then J is nxm, and
  // columns m+1 ... n of Q are the nullspace basis of J

  // I cannot assume this, since J might not be full (row) rank.
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

  const gradientObjs = Object.values(constrFnCache).concat(
    Object.values(objFnCache)
  );

  const res = gradientObjs.map((gradientObj) => {
    return gradientObj.gradf(xs);
  });

  return res;

  // should I also use the objective fns?
  const gradientObjs2 = Object.values(objFnCache);
  const res2 = gradientObjs2.map((gradientObj) => {
    return gradientObj.gradf(xs);
  });

  return res.concat(res2);
};

export const putNullspaceBasisVectorsInState = (state: State): State => {
  // jacobian stuff
  const j = getConstrFnGradientList(state);
  if (j.length > 0 && j[0].length > 0) {
    // can't run SVD on empty matrices
    const nspvcs = getNullspaceBasisVectorsQR(j);
    const zeroWeights = nspvcs.map((vec) => {
      return 0;
    });
    // getNullspaceBasisVectorsQR(j);
    const s = {
      ...state,
      params: {
        ...state.params,
        nullspaceVectors: nspvcs,
        nullspaceVectorWeights: zeroWeights,
      },
    };

    return s; // put the nspacevecs in the state
  }
  return state; // don't modify
};

// update state
//
const addVecToVaryingVals = (
  s: State,
  nullSpaceVec: number[],
  scalingFactor: number = 10
): [State, State] => {
  const xs = s.varyingValues;
  const v1 = new eig.Matrix(xs);
  const v2 = new eig.Matrix(nullSpaceVec);
  const sum = v1.matAdd(v2.mul(scalingFactor));

  // update the rest of state
  const varyingValues = eigObjToMatrix(sum).flat();

  let newState = { ...s };

  newState.translation = insertVaryings(
    newState.translation,
    _.zip(newState.varyingPaths, varyingValues.map(differentiable)) as [
      Path,
      VarAD
    ][]
  );

  newState.varyingValues = varyingValues;
  newState = evalShapes(newState);
  return [newState, s];
};

export const getNewVaryingVals = (
  s: State,
  nullSpaceVec: number[],
  scalingFactor: number
): number[] => {
  const xs = s.varyingValues;
  const v1 = new eig.Matrix(xs);
  const v2 = new eig.Matrix(nullSpaceVec);
  const sum = v1.matAdd(v2.mul(scalingFactor));

  const varyingValues = eigObjToMatrix(sum).flat();
  return varyingValues;
};

export const addWeightedVecs = (
  v1: number[],
  w1: number,
  v2: number[],
  w2: number
): number[] => {
  console.log(v1, v2);
  const vecObj1 = new eig.Matrix(v1);
  const vecObj2 = new eig.Matrix(v2);
  const sum = vecObj1.mul(w1).matAdd(vecObj2.mul(w2));
  const res = eigObjToMatrix(sum).flat();
  // console.log('res', res)
  return res;
};

export const updateStateVaryingVals = (
  s: State,
  varyingValues: number[]
): State => {
  let newState = { ...s };

  newState.translation = insertVaryings(
    newState.translation,
    _.zip(newState.varyingPaths, varyingValues.map(differentiable)) as [
      Path,
      VarAD
    ][]
  );

  newState.varyingValues = varyingValues;
  newState = evalShapes(newState);
  return newState;
};
