import { numbered } from "compiler/Style";
import eig from "eigen";
import { State } from "types/state";
import { genFns } from "./Optimizer";

// internal typing
type Matrix = number[][];

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

  // eig.GC.flush();

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
