declare module "eigen" {
  // these types are already present in the Eigen.js repository on GitHub, but
  // we need to have them here until a new a new version of that library is
  // published to npm; see this comment:
  // https://github.com/BertrandBev/eigen-js/pull/37#issuecomment-1058446844

  import HashMap from "hashmap";

  declare namespace eig {
    class GC {
      static add(...addList: unknown[]): void;
      static pushException(...exceptionList: unknown[]): void;
      static popException(...exceptionList: unknown[]): void;
      static flush(): number;
      static set(ref: unknown, name: string, newObj: unknown): void;
      static initClass(classes: Set<unknown>, Class: unknown): unknown;
      static objects: Set<unknown>;
      static whitelist: HashMap<unknown, number>;
    }

    const ready: Promise<void>;

    class Vector {
      constructor();
      push_back(value: number): void;
      resize(count: number, value: number): void;
      size(): number;
      get(index: number): number | undefined;
      set(index: number, value: number): true;
    }

    class Vector2d {
      constructor();
      push_back(value: Vector): void;
      resize(count: number, value: Vector): void;
      size(): number;
      get(index: number): Vector | undefined;
      set(index: number, value: Vector): true;
    }

    class Complex {
      constructor(re: number, im: number);
      real(): number;
      imag(): number;
    }

    class Matrix {
      constructor(arg0: number, arg1?: number);
      constructor(arg0: number[] | number[][] | Matrix);
      static identity(m: number, n: number): Matrix;
      static ones(m: number, n: number): Matrix;
      static constant(m: number, n: number, x: number): Matrix;
      static random(m: number, n: number): Matrix;
      static diagonal(vector: Matrix): Matrix;
      static fromVector(v: Vector2d): Matrix;
      transpose(): Matrix;
      transposeSelf(): Matrix;
      inverse(): Matrix;
      rows(): number;
      cols(): number;
      norm(): number;
      normSqr(): number;
      l1Norm(): number;
      lInfNorm(): number;
      rank(): number;
      det(): number;
      sum(): number;
      block(i: number, j: number, di: number, dj: number): Matrix;
      setBlock(i: number, j: number, block: Matrix): void;
      mul(s: number): Matrix;
      mulSelf(s: number): Matrix;
      div(s: number): Matrix;
      divSelf(s: number): Matrix;
      matAdd(B: Matrix): Matrix;
      matAddSelf(B: Matrix): Matrix;
      matSub(B: Matrix): Matrix;
      matSubSelf(B: Matrix): Matrix;
      matMul(B: Matrix): Matrix;
      matMulSelf(B: Matrix): Matrix;
      get(i: number, j?: number): number;
      set(i: number, j: number, s: number): void;
      set(i: number, s: number): void;
      hcat(B: Matrix): Matrix;
      vcat(B: Matrix): Matrix;
      print(title: string): void;
      clamp(lo: number, hi: number): Matrix;
      clampSelf(lo: number, hi: number): Matrix;
      length(): number;
      vGet(i: number): number;
      vSet(i: number, s: number): void;
      dot(B: Matrix): number;
    }

    class ComplexDenseMatrix {
      constructor(m: number, n: number);
      constructor(B: ComplexDenseMatrix);
      static identity(m: number, n: number): ComplexDenseMatrix;
      static ones(m: number, n: number): ComplexDenseMatrix;
      static constant(m: number, n: number, x: number): ComplexDenseMatrix;
      static random(m: number, n: number): ComplexDenseMatrix;
      transpose(): ComplexDenseMatrix;
      inverse(): ComplexDenseMatrix;
      conjugate(): ComplexDenseMatrix;
      rows(): number;
      cols(): number;
      norm(): number;
      rank(): number;
      sum(): Complex;
      block(i: number, j: number, di: number, dj: number): ComplexDenseMatrix;
      mul(s: Complex): ComplexDenseMatrix;
      div(s: Complex): ComplexDenseMatrix;
      matAdd(B: ComplexDenseMatrix): ComplexDenseMatrix;
      matSub(B: ComplexDenseMatrix): ComplexDenseMatrix;
      matMul(B: ComplexDenseMatrix): ComplexDenseMatrix;
      get(i: number, j: number): Complex;
      set(i: number, j: number, s: Complex): void;
      hcat(B: ComplexDenseMatrix): ComplexDenseMatrix;
      vcat(B: ComplexDenseMatrix): ComplexDenseMatrix;
      print(title: string): void;
    }

    class TripletVector {
      constructor(capacity: number);
      add(i: number, j: number, x: number): void;
      addBlock(i: number, j: number, mat: Matrix): void;
      addDiag(i: number, j: number, diag: Matrix): void;
    }

    class SparseMatrix {
      constructor(m: number, n: number);
      constructor(m: number, n: number, tripletVector: TripletVector);
      constructor(B: SparseMatrix);
      static identity(m: number, n: number): SparseMatrix;
      static diag(d: Matrix): SparseMatrix;
      static fromTriplets(
        m: number,
        n: number,
        array: [number, number, number][]
      ): SparseMatrix;
      transpose(): SparseMatrix;
      rows(): number;
      cols(): number;
      nonZeros(): number;
      frobeniusNorm(): number;
      block(r0: number, r1: number, c0: number, c1: number): SparseMatrix;
      toDense(): Matrix;
      mul(s: number): SparseMatrix;
      mulSelf(s: number): void;
      div(s: number): SparseMatrix;
      divSelf(s: number): void;
      matAdd(B: SparseMatrix): SparseMatrix;
      matAddSelf(B: SparseMatrix): void;
      matSub(B: SparseMatrix): SparseMatrix;
      matSubSelf(B: SparseMatrix): void;
      matMul(B: SparseMatrix): SparseMatrix;
      get(i: number, j: number): number;
      set(i: number, j: number, s: number): void;
      print(title: string): void;
    }

    type ComputationInfo = unknown;

    interface EigenSolverResult {
      info: ComputationInfo;
      eigenvalues: ComplexDenseMatrix;
      eigenvectors: ComplexDenseMatrix;
    }

    interface CareSolverResult {
      info: ComputationInfo;
      K: Matrix;
      S: Matrix;
    }

    class Solvers {
      static eigenSolve(
        matrix: Matrix,
        computeEigenvectors: boolean
      ): EigenSolverResult;
      static careSolve(
        A: Matrix,
        B: Matrix,
        Q: Matrix,
        R: Matrix
      ): CareSolverResult;
      static solve(
        P: SparseMatrix,
        q: Matrix,
        A: SparseMatrix,
        l: Matrix,
        u: Matrix
      ): Matrix;
    }

    interface CholeskyResult {
      L: Matrix;
    }

    interface LUResult {
      L: Matrix;
      U: Matrix;
      P: Matrix;
      Q: Matrix;
    }

    interface QRResult {
      Q: Matrix;
      R: Matrix;
    }

    interface SVDResult {
      sv: Matrix;
      U: Matrix;
      V: Matrix;
    }

    class Decompositions {
      static cholesky(M: Matrix): CholeskyResult;
      static lu(M: Matrix): LUResult;
      static qr(M: Matrix): QRResult;
      static svd(M: Matrix, thin: boolean): SVDResult;
    }

    class QuadProgSolver {
      static solve(
        P: SparseMatrix,
        q: Matrix,
        A: SparseMatrix,
        l: Matrix,
        u: Matrix
      ): Matrix;
      static solveSparse(): void;
      static solveBasic(): void;
    }

    class Random {
      static normal(mean: Matrix, cov: Matrix, samples: number): Matrix;
    }
  }

  export default eig;
}
