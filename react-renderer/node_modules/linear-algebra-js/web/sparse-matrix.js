"use strict";

class SparseMatrix {
	/**
	 * This class represents a m by n real matrix where only nonzero entries
	 * are stored explicitly. Do not create a SparseMatrix from its constructor,
	 * instead use static factory methods such as fromTriplet, identity and diag.
	 * @constructor SparseMatrix
	 * @example
	 * let T = new Triplet(100, 100);
	 * T.addEntry(3.4, 11, 43);
	 * T.addEntry(6.4, 99, 99);
	 * let A = SparseMatrix.fromTriplet(T);
	 *
	 * let B = SparseMatrix.identity(10, 10);
	 *
	 * let d = DenseMatrix.ones(100, 1);
	 * let C = SparseMatrix.diag(d);
	 */
	constructor(data) {
		this.data = data;
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this sparse matrix.
	 * @ignore
	 * @method SparseMatrix#delete
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * Initializes a sparse matrix from a {@link Triplet} object.
	 * @method SparseMatrix.fromTriplet
	 * @param {Triplet} T A triplet object containing only the nonzero entries that
	 * need to be stored in this sparse matrix.
	 * @returns {SparseMatrix}
	 */
	static fromTriplet(T) {
		return new SparseMatrix(new Module.SparseMatrix(T.data));
	}

	/**
	 * Initializes a m by n sparse identity matrix.
	 * @method SparseMatrix.identity
	 * @param {number} m The number of rows in this sparse matrix.
	 * @param {number} n The number of columns in this sparse matrix.
	 * @returns {SparseMatrix}
	 */
	static identity(m, n) {
		return new SparseMatrix(Module.SparseMatrix.identity(m, n));
	}

	/**
	 * Initializes a sparse diagonal matrix.
	 * @method SparseMatrix.diag
	 * @param {DenseMatrix} d The dense vector (d.nCols() == 1) used to initialize
	 * this sparse diagonal matrix.
	 * @returns {SparseMatrix}
	 */
	static diag(d) {
		return new SparseMatrix(Module.SparseMatrix.diag(d.data));
	}

	/**
	 * Returns the transpose of this sparse matrix.
	 * @method SparseMatrix#transpose
	 * @returns {SparseMatrix}
	 */
	transpose() {
		return new SparseMatrix(this.data.transpose());
	}

	/**
	 * Returns the inverse of this diagonal sparse matrix.
	 * @method SparseMatrix#invertDiagonal
	 * @returns {SparseMatrix}
	 */
	invertDiagonal() {
		let N = this.nRows();
		let X = this.timesDense(DenseMatrix.ones(N, 1));
		let T = new Triplet(N, N);
		for (let i = 0; i < N; i++) {
			T.addEntry(1 / X.get(i, 0), i, i);
		}

		return SparseMatrix.fromTriplet(T);
	}

	/**
	 * Returns the number of rows in this sparse matrix.
	 * @method SparseMatrix#nRows
	 * @returns {number}
	 */
	nRows() {
		return this.data.nRows();
	}

	/**
	 * Returns the number of columns in this sparse matrix.
	 * @method SparseMatrix#nCols
	 * @returns {number}
	 */
	nCols() {
		return this.data.nCols();
	}

	/**
	 * Returns the number of nonzero entries in this sparse matrix.
	 * @method SparseMatrix#nnz
	 * @returns {number}
	 */
	nnz() {
		return this.data.nnz();
	}

	/**
	 * Computes the frobenius norm of this sparse matrix.
	 * @method SparseMatrix#frobeniusNorm
	 * @returns {number}
	 */
	frobeniusNorm() {
		return this.data.frobeniusNorm();
	}

	/**
	 * Extracts a sparse sub-matrix in the range [r0, r1) x [c0, c1), i.e., a matrix
	 * of size (r1 - r0) x (c1 - c0) starting at indices (r0, c0).
	 * @method SparseMatrix#subMatrix
	 * @param {number} r0 The start row index.
	 * @param {number} r1 The end row index (not included).
	 * @param {number} c0 The start column index.
	 * @param {number} c1 The end column index (not included).
	 * @returns {SparseMatrix}
	 */
	subMatrix(r0, r1, c0, c1) {
		return new SparseMatrix(this.data.subMatrix(r0, r1, c0, c1));
	}

	/**
	 * Returns a sparse {@link Cholesky} factorization of this sparse matrix.
	 * @method SparseMatrix#chol
	 * @returns {Cholesky}
	 */
	chol() {
		return new Cholesky(this.data.chol());
	}

	/**
	 * Returns a sparse {@link LU} factorization of this sparse matrix.
	 * @method SparseMatrix#lu
	 * @returns {LU}
	 */
	lu() {
		return new LU(this.data.lu());
	}

	/**
	 * Returns a sparse {@link QR} factorization of this sparse matrix.
	 * @method SparseMatrix#qr
	 * @returns {QR}
	 */
	qr() {
		return new QR(this.data.qr());
	}

	/**
	 * Returns a dense copy of this sparse matrix.
	 * @method SparseMatrix#toDense
	 * @returns {DenseMatrix}
	 */
	toDense() {
		return new DenseMatrix(this.data.toDense());
	}

	/**
	 * A += B
	 * @method SparseMatrix#incrementBy
	 * @param {SparseMatrix} B The sparse matrix added to this sparse matrix.
	 */
	incrementBy(B) {
		this.data.incrementBy(B.data);
	}

	/**
	 * A -= B
	 * @method SparseMatrix#decrementBy
	 * @param {SparseMatrix} B The sparse matrix subtracted from this sparse matrix.
	 */
	decrementBy(B) {
		this.data.decrementBy(B.data);
	}

	/**
	 * A *= s
	 * @method SparseMatrix#scaleBy
	 * @param {number} s The number this sparse matrix is scaled by.
	 */
	scaleBy(s) {
		this.data.scaleBy(s);
	}

	/**
	 * Returns A + B
	 * @method SparseMatrix#plus
	 * @param {SparseMatrix} B The sparse matrix added to this sparse matrix.
	 * @returns {SparseMatrix}
	 */
	plus(B) {
		return new SparseMatrix(this.data.plus(B.data));
	}

	/**
	 * Returns A - B
	 * @method SparseMatrix#minus
	 * @param {SparseMatrix} B The sparse matrix subtracted from this sparse matrix.
	 * @returns {SparseMatrix}
	 */
	minus(B) {
		return new SparseMatrix(this.data.minus(B.data));
	}

	/**
	 * Returns A * s
	 * @method SparseMatrix#timesReal
	 * @param {number} s The number this sparse matrix is multiplied by.
	 * @returns {SparseMatrix}
	 */
	timesReal(s) {
		return new SparseMatrix(this.data.timesReal(s));
	}

	/**
	 * Returns A * X
	 * @method SparseMatrix#timesDense
	 * @param {DenseMatrix} X The dense matrix this sparse matrix is multiplied by.
	 * @returns {DenseMatrix}
	 */
	timesDense(X) {
		return new DenseMatrix(this.data.timesDense(X.data));
	}

	/**
	 * Returns A * B
	 * @method SparseMatrix#timesSparse
	 * @param {SparseMatrix} B The sparse matrix this sparse matrix is multiplied by.
	 * @returns {SparseMatrix}
	 */
	timesSparse(B) {
		return new SparseMatrix(this.data.timesSparse(B.data));
	}
}

class Triplet {
	/**
	 * This class represents a small structure to hold nonzero entries in a {@link SparseMatrix}.
	 * Each entry is a triplet of a value and the (i, j)th indices, i.e., (x, i, j).
	 * @constructor Triplet
	 * @param {number} m The number of rows in the sparse matrix that will be initialized
	 * from this triplet.
	 * @param {number} n The number of columns in the sparse matrix that will be initialized
	 * from this triplet.
	 * @example
	 * let T = new Triplet(100, 100);
	 * T.addEntry(3.4, 11, 43);
	 * T.addEntry(6.4, 99, 99);
	 *
	 * let A = SparseMatrix.fromTriplet(T);
	 */
	constructor(m, n) {
		this.data = new Module.Triplet(m, n);
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this sparse matrix.
	 * @method Triplet#delete
	 * @ignore
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * A(i, j) += x
	 * @method Triplet#addEntry
	 * @param {number} x The value of the nonzero entry being inserted into this triplet.
	 * @param {number} i The ith row of the sparse matrix that will be initialized
	 * from this triplet.
	 * @param {number} j The jth column of the sparse matrix that will be initialized
	 * from this triplet.
	 */
	addEntry(x, i, j) {
		this.data.addEntry(i, j, x);
	}
}

class Cholesky {
	/**
	 * This class represents a Choleksy LL^T factorization of a square positive definite
	 * {@link SparseMatrix}. The factorization is computed on the first call to solvePositiveDefinite,
	 * and is reused in subsequent calls to solvePositiveDefinite (e.g. when only the
	 * right hand side b of the linear system Ax = b changes) unless the sparse matrix
	 * itself is altered through operations such as *=, += and -=. Do not use the constructor
	 * to initialize this class, instead access the Choleksy factorization of a sparse
	 * matrix directly from the matrix itself.
	 * @constructor Cholesky
	 * @example
	 * // solve the linear system Ax = b, where A is a square positive definite sparse matrix
	 * let A = SparseMatrix.identity(5, 5);
	 * let b = DenseMatrix.ones(5, 1);
	 *
	 * let llt = A.chol();
	 * let x = llt.solvePositiveDefinite(b);
	 *
	 * b.scaleBy(5);
	 * x = llt.solvePositiveDefinite(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a square positive definite sparse matrix.
	 * @method Cholesky#solvePositiveDefinite
	 * @param {DenseMatrix} b The dense right hand side of the linear system Ax = b.
	 * @returns {DenseMatrix}
	 */
	solvePositiveDefinite(b) {
		return new DenseMatrix(this.data.solvePositiveDefinite(b.data));
	}
}

class LU {
	/**
	 * This class represents a LU factorization of a square {@link SparseMatrix}. The factorization
	 * is computed on the first call to solveSquare, and is reused in subsequent calls
	 * to solveSquare (e.g. when only the right hand side b of the linear system Ax = b
	 * changes) unless the sparse matrix itself is altered through operations such as
	 * *=, += and -=. Do not use the constructor to initialize this class, instead
	 * access the LU factorization of a sparse matrix directly from the matrix itself.
	 * @constructor LU
	 * @example
	 * // solve the linear system Ax = b, where A is a square sparse matrix
	 * let A = SparseMatrix.identity(5, 5);
	 * let b = DenseMatrix.ones(5, 1);
	 *
	 * let lu = A.lu();
	 * let x = lu.solveSquare(b);
	 *
	 * b.scaleBy(5);
	 * x = lu.solveSquare(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a square sparse matrix.
	 * @method LU#solveSquare
	 * @param {DenseMatrix} b The dense right hand side of the linear system Ax = b.
	 * @returns {DenseMatrix}
	 */
	solveSquare(b) {
		return new DenseMatrix(this.data.solveSquare(b.data));
	}
}

class QR {
	/**
	 * This class represents a QR factorization of a rectangular {@link SparseMatrix}.
	 * The factorization is computed on the first call to solve, and is reused in
	 * subsequent calls to solve (e.g. when only the right hand side b of the linear
	 * system Ax = b changes) unless the sparse matrix itself is altered through
	 * operations such as *=, += and -=. Do not use the constructor to initialize
	 * this class, instead access the QR factorization of a sparse matrix directly
	 * from the matrix itself.
	 * @constructor QR
	 * @example
	 * // solve the linear system Ax = b, where A is a rectangular sparse matrix
	 * let A = SparseMatrix.identity(5, 5);
	 * let b = DenseMatrix.ones(5, 1);
	 *
	 * let qr = A.qr();
	 * let x = qr.solve(b);
	 *
	 * b.scaleBy(5);
	 * x = qr.solve(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a rectangular sparse matrix.
	 * @method QR#solve
	 * @param {DenseMatrix} b The dense right hand side of the linear system Ax = b.
	 * @returns {DenseMatrix}
	 */
	solve(b) {
		return new DenseMatrix(this.data.solve(b.data));
	}
}
