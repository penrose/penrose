"use strict";

class ComplexSparseMatrix {
	/**
	 * This class represents a m by n complex matrix where only nonzero entries
	 * are stored explicitly. Do not create a ComplexSparseMatrix from its constructor,
	 * instead use static factory methods such as fromTriplet, identity and diag.
	 * @constructor ComplexSparseMatrix
	 * @example
	 * let T = new ComplexTriplet(100, 100);
	 * T.addEntry(new Complex(3.4, 6.4), 11, 43);
	 * T.addEntry(new Complex(6.4, 3.4), 99, 99);
	 * let A = ComplexSparseMatrix.fromTriplet(T);
	 *
	 * let B = ComplexSparseMatrix.identity(10, 10);
	 *
	 * let d = ComplexDenseMatrix.ones(100, 1);
	 * let C = ComplexSparseMatrix.diag(d);
	 */
	constructor(data) {
		this.data = data;
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this complex sparse matrix.
	 * @ignore
	 * @method ComplexSparseMatrix#delete
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * Initializes a complex sparse matrix from a {@link ComplexTriplet} object.
	 * @method ComplexSparseMatrix.fromTriplet
	 * @param {ComplexTriplet} T A complex triplet object containing only the nonzero
	 * entries that need to be stored in this complex sparse matrix.
	 * @returns {ComplexSparseMatrix}
	 */
	static fromTriplet(T) {
		return new ComplexSparseMatrix(new Module.ComplexSparseMatrix(T.data));
	}

	/**
	 * Initializes a m by n complex sparse identity matrix.
	 * @method ComplexSparseMatrix.identity
	 * @param {number} m The number of rows in this complex sparse matrix.
	 * @param {number} n The number of columns in this complex sparse matrix.
	 * @returns {ComplexSparseMatrix}
	 */
	static identity(m, n) {
		return new ComplexSparseMatrix(Module.ComplexSparseMatrix.identity(m, n));
	}

	/**
	 * Initializes a complex sparse diagonal matrix.
	 * @method ComplexSparseMatrix.diag
	 * @param {ComplexDenseMatrix} d The complex dense vector (d.nCols() == 1) used
	 * to initialize this complex sparse diagonal matrix.
	 * @returns {ComplexSparseMatrix}
	 */
	static diag(d) {
		return new ComplexSparseMatrix(Module.ComplexSparseMatrix.diag(d.data));
	}

	/**
	 * Returns the transpose of this complex sparse matrix.
	 * @method ComplexSparseMatrix#transpose
	 * @returns {ComplexSparseMatrix}
	 */
	transpose() {
		return new ComplexSparseMatrix(this.data.transpose());
	}

	/**
	 * Returns the inverse of this diagonal complex sparse matrix.
	 * @method ComplexSparseMatrix#invertDiagonal
	 * @returns {ComplexSparseMatrix}
	 */
	invertDiagonal() {
		let N = this.nRows();
		let X = this.timesDense(ComplexDenseMatrix.ones(N, 1));
		let T = new ComplexTriplet(N, N);
		for (let i = 0; i < N; i++) {
			T.addEntry(X.get(i, 0).inverse(), i, i);
		}

		return ComplexSparseMatrix.fromTriplet(T);
	}

	/**
	 * Returns the conjugate of this complex sparse matrix.
	 * @method ComplexSparseMatrix#conjugate
	 * @returns {ComplexSparseMatrix}
	 */
	conjugate() {
		return new ComplexSparseMatrix(this.data.conjugate());
	}

	/**
	 * Returns the number of rows in this complex sparse matrix.
	 * @method ComplexSparseMatrix#nRows
	 * @returns {number}
	 */
	nRows() {
		return this.data.nRows();
	}

	/**
	 * Returns the number of columns in this complex sparse matrix.
	 * @method ComplexSparseMatrix#nCols
	 * @returns {number}
	 */
	nCols() {
		return this.data.nCols();
	}

	/**
	 * Returns the number of nonzero entries in this complex sparse matrix.
	 * @method ComplexSparseMatrix#nnz
	 * @returns {number}
	 */
	nnz() {
		return this.data.nnz();
	}

	/**
	 * Computes the frobenius norm of this complex sparse matrix.
	 * @method ComplexSparseMatrix#frobeniusNorm
	 * @returns {number}
	 */
	frobeniusNorm() {
		return this.data.frobeniusNorm();
	}

	/**
	 * Extracts a complex sparse sub-matrix in the range [r0, r1) x [c0, c1), i.e.,
	 * a matrix of size (r1 - r0) x (c1 - c0) starting at indices (r0, c0).
	 * @method ComplexSparseMatrix#subMatrix
	 * @param {number} r0 The start row index.
	 * @param {number} r1 The end row index (not included).
	 * @param {number} c0 The start column index.
	 * @param {number} c1 The end column index (not included).
	 * @returns {ComplexSparseMatrix}
	 */
	subMatrix(r0, r1, c0, c1) {
		return new ComplexSparseMatrix(this.data.subMatrix(r0, r1, c0, c1));
	}

	/**
	 * Returns a sparse {@link ComplexCholesky} factorization of this complex sparse matrix.
	 * @method ComplexSparseMatrix#chol
	 * @returns {ComplexCholesky}
	 */
	chol() {
		return new ComplexCholesky(this.data.chol());
	}

	/**
	 * Returns a sparse {@link ComplexLU} factorization of this complex sparse matrix.
	 * @method ComplexSparseMatrix#lu
	 * @returns {ComplexLU}
	 */
	lu() {
		return new ComplexLU(this.data.lu());
	}

	/**
	 * Returns a sparse {@link ComplexQR} factorization of this complex sparse matrix.
	 * @method ComplexSparseMatrix#qr
	 * @returns {ComplexQR}
	 */
	qr() {
		return new ComplexQR(this.data.qr());
	}

	/**
	 * Returns a dense copy of this complex sparse matrix.
	 * @method ComplexSparseMatrix#toDense
	 * @returns {ComplexDenseMatrix}
	 */
	toDense() {
		return new ComplexDenseMatrix(this.data.toDense());
	}

	/**
	 * A += B
	 * @method ComplexSparseMatrix#incrementBy
	 * @param {ComplexSparseMatrix} B The complex sparse matrix added to this complex
	 * sparse matrix.
	 */
	incrementBy(B) {
		this.data.incrementBy(B.data);
	}

	/**
	 * A -= B
	 * @method ComplexSparseMatrix#decrementBy
	 * @param {ComplexSparseMatrix} B The complex sparse matrix subtracted from
	 * this complex sparse matrix.
	 */
	decrementBy(B) {
		this.data.decrementBy(B.data);
	}

	/**
	 * A *= s
	 * @method ComplexSparseMatrix#scaleBy
	 * @param {Complex} s The complex number this complex sparse matrix is scaled by.
	 */
	scaleBy(s) {
		this.data.scaleBy(s.data);
	}

	/**
	 * Returns A + B
	 * @method ComplexSparseMatrix#plus
	 * @param {ComplexSparseMatrix} B The complex sparse matrix added to this complex
	 * sparse matrix.
	 * @returns {ComplexSparseMatrix}
	 */
	plus(B) {
		return new ComplexSparseMatrix(this.data.plus(B.data));
	}

	/**
	 * Returns A - B
	 * @method ComplexSparseMatrix#minus
	 * @param {ComplexSparseMatrix} B The complex sparse matrix subtracted from this
	 * complex sparse matrix.
	 * @returns {ComplexSparseMatrix}
	 */
	minus(B) {
		return new ComplexSparseMatrix(this.data.minus(B.data));
	}

	/**
	 * Returns A * s
	 * @method ComplexSparseMatrix#timesComplex
	 * @param {Complex} s The complex number this complex sparse matrix is multiplied by.
	 * @returns {ComplexSparseMatrix}
	 */
	timesComplex(s) {
		return new ComplexSparseMatrix(this.data.timesComplex(s.data));
	}

	/**
	 * Returns A * X
	 * @method ComplexSparseMatrix#timesDense
	 * @param {ComplexDenseMatrix} X The complex dense matrix this complex sparse matrix
	 * is multiplied by.
	 * @returns {ComplexDenseMatrix}
	 */
	timesDense(X) {
		return new ComplexDenseMatrix(this.data.timesDense(X.data));
	}

	/**
	 * Returns A * B
	 * @method ComplexSparseMatrix#timesSparse
	 * @param {ComplexSparseMatrix} B The complex sparse matrix this complex sparse matrix
	 * is multiplied by.
	 * @returns {ComplexSparseMatrix}
	 */
	timesSparse(B) {
		return new ComplexSparseMatrix(this.data.timesSparse(B.data));
	}
}

class ComplexTriplet {
	/**
	 * This class represents a small structure to hold nonzero entries in a {@link ComplexSparseMatrix}.
	 * Each entry is a triplet of a complex value and the (i, j)th indices, i.e., (x, i, j).
	 * @constructor ComplexTriplet
	 * @param {number} m The number of rows in the complex sparse matrix that will be
	 * initialized from this complex triplet.
	 * @param {number} n The number of columns in the complex sparse matrix that will be
	 * initialized from this complex triplet.
	 * @example
	 * let T = new ComplexTriplet(100, 100);
	 * T.addEntry(new Complex(3.4, 6.4), 11, 43);
	 * T.addEntry(new Complex(6.4, 3.4), 99, 99);
	 *
	 * let A = ComplexSparseMatrix.fromTriplet(T);
	 */
	constructor(m, n) {
		this.data = new Module.ComplexTriplet(m, n);
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this sparse matrix.
	 * @ignore
	 * @method ComplexTriplet#delete
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * A(i, j) += x
	 * @method ComplexTriplet#addEntry
	 * @param {number} x The value of the nonzero entry being inserted into this
	 * complex triplet.
	 * @param {number} i The ith row of the complex sparse matrix that will be initialized
	 * from this complex triplet.
	 * @param {number} j The jth column of the complex sparse matrix that will be
	 * initialized from this complex triplet.
	 */
	addEntry(x, i, j) {
		this.data.addEntry(i, j, x.data);
	}
}

class ComplexCholesky {
	/**
	 * This class represents a complex Choleksy LL^T factorization of a square and
	 * positive definite {@link ComplexSparseMatrix}. The factorization is computed on the
	 * first call to solvePositiveDefinite, and is reused in subsequent calls to
	 * solvePositiveDefinite (e.g. when only the right hand side b of the linear system
	 * Ax = b changes) unless the complex sparse matrix itself is altered through
	 * operations such as *=, += and -=. Do not use the constructor to initialize
	 * this class, instead access the complex Choleksy factorization of a complex
	 * sparse matrix directly from the matrix itself.
	 * @constructor ComplexCholesky
	 * @example
	 * // solve the linear system Ax = b, where A is a square
	 * // and complex positive definite sparse matrix
	 * let A = ComplexSparseMatrix.identity(5, 5);
	 * let b = ComplexDenseMatrix.ones(5, 1);
	 *
	 * let llt = A.chol();
	 * let x = llt.solvePositiveDefinite(b);
	 *
	 * b.scaleBy(new Complex(5, 0));
	 * x = llt.solvePositiveDefinite(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a square and complex positive
	 * definite sparse matrix.
	 * @method ComplexCholesky#solvePositiveDefinite
	 * @param {ComplexDenseMatrix} b The complex dense right hand side of the linear system Ax = b.
	 * @returns {ComplexDenseMatrix}
	 */
	solvePositiveDefinite(b) {
		return new ComplexDenseMatrix(this.data.solvePositiveDefinite(b.data));
	}
}

class ComplexLU {
	/**
	 * This class represents a complex LU factorization of a square {@link ComplexSparseMatrix}.
	 * The factorization is computed on the first call to solveSquare, and is reused
	 * in subsequent calls to solveSquare (e.g. when only the right hand side b
	 * of the linear system Ax = b changes) unless the complex sparse matrix itself
	 * is altered through operations such as *=, += and -=. Do not use the constructor
	 * to initialize this class, instead access the complex LU factorization
	 * of a complex sparse matrix directly from the matrix itself.
	 * @constructor ComplexLU
	 * @example
	 * // solve the linear system Ax = b, where A is a square and complex sparse matrix
	 * let A = ComplexSparseMatrix.identity(5, 5);
	 * let b = ComplexDenseMatrix.ones(5, 1);
	 *
	 * let lu = A.lu();
	 * let x = lu.solveSquare(b);
	 *
	 * b.scaleBy(new Complex(5, 0));
	 * x = lu.solveSquare(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a square and complex sparse matrix.
	 * @method ComplexLU#solveSquare
	 * @param {ComplexDenseMatrix} b The complex dense right hand side of the linear system Ax = b.
	 * @returns {ComplexDenseMatrix}
	 */
	solveSquare(b) {
		return new ComplexDenseMatrix(this.data.solveSquare(b.data));
	}
}

class ComplexQR {
	/**
	 * This class represents a complex QR factorization of a rectangular {@link ComplexSparseMatrix}.
	 * The factorization is computed on the first call to solve, and is reused in
	 * subsequent calls to solve (e.g. when only the right hand side b of the linear
	 * system Ax = b changes) unless the complex sparse matrix itself is altered
	 * through operations such as *=, += and -=. Do not use the constructor to initialize
	 * this class, instead access the complex QR factorization of a complex sparse
	 * matrix directly from the matrix itself.
	 * @constructor ComplexQR
	 * @example
	 * // solve the linear system Ax = b, where A is a rectangular and complex sparse matrix
	 * let A = ComplexSparseMatrix.identity(5, 5);
	 * let b = ComplexDenseMatrix.ones(5, 1);
	 *
	 * let qr = A.qr();
	 * let x = qr.solve(b);
	 *
	 * b.scaleBy(Complex(5, 0));
	 * x = qr.solve(b); // factorization is reused
	 */
	constructor(data) {
		this.data = data;
	}

	/**
	 * Solves the linear system Ax = b, where A is a rectangular and complex sparse matrix.
	 * @method ComplexQR#solve
	 * @param {ComplexDenseMatrix} b The dense right hand side of the linear system Ax = b.
	 * @returns {ComplexDenseMatrix}
	 */
	solve(b) {
		return new ComplexDenseMatrix(this.data.solve(b.data));
	}
}
