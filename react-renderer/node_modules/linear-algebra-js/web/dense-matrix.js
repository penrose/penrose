"use strict";

class DenseMatrix {
	/**
	 * This class represents a real m by n real matrix where every entry, including
	 * zero-valued entries, is stored explicitly. Do not create a DenseMatrix
	 * from its constructor, instead use static factory methods such as zeros,
	 * identity, ones, constant and random.
	 * @constructor DenseMatrix
	 * @example
	 * let A = DenseMatrix.zeros(20, 5);
	 * let B = DenseMatrix.identity(10, 10);
	 * let C = DenseMatrix.ones(100, 1);
	 * let D = DenseMatrix.constant(4.6, 5, 5);
	 * let E = DenseMatrix.random(5, 20);
	 */
	constructor(data) {
		this.data = data;
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this dense matrix.
	 * @ignore
	 * @method DenseMatrix#delete
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * Initializes a m by n matrix of zeros.
	 * @method DenseMatrix.zeros
	 * @param {number} m The number of rows in this dense matrix.
	 * @param {number} n The number of columns in this dense matrix.
	 * @returns {DenseMatrix}
	 */
	static zeros(m, n = 1) {
		return new DenseMatrix(new Module.DenseMatrix(m, n));
	}

	/**
	 * Initializes a m by n identity matrix.
	 * @method DenseMatrix.identity
	 * @param {number} m The number of rows in this dense matrix.
	 * @param {number} n The number of columns in this dense matrix.
	 * @returns {DenseMatrix}
	 */
	static identity(m, n = 1) {
		return new DenseMatrix(Module.DenseMatrix.identity(m, n));
	}

	/**
	 * Initializes a m by n matrix of ones.
	 * @method DenseMatrix.ones
	 * @param {number} m The number of rows in this dense matrix.
	 * @param {number} n The number of columns in this dense matrix.
	 * @returns {DenseMatrix}
	 */
	static ones(m, n = 1) {
		return new DenseMatrix(Module.DenseMatrix.ones(m, n));
	}

	/**
	 * Initializes a m by n constant matrix.
	 * @method DenseMatrix.constant
	 * @param {number} x The constant value stored in every entry of this dense matrix.
	 * @param {number} m The number of rows in this dense matrix.
	 * @param {number} n The number of columns in this dense matrix.
	 * @returns {DenseMatrix}
	 */
	static constant(x, m, n = 1) {
		return new DenseMatrix(Module.DenseMatrix.constant(m, n, x));
	}

	/**
	 * Initializes a m by n random matrix.
	 * @method DenseMatrix.random
	 * @param {number} m The number of rows in this dense matrix.
	 * @param {number} n The number of columns in this dense matrix.
	 * @returns {DenseMatrix}
	 */
	static random(m, n = 1) {
		return new DenseMatrix(Module.DenseMatrix.random(m, n));
	}

	/**
	 * Returns the transpose of this dense matrix.
	 * @method DenseMatrix#transpose
	 * @returns {DenseMatrix}
	 */
	transpose() {
		return new DenseMatrix(this.data.transpose());
	}

	/**
	 * Returns the number of rows in this dense matrix.
	 * @method DenseMatrix#nRows
	 * @returns {number}
	 */
	nRows() {
		return this.data.nRows();
	}

	/**
	 * Returns the number of columns in this dense matrix.
	 * @method DenseMatrix#nCols
	 * @returns {number}
	 */
	nCols() {
		return this.data.nCols();
	}

	/**
	 * Computes the lInfinity, l1 or l2 norm of this dense matrix.
	 * @method DenseMatrix#norm
	 * @param {number} n Computes the lInfinity norm if n = 0, l1 norm if n = 1
	 * and l2 norm if n = 2.
	 * @returns {number}
	 */
	norm(n = 2) {
		return this.data.norm(n);
	}

	/**
	 * Returns the rank of this dense matrix.
	 * @method DenseMatrix#rank
	 * @returns {number}
	 */
	rank() {
		return this.data.rank();
	}

	/**
	 * Sums all the entries in this dense matrix.
	 * @method DenseMatrix#sum
	 * @returns {number}
	 */
	sum() {
		return this.data.sum();
	}

	/**
	 * Extracts a sub-matrix in the range [r0, r1) x [c0, c1), i.e., a matrix
	 * of size (r1 - r0) x (c1 - c0) starting at indices (r0, c0).
	 * @method DenseMatrix#subMatrix
	 * @param {number} r0 The start row index.
	 * @param {number} r1 The end row index (not included).
	 * @param {number} c0 The start column index.
	 * @param {number} c1 The end column index (not included).
	 * @returns {DenseMatrix}
	 */
	subMatrix(r0, r1, c0 = 0, c1 = 1) {
		return new DenseMatrix(this.data.subMatrix(r0, r1, c0, c1));
	}

	/**
	 * A += B
	 * @method DenseMatrix#incrementBy
	 * @param {DenseMatrix} B The dense matrix added to this dense matrix.
	 */
	incrementBy(B) {
		this.data.incrementBy(B.data);
	}

	/**
	 * A -= B
	 * @method DenseMatrix#decrementBy
	 * @param {DenseMatrix} B The dense matrix subtracted from this dense matrix.
	 */
	decrementBy(B) {
		this.data.decrementBy(B.data);
	}

	/**
	 * A *= s
	 * @method DenseMatrix#scaleBy
	 * @param {number} s The number this dense matrix is scaled by.
	 */
	scaleBy(s) {
		this.data.scaleBy(s);
	}

	/**
	 * Returns A + B
	 * @method DenseMatrix#plus
	 * @param {DenseMatrix} B The dense matrix added to this dense matrix.
	 * @returns {DenseMatrix}
	 */
	plus(B) {
		return new DenseMatrix(this.data.plus(B.data));
	}

	/**
	 * Returns A - B
	 * @method DenseMatrix#minus
	 * @param {DenseMatrix} B The dense matrix subtracted from this dense matrix.
	 * @returns {DenseMatrix}
	 */
	minus(B) {
		return new DenseMatrix(this.data.minus(B.data));
	}

	/**
	 * Returns A * s
	 * @method DenseMatrix#timesReal
	 * @param {number} s The number this dense matrix is multiplied by.
	 * @returns {DenseMatrix}
	 */
	timesReal(s) {
		return new DenseMatrix(this.data.timesReal(s));
	}

	/**
	 * Returns A * B
	 * @method DenseMatrix#timesDense
	 * @param {DenseMatrix} B The dense matrix this dense matrix is multiplied by.
	 * @returns {DenseMatrix}
	 */
	timesDense(B) {
		return new DenseMatrix(this.data.timesDense(B.data));
	}

	/**
	 * Returns -A
	 * @method DenseMatrix#negated
	 * @return {DenseMatrix}
	 */
	negated() {
		return new DenseMatrix(this.data.negated());
	}

	/**
	 * Returns A(i, j)
	 * @method DenseMatrix#get
	 * @param {number} i The ith row of this dense matrix.
	 * @param {number} j The jth column of this dense matrix.
	 * @return {number}
	 */
	get(i, j = 0) {
		return this.data.get(i, j);
	}

	/**
	 * A(i, j) = x
	 * @method DenseMatrix#set
	 * @param {number} x The real value the (i, j)th entry of this dense matrix is set to.
	 * @param {number} i The ith row of this dense matrix.
	 * @param {number} j The jth column of this dense matrix.
	 */
	set(x, i, j = 0) {
		this.data.set(i, j, x);
	}

	/**
	 * Concatenates two dense matrices horizontally.
	 * @method DenseMatrix#hcat
	 * @param {DenseMatrix} B The dense matrix that is concatenated horizontally
	 * with this dense matrix.
	 * @return {DenseMatrix}
	 */
	hcat(B) {
		return new DenseMatrix(this.data.hcat(B.data));
	}

	/**
	 * Concatenates two dense matrices vertically.
	 * @method DenseMatrix#vcat
	 * @param {DenseMatrix} B The dense matrix that is concatenated vertically
	 * with this dense matrix.
	 * @return {DenseMatrix}
	 */
	vcat(B) {
		return new DenseMatrix(this.data.vcat(B.data));
	}
}
