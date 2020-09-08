"use strict";

class Vector {
	/**
	 * This class represents an element of Euclidean 3-space, along with all the usual
	 * vector space operations (addition, multiplication by scalars, etc.).
	 * @constructor Vector
	 * @property {number} x The x component of this vector. Default value is 0.
	 * @property {number} y The y component of this vector. Default value is 0.
	 * @property {number} z The z component of this vector. Default value is 0.
	 */
	constructor(x = 0, y = 0, z = 0) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	/**
	 * Computes the Euclidean length of this vector.
	 * @method Vector#norm
	 * @returns {number}
	 */
	norm() {
		return Math.sqrt(this.norm2());
	}

	/**
	 * Computes the Euclidean length squared of this vector.
	 * @method Vector#norm2
	 * @returns {number}
	 */
	norm2() {
		return this.dot(this);
	}

	/**
	 * Divides this vector by its Euclidean length.
	 * @method Vector#normalize
	 */
	normalize() {
		let n = this.norm();
		this.x /= n;
		this.y /= n;
		this.z /= n;
	}

	/**
	 * Returns a normalized copy of this vector.
	 * @method Vector#unit
	 * @returns {Vector}
	 */
	unit() {
		let n = this.norm();
		let x = this.x / n;
		let y = this.y / n;
		let z = this.z / n;

		return new Vector(x, y, z);
	}

	/**
	 * Checks whether this vector's components are finite.
	 * @method Vector#isValid
	 * @returns {boolean}
	 */
	isValid() {
		return !isNaN(this.x) && !isNaN(this.y) && !isNaN(this.z) &&
			isFinite(this.x) && isFinite(this.y) && isFinite(this.z);
	}

	/**
	 * u += v
	 * @method Vector#incrementBy
	 * @param {Vector} v The vector added to this vector.
	 */
	incrementBy(v) {
		this.x += v.x;
		this.y += v.y;
		this.z += v.z;
	}

	/**
	 * u -= v
	 * @method Vector#decrementBy
	 * @param {Vector} v The vector subtracted from this vector.
	 */
	decrementBy(v) {
		this.x -= v.x;
		this.y -= v.y;
		this.z -= v.z;
	}

	/**
	 * u *= s
	 * @method Vector#scaleBy
	 * @param {number} s The number this vector is scaled by.
	 */
	scaleBy(s) {
		this.x *= s;
		this.y *= s;
		this.z *= s;
	}

	/**
	 * u /= s
	 * @method Vector#divideBy
	 * @param {number} s The number this vector is divided by.
	 */
	divideBy(s) {
		this.scaleBy(1 / s);
	}

	/**
	 * Returns u + v
	 * @method Vector#plus
	 * @param {Vector} v The vector added to this vector.
	 * @return {Vector}
	 */
	plus(v) {
		return new Vector(this.x + v.x, this.y + v.y, this.z + v.z);
	}

	/**
	 * Returns u - v
	 * @method Vector#minus
	 * @param {Vector} v The vector subtracted from this vector.
	 * @return {Vector}
	 */
	minus(v) {
		return new Vector(this.x - v.x, this.y - v.y, this.z - v.z);
	}

	/**
	 * Returns u * s
	 * @method Vector#times
	 * @param {number} s The number this vector is multiplied by.
	 * @return {Vector}
	 */
	times(s) {
		return new Vector(this.x * s, this.y * s, this.z * s);
	}

	/**
	 * Returns u / s
	 * @method Vector#over
	 * @param {number} s The number this vector is divided by.
	 * @return {Vector}
	 */
	over(s) {
		return this.times(1 / s);
	}

	/**
	 * Returns -u
	 * @method Vector#negated
	 * @return {Vector}
	 */
	negated() {
		return this.times(-1);
	}

	/**
	 * Computes the dot product of this vector and v
	 * @method Vector#dot
	 * @param {Vector} v The vector this vector is dotted with.
	 * @return {number}
	 */
	dot(v) {
		return this.x * v.x + this.y * v.y + this.z * v.z;
	}

	/**
	 * Computes the cross product of this vector and v
	 * @method Vector#cross
	 * @param {Vector} v The vector this vector is crossed with.
	 * @return {Vector}
	 */
	cross(v) {
		return new Vector(
			this.y * v.z - this.z * v.y,
			this.z * v.x - this.x * v.z,
			this.x * v.y - this.y * v.x);
	}
}
