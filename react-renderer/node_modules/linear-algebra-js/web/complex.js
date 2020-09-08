"use strict";

class Complex {
	/**
	 * This class represents a complex number a + bi.
	 * @constructor Complex
	 * @param {number} re The real component of this complex number.
	 * @param {number} im The imaginary component of this complex number.
	 */
	constructor(re = 0, im = 0) {
		this.data = new Module.Complex(re, im);
		memoryManager.objectList.push(this);
	}

	/**
	 * Deletes the emscripten heap allocated data of this complex number.
	 * @method Complex#delete
	 * @ignore
	 */
	delete() {
		this.data.delete();
	}

	/**
	 * The real component of this complex number.
	 * @member Complex#re
	 * @type {number}
	 */
	get re() {
		return Module.real(this.data);
	}

	/**
	 * The imaginary component of this complex number.
	 * @member Complex#im
	 * @type {number}
	 */
	get im() {
		return Module.imag(this.data);
	}

	/**
	 * Computes the phase angle of this complex number.
	 * @method Complex#arg
	 * @returns {number}
	 */
	arg() {
		return Math.atan2(this.im, this.re);
	}

	/**
	 * Computes the norm of this complex number.
	 * @method Complex#norm
	 * @returns {number}
	 */
	norm() {
		return Math.sqrt(this.norm2());
	}

	/**
	 * Computes the squared norm of this complex number.
	 * @method Complex#norm2
	 * @returns {number}
	 */
	norm2() {
		return this.re * this.re + this.im * this.im;
	}

	/**
	 * Computes a - bi
	 * @method Complex#conjugate
	 * @returns {Complex}
	 */
	conjugate() {
		return new Complex(this.re, -this.im);
	}

	/**
	 * Computes (a + bi)^-1
	 * @method Complex#inverse
	 * @returns {Complex}
	 */
	inverse() {
		return this.conjugate().overReal(this.norm2());
	}

	/**
	 * Computes the polar form ae^(iθ), where a is the norm and θ is the
	 * phase angle of this complex number.
	 * @method Complex#polar
	 * @returns {Complex}
	 */
	polar() {
		let a = this.norm();
		let theta = this.arg();

		return new Complex(Math.cos(theta) * a, Math.sin(theta) * a);
	}

	/**
	 * Exponentiates this complex number.
	 * @method Complex#exp
	 * @returns {Complex}
	 */
	exp() {
		let a = Math.exp(this.re);
		let theta = this.im;

		return new Complex(Math.cos(theta) * a, Math.sin(theta) * a);
	}

	/**
	 * Returns u + v
	 * @method Complex#plus
	 * @param {Complex} v The complex number added to this complex number.
	 * @return {Complex}
	 */
	plus(v) {
		return new Complex(this.re + v.re, this.im + v.im);
	}

	/**
	 * Returns u - v
	 * @method Complex#minus
	 * @param {Complex} v The complex number subtracted from this complex number.
	 * @return {Complex}
	 */
	minus(v) {
		return new Complex(this.re - v.re, this.im - v.im);
	}

	/**
	 * Returns u * s
	 * @method Complex#timesReal
	 * @param {number} s The number this complex number is multiplied by.
	 * @return {Complex}
	 */
	timesReal(s) {
		return new Complex(this.re * s, this.im * s);
	}

	/**
	 * Returns u / s
	 * @method Complex#overReal
	 * @param {number} s The number this complex number is divided by.
	 * @return {Complex}
	 */
	overReal(s) {
		return this.timesReal(1 / s);
	}

	/**
	 * Returns u * v
	 * @method Complex#timesComplex
	 * @param {Complex} v The complex number this complex number is multiplied by.
	 * @return {Complex}
	 */
	timesComplex(v) {
		let a = this.re;
		let b = this.im;
		let c = v.re;
		let d = v.im;

		let re = a * c - b * d;
		let im = a * d + b * c;

		return new Complex(re, im);
	}

	/**
	 * Returns u / v
	 * @method Complex#overComplex
	 * @param {Complex} v The complex number this complex number is divided by.
	 * @return {Complex}
	 */
	overComplex(v) {
		return this.timesComplex(v.inverse());
	}
}
