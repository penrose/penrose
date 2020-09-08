"use strict";

class EmscriptenMemoryManager {
	/**
	 * This class serves as a semi automatic memory manager/garbage collector
	 * for objects that internally store data and perform operations on the {@link http://emscripten.org emscripten}
	 * heap, and hence cannot be freed by the Javascript garbage collector. These
	 * include {@link DenseMatrix}, {@link SparseMatrix}, {@link Triplet}, {@link Complex}, {@link ComplexDenseMatrix},
	 * {@link ComplexSparseMatrix} and {@link ComplexTriplet}. Since operations involving the above
	 * matrices and linear algebra entities are frequent and generate a lot of intermediate
	 * variables, EmscriptenMemoryManager automatically tracks all objects that are
	 * allocated on the emscripten heap to ease the burden of manual memory management.
	 * The user is required to inform the EmscriptenMemoryManager about when it should
	 * clear the heap and which objects it should *not* delete while doing so.
	 * @constructor EmscriptenMemoryManager
	 * @property {Object[]} objectList Array of objects allocated on the emscripten heap.
	 * @example
	 * let memoryManager = new EmscriptenMemoryManager();
	 *
	 * let A = SparseMatrix.identity(100, 100);
	 * let x = DenseMatrix.random(100, 1);
	 * let B = A.timesDense(x).plus(x);
	 *
	 * // delete all objects created in the previous three calls except B
	 * memoryManager.deleteExcept([B]);
	 */
	constructor() {
		this.objectList = [];
	}

	/**
	 * Deletes all objects in this memory manager's list of emscripten heap allocated
	 * objects except those in the array passed to this method.
	 * @method EmscriptenMemoryManager#deleteExcept
	 * @param {Object[]} exceptList Array of objects allocated on the emscripten heap
	 * that should not to be deleted.
	 */
	deleteExcept(exceptList) {
		for (let object of this.objectList) {
			let deleteObject = true;

			for (let except of exceptList) {
				if (object === except) {
					deleteObject = false;
				}
			}

			if (deleteObject) {
				object.delete();
			}
		}

		this.objectList = exceptList;
	}
}
