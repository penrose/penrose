"use strict";

describe("Vector", function() {
	let u = new Vector(3, 4, 0);
	describe("constructor", function() {
		it("creates a vector with specified values", function() {
			let success = u.x === 3 && u.y === 4 && u.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	let v = new Vector();
	v.x = 3;
	v.y = 0;
	v.z = 3;
	describe("property setter", function() {
		it("sets the properties", function() {
			let success = v.x === 3 && v.y === 0 && v.z === 3;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("norm", function() {
		it("equals sqrt(dot(u, u))", function() {
			let success = Math.abs((Math.sqrt(u.dot(u))) - u.norm()) < 1e-8;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("norm2", function() {
		it("equals dot(u, u)", function() {
			let success = Math.abs(u.dot(u) - u.norm2()) < 1e-8;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("normalize", function() {
		it("norm = 1", function() {
			let w = new Vector(u.x, u.y, u.z);
			w.normalize();
			let success = Math.abs(1.0 - w.norm()) < 1e-8;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("unit", function() {
		it("returns normalized copy", function() {
			let w = u.unit();
			let success = w.x === 3 / 5 && w.y === 4 / 5 && w.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("isValid", function() {
		it("checks if entries are finite numbers", function() {
			let w = new Vector(-Infinity, 3, 0);
			let x = new Vector(1, 2, 3);
			let y = new Vector(0, 3, NaN);
			let success = !w.isValid() && x.isValid() && !y.isValid();
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+=", function() {
		it("adds a vector", function() {
			let w = new Vector(u.x, u.y, u.z);
			w.incrementBy(v);
			let success = w.x === 6 && w.y === 4 && w.z === 3;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-=", function() {
		it("substracts a vector", function() {
			let w = new Vector(u.x, u.y, u.z);
			w.decrementBy(v);
			let success = w.x === 0 && w.y === 4 && w.z === -3;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*=", function() {
		it("multiplies by a number", function() {
			let w = new Vector(u.x, u.y, u.z);
			w.scaleBy(2);
			let success = w.x === 6 && w.y === 8 && w.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("/=", function() {
		it("divides by a number", function() {
			let w = new Vector(u.x, u.y, u.z);
			w.divideBy(2);
			let success = w.x === 3 / 2 && w.y === 2 && w.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+", function() {
		it("adds two vectors", function() {
			let w = u.plus(v);
			let success = w.x === 6 && w.y === 4 && w.z === 3;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-", function() {
		it("subtracts two vectors", function() {
			let w = u.minus(v);
			let success = w.x === 0 && w.y === 4 && w.z === -3;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*", function() {
		it("multiplies a vector and a number", function() {
			let w = u.times(2);
			let success = w.x === 6 && w.y === 8 && w.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("/", function() {
		it("divides a vector by a number", function() {
			let w = u.over(2);
			let success = w.x === 3 / 2 && w.y === 2 && w.z === 0;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("unary -", function() {
		let w = u.negated();
		let success = w.x === -3 && w.y === -4 && w.z === 0;
		chai.assert.strictEqual(success, true);
	});

	describe("dot", function() {
		it("computes the dot product of two vectors", function() {
			let success = u.dot(v) === 9;
			chai.assert.strictEqual(success, true);
		});
	});

	describe("cross", function() {
		it("computes the cross product of two vectors", function() {
			let w = u.cross(v);
			let success = w.x === 12 && w.y === -9 && w.z === -12;
			chai.assert.strictEqual(success, true);
		});
	});
});

describe("DenseMatrix", function() {
	describe("zeros", function() {
		it("initializes a matrix of zeros", function() {
			let success = true;

			let A = DenseMatrix.zeros(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("identity", function() {
		it("initializes an identity matrix", function() {
			let success = true;

			let A = DenseMatrix.identity(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = i === j ? A.get(i, j) === 1 : A.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("ones", function() {
		it("initializes a matrix of ones", function() {
			let success = true;

			let A = DenseMatrix.ones(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j) === 1;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("constant", function() {
		it("initializes constant matrix", function() {
			let success = true;

			let A = DenseMatrix.constant(5, 3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j) === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("transpose", function() {
		it("computes matrix transpose", function() {
			let success = true;

			let A = DenseMatrix.zeros(2, 2);
			A.set(1, 0, 0);
			A.set(2, 1, 0);
			A.set(3, 0, 1);
			A.set(4, 1, 1);

			let AT = A.transpose();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j) === AT.get(j, i);
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("nRows and nCols", function() {
		it("checks the row and column count", function() {
			let A = DenseMatrix.zeros(5, 17);
			let success = A.nRows() === 5 && A.nCols() === 17;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("norm", function() {
		it("computes the matrix 1, 2 and infinity norms", function() {
			let A = DenseMatrix.zeros(2, 2);
			A.set(3, 0, 0);
			A.set(4, 1, 0);

			let success = Math.abs(4 - A.norm(0)) < 1e-8 &&
				Math.abs(7 - A.norm(1)) < 1e-8 &&
				Math.abs(5 - A.norm(2)) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("rank", function() {
		it("computes the rank", function() {
			let A = DenseMatrix.zeros(2, 2);
			A.set(1, 0, 0);
			A.set(1, 1, 1);

			let success = A.rank() === 2;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("sum", function() {
		it("computes the matrix sum", function() {
			let A = DenseMatrix.zeros(2, 2);
			A.set(1, 0, 0);
			A.set(2, 1, 0);
			A.set(3, 0, 1);
			A.set(4, 1, 1);

			let success = Math.abs(10 - A.sum()) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("subMatrix", function() {
		it("extracts subMatrix", function() {
			let success = true;

			let A = DenseMatrix.zeros(2, 3);
			A.set(1, 0, 0);
			A.set(2, 0, 1);
			A.set(3, 0, 2);
			A.set(4, 1, 0);
			A.set(5, 1, 1);
			A.set(6, 1, 2);

			let B = A.subMatrix(0, 2, 1, 2);
			for (let i = 0; i < 2; i++) {
				for (let j = 1; j < 2; j++) {
					success = A.get(i, j) === B.get(i, 0);
					if (!success) break;
				}
			}

			if (success) {
				let C = A.subMatrix(1, 2, 0, 3);
				for (let i = 1; i < 2; i++) {
					for (let j = 0; j < 3; j++) {
						success = A.get(i, j) === C.get(0, j);
						if (!success) break;
					}
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*=", function() {
		it("scales by a real number", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			A.scaleBy(5);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j) === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+=", function() {
		it("adds a matrix", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			A.incrementBy(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j) === 2;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-=", function() {
		it("subtracts a matrix", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			A.decrementBy(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* real", function() {
		it("multiples a matrix and a real number", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = A.timesReal(5);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = B.get(i, j) === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* matrix", function() {
		it("multiples two matrices", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			let C = A.timesDense(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j) === 2;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+", function() {
		it("adds two matrices", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			let C = A.plus(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j) === 2;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-", function() {
		it("subtracts two matrices", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			let C = A.minus(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("unary -", function() {
		it("negates a matrix", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = A.negated();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = B.get(i, j) === -A.get(i, j);
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("hcat", function() {
		it("concatenates two matrices horizontally", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			let C = A.hcat(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 4; j++) {
					success = C.get(i, j) === 1;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("vcat", function() {
		it("concatenates two matrices vertically", function() {
			let success = true;

			let A = DenseMatrix.ones(2, 2);
			let B = DenseMatrix.ones(2, 2);
			let C = A.vcat(B);
			for (let i = 0; i < 4; i++) {
				for (let j = 0; j < 0; j++) {
					success = C.get(i, j) === 1;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});
});

describe("SparseMatrix", function() {
	describe("fromTriplet", function() {
		it("constructs sparse matrix from triplet", function() {
			let success = true;

			let T = new Triplet(2, 2);
			T.addEntry(1, 0, 0);
			T.addEntry(2, 1, 0);
			T.addEntry(3, 0, 1);
			T.addEntry(4, 1, 1);

			let S = SparseMatrix.fromTriplet(T);
			let A = S.toDense();

			success = A.get(0, 0) === 1 && A.get(1, 0) === 2 &&
				A.get(0, 1) === 3 && A.get(1, 1) === 4;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("identity", function() {
		it("initializes an identity matrix", function() {
			let success = true;

			let S = SparseMatrix.identity(3, 3);
			let A = S.toDense();
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = i === j ? A.get(i, j) === 1 : A.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("diag", function() {
		it("initializes a diagonal matrix", function() {
			let success = true;

			let d = DenseMatrix.ones(3, 1)
			let S = SparseMatrix.diag(d);
			let A = S.toDense();
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = i === j ? A.get(i, j) === 1 : A.get(i, j) === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("transpose", function() {
		it("computes matrix transpose", function() {
			let success = true;

			let T = new Triplet(2, 2);
			T.addEntry(1, 0, 0);
			T.addEntry(2, 1, 0);
			T.addEntry(3, 0, 1);
			T.addEntry(4, 1, 1);

			let S = SparseMatrix.fromTriplet(T);
			let ST = S.transpose();
			let A = S.toDense();
			let AT = ST.toDense();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j) === AT.get(j, i);
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("invertDiagonal", function() {
		it("computes diagonal inverse", function() {
			let success = true;

			let T = new Triplet(2, 2);
			T.addEntry(1, 0, 0);
			T.addEntry(2, 1, 1);

			let S = SparseMatrix.fromTriplet(T);
			let ST = S.invertDiagonal();
			let AT = ST.toDense();
			success = AT.get(0, 0) === 1 && AT.get(1, 1) === 0.5;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("nRows, nCols and nnz", function() {
		it("checks the row, column and non zero count", function() {
			let S = SparseMatrix.identity(5, 5);
			let success = S.nRows() === 5 && S.nCols() === 5 && S.nnz() === 5;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("frobeniusNorm", function() {
		it("computes the frobenius norm", function() {
			let T = new Triplet(2, 2);
			T.addEntry(3, 0, 0);
			T.addEntry(4, 1, 0);

			let S = SparseMatrix.fromTriplet(T);

			let success = Math.abs(5 - S.frobeniusNorm()) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("subMatrix", function() {
		it("extracts subMatrix", function() {
			let success = true;

			let T = new Triplet(2, 3);
			T.addEntry(1, 0, 0);
			T.addEntry(2, 0, 1);
			T.addEntry(3, 0, 2);
			T.addEntry(4, 1, 0);
			T.addEntry(5, 1, 1);
			T.addEntry(6, 1, 2);

			let S = SparseMatrix.fromTriplet(T);
			let A = S.toDense();

			let SB = S.subMatrix(0, 2, 1, 2);
			let B = SB.toDense();
			for (let i = 0; i < 2; i++) {
				for (let j = 1; j < 2; j++) {
					success = A.get(i, j) === B.get(i, 0);
					if (!success) break;
				}
			}

			if (success) {
				let SC = S.subMatrix(1, 2, 0, 3);
				let C = SC.toDense();
				for (let i = 1; i < 2; i++) {
					for (let j = 0; j < 3; j++) {
						success = A.get(i, j) === C.get(0, j);
						if (!success) break;
					}
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("chol", function() {
		it("solves PSD system Ax = b", function() {
			let success = true

			let b = DenseMatrix.ones(2, 1);
			b.scaleBy(6);

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(3);

			let llt = S.chol();
			let x = llt.solvePositiveDefinite(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j) - 2) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("lu", function() {
		it("solves square system Ax = b", function() {
			let success = true

			let b = DenseMatrix.ones(2, 1);
			b.scaleBy(6);

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(3);

			let lu = S.lu();
			let x = lu.solveSquare(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j) - 2) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("qr", function() {
		it("solves system Ax = b", function() {
			let success = true

			let b = DenseMatrix.ones(2, 1);
			b.scaleBy(6);

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(3);

			let qr = S.qr();
			let x = qr.solve(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j) - 2) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*=", function() {
		it("scales by a real number", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(5);
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+=", function() {
		it("adds a matrix", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			let SB = SparseMatrix.identity(2, 2);
			S.incrementBy(SB);
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 2;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-=", function() {
		it("subtracts a matrix", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			let SB = SparseMatrix.identity(2, 2);
			S.decrementBy(SB);
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 0;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* real", function() {
		it("multiples a matrix and a real number", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			let SB = S.timesReal(5);
			let B = SB.toDense();
			for (let i = 0; i < 2; i++) {
				success = B.get(i, i) === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* dense", function() {
		it("multiples a sparse and dense matrix", function() {
			let success = true;

			let S = SparseMatrix.identity(3, 3);
			let X = DenseMatrix.ones(3, 1);
			let A = S.timesDense(X);
			for (let i = 0; i < 3; i++) {
				success = A.get(i, 0) === 1;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* sparse", function() {
		it("multiples two matrices", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(5);
			let SB = SparseMatrix.identity(2, 2);
			let SC = S.timesSparse(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+", function() {
		it("adds two matrices", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(5);
			let SB = SparseMatrix.identity(2, 2);
			let SC = S.plus(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 6;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-", function() {
		it("subtracts two matrices", function() {
			let success = true;

			let S = SparseMatrix.identity(2, 2);
			S.scaleBy(5);
			let SB = SparseMatrix.identity(2, 2);
			let SC = S.minus(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i) === 4;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});
});

describe("ComplexDenseMatrix", function() {
	describe("zeros", function() {
		it("initializes a matrix of zeros", function() {
			let success = true;

			let A = ComplexDenseMatrix.zeros(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j).re === 0 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("identity", function() {
		it("initializes an identity matrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.identity(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success =
						i === j ?
						A.get(i, j).re === 1 && A.get(i, j).im === 0 :
						A.get(i, j).re === 0 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("ones", function() {
		it("initializes a matrix of ones", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j).re === 1 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("constant", function() {
		it("initializes constant matrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.constant(new Complex(5, 5), 3, 3);
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success = A.get(i, j).re === 5 && A.get(i, j).im === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("transpose", function() {
		it("computes matrix transpose", function() {
			let success = true;

			let A = ComplexDenseMatrix.zeros(2, 2);
			A.set(new Complex(1, 1), 0, 0);
			A.set(new Complex(2, 2), 1, 0);
			A.set(new Complex(3, 3), 0, 1);
			A.set(new Complex(4, 4), 1, 1);

			let AT = A.transpose();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === AT.get(j, i).re && A.get(i, j).im === AT.get(j, i).im;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("conjugate", function() {
		it("computes matrix conjugate", function() {
			let success = true;

			let A = ComplexDenseMatrix.zeros(2, 2);
			A.set(new Complex(1, 1), 0, 0);
			A.set(new Complex(2, 2), 1, 0);
			A.set(new Complex(3, 3), 0, 1);
			A.set(new Complex(4, 4), 1, 1);

			let AC = A.conjugate();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === AC.get(i, j).re && A.get(i, j).im === -AC.get(i, j).im;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("nRows and nCols", function() {
		it("checks the row and column count", function() {
			let A = ComplexDenseMatrix.zeros(5, 17);
			let success = A.nRows() === 5 && A.nCols() === 17;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("norm", function() {
		it("computes the matrix 1, 2 and infinity norms", function() {
			let A = ComplexDenseMatrix.zeros(2, 2);
			A.set(new Complex(3, 0), 0, 0);
			A.set(new Complex(4, 0), 1, 0);

			let success = Math.abs(4 - A.norm(0)) < 1e-8 &&
				Math.abs(7 - A.norm(1)) < 1e-8 &&
				Math.abs(5 - A.norm(2)) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("rank", function() {
		it("computes the rank", function() {
			let A = ComplexDenseMatrix.zeros(2, 2);
			A.set(new Complex(1, 0), 0, 0);
			A.set(new Complex(1, 0), 1, 1);

			let success = A.rank() === 2;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("sum", function() {
		it("computes the matrix sum", function() {
			let A = ComplexDenseMatrix.zeros(2, 2);
			A.set(new Complex(1, 1), 0, 0);
			A.set(new Complex(2, 2), 1, 0);
			A.set(new Complex(3, 3), 0, 1);
			A.set(new Complex(4, 4), 1, 1);

			let success = Math.abs(10 - A.sum().re) < 1e-8 && Math.abs(10 - A.sum().im) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("subMatrix", function() {
		it("extracts subMatrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.zeros(2, 3);
			A.set(new Complex(1, 1), 0, 0);
			A.set(new Complex(2, 2), 0, 1);
			A.set(new Complex(3, 3), 0, 2);
			A.set(new Complex(4, 4), 1, 0);
			A.set(new Complex(5, 5), 1, 1);
			A.set(new Complex(6, 6), 1, 2);

			let B = A.subMatrix(0, 2, 1, 2);
			for (let i = 0; i < 2; i++) {
				for (let j = 1; j < 2; j++) {
					success = A.get(i, j).re === B.get(i, 0).re && A.get(i, j).im === B.get(i, 0).im;
					if (!success) break;
				}
			}

			if (success) {
				let C = A.subMatrix(1, 2, 0, 3);
				for (let i = 1; i < 2; i++) {
					for (let j = 0; j < 3; j++) {
						success = A.get(i, j).re === C.get(0, j).re && A.get(i, j).im === C.get(0, j).im;
						if (!success) break;
					}
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*=", function() {
		it("scales by a complex number", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			A.scaleBy(new Complex(5, 5));
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === 5 && A.get(i, j).im === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+=", function() {
		it("adds a matrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			A.incrementBy(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === 2 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-=", function() {
		it("subtracts a matrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			A.decrementBy(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === 0 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* complex", function() {
		it("multiples a matrix and a complex number", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = A.timesComplex(new Complex(5, 5));
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = B.get(i, j).re === 5 && B.get(i, j).im === 5;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* matrix", function() {
		it("multiples two matrices", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			let C = A.timesDense(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j).re === 2 && C.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+", function() {
		it("adds two matrices", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			let C = A.plus(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j).re === 2 && C.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-", function() {
		it("subtracts two matrices", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			let C = A.minus(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = C.get(i, j).re === 0 && C.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("unary -", function() {
		it("negates a matrix", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = A.negated();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = B.get(i, j).re === -A.get(i, j).re && B.get(i, j).im === -A.get(i, j).im;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("hcat", function() {
		it("concatenates two matrices horizontally", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			let C = A.hcat(B);
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 4; j++) {
					success = C.get(i, j).re === 1 && C.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("vcat", function() {
		it("concatenates two matrices vertically", function() {
			let success = true;

			let A = ComplexDenseMatrix.ones(2, 2);
			let B = ComplexDenseMatrix.ones(2, 2);
			let C = A.vcat(B);
			for (let i = 0; i < 4; i++) {
				for (let j = 0; j < 0; j++) {
					success = C.get(i, j).re === 1 && C.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});
});

describe("ComplexSparseMatrix", function() {
	describe("fromTriplet", function() {
		it("constructs sparse matrix from triplet", function() {
			let success = true;

			let T = new ComplexTriplet(2, 2);
			T.addEntry(new Complex(1, 1), 0, 0);
			T.addEntry(new Complex(2, 2), 1, 0);
			T.addEntry(new Complex(3, 3), 0, 1);
			T.addEntry(new Complex(4, 4), 1, 1);

			let S = ComplexSparseMatrix.fromTriplet(T);
			let A = S.toDense();

			success = A.get(0, 0).re === 1 && A.get(1, 0).re === 2 &&
				A.get(0, 1).re === 3 && A.get(1, 1).re === 4 &&
				A.get(0, 0).im === 1 && A.get(1, 0).im === 2 &&
				A.get(0, 1).im === 3 && A.get(1, 1).im === 4;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("identity", function() {
		it("initializes an identity matrix", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(3, 3);
			let A = S.toDense();
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success =
						i === j ?
						A.get(i, j).re === 1 && A.get(i, j).im === 0 :
						A.get(i, j).re === 0 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("diag", function() {
		it("initializes a diagonal matrix", function() {
			let success = true;

			let d = ComplexDenseMatrix.ones(3, 1)
			let S = ComplexSparseMatrix.diag(d);
			let A = S.toDense();
			for (let i = 0; i < 3; i++) {
				for (let j = 0; j < 3; j++) {
					success =
						i === j ?
						A.get(i, j).re === 1 && A.get(i, j).im === 0 :
						A.get(i, j).re === 0 && A.get(i, j).im === 0;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("transpose", function() {
		it("computes matrix transpose", function() {
			let success = true;

			let T = new ComplexTriplet(2, 2);
			T.addEntry(new Complex(1, 1), 0, 0);
			T.addEntry(new Complex(2, 2), 1, 0);
			T.addEntry(new Complex(3, 3), 0, 1);
			T.addEntry(new Complex(4, 4), 1, 1);

			let S = ComplexSparseMatrix.fromTriplet(T);
			let ST = S.transpose();
			let A = S.toDense();
			let AT = ST.toDense();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === AT.get(j, i).re && A.get(i, j).im === AT.get(j, i).im;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("invertDiagonal", function() {
		it("computes diagonal inverse", function() {
			let success = true;

			let T = new ComplexTriplet(2, 2);
			T.addEntry(new Complex(1), 0, 0);
			T.addEntry(new Complex(2), 1, 1);

			let S = ComplexSparseMatrix.fromTriplet(T);
			let ST = S.invertDiagonal();
			let AT = ST.toDense();
			success = AT.get(0, 0).re === 1 && AT.get(1, 1).re === 0.5;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("conjugate", function() {
		it("computes matrix conjugate", function() {
			let success = true;

			let T = new ComplexTriplet(2, 2);
			T.addEntry(new Complex(1, 1), 0, 0);
			T.addEntry(new Complex(2, 2), 1, 0);
			T.addEntry(new Complex(3, 3), 0, 1);
			T.addEntry(new Complex(4, 4), 1, 1);

			let S = ComplexSparseMatrix.fromTriplet(T);
			let SC = S.conjugate();
			let A = S.toDense();
			let AC = SC.toDense();
			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 2; j++) {
					success = A.get(i, j).re === AC.get(i, j).re && A.get(i, j).im === -AC.get(i, j).im;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("nRows, nCols and nnz", function() {
		it("checks the row, column and non zero count", function() {
			let S = ComplexSparseMatrix.identity(5, 5);
			let success = S.nRows() === 5 && S.nCols() === 5 && S.nnz() === 5;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("frobeniusNorm", function() {
		it("computes the frobenius norm", function() {
			let T = new ComplexTriplet(2, 2);
			T.addEntry(new Complex(3, 0), 0, 0);
			T.addEntry(new Complex(4, 0), 1, 0);

			let S = ComplexSparseMatrix.fromTriplet(T);

			let success = Math.abs(5 - S.frobeniusNorm()) < 1e-8;

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("subMatrix", function() {
		it("extracts subMatrix", function() {
			let success = true;

			let T = new ComplexTriplet(2, 3);
			T.addEntry(new Complex(1, 1), 0, 0);
			T.addEntry(new Complex(2, 2), 0, 1);
			T.addEntry(new Complex(3, 3), 0, 2);
			T.addEntry(new Complex(4, 4), 1, 0);
			T.addEntry(new Complex(5, 5), 1, 1);
			T.addEntry(new Complex(6, 6), 1, 2);

			let S = ComplexSparseMatrix.fromTriplet(T);
			let A = S.toDense();

			let SB = S.subMatrix(0, 2, 1, 2);
			let B = SB.toDense();
			for (let i = 0; i < 2; i++) {
				for (let j = 1; j < 2; j++) {
					success = A.get(i, j).re === B.get(i, 0).re && A.get(i, j).im === B.get(i, 0).im;
					if (!success) break;
				}
			}

			if (success) {
				let SC = S.subMatrix(1, 2, 0, 3);
				let C = SC.toDense();
				for (let i = 1; i < 2; i++) {
					for (let j = 0; j < 3; j++) {
						success = A.get(i, j).re === C.get(0, j).re && A.get(i, j).im === C.get(0, j).im;
						if (!success) break;
					}
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("chol", function() {
		it("solves PSD system Ax = b", function() {
			let success = true

			let b = ComplexDenseMatrix.ones(2, 1);
			b.scaleBy(new Complex(6, 6));

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(3, 3));

			let llt = S.chol();
			let x = llt.solvePositiveDefinite(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j).re - 2) < 1e-8 && Math.abs(x.get(i, j).im - 2) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("lu", function() {
		it("solves square system Ax = b", function() {
			let success = true

			let b = ComplexDenseMatrix.ones(2, 1);
			b.scaleBy(new Complex(6, 6));

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(3, 3));

			let lu = S.lu();
			let x = lu.solveSquare(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j).re - 2) < 1e-8 && Math.abs(x.get(i, j).im) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("qr", function() {
		it("solves system Ax = b", function() {
			let success = true

			let b = ComplexDenseMatrix.ones(2, 1);
			b.scaleBy(new Complex(6, 6));

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(3, 3));

			let qr = S.qr();
			let x = qr.solve(b);

			for (let i = 0; i < 2; i++) {
				for (let j = 0; j < 1; j++) {
					success = Math.abs(x.get(i, j).re - 2) < 1e-8 && Math.abs(x.get(i, j).im) < 1e-8;
					if (!success) break;
				}
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("*=", function() {
		it("multiplies by a complex number", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(5, 5));
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 5 && A.get(i, i).im === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+=", function() {
		it("adds a matrix", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			let SB = ComplexSparseMatrix.identity(2, 2);
			S.incrementBy(SB);
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 2 && A.get(i, i).im === 0;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-=", function() {
		it("subtracts a matrix", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			let SB = ComplexSparseMatrix.identity(2, 2);
			S.decrementBy(SB);
			let A = S.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 0 && A.get(i, i).im === 0;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* complex", function() {
		it("multiples a matrix and a complex number", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			let SB = S.timesComplex(new Complex(5, 5));
			let B = SB.toDense();
			for (let i = 0; i < 2; i++) {
				success = B.get(i, i).re === 5 && B.get(i, i).im === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* dense", function() {
		it("multiples a sparse and dense matrix", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(3, 3);
			let X = ComplexDenseMatrix.ones(3, 1);
			let A = S.timesDense(X);
			for (let i = 0; i < 3; i++) {
				success = A.get(i, 0).re === 1 && A.get(i, 0).im === 0;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("* sparse", function() {
		it("multiples two matrices", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(5, 5));
			let SB = ComplexSparseMatrix.identity(2, 2);
			let SC = S.timesSparse(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 5 && A.get(i, i).im === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("+", function() {
		it("adds two matrices", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(5, 5));
			let SB = ComplexSparseMatrix.identity(2, 2);
			let SC = S.plus(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 6 && A.get(i, i).im === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});

	describe("-", function() {
		it("subtracts two matrices", function() {
			let success = true;

			let S = ComplexSparseMatrix.identity(2, 2);
			S.scaleBy(new Complex(5, 5));
			let SB = ComplexSparseMatrix.identity(2, 2);
			let SC = S.minus(SB);
			let A = SC.toDense();
			for (let i = 0; i < 2; i++) {
				success = A.get(i, i).re === 4 && A.get(i, i).im === 5;
				if (!success) break;
			}

			memoryManager.deleteExcept([]);
			chai.assert.strictEqual(success, true);
		});
	});
});
