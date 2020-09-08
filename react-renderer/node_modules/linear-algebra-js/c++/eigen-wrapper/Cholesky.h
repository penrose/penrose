#pragma once

#include <Eigen/SparseCholesky>

template<typename T>
class DenseMatrix;

template<typename T>
class SparseMatrix;

template<typename T>
class Cholesky {
public:
	// constructor
	Cholesky(SparseMatrix<T>& A);

	// clears both symbolic and numeric factorization --
	// should be called following any change to nonzero entries
	void clear();

	// clears only numeric factorization --
	// should be called following any change to the values
	// of nonzero entries
	void clearNumeric();

	// solve positive definite
	DenseMatrix<T> solvePositiveDefinite(DenseMatrix<T> *b);

protected:
	// builds symbolic factorization
	void buildSymbolic();

	// builds numeric factorization
	void buildNumeric();

	// updates factorizations
	void update();

	// members
	SparseMatrix<T>& A;
	Eigen::SimplicialCholesky<Eigen::SparseMatrix<T>> solver;
	bool validSymbolic;
	bool validNumeric;
};
