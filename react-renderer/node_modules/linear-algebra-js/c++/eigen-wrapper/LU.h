#pragma once

#include <Eigen/SparseLU>

template<typename T>
class DenseMatrix;

template<typename T>
class SparseMatrix;

template<typename T>
class LU {
public:
    // constructor
    LU(SparseMatrix<T>& A);

    // clears both symbolic and numeric factorization --
    // should be called following any change to nonzero entries
    void clear();

    // clears only numeric factorization --
    // should be called following any change to the values
    // of nonzero entries
    void clearNumeric();

    // solve square
    DenseMatrix<T> solveSquare(DenseMatrix<T> *b);

protected:
    // builds symbolic factorization
    void buildSymbolic();

    // builds numeric factorization
    void buildNumeric();

    // updates factorizations
    void update();

    // members
    SparseMatrix<T>& A;
    Eigen::SparseLU<Eigen::SparseMatrix<T>> solver;
    bool validSymbolic;
    bool validNumeric;
};
