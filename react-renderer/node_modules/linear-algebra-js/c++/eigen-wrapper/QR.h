#pragma once

#include <Eigen/SparseQR>

template<typename T>
class DenseMatrix;

template<typename T>
class SparseMatrix;

template<typename T>
class QR {
public:
    // constructor
    QR(SparseMatrix<T>& A);

    // clears both symbolic and numeric factorization --
    // should be called following any change to nonzero entries
    void clear();

    // clears only numeric factorization --
    // should be called following any change to the values
    // of nonzero entries
    void clearNumeric();

    // solve 
    DenseMatrix<T> solve(DenseMatrix<T> *b);

protected:
    // builds symbolic factorization
    void buildSymbolic();

    // builds numeric factorization
    void buildNumeric();

    // updates factorizations
    void update();

    // members
    SparseMatrix<T>& A;
    Eigen::SparseQR<Eigen::SparseMatrix<T>, Eigen::COLAMDOrdering<int>> solver;
    bool validSymbolic;
    bool validNumeric;
};
