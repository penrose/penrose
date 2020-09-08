#pragma once

#include "SparseFactorization.h"
#include "DenseMatrix.h"

template<typename T>
class Triplet;

template<typename T>
class SparseMatrix {
public:
    // constructor
    SparseMatrix(int m, int n);

    // constructor
    SparseMatrix(Triplet<T> *triplet);

    // constructor
    SparseMatrix(const Eigen::SparseMatrix<T>& data);

    // copy constructor
    SparseMatrix(const SparseMatrix<T>& B);

    // assignment operators
    SparseMatrix<T>& operator=(const Eigen::SparseMatrix<T>& data);
    SparseMatrix<T>& operator=(const SparseMatrix<T>& B);

    // returns identity
    static SparseMatrix<T> identity(int m, int n);

    // return sparse diagonal matrix
    static SparseMatrix<T> diag(const DenseMatrix<T>& d);

    // returns transpose
    SparseMatrix<T> transpose() const;
    
    // returns conjugate
    SparseMatrix<T> conjugate() const;

    // returns number of rows
    int nRows() const;

    // returns number of columns
    int nCols() const;

    // returns number of non zeros
    int nnz() const;

    // returns frobenius norm
    double frobeniusNorm() const;

    // extracts submatrix in range [r0, r1) x [c0, c1)
    SparseMatrix<T> subMatrix(int r0, int r1, int c0, int c1) const;

    // returns dense
    DenseMatrix<T> toDense() const;

    // returns cholesky factorization
    Cholesky<T> *chol();

    // returns LU factorization
    LU<T> *lu();

    // returns QR factorization
    QR<T> *qr();

    // returns a copy of the eigen representation
    Eigen::SparseMatrix<T> copy() const;

    // returns eigen representation
    Eigen::SparseMatrix<T>& toEigen();

    // math
    SparseMatrix<T> operator*(const T& s);
    SparseMatrix<T> operator+(SparseMatrix<T> *B);
    SparseMatrix<T> operator-(SparseMatrix<T> *B);
    SparseMatrix<T> operator*(SparseMatrix<T> *B);
    DenseMatrix<T> operator*(DenseMatrix<T> *X);

    void operator*=(const T& s);
    void operator+=(SparseMatrix<T> *B);
    void operator-=(SparseMatrix<T> *B);

protected:
    // members
    SparseFactorization<T> factorization;
    Eigen::SparseMatrix<T> data;
};

template<typename T>
class Triplet {
public:
    // constructor
    Triplet(int m, int n);

    // add entry
    void add(int i, int j, const T& x);

    // returns choldmod representation
    vector<Eigen::Triplet<T>>& toEigen();

protected:
    // increases capacity
    void increaseCapacity();

    // member
    vector<Eigen::Triplet<T>> data;
    int m, n, capacity;
    friend class SparseMatrix<T>;
};

#include "SparseMatrix.inl"
