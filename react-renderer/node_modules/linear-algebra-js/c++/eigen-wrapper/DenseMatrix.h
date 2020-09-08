#pragma once

#include <vector>

using namespace std;

template<typename T>
class SparseMatrix;

template<typename T>
class DenseMatrix {
public:
    // constructor
    DenseMatrix(int m, int n);
    
    // constructor
    DenseMatrix(const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& data);
    
    // copy constructor
    DenseMatrix(const DenseMatrix<T>& B);
    
    // assignment operators
    DenseMatrix<T>& operator=(const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& data);
    DenseMatrix<T>& operator=(const DenseMatrix<T>& B);
    
    // returns identity
    static DenseMatrix<T> identity(int m, int n);
    
    // returns ones
    static DenseMatrix<T> ones(int m, int n);
    
    // returns constant matrix
    static DenseMatrix<T> constant(int m, int n, const T& x);
    
    // returns random matrix
    static DenseMatrix<T> random(int m, int n);
    
    // returns transpose
    DenseMatrix<T> transpose() const;
    
    // returns conjugate
    DenseMatrix<T> conjugate() const;
    
    // returns number of rows
    int nRows() const;
    
    // returns number of columns
    int nCols() const;
    
    // n = 0 -> Infinity Norm, n = l1 -> 1 Norm, n = 2 -> l2 Norm
    double norm(int n) const;
    
    // returns the rank of the matrix
    double rank() const;
    
    // returns sum
    T sum() const;
    
    // extracts submatrix in range [r0, r1) x [c0, c1)
    DenseMatrix<T> subMatrix(int r0, int r1, int c0, int c1) const;
    
    // returns a copy of the eigen representation
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> copy() const;
    
    // returns eigen representation
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& toEigen();
    
    // math
    DenseMatrix<T> operator*(const T& s);
    DenseMatrix<T> operator+(DenseMatrix<T> *B);
    DenseMatrix<T> operator-(DenseMatrix<T> *B);
    DenseMatrix<T> operator*(DenseMatrix<T> *B);
    DenseMatrix<T> operator-();
    
    void operator*=(const T& s);
    void operator+=(DenseMatrix<T> *B);
    void operator-=(DenseMatrix<T> *B);
    
    // access
    T get(int r, int c) const;
    void set(int r, int c, const T& s);
    
    // horizontal concatentation
    DenseMatrix<T> hcat(DenseMatrix<T> *B);
    
    // vertical concatentation
    DenseMatrix<T> vcat(DenseMatrix<T> *B);
    
protected:
    // member
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> data;
    friend class SparseMatrix<T>;
};

#include "DenseMatrix.inl"
