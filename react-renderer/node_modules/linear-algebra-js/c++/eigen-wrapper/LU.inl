#include "SparseMatrix.h"

template<typename T>
LU<T>::LU(SparseMatrix<T>& A_):
A(A_),
validSymbolic(false),
validNumeric(false)
{

}

template<typename T>
void LU<T>::clear()
{
    validSymbolic = false;
    validNumeric = false;
}

template<typename T>
void LU<T>::clearNumeric()
{
    validNumeric = false;
}

template<typename T>
void LU<T>::buildSymbolic()
{
    clear();

    Eigen::SparseMatrix<T>& E = A.toEigen();
    E.makeCompressed();

    // it seems like there is memory related bug in Eigen that
    // causes solver.info() to return a seemingly random number
    // rather than Eigen::Success in SparseLU and SparseQR, but
    // not in SimplicialCholesky
    solver.analyzePattern(E);
    validSymbolic = true;

    solver.factorize(E);
    if (solver.info() == Eigen::Success) validNumeric = true;
}

template<typename T>
void LU<T>::buildNumeric()
{
    Eigen::SparseMatrix<T>& E = A.toEigen();
    E.makeCompressed();

    solver.factorize(E);
    if (solver.info() == Eigen::Success) validNumeric = true;
}

template<typename T>
void LU<T>::update()
{
    if (!validSymbolic) buildSymbolic();
    else if (!validNumeric) buildNumeric();
}

template<typename T>
DenseMatrix<T> LU<T>::solveSquare(DenseMatrix<T> *b)
{
    update();
    return DenseMatrix<T>(solver.solve(b->toEigen()));
}
