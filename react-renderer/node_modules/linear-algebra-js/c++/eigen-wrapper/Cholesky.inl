#include "SparseMatrix.h"

template<typename T>
Cholesky<T>::Cholesky(SparseMatrix<T>& A_):
A(A_),
validSymbolic(false),
validNumeric(false)
{

}

template<typename T>
void Cholesky<T>::clear()
{
    validSymbolic = false;
    validNumeric = false;
}

template<typename T>
void Cholesky<T>::clearNumeric()
{
    validNumeric = false;
}

template<typename T>
void Cholesky<T>::buildSymbolic()
{
    clear();

    Eigen::SparseMatrix<T>& E = A.toEigen();
    E.makeCompressed();

    solver.analyzePattern(E);
    if (solver.info() == Eigen::Success) {
        validSymbolic = true;

        solver.factorize(E);
        if (solver.info() == Eigen::Success) validNumeric = true;
    }
}

template<typename T>
void Cholesky<T>::buildNumeric()
{
    Eigen::SparseMatrix<T>& E = A.toEigen();
    E.makeCompressed();

    solver.factorize(E);
    if (solver.info() == Eigen::Success) validNumeric = true;
}

template<typename T>
void Cholesky<T>::update()
{
    if (!validSymbolic) buildSymbolic();
    else if (!validNumeric) buildNumeric();
}

template<typename T>
DenseMatrix<T> Cholesky<T>::solvePositiveDefinite(DenseMatrix<T> *b)
{
    update();
    return DenseMatrix<T>(solver.solve(b->toEigen()));
}
