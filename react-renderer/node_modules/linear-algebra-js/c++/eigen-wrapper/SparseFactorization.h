#pragma once

#include "Cholesky.h"
#include "LU.h"
#include "QR.h"

template<typename T>
class SparseFactorization {
public:
	// constructor
    SparseFactorization(SparseMatrix<T>& A);

    // clears both symbolic and numeric factorization --
    // should be called following any change to nonzero entries
    void clear();

    // clears only numeric factorization --
    // should be called following any change to the values
    // of nonzero entries
    void clearNumeric();

    // members
    Cholesky<T> llt;
    LU<T> lu;
    QR<T> qr;
};

#include "Cholesky.inl"
#include "LU.inl"
#include "QR.inl"
#include "SparseFactorization.inl"
