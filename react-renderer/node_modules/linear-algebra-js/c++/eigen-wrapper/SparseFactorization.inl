template<typename T>
SparseFactorization<T>::SparseFactorization(SparseMatrix<T>& A):
llt(A),
lu(A),
qr(A)
{

}

template<typename T>
void SparseFactorization<T>::clear()
{
    llt.clear();
    lu.clear();
    qr.clear();
}

template<typename T>
void SparseFactorization<T>::clearNumeric()
{
    llt.clearNumeric();
    lu.clearNumeric();
    qr.clearNumeric();
}