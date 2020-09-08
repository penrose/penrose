template<typename T>
SparseMatrix<T>::SparseMatrix(int m, int n):
factorization(*this)
{
    data = Eigen::SparseMatrix<T>(m, n);
    data.setZero();
}

template<typename T>
SparseMatrix<T>::SparseMatrix(Triplet<T> *triplet):
factorization(*this),
data(triplet->m, triplet->n)
{
    vector<Eigen::Triplet<T>>& eigenTriplet = triplet->toEigen();
    data.setFromTriplets(eigenTriplet.begin(), eigenTriplet.end());
}

template<typename T>
SparseMatrix<T>::SparseMatrix(const Eigen::SparseMatrix<T>& data_):
factorization(*this),
data(data_)
{

}

template<typename T>
SparseMatrix<T>::SparseMatrix(const SparseMatrix<T>& B):
factorization(*this),
data(B.copy())
{

}

template<typename T>
SparseMatrix<T>& SparseMatrix<T>::operator=(const Eigen::SparseMatrix<T>& data_)
{
    if (&data != &data_) {
        factorization.clear();
        data = data_;
    }

    return *this;
}

template<typename T>
SparseMatrix<T>& SparseMatrix<T>::operator=(const SparseMatrix<T>& B)
{
    if (this != &B) {
        factorization.clear();
        data = B.copy();
    }

    return *this;
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::identity(int m, int n)
{
    Eigen::SparseMatrix<T> id(m, n);
    id.setIdentity();

    return SparseMatrix<T>(id);
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::diag(const DenseMatrix<T>& d)
{
    Triplet<T> triplet(d.nRows(), d.nRows());
    for (int i = 0; i < d.nRows(); i++) triplet.add(i, i, d.get(i, 0));

    return SparseMatrix<T>(&triplet);
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::transpose() const
{
    return SparseMatrix<T>(data.transpose());
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::conjugate() const
{
    return SparseMatrix<T>(data.conjugate());
}

template<typename T>
int SparseMatrix<T>::nRows() const
{
    return (int)data.rows();
}

template<typename T>
int SparseMatrix<T>::nCols() const
{
    return (int)data.cols();
}

template<typename T>
int SparseMatrix<T>::nnz() const
{
    return (int)data.nonZeros();
}

template<typename T>
double SparseMatrix<T>::frobeniusNorm() const
{
    return data.norm();
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::subMatrix(int r0, int r1, int c0, int c1) const
{
    return SparseMatrix<T>(data.block(r0, c0, r1 - r0, c1 - c0));
}

template<typename T>
DenseMatrix<T> SparseMatrix<T>::toDense() const
{
    return DenseMatrix<T>(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>(data));
}

template<typename T>
Cholesky<T>* SparseMatrix<T>::chol()
{
    return &factorization.llt;
}

template<typename T>
LU<T>* SparseMatrix<T>::lu()
{
    return &factorization.lu;
}

template<typename T>
QR<T>* SparseMatrix<T>::qr()
{
    return &factorization.qr;
}

template<typename T>
Eigen::SparseMatrix<T> SparseMatrix<T>::copy() const
{
    return data;
}

template<typename T>
Eigen::SparseMatrix<T>& SparseMatrix<T>::toEigen()
{
    return data;
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::operator*(const T& s)
{
    return SparseMatrix<T>(data*s);
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::operator+(SparseMatrix<T> *B)
{
    return SparseMatrix<T>(data + B->data);
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::operator-(SparseMatrix<T> *B)
{
    return SparseMatrix<T>(data - B->data);
}

template<typename T>
SparseMatrix<T> SparseMatrix<T>::operator*(SparseMatrix<T> *B)
{
    return SparseMatrix<T>(data*B->data);
}

template<typename T>
DenseMatrix<T> SparseMatrix<T>::operator*(DenseMatrix<T> *X)
{
    return DenseMatrix<T>(data*X->data);;
}

template<typename T>
void SparseMatrix<T>::operator*=(const T& s)
{
    data *= s;
    factorization.clearNumeric();
}

template<typename T>
void SparseMatrix<T>::operator+=(SparseMatrix<T> *B)
{
    data += B->data;
    factorization.clear();
}

template<typename T>
void SparseMatrix<T>::operator-=(SparseMatrix<T> *B)
{
    data -= B->data;
    factorization.clear();
}

template<typename T>
Triplet<T>::Triplet(int m_, int n_):
m(m_),
n(n_),
capacity(m_)
{
    data.reserve(capacity);
}

template<typename T>
void Triplet<T>::add(int i, int j, const T& x)
{
    if ((int)data.size() == capacity) increaseCapacity();
    data.push_back(Eigen::Triplet<T>(i, j, x));
}

template<typename T>
vector<Eigen::Triplet<T>>& Triplet<T>::toEigen()
{
    return data;
}

template<typename T>
void Triplet<T>::increaseCapacity()
{
    capacity *= 2;
    data.reserve(capacity);
}
