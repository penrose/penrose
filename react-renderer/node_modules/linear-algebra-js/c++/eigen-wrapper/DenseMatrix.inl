#include <Eigen/Dense>

template<typename T>
DenseMatrix<T>::DenseMatrix(int m, int n)
{
    data = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>::Zero(m, n);
}

template<typename T>
DenseMatrix<T>::DenseMatrix(const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& data_):
data(data_)
{

}

template<typename T>
DenseMatrix<T>::DenseMatrix(const DenseMatrix<T>& B):
data(B.copy())
{

}

template<typename T>
DenseMatrix<T>& DenseMatrix<T>::operator=(const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& data_)
{
    if (&data != &data_) data = data_;

    return *this;
}

template<typename T>
DenseMatrix<T>& DenseMatrix<T>::operator=(const DenseMatrix<T>& B)
{
    if (this != &B) data = B.copy();

    return *this;
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::identity(int m, int n)
{
    return DenseMatrix<T>(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>::Identity(m, n));
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::ones(int m, int n)
{
    return DenseMatrix<T>(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>::Ones(m, n));
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::constant(int m, int n, const T& x)
{
    return DenseMatrix<T>(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>::Constant(m, n, x));
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::random(int m, int n)
{
    return DenseMatrix<T>(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>::Random(m, n));
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::transpose() const
{
    return DenseMatrix<T>(data.transpose());
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::conjugate() const
{
    return DenseMatrix<T>(data.conjugate());
}

template<typename T>
int DenseMatrix<T>::nRows() const
{
    return (int)data.rows();
}

template<typename T>
int DenseMatrix<T>::nCols() const
{
    return (int)data.cols();
}

template<typename T>
double DenseMatrix<T>::norm(int n) const
{
    if (n == 0) return data.template lpNorm<Eigen::Infinity>();
    else if (n == 1) return data.template lpNorm<1>();
    return data.norm();
}

template<typename T>
double DenseMatrix<T>::rank() const
{
    Eigen::ColPivHouseholderQR<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> qr(data);
    return qr.rank();
}

template<typename T>
T DenseMatrix<T>::sum() const
{
    return data.sum();
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::subMatrix(int r0, int r1, int c0, int c1) const
{
    return DenseMatrix<T>(data.block(r0, c0, r1 - r0, c1 - c0));
}

template<typename T>
Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> DenseMatrix<T>::copy() const
{
    return data;
}

template<typename T>
Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& DenseMatrix<T>::toEigen()
{
    return data;
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::operator*(const T& s)
{
    return DenseMatrix<T>(data*s);
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::operator+(DenseMatrix<T> *B)
{
    return DenseMatrix<T>(data + B->data);
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::operator-(DenseMatrix<T> *B)
{
    return DenseMatrix<T>(data - B->data);
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::operator*(DenseMatrix<T> *B)
{
    return DenseMatrix<T>(data*B->data);
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::operator-()
{
    return DenseMatrix<T>(data*T(-1.0));
}

template<typename T>
void DenseMatrix<T>::operator*=(const T& s)
{
    data *= s;
}

template<typename T>
void DenseMatrix<T>::operator+=(DenseMatrix<T> *B)
{
    data += B->data;
}

template<typename T>
void DenseMatrix<T>::operator-=(DenseMatrix<T> *B)
{
    data -= B->data;
}

template<typename T>
T DenseMatrix<T>::get(int r, int c) const
{
    return data(r, c);
}

template<typename T>
void DenseMatrix<T>::set(int r, int c, const T& s)
{
    data(r, c) = s;
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::hcat(DenseMatrix<T> *B)
{
    int m = data.rows();
    int n1 = data.cols();
    int n2 = B->data.cols();
    DenseMatrix<T> C(m, n1 + n2);
    
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n1; j++) {
            C.set(i, j, data(i, j));
        }
        
        for (int j = 0; j < n2; j++) {
            C.set(i, n1 + j, B->data(i, j));
        }
    }
    
    return C;
}

template<typename T>
DenseMatrix<T> DenseMatrix<T>::vcat(DenseMatrix<T> *B)
{
    int m1 = data.rows();
    int m2 = B->data.rows();
    int n = data.cols();
    DenseMatrix<T> C(m1 + m2, n);
    
    for (int j = 0; j < n; j++) {
        for (int i = 0; i < m1; i++) {
            C.set(i, j, data(i, j));
        }
        
        for (int i = 0; i < m2; i++) {
            C.set(m1 + i, j, B->data(i, j));
        }
    }
    
    return C;
}
