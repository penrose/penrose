#include "eigen/include/SparseFactorization.h"
#include <emscripten/bind.h>

using namespace emscripten;

EMSCRIPTEN_BINDINGS(LinearAlgebra) {
        // Dense Matrix
        class_<DenseMatrix<double> >("DenseMatrix")
        .constructor<int, int>()
        .constructor<const DenseMatrix<double>&>()
        .class_function("identity", &DenseMatrix<double>::identity)
        .class_function("ones", &DenseMatrix<double>::ones)
        .class_function("constant", &DenseMatrix<double>::constant)
        .class_function("random", &DenseMatrix<double>::random)
        .function("transpose", &DenseMatrix<double>::transpose)
        .function("nRows", &DenseMatrix<double>::nRows)
        .function("nCols", &DenseMatrix<double>::nCols)
        .function("norm", &DenseMatrix<double>::norm)
        .function("rank", &DenseMatrix<double>::rank)
        .function("sum", &DenseMatrix<double>::sum)
        .function("subMatrix", select_overload<DenseMatrix<double>(int, int, int, int) const>(&DenseMatrix<double>::subMatrix))
        .function("scaleBy", &DenseMatrix<double>::operator*=, allow_raw_pointers())
        .function("incrementBy", &DenseMatrix<double>::operator+=, allow_raw_pointers())
        .function("decrementBy", &DenseMatrix<double>::operator-=, allow_raw_pointers())
        .function("timesReal", select_overload<DenseMatrix<double>(const double&)>(&DenseMatrix<double>::operator*))
        .function("timesDense", select_overload<DenseMatrix<double>(DenseMatrix<double>*)>(&DenseMatrix<double>::operator*), allow_raw_pointers())
        .function("plus", select_overload<DenseMatrix<double>(DenseMatrix<double>*)>(&DenseMatrix<double>::operator+), allow_raw_pointers())
        .function("minus", select_overload<DenseMatrix<double>(DenseMatrix<double>*)>(&DenseMatrix<double>::operator-), allow_raw_pointers())
        .function("negated", select_overload<DenseMatrix<double>()>(&DenseMatrix<double>::operator-))
        .function("get", &DenseMatrix<double>::get)
        .function("set", &DenseMatrix<double>::set)
        .function("hcat", &DenseMatrix<double>::hcat, allow_raw_pointers())
        .function("vcat", &DenseMatrix<double>::vcat, allow_raw_pointers());

        // Triplet
        class_<Triplet<double> >("Triplet")
        .constructor<int, int>()
        .function("addEntry", &Triplet<double>::add);

        // Sparse Matrix
        class_<SparseMatrix<double> >("SparseMatrix")
        .constructor<int, int>()
        .constructor<Triplet<double> *>()
        .class_function("identity", &SparseMatrix<double>::identity)
        .class_function("diag", &SparseMatrix<double>::diag)
        .function("transpose", &SparseMatrix<double>::transpose)
        .function("nRows", &SparseMatrix<double>::nRows)
        .function("nCols", &SparseMatrix<double>::nCols)
        .function("nnz", &SparseMatrix<double>::nnz)
        .function("frobeniusNorm", &SparseMatrix<double>::frobeniusNorm)
        .function("subMatrix", select_overload<SparseMatrix<double>(int, int, int, int) const>(&SparseMatrix<double>::subMatrix))
        .function("toDense", &SparseMatrix<double>::toDense)
        .function("chol", &SparseMatrix<double>::chol, allow_raw_pointers())
        .function("lu", &SparseMatrix<double>::lu, allow_raw_pointers())
        .function("qr", &SparseMatrix<double>::qr, allow_raw_pointers())
        .function("scaleBy", &SparseMatrix<double>::operator*=, allow_raw_pointers())
        .function("incrementBy", &SparseMatrix<double>::operator+=, allow_raw_pointers())
        .function("decrementBy", &SparseMatrix<double>::operator-=, allow_raw_pointers())
        .function("timesReal", select_overload<SparseMatrix<double>(const double&)>(&SparseMatrix<double>::operator*))
        .function("timesSparse", select_overload<SparseMatrix<double>(SparseMatrix<double>*)>(&SparseMatrix<double>::operator*), allow_raw_pointers())
        .function("plus", select_overload<SparseMatrix<double>(SparseMatrix<double>*)>(&SparseMatrix<double>::operator+), allow_raw_pointers())
        .function("minus", select_overload<SparseMatrix<double>(SparseMatrix<double>*)>(&SparseMatrix<double>::operator-), allow_raw_pointers())
        .function("timesDense", select_overload<DenseMatrix<double>(DenseMatrix<double>*)>(&SparseMatrix<double>::operator*), allow_raw_pointers());

        // Cholesky
        class_<Cholesky<double> >("Cholesky")
        .constructor<SparseMatrix<double>&>()
        .function("solvePositiveDefinite", &Cholesky<double>::solvePositiveDefinite, allow_raw_pointers());

        // LU
        class_<LU<double> >("LU")
        .constructor<SparseMatrix<double>&>()
        .function("solveSquare", &LU<double>::solveSquare, allow_raw_pointers());

        // QR
        class_<QR<double> >("QR")
        .constructor<SparseMatrix<double>&>()
        .function("solve", &QR<double>::solve, allow_raw_pointers());

        // Complex
        class_<complex<double> >("Complex")
        .constructor<double, double>();

        emscripten::function("real", select_overload<double(const complex<double>&)>(&real));
        emscripten::function("imag", select_overload<double(const complex<double>&)>(&imag));

        // Complex Dense Matrix
        class_<DenseMatrix<complex<double> > >("ComplexDenseMatrix")
        .constructor<int, int>()
        .constructor<const DenseMatrix<complex<double> >&>()
        .class_function("identity", &DenseMatrix<complex<double> >::identity)
        .class_function("ones", &DenseMatrix<complex<double> >::ones)
        .class_function("constant", &DenseMatrix<complex<double> >::constant)
        .class_function("random", &DenseMatrix<complex<double> >::random)
        .function("transpose", &DenseMatrix<complex<double> >::transpose)
        .function("conjugate", &DenseMatrix<complex<double> >::conjugate)
        .function("nRows", &DenseMatrix<complex<double> >::nRows)
        .function("nCols", &DenseMatrix<complex<double> >::nCols)
        .function("norm", &DenseMatrix<complex<double> >::norm)
        .function("rank", &DenseMatrix<complex<double> >::rank)
        .function("sum", &DenseMatrix<complex<double> >::sum)
        .function("subMatrix", select_overload<DenseMatrix<complex<double> >(int, int, int, int) const>(&DenseMatrix<complex<double> >::subMatrix))
        .function("scaleBy", &DenseMatrix<complex<double> >::operator*=, allow_raw_pointers())
        .function("incrementBy", &DenseMatrix<complex<double> >::operator+=, allow_raw_pointers())
        .function("decrementBy", &DenseMatrix<complex<double> >::operator-=, allow_raw_pointers())
        .function("timesComplex", select_overload<DenseMatrix<complex<double> >(const complex<double>&)>(&DenseMatrix<complex<double> >::operator*))
        .function("timesDense", select_overload<DenseMatrix<complex<double> >(DenseMatrix<complex<double> >*)>(&DenseMatrix<complex<double> >::operator*), allow_raw_pointers())
        .function("plus", select_overload<DenseMatrix<complex<double> >(DenseMatrix<complex<double> >*)>(&DenseMatrix<complex<double> >::operator+), allow_raw_pointers())
        .function("minus", select_overload<DenseMatrix<complex<double> >(DenseMatrix<complex<double> >*)>(&DenseMatrix<complex<double> >::operator-), allow_raw_pointers())
        .function("negated", select_overload<DenseMatrix<complex<double> >()>(&DenseMatrix<complex<double> >::operator-))
        .function("get", &DenseMatrix<complex<double> >::get)
        .function("set", &DenseMatrix<complex<double> >::set)
        .function("hcat", &DenseMatrix<complex<double> >::hcat, allow_raw_pointers())
        .function("vcat", &DenseMatrix<complex<double> >::vcat, allow_raw_pointers());

        // Complex Triplet
        class_<Triplet<complex<double> > >("ComplexTriplet")
        .constructor<int, int>()
        .function("addEntry", &Triplet<complex<double> >::add);

        // Complex Sparse Matrix
        class_<SparseMatrix<complex<double> > >("ComplexSparseMatrix")
        .constructor<int, int>()
        .constructor<Triplet<complex<double> > *>()
        .class_function("identity", &SparseMatrix<complex<double> >::identity)
        .class_function("diag", &SparseMatrix<complex<double> >::diag)
        .function("transpose", &SparseMatrix<complex<double> >::transpose)
        .function("conjugate", &SparseMatrix<complex<double> >::conjugate)
        .function("nRows", &SparseMatrix<complex<double> >::nRows)
        .function("nCols", &SparseMatrix<complex<double> >::nCols)
        .function("nnz", &SparseMatrix<complex<double> >::nnz)
        .function("frobeniusNorm", &SparseMatrix<complex<double> >::frobeniusNorm)
        .function("subMatrix", select_overload<SparseMatrix<complex<double> >(int, int, int, int) const>(&SparseMatrix<complex<double> >::subMatrix))
        .function("toDense", &SparseMatrix<complex<double> >::toDense)
        .function("chol", &SparseMatrix<complex<double> >::chol, allow_raw_pointers())
        .function("lu", &SparseMatrix<complex<double> >::lu, allow_raw_pointers())
        .function("qr", &SparseMatrix<complex<double> >::qr, allow_raw_pointers())
        .function("scaleBy", &SparseMatrix<complex<double> >::operator*=, allow_raw_pointers())
        .function("incrementBy", &SparseMatrix<complex<double> >::operator+=, allow_raw_pointers())
        .function("decrementBy", &SparseMatrix<complex<double> >::operator-=, allow_raw_pointers())
        .function("timesComplex", select_overload<SparseMatrix<complex<double> >(const complex<double>&)>(&SparseMatrix<complex<double> >::operator*))
        .function("timesSparse", select_overload<SparseMatrix<complex<double> >(SparseMatrix<complex<double> >*)>(&SparseMatrix<complex<double> >::operator*), allow_raw_pointers())
        .function("plus", select_overload<SparseMatrix<complex<double> >(SparseMatrix<complex<double> >*)>(&SparseMatrix<complex<double> >::operator+), allow_raw_pointers())
        .function("minus", select_overload<SparseMatrix<complex<double> >(SparseMatrix<complex<double> >*)>(&SparseMatrix<complex<double> >::operator-), allow_raw_pointers())
        .function("timesDense", select_overload<DenseMatrix<complex<double> >(DenseMatrix<complex<double> >*)>(&SparseMatrix<complex<double> >::operator*), allow_raw_pointers());

        // Complex Cholesky
        class_<Cholesky<complex<double> > >("ComplexCholesky")
        .constructor<SparseMatrix<complex<double> >&>()
        .function("solvePositiveDefinite", &Cholesky<complex<double> >::solvePositiveDefinite, allow_raw_pointers());

        // Complex LU
        class_<LU<complex<double> > >("ComplexLU")
        .constructor<SparseMatrix<complex<double> >&>()
        .function("solveSquare", &LU<complex<double> >::solveSquare, allow_raw_pointers());

        // Complex QR
        class_<QR<complex<double> > >("ComplexQR")
        .constructor<SparseMatrix<complex<double> >&>()
        .function("solve", &QR<complex<double> >::solve, allow_raw_pointers());
}
