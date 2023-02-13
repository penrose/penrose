
Additions to Style language:

* operator ' now performs matrix transpose

* operator * now performs matrix-vector multiply
* operator * now performs vector-matrix multiply

* operator * now performs scalar-matrix multiply
* operator * now performs matrix-scalar multiply

* operator * now performs matrix-matrix multiply
* operator + now performs matrix-matrix sum
* operator - now performs matrix-matrix difference

Methods added to `Autodiff.ts`:

* `smmul` — scalar-matrix multiplication
* `msmul` — matrix-scalar division
* `vmmul` — left vector-matrix multiplication
* `mvmul` — right matrix-vector multiplication
* `mmmul` — matrix-matrix multiplication
* `mmadd` — matrix-matrix addition
* `mmsub` — matrix-matrix subtraction
* `mtrans` — matrix transpose

