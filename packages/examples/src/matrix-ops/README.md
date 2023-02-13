Additions to Style language:

* operator `'` now performs matrix transpose
             `
* operator `*` now performs matrix-vector multiply
* operator `*` now performs vector-matrix multiply
             `
* operator `*` now performs scalar-matrix multiply
* operator `*` now performs matrix-scalar multiply
             `
* operator `*` now performs matrix-matrix multiply
* operator `+` now performs matrix-matrix sum
* operator `-` now performs matrix-matrix difference

* operator `.*` now performs elementwise vector-vector multiplication
* operator `./` now performs elementwise vector-vector division

* operator `.*` now performs elementwise matrix-matrix multiplication
* operator `./` now performs elementwise matrix-matrix division

Methods added to `Autodiff.ts`:

* `vouter` — vector-vector outer product
* `smmul` — scalar-matrix multiplication
* `msmul` — matrix-scalar division
* `vmmul` — left vector-matrix multiplication
* `mvmul` — right matrix-vector multiplication
* `mmmul` — matrix-matrix multiplication
* `mmadd` — matrix-matrix addition
* `mmsub` — matrix-matrix subtraction
* `mtrans` — matrix transpose
* `ewvvmul` — elementwise vector-vector multiplication
* `ewvvdiv` — elementwise vector-vector division
* `ewmmmul` — elementwise matrix-matrix multiplication
* `ewmmdiv` — elementwise matrix-matrix division

Functions added to `Functions.ts`:

* `cross` — 3D cross product (exposes existing `cross3` autodiff method)
* `outerProduct` — outer product of two vectors of equal length

