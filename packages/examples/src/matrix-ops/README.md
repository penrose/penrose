### matrix-ops

The `matrix-ops` example exists to help debug and validate matrix and vector operations in Style. It draws a grid where vectors are represented as arrows, and matrices are represented as pairs of arrows corresponding to the matrix rows. The resulting diagrams make it easy to quickly "spot check" computations carried out by Style with diagrams generated in other trusted software (such as _Mathematica_).

A battery of tests are provided in `tests/`. These tests are quite thorough, albeit not completely exhaustive. Generating additional tests based on this template should be straightforward.

A neat feature of Style taken advantage of in `matrix-ops.sty` is that `label` strings can be overridden, and assembled from labels of operands. For instance, an elementwise matrix-matrix product of matrices `A` and `B` automatically generates a label string `(A⊙B)`.
