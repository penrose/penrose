## Key

Note that each bar component rounds up to the nearest 100ms, so each full bar is an overestimate by up to 400ms.

```
     0s   1s   2s   3s   4s   5s   6s   7s   8s
     |    |    |    |    |    |    |    |    |
name ▝▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▞▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▖
      compiling labeling optimizing rendering
```

If a row has only one bar instead of four, that means it's not a trio and the bar just shows the total time spent for that example, again rounded up to the nearest 100ms.

## Data

```
                                                          0s   1s   2s   3s   4s   5s   6s
                                                          |    |    |    |    |    |    |
set-theory-domain/tree-venn                               ▝▀▚▚
set-theory-domain/tree-tree                               ▝▚▚
set-theory-domain/tree-venn-3d                            ▝▚▚
group-theory/quaternion-multiplication-table              ▝▀▀▀▄▄▞▄
group-theory/quaternion-cayley-graph                      ▝▀▞▖
atoms-and-bonds/wet-floor                                 ▝▚▀▚
atoms-and-bonds/one-water-molecule                        ▝▞▖
set-theory-domain/continuousmap                           ▝▞▖
linear-algebra-domain/two-vectors-perp                    ▝▞▖
molecules/nitricacid-lewis                                ▝▀▀▀▞▀▖
exterior-algebra/vector-wedge                             ▝▚▚
shape-spec/all-shapes                                     ▝▚▚▖
shape-spec/arrowheads                                     ▝▚▚
graph-domain/textbook/sec1/fig1                           ▝▀▀▞▚
graph-domain/textbook/sec1/fig2                           ▝▀▀▚▀▖
graph-domain/textbook/sec1/fig3                           ▝▀▀▚▀▖
graph-domain/textbook/sec1/fig4                           ▝▀▀▀▚▀▚
graph-domain/textbook/sec1/fig5                           ▝▀▀▀▚▀▀▖
graph-domain/textbook/sec1/fig6                           ▝▀▀▀▀▀▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
graph-domain/textbook/sec1/fig7                           ▝▀▞▖
graph-domain/textbook/sec1/fig8a                          ▝▀▀▀▞▀▀▖
graph-domain/textbook/sec1/fig8b                          ▝▀▀▞▚
graph-domain/textbook/sec1/fig9                           ▝▀▚▚
graph-domain/textbook/sec1/fig10                          ▝▀▚▚
graph-domain/textbook/sec1/fig11                          ▝▀▀▀▞▖
graph-domain/textbook/sec1/fig12                          ▝▀▀▀▀▀▞▀▀▀▖
graph-domain/textbook/sec1/fig13                          ▝▀▀▚▀▖
graph-domain/textbook/sec2/fig3                           ▝▀▀▀▀▀▞▀▀▖
graph-domain/textbook/sec2/fig4                           ▝▀▀▀▀▀▞▀▚
graph-domain/textbook/sec2/fig5                           ▝▀▀▀▀▚▀▀▖
graph-domain/textbook/sec2/fig6                           ▝▀▀▀▀▀▞▀▀▚
graph-domain/textbook/sec2/fig9                           ▝▀▀▀▀▀▀▀▚▀▀▀▚
graph-domain/textbook/sec2/fig10a                         ▝▀▚▀▚
graph-domain/textbook/sec2/fig10b                         ▝▀▚▀▖
graph-domain/textbook/sec2/fig11a                         ▝▀▞▖
graph-domain/textbook/sec2/fig11b                         ▝▀▞▖
graph-domain/textbook/sec2/fig11c                         ▝▀▞▖
graph-domain/textbook/sec2/fig12                          ▝▀▚▚
graph-domain/textbook/sec2/fig13                          ▝▀▀▀▀▀▀▀▀▀▀▀▀▞▀▀▀▀▀▀▚
graph-domain/textbook/sec2/fig14                          ▝▀▚▚
graph-domain/textbook/sec2/fig16b                         ▝▀▚▚
geometry-domain/textbook_problems/c05p13                  ▝▀▞▖
geometry-domain/textbook_problems/c01p01                  ▝▀▞▖
geometry-domain/textbook_problems/c03p01                  ▝▀▞▖
geometry-domain/textbook_problems/c05p01                  ▝▀▞▖
geometry-domain/textbook_problems/ex                      ▝▀▚▚
triangle-mesh-3d/two-triangles                            ▝▚▚
random-sampling/test                                      ▝▀▀▞▖
geometry-domain/textbook_problems/c11p12                  ▝▀▞▖
word-cloud/example                                        ▝▀▞▚
geometry-domain/siggraph-teaser                           ▝▀▞▖
minkowski-tests/maze/non-convex                           ▝▚▚
lagrange-bases/lagrange-bases                             ▝▞▖
hypergraph/hypergraph                                     ▝▀▀▚▀▀▀▀▀▀▀▀▀▀▚
persistent-homology/persistent-homology                   ▝▀▚▀▀▀▀▀▀▖
walk-on-spheres/laplace-estimator                         ▝▚▚
walk-on-spheres/poisson-estimator                         ▝▀▞▚
walk-on-spheres/nested-estimator                          ▝▀▚▀▀▀▖
walk-on-spheres/offcenter-estimator                       ▝▀▚▀▖
shape-distance/points-around-star                         ▝▀▚▚
shape-distance/points-around-polyline                     ▝▀▞▖
shape-distance/points-around-line                         ▝▚▚
shape-distance/lines-around-rect                          ▝▞▖
closest-point/test                                        ▝▀▀▀▞▖
fake-3d-linear-algebra/projection                         ▝▞▖
animation/center-shrink-circle                            ▝▞▖
structural-formula/molecules/caffeine                     ▝▀▀▞▖
mobius/mobius                                             ▝▞▖
molecules/glutamine                                       ▝▚▚
matrix-ops/tests/matrix-matrix-addition                   ▝▚▚
matrix-ops/tests/matrix-matrix-division-elementwise       ▝▞▖
matrix-ops/tests/matrix-matrix-multiplication-elementwise ▝▚▚
matrix-ops/tests/matrix-matrix-multiplication             ▝▚▚
matrix-ops/tests/matrix-matrix-subtraction                ▝▚▚
matrix-ops/tests/matrix-transpose                         ▝▚▚
matrix-ops/tests/matrix-vector-left-multiplication        ▝▚▚
matrix-ops/tests/matrix-vector-right-multiplication       ▝▚▚
matrix-ops/tests/scalar-vector-division                   ▝▚▚
matrix-ops/tests/scalar-vector-left-multiplication        ▝▚▚
matrix-ops/tests/scalar-vector-right-multiplication       ▝▚▚
matrix-ops/tests/vector-vector-addition                   ▝▞▖
matrix-ops/tests/vector-vector-division-elementwise       ▝▞▖
matrix-ops/tests/vector-vector-multiplication-elementwise ▝▞▖
matrix-ops/tests/vector-vector-outerproduct               ▝▞▖
matrix-ops/tests/vector-vector-subtraction                ▝▞▖
logic-circuit-domain/half-adder                           ▝▚▚
curve-examples/cubic-bezier                               ▝▚▀▖
triangle-mesh-2d/diagrams/cotan-formula                   ▝▚▚
triangle-mesh-2d/diagrams/concyclic-pair                  ▝▀▞▖
triangle-mesh-2d/diagrams/halfedge-mesh                   ▝▚▚
triangle-mesh-2d/diagrams/relative-orientation            ▝▚▚
triangle-mesh-2d/diagrams/triangle-centers                ▝▚▚
triangle-mesh-2d/diagrams/angle-equivalence               ▝▀▞▚
graph-domain/textbook/sec5/ex32                           ▝▀▀▀▀▀▀▚▀▀▀▀▀▀▀▀▖
curve-examples/open-elastic-curve                         ▝▀▚▀▚
curve-examples/closed-elastic-curve                       ▝▀▚▀▖
graph-domain/other-examples/arpanet                       ▝▀▀▀▀▀▚▀▀▀▀▀▚
graph-domain/other-examples/nyc-subway                    ▝▀▀▀▀▀▀▀▀▚▀▀▀▀▀▀▀▖
curve-examples/blobs                                      ▝▀▀▀▚▀▀▀▀▀▀▀▀▀▀▀▀▖
curve-examples/space-curves                               ▝▀▀▀▀▚▀▀▀▖
solid/example                                             ▐
```
