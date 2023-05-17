export interface Trio {
  substance: string;
  style: string;
  domain: string;
  variation: string;
}

export interface Meta {
  get: () => Promise<Trio>;
  name?: string;
  gallery?: boolean;
}

const trio = async (x: Promise<{ default: Trio }>): Promise<Trio> =>
  (await x).default;

export const registry: Meta[] = [
  {
    get: () => trio(import("./set-theory-domain/tree-venn")),
    name: "Sets as Venn Diagram",
    gallery: true,
  },
  {
    get: () => trio(import("./set-theory-domain/tree-tree")),
    name: "Sets as Tree Diagram",
    gallery: true,
  },
  {
    get: () => trio(import("./set-theory-domain/tree-venn-3d")),
    name: "Sets as Venn Diagram in 2.5D",
    gallery: true,
  },
  {
    get: () =>
      trio(
        import(
          "./group-theory/quaternion-group-group-theory-multiplication-table"
        )
      ),
    name: "Quaternions as table",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./group-theory/quaternion-group-group-theory-cayley-graph")),
    name: "Quaternions as Cayley graph",
    gallery: true,
  },
  {
    get: () => trio(import("./atoms-and-bonds/wet-floor-atoms-and-bonds")),
    name: "Wet Floor",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./atoms-and-bonds/one-water-molecule-atoms-and-bonds")),
    name: "A Water Molecule",
    gallery: true,
  },
  {
    get: () => trio(import("./set-theory-domain/continuousmap-continuousmap")),
    name: "Continous Map",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./linear-algebra-domain/two-vectors-perp-vectors-dashed")),
    name: "Two Perpendicular Vectors",
    gallery: true,
  },
  {
    get: () => trio(import("./molecules/nitricacid-lewis")),
    name: "Lewis Structure of Nitric Acid",
    gallery: true,
  },
  {
    get: () => trio(import("./exterior-algebra/vector-wedge-exterior-algebra")),
  },
  { get: () => trio(import("./shape-spec/allShapes-allShapes")) },
  { get: () => trio(import("./shape-spec/arrowheads-arrowheads")) },
  {
    get: () =>
      trio(import("./graph-domain/computer-network-graph-simple-graph")),
  },
  {
    get: () =>
      trio(
        import(
          "./graph-domain/computer-network-multiple-links-graph-pseudograph"
        )
      ),
  },
  {
    get: () =>
      trio(
        import(
          "./graph-domain/computer-network-diagnostic-links-graph-pseudograph"
        )
      ),
  },
  {
    get: () =>
      trio(
        import(
          "./graph-domain/communications-network-one-way-links-graph-simple-directed-graph"
        )
      ),
  },
  {
    get: () =>
      trio(
        import(
          "./graph-domain/computer-network-multiple-one-way-links-graph-directed-multigraph"
        )
      ),
  },
  {
    get: () =>
      trio(import("./graph-domain/acquaintanceship-graph-simple-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/influence-graph-simple-directed-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/call-directed-graph-directed-multigraph")),
  },
  {
    get: () => trio(import("./graph-domain/call-undirected-graph-pseudograph")),
  },
  {
    get: () =>
      trio(
        import("./graph-domain/module-dependency-graph-simple-directed-graph")
      ),
  },
  {
    get: () =>
      trio(import("./graph-domain/precedence-graph-simple-directed-graph")),
  },
  {
    get: () => trio(import("./graph-domain/niche-overlap-graph-simple-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/protein-interaction-graph-simple-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/round-robin-graph-simple-directed-graph")),
  },
  { get: () => trio(import("./graph-domain/complete-graphs-simple-graph")) },
  { get: () => trio(import("./graph-domain/cycle-graphs-simple-graph")) },
  { get: () => trio(import("./graph-domain/wheel-graphs-simple-graph")) },
  { get: () => trio(import("./graph-domain/cube-graphs-simple-graph")) },
  {
    get: () =>
      trio(import("./graph-domain/complete-bipartite-graphs-simple-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/jobs-trained-matching-graph-simple-graph")),
  },
  {
    get: () =>
      trio(
        import("./graph-domain/jobs-trained-no-matching-graph-simple-graph")
      ),
  },
  {
    get: () => trio(import("./graph-domain/star-topology-graph-simple-graph")),
  },
  {
    get: () => trio(import("./graph-domain/ring-topology-graph-simple-graph")),
  },
  {
    get: () =>
      trio(import("./graph-domain/hybrid-topology-graph-simple-graph")),
  },
  { get: () => trio(import("./graph-domain/linear-array-graph-simple-graph")) },
  { get: () => trio(import("./graph-domain/mesh-network-graph-simple-graph")) },
  {
    get: () =>
      trio(import("./graph-domain/hypercube-network-graph-simple-graph")),
  },
  { get: () => trio(import("./graph-domain/union-graph-simple-graph")) },
  {
    get: () => trio(import("./geometry-domain/incenter-triangle-euclidean")),
    name: "Triangle Incenter",
    gallery: true,
  },
  {
    get: () => trio(import("./geometry-domain/collinear-euclidean")),
    name: "Collinear Points",
    gallery: true,
  },
  { get: () => trio(import("./geometry-domain/parallel-lines-euclidean")) },
  {
    get: () => trio(import("./geometry-domain/midsegment-triangles-euclidean")),
  },
  {
    get: () => trio(import("./geometry-domain/congruent-triangles-euclidean")),
  },
  {
    get: () =>
      trio(import("./triangle-mesh-3d/two-triangles-triangle-mesh-3d")),
    name: "Two 3D Triangles",
    gallery: true,
  },
  { get: () => trio(import("./geometry-domain/circle-example-euclidean")) },
  {
    get: () => trio(import("./word-cloud/word-cloud-example-word-cloud")),
    name: "Word Cloud",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./geometry-domain/siggraph-teaser-euclidean-teaser")),
    name: "SIGGRAPH teaser - Euclidean Geometry",
    gallery: true,
  },
  { get: () => trio(import("./minkowski-tests/maze/non-convex-non-convex")) },
  {
    get: () => trio(import("./lagrange-bases/lagrange-bases-lagrange-bases")),
    name: "Lagrange Bases",
    gallery: true,
  },
  { get: () => trio(import("./hypergraph/hypergraph-hypergraph")) },
  {
    get: () =>
      trio(
        import("./persistent-homology/persistent-homology-persistent-homology")
      ),
    name: "Persistent Homology",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./walk-on-spheres/wos-laplace-estimator-walk-on-spheres")),
    name: "Walk on Spheres - Laplace Estimator",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./walk-on-spheres/wos-poisson-estimator-walk-on-spheres")),
    name: "Walk on Spheres - Poisson Estimator",
  },
  {
    get: () =>
      trio(import("./walk-on-spheres/wos-nested-estimator-walk-on-spheres")),
    name: "Walk on Spheres - Nested Estimator",
  },
  {
    get: () =>
      trio(import("./walk-on-spheres/wos-offcenter-estimator-walk-on-spheres")),
    name: "Walk on Spheres - Off-Center Estimator",
  },
  {
    get: () =>
      trio(import("./shape-distance/points-around-star-shape-distance")),
  },
  {
    get: () =>
      trio(import("./shape-distance/points-around-polyline-shape-distance")),
  },
  {
    get: () =>
      trio(import("./shape-distance/points-around-line-shape-distance")),
  },
  {
    get: () =>
      trio(import("./shape-distance/lines-around-rect-rect-line-dist")),
  },
  {
    get: () => trio(import("./closest-point/closest-point-test-closest-point")),
    name: "Closest point test",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import("./fake-3d-linear-algebra/3d-projection-fake-3d-linear-algebra")
      ),
  },
  {
    get: () => trio(import("./exterior-algebra/vector-wedge-exterior-algebra")),
  },
  { get: () => trio(import("./animation/center-shrink-circle-animation")) },
  {
    get: () => trio(import("./structural-formula/caffeine-structural-formula")),
    name: "A Caffeine Molecule",
    gallery: true,
  },
  { get: () => trio(import("./mobius/mobius-mobius")) },
  { get: () => trio(import("./molecules/glutamine-molecules-basic")) },
  {
    get: () => trio(import("./matrix-ops/matrix-matrix-addition-matrix-ops")),
    name: "Matrix-matrix addition",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import("./matrix-ops/matrix-matrix-division-elementwise-matrix-ops")
      ),
    name: "Matrix-matrix division (elementwise)",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import(
          "./matrix-ops/matrix-matrix-multiplication-elementwise-matrix-ops"
        )
      ),
    name: "Matrix-matrix multiplication (elementwise)",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/matrix-matrix-multiplication-matrix-ops")),
    name: "Matrix-matrix multiplication",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/matrix-matrix-subtraction-matrix-ops")),
    name: "Matrix-matrix subtraction",
    gallery: false,
  },
  {
    get: () => trio(import("./matrix-ops/matrix-transpose-matrix-ops")),
    name: "Matrix transpose",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/matrix-vector-left-multiplication-matrix-ops")),
    name: "Matrix-vector left multiplication",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import("./matrix-ops/matrix-vector-right-multiplication-matrix-ops")
      ),
    name: "Matrix-vector right multiplication",
    gallery: false,
  },
  {
    get: () => trio(import("./matrix-ops/scalar-vector-division-matrix-ops")),
    name: "Scalar-vector division",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/scalar-vector-left-multiplication-matrix-ops")),
    name: "Scalar-vector left multiplication",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import("./matrix-ops/scalar-vector-right-multiplication-matrix-ops")
      ),
    name: "Scalar-vector right multiplication",
    gallery: false,
  },
  {
    get: () => trio(import("./matrix-ops/vector-vector-addition-matrix-ops")),
    name: "Vector-vector addition",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import("./matrix-ops/vector-vector-division-elementwise-matrix-ops")
      ),
    name: "Vector-vector division elementwise",
    gallery: false,
  },
  {
    get: () =>
      trio(
        import(
          "./matrix-ops/vector-vector-multiplication-elementwise-matrix-ops"
        )
      ),
    name: "Vector-vector multiplication elementwise",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/vector-vector-outerproduct-matrix-ops")),
    name: "Vector-vector outerproduct",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./matrix-ops/vector-vector-subtraction-matrix-ops")),
    name: "Vector-vector subtraction",
    gallery: false,
  },
  {
    get: () =>
      trio(import("./logic-circuit-domain/half-adder-distinctive-shape")),
  },
  { get: () => trio(import("./curve-examples/cubic-bezier-cubic-bezier")) },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/cotan-formula-triangle-mesh-2d")),
    name: "Cotan Formula",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/concyclic-pair-triangle-mesh-2d")),
    name: "Concyclic Euclidean Edge Flip",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/halfedge-mesh-triangle-mesh-2d")),
    name: "Half Edge Mesh",
    gallery: true,
  },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/relative-orientation-triangle-mesh-2d")),
  },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/triangle-centers-triangle-mesh-2d")),
    name: "Triangle Centers",
  },
  {
    get: () =>
      trio(import("./triangle-mesh-2d/angle-equivalence-triangle-mesh-2d")),
    name: "Rigid Conformal Mapping of a 2D Mesh",
  },
  {
    get: () => trio(import("./graph-domain/sec5-ex32-simple-curved-graph")),
    name: "Curved graph example",
  },
  {
    get: () => trio(import("./curve-examples/cubic-bezier-open-elastic-curve")),
    name: "Open elastic curve example",
  },
  {
    get: () =>
      trio(import("./curve-examples/cubic-bezier-closed-elastic-curve")),
    name: "Closed elastic curve example",
  },
  {
    get: () => trio(import("./graph-domain/arpanet-simple-curved-graph-dots")),
    name: "Curved graph example with dots",
  },
  {
    get: () =>
      trio(import("./graph-domain/nyc-subway-simple-curved-graph-boxes")),
    name: "Curved graph example with boxes",
  },
  { get: () => trio(import("./curve-examples/blobs-blobs")), name: "Blobs" },
];
