export * from "./core/builder.js";
export * from "./core/computation.js";
export * as constraints from "./core/constraints.js";
export * from "./core/diagram.js";
export * as objectives from "./core/objectives.js";
export * from "./core/program.js";
export * from "./core/types.js";
export { canvas } from "./core/utils.js";
export * from "./domains/finite-topology.js";
export * from "./domains/metric-completeness.js";
export * from "./domains/metric-covers.js";
export * from "./domains/metric-spaces.js";
export * from "./domains/point-set-topology.js";
export * from "./domains/set-theory.js";
export * from "./domains/surface-topology.js";
export * from "./domains/topological-vocabulary.js";
export * from "./styles/bounded-sets.js";
export * from "./styles/closed-interval.js";
export * from "./styles/convergence.js";
export * from "./styles/countable-enumeration.js";
export * from "./styles/covering-properties.js";
export * from "./styles/derived-sets.js";
export * from "./styles/dyadic-neighborhoods.js";
export * from "./styles/endpoint-identification.js";
export * from "./styles/function-neighborhoods.js";
export * from "./styles/function-sequences.js";
export * from "./styles/identification-sources.js";
export * from "./styles/limit-uniqueness.js";
export * from "./styles/local-compactness.js";
export * from "./styles/lower-limit-topology.js";
export * from "./styles/metric-comparison.js";
export * from "./styles/metric-continuity.js";
export * from "./styles/metric-neighborhoods.js";
export * from "./styles/neighborhood-inclusion.js";
export * from "./styles/oscillating-sine-curve.js";
export * from "./styles/parametric-surfaces.js";
export * from "./styles/path-extensions.js";
export * from "./styles/point-set-topology.js";
export * from "./styles/product-surfaces.js";
export * from "./styles/product-topology.js";
export * from "./styles/projection-preimage.js";
export * from "./styles/quotient-spaces.js";
export * from "./styles/separation-axioms.js";
export * from "./styles/sequence-convergence.js";
export * from "./styles/set-theory.js";
export * from "./styles/subspaces.js";
export * from "./styles/topological-projections.js";
export * from "./styles/topology-bases.js";

export {
  acos,
  add,
  and,
  asin,
  atan,
  cos,
  div,
  eq,
  exp,
  gt,
  gte,
  ifCond,
  ln,
  lt,
  lte,
  mul,
  neg,
  not,
  ops,
  or,
  pow,
  sin,
  sqrt,
  sub,
  tan,
} from "@penrose/core";
export * from "./styles/baire-category.js";
export * from "./styles/components.js";
export * from "./styles/connected-closures.js";
export * from "./styles/connected-products.js";
export * from "./styles/connectedness.js";
export * from "./styles/contraction-iterates.js";
export * from "./styles/convexity.js";
export * from "./styles/directed-intersections.js";
export * from "./styles/fundamental-groups.js";
export * from "./styles/homotopies.js";
export * from "./styles/homotopy-constructions.js";
export * from "./styles/homotopy-equivalence.js";
export * from "./styles/metric-completeness.js";
export * from "./styles/metric-covers.js";
export * from "./styles/planar-graph-spaces.js";
export * from "./styles/separated-disks.js";

export {
  associativeLoopParameter,
  concatenatedLoopParameter,
  inverseCancellationParameter,
  trigonometricLoopValue,
  unitLoopParameter,
} from "./domains/loop-operations.js";
export {
  circularBasedLoop,
  piecewisePlaneLoopData,
  piecewisePlaneLoopValue,
  planeCurveSegmentValue,
  planeLoopContractionValue,
  planeLoopDiskBound,
  type CircularPlaneSegment,
  type CubicPlaneSegment,
  type PiecewisePlaneLoopData,
  type PlaneCurvePoint,
  type PlaneCurveSegment,
  type PlaneLoopDiskBound,
  type PlaneLoopDiskBoundOptions,
} from "./domains/plane-curves.js";
export {
  decimalTableStyle,
  decimalTableStyleFor,
  type DecimalTableStyleOptions,
} from "./styles/decimal-tables.js";
export { groupKernelStyle } from "./styles/group-kernels.js";
export {
  groupTableStyle,
  groupTableStyleFor,
  type GroupTableStyleOptions,
} from "./styles/group-tables.js";
export { inverseCancellationStyle } from "./styles/loop-retracing.js";
export {
  TrigonometricLoopView,
  inverseLoopStyle,
  loopFamilyStyle,
  loopOperationStyle,
  loopStyle,
} from "./styles/loops.js";
export {
  PiecewisePlaneLoopView,
  planeLoopFamilyStyle,
  planeLoopPathData,
  type PlaneLoopStyleOptions,
} from "./styles/plane-curves.js";
