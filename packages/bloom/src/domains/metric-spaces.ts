import { domain } from "../core/program.js";

/** The four metrics used for the plane in Chapter 2 of Gemignani. */
export type PlaneMetric = "euclidean" | "taxicab" | "discrete" | "supremum";

const declarations = domain("metric-spaces");

const MetricPlane = declarations.type<"MetricPlane", { metric: PlaneMetric }>(
  "MetricPlane",
);

const Neighborhood = declarations.type<
  "Neighborhood",
  {
    point: readonly [number, number];
    rho: number;
    coordinateNames?: readonly [string, string];
    radiusLabel?: string;
  }
>("Neighborhood");

const InSpace = declarations.predicate("InSpace", [Neighborhood, MetricPlane]);
const NeighborhoodContainedIn = declarations.predicate(
  "NeighborhoodContainedIn",
  [Neighborhood, Neighborhood],
);

/** Example 5 uses every function between the two intervals, not just continuous ones. */
const FunctionSpace = declarations.type<
  "FunctionSpace",
  {
    metric: "uniform";
    domain: readonly [number, number];
    codomain: readonly [number, number];
  }
>("FunctionSpace");
const ScalarFunction = declarations.type("ScalarFunction");
const FunctionNeighborhood = declarations.type<
  "FunctionNeighborhood",
  { rho: number; radiusLabel?: string }
>("FunctionNeighborhood");
const FunctionInSpace = declarations.predicate("FunctionInSpace", [
  ScalarFunction,
  FunctionSpace,
]);
const FunctionNeighborhoodOf = declarations.predicate(
  "FunctionNeighborhoodOf",
  [FunctionNeighborhood, ScalarFunction],
);

const FunctionSequence = declarations
  .type("FunctionSequence")
  .withData<{ formula: string }>();
const PowerFunction = declarations
  .type("PowerFunction", ScalarFunction)
  .withData<{ exponent: number }>();
const EndpointLimitFunction = declarations.type(
  "EndpointLimitFunction",
  ScalarFunction,
);
const FunctionSequenceInSpace = declarations.predicate(
  "FunctionSequenceInSpace",
  [FunctionSequence, FunctionSpace],
);
const FunctionTerm = declarations.predicate("FunctionTerm", [
  PowerFunction,
  FunctionSequence,
]);
const PointwiseConvergesTo = declarations.predicate("PointwiseConvergesTo", [
  FunctionSequence,
  ScalarFunction,
  FunctionSpace,
]);
const FailsToConvergeUniformlyTo = declarations.predicate(
  "FailsToConvergeUniformlyTo",
  [FunctionSequence, ScalarFunction, FunctionSpace],
);

const MetricPoint = declarations.type<
  "MetricPoint",
  { point: readonly [number, number] }
>("MetricPoint");
const PointInSpace = declarations.predicate("PointInSpace", [
  MetricPoint,
  MetricPlane,
]);
const NeighborhoodAt = declarations.predicate("NeighborhoodAt", [
  Neighborhood,
  MetricPoint,
]);
const InsideNeighborhood = declarations.predicate("InsideNeighborhood", [
  MetricPoint,
  Neighborhood,
]);

const MetricLine = declarations.type<"MetricLine", { metric: "absolute" }>(
  "MetricLine",
);
const ClosedInterval = declarations.type<
  "ClosedInterval",
  { bounds: readonly [number, number] }
>("ClosedInterval");
const LinePoint = declarations.type<"LinePoint", { position: number }>(
  "LinePoint",
);
const LineNeighborhood = declarations.type<
  "LineNeighborhood",
  { center: number; rho: number }
>("LineNeighborhood");
const IntervalInLine = declarations.predicate("IntervalInLine", [
  ClosedInterval,
  MetricLine,
]);
const LinePointInSpace = declarations.predicate("LinePointInSpace", [
  LinePoint,
  MetricLine,
]);
const OutsideInterval = declarations.predicate("OutsideInterval", [
  LinePoint,
  ClosedInterval,
]);
const LineNeighborhoodAt = declarations.predicate("LineNeighborhoodAt", [
  LineNeighborhood,
  LinePoint,
]);
const LineNeighborhoodInSpace = declarations.predicate(
  "LineNeighborhoodInSpace",
  [LineNeighborhood, MetricLine],
);
const LineNeighborhoodAvoidsInterval = declarations.predicate(
  "LineNeighborhoodAvoidsInterval",
  [LineNeighborhood, ClosedInterval],
);

/** Infinite sequences are mathematical objects; styles may draw a finite prefix. */
const PointSequence = declarations.type<"PointSequence", { formula?: string }>(
  "PointSequence",
);
const SequenceTail = declarations.type<"SequenceTail", { after: number }>(
  "SequenceTail",
);
const SequenceInPlane = declarations.predicate("SequenceInPlane", [
  PointSequence,
  MetricPlane,
]);
const ConvergesTo = declarations.predicate("ConvergesTo", [
  PointSequence,
  MetricPoint,
  MetricPlane,
]);
const TailOfSequence = declarations.predicate("TailOfSequence", [
  SequenceTail,
  PointSequence,
]);
const TailInNeighborhood = declarations.predicate("TailInNeighborhood", [
  SequenceTail,
  Neighborhood,
]);

/** Hypotheses in the uniqueness proof, deliberately separate from ConvergesTo. */
const ProposedLimitOf = declarations.predicate("ProposedLimitOf", [
  PointSequence,
  MetricPoint,
  MetricPlane,
]);
const DistinctMetricPoints = declarations.predicate("DistinctMetricPoints", [
  MetricPoint,
  MetricPoint,
]);
const DisjointNeighborhoods = declarations.predicate("DisjointNeighborhoods", [
  Neighborhood,
  Neighborhood,
]);

/** Generic metric spaces and their points have no drawing coordinates. */
const MetricSpace = declarations
  .type("MetricSpace")
  .withData<{ metricLabel?: string }>();
const SpacePoint = declarations.type("SpacePoint");
const SpaceNeighborhood = declarations.type("SpaceNeighborhood").withData<{
  rho: number;
  radiusLabel?: string;
}>();
const MetricMap = declarations
  .type("MetricMap")
  .withData<{ formula?: string }>();
const SpacePointInSpace = declarations.predicate("SpacePointInSpace", [
  SpacePoint,
  MetricSpace,
]);
const SpaceNeighborhoodInSpace = declarations.predicate(
  "SpaceNeighborhoodInSpace",
  [SpaceNeighborhood, MetricSpace],
);
const SpaceNeighborhoodAt = declarations.predicate("SpaceNeighborhoodAt", [
  SpaceNeighborhood,
  SpacePoint,
]);
const InsideSpaceNeighborhood = declarations.predicate(
  "InsideSpaceNeighborhood",
  [SpacePoint, SpaceNeighborhood],
);
const MapBetweenSpaces = declarations.predicate("MapBetweenSpaces", [
  MetricMap,
  MetricSpace,
  MetricSpace,
]);
const MapsPoint = declarations.predicate("MapsPoint", [
  MetricMap,
  SpacePoint,
  SpacePoint,
]);
const ContinuousAt = declarations.predicate("ContinuousAt", [
  MetricMap,
  SpacePoint,
]);
const MapsNeighborhoodInto = declarations.predicate("MapsNeighborhoodInto", [
  MetricMap,
  SpaceNeighborhood,
  SpaceNeighborhood,
]);

/** A coordinate projection's inverse image is infinite in the other coordinate. */
const CoordinateProjection = declarations
  .type("CoordinateProjection")
  .withData<{ coordinate: 0 | 1 }>();
const VerticalOpenStrip = declarations
  .type("VerticalOpenStrip")
  .withData<{ center: number; halfWidth: number }>();
const ProjectionBetween = declarations.predicate("ProjectionBetween", [
  CoordinateProjection,
  MetricPlane,
  MetricLine,
]);
const StripInPlane = declarations.predicate("StripInPlane", [
  VerticalOpenStrip,
  MetricPlane,
]);
const ProjectionInverseImage = declarations.predicate(
  "ProjectionInverseImage",
  [VerticalOpenStrip, CoordinateProjection, LineNeighborhood],
);

/** Mathematical facts are independent of the diagram's layout and appearance. */
export const metricSpaces = declarations.make({
  MetricPlane,
  Neighborhood,
  InSpace,
  NeighborhoodContainedIn,
  FunctionSpace,
  ScalarFunction,
  FunctionNeighborhood,
  FunctionInSpace,
  FunctionNeighborhoodOf,
  FunctionSequence,
  PowerFunction,
  EndpointLimitFunction,
  FunctionSequenceInSpace,
  FunctionTerm,
  PointwiseConvergesTo,
  FailsToConvergeUniformlyTo,
  MetricPoint,
  PointInSpace,
  NeighborhoodAt,
  InsideNeighborhood,
  MetricLine,
  ClosedInterval,
  LinePoint,
  LineNeighborhood,
  IntervalInLine,
  LinePointInSpace,
  OutsideInterval,
  LineNeighborhoodAt,
  LineNeighborhoodInSpace,
  LineNeighborhoodAvoidsInterval,
  PointSequence,
  SequenceTail,
  SequenceInPlane,
  ConvergesTo,
  TailOfSequence,
  TailInNeighborhood,
  ProposedLimitOf,
  DistinctMetricPoints,
  DisjointNeighborhoods,
  MetricSpace,
  SpacePoint,
  SpaceNeighborhood,
  MetricMap,
  SpacePointInSpace,
  SpaceNeighborhoodInSpace,
  SpaceNeighborhoodAt,
  InsideSpaceNeighborhood,
  MapBetweenSpaces,
  MapsPoint,
  ContinuousAt,
  MapsNeighborhoodInto,
  CoordinateProjection,
  VerticalOpenStrip,
  ProjectionBetween,
  StripInPlane,
  ProjectionInverseImage,
});

/** The numerical distance function, useful for checking a substance program. */
export function planeDistance(
  metric: PlaneMetric,
  x: readonly [number, number],
  y: readonly [number, number],
): number {
  const dx = Math.abs(x[0] - y[0]);
  const dy = Math.abs(x[1] - y[1]);
  switch (metric) {
    case "euclidean":
      return Math.hypot(dx, dy);
    case "taxicab":
      return dx + dy;
    case "discrete":
      return dx === 0 && dy === 0 ? 0 : 1;
    case "supremum":
      return Math.max(dx, dy);
  }
}

/** Gemignani's N(x, ρ) uses the strict inequality D(x, y) < ρ. */
export const inNeighborhood = (
  metric: PlaneMetric,
  center: readonly [number, number],
  rho: number,
  point: readonly [number, number],
) => rho > 0 && planeDistance(metric, center, point) < rho;

/** Disjoint open neighborhoods used to contradict two distinct proposed limits. */
export function separatedLimitRadius(
  metric: PlaneMetric,
  y: readonly [number, number],
  other: readonly [number, number],
): number {
  if (![...y, ...other].every(Number.isFinite))
    throw new Error("Proposed limit coordinates must be finite");
  const radius = planeDistance(metric, y, other) / 2;
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error(
      "The proposed limits must be distinct with finite separation",
    );
  return radius;
}

/** f(x,y)=x: no condition or finite clipping bound is imposed on y. */
export function inProjectionInverseImage(
  point: readonly [number, number],
  center: number,
  rho: number,
): boolean {
  if (![...point, center, rho].every(Number.isFinite) || !(rho > 0))
    throw new Error(
      "Projection neighborhoods require finite data and positive radius",
    );
  return Math.abs(point[0] - center) < rho;
}

/** Example 13: a sequence that converges pointwise but not in the uniform metric. */
export function powerSequenceTerm(n: number, x: number): number {
  if (
    !Number.isSafeInteger(n) ||
    n < 1 ||
    !Number.isFinite(x) ||
    x < 0 ||
    x > 1
  ) {
    throw new Error("Power sequence needs a positive integer n and x in [0,1]");
  }
  return x ** n;
}

export function powerSequencePointwiseLimit(x: number): number {
  if (!Number.isFinite(x) || x < 0 || x > 1)
    throw new Error("The limit function has domain [0,1]");
  return x === 1 ? 1 : 0;
}

/** The supremum is 1 for every n, although no point attains this difference. */
export function powerSequenceUniformDistance(n: number): number {
  powerSequenceTerm(n, 0);
  return 1;
}

/** The positive radius used in the proof that an open metric ball is open. */
export function interiorNeighborhoodRadius(
  metric: PlaneMetric,
  center: readonly [number, number],
  interior: readonly [number, number],
  rho: number,
): number {
  if (
    !center.every(Number.isFinite) ||
    !interior.every(Number.isFinite) ||
    !Number.isFinite(rho)
  ) {
    throw new Error("Neighborhood data must be finite");
  }
  const q = rho - planeDistance(metric, center, interior);
  if (!(q > 0))
    throw new Error("The chosen point must lie inside the open neighborhood");
  return q;
}

/** Distance from an exterior point to a nonempty closed interval. */
export function intervalComplementRadius(
  x: number,
  bounds: readonly [number, number],
): number {
  const [a, b] = bounds;
  if (![x, a, b].every(Number.isFinite) || a > b || !(x < a || x > b)) {
    throw new Error("The point must lie outside a finite closed interval");
  }
  return Math.min(Math.abs(x - a), Math.abs(b - x));
}

/** Example 12's actual mathematical sequence, rather than a drawing sampler. */
export function reciprocalHeightTerm(n: number): readonly [number, number] {
  if (!Number.isInteger(n) || n < 1)
    throw new Error("Sequence indices are positive integers");
  return [1, 1 / n];
}

/** A positive integer M strictly greater than 1/ρ, as used in Example 12. */
export function reciprocalConvergenceThreshold(rho: number): number {
  const threshold = Math.floor(1 / rho) + 1;
  if (!(rho > 0) || !Number.isFinite(rho) || !Number.isSafeInteger(threshold)) {
    throw new Error(
      "The neighborhood radius must admit a finite safe integer threshold",
    );
  }
  return threshold;
}
