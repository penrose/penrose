import { domain, proposition, type EntityOf } from "../core/program.js";
import { declareSetTheory } from "./set-theory.js";

export type PlaneCoordinates = readonly [number, number];

const declarations = domain("point-set-topology");
const sets = declareSetTheory(declarations);
export interface SubsetCollapseData {
  collapsed: EntityOf<typeof sets.Set>;
}
const OpenSet = declarations.type("OpenSet", sets.Set);
const ClosedSet = declarations.type("ClosedSet", sets.Set);
const CoordinatePoint = declarations
  .type("CoordinatePoint", sets.Point)
  .withData<{ coordinates: PlaneCoordinates }>();
const OpenDisk = declarations
  .type("OpenDisk", OpenSet)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const Singleton = declarations.type("Singleton", ClosedSet);
const ClopenSingleton = declarations.type(
  "ClopenSingleton",
  Singleton,
  OpenSet,
);
const ClosedRegion = declarations.type("ClosedRegion", ClosedSet);
const Neighborhood = declarations.type("Neighborhood", OpenSet);
const DiskNeighborhood = declarations.type(
  "DiskNeighborhood",
  OpenDisk,
  Neighborhood,
);
const NeighborhoodUnion = declarations.type("NeighborhoodUnion", OpenSet);
const ReciprocalGraph = declarations
  .type("ReciprocalGraph", ClosedSet)
  .withData<{ coefficient: number }>();
const AffineLine = declarations
  .type("AffineLine", ClosedSet)
  .withData<{ coefficients: readonly [number, number, number] }>();

export interface RealIntervalData {
  a: number;
  b: number;
  leftClosed: boolean;
  rightClosed: boolean;
  endpointNames?: readonly [string, string];
}

const RealInterval = declarations
  .type("RealInterval", sets.Set)
  .withData<RealIntervalData>();
const OpenInterval = declarations
  .type("OpenInterval", RealInterval, OpenSet)
  .withData<{ leftClosed: false; rightClosed: false }>();
const ClosedInterval = declarations
  .type("ClosedInterval", RealInterval, ClosedSet)
  .withData<{ leftClosed: true; rightClosed: true }>();
const HalfLine = declarations.type("HalfLine", OpenSet).withData<{
  bound: number;
  direction: "left" | "right";
  boundName?: string;
}>();
const ClopenHalfLine = declarations.type("ClopenHalfLine", HalfLine, ClosedSet);
/** An open half-plane a*x + b*y < c; a and b cannot both be zero. */
const OpenHalfPlane = declarations
  .type("OpenHalfPlane", OpenSet)
  .withData<{ coefficients: readonly [number, number, number] }>();
const OpenSquare = declarations
  .type("OpenSquare", OpenSet)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const OpenTriangle = declarations.type("OpenTriangle", OpenSet).withData<{
  vertices: readonly [PlaneCoordinates, PlaneCoordinates, PlaneCoordinates];
}>();
const ClosedDisk = declarations
  .type("ClosedDisk", ClosedSet)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const CircleBoundary = declarations
  .type("CircleBoundary", ClosedSet)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const DiskExterior = declarations
  .type("DiskExterior", OpenSet)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const EndpointPair = declarations
  .type("EndpointPair", ClosedSet)
  .withData<{ endpoints: readonly [number, number] }>();
const Topology = declarations.type("Topology");
const SetFamily = declarations.type("SetFamily");
const FiniteSetFamily = declarations.type("FiniteSetFamily", SetFamily);
const OpenCover = declarations.type("OpenCover", SetFamily);
const FiniteOpenCover = declarations.type(
  "FiniteOpenCover",
  OpenCover,
  FiniteSetFamily,
);
const MetricBallFamily = declarations
  .type("MetricBallFamily", SetFamily)
  .withData<{ radius: number }>();
const MetricClosureFamily = declarations
  .type("MetricClosureFamily", SetFamily)
  .withData<{ radius: number }>();
const SeparatedSubset = declarations
  .type("SeparatedSubset", sets.Set)
  .withData<{ separation: number }>();
const MaximalSeparatedSubset = declarations.type(
  "MaximalSeparatedSubset",
  SeparatedSubset,
);
/** Points whose initial intervals admit a finite subcover of the specified cover. */
const FiniteCoverReachSet = declarations.type("FiniteCoverReachSet", sets.Set);
const NeighborhoodSystem = declarations.type("NeighborhoodSystem", SetFamily);
const DirectedSet = declarations.type("DirectedSet", sets.Set);
const NeighborhoodDirectedSet = declarations
  .type("NeighborhoodDirectedSet", DirectedSet)
  .withData<{ order: "reverse-inclusion" }>();
const ProductDirectedSet = declarations.type("ProductDirectedSet", DirectedSet);
const PartitionDirectedSet = declarations
  .type("PartitionDirectedSet", DirectedSet)
  .withData<{ order: "finer-first" }>();
const IntervalPartition = declarations
  .type("IntervalPartition", sets.Set)
  .withData<{ points: readonly number[] }>();
const Net = declarations.type("Net");
const Ultranet = declarations.type("Ultranet", Net);
const Filter = declarations.type("Filter", SetFamily);
const FilterBase = declarations.type("FilterBase", SetFamily);
const Ultrafilter = declarations.type("Ultrafilter", Filter);
const Basis = declarations.type("Basis", SetFamily);
const Subbasis = declarations.type("Subbasis", SetFamily);

const TopologyOn = declarations.predicate("TopologyOn", [Topology, sets.Set]);
const BasisFor = declarations.predicate("BasisFor", [Basis, Topology]);
const SubbasisFor = declarations.predicate("SubbasisFor", [Subbasis, Topology]);
const SetInFamily = declarations.predicate("SetInFamily", [
  sets.Set,
  SetFamily,
]);
const FiniteIntersectionOf = declarations.predicate("FiniteIntersectionOf", [
  sets.Set,
  FiniteSetFamily,
]);
const UnionOf = declarations.predicate("UnionOf", [sets.Set, SetFamily]);
const OpenCoverOf = declarations.predicate("OpenCoverOf", [
  OpenCover,
  sets.Set,
  Topology,
]);
const SubfamilyOf = declarations.predicate("SubfamilyOf", [
  SetFamily,
  SetFamily,
]);
const FamilyIncludedIn = declarations.predicate("FamilyIncludedIn", [
  SetFamily,
  SetFamily,
]);
const MetricBallsAt = declarations.predicate("MetricBallsAt", [
  MetricBallFamily,
  SeparatedSubset,
  Topology,
]);
const ClosuresOfFamily = declarations.predicate("ClosuresOfFamily", [
  MetricClosureFamily,
  MetricBallFamily,
  Topology,
]);
const SeparatedIn = declarations.predicate("SeparatedIn", [
  SeparatedSubset,
  sets.Set,
  Topology,
]);
/** Each center belongs only to its own ball among the named covering sets. */
const IndispensableCenteredCover = declarations.predicate(
  "IndispensableCenteredCover",
  [OpenCover, MetricBallFamily, SeparatedSubset, OpenSet],
);
const FiniteCoverReachOf = declarations.predicate("FiniteCoverReachOf", [
  FiniteCoverReachSet,
  OpenCover,
  sets.Set,
]);
const EqualTopologies = declarations.predicate("EqualTopologies", [
  Topology,
  Topology,
]);
const InteriorOf = declarations.predicate("InteriorOf", [
  OpenSet,
  sets.Set,
  Topology,
]);
const ClosureOf = declarations.predicate("ClosureOf", [
  ClosedSet,
  sets.Set,
  Topology,
]);
const FrontierOf = declarations.predicate("FrontierOf", [
  ClosedSet,
  sets.Set,
  Topology,
]);
const ExteriorOf = declarations.predicate("ExteriorOf", [
  OpenSet,
  sets.Set,
  Topology,
]);
const DerivedSetOf = declarations.predicate("DerivedSetOf", [
  sets.Set,
  sets.Set,
  Topology,
]);

/** Openness and closedness are relative to the declared topology. */
const OpenIn = declarations.predicate("OpenIn", [OpenSet, Topology]);
const ClosedIn = declarations.predicate("ClosedIn", [ClosedSet, Topology]);
const T0 = declarations.predicate("T0", [Topology]);
const T1 = declarations.predicate("T1", [Topology]);
const T2 = declarations.predicate("T2", [Topology]);
/** Gemignani's T3 separates a point and a closed set; it does not assume T1. */
const T3 = declarations.predicate("T3", [Topology]);
/** In this edition, Regular means both T3 and T1. */
const Regular = declarations.predicate("Regular", [Topology]);
const NotT3 = declarations.predicate("NotT3", [Topology]);
const T4 = declarations.predicate("T4", [Topology]);
const Normal = declarations.predicate("Normal", [Topology]);
const NotNormal = declarations.predicate("NotNormal", [Topology]);
const NotCompact = declarations.predicate("NotCompact", [Topology]);
const LocallyCompact = declarations.predicate("LocallyCompact", [Topology]);
const LocallyCompactIn = declarations.predicate("LocallyCompactIn", [
  sets.Set,
  Topology,
]);
const NotLocallyCompact = declarations.predicate("NotLocallyCompact", [
  Topology,
]);
const AssumedCompactIn = declarations.predicate("AssumedCompactIn", [
  sets.Set,
  Topology,
]);
/** A specific containing metric neighborhood witnesses boundedness. */
const BoundedBy = declarations.predicate("BoundedBy", [sets.Set, OpenSet]);
const CompactNeighborhoodWithin = declarations.predicate(
  "CompactNeighborhoodWithin",
  [sets.Point, Neighborhood, OpenSet, sets.Set, Topology],
);
const HasNoFiniteSubcover = declarations.predicate("HasNoFiniteSubcover", [
  OpenCover,
  sets.Set,
  Topology,
]);
const Compact = declarations.predicate("Compact", [Topology]);
const CompactIn = declarations.predicate("CompactIn", [sets.Set, Topology]);
const Lindelof = declarations.predicate("Lindelof", [Topology]);
const Countable = declarations.predicate("Countable", [sets.Set]);
const Discrete = declarations.predicate("Discrete", [Topology]);
const Nonempty = declarations.predicate("Nonempty", [sets.Set]);
const IntersectionOf = declarations.predicate("IntersectionOf", [
  sets.Set,
  sets.Set,
  sets.Set,
]);
const SetSeparation = declarations.predicate("SetSeparation", [
  sets.Set,
  sets.Set,
  OpenSet,
  OpenSet,
  Topology,
]);
const PointClosedSetSeparation = declarations.predicate(
  "PointClosedSetSeparation",
  [sets.Point, ClosedSet, Neighborhood, OpenSet, Topology],
);
const ComplementOf = declarations.predicate("ComplementOf", [
  sets.Set,
  sets.Set,
  sets.Set,
]);
/** A chosen witness V with x in V and Cl(V) contained in U. */
const NeighborhoodClosureWithin = declarations.predicate(
  "NeighborhoodClosureWithin",
  [Neighborhood, ClosedSet, Neighborhood, sets.Point, Topology],
);
const IntersectionWitness = declarations.predicate("IntersectionWitness", [
  sets.Point,
  OpenSet,
  OpenSet,
  Topology,
]);

const SingletonOf = declarations.predicate("SingletonOf", [
  Singleton,
  sets.Point,
]);
const BoundaryPoint = declarations.predicate("BoundaryPoint", [
  sets.Point,
  sets.Set,
]);
const Outside = declarations.predicate("Outside", [sets.Point, sets.Set]);
const NeighborhoodOf = declarations.predicate("NeighborhoodOf", [
  Neighborhood,
  sets.Point,
]);
const HausdorffNeighborhoodPair = declarations.predicate(
  "HausdorffNeighborhoodPair",
  [sets.Point, sets.Point, Neighborhood, Neighborhood, Topology],
);
const ZeroSetDistance = declarations.predicate("ZeroSetDistance", [
  sets.Set,
  sets.Set,
]);
const ZeroPointDistance = declarations.predicate("ZeroPointDistance", [
  sets.Point,
  sets.Set,
]);
/** U = N(x, D(x,F)/2), V = union over y in F of N(y, D(x,F)/2). */
const PointClosedSeparation = declarations.predicate("PointClosedSeparation", [
  sets.Point,
  ClosedSet,
  Neighborhood,
  NeighborhoodUnion,
]);
/** U and V are the unions of the half-distance neighborhoods in Proposition 13. */
const ClosedSetsSeparation = declarations.predicate("ClosedSetsSeparation", [
  ClosedSet,
  ClosedSet,
  NeighborhoodUnion,
  NeighborhoodUnion,
]);

/** Maps and quotient constructions reuse the existing set, point and topology vocabulary. */
const TopologicalMap = declarations
  .type("TopologicalMap")
  .withData<{ formula?: string }>();
const MapBetween = declarations.predicate("MapBetween", [
  TopologicalMap,
  sets.Set,
  sets.Set,
]);
const MapsTo = declarations.predicate("MapsTo", [
  TopologicalMap,
  sets.Point,
  sets.Point,
]);
const ContinuousMap = declarations.predicate("ContinuousMap", [
  TopologicalMap,
  Topology,
  Topology,
]);
const Homeomorphism = declarations.predicate("Homeomorphism", [
  TopologicalMap,
  Topology,
  Topology,
]);
const InverseMaps = declarations.predicate("InverseMaps", [
  TopologicalMap,
  TopologicalMap,
]);
const LinearSegment = declarations
  .type("LinearSegment", ClosedSet)
  .withData<{ endpoints: readonly [PlaneCoordinates, PlaneCoordinates] }>();
const TriangleBoundary = declarations
  .type("TriangleBoundary", ClosedSet)
  .withData<{
    vertices: readonly [PlaneCoordinates, PlaneCoordinates, PlaneCoordinates];
  }>();
const CentralProjection = declarations
  .type("CentralProjection", TopologicalMap)
  .withData<{ center: PlaneCoordinates }>();
const ProjectionCenter = declarations.predicate("ProjectionCenter", [
  CentralProjection,
  CoordinatePoint,
]);
const CentralProjectionBetween = declarations.predicate(
  "CentralProjectionBetween",
  [CentralProjection, LinearSegment, LinearSegment],
);
const RadialProjection = declarations
  .type("RadialProjection", TopologicalMap)
  .withData<{ center: PlaneCoordinates }>();
const RadialProjectionBetween = declarations.predicate(
  "RadialProjectionBetween",
  [RadialProjection, TriangleBoundary, CircleBoundary],
);
const EquivalenceRelation = declarations
  .type("EquivalenceRelation")
  .withData<{ rule?: string }>();
const EndpointIdentification = declarations
  .type("EndpointIdentification", EquivalenceRelation)
  .withData<{ bounds: readonly [number, number] }>();
/** Every point of the named subset forms one class; other points are singletons. */
const SubsetCollapseRelation = declarations
  .type("SubsetCollapseRelation", EquivalenceRelation)
  .withData<SubsetCollapseData>();
/** Antipodal boundary points are identified; interior points remain singletons. */
const AntipodalBoundaryIdentification = declarations
  .type("AntipodalBoundaryIdentification", EquivalenceRelation)
  .withData<{ center: PlaneCoordinates; radius: number }>();
const ClosedRectangle = declarations
  .type("ClosedRectangle", ClosedSet)
  .withData<{ bounds: readonly [number, number, number, number] }>();
const ClosedPolygon = declarations
  .type("ClosedPolygon", ClosedSet)
  .withData<{ vertices: readonly PlaneCoordinates[] }>();
const PolygonBoundary = declarations
  .type("PolygonBoundary", ClosedSet)
  .withData<{ vertices: readonly PlaneCoordinates[] }>();
const EuclideanPlane = declarations.type("EuclideanPlane", sets.Set);
/** The locus x-y belongs to period*Z; it is a subset, not a partition of R². */
const IntegerDifferenceLocus = declarations
  .type("IntegerDifferenceLocus", sets.Set)
  .withData<{ period: number }>();
const BoundaryOf = declarations.predicate("BoundaryOf", [ClosedSet, sets.Set]);
const QuotientSpace = declarations.type("QuotientSpace", sets.Set);
/** A class is a set of source points and a point of the quotient. */
const EquivalenceClass = declarations.type(
  "EquivalenceClass",
  sets.Set,
  sets.Point,
);
const QuotientMap = declarations.type("QuotientMap", TopologicalMap);
const EquivalenceOn = declarations.predicate("EquivalenceOn", [
  EquivalenceRelation,
  sets.Set,
]);
const EquivalentUnder = declarations.predicate("EquivalentUnder", [
  sets.Point,
  sets.Point,
  EquivalenceRelation,
]);
const QuotientOf = declarations.predicate("QuotientOf", [
  QuotientSpace,
  sets.Set,
  EquivalenceRelation,
]);
const ClassOf = declarations.predicate("ClassOf", [
  EquivalenceClass,
  sets.Point,
  EquivalenceRelation,
]);
const ClassInQuotient = declarations.predicate("ClassInQuotient", [
  EquivalenceClass,
  QuotientSpace,
]);
const IdentificationMap = declarations.predicate("IdentificationMap", [
  QuotientMap,
  sets.Set,
  QuotientSpace,
  EquivalenceRelation,
]);
const IdentificationTopology = declarations.predicate(
  "IdentificationTopology",
  [Topology, Topology, QuotientMap],
);
const FactorsThrough = declarations.predicate("FactorsThrough", [
  TopologicalMap,
  QuotientMap,
  TopologicalMap,
]);
const CircleParameterization = declarations
  .type("CircleParameterization", TopologicalMap)
  .withData<{
    bounds: readonly [number, number];
    center: PlaneCoordinates;
    radius: number;
  }>();
const CircleParameterizes = declarations.predicate("CircleParameterizes", [
  CircleParameterization,
  RealInterval,
  CircleBoundary,
]);

/** Binary products, coordinate projections and subspaces share the set context. */
const RealLine = declarations.type("RealLine", sets.Set);
const RealPoint = declarations.type("RealPoint", sets.Point).withData<{
  coordinate: number;
}>();
/** The expression is exact mathematical data; approximation is only for order/display checks. */
const IrrationalPoint = declarations
  .type("IrrationalPoint", sets.Point)
  .withData<{ expression: string; approximation: number }>();
const IrrationalInInterval = declarations.predicate("IrrationalInInterval", [
  IrrationalPoint,
  OpenInterval,
]);
/** A contradiction hypothesis, deliberately not a true supremum assertion. */
const AssumedSupremumOf = declarations.predicate("AssumedSupremumOf", [
  RealPoint,
  FiniteCoverReachSet,
]);
const InitialIntervalAt = declarations.predicate("InitialIntervalAt", [
  RealInterval,
  RealPoint,
]);
/** Nonzero points have usual interval neighborhoods; zero excludes all c/n. */
const DeletedReciprocalTopology = declarations
  .type("DeletedReciprocalTopology", Topology)
  .withData<{ coefficient: number }>();
/** The infinite set {c/n : n is a positive integer}; closed in the deleted topology. */
const ReciprocalSequenceSet = declarations
  .type("ReciprocalSequenceSet", ClosedSet)
  .withData<{ coefficient: number }>();
const DeletedReciprocalNeighborhood = declarations
  .type("DeletedReciprocalNeighborhood", Neighborhood)
  .withData<{ radius: number }>();
const OpenIntervalNeighborhood = declarations.type(
  "OpenIntervalNeighborhood",
  OpenInterval,
  Neighborhood,
);
const ReciprocalSetFor = declarations.predicate("ReciprocalSetFor", [
  ReciprocalSequenceSet,
  DeletedReciprocalTopology,
]);
const DeletedReciprocalNeighborhoodOf = declarations.predicate(
  "DeletedReciprocalNeighborhoodOf",
  [
    DeletedReciprocalNeighborhood,
    RealPoint,
    ReciprocalSequenceSet,
    DeletedReciprocalTopology,
  ],
);
/** No disjoint open pair separates this point and closed set. */
const UnseparablePointClosedSet = declarations.predicate(
  "UnseparablePointClosedSet",
  [sets.Point, ClosedSet, Topology],
);
const ProductSet = declarations.type("ProductSet", sets.Set);
const OpenProductSet = declarations.type("OpenProductSet", ProductSet, OpenSet);
const ClosedProductRectangle = declarations.type(
  "ClosedProductRectangle",
  ClosedRectangle,
  ProductSet,
);
const Subspace = declarations.type("Subspace", sets.Set);
const OpenSubspace = declarations.type("OpenSubspace", Subspace, OpenSet);
const RationalSubspace = declarations.type("RationalSubspace", Subspace);
const RationalRayCoverAt = declarations.predicate("RationalRayCoverAt", [
  OpenCover,
  IrrationalPoint,
  sets.Set,
  RationalSubspace,
  Topology,
]);
const LocallyClosedIn = declarations.predicate("LocallyClosedIn", [
  Subspace,
  sets.Set,
  Topology,
]);
const ProductPoint = declarations.type("ProductPoint", sets.Point);
const ProductPairOf = declarations.predicate("ProductPairOf", [
  ProductPoint,
  sets.Point,
  sets.Point,
]);
export interface SineCurveData {
  frequency: number;
  amplitude: number;
}
const OscillatingSineGraph = declarations
  .type("OscillatingSineGraph", sets.Set)
  .withData<SineCurveData>();
const OriginAdjoinedSineCurve = declarations
  .type("OriginAdjoinedSineCurve", Subspace)
  .withData<SineCurveData>();
const OscillatingSineMap = declarations
  .type("OscillatingSineMap", TopologicalMap)
  .withData<SineCurveData>();
const OscillationSequence = declarations
  .type("OscillationSequence", Net)
  .withData<{ height: number }>();
const SineCurveImageOf = declarations.predicate("SineCurveImageOf", [
  OriginAdjoinedSineCurve,
  OscillatingSineGraph,
  CoordinatePoint,
]);
const SineGraphMapOn = declarations.predicate("SineGraphMapOn", [
  OscillatingSineMap,
  HalfLine,
  ClopenHalfLine,
  OscillatingSineGraph,
]);
const OscillationSequenceIn = declarations.predicate("OscillationSequenceIn", [
  OscillationSequence,
  OscillatingSineGraph,
]);
const FailsLocalCompactnessAt = declarations.predicate(
  "FailsLocalCompactnessAt",
  [sets.Point, Subspace, Topology],
);
const OneToOne = declarations.predicate("OneToOne", [TopologicalMap]);
const Onto = declarations.predicate("Onto", [TopologicalMap, sets.Set]);
const OpenMap = declarations.predicate("OpenMap", [
  TopologicalMap,
  Topology,
  Topology,
]);
const NotOpenMap = declarations.predicate("NotOpenMap", [
  TopologicalMap,
  Topology,
  Topology,
]);
const LowerLimitTopology = declarations.type("LowerLimitTopology", Topology);
/** [a,b) is open and closed in the lower-limit topology, not in the usual topology. */
const LowerLimitInterval = declarations
  .type("LowerLimitInterval", RealInterval, Neighborhood, ClosedSet)
  .withData<{ leftClosed: true; rightClosed: false }>();
const LowerLimitPlaneNeighborhood = declarations.type(
  "LowerLimitPlaneNeighborhood",
  OpenProductSet,
  ClosedSet,
  Neighborhood,
);
/** An exact dyadic real number, including the endpoint indices zero and one. */
const DyadicRational = declarations
  .type("DyadicRational", RealPoint)
  .withData<{ numerator: number; exponent: number }>();
const DyadicOpenFamily = declarations.type("DyadicOpenFamily", SetFamily);
/** A⊆U(q), U(q) disjoint from B, and Cl U(q)⊆U(q') when q<q'. */
const DyadicFamilyFor = declarations.predicate("DyadicFamilyFor", [
  DyadicOpenFamily,
  ClosedSet,
  ClosedSet,
  Topology,
]);
const DyadicIndexOf = declarations.predicate("DyadicIndexOf", [
  OpenSet,
  DyadicRational,
  DyadicOpenFamily,
]);
const DyadicSeparationStep = declarations.predicate("DyadicSeparationStep", [
  DyadicOpenFamily,
  OpenSet,
  ClosedSet,
  OpenSet,
]);
const DyadicRefinementStep = declarations.predicate("DyadicRefinementStep", [
  DyadicOpenFamily,
  OpenSet,
  ClosedSet,
  OpenSet,
  ClosedSet,
  OpenSet,
]);
const TopologicalPath = declarations.type("TopologicalPath", TopologicalMap);
/** The two horizontal edges, rather than the entire boundary of the square. */
const HorizontalBoundaryPair = declarations.type(
  "HorizontalBoundaryPair",
  ClosedSet,
  Subspace,
  OpenSubspace,
);
const HorizontalEdgesOf = declarations.predicate("HorizontalEdgesOf", [
  HorizontalBoundaryPair,
  ClosedRectangle,
]);
/** extension restricted to the named closed domain is the original map. */
const ExtensionOf = declarations.predicate("ExtensionOf", [
  TopologicalMap,
  TopologicalMap,
  ClosedSet,
  sets.Set,
]);
const BoundaryPathsOf = declarations.predicate("BoundaryPathsOf", [
  TopologicalMap,
  TopologicalPath,
  TopologicalPath,
  HorizontalBoundaryPair,
]);
const NotT2 = declarations.predicate("NotT2", [Topology]);
/** Every pair of neighborhoods of these distinct points intersects. */
const UnseparablePoints = declarations.predicate("UnseparablePoints", [
  sets.Point,
  sets.Point,
  Topology,
]);
const NeighborhoodSystemAt = declarations.predicate("NeighborhoodSystemAt", [
  NeighborhoodSystem,
  sets.Point,
  Topology,
]);
const NeighborhoodsDirectedBy = declarations.predicate(
  "NeighborhoodsDirectedBy",
  [NeighborhoodSystem, NeighborhoodDirectedSet],
);
const NeighborhoodInSystem = declarations.predicate("NeighborhoodInSystem", [
  Neighborhood,
  NeighborhoodSystem,
]);
const DirectedProductOf = declarations.predicate("DirectedProductOf", [
  ProductDirectedSet,
  DirectedSet,
  DirectedSet,
]);
const PartitionOf = declarations.predicate("PartitionOf", [
  IntervalPartition,
  ClosedInterval,
]);
const PartitionsOfInterval = declarations.predicate("PartitionsOfInterval", [
  PartitionDirectedSet,
  ClosedInterval,
]);
/** The book puts the finer partition first in P₁≤P₂. */
const PartitionRefines = declarations.predicate("PartitionRefines", [
  IntervalPartition,
  IntervalPartition,
]);
const PartitionInFamily = declarations.predicate("PartitionInFamily", [
  IntervalPartition,
  PartitionDirectedSet,
]);
const NetIndexedBy = declarations.predicate("NetIndexedBy", [Net, DirectedSet]);
const NetIn = declarations.predicate("NetIn", [Net, sets.Set]);
const NetConvergesTo = declarations.predicate("NetConvergesTo", [
  Net,
  sets.Point,
  Topology,
]);
const NetLimitPoint = declarations.predicate("NetLimitPoint", [
  sets.Point,
  Net,
  Topology,
]);
const EventuallyIn = declarations.predicate("EventuallyIn", [Net, sets.Set]);
const OftenIn = declarations.predicate("OftenIn", [Net, sets.Set]);
const SubnetOf = declarations.predicate("SubnetOf", [Net, Net]);
const CofinalOrderMap = declarations.predicate("CofinalOrderMap", [
  TopologicalMap,
  DirectedSet,
  DirectedSet,
]);
const SelectionNetOf = declarations.predicate("SelectionNetOf", [
  Net,
  TopologicalMap,
]);
/** A selector on all pairs of neighborhoods, not only a finite drawn sample. */
const ChoosesFromIntersections = declarations.predicate(
  "ChoosesFromIntersections",
  [TopologicalMap, NeighborhoodSystem, NeighborhoodSystem],
);
const SelectedIntersectionValue = declarations.predicate(
  "SelectedIntersectionValue",
  [TopologicalMap, Neighborhood, Neighborhood, sets.Point],
);
const FilterOn = declarations.predicate("FilterOn", [Filter, sets.Set]);
const FilterBasisFor = declarations.predicate("FilterBasisFor", [
  FilterBase,
  Filter,
]);
const FilterFinerThan = declarations.predicate("FilterFinerThan", [
  Filter,
  Filter,
]);
const FilterConvergesTo = declarations.predicate("FilterConvergesTo", [
  Filter,
  sets.Point,
  Topology,
]);
const FilterLimitPoint = declarations.predicate("FilterLimitPoint", [
  sets.Point,
  Filter,
  Topology,
]);
const AssociatedFilterOf = declarations.predicate("AssociatedFilterOf", [
  Filter,
  Net,
]);
const AffineSubspace = declarations.type(
  "AffineSubspace",
  AffineLine,
  Subspace,
);
/** Openness is relative to the topology supplied by OpenIn. */
const OpenAffineSubspace = declarations.type(
  "OpenAffineSubspace",
  AffineSubspace,
  OpenSet,
);
const EmptySet = declarations.type("EmptySet", OpenSet, ClosedSet);
const CoordinateProjection = declarations
  .type("CoordinateProjection", TopologicalMap)
  .withData<{ coordinate: 1 | 2 }>();
const EmbeddingMap = declarations.type("EmbeddingMap", TopologicalMap);
const CoordinateEmbedding = declarations
  .type("CoordinateEmbedding", EmbeddingMap)
  .withData<{ varyingCoordinate: 1 | 2; fixedCoordinate: number }>();
const ProductOf = declarations.predicate("ProductOf", [
  ProductSet,
  sets.Set,
  sets.Set,
]);
const ProductTopologyOf = declarations.predicate("ProductTopologyOf", [
  Topology,
  Topology,
  Topology,
]);
const ProjectionOf = declarations.predicate("ProjectionOf", [
  CoordinateProjection,
  ProductSet,
  sets.Set,
]);
const InverseImageOf = declarations.predicate("InverseImageOf", [
  sets.Set,
  TopologicalMap,
  sets.Set,
]);
const ImageOf = declarations.predicate("ImageOf", [
  sets.Set,
  TopologicalMap,
  sets.Set,
]);
const EmbeddingInto = declarations.predicate("EmbeddingInto", [
  EmbeddingMap,
  sets.Set,
  ProductSet,
  Subspace,
]);
const SubspaceTopologyOf = declarations.predicate("SubspaceTopologyOf", [
  Topology,
  Subspace,
  Topology,
]);
/** The first map has the same values, with codomain restricted to the named image. */
const CorestrictionOf = declarations.predicate("CorestrictionOf", [
  TopologicalMap,
  TopologicalMap,
  sets.Set,
]);

/** Proof assumptions remain nested propositions rather than asserted false facts. */
const Hypothesis = declarations.predicate("Hypothesis", [proposition]);
const Connected = declarations.predicate("Connected", [Topology]);
const ConnectedIn = declarations.predicate("ConnectedIn", [sets.Set, Topology]);
const PathConnected = declarations.predicate("PathConnected", [Topology]);
const NotPathConnected = declarations.predicate("NotPathConnected", [Topology]);
const PolygonallyConnected = declarations.predicate("PolygonallyConnected", [
  Topology,
]);
const TopologicalSeparationOf = declarations.predicate(
  "TopologicalSeparationOf",
  [sets.Set, sets.Set, sets.Set, Topology],
);
const SupremumOf = declarations.predicate("SupremumOf", [RealPoint, sets.Set]);
/** S={s in I: s<=u or [u,s] is a subset of U}. */
const InitialSegmentsIn = declarations.predicate("InitialSegmentsIn", [
  sets.Set,
  ClosedInterval,
  RealPoint,
  sets.Set,
]);
const PuncturedPlane = declarations.type("PuncturedPlane", Subspace);
const DeletedPointFrom = declarations.predicate("DeletedPointFrom", [
  PuncturedPlane,
  EuclideanPlane,
  CoordinatePoint,
]);
/** An ordered finite sequence of coordinates determines its union of closed segments. */
const PolygonalPath = declarations.type("PolygonalPath", Subspace).withData<{
  vertices: readonly PlaneCoordinates[];
}>();
const SegmentBetween = declarations.predicate("SegmentBetween", [
  LinearSegment,
  CoordinatePoint,
  CoordinatePoint,
]);
const SegmentOfPath = declarations.predicate("SegmentOfPath", [
  LinearSegment,
  PolygonalPath,
]);
const VertexOfPath = declarations.predicate("VertexOfPath", [
  CoordinatePoint,
  PolygonalPath,
]);
const PolygonalPathBetween = declarations.predicate("PolygonalPathBetween", [
  PolygonalPath,
  CoordinatePoint,
  CoordinatePoint,
  sets.Set,
]);

/** Mathematical topology vocabulary, composed in the same set/point type context. */
export const pointSetTopology = declarations.make({
  ...sets,
  OpenSet,
  ClosedSet,
  CoordinatePoint,
  OpenDisk,
  DiskNeighborhood,
  Singleton,
  ClopenSingleton,
  ClosedRegion,
  Neighborhood,
  NeighborhoodUnion,
  ReciprocalGraph,
  AffineLine,
  SingletonOf,
  BoundaryPoint,
  Outside,
  NeighborhoodOf,
  ZeroSetDistance,
  HausdorffNeighborhoodPair,
  ZeroPointDistance,
  PointClosedSeparation,
  ClosedSetsSeparation,
  RealInterval,
  OpenInterval,
  ClosedInterval,
  HalfLine,
  OpenHalfPlane,
  OpenSquare,
  OpenTriangle,
  ClosedDisk,
  CircleBoundary,
  DiskExterior,
  EndpointPair,
  Topology,
  SetFamily,
  FiniteSetFamily,
  NeighborhoodSystem,
  OpenCover,
  FiniteOpenCover,
  MetricBallFamily,
  MetricClosureFamily,
  SeparatedSubset,
  MaximalSeparatedSubset,
  FiniteCoverReachSet,
  DirectedSet,
  NeighborhoodDirectedSet,
  ProductDirectedSet,
  PartitionDirectedSet,
  IntervalPartition,
  Net,
  Ultranet,
  Filter,
  FilterBase,
  Ultrafilter,
  Basis,
  Subbasis,
  TopologyOn,
  BasisFor,
  SubbasisFor,
  SetInFamily,
  FiniteIntersectionOf,
  UnionOf,
  EqualTopologies,
  OpenCoverOf,
  SubfamilyOf,
  FamilyIncludedIn,
  MetricBallsAt,
  ClosuresOfFamily,
  SeparatedIn,
  IndispensableCenteredCover,
  FiniteCoverReachOf,
  InteriorOf,
  ClosureOf,
  FrontierOf,
  ExteriorOf,
  DerivedSetOf,
  OpenIn,
  ClosedIn,
  T0,
  T1,
  T2,
  T3,
  Regular,
  NotT3,
  T4,
  Normal,
  NotNormal,
  Discrete,
  NotCompact,
  LocallyCompact,
  LocallyCompactIn,
  NotLocallyCompact,
  AssumedCompactIn,
  BoundedBy,
  CompactNeighborhoodWithin,
  HasNoFiniteSubcover,
  Compact,
  CompactIn,
  Lindelof,
  Countable,
  Nonempty,
  IntersectionOf,
  SetSeparation,
  PointClosedSetSeparation,
  ComplementOf,
  NeighborhoodClosureWithin,
  IntersectionWitness,
  TopologicalMap,
  MapBetween,
  MapsTo,
  ContinuousMap,
  Homeomorphism,
  InverseMaps,
  LinearSegment,
  TriangleBoundary,
  CentralProjection,
  ProjectionCenter,
  CentralProjectionBetween,
  RadialProjection,
  RadialProjectionBetween,
  EquivalenceRelation,
  EndpointIdentification,
  SubsetCollapseRelation,
  AntipodalBoundaryIdentification,
  ClosedRectangle,
  ClosedPolygon,
  PolygonBoundary,
  EuclideanPlane,
  IntegerDifferenceLocus,
  BoundaryOf,
  QuotientSpace,
  EquivalenceClass,
  QuotientMap,
  EquivalenceOn,
  EquivalentUnder,
  QuotientOf,
  ClassOf,
  ClassInQuotient,
  IdentificationMap,
  IdentificationTopology,
  FactorsThrough,
  CircleParameterization,
  CircleParameterizes,
  RealLine,
  RealPoint,
  DeletedReciprocalTopology,
  IrrationalPoint,
  IrrationalInInterval,
  AssumedSupremumOf,
  InitialIntervalAt,
  ReciprocalSequenceSet,
  DeletedReciprocalNeighborhood,
  OpenIntervalNeighborhood,
  ReciprocalSetFor,
  DeletedReciprocalNeighborhoodOf,
  UnseparablePointClosedSet,
  ProductSet,
  OpenProductSet,
  ClosedProductRectangle,
  LowerLimitTopology,
  LowerLimitInterval,
  LowerLimitPlaneNeighborhood,
  DyadicRational,
  DyadicOpenFamily,
  DyadicFamilyFor,
  DyadicIndexOf,
  DyadicSeparationStep,
  DyadicRefinementStep,
  TopologicalPath,
  HorizontalBoundaryPair,
  HorizontalEdgesOf,
  ExtensionOf,
  BoundaryPathsOf,
  NotT2,
  UnseparablePoints,
  NeighborhoodSystemAt,
  NeighborhoodsDirectedBy,
  NeighborhoodInSystem,
  DirectedProductOf,
  PartitionOf,
  PartitionsOfInterval,
  PartitionRefines,
  PartitionInFamily,
  NetIndexedBy,
  NetIn,
  NetConvergesTo,
  NetLimitPoint,
  EventuallyIn,
  OftenIn,
  SubnetOf,
  CofinalOrderMap,
  SelectionNetOf,
  ChoosesFromIntersections,
  SelectedIntersectionValue,
  FilterOn,
  FilterBasisFor,
  FilterFinerThan,
  FilterConvergesTo,
  FilterLimitPoint,
  AssociatedFilterOf,
  Subspace,
  AffineSubspace,
  RationalSubspace,
  RationalRayCoverAt,
  LocallyClosedIn,
  ProductPoint,
  ProductPairOf,
  OscillatingSineGraph,
  OriginAdjoinedSineCurve,
  OscillatingSineMap,
  OscillationSequence,
  SineCurveImageOf,
  SineGraphMapOn,
  OscillationSequenceIn,
  FailsLocalCompactnessAt,
  OneToOne,
  Onto,
  OpenMap,
  NotOpenMap,
  OpenAffineSubspace,
  EmptySet,
  CoordinateProjection,
  EmbeddingMap,
  CoordinateEmbedding,
  ProductOf,
  ProductTopologyOf,
  ProjectionOf,
  InverseImageOf,
  ImageOf,
  EmbeddingInto,
  SubspaceTopologyOf,
  CorestrictionOf,
  Hypothesis,
  Connected,
  ConnectedIn,
  PathConnected,
  NotPathConnected,
  PolygonallyConnected,
  TopologicalSeparationOf,
  SupremumOf,
  InitialSegmentsIn,
  PuncturedPlane,
  DeletedPointFrom,
  PolygonalPath,
  SegmentBetween,
  SegmentOfPath,
  VertexOfPath,
  PolygonalPathBetween,
});

export type MathematicalDisk = EntityOf<typeof OpenDisk>;

/** Coordinate incidence on a closed segment, including either endpoint. */
export function pointOnClosedSegment(
  point: PlaneCoordinates,
  start: PlaneCoordinates,
  end: PlaneCoordinates,
  tolerance = 1e-10,
) {
  if (
    ![...point, ...start, ...end, tolerance].every(Number.isFinite) ||
    tolerance < 0
  )
    throw new Error(
      "Closed-segment incidence requires finite coordinates and tolerance",
    );
  const dx = end[0] - start[0],
    dy = end[1] - start[1];
  const denominator = dx * dx + dy * dy;
  if (!Number.isFinite(denominator))
    throw new Error("Segment coordinate differences must be representable");
  const t =
    denominator === 0
      ? 0
      : Math.max(
          0,
          Math.min(
            1,
            ((point[0] - start[0]) * dx + (point[1] - start[1]) * dy) /
              denominator,
          ),
        );
  return (
    Math.hypot(point[0] - start[0] - t * dx, point[1] - start[1] - t * dy) <=
    tolerance
  );
}

/** A finite polygonal route avoids a point exactly when every segment does. */
export function polygonalPathAvoids(
  vertices: readonly PlaneCoordinates[],
  point: PlaneCoordinates,
  tolerance = 1e-10,
) {
  if (vertices.length < 2)
    throw new Error("A polygonal path needs at least two vertices");
  return vertices
    .slice(1)
    .every((p, i) => !pointOnClosedSegment(point, vertices[i], p, tolerance));
}

/** The radii in the metric covering construction, independent of schematic display spacing. */
export function separatedCoverRadii(separation: number) {
  const half = separation / 2,
    quarter = separation / 4;
  if (!(quarter > 0) || !Number.isFinite(separation))
    throw new Error(
      "Separated covers require a positive finite separation with representable radii",
    );
  return { half, quarter };
}

/** One interval from the cover extends finite reach beyond an interior proposed supremum. */
export function extendFiniteCoverReach(u: number, interval: RealIntervalData) {
  if (
    !(u > 0 && u < 1) ||
    !inRealInterval(interval, u) ||
    interval.leftClosed ||
    interval.rightClosed
  )
    throw new Error(
      "Finite cover reach needs an interior u and an open interval containing it",
    );
  const next = (u + Math.min(interval.b, 1)) / 2;
  if (!(next > u && next < interval.b && next <= 1))
    throw new Error(
      "The interval must admit a representable finite-reach extension",
    );
  return next;
}

/** Mesh of a finite, strictly increasing partition; no omitted points are implied. */
export function partitionMesh(points: readonly number[]) {
  if (
    points.length < 2 ||
    points.some(
      (p, i) => !Number.isFinite(p) || (i > 0 && !(p > points[i - 1])),
    )
  )
    throw new Error(
      "A partition needs at least two strictly increasing finite points",
    );
  const gaps = points.slice(1).map((p, i) => p - points[i]);
  if (gaps.some((gap) => !Number.isFinite(gap)))
    throw new Error("Partition gaps must be representable and finite");
  return Math.max(...gaps);
}

/** Exact finite refinement with the same endpoints, matching the book's inclusion. */
export function partitionRefines(
  finer: readonly number[],
  coarser: readonly number[],
) {
  partitionMesh(finer);
  partitionMesh(coarser);
  return (
    finer[0] === coarser[0] &&
    finer[finer.length - 1] === coarser[coarser.length - 1] &&
    coarser.every((p) => finer.includes(p))
  );
}

/** Binary fractions in [0,1], exactly representable at these bounded indices. */
export function dyadicValue(numerator: number, exponent: number) {
  if (
    !Number.isSafeInteger(exponent) ||
    exponent < 0 ||
    exponent > 52 ||
    !Number.isSafeInteger(numerator) ||
    numerator < 0 ||
    numerator > 2 ** exponent
  )
    throw new Error("A dyadic index needs 0<=n<=2^k and integer 0<=k<=52");
  return numerator / 2 ** exponent;
}

/** Strictly ordered neighboring indices used when inserting an odd dyadic numerator. */
export function dyadicRefinementValues(numerator: number, exponent: number) {
  if (
    !Number.isSafeInteger(numerator) ||
    numerator % 2 !== 1 ||
    exponent < 0 ||
    exponent >= 52
  )
    throw new Error(
      "Dyadic refinement needs an odd numerator and exponent below 52",
    );
  return [
    dyadicValue(numerator - 1, exponent + 1),
    dyadicValue(numerator, exponent + 1),
    dyadicValue(numerator + 1, exponent + 1),
  ] as const;
}

/** An exact JavaScript realization of the named term, with a safe integer index. */
export function reciprocalSequenceTerm(coefficient: number, index: number) {
  if (
    !(coefficient > 0) ||
    !Number.isFinite(coefficient) ||
    !Number.isSafeInteger(index) ||
    index < 1
  )
    throw new Error(
      "Reciprocal terms need a finite positive coefficient and positive safe integer index",
    );
  const term = coefficient / index;
  if (!(term > 0))
    throw new Error(
      "The reciprocal term must remain representable and positive",
    );
  return term;
}

/** Membership of representable c/n terms; no epsilon band is excluded around F. */
export function isReciprocalSequenceTerm(coefficient: number, value: number) {
  reciprocalSequenceTerm(coefficient, 1);
  if (!Number.isFinite(value))
    throw new Error("Reciprocal membership requires a finite real coordinate");
  if (!(value > 0)) return false;
  const index = Math.round(coefficient / value);
  return (
    Number.isSafeInteger(index) && index >= 1 && value === coefficient / index
  );
}

/** Zero's basic neighborhood (-radius,radius) minus the entire reciprocal set. */
export function inDeletedReciprocalNeighborhood(
  radius: number,
  coefficient: number,
  value: number,
) {
  if (!(radius > 0) || !Number.isFinite(radius))
    throw new Error("A deleted neighborhood needs a finite positive radius");
  return (
    !isReciprocalSequenceTerm(coefficient, value) && Math.abs(value) < radius
  );
}

/**
 * Any open U containing F contains an interval around c/n. If c/n < radius,
 * a point strictly between c/(n+1) and c/n lies in that interval and V.
 * epsilon is arbitrary positive local openness data, not a drawing radius.
 */
export function reciprocalNeighborhoodOverlap(
  radius: number,
  coefficient: number,
  index: number,
  epsilon: number,
) {
  const term = reciprocalSequenceTerm(coefficient, index),
    next = reciprocalSequenceTerm(coefficient, index + 1);
  if (
    !(radius > term) ||
    !Number.isFinite(radius) ||
    !(epsilon > 0) ||
    !Number.isFinite(epsilon)
  )
    throw new Error(
      "Overlap needs c/n inside (-radius,radius) and a positive open interval radius",
    );
  const point = term - Math.min(epsilon / 2, (term - next) / 2);
  if (
    !(point > next && point < term) ||
    !inDeletedReciprocalNeighborhood(radius, coefficient, point) ||
    !(Math.abs(point - term) < epsilon)
  )
    throw new Error(
      "The overlap witness needs enough floating point precision",
    );
  return { term, point, interval: [term - epsilon, term + epsilon] as const };
}

/** A null factor denotes all of R; interval inclusion follows its mathematical data. */
export function inIntervalProduct(
  x: RealIntervalData | null,
  y: RealIntervalData | null,
  point: PlaneCoordinates,
) {
  if (!point.every(Number.isFinite))
    throw new Error("Product membership requires finite coordinates");
  for (const factor of [x, y])
    if (
      factor &&
      (!(factor.a < factor.b) || ![factor.a, factor.b].every(Number.isFinite))
    )
      throw new Error("Product intervals require finite increasing endpoints");
  return (
    (!x || inRealInterval(x, point[0])) && (!y || inRealInterval(y, point[1]))
  );
}

export function coordinateEmbed(
  map: { varyingCoordinate: 1 | 2; fixedCoordinate: number },
  coordinate: number,
): PlaneCoordinates {
  if (![coordinate, map.fixedCoordinate].every(Number.isFinite))
    throw new Error("Coordinate embeddings require finite coordinates");
  return map.varyingCoordinate === 1
    ? [coordinate, map.fixedCoordinate]
    : [map.fixedCoordinate, coordinate];
}

export function coordinateProject(coordinate: 1 | 2, point: PlaneCoordinates) {
  if (!point.every(Number.isFinite))
    throw new Error("Coordinate projections require finite coordinates");
  return point[coordinate - 1];
}

export function inClosedRectangle(
  bounds: readonly [number, number, number, number],
  p: PlaneCoordinates,
) {
  const [x0, y0, x1, y1] = bounds;
  return x0 <= p[0] && p[0] <= x1 && y0 <= p[1] && p[1] <= y1;
}

/** Exact arithmetic on supplied finite coordinates; callers may sample the locus. */
export function inIntegerDifferenceLocus(period: number, p: PlaneCoordinates) {
  if (!(period > 0) || ![period, ...p].every(Number.isFinite))
    throw new Error(
      "An integer-difference locus needs a finite positive period",
    );
  return Number.isInteger((p[0] - p[1]) / period);
}

export function antipodalDiskEquivalent(
  disk: { center: PlaneCoordinates; radius: number },
  a: PlaneCoordinates,
  b: PlaneCoordinates,
) {
  const same = a[0] === b[0] && a[1] === b[1];
  const boundary = (p: PlaneCoordinates) =>
    Math.hypot(p[0] - disk.center[0], p[1] - disk.center[1]) === disk.radius;
  const inside = (p: PlaneCoordinates) =>
    Math.hypot(p[0] - disk.center[0], p[1] - disk.center[1]) <= disk.radius;
  if (
    !(disk.radius > 0) ||
    ![...disk.center, disk.radius, ...a, ...b].every(Number.isFinite)
  )
    throw new Error(
      "Antipodal identification needs finite points and positive radius",
    );
  return (
    inside(a) &&
    inside(b) &&
    (same ||
      (boundary(a) &&
        boundary(b) &&
        a[0] + b[0] === 2 * disk.center[0] &&
        a[1] + b[1] === 2 * disk.center[1]))
  );
}

export const inOpenDisk = (
  disk: Pick<MathematicalDisk, "center" | "radius">,
  p: PlaneCoordinates,
) =>
  disk.radius > 0 &&
  Math.hypot(p[0] - disk.center[0], p[1] - disk.center[1]) < disk.radius;

/** An infimum; zero does not assert membership in the open disk. */
export const pointDiskDistance = (
  disk: Pick<MathematicalDisk, "center" | "radius">,
  p: PlaneCoordinates,
) =>
  Math.max(
    0,
    Math.hypot(p[0] - disk.center[0], p[1] - disk.center[1]) - disk.radius,
  );

/** Distance of (x,c/x) to the x-axis. It is positive for every finite x != 0. */
export function reciprocalDistanceToXAxis(coefficient: number, x: number) {
  if (
    !Number.isFinite(coefficient) ||
    coefficient === 0 ||
    !Number.isFinite(x) ||
    x === 0
  )
    throw new Error(
      "A reciprocal graph requires finite nonzero coefficient and argument",
    );
  return Math.abs(coefficient / x);
}

export function inRealInterval(interval: RealIntervalData, x: number) {
  return (
    (interval.leftClosed ? x >= interval.a : x > interval.a) &&
    (interval.rightClosed ? x <= interval.b : x < interval.b)
  );
}

export function inOpenHalfPlane(
  coefficients: readonly [number, number, number],
  p: PlaneCoordinates,
) {
  return coefficients[0] * p[0] + coefficients[1] * p[1] < coefficients[2];
}

export function inOpenTriangle(
  vertices: readonly [PlaneCoordinates, PlaneCoordinates, PlaneCoordinates],
  p: PlaneCoordinates,
) {
  const cross = (a: PlaneCoordinates, b: PlaneCoordinates) =>
    (b[0] - a[0]) * (p[1] - a[1]) - (b[1] - a[1]) * (p[0] - a[0]);
  const signs = vertices.map((v, i) => cross(v, vertices[(i + 1) % 3]));
  return signs.every((v) => v > 0) || signs.every((v) => v < 0);
}

/** For a nondegenerate real interval in the usual topology, regardless of its endpoints. */
export function intervalDerivedMembership(
  interval: RealIntervalData,
  x: number,
) {
  if (
    !(interval.a < interval.b) ||
    ![interval.a, interval.b, x].every(Number.isFinite)
  )
    throw new Error(
      "Derived interval sets require finite ordered endpoints and point",
    );
  return {
    interior: interval.a < x && x < interval.b,
    closure: interval.a <= x && x <= interval.b,
    frontier: x === interval.a || x === interval.b,
    exterior: x < interval.a || x > interval.b,
    derived: interval.a <= x && x <= interval.b,
  };
}

/** Derived sets of an open disk in the Euclidean plane, not an arbitrary topology. */
export function diskDerivedMembership(
  disk: Pick<MathematicalDisk, "center" | "radius">,
  p: PlaneCoordinates,
) {
  if (
    !(disk.radius > 0) ||
    ![...disk.center, disk.radius, ...p].every(Number.isFinite)
  )
    throw new Error(
      "Derived disk sets require finite coordinates and positive radius",
    );
  const distance = Math.hypot(p[0] - disk.center[0], p[1] - disk.center[1]);
  return {
    interior: distance < disk.radius,
    closure: distance <= disk.radius,
    frontier: distance === disk.radius,
    exterior: distance > disk.radius,
    derived: distance <= disk.radius,
  };
}

const cross2 = (a: PlaneCoordinates, b: PlaneCoordinates) =>
  a[0] * b[1] - a[1] * b[0];
const difference = (
  a: PlaneCoordinates,
  b: PlaneCoordinates,
): PlaneCoordinates => [a[0] - b[0], a[1] - b[1]];

/** Intersection of the forward ray P→x with a finite closed target segment. */
export function centralProjectToSegment(
  center: PlaneCoordinates,
  point: PlaneCoordinates,
  endpoints: readonly [PlaneCoordinates, PlaneCoordinates],
): PlaneCoordinates {
  if (
    ![...center, ...point, ...endpoints[0], ...endpoints[1]].every(
      Number.isFinite,
    )
  )
    throw new Error("Projection geometry must be finite");
  const ray = difference(point, center),
    edge = difference(endpoints[1], endpoints[0]),
    offset = difference(endpoints[0], center);
  const denominator = cross2(ray, edge);
  if (!Number.isFinite(denominator) || denominator === 0)
    throw new Error("The projection ray must meet the segment transversely");
  const t = cross2(offset, edge) / denominator,
    u = cross2(offset, ray) / denominator;
  if (
    !Number.isFinite(t) ||
    !Number.isFinite(u) ||
    !(t > 0) ||
    u < -1e-12 ||
    u > 1 + 1e-12
  )
    throw new Error("The forward projection must meet the target segment");
  const fraction = Math.max(0, Math.min(1, u));
  return [
    endpoints[0][0] + fraction * edge[0],
    endpoints[0][1] + fraction * edge[1],
  ];
}

/** Radial homeomorphism from a triangle boundary to a circle about an interior center. */
export function radialProjectToCircle(
  center: PlaneCoordinates,
  radius: number,
  point: PlaneCoordinates,
): PlaneCoordinates {
  if (![...center, ...point, radius].every(Number.isFinite) || !(radius > 0))
    throw new Error(
      "Radial projection needs finite geometry and a positive radius",
    );
  const ray = difference(point, center),
    length = Math.hypot(...ray);
  if (!(length > 0) || !Number.isFinite(length))
    throw new Error("The projected point must differ from the center");
  const image: PlaneCoordinates = [
    center[0] + radius * (ray[0] / length),
    center[1] + radius * (ray[1] / length),
  ];
  if (!image.every(Number.isFinite))
    throw new Error("Projected circle coordinates must remain finite");
  return image;
}

/** Its inverse: the unique forward ray intersection with a convex triangle boundary. */
export function radialProjectToTriangle(
  center: PlaneCoordinates,
  vertices: readonly [PlaneCoordinates, PlaneCoordinates, PlaneCoordinates],
  point: PlaneCoordinates,
): PlaneCoordinates {
  if (
    ![...center, ...point, ...vertices.flat()].every(Number.isFinite) ||
    !inOpenTriangle(vertices, center)
  )
    throw new Error(
      "The radial center must lie strictly inside a finite triangle",
    );
  const ray = difference(point, center);
  if (Math.hypot(...ray) === 0)
    throw new Error("The projected point must differ from the center");
  let nearest = Infinity;
  for (let i = 0; i < 3; i++) {
    const start = vertices[i],
      edge = difference(vertices[(i + 1) % 3], start),
      offset = difference(start, center);
    const denominator = cross2(ray, edge);
    if (denominator === 0) continue;
    const t = cross2(offset, edge) / denominator,
      u = cross2(offset, ray) / denominator;
    if (Number.isFinite(t) && t > 0 && u >= -1e-12 && u <= 1 + 1e-12)
      nearest = Math.min(nearest, t);
  }
  if (!Number.isFinite(nearest))
    throw new Error("The ray must meet the triangle boundary");
  return [center[0] + nearest * ray[0], center[1] + nearest * ray[1]];
}

/** Endpoint identification leaves every interior point in its own singleton class. */
export function endpointIdentified(
  bounds: readonly [number, number],
  x: number,
  y: number,
): boolean {
  const [a, b] = bounds;
  if (
    ![a, b, x, y].every(Number.isFinite) ||
    !(a < b) ||
    !Number.isFinite(b - a) ||
    x < a ||
    x > b ||
    y < a ||
    y > b
  )
    throw new Error(
      "Endpoint identification is defined on a finite closed interval",
    );
  return x === y || ((x === a || x === b) && (y === a || y === b));
}

/** A circle realization whose endpoint values agree exactly, including numerically. */
export function endpointCirclePoint(
  bounds: readonly [number, number],
  t: number,
  center: PlaneCoordinates = [0, 0],
  radius = 1,
): PlaneCoordinates {
  endpointIdentified(bounds, t, t);
  if (![...center, radius].every(Number.isFinite) || !(radius > 0))
    throw new Error(
      "Circle realization requires finite geometry and positive radius",
    );
  if (t === bounds[0] || t === bounds[1])
    return [center[0], center[1] + radius];
  const angle =
    Math.PI / 2 + (2 * Math.PI * (t - bounds[0])) / (bounds[1] - bounds[0]);
  return [
    center[0] + radius * Math.cos(angle),
    center[1] + radius * Math.sin(angle),
  ];
}

/** A rational in the gap left by finitely many rays based on either side of an irrational cut.
 * The exact irrational expression remains in IrrationalPoint; cut is its numerical approximation.
 * This finite witness checks the construction, without enumerating the infinite rational cover.
 */
export function rationalRayGapWitness(
  cut: number,
  interval: readonly [number, number],
  bases: readonly number[],
): {
  numerator: number;
  denominator: number;
  coordinate: number;
  gap: readonly [number, number];
} {
  const [a, b] = interval;
  if (
    ![cut, a, b, ...bases].every(Number.isFinite) ||
    !(a < cut && cut < b) ||
    bases.some((q) => q === cut)
  )
    throw new Error(
      "The irrational cut must lie strictly inside a finite interval and differ from every rational base",
    );
  const left = Math.max(a, ...bases.filter((q) => q < cut));
  const right = Math.min(b, ...bases.filter((q) => q > cut));
  for (let k = 0; k <= 52; k++) {
    const denominator = 2 ** k;
    const numerator = Math.floor(left * denominator) + 1;
    const coordinate = numerator / denominator;
    if (
      Number.isSafeInteger(numerator) &&
      left < coordinate &&
      coordinate < right
    )
      return { numerator, denominator, coordinate, gap: [left, right] };
  }
  throw new Error(
    "The remaining gap is too small for a finite dyadic witness at machine precision",
  );
}

/** The positive branch of (x, A sin(k/x)); the omitted x=0 is not a graph point. */
export function sineCurveValue(
  x: number,
  frequency = 1,
  amplitude = 1,
): PlaneCoordinates {
  if (
    ![x, frequency, amplitude].every(Number.isFinite) ||
    !(x > 0 && frequency > 0 && amplitude > 0) ||
    !Number.isFinite(frequency / x)
  )
    throw new Error(
      "The sine graph requires finite positive x, frequency and amplitude",
    );
  return [x, amplitude * Math.sin(frequency / x)];
}

/** The continuous bijection on {-1} union (0,infinity), not an extension across a punctured interval. */
export function originAdjoinedSineMapValue(
  x: number,
  frequency = 1,
  amplitude = 1,
): PlaneCoordinates {
  // Validate graph parameters even for the isolated point.
  sineCurveValue(1, frequency, amplitude);
  if (x === -1) return [0, 0];
  return sineCurveValue(x, frequency, amplitude);
}

/** Nonzero fixed-height graph points tend to (0,height), which lies outside the origin-adjoined graph. */
export function sineAccumulationTerm(
  height: number,
  n: number,
  frequency = 1,
  amplitude = 1,
): PlaneCoordinates {
  sineCurveValue(1, frequency, amplitude);
  if (
    !Number.isFinite(height) ||
    height === 0 ||
    Math.abs(height) > amplitude ||
    !Number.isSafeInteger(n) ||
    n < 1
  )
    throw new Error(
      "An accumulation witness requires a nonzero height within the amplitude and a positive integer index",
    );
  const phase = 2 * Math.PI * n + Math.asin(height / amplitude);
  const x = frequency / phase;
  if (!(x > 0) || !Number.isFinite(x))
    throw new Error("The accumulation point must have finite positive x");
  return [x, height];
}
