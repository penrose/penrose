import type { DomainProgramBuilder, TypeDeclaration } from "../core/program.js";
import { declareGroupTheory } from "./set-theory.js";

/** Finite Fourier coefficients of a genuine plane loop with integer frequencies. */
export interface TrigonometricLoopData {
  readonly constant: readonly [number, number];
  readonly cosine: readonly (readonly [number, number])[];
  readonly sine: readonly (readonly [number, number])[];
}

export interface TopologicalVocabularyBase {
  readonly Set: TypeDeclaration;
  readonly Point: TypeDeclaration;
  readonly RealPoint: TypeDeclaration;
  readonly Topology: TypeDeclaration;
  readonly SetFamily: TypeDeclaration;
  readonly OpenCover: TypeDeclaration;
  readonly CountableSetFamily: TypeDeclaration;
  readonly Basis: TypeDeclaration;
  readonly NeighborhoodSystem: TypeDeclaration;
  readonly Neighborhood: TypeDeclaration;
  readonly TopologicalMap: TypeDeclaration;
  readonly Metric: TypeDeclaration;
  readonly DirectedSet: TypeDeclaration;
  readonly Net: TypeDeclaration;
  readonly Filter: TypeDeclaration;
  readonly FilterBase: TypeDeclaration;
  readonly Homotopy: TypeDeclaration;
  readonly TopologicalPath: TypeDeclaration;
}

/**
 * Shape-free vocabulary for the book's unillustrated sections and exercises.
 * Compose it with declarations from the same builder before sealing the domain.
 * Predicates express author-supplied mathematical facts; they do not infer
 * metrizability, compactness, infinite convergence, or homotopy classes from a
 * rendered picture. In particular, equal induced maps require a homotopy that
 * fixes the base point, rather than an arbitrary unbased homotopy (§11.4).
 */
export function declareTopologicalVocabulary<
  const D extends string,
  const B extends TopologicalVocabularyBase,
>(declarations: DomainProgramBuilder<D>, base: B) {
  const groups = declareGroupTheory(declarations, {
    Set: base.Set as B["Set"],
    Point: base.Point as B["Point"],
    Function: base.TopologicalMap as B["TopologicalMap"],
  });
  return declareTopologicalVocabularyWithGroups(declarations, base, groups);
}

/** Reuse existing algebra declarations when composing full set theory and topology. */
export function declareTopologicalVocabularyWithGroups<
  const D extends string,
  const B extends TopologicalVocabularyBase,
  const G extends {
    Group: TypeDeclaration;
    GroupHomomorphism: TypeDeclaration;
    GroupIsomorphism: TypeDeclaration;
  },
>(declarations: DomainProgramBuilder<D>, base: B, groups: G) {
  // Indexed access annotations retain nominal parent types in declaration emit.
  const Set: B["Set"] = base.Set;
  const Point: B["Point"] = base.Point;
  const RealPoint: B["RealPoint"] = base.RealPoint;
  const Topology: B["Topology"] = base.Topology;
  const SetFamily: B["SetFamily"] = base.SetFamily;
  const OpenCover: B["OpenCover"] = base.OpenCover;
  const CountableSetFamily: B["CountableSetFamily"] = base.CountableSetFamily;
  const Basis: B["Basis"] = base.Basis;
  const NeighborhoodSystem: B["NeighborhoodSystem"] = base.NeighborhoodSystem;
  const Neighborhood: B["Neighborhood"] = base.Neighborhood;
  const TopologicalMap: B["TopologicalMap"] = base.TopologicalMap;
  const Metric: B["Metric"] = base.Metric;
  const DirectedSet: B["DirectedSet"] = base.DirectedSet;
  const Net: B["Net"] = base.Net;
  const Filter: B["Filter"] = base.Filter;
  const FilterBase: B["FilterBase"] = base.FilterBase;
  const Homotopy: B["Homotopy"] = base.Homotopy;
  const TopologicalPath: B["TopologicalPath"] = base.TopologicalPath;

  const Group: G["Group"] = groups.Group;
  const GroupHomomorphism: G["GroupHomomorphism"] = groups.GroupHomomorphism;
  const GroupIsomorphism: G["GroupIsomorphism"] = groups.GroupIsomorphism;
  const IndexSet = declarations.type("IndexSet", Set);
  const Index = declarations.type("Index", Point);
  const FiniteSubset = declarations.type("FiniteSubset", Set);
  const IndexedSetFamily = declarations.type("IndexedSetFamily", SetFamily);
  const IndexedTopologyFamily = declarations.type("IndexedTopologyFamily");
  const IndexedProduct = declarations.type("IndexedProduct", Set);
  const ProductElement = declarations.type("ProductElement", Point);
  const IndexedCoordinateProjection = declarations.type(
    "IndexedCoordinateProjection",
    TopologicalMap,
  );
  const FiniteSupportCylinder = declarations.type("FiniteSupportCylinder", Set);
  const CountableBasis = declarations.type(
    "CountableBasis",
    Basis,
    CountableSetFamily,
  );
  const CountableNeighborhoodSystem = declarations.type(
    "CountableNeighborhoodSystem",
    NeighborhoodSystem,
  );
  const DirectedIndex = declarations.type("DirectedIndex", Index);
  const DirectedOrder = declarations.type("DirectedOrder");
  const Sequence = declarations.type("Sequence", Net);
  const IterationSequence = declarations.type("IterationSequence", Sequence);
  const Compactification = declarations.type("Compactification", Topology);
  const OnePointCompactification = declarations.type(
    "OnePointCompactification",
    Compactification,
  );
  const LipschitzMap = declarations
    .type("LipschitzMap", TopologicalMap)
    .withData<{ lipschitzConstant: number }>();
  const ContractionMap = declarations.type("ContractionMap", LipschitzMap);
  const AffineRealContraction = declarations
    .type("AffineRealContraction", ContractionMap)
    .withData<{ slope: number; intercept: number }>();
  const RealIterationSample = declarations
    .type("RealIterationSample", RealPoint)
    .withData<{ index: number }>();
  const LebesgueNumber = declarations
    .type("LebesgueNumber")
    .withData<{ value: number }>();
  const MetricValue = declarations
    .type("MetricValue")
    .withData<{ value: number }>();
  const FunctionSpace = declarations.type("FunctionSpace", Set);
  const FunctionPoint = declarations.type(
    "FunctionPoint",
    TopologicalMap,
    Point,
  );
  const PointwiseNeighborhood = declarations
    .type("PointwiseNeighborhood", Neighborhood)
    .withData<{ radius: number }>();
  const ContinuousFunctionSet = declarations.type(
    "ContinuousFunctionSet",
    FunctionSpace,
  );
  const LoopSpace = declarations.type("LoopSpace", FunctionSpace);
  const Loop = declarations.type("Loop", TopologicalPath);
  const TrigonometricLoop = declarations
    .type("TrigonometricLoop", Loop)
    .withData<TrigonometricLoopData>();
  const ConstantLoop = declarations.type("ConstantLoop", Loop);
  const LoopConcatenation = declarations.type("LoopConcatenation", Loop);
  const InverseLoop = declarations.type("InverseLoop", Loop);
  // A homotopy class is both a set of loops and an element of a group.
  const LoopHomotopyClass = declarations.type("LoopHomotopyClass", Set, Point);
  const FundamentalGroup = declarations.type("FundamentalGroup", Group);
  const InducedHomomorphism = declarations.type(
    "InducedHomomorphism",
    GroupHomomorphism,
  );
  const BasepointChangeIsomorphism = declarations.type(
    "BasepointChangeIsomorphism",
    GroupIsomorphism,
  );
  const RetractionMap = declarations.type("RetractionMap", TopologicalMap);
  const HomotopyEquivalence = declarations.type("HomotopyEquivalence");
  const Isometry = declarations.type("Isometry", TopologicalMap);
  const MetricCompletion = declarations.type("MetricCompletion", Set);
  const Pseudometric = declarations.type("Pseudometric");
  const OrderRelation = declarations.type("OrderRelation");
  const InclusionMap = declarations.type("InclusionMap", TopologicalMap);
  const HilbertCube = declarations.type("HilbertCube", IndexedProduct);
  // The conclusion names these topics without defining their machinery.
  const HigherHomotopyGroup = declarations
    .type("HigherHomotopyGroup", Group)
    .withData<{ degree: number }>();
  const HomologyGroup = declarations
    .type("HomologyGroup", Group)
    .withData<{ degree: number }>();
  const CohomologyGroup = declarations
    .type("CohomologyGroup", Group)
    .withData<{ degree: number }>();

  return {
    ...groups,
    Isometry,
    MetricCompletion,
    Pseudometric,
    OrderRelation,
    InclusionMap,
    HilbertCube,
    HigherHomotopyGroup,
    HomologyGroup,
    CohomologyGroup,
    OrderTopologyFor: declarations.predicate("OrderTopologyFor", [
      Topology,
      Set,
      OrderRelation,
    ]),
    ClosedMap: declarations.predicate("ClosedMap", [
      TopologicalMap,
      Topology,
      Topology,
    ]),
    InclusionOf: declarations.predicate("InclusionOf", [
      InclusionMap,
      Set,
      Set,
    ]),
    DiagonalOf: declarations.predicate("DiagonalOf", [Set, Set]),
    TopologicalEqualSets: declarations.predicate("TopologicalEqualSets", [
      Set,
      Set,
    ]),
    CondensationPointOf: declarations.predicate("CondensationPointOf", [
      Point,
      Set,
      Topology,
    ]),
    CondensationSetOf: declarations.predicate("CondensationSetOf", [
      Set,
      Set,
      Topology,
    ]),
    AccumulationPointOf: declarations.predicate("AccumulationPointOf", [
      Point,
      Set,
      Topology,
    ]),
    Metacompact: declarations.predicate("Metacompact", [Topology]),
    Pseudocompact: declarations.predicate("Pseudocompact", [Topology]),
    Continuum: declarations.predicate("Continuum", [Topology]),
    IrreducibleAbout: declarations.predicate("IrreducibleAbout", [
      Topology,
      Set,
    ]),
    IrreduciblyConnectedBetween: declarations.predicate(
      "IrreduciblyConnectedBetween",
      [Topology, Point, Point],
    ),
    CutPoint: declarations.predicate("CutPoint", [Point, Topology]),
    NoncutPoint: declarations.predicate("NoncutPoint", [Point, Topology]),
    Disconnects: declarations.predicate("Disconnects", [Set, Topology]),
    QuasiComponentOf: declarations.predicate("QuasiComponentOf", [
      Set,
      Point,
      Topology,
    ]),
    SubcontinuumOf: declarations.predicate("SubcontinuumOf", [
      Topology,
      Topology,
      TopologicalMap,
    ]),
    Nonmetrizable: declarations.predicate("Nonmetrizable", [Topology]),
    CompletelyNormal: declarations.predicate("CompletelyNormal", [Topology]),
    AbsoluteRetract: declarations.predicate("AbsoluteRetract", [Topology]),
    EquivalentMetricsOn: declarations.predicate("EquivalentMetricsOn", [
      Metric,
      Metric,
      Set,
    ]),
    IsometryBetween: declarations.predicate("IsometryBetween", [
      Isometry,
      Set,
      Metric,
      Set,
      Metric,
    ]),
    MetricCompletionOf: declarations.predicate("MetricCompletionOf", [
      MetricCompletion,
      Metric,
      Set,
      Metric,
      Isometry,
    ]),
    TotallyBoundedIn: declarations.predicate("TotallyBoundedIn", [Set, Metric]),
    BoundedInMetric: declarations.predicate("BoundedInMetric", [Set, Metric]),
    BoundedFunction: declarations.predicate("BoundedFunction", [
      TopologicalMap,
    ]),
    UniformlyContinuousBetween: declarations.predicate(
      "UniformlyContinuousBetween",
      [TopologicalMap, Metric, Metric],
    ),
    PseudometricOn: declarations.predicate("PseudometricOn", [
      Pseudometric,
      Set,
    ]),
    PseudometricInducesTopology: declarations.predicate(
      "PseudometricInducesTopology",
      [Pseudometric, Topology],
    ),
    MetricAsPseudometric: declarations.predicate("MetricAsPseudometric", [
      Pseudometric,
      Metric,
    ]),
    HigherHomotopyGroupOf: declarations.predicate("HigherHomotopyGroupOf", [
      HigherHomotopyGroup,
      Topology,
      Point,
    ]),
    HomologyGroupOf: declarations.predicate("HomologyGroupOf", [
      HomologyGroup,
      Topology,
    ]),
    CohomologyGroupOf: declarations.predicate("CohomologyGroupOf", [
      CohomologyGroup,
      Topology,
    ]),
    IndexSet,
    Index,
    FiniteSubset,
    IndexedSetFamily,
    IndexedTopologyFamily,
    IndexedProduct,
    ProductElement,
    IndexedCoordinateProjection,
    FiniteSupportCylinder,
    CountableBasis,
    CountableNeighborhoodSystem,
    DirectedIndex,
    DirectedOrder,
    Sequence,
    IterationSequence,
    Compactification,
    OnePointCompactification,
    LipschitzMap,
    ContractionMap,
    AffineRealContraction,
    RealIterationSample,
    LebesgueNumber,
    MetricValue,
    FunctionPoint,
    PointwiseNeighborhood,
    FunctionSpace,
    ContinuousFunctionSet,
    LoopSpace,
    Loop,
    TrigonometricLoop,
    ConstantLoop,
    LoopConcatenation,
    InverseLoop,
    LoopHomotopyClass,
    FundamentalGroup,
    InducedHomomorphism,
    BasepointChangeIsomorphism,
    RetractionMap,
    HomotopyEquivalence,
    FinerTopologyThan: declarations.predicate("FinerTopologyThan", [
      Topology,
      Topology,
    ]),
    CoarserTopologyThan: declarations.predicate("CoarserTopologyThan", [
      Topology,
      Topology,
    ]),
    Indiscrete: declarations.predicate("Indiscrete", [Topology]),
    CofiniteTopology: declarations.predicate("CofiniteTopology", [Topology]),
    TopologicalCompositionOf: declarations.predicate(
      "TopologicalCompositionOf",
      [TopologicalMap, TopologicalMap, TopologicalMap],
    ),
    TopologicalRestrictionOf: declarations.predicate(
      "TopologicalRestrictionOf",
      [TopologicalMap, TopologicalMap, Set],
    ),
    IndexedFamilyOver: declarations.predicate("IndexedFamilyOver", [
      IndexedSetFamily,
      IndexSet,
    ]),
    IndexedSetAt: declarations.predicate("IndexedSetAt", [
      IndexedSetFamily,
      Index,
      Set,
    ]),
    IndexedTopologyAt: declarations.predicate("IndexedTopologyAt", [
      IndexedTopologyFamily,
      Index,
      Topology,
    ]),
    IndexedProductOf: declarations.predicate("IndexedProductOf", [
      IndexedProduct,
      IndexedSetFamily,
      IndexSet,
    ]),
    ProductCoordinateAt: declarations.predicate("ProductCoordinateAt", [
      ProductElement,
      Index,
      Point,
    ]),
    IndexedProjectionOf: declarations.predicate("IndexedProjectionOf", [
      IndexedCoordinateProjection,
      IndexedProduct,
      Index,
      Set,
    ]),
    IndexedProductTopologyOf: declarations.predicate(
      "IndexedProductTopologyOf",
      [Topology, IndexedTopologyFamily, IndexSet],
    ),
    CylinderSupportedOn: declarations.predicate("CylinderSupportedOn", [
      FiniteSupportCylinder,
      IndexedProduct,
      FiniteSubset,
    ]),
    CoverOf: declarations.predicate("CoverOf", [SetFamily, Set]),
    Refines: declarations.predicate("Refines", [SetFamily, SetFamily]),
    FiniteFamily: declarations.predicate("FiniteFamily", [SetFamily]),
    CountableFamily: declarations.predicate("CountableFamily", [SetFamily]),
    LocallyFiniteIn: declarations.predicate("LocallyFiniteIn", [
      SetFamily,
      Topology,
    ]),
    PointFiniteIn: declarations.predicate("PointFiniteIn", [
      SetFamily,
      Topology,
    ]),
    FiniteIntersectionProperty: declarations.predicate(
      "FiniteIntersectionProperty",
      [SetFamily],
    ),
    FirstCountable: declarations.predicate("FirstCountable", [Topology]),
    SecondCountable: declarations.predicate("SecondCountable", [Topology]),
    Separable: declarations.predicate("Separable", [Topology]),
    CountableDenseSubsetOf: declarations.predicate("CountableDenseSubsetOf", [
      Set,
      Topology,
    ]),
    CountableNeighborhoodSystemAt: declarations.predicate(
      "CountableNeighborhoodSystemAt",
      [CountableNeighborhoodSystem, Point, Topology],
    ),
    DirectedOrderOn: declarations.predicate("DirectedOrderOn", [
      DirectedOrder,
      DirectedSet,
    ]),
    IndexIn: declarations.predicate("IndexIn", [DirectedIndex, DirectedSet]),
    IndexPrecedes: declarations.predicate("IndexPrecedes", [
      DirectedOrder,
      DirectedIndex,
      DirectedIndex,
    ]),
    DirectedUpperBoundFor: declarations.predicate("DirectedUpperBoundFor", [
      DirectedIndex,
      DirectedIndex,
      DirectedIndex,
      DirectedOrder,
    ]),
    NetValueAt: declarations.predicate("NetValueAt", [
      Net,
      DirectedIndex,
      Point,
    ]),
    ImageNetOf: declarations.predicate("ImageNetOf", [
      Net,
      TopologicalMap,
      Net,
    ]),
    SubsequenceOf: declarations.predicate("SubsequenceOf", [
      Sequence,
      Sequence,
      TopologicalMap,
    ]),
    FilterContains: declarations.predicate("FilterContains", [Filter, Set]),
    FilterBaseContains: declarations.predicate("FilterBaseContains", [
      FilterBase,
      Set,
    ]),
    GeneratedFilterOf: declarations.predicate("GeneratedFilterOf", [
      Filter,
      FilterBase,
    ]),
    FilterImageBaseOf: declarations.predicate("FilterImageBaseOf", [
      FilterBase,
      TopologicalMap,
      Filter,
    ]),
    PrincipalFilterAt: declarations.predicate("PrincipalFilterAt", [
      Filter,
      Point,
      Set,
    ]),
    CofiniteFilterOn: declarations.predicate("CofiniteFilterOn", [Filter, Set]),
    FreeFilter: declarations.predicate("FreeFilter", [Filter]),
    NetBasedOn: declarations.predicate("NetBasedOn", [Net, Filter]),
    CompactificationOf: declarations.predicate("CompactificationOf", [
      Compactification,
      Topology,
      TopologicalMap,
    ]),
    IdealPointOf: declarations.predicate("IdealPointOf", [
      Point,
      OnePointCompactification,
    ]),
    SequentiallyCompact: declarations.predicate("SequentiallyCompact", [
      Topology,
    ]),
    CountablyCompact: declarations.predicate("CountablyCompact", [Topology]),
    LocallyConnected: declarations.predicate("LocallyConnected", [Topology]),
    LocallyPathConnected: declarations.predicate("LocallyPathConnected", [
      Topology,
    ]),
    TotallyDisconnected: declarations.predicate("TotallyDisconnected", [
      Topology,
    ]),
    PathComponentOf: declarations.predicate("PathComponentOf", [Set, Topology]),
    Metrizable: declarations.predicate("Metrizable", [Topology]),
    LocallyMetrizable: declarations.predicate("LocallyMetrizable", [Topology]),
    Paracompact: declarations.predicate("Paracompact", [Topology]),
    CompletelyRegular: declarations.predicate("CompletelyRegular", [Topology]),
    Tychonoff: declarations.predicate("Tychonoff", [Topology]),
    BaireSpace: declarations.predicate("BaireSpace", [Topology]),
    FirstCategoryIn: declarations.predicate("FirstCategoryIn", [Set, Topology]),
    SecondCategoryIn: declarations.predicate("SecondCategoryIn", [
      Set,
      Topology,
    ]),
    LipschitzBetween: declarations.predicate("LipschitzBetween", [
      LipschitzMap,
      Metric,
      Metric,
    ]),
    ContractionOn: declarations.predicate("ContractionOn", [
      ContractionMap,
      Set,
      Metric,
    ]),
    LebesgueNumberFor: declarations.predicate("LebesgueNumberFor", [
      LebesgueNumber,
      OpenCover,
      Set,
      Metric,
    ]),
    IteratesOf: declarations.predicate("IteratesOf", [
      IterationSequence,
      ContractionMap,
      Point,
    ]),
    IterationSampleOf: declarations.predicate("IterationSampleOf", [
      RealIterationSample,
      IterationSequence,
    ]),
    FixedPointOf: declarations.predicate("FixedPointOf", [
      Point,
      TopologicalMap,
    ]),
    FunctionSpaceOf: declarations.predicate("FunctionSpaceOf", [
      FunctionSpace,
      Set,
      Set,
    ]),
    PointwiseTopologyOn: declarations.predicate("PointwiseTopologyOn", [
      Topology,
      FunctionSpace,
    ]),
    FiniteControlNeighborhoodOf: declarations.predicate(
      "FiniteControlNeighborhoodOf",
      [PointwiseNeighborhood, FunctionPoint, FiniteSubset, Metric],
    ),
    DistanceBetweenPoints: declarations.predicate("DistanceBetweenPoints", [
      MetricValue,
      Metric,
      Point,
      Point,
    ]),
    DistanceToSet: declarations.predicate("DistanceToSet", [
      MetricValue,
      Metric,
      Point,
      Set,
    ]),
    DistanceBetweenSets: declarations.predicate("DistanceBetweenSets", [
      MetricValue,
      Metric,
      Set,
      Set,
    ]),
    FunctionInSpace: declarations.predicate("FunctionInSpace", [
      TopologicalMap,
      FunctionSpace,
    ]),
    ContinuousFunctionSetOf: declarations.predicate("ContinuousFunctionSetOf", [
      ContinuousFunctionSet,
      Topology,
      Topology,
    ]),
    SeparatesPoints: declarations.predicate("SeparatesPoints", [
      ContinuousFunctionSet,
      Topology,
    ]),
    LoopSpaceOf: declarations.predicate("LoopSpaceOf", [
      LoopSpace,
      Topology,
      Point,
    ]),
    LoopBasedAt: declarations.predicate("LoopBasedAt", [Loop, Point, Topology]),
    LoopInSpace: declarations.predicate("LoopInSpace", [Loop, LoopSpace]),
    ConstantLoopAt: declarations.predicate("ConstantLoopAt", [
      ConstantLoop,
      Point,
      Topology,
    ]),
    LoopClassOf: declarations.predicate("LoopClassOf", [
      LoopHomotopyClass,
      Loop,
      Topology,
      Point,
    ]),
    FundamentalGroupOf: declarations.predicate("FundamentalGroupOf", [
      FundamentalGroup,
      Topology,
      Point,
    ]),
    ClassInFundamentalGroup: declarations.predicate("ClassInFundamentalGroup", [
      LoopHomotopyClass,
      FundamentalGroup,
    ]),
    LoopProductOf: declarations.predicate("LoopProductOf", [
      LoopConcatenation,
      Loop,
      Loop,
    ]),
    LoopInverseOf: declarations.predicate("LoopInverseOf", [InverseLoop, Loop]),
    LoopClassProductOf: declarations.predicate("LoopClassProductOf", [
      LoopHomotopyClass,
      LoopHomotopyClass,
      LoopHomotopyClass,
      FundamentalGroup,
    ]),
    InducedHomomorphismOf: declarations.predicate("InducedHomomorphismOf", [
      InducedHomomorphism,
      TopologicalMap,
      FundamentalGroup,
      FundamentalGroup,
    ]),
    InducedLoopClassImage: declarations.predicate("InducedLoopClassImage", [
      InducedHomomorphism,
      LoopHomotopyClass,
      LoopHomotopyClass,
    ]),
    BasepointChangeAlong: declarations.predicate("BasepointChangeAlong", [
      BasepointChangeIsomorphism,
      TopologicalPath,
      FundamentalGroup,
      FundamentalGroup,
    ]),
    NullHomotopic: declarations.predicate("NullHomotopic", [
      Loop,
      Topology,
      Point,
    ]),
    SimplyConnected: declarations.predicate("SimplyConnected", [Topology]),
    RetractsOnto: declarations.predicate("RetractsOnto", [
      RetractionMap,
      Set,
      Set,
    ]),
    RetractOf: declarations.predicate("RetractOf", [Set, Topology]),
    DeformationRetractionOf: declarations.predicate("DeformationRetractionOf", [
      Homotopy,
      RetractionMap,
      Set,
    ]),
    HomotopyEquivalent: declarations.predicate("HomotopyEquivalent", [
      Topology,
      Topology,
    ]),
    HomotopyEquivalenceBetween: declarations.predicate(
      "HomotopyEquivalenceBetween",
      [HomotopyEquivalence, Topology, Topology],
    ),
    HomotopyInverseMaps: declarations.predicate("HomotopyInverseMaps", [
      TopologicalMap,
      TopologicalMap,
      Topology,
      Topology,
    ]),
  };
}

/** Validated data for a claimed strict Lipschitz contraction, 0 ≤ k < 1. */
export function contractionData(lipschitzConstant: number) {
  if (
    !Number.isFinite(lipschitzConstant) ||
    lipschitzConstant < 0 ||
    lipschitzConstant >= 1
  )
    throw new Error(
      "A contraction needs a finite Lipschitz constant 0 ≤ k < 1",
    );
  return Object.freeze({ lipschitzConstant });
}

/**
 * An affine contraction f(x)=ax+b on the complete real line (§10.3, Exercise 5).
 * The Lipschitz constant |a| and unique fixed point b/(1−a) follow algebraically;
 * this helper does not infer a bound for an arbitrary callback from samples.
 */
export function affineRealContraction(slope: number, intercept: number) {
  const data = contractionData(Math.abs(slope));
  if (!Number.isFinite(intercept))
    throw new Error("An affine intercept must be finite");
  const fixedPoint = intercept / (1 - slope);
  if (!Number.isFinite(fixedPoint))
    throw new Error("The fixed point exceeds numerical range");
  const apply = (value: number): number => {
    const result = slope * value + intercept;
    if (!Number.isFinite(value) || !Number.isFinite(result))
      throw new Error("A contraction value exceeds numerical range");
    return result;
  };
  const iterates = (initial: number, steps: number): readonly number[] => {
    if (
      !Number.isFinite(initial) ||
      !Number.isSafeInteger(steps) ||
      steps < 0 ||
      steps > 10000
    )
      throw new Error(
        "Finite iterates need a finite initial value and 0–10000 steps",
      );
    const result = [initial];
    for (let i = 0; i < steps; i++)
      result.push(apply(result[result.length - 1]));
    return Object.freeze(result);
  };
  // For real arithmetic, |f^n(x)-x*| = |a|^n |x-x*| for every n≥0.
  const errorBound = (initial: number, n: number): number => {
    if (!Number.isFinite(initial) || !Number.isSafeInteger(n) || n < 0)
      throw new Error(
        "An error bound needs a finite initial value and an integer n≥0",
      );
    const initialError = Math.abs(initial - fixedPoint);
    const result = data.lipschitzConstant ** n * initialError;
    if (!Number.isFinite(initialError) || !Number.isFinite(result))
      throw new Error("The error bound exceeds numerical range");
    if (result === 0 && initialError > 0 && data.lipschitzConstant > 0)
      throw new Error("The nonzero error bound underflows numerical precision");
    return result;
  };
  return Object.freeze({
    ...data,
    data: Object.freeze({ ...data, slope, intercept }),
    slope,
    intercept,
    fixedPoint,
    apply,
    iterates,
    errorBound,
  });
}
