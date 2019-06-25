type Set
type Reals
type Real
type Point
type Function
type Interval
type OpenInterval
type ClosedInterval
type LeftClopenInterval
type RightClopenInterval

constructor CreateInterval: Real left * Real right -> Interval
constructor CreateOpenInterval: Real left * Real right -> OpenInterval
constructor CreateClosedInterval: Real left * Real right -> ClosedInterval
constructor CreateLeftClopenInterval: Real left * Real right -> LeftClopenInterval
constructor CreateRightClopenInterval: Real left * Real right -> RightClopenInterval
constructor CreateFunction: Set domain * Set codomain -> Function

constructor Pt: Real x * Real y -> Point

-- Subtyping relationships

Reals <: Set
Interval <: Set
Reals <: Interval
OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval
-- Reals <: OpenInterval
-- TODO: check that the Style records for the Substance subtypes satisfy the subtyping relationship as well
-- AND that the GPIs satisfy the subtyping relationship (e.g. lines, arrows having start/end fields)

-- Operators on intervals
function Union: Interval I * Interval J -> Interval
function Intersection: Interval I * Interval J -> Interval

-- Operators on functions, points, and intervals
function DerivativeAtP: Function f * Real p -> Real
function DerivativeOverD: Function f -> Function
function Integral: Interval I * Function f -> Real
function Apply: Function f * Real p -> Real
-- We don't know if applying f to an interval yields an interval. Substance should be able to cast it to an Interval
function ApplyOver: Function f * Interval I -> Interval
-- operator applyOver (f : Function, I : Interval) -> Set
-- (g . f): Does not check f's codomain matches g's domain
function Compose: Function f * Function g -> Function

-- Predicates on reals
predicate PosInfinite: Real x
predicate NegInfinite: Real x

-- Predicates on intervals
predicate Bounded: Interval i
predicate LeftBounded: Interval i
predicate RightBounded: Interval i
predicate Unbounded: Interval i
predicate Compact: Interval i
predicate Disconnected: Interval i
predicate Degenerate: Interval i
predicate Empty: Interval i

-- Predicates on functions
predicate Continuous: Function f
predicate Discontinuous: Function f
predicate Differentiable: Function f
predicate Integrable: Function f
predicate Invertible: Function f
predicate Monotonic: Function f
predicate Decreasing: Function f
predicate Increasing: Function f

-- Predicates for containment
predicate In: Real x * Set X
predicate In2: Point p * Set X * Set Y
predicate Subset: Set X * Set Y

-- Predicates on points
predicate LessThan: Real p1 * Real p2
predicate ClosedEnd: Real p * Interval I
predicate OpenEnd: Real p * Interval I

-- Syntactic Sugars


notation "A := [a,b] ⊆ X" ~ "ClosedInterval A; A := CreateClosedInterval(a, b);Subset(A, X)"
notation "A := (a,b) ⊆ X" ~ "OpenInterval A; A := CreateOpenInterval(a, b);Subset(A, X)"
notation "A := [a,b) ⊆ X" ~ "LeftClopenInterval A; A := CreateLeftClopenInterval(a, b);Subset(A, X)"

notation "X ⊆ Y" ~ "Subset(X, Y)"
notation "Real X ∈ Y" ~ "Real X;In(X, Y)"
notation "X ∪ Y" ~ "Union(X, Y)"

notation "f : A -> B" ~ "Function f; f := CreateFunction(A,B)"
notation "f(x)" ~ "Apply(f, x)"
notation "f ` (x)" ~ "DerivativeAtP(f, x)"


-- Prelude exports
value R : Reals
