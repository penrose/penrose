tconstructor Set : type
tconstructor Reals : type
tconstructor Real : type
tconstructor Point : type
tconstructor Function : type
tconstructor Interval : type
tconstructor OpenInterval : type
tconstructor ClosedInterval : type
tconstructor LeftClopenInterval : type
tconstructor RightClopenInterval : type

vconstructor CreateInterval (left : Real, right : Real) : Interval
vconstructor CreateOpenInterval (left : Real, right : Real) : OpenInterval
vconstructor CreateClosedInterval (left : Real, right : Real) : ClosedInterval
vconstructor CreateLeftClopenInterval (left : Real, right : Real) : LeftClopenInterval
vconstructor CreateRightClopenInterval (left : Real, right : Real) : RightClopenInterval
vconstructor CreateFunction (domain : Set, codomain : Set) : Function

vconstructor Pt (x : Real, y : Real) : Point

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
operator Union (I : Interval, J : Interval) : Interval
operator Intersection (I : Interval, J : Interval)  : Interval

-- Operators on functions, points, and intervals
operator DerivativeAtP (f : Function, p : Real) : Real
operator DerivativeOverD (f : Function) : Function
operator Integral (I : Interval, f : Function) : Real
operator Apply (f : Function, p : Real) : Real
-- We don't know if applying f to an interval yields an interval. Substance should be able to cast it to an Interval
operator ApplyOver (f : Function, I : Interval) : Interval
-- operator applyOver (f : Function, I : Interval) : Set
-- (g . f): Does not check f's codomain matches g's domain
operator Compose (f : Function, g : Function) : Function

-- Predicates on reals
predicate PosInfinite(x : Real) : Prop
predicate NegInfinite(x : Real) : Prop

-- Predicates on intervals
predicate Bounded (i : Interval) : Prop
predicate LeftBounded (i : Interval) : Prop
predicate RightBounded (i : Interval) : Prop
predicate Unbounded (i : Interval) : Prop
predicate Compact (i : Interval) : Prop
predicate Disconnected (i : Interval) : Prop
predicate Degenerate (i : Interval) : Prop
predicate Empty (i : Interval) : Prop

-- Predicates on functions
predicate Continuous (f : Function) : Prop
predicate Discontinuous (f : Function) : Prop
predicate Differentiable (f : Function) : Prop
predicate Integrable (f : Function) : Prop
predicate Invertible (f : Function) : Prop
predicate Monotonic (f : Function) : Prop
predicate Decreasing (f : Function) : Prop
predicate Increasing (f : Function) : Prop

-- Predicates for containment
predicate In (x : Real, X : Set) : Prop
predicate In2 (p : Point, X : Set, Y : Set) : Prop
predicate Subset (X : Set, Y : Set) : Prop

-- Predicates on points
predicate LessThan (p1 : Real, p2 : Real) : Prop
predicate ClosedEnd (p : Real, I : Interval) : Prop
predicate OpenEnd (p : Real, I : Interval) : Prop

-- Syntactic Sugars


StmtNotation "A := [a,b] ⊆ X" -> "ClosedInterval A; A := CreateClosedInterval(a, b);Subset(A, X)"
StmtNotation "A := (a,b) ⊆ X" -> "OpenInterval A; A := CreateOpenInterval(a, b);Subset(A, X)"
StmtNotation "A := [a,b) ⊆ X" -> "LeftClopenInterval A; A := CreateLeftClopenInterval(a, b);Subset(A, X)"

StmtNotation "X ⊆ Y" -> "Subset(X, Y)"
StmtNotation "Real X ∈ Y" -> "Real X;In(X, Y)"
StmtNotation "X ∪ Y" -> "Union(X, Y)"

StmtNotation "f : A -> B" -> "Function f; f := CreateFunction(A,B)"
StmtNotation "f(x)" -> "Apply(f, x)"
StmtNotation "f ` (x)" -> "DerivativeAtP(f, x)"


-- Prelude exports
value R : Reals