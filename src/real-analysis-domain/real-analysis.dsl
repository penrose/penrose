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
operator union (I : Interval, J : Interval) : Interval
operator intersection (I : Interval, J : Interval)  : Interval

-- Operators on functions, points, and intervals
operator derivativeAtP (f : Function, p : Real) : Real
operator derivativeOverD (f : Function) : Function
operator integral (I : Interval, f : Function) : Real
operator apply (f : Function, p : Real) : Real
-- We don't know if applying f to an interval yields an interval. Substance should be able to cast it to an Interval
operator applyOver (f : Function, I : Interval) : Interval
-- operator applyOver (f : Function, I : Interval) : Set
-- (g . f): Does not check f's codomain matches g's domain
operator compose (f : Function, g : Function) : Function

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

-- Prelude exports
-- value RR : Reals

-- Notation is ASCII only
-- notation "f.domain" -> "Dom(f)"
-- notation "f.codomain" := "Cod(f)"
-- notation "Subset(X, Y)" := "X Subset Y"
-- TODO: specify infix predicates

-- Unicode display
-- display "Subset" -> ⊂
-- display "RR" -> R
