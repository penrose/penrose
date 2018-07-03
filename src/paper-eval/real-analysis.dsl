tconstructor Set : type
tconstructor Reals : type
tconstructor Real : type
tconstructor Point : type
tconstructor Function : type
tconstructor Interval : type

vconstructor CreateInterval (left : Real, right : Real) : Interval
vconstructor OpenInterval (left : Real, right : Real) : OpenInterval
vconstructor ClosedInterval (left : Real, right : Real) : ClosedInterval
vconstructor LeftClopenInterval (left : Real, right : Real) : LeftClopenInterval
vconstructor RightClopenInterval (left : Real, right : Real) : RightClopenInterval
vconstructor Pt (x : Real, y : Real) : Point

-- Subtyping relationships
Reals <: Set
Interval <: Set

OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval

-- Operators on intervals
operator union (I : Interval, J : Interval) : Interval
operator intersection (I : Interval, J : Interval)  : Interval

-- Operators on functions, points, and intervals
operator derivativeAtP (f : Function, p : Real) : Real
operator derivativeOverD (f : Function) : Function
operator integral (I : Interval, f : Function) : Real
operator apply (f : Function, p : Real) : Real

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
predicate From (f : Function, domain : Set, codomain : Set) : Prop
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
--value RR : Reals

-- notation is ASCII only
-- kw NEW := OLD
--notation "f.domain" -> "Dom(f)"
-- TODO: flip rest
--notation "Cod(f)" := "f.codomain"
--notation "X Subset Y" := Subset(X, Y)
