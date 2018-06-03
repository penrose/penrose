tconstructor Reals : type
tconstructor Interval : type
tconstructor Point : type
tconstructor Point2D : type
tconstructor Function : type
tconstructor List ('X : type) : type

vconstructor Cons ['X : type] (head : 'X, tail : List('X)) : List('X)
vconstructor Nil ['X : type] : List('X)
vconstructor IntervalCons (p1 : Point, p2 : Point) : Interval
vconstructor FunctionConsRealD (d : Reals, c : Reals) : Function
vconstructor FunctionConsIntervalD (d : Interval, c : Reals) : Function
vconstructor Point2DCons (p1 : Point, p2 : Point) : Point2D

operator Union (iList : List(Interval)) : List(Interval)
operator Intersection (iList : List(Interval))  : Interval

operator DerivativeAtP (p : Point, f : Function) : Point
operator DerivativeOverD (f : Function) : Function
operator Integral (I : Interval, f : Function) : Point
operator Apply (p : Point, f : Function) : Point

predicate Open (i : Interval) : Prop
predicate Closed (i : Interval) : Prop
predicate Clopen (i : Interval) : Prop
predicate LeftClopen (i : Interval) : Prop
predicate RightClopen (i : Interval) : Prop
predicate Bounded (i : Interval) : Prop
predicate LeftBounded (i : Interval) : Prop
predicate RightBounded (i : Interval) : Prop
predicate Unbounded (i : Interval) : Prop
predicate Compact (i : Interval) : Prop
predicate Degenerate (i : Interval) : Prop
predicate Empty (i : Interval) : Prop

predicate Continuous (f : Function) : Prop
predicate Discontinuous (f : Function) : Prop
predicate Differentiable (f : Function) : Prop
predicate Integrable (f : Function) : Prop
predicate Invertible (f : Function) : Prop
predicate Monotonic (f : Function) : Prop
predicate MonotonicDecreasing (f : Function) : Prop
predicate MonotonicIncreasing (f : Function) : Prop

predicate LessThan (p1 : Point, p2 : Point) : Prop
predicate GreaterThan (p1 : Point, p2 : Point) : Prop

predicate InIntervalP (p : Point, I : Interval) : Prop
predicate InIntervalI (J : Interval, I : Interval) : Prop
predicate InRealsP (p : Point, R : Reals) : Prop
predicate InRealsI (J : Interval, R : Reals) : Prop
predicate InIntervalP2D (p : Point2D, J : Interval, I : Interval) : Prop
predicate InRealsP2D (p : Point2D, D : Reals, C : Reals) : Prop
predicate InRealsIntervalP2D (p : Point2D, R : Reals, I : Interval) : Prop
predicate InIntervalRealsP2D (p : Point2D, I : Interval, R : Reals) : Prop
