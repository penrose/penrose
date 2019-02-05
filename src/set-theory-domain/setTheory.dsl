tconstructor Set: type
tconstructor Point: type
tconstructor Map: type

operator Intersection (a : Set, b : Set) : Set
operator Union (a : Set, b : Set) : Set
operator Substraciton (a : Set, b : Set) : Set
operator CartesianProduct (a : Set, b : Set) : Set
operator Difference (a : Set, b : Set) : Set
operator Subset (a : Set, b : Set) : Set
operator AddPoint (p1 : Point, s1 : Set) : Set

predicate From (f : Map, domain : Set, codomain : Set) : Prop
predicate Empty (s : Set) : Prop
predicate Nonempty (s : Set) : Prop
predicate Intersect (s1 : Set, s2 : Set) : Prop
predicate NotIntersecting (s1 : Set, s2 : Set) : Prop
predicate IsSubset (s1 : Set, s2 : Set) : Prop
predicate NotSubset (s1 : Set, s2 : Set) : Prop
predicate PointIn (s1 : Set, p1 : Point) : Prop
predicate PointNotIn (s1 : Set, p1 : Point) : Prop
predicate Injection (m : Map) : Prop
predicate Surjection (m : Map) : Prop
predicate Bijection (m : Map) : Prop
