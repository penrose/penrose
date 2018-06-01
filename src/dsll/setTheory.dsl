tconstructor Set: type
tconstructor Map(a : Set, b : Set) : type
tconstructor Map2('a : type,'b : type) : type
tconstructor Point: type

operator Intersection (a : Set, b : Set) : Set
operator Union (a : Set, b : Set) : Set
operator Substraciton (a : Set, b : Set) : Set
operator CartesianProduct (a : Set, b : Set) : Set
operator Difference (a : Set, b : Set) : Set
operator Subset (a : Set, b : Set) : Set
operator AddPoint (p1 : Point, s1 : Set) : Set
operator Apply ['A : type,'B : type] (m1 : Map('A,'B), s1 : 'A) : 'B

predicate Empty (s : Set) : Prop
predicate NonEmpty (s : Set) : Prop
predicate Intersect (s1 : Set, s2 : Set) : Prop
predicate NonIntersect (s1 : Set, s2 : Set) : Prop
predicate IsSubset (s1 : Set, s2 : Set) : Prop
predicate NoSubset (s1 : Set, s2 : Set) : Prop
predicate PointIn (s1 : Set, p1 : Point) : Prop
predicate PointNotIn (s1 : Set, p1 : Point) : Prop
predicate Injection ['A : type, 'B : type] (m : Map('A,'B)) : Prop
predicate Surjection ['A : type, 'B : type] (m : Map2('A,'B)) : Prop
predicate Bijection ['A : type, 'B : type] (m : Map('A,'B)) : Prop
