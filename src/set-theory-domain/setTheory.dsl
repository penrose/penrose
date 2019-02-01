
----------------------------- Type Constructors --------------------------------

tconstructor Set: type
tconstructor Point: type
tconstructor Map: type

-------------------------------- Operators -------------------------------------

operator Intersection (a : Set, b : Set) : Set
operator Union (a : Set, b : Set) : Set
operator Substraciton (a : Set, b : Set) : Set
operator CartesianProduct (a : Set, b : Set) : Set
operator Difference (a : Set, b : Set) : Set
operator Subset (a : Set, b : Set) : Set
operator AddPoint (p1 : Point, s1 : Set) : Set
operator Apply ['A : type,'B : type] (m1 : Map('A,'B), s1 : 'A) : 'B

-------------------------------- Predicates -------------------------------------

predicate From (f : Map, domain : Set, codomain : Set) : Prop
predicate Empty (s : Set) : Prop
predicate Nonempty (s : Set) : Prop
predicate Intersect (s1 : Set, s2 : Set) : Prop
predicate NotIntersecting (s1 : Set, s2 : Set) : Prop
predicate IsSubset (s1 : Set, s2 : Set) : Prop
predicate NotSubset (s1 : Set, s2 : Set) : Prop
predicate PointIn (s1 : Set, p1 : Point) : Prop
predicate PointNotIn (s1 : Set, p1 : Point) : Prop
predicate Injection ['A : type, 'B : type] (m : Map('A,'B)) : Prop
predicate Surjection ['A : type, 'B : type] (m : Map2('A,'B)) : Prop
predicate Bijection ['A : type, 'B : type] (m : Map('A,'B)) : Prop

------------------------- Syntactic Sugar Definition ---------------------------

StmtNotation "C := A ∩ B" -> "C := Intersection(A,B)"
StmtNotation "C := A ∪ B" -> "C := Union(A,B)"
StmtNotation "C := A \ B" -> "C := Substraciton(A,B)"
StmtNotation "C := A ⨯ B" -> "C := CartesianProduct(A,B)"
StmtNotation "p ∈ A" -> "PointIn(A,p)"
StmtNotation "p ∉ A" -> "PointNotIn(A,p)"
StmtNotation "A ⊆ B" -> "IsSubset(A,B)"
StmtNotation "A ⊈ B" -> "NotSubset(A,B)"
StmtNotation "A = ∅" -> "Empty(A,B)"
StmtNotation "A = ∅" -> "Empty(A,B)"
StmtNotation "A ≠ ∅" -> "Nonempty(A,B)"
