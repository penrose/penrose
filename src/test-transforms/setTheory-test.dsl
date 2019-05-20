tconstructor Set : type -- CircleTransform
tconstructor Point : type -- RectangleTransform

predicate Intersect (s1 : Set, s2 : Set) : Prop
predicate NotIntersecting (s1 : Set, s2 : Set) : Prop
predicate IsSubset (s1 : Set, s2 : Set) : Prop

predicate Lineup (p1 : Point, p2 : Point) : Prop
predicate DisjointSP (s : Set, p : Point) : Prop
predicate Chain (p1 : Point, p2 : Point) : Prop