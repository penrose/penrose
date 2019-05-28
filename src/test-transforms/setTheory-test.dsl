type Set -- CircleTransform
type Point -- RectangleTransform

predicate Intersect : Set s1 * Set s2
predicate NotIntersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2

predicate Lineup : Point p1 * Point p2
predicate DisjointSP : Set s * Point p
predicate Chain : Point p1 * Point p2