type Set
type Point

-- function Intersection : Set a * Set b -> Set
-- function Subset : Set a * Set b -> Set

predicate Intersect : Set s1 * Set s2
predicate NotIntersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate NotSubset : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate PointNotIn : Set s * Point p
