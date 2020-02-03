type Set
type Point
type Map

-- constructor EmptySet () : Set
constructor Singleton : Point p -> Set

function Intersection : Set a * Set b -> Set
function Union : Set a * Set b -> Set
function Subtraction : Set a * Set b -> Set
function CartesianProduct : Set a * Set b -> Set
function Difference : Set a * Set b -> Set
function Subset : Set a * Set b -> Set
function AddPoint : Point p * Set s1 -> Set

predicate Not : Prop p1
predicate From : Map f * Set domain * Set codomain
predicate Empty : Set s
predicate Nonempty : Set s
predicate Intersect : Set s1 * Set s2
predicate NotIntersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate NotSubset : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate PointNotIn : Set s * Point p
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map