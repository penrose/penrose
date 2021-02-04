export const domainStr = `
type Set
type Point
type Map

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
predicate Intersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map

notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"
notation "A ∩ B = ∅" ~ "Not(Intersecting(A, B))"
notation "f: A -> B" ~ "Map f; From(f, A, B)"
`;

export const subStrSugared = `
`;

export const subStrUnsugared = `
Set A, B, C
IsSubset(B, A)
IsSubset(C, A)
Not(Intersecting(B, C))
AutoLabel All
`;

// TODO < The third selector is not matching -- why? Nested predicates, predicate argument equality?
// Should add some tests for those fns

// basic circle
export const styStr = `
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }

    x.text = Text {
        string : x.label
    }

    ensure contains(x.icon, x.text)
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
    encourage sameCenter(x.text, x.icon)
    x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {

    ensure smallerThan(x.icon, y.icon)
    ensure outsideOf(y.text, x.icon)
    ensure contains(y.icon, x.icon, 5.0)
    x.icon above y.icon
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.icon, y.icon)
}
`;
