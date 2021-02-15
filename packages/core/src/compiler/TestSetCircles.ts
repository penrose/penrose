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
Set A, B, C, D, E, F, G

IsSubset(B, A)
IsSubset(C, A)
IsSubset(D, B)
IsSubset(E, B)
IsSubset(F, C)
IsSubset(G, C)

Not(Intersecting(E, D))
Not(Intersecting(F, G))
Not(Intersecting(B, C))

AutoLabel All
`;

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

-- TODO: Fix that the resample hack breaks on switching examples since it saves the cached functions...
-- TOOD: Also breaks if you resample without generating the function on first sample. Clearly this should be part of the state

---

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.icon, y.icon)
}

-- --------- NEW


forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.icon, y.icon)
    ensure outsideOf(y.text, x.icon)
    ensure outsideOf(x.text, y.icon)
}

-- TODO: The code below hasn't been ported to web-perf yet
forall Point p {
    p.offset = 20.0
    p.icon = Circle {
        strokeWidth : 0.0
        color : rgba(0.0, 0.0, 0.0, 1.0)
        r : 3.0
    }

    p.text = Text {
        string : p.label
        center : p.icon.center + (p.offset, p.offset)
    }

    -- TODO: Why isn't the offset working?
    -- TODO: Port atDist?
    p.textLayering = p.text above p.icon
}

Point p
with Set A
where PointIn(A, p) {
    ensure contains(A.icon, p.icon, 0.3 * A.icon.r)
    p.layering = p.icon above A.icon
}

Point p
with Set A
where Not(PointIn(A, p)) {
    ensure disjoint(A.icon, p.icon)
}
`;
