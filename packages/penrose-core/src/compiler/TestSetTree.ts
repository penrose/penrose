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
 Set x {
    x.icon = Text { string : x.label }
}

Set x; Set y {
   -- Try to make sure no labels overlap
   encourage repel(x.icon, y.icon, 5.0)
}

-- TODO: Phase this back in

Set x; Set y
where IsSubset(x, y) {
    arrow = Arrow {
      thickness : 2.0
      color : rgba(0.0, 0.0, 0.0, 1.0)
    }

    -- Draw the arrow from y to x
    encourage centerArrow(arrow, x.icon, y.icon)

    -- Position y above x
    encourage above(y.icon, x.icon)

    -- Have sets 'fight' to be aligned with the superset's x-position
    encourage equal(x.icon.center[0], y.icon.center[0])
}

/*
-- TODO: This one currently causes convergence to become very slow but works eventually

Set x1; Set x2
where IsSubset(x1, S); IsSubset(x2, S)
with Set S {
   -- If two sets are direct subsets of the same set, give them the same y-position
   heightFn = ensure sameHeight(x1.icon, x2.icon)
}
*/
`;
