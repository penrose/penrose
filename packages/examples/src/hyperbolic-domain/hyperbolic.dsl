type HyperbolicPlane
type Point
type IdealPoint <: Point
type Segment
type Horocycle

predicate In(Point, HyperbolicPlane)
predicate IsCenter(IdealPoint, Horocycle)

constructor MakeSegment(Point endpoint1, Point endpoint2) -> Segment

notation "{ a, b }" ~ "MakeSegment( a, b )"
notation "p ∈ H" ~ "In( p, H )"

