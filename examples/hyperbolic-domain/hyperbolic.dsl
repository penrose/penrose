type HyperbolicPlane
type Point
type IdealPoint
type Segment
type Horocycle
type Geodesic

IdealPoint <: Point

predicate In: Point * HyperbolicPlane
predicate IsCenter: IdealPoint * Horocycle
predicate PassesThrough: Geodesic * IdealPoint * IdealPoint

constructor MakeSegment: Point endpoint1 * Point endpoint2 -> Segment
constructor MakeGeodesic: IdealPoint endpoint1 * IdealPoint endpoint2 -> Geodesic

notation "{ a, b }" ~ "MakeSegment( a, b )"
notation "p ∈ H" ~ "In( p, H )"

