type HyperbolicPlane
type Point
type IdealPoint
type Segment
type Geodesic
type Horocycle
type Geodesic

IdealPoint <: Point

predicate In: Point * HyperbolicPlane
predicate IsCenter: IdealPoint * Horocycle
predicate PassesThrough: Geodesic * IdealPoint * IdealPoint

-- asserts that g is perpendicular to h and passes through p
predicate PerpendicularTo: Geodesic g * Geodesic h * IdealPoint p

constructor MakeSegment: Point endpoint1 * Point endpoint2 -> Segment
constructor MakeGeodesic: IdealPoint endpoint1 * IdealPoint endpoint2 -> Geodesic

notation "{ a, b }" ~ "MakeSegment( a, b )"
notation "a <-> b" ~ "MakeGeodesic( a, b )"
notation "p ∈ H" ~ "In( p, H )"
