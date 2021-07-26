type Point

type Linelike
type Ray
type Line
type Segment

type Angle
type Arc

type Triangle
type Rectangle
type Quadrilateral
type Circle

type Plane

-- Subtypes
Ray <: Linelike
Line <: Linelike
Segment <: Linelike
Rectangle <: Quadrilateral

constructor MkSegment : Point p * Point q -> Segment
constructor MkRay : Point base * Point direction -> Ray
constructor MkLine : Point p * Point q -> Line

constructor InteriorAngle : Point p * Point q * Point r -> Angle
constructor TriangleVertex : Point p * Point q * Point r -> Angle
constructor MkTriangle : Point p * Point q * Point r -> Triangle
constructor MkRectangle : Point p * Point q * Point r * Point s -> Rectangle
constructor MkQuadrilateral : Point p * Point q * Point r * Point s -> Quadrilateral
constructor MkMidpoint : Linelike l -> Point
constructor MkCircleR : Point center * Point radius -> Circle
-- constructor MkCircleD : Point diam1 * Point diam2 -> Circle  -- TODO can be reimplemented when #621 is resolved

-- -- TODO: subtyping on the return types
-- function Sum : Angle * Angle -> Angle
-- function Intersection : Linelike * Linelike -> Point
-- function Altitude : Triangle * Angle -> Segment
-- function Endpoint : Segment -> Point
function MidSegment : Triangle * Point * Point -> Segment
function Radius : Circle c * Point p -> Segment
function Chord : Circle c * Point p * Point q -> Segment
function Diameter : Circle c * Point p * Point q -> Segment
function PerpendicularBisector : Segment * Point -> Segment
function PerpendicularBisectorLabelPts : Segment * Point * Point -> Segment -- same as PerpendicularBisector but it takes a segment + 2 points as args so that the midpoint can be labeled

predicate On : Point * Linelike
predicate In : Point * Plane
predicate Not : Prop
predicate ParallelMarker1 : Linelike * Linelike
predicate AngleBisector : Angle * Linelike
-- predicate Perpendicular : Linelike * Linelike
predicate Midpoint : Linelike * Point
predicate Scalene : Triangle
predicate Collinear : Point * Point * Point
predicate EqualAngleMarker1 : Angle * Angle
predicate EqualAngleMarker2 : Angle * Angle
predicate EqualAngleMarker3 : Angle * Angle
predicate EqualLengthMarker1 : Linelike * Linelike
predicate EqualLengthMarker2 : Linelike * Linelike -- TODO implement, blocked
predicate EqualLengthMarker3 : Linelike * Linelike -- TODO implement, blocked
predicate EqualAngle : Angle * Angle
predicate EqualLength : Linelike * Linelike 
predicate RightMarked : Angle
predicate RightUnmarked : Angle
predicate Supplementary : Angle -- TODO broken
predicate Parallel : Linelike * Linelike
predicate Incenter : Point * Triangle
predicate Orthocenter : Point * Triangle
predicate Centroid : Point * Triangle
predicate Circumcenter : Point * Triangle
predicate Parallelogram : Quadrilateral
predicate OnCircle : Circle * Point
predicate CircleCenter : Circle * Point

-- notation "{p, q}" ~ "MkSegment(p, q)"
-- notation "{p, q, r}" ~ "MkTriangle(p, q, r)"
-- notation "{p, q, r, s}" ~ "MkRectangle(p, q, r, s)"
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"