-- type Set
type Point

type Linelike
type Ray
type Line
type Segment

type Angle
type Arc

type Triangle
-- type Square
type Rectangle
type Quadrilateral
type Circle

type Plane

-- Subtypes
Ray <: Linelike
Line <: Linelike
Segment <: Linelike
Rectangle <: Quadrilateral
-- Square <: Rectangle

-- Point <: Set
-- Linelike <: Set
-- Triangle <: Set
-- Square <: Set
-- -- Rectangle <: Set
-- Circle <: Set

constructor MkSegment : Point p * Point q -> Segment
constructor MkRay : Point base * Point direction -> Ray
constructor MkLine : Point p * Point q -> Line

constructor InteriorAngle : Point p * Point q * Point r -> Angle
constructor TriangleVertex : Point p * Point q * Point r -> Angle
constructor MkTriangle : Point p * Point q * Point r -> Triangle
-- constructor MkTriangleL : Linelike l * Linelike m * Linelike n -> Triangle
-- constructor MkSquareP : Point p * Point q * Point r * Point s -> Square
-- constructor MkSquare : Point p * Point q * Point r * Point s -> Square -- Assuming the first two points are the segment of a triangle
constructor MkRectangle : Point p * Point q * Point r * Point s -> Rectangle
constructor MkQuadrilateral : Point p * Point q * Point r * Point s -> Quadrilateral
constructor MkMidpoint : Linelike l -> Point
constructor MkCircleR : Point center * Point radius -> Circle
-- constructor MkCircleD : Point diam1 * Point diam2 -> Circle  -- TODO can be reimplemented when #621 is resolved

-- -- TODO: subtyping on the return types
-- function PerpendicularBisector : Linelike -> Ray
-- function Sum : Angle * Angle -> Angle
-- function Intersection : Linelike * Linelike -> Point
-- function Altitude : Triangle * Angle -> Segment
-- function Endpoint : Segment -> Point
function MidSegment : Triangle * Point * Point -> Segment
function Radius : Circle c * Point p -> Segment
function Chord : Circle c * Point p * Point q -> Segment
function Diameter : Circle c * Point p * Point q -> Segment

-- predicate Acute : Angle
-- predicate Obtuse : Angle
predicate On : Point * Linelike
predicate In : Point * Plane
predicate Not : Prop
predicate ParallelMarker1 : Linelike * Linelike
predicate Bisector : Angle * Linelike * Point -- TODO fix
-- predicate Perpendicular : Linelike * Linelike
-- predicate EquilateralT : Triangle
-- predicate RightT : Triangle
predicate Midpoint : Linelike * Point
predicate Scalene : Triangle
predicate Similar : Triangle * Triangle -- TODO implement
predicate Collinear : Point * Point * Point
predicate EqualAngleMarker1 : Angle * Angle
predicate EqualAngleMarker2 : Angle * Angle
predicate EqualAngleMarker3 : Angle * Angle
predicate EqualLengthMarker1 : Segment * Segment
predicate EqualLengthMarker2 : Segment * Segment -- TODO implement
predicate EqualLengthMarker3 : Segment * Segment -- TODO implement
predicate EqualAngle : Angle * Angle
predicate EqualLength : Segment * Segment 
predicate RightMarked : Angle
predicate RightUnmarked : Angle
predicate Supplementary : Angle -- TODO
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
-- notation "[p, q, r, s]" ~ "MkSquare(p, q, r, s)"
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"