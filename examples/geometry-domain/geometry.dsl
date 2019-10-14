type Point

type Linelike
type Ray
type Line
type Segment

type Angle

type Triangle
type Square
type Circle

-- TOOD: fix my syntax highlighting

-- Subtypes. Should Linelike be a typeclass?
Ray <: Linelike
Line <: Linelike
Segment <: Linelike

-- TODO: optional naming for constructors

-- TODO: rename to MakeSegment and MakeTriangle (etc.) everywhere
constructor Intersection : Linelike l * Linelike m -> Point
constructor MkSegment : Point p * Point q -> Segment
constructor MkRay : Point base * Point direction -> Ray
constructor MkLine : Point p * Point q -> Point

-- constructor AngleBetween : Linelike l * Linelike m -> Angle
constructor AngleBetween : Point p * Point q * Point r -> Angle
constructor TriangleVertex : Point p * Point q * Point r -> Angle

constructor MkTriangleP : Point p * Point q * Point r -> Triangle
constructor MkTriangleL : Linelike l * Linelike m * Linelike n -> Triangle
constructor MkCircleR : Point center * Point radius -> Circle
constructor MkCircleD : Point diam1 * Point diam2 -> Circle

-- TODO: subtyping on the return types
function Midpoint : Linelike -> Point
function Bisector : Angle -> Ray
function PerpendicularBisector : Linelike -> Ray
function Sum : Angle * Angle -> Angle

predicate Acute : Angle
predicate Obtuse : Angle
predicate Right : Angle
predicate In : Point * Line
predicate Not : Predicate
predicate Parallel : Linelike * Linelike
predicate Perpendicular : Linelike * Linelike
predicate EquilateralT : Triangle
predicate RightT : Triangle
predicate Scalene : Triangle
predicate Similar : Triangle * Triangle

notation "{p, q}" ~ "MkSegment(p, q)"
notation "{p, q, r}" ~ "MkTriangleP(p, q, r)"