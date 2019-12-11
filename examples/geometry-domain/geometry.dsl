type Set
type Point

type Linelike
type Ray
type Line
type Segment

type Angle

type Triangle
type Square
type Rectangle
type Circle

-- TOOD: fix my syntax highlighting

-- Subtypes. Should Linelike be a typeclass?
Ray <: Linelike
Line <: Linelike
Segment <: Linelike
Square <: Rectangle

Point <: Set
Linelike <: Set
Triangle <: Set
Square <: Set
Rectangle <: Set
Circle <: Set

-- TODO: optional naming for constructors

-- TODO: rename to MakeSegment and MakeTriangle (etc.) everywhere
constructor MkSegment : Point p * Point q -> Segment
constructor MkRay : Point base * Point direction -> Ray
constructor MkLine : Point p * Point q -> Point

-- constructor InteriorAngle : Linelike l * Linelike m -> Angle
constructor InteriorAngle : Point p * Point q * Point r -> Angle
constructor TriangleVertex : Point p * Point q * Point r -> Angle

constructor MkTriangleP : Point p * Point q * Point r -> Triangle
constructor MkTriangleL : Linelike l * Linelike m * Linelike n -> Triangle
constructor MkCircleR : Point center * Point radius -> Circle
constructor MkCircleD : Point diam1 * Point diam2 -> Circle
constructor MkSquareP : Point p * Point q * Point r * Point s -> Square
constructor MkSquareSegP : Point p * Point q * Point r * Point s -> Square -- Assuming the first two points are the segment of a triangle
constructor MkRectangleP : Point p * Point q * Point r * Point s -> Rectangle

-- TODO: subtyping on the return types
function Midpoint : Linelike -> Point
function Bisector : Angle -> Ray
function PerpendicularBisector : Linelike -> Ray
function Sum : Angle * Angle -> Angle
function Intersection : Linelike * Linelike -> Point
function Altitude : Triangle * Angle -> Segment

predicate Acute : Angle
predicate Obtuse : Angle
predicate Right : Angle
predicate On : Point * Linelike
predicate Not : Predicate
predicate Parallel : Linelike * Linelike
predicate Perpendicular : Linelike * Linelike
predicate EquilateralT : Triangle
predicate RightT : Triangle
predicate Scalene : Triangle
predicate Similar : Triangle * Triangle
predicate Disjoint : Set * Set

notation "{p, q}" ~ "MkSegment(p, q)"
notation "{p, q, r}" ~ "MkTriangleP(p, q, r)"
notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"