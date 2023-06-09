const n=`-- ~~~~~~~~~~~~~~~~ TYPES ~~~~~~~~~~~~~~~~
type Shape
type Point <: Shape
type Linelike <: Shape
type Ray <: Linelike
type Line <: Linelike
type Segment <: Linelike

type Angle <: Shape

type Triangle <: Shape
type Quadrilateral <: Shape
type Rectangle <: Quadrilateral
type Circle <: Shape

type Plane <: Shape

-- ~~~~~~~~~~~~~~~~ CONSTRUCTORS ~~~~~~~~~~~~~~~~
-- Lines and Points
constructor Segment(Point p, Point q)
constructor Ray(Point base, Point direction)
constructor Line(Point p, Point q)
constructor Midpoint(Linelike l) -> Point

-- Angles
constructor InteriorAngle(Point p, Point q, Point r) -> Angle

-- Polygons/Shapes
constructor Triangle(Point p, Point q, Point r)
constructor Rectangle(Point p, Point q, Point r, Point s)
constructor Quadrilateral(Point p, Point q, Point r, Point s)
constructor CircleR(Point center, Point radius) -> Circle
-- constructor CircleD(Point diam1, Point diam2) -> Circle  -- TODO can be reimplemented when #621 is resolved

-- ~~~~~~~~~~~~~~~~ FUNCTIONS ~~~~~~~~~~~~~~~~
-- Lines and Points
function Bisector(Angle) -> Ray
function PerpendicularBisector(Segment, Point) -> Segment
function PerpendicularBisectorLabelPts(Segment, Point, Point) -> Segment -- same as PerpendicularBisector but it takes a segment + 2 points as args for labeling

-- Polygons/Shapes
function MidSegment(Triangle, Point, Point) -> Segment
function Radius(Circle c, Point p) -> Segment
function Chord(Circle c, Point p, Point q) -> Segment
function Diameter(Circle c, Point p, Point q) -> Segment

-- Unimplemented
-- function Sum(Angle, Angle) -> Angle
-- function Intersection(Linelike, Linelike) -> Point
-- function Altitude(Triangle, Angle) -> Segment
-- function Endpoint(Segment) -> Point

-- ~~~~~~~~~~~~~~~~ PREDICATES ~~~~~~~~~~~~~~~~
-- Lines and Points
predicate On(Point, Linelike)
predicate In(Point, Plane)
predicate Midpoint(Linelike, Point)
predicate Collinear(Point, Point, Point)
predicate ParallelMarker1(Linelike, Linelike)
predicate EqualLengthMarker(Linelike, Linelike)
predicate EqualLength(Linelike, Linelike)
predicate Parallel(Linelike, Linelike)

-- Angles
predicate Acute(Angle) 
predicate Obtuse(Angle) 
predicate RightMarked(Angle)
predicate RightUnmarked(Angle)
predicate AngleBisector(Angle, Linelike)
predicate EqualAngleMarker(Angle, Angle)
predicate EqualAngle(Angle, Angle)

-- Polygons/Shapes
predicate Parallelogram(Quadrilateral)
predicate OnCircle(Circle, Point)
predicate CircleCenter(Circle, Point)
predicate Incenter(Point, Triangle)
predicate Orthocenter(Point, Triangle)
predicate Centroid(Point, Triangle)
predicate Circumcenter(Point, Triangle)

-- notation "{p, q}" ~ "Segment(p, q)"
-- notation "{p, q, r}" ~ "Triangle(p, q, r)"
-- notation "{p, q, r, s}" ~ "Rectangle(p, q, r, s)"
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"`;export{n as d};
//# sourceMappingURL=geometry.domain-952a3e66.js.map
