-- Mesh combinatorics
type Vertex
type Edge
type Halfedge
type DualEdge
type Triangle
type Corner

constructor MakeEdge(Vertex i, Vertex j) -> Edge
constructor MakeHalfedge(Vertex from, Vertex to) -> Halfedge
constructor MakeTriangle(Vertex i, Vertex j, Vertex k) -> Triangle
constructor MakeCorner( Vertex inner, Vertex outer1, Vertex outer2 ) -> Corner
constructor MakeDualEdge(Triangle a, Triangle b) -> DualEdge

-- Geometry
type Point
type Circle
type Length

constructor Barycenter(Triangle t) -> Point
constructor Circumcenter(Triangle t) -> Point
constructor Incenter(Triangle t) -> Point

constructor Circumcircle(Triangle t) -> Circle
constructor Incircle(Triangle t) -> Circle

constructor LengthOf(Edge e) -> Length
