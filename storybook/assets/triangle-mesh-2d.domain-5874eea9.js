import{m as e}from"./resolver-f1d01b85.js";const r=e("triangle-mesh-2d"),t=`layout = [shapes, labels]

canvas {
   width = 480
   height = 360
}

Colors {
   color black = rgba(0.,0.,0.,1.)
   color red = rgba(1.,0.,0.,1.)
   color green = rgba(0.,.7,0.,1.)
   color blue = rgba(0.,0,1.,1.)
   color white = rgba(1.,1.,1.,1.)
   color lightGray = rgba(.8,.8,.8,1.)
   color clear = rgba(0.,0.,0.,0.)
   color darkBlue = rgba( 27./255., 31./255., 138./255., 1. )
   color semiBlue = rgba( 27./255., 31./255., 138./255., .2 )
}

Global {
   scalar vertexRadius = 2.5
   scalar pointRadius = 2.0
   scalar lineThickness = 1.
   scalar labelBoundRadius = 8.
   scalar lineOffset = 8.0
   scalar wedgeSize = 30.0

   shape box = Rectangle {
      center: (0.,0.)
      fillColor: none()
      strokeWidth: 2.
      width: canvas.width
      height: canvas.height
   }

   -- string fontFamily = "Linux Libertine O"
   string fontFamily = "Palatino"
   string fontSize = "18px"
}

forall Vertex v {

   vec2 v.center = (?,?)

   -- black dot
   shape v.icon = Circle {
      fillColor: Colors.black
      r: Global.vertexRadius
      center: v.center
   }

   -- make sure the dot is on the canvas
   constraint v.onCanvas = ensure contains( Global.box, v.icon )
}

forall Vertex v
where v has label {

   v.labelAngle = ? in labels
   v.labelRadius = ? in labels

   -- label
   shape v.text = Text {
      center: v.icon.center + v.labelRadius*( cos(v.labelAngle), sin(v.labelAngle ) )
      string: v.label
      fontFamily: Global.fontFamily
      fontSize: Global.fontSize
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- make sure label doesn't get too far from the dot
   ensure inRange( v.labelRadius, 1.02*Global.labelBoundRadius, 1.1*Global.labelBoundRadius ) in labels

   -- invisible circle around label
   -- (used to prevent overlap)
   -- TODO requires #741 to use actual text bounding box instead
   shape v.bounds = Circle {
      center: v.text.center
      r: Global.labelBoundRadius
      fillColor: none()
   }

   -- make sure the label is on the canvas
   ensure contains( Global.box, v.bounds ) in labels

   -- keep the label near the dot
   encourage equal( norm(v.text.center - v.icon.center), Global.labelBoundRadius ) in labels
}

-- make sure vertex labels don't overlap
forall Vertex u; Vertex v
where u has label; v has label { -- requires #774 to replace with "where u has label; v has label"; currently hacked-in using predicate in Domain program
   ensure disjoint( u.bounds, v.bounds ) in labels
}

forall Edge e; Vertex i; Vertex j
where e := MakeEdge(i,j) {

   -- grab edge endpoints
   vec2 pi = i.icon.center
   vec2 pj = j.icon.center
   
   -- black line between endpoints
   shape e.segment = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      strokeLinecap: "round"
   }

   -- (DEBUG) draw full line through segment
   -- vec2 m = (pi+pj)/2.
   -- shape e.line = Line {
   --    start: m + 100.*(pi-m)
   --    end: m + 100.*(pj-m)
   --    strokeColor: rgba( 0., 0., 0., .35 )
   --    strokeWidth: Global.lineThickness
   --    style: "dashed"
   --    strokeDasharray: "4,4"
   -- }
}

forall Edge e; Vertex i; Vertex j
where e := MakeEdge(i,j); i has label; j has label {
   -- make sure edge doesn't cover the
   -- labels of its two endpoints
   ensure disjoint( i.bounds, e.segment, 1. ) in labels
   ensure disjoint( j.bounds, e.segment, 1. ) in labels
}

forall Edge e
where IsBoundaryEdge( e ) {
   override e.segment.strokeWidth = 1.5*Global.lineThickness
}

-- make sure no edge ij covers the label of
-- any other vertex k
forall Edge e; Vertex i; Vertex j; Vertex k
where e := MakeEdge(i,j); k has label {
   
   ensure disjoint( k.bounds, e.segment, 1. ) in labels
}

forall Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k)
{
   vec2 a = i.icon.center
   vec2 b = j.icon.center
   vec2 c = k.icon.center

   -- Define circles associated with each triangle, even
   -- if they're never used, so that they can be easily
   -- referenced by other rules.  (The overhead here is
   -- minimal, especially because we don't need to
   -- differentiate through these quantities unless they
   -- are explicitly incorporated into some later ensure/
   -- encourage statement.
   vec2 t.circumcenter = circumcenter(a,b,c)
   scalar t.circumradius = circumradius(a,b,c)
   vec2 t.incenter = incenter(a,b,c)
   scalar t.inradius = inradius(a,b,c)

   shape t.icon = Path {
      d: pathFromPoints("closed", [a,b,c])
      fillColor: Colors.semiBlue
      strokeColor: none()
   }

   -- Make sure triangles are positively
   -- oriented and not tiny, by making their
   -- signed area greater than some fixed constant.
   scalar A = cross2D( b-a, c-a )
   objective t.positiveArea = encourage lessThan( 4000., A )

   -- Also ensure triangles are not slivers by making
   -- sure two of their angles are in a reasonable range
   -- (the third will of course make the sum equal to 180)
   scalar aTheta = angleFrom( b-a, c-a )
   scalar bTheta = angleFrom( c-b, a-b )
   constraint t.goodAngleA = ensure inRange( aTheta, toRadians(40.), toRadians(80.) ) in labels
   constraint t.goodAngleB = ensure inRange( bTheta, toRadians(40.), toRadians(80.) ) in labels
}

forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k) {

   vec2 p = i.center
   vec2 q = j.center
   vec2 r = k.center
   
   scalar s = .25 -- arc radius as fraction of edge length
   scalar c.R = Global.wedgeSize -- s*norm(q-p)
   vec2 c.u = (q-p)/norm(q-p)
   vec2 c.v = (r-p)/norm(r-p)
   vec2 x = p + c.R*c.u
   vec2 y = p + c.R*c.v

   -- for use in later constraints
   c.wedgeAngle = angleBetween( q-p, r-p )

   shape c.arc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", x, y, (c.R,c.R), 0., 0, 0 )
      strokeWidth: .75
   }

   shape c.arcFill = Path {
      d: wedge( i.center, x, y, (c.R,c.R), 0., 0, 0 )
      fillColor: Colors.semiBlue
      strokeColor: none()
   }

   shape c.arcBall = Circle {
      center: i.center
      r: c.R
      fillColor: none()
      strokeColor: none()
   }
}

forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k); c has text label {

   vec2 c.center = (?,?)
   vec2 p = i.center
   scalar R0 = Global.labelBoundRadius

   shape c.text = Text {
      string: c.label
      center: c.center
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- shape c.text = Text {
   --    string: c.label
   --    center: p + (c.R+R0)*unit(c.u+c.v)
   --    fontSize: Global.fontSize
   --    fontFamily: Global.fontFamily
   --    fontStyle: "italic"
   --    fillColor: Colors.black
   -- }

   shape c.textBounds = Circle {
      center: c.text.center
      r: 1.1 * .5 * max(c.text.width, c.text.height)
      fillColor: none()
   }

   vec2 w = (c.u+c.v)/2.
   vec2 z = c.center - i.center
   ensure lessThan( angleBetween(w,z), c.wedgeAngle/4. )
   encourage near( c.textBounds, i.icon ) in labels
   ensure disjoint( c.textBounds, c.arcBall ) in labels
}

forall Corner c1; Corner c2
where c1 has text label; c2 has text label {
   ensure disjoint( c1.textBounds, c2.textBounds ) in labels
}

forall Point p {

   vec2 p.center = (?,?)

   -- white dot
   shape p.icon = Circle {
      fillColor: Colors.white
      strokeColor: Colors.black
      r: Global.pointRadius
      center: p.center
      strokeWidth: 1.
   }

   -- label
   shape p.text = Text {
      center: (?,?)
      string: ""
      fontFamily: Global.fontFamily
      fontSize: Global.fontSize
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- make sure the dot and label are both
   -- on the canvas
   ensure contains( Global.box, p.icon )

   -- keep the label near the dot
   -- encourage equal( norm(p.text.center - p.icon.center), 0. )
}

-- draw all points above all triangles
forall Point p; Triangle t {
   layer p.icon above t.icon
}

-- draw halfedges as "hook" arrows
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(i,j); t := MakeTriangle(i,j,k)
{
   h.start = i.center
   h.end = j.center
   h.opposite = k.center
}
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(j,k); t := MakeTriangle(i,j,k)
{
   h.start = j.center
   h.end = k.center
   h.opposite = i.center
}
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(k,i); t := MakeTriangle(i,j,k)
{
   h.start = k.center
   h.end = i.center
   h.opposite = j.center
}
forall Halfedge e {

   vec2 p0 = e.start
   vec2 p1 = e.end
   vec2 p2 = e.opposite

   -- unit vectors along the three edges
   vec2 t01 = unit(p1-p0)
   vec2 t12 = unit(p2-p1)
   vec2 t20 = unit(p0-p2)

   -- angle bisectors at the two endpoints
   vec2 z0 = unit(t01-t20)
   vec2 z1 = unit(t12-t01)

   -- angles at the two endpoints
   scalar alpha0 = angleBetween( p1-p0, p2-p0 )
   scalar alpha1 = angleBetween( p2-p1, p0-p1 )

   -- compute corners of constant-width inset
   scalar h = Global.lineOffset
   scalar r0 = h / sin(alpha0/2.)
   scalar r1 = h / sin(alpha1/2.)
   vec2 q0 = p0 + r0*z0
   vec2 q1 = p1 + r1*z1

   -- compute truncated segment endpoints
   scalar L = norm(p1-p0)
   vec2 m = (q0+q1)/2.
   vec2 Q0 = m + (L/4.)*t01
   vec2 Q1 = m - (L/4.)*t01

   vec2 n = rot90(t01)
   vec2 Q2 = Q0 + h*(n-t01)

   e.icon = Path {
      strokeWidth: 2.*Global.lineThickness
      strokeColor: Colors.black
      fillColor: none()
      d: pathFromPoints("open", [Q2,Q0,Q1])
   }

   e.labelText = Text {
      center: m + h*n
      string: e.label
      fontFamily: "Courier"
      fontSize: "6.75px"
      fillColor: Colors.black
      rotation: -toDegrees(angleOf(t01))
   }
}

-- default shape for a circle
forall Circle C {

   shape C.icon = Circle {
      center: (?,?)
      r: ?
      fillColor: Colors.semiBlue
   }

   constraint C.validRadius = ensure inRange( C.icon.r, 100, 200 )
   constraint C.onCanvas = ensure contains(Global.box, C.icon)
}

-- Barycenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Barycenter(t) {
   override p.center = barycenter( i.center, j.center, k.center )
   override p.icon.fillColor = Colors.red
}

-- Circumcenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Circumcenter(t) {
   -- put point at circumcenter 
   override p.center = t.circumcenter
}

-- Circumcircle
forall Circle C; Triangle t
where C := Circumcircle(t) {

   -- since the circumcircle can be huge, it's
   -- too restrictive to force it on the canvas
   delete C.onCanvas

   -- the circumradius will be valid by construction
   delete C.validRadius

   override C.icon.center = t.circumcenter
   override C.icon.r = t.circumradius
   override C.icon.fillColor = none()
   override C.icon.strokeColor = rgba( 0., 0., 0., .2 )
   override C.icon.strokeWidth = 1.5
}

-- Incenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Incenter(t) {

   -- put point at incenter 
   override p.center = t.incenter
}

-- Incircle
forall Circle C; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); C := Incircle(t) {
   
   delete C.onCanvas
   delete C.validRadius

   override C.icon.center = t.incenter
   override C.icon.r = t.inradius
   override C.icon.fillColor = none()
   override C.icon.strokeColor = rgba( 0., 0., 0., .2 )
   override C.icon.strokeWidth = 1.5
}

forall DualEdge d; Triangle t1; Triangle t2
where d := MakeDualEdge(t1,t2) {

   shape d.segment = Line {
      start: t1.circumcenter
      end: t2.circumcenter
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      style: "dashed"
      strokeDasharray: "4,4"
   }
}

-- if a length is constructed from an edge, set its
-- endpoints to the edge endpoints (setting things
-- up this way allows us to keep Length fairly generic,
-- e.g., it can also be used to draw the length of a
-- dual edge without duplicating code)
forall Length l; Edge e; Vertex i; Vertex j
where l := EdgeLength(e); e := MakeEdge(i,j) {
   vec2 l.x0 = e.segment.start
   vec2 l.x1 = e.segment.end
}
   
-- if a length is constructed from a dual edge, set its
-- endpoints to the dual edge endpoints
forall Length l; DualEdge d; Triangle s; Triangle t
where l := DualEdgeLength(d); d := MakeDualEdge(s,t) {
   vec2 l.x0 = d.segment.start
   vec2 l.x1 = d.segment.end
}
   
-- draw lengths as segments with straight ends
forall Length l {

   vec2 u = unit(l.x1-l.x0)
   vec2 n = -rot90(u)
   scalar w = Global.lineOffset

   scalar l.strokeWidth = .8*Global.lineThickness

   shape l.marker = Line {
      start: l.x0 + w*n
      end: l.x1 + w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }
   shape l.end0 = Line {
      start: l.marker.start + .35*w*n
      end: l.marker.start - .35*w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }
   shape l.end1 = Line {
      start: l.marker.end + .35*w*n
      end: l.marker.end - .35*w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }

   vec2 m = (l.x0+l.x1)/2.
   shape l.text = Equation {
      center: m + 2.*w*n
      string: l.label
      fillColor: Colors.black
      fontSize: Global.fontSize
   }
}

-- mark a positively-oriented triangle with a
-- circular arrow pointing in the counter-clockwise
-- direction
forall Triangle t
where IsPositivelyOriented(t) {
   
   scalar R = .5*t.inradius
   scalar t0 = toRadians( -60. )
   scalar t1 = toRadians( 240. )
   vec2 p0 = t.incenter + R*( cos(t0), sin(t0) )
   vec2 p1 = t.incenter + R*( cos(t1), sin(t1) )

   shape t.orientationArc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", p0, p1, (R,R), 0., 1, 0 )
      strokeWidth: 2.*Global.lineThickness
      endArrowhead: "straight"
      endArrowheadSize: .5
   }
}
forall Triangle t
where IsNegativelyOriented(t) {
   
   scalar R = .5*t.inradius
   scalar t0 = toRadians( -60. )
   scalar t1 = toRadians( 240. )
   vec2 p0 = t.incenter + R*( cos(t0), sin(t0) )
   vec2 p1 = t.incenter + R*( cos(t1), sin(t1) )

   shape t.orientationArc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", p0, p1, (R,R), 0., 1, 0 )
      strokeWidth: 2.*Global.lineThickness
      startArrowhead: "straight"
      startArrowheadSize: .5
   }
}

-- draw flipped edge as dashed line
forall Edge e
where IsFlipped( e ) {
   e.segment.strokeDasharray = "5,5"
   override e.segment.strokeLinecap = "butt"
}

-- put edge labels on one side of the edge or another
forall Edge ij; Vertex i; Vertex j
where ij := MakeEdge(i,j); ij has math label {

   vec2 pi = i.center
   vec2 pj = j.center
   vec2 m = (pi+pj)/2.
   vec2 u = unit(pj-pi)
   vec2 n = rot90(u)

   vec2 ij.c = (?,?)
   scalar h = ?

   -- ensure that offset h is either plus or minus 1
   scalar s = h*h
   ensure equal( 1., s )

   -- put label at edge midpoint, offset in the plus
   -- or minus normal direction by some fixed amount
   shape ij.labelText = Equation {
      center: m + 12.*h*n
      string: ij.label
      fontSize: Global.fontSize
      fillColor: Colors.black
   }
}

-- if triangles are concyclic, put vertices on a common circle
forall Triangle s; Triangle t; Vertex i; Vertex j; Vertex k; Vertex l
where s := MakeTriangle(i,j,k); t := MakeTriangle(j,i,l); Concyclic(s,t) {

   scalar r0 = 100.
   vec2 c0 = (?,?)

   shape disk = Circle {
      center: c0
      r: r0
      fillColor: Colors.semiBlue
   }

   ensure contains( Global.box, disk )
   ensure equal( norm( i.center-c0 ), r0 )
   ensure equal( norm( j.center-c0 ), r0 )
   ensure equal( norm( k.center-c0 ), r0 )
   ensure equal( norm( l.center-c0 ), r0 )
}

forall Point p; Edge ab; Edge cd; Vertex a; Vertex b; Vertex c; Vertex d
where p := Intersection(ab,cd); ab := MakeEdge(a,b); cd := MakeEdge(c,d) {

   vec2 x = a.center
   vec2 y = b.center
   vec2 z = c.center
   vec2 w = d.center

   override p.center = lineLineIntersection( x, y, z, w )

   layer p.icon above ab.segment
   layer p.icon above cd.segment
}

-- Specific to diagrams/angle-equivalence.substance
-- TODO requires #776 to import these rules from an example-specific Style module

-- Don't draw vertices as dots in this
-- example (but still keep the icons, for
-- other purposes).
--- angle-equivalence --- forall Vertex v {
--- angle-equivalence ---    override v.icon.r = 0.
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- similarityMap {
--- angle-equivalence ---    scalar scale = .5
--- angle-equivalence ---    scalar angle = ?
--- angle-equivalence ---    vec2 shift = (canvas.width/3.,0.)
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- forall Vertex v; Vertex v0
--- angle-equivalence --- where v := similarity(v0) {
--- angle-equivalence --- 
--- angle-equivalence ---    scalar s = similarityMap.scale
--- angle-equivalence ---    scalar theta = similarityMap.angle
--- angle-equivalence ---    vec2 u = similarityMap.shift
--- angle-equivalence --- 
--- angle-equivalence ---    override v.center = s*rotateBy(v0.center,theta) + u
--- angle-equivalence ---    delete v.onCanvas
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- -- don't constrain triangle geometry so much
--- angle-equivalence --- forall Triangle t; Vertex i; Vertex j; Vertex k; Vertex i0
--- angle-equivalence --- where t := MakeTriangle(i,j,k); i := similarity(i0) {
--- angle-equivalence ---    delete t.positiveArea
--- angle-equivalence ---    delete t.goodAngleA
--- angle-equivalence ---    delete t.goodAngleB
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- -- make sure triangles are disjoint if they are made similar
--- angle-equivalence --- forall Triangle t1, t2
--- angle-equivalence --- where t1 := MakeTriangle(i, j, k); t2 := MakeTriangle(I, J, K); i := similarity(i); j := similarity(j); k := similarity(k);
--- angle-equivalence --- with Vertex i, j, k, I, J, K {
--- angle-equivalence ---    ensure disjoint(t1.icon, t2.icon)
--- angle-equivalence --- }

-- Specific to diagrams/concyclic-pair.substance
-- TODO requires #776 to import these rules from an example-specific Style module

--- concyclic-pair --- -- make vertex dots bigger
--- concyclic-pair --- forall Vertex v {
--- concyclic-pair ---    override v.icon.r = 1.75*Global.vertexRadius
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make corner labels larger and dark blue
--- concyclic-pair --- forall Corner c; Vertex i; Vertex j; Vertex k
--- concyclic-pair --- where c := MakeCorner(i,j,k); c has text label {
--- concyclic-pair ---    override c.text.fontSize = "25px"
--- concyclic-pair ---    override c.text.fillColor = Colors.darkBlue
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- don't draw black outline on arcs
--- concyclic-pair --- forall Corner c; Vertex i; Vertex j; Vertex k
--- concyclic-pair --- where c := MakeCorner(i,j,k) {
--- concyclic-pair ---    delete c.arc
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make edges thicker
--- concyclic-pair --- forall Edge e {
--- concyclic-pair ---    override e.segment.strokeWidth = 2*Global.lineThickness
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make boundary edges thicker
--- concyclic-pair --- forall Edge e
--- concyclic-pair --- where IsBoundaryEdge( e ) {
--- concyclic-pair ---    override e.segment.strokeWidth = 3*Global.lineThickness
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- corner labels shouldn't overlap diagonals
--- concyclic-pair --- forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
--- concyclic-pair --- where c := MakeCorner(i,j,k); e := MakeEdge(l,i) {
--- concyclic-pair ---    ensure disjoint( c.textBounds, e.segment )
--- concyclic-pair --- }
--- concyclic-pair --- forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
--- concyclic-pair --- where c := MakeCorner(i,j,k); e := MakeEdge(i,l) {
--- concyclic-pair ---    ensure disjoint( c.textBounds, e.segment )
--- concyclic-pair --- }`,a=`-- Mesh combinatorics
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

predicate IsBoundaryVertex(Vertex v)
predicate IsBoundaryEdge(Edge e)
predicate IsBoundaryTriangle(Triangle t)

predicate HasLabel(Vertex v)

predicate IsPositivelyOriented(Triangle t)
predicate IsNegativelyOriented(Triangle t)

-- Geometry
type Point
type Circle
type Length

constructor Barycenter(Triangle t) -> Point
constructor Circumcenter(Triangle t) -> Point
constructor Incenter(Triangle t) -> Point

constructor Circumcircle(Triangle t) -> Circle
constructor Incircle(Triangle t) -> Circle

constructor EdgeLength(Edge e) -> Length
constructor DualEdgeLength(DualEdge d) -> Length

constructor Intersection(Edge e, Edge f) -> Point

-- Specific to angle-equivalence.substance
function similarity(Vertex i) -> Vertex

-- Specific to concyclic-pair.substance
predicate IsFlipped(Edge e)
predicate Concyclic(Triangle s, Triangle t)

`;export{a as d,r,t as s};
//# sourceMappingURL=triangle-mesh-2d.domain-5874eea9.js.map
