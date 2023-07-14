import{m as n}from"./resolver-b9429209.js";import"./iframe-8d1c39a4.js";const e=`-- outer part of "A"
Point a1, a2, a3, a4, a5, a6, a7, a8
Let a12 := Segment( a1, a2 )
Let a23 := Segment( a2, a3 )
Let a34 := Segment( a3, a4 )
Let a45 := Segment( a4, a5 )
Let a56 := Segment( a5, a6 )
Let a67 := Segment( a6, a7 )
Let a78 := Segment( a7, a8 )
Let a81 := Segment( a8, a1 )

-- inner hole of "A"
Point b1, b2, b3
Let b12 := Segment( b1, b2 )
Let b23 := Segment( b2, b3 )
Let b31 := Segment( b3, b1 )

-- all of these segments are oriented
IsOriented( a12 )
IsOriented( a23 )
IsOriented( a34 )
IsOriented( a45 )
IsOriented( a56 )
IsOriented( a67 )
IsOriented( a78 )
IsOriented( a81 )
IsOriented( b12 )
IsOriented( b23 )
IsOriented( b31 )

-- point at which we want to measure the winding number
Point x
Label x $x$

-- oriented angle of each segment relative to x
Angle alpha12 := InteriorAngle(x,a1,a2)
Angle alpha23 := InteriorAngle(x,a2,a3)
Angle alpha34 := InteriorAngle(x,a3,a4)
Angle alpha45 := InteriorAngle(x,a4,a5)
Angle alpha56 := InteriorAngle(x,a5,a6)
Angle alpha67 := InteriorAngle(x,a6,a7)
Angle alpha78 := InteriorAngle(x,a7,a8)
Angle alpha81 := InteriorAngle(x,a8,a1)
Angle beta12 := InteriorAngle(x,b1,b2)
Angle beta23 := InteriorAngle(x,b2,b3)
Angle beta31 := InteriorAngle(x,b3,b1)
IsOriented( alpha12 )
IsOriented( alpha23 )
IsOriented( alpha34 )
IsOriented( alpha45 )
IsOriented( alpha56 )
IsOriented( alpha67 )
IsOriented( alpha78 )
IsOriented( alpha81 )
IsOriented( beta12 )
IsOriented( beta23 )
IsOriented( beta31 )

`,a=n("walk-on-spheres"),t=`-- 

layout = [ shapeStage, labelStage ]

-- diagram dimensions (in px; multiply by 96/72 to convert to pt)
canvas {
   width = 306.66 -- ==230
   height = 200.00 -- ==150
}

--------------------------------------------------
-- Global constants ------------------------------
--------------------------------------------------

colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color lightGray = rgba(.8,.8,.8,1)
   color clearGray = rgba(0,0,0,.2)
   color darkBlue = #1b1f8a
}

global {
   scalar toPt = 96/72 -- constant for converting sizes from pt to px

   -- dot style
   color dotColor = colors.black
   scalar dotRadius = toPt * 1.75
   scalar dotStroke = toPt * .75

   -- line and arrow style
   scalar lineWidth = toPt * 1
   scalar arrowLength = 25.0

   -- label style
   string labelStyle = "italic"
   string labelFamily = "Linux Libertine"
   color labelColor = #000
   string labelSize = "13.3333px" -- equivalent to 10pt (multiply by 96/72)
   scalar labelHeight = 10
   scalar labelDistance = 8.0

   -- layout parameters
   scalar padding = 2.0 -- amount of padding to prevent overlap
}

--------------------------------------------------
-- Points ----------------------------------------
--------------------------------------------------

forall Point p {

   -- location
   scalar p.x0 = ? in shapeStage
   scalar p.x1 = ? in shapeStage
   p.x = (p.x0,p.x1)

   shape p.icon = Circle {
      fillColor: global.dotColor
      r: global.dotRadius
      center: p.x
   }
}

forall Point p
where p has label {

   scalar theta = ? in labelStage
   scalar r = global.labelDistance

   p.labelText = Equation {
      center: p.x + r*(cos(theta),sin(theta))
      string: p.label
      fontSize: global.labelSize
      --fontStyle: global.labelStyle
      --fontFamily: global.labelFamily
      fillColor: global.labelColor
   }
   layer p.labelText above p.icon
}

--------------------------------------------------
-- Vectors ---------------------------------------
--------------------------------------------------

forall Vector v {

   -- vector origin
   scalar v.x0 = ? in shapeStage
   scalar v.x1 = ? in shapeStage
   v.x = (v.x0,v.x1)

   -- vector direction
   scalar v.v0 = ? in shapeStage
   scalar v.v1 = ? in shapeStage
   v.v = (v.v0,v.v1)

   shape v.icon = Line {
      start: v.x
      end: v.x + global.arrowLength*unit(v.v)
      strokeColor: colors.darkBlue
      strokeWidth: global.lineWidth
      endArrowhead: "straight"
      endArrowheadSize: .5
      fill: "none"
   }
}

forall Vector v
where v has label {

   scalar phi = ? in labelStage
   scalar theta = (MathPI()/2.)*cos(phi)
   vec2 w = unit(v.v)
   vec2 u = rotateBy( w, theta )
   scalar r = global.labelDistance

   v.labelText = Equation {
      string: v.label
      center: v.x + global.arrowLength*w + r*u
      fontSize: global.labelSize
      fillColor: v.icon.strokeColor
      --fontStyle: global.labelStyle
      --fontFamily: global.labelFamily
      ensureOnCanvas: false
   }
}

forall Vector v; Point p
where RootedAt(p,v) {
   override v.x = p.x
   layer p.icon above v.icon
}

-- draw segment normal as a vector at its midpoint
forall Vector v; Segment s
where v := Normal(s) {
   override v.x = (s.a + s.b)/2
   override v.v = global.arrowLength * rot90(unit(s.b - s.a))
}

--------------------------------------------------
-- Segments --------------------------------------
--------------------------------------------------

forall Segment s {

   -- first endpoint
   scalar a0 = ? in shapeStage
   scalar a1 = ? in shapeStage
   vec2 s.a = (a0,a1)

   -- second endpoint
   scalar b0 = ? in shapeStage
   scalar b1 = ? in shapeStage
   vec2 s.b = (b0,b1)

   shape s.icon = Line {
      start: s.a
      end: s.b
      strokeColor: colors.black
      strokeWidth: global.lineWidth
      fill: "none"
   }
}

forall Segment s; Point p; Point q
where s := Segment(p,q) {
   -- replace the endpoints with the
   -- locations of the given points
   override s.a = p.x
   override s.b = q.x
}

forall Segment s
where s has label {
   s.labelText = Equation {
      string: s.label
      fontSize: global.labelSize
      --fontStyle: global.labelStyle
      --fontFamily: global.labelFamily
      fillColor: global.labelColor
      ensureOnCanvas: false
   }
   layer s.labelText above s.icon

   vec2 m = (s.a + s.b)/2.
   scalar d = norm( m - s.labelText.center )
   encourage d == global.labelDistance in labelStage

   ensure disjoint( s.labelText, s.icon, global.padding ) in labelStage
}

-- Oriented segments are given an orientation marker in
-- the middle, going from the first to second point used
-- to define the segment.
forall Segment s
where IsOriented( s ) {

   scalar h = global.dotRadius * 2.5

   vec2 m = (s.a + s.b)/2
   vec2 t = unit(s.b - s.a) -- tangent
   vec2 n = rot90(t) -- normal
   shape s.orientationMarker = Polyline {
      points: ( m + h*(n-t/2), m + h*t/2, m + h*(-n-t/2) )
      strokeColor: s.icon.strokeColor
      strokeWidth: s.icon.strokeWidth
   }
}

forall Segment s
where IsDashed( s ) {
   override s.icon.style = "dashed"
   override s.icon.strokeDasharray = "4,4"
}

--------------------------------------------------
-- Angle markers --------------------------------
--------------------------------------------------

forall Angle a {
   scalar a.theta0 = unitRandom() * 2*MathPI()
   scalar a.theta1 = unitRandom() * 2*MathPI()
   scalar a.radius = 20
   vec2 a.center = (0,0)

   shape a.icon = Path {
      d: circularArc( "open", a.center, a.radius, a.theta0, a.theta1 )
      strokeColor: #000
      strokeWidth: 1
      ensureOnCanvas: false
   }
}

forall Angle a
where IsOriented( a ) {
   override a.icon.endArrowhead = "straight"
   override a.icon.endArrowheadSize = 0.5
}

forall Angle a; Point x,y,z
where a := InteriorAngle(x,y,z) {
   vec2 u = y.x - x.x
   vec2 v = z.x - x.x
   override a.theta0 = angleOf(u)
   scalar s = sign(cross2D(u,v))
   scalar arrowheadArc = 4
   scalar dTheta = s * ( angleBetween(u,v) - arrowheadArc/a.radius )
   override a.theta1 = a.theta0 + dTheta
   override a.center = x.x
}

forall Angle a; Triangle t {
   layer a.icon above t.icon
}

--------------------------------------------------
-- Length markers --------------------------------
--------------------------------------------------

-- draw length markers as segments with perpendicular markers at the ends
forall Length l {

   -- marker parameters
   scalar l.markerSize = 2*global.dotRadius
   scalar l.shortening = 2*global.dotRadius

   -- first endpoint of labeled segment
   scalar a0 = ? in shapeStage
   scalar a1 = ? in shapeStage
   vec2 l.a = (a0,a1)

   -- second endpoint of labeled segment
   scalar b0 = ? in shapeStage
   scalar b1 = ? in shapeStage
   vec2 l.b = (b0,b1)

   vec2 l.t = unit( l.a - l.b ) -- tangent
   vec2 l.n = rot90( l.t ) -- normal

   -- offset length marker by +/- offset size
   scalar offsetSize = 8.0
   scalar l.h = ? in shapeStage
   ensure l.h*l.h == sqr(offsetSize) in shapeStage
   -- scalar l.h = 8

   -- endpoints of length marker
   scalar l.x = l.a - l.shortening*l.t + l.h*l.n
   scalar l.y = l.b + l.shortening*l.t + l.h*l.n

   l.icon = Line {
      start: l.x
      end: l.y
      strokeWidth: .75
      strokeColor: colors.lightGray
      fill: "none"
   }
   l.end0 = Line {
      start: l.x - l.markerSize*l.n
      end: l.x + l.markerSize*l.n
      strokeWidth: l.icon.strokeWidth
      strokeColor: l.icon.strokeColor
      fill: "none"
   }
   l.end1 = Line {
      start: l.y - l.markerSize*l.n
      end: l.y + l.markerSize*l.n
      strokeWidth: l.icon.strokeWidth
      strokeColor: l.icon.strokeColor
      fill: "none"
   }
}

forall Length l; Point a; Point b
where l := LengthBetween(a,b) {
   override l.a = a.x
   override l.b = b.x
}

forall Length l; Segment s
where l := LengthOf(s) {
   override l.a = s.a
   override l.b = s.b
   override l.icon.ensureOnCanvas = false
}

forall Length l
where l has label {

   -- offset label in same direction as length marker
   vec2 m = (l.x + l.y)/2.
   vec2 c = m + l.h*l.n/1.5

   l.labelText = Equation {
      center: c
      string: l.label
      fontSize: global.labelSize
      -- fontStyle: global.labelStyle
      -- fontFamily: global.labelFamily
      fillColor: l.icon.strokeColor
      ensureOnCanvas: false
   }
   layer l.labelText above l.icon
}

--------------------------------------------------
-- Triangles -------------------------------------
--------------------------------------------------

forall Triangle t
{
   -- vertices
   vec2 t.a = (? in shapeStage, ? in shapeStage)
   vec2 t.b = (? in shapeStage, ? in shapeStage)
   vec2 t.c = (? in shapeStage, ? in shapeStage)

   shape t.icon = Polygon {
      points: [ t.a, t.b, t.c ]
      fillColor: colors.clearGray
   }
}

forall Triangle t; Point p1; Point p2; Point p3
where t := Triangle(p1, p2, p3)
{
   override t.a = p1.x
   override t.b = p2.x
   override t.c = p3.x
}

forall Point p; Triangle t; Point q0; Point q1; Point q2
where InTri( p, t ); t := Triangle(q0, q1, q2) {
   override p.x = triangleRandom(q0.x, q1.x, q2.x)
}


--------------------------------------------------
-- Polylines -------------------------------------
--------------------------------------------------

forall Polyline M {

   vec2 M.p0 = (? in shapeStage, ? in shapeStage)
   scalar theta0 = random(0,1)*2.*MathPI()
   scalar theta1 = theta0 + random(.5,1)*MathPI()/2
   scalar theta2 = theta1 + random(.5,1)*MathPI()/2
   scalar theta3 = theta2 - random(.5,1)*MathPI()/2
   scalar L = canvas.width/5
   vec2 M.p1 = M.p0 + L*(cos(theta0),sin(theta0))
   vec2 M.p2 = M.p1 + L*(cos(theta1),sin(theta1))
   vec2 M.p3 = M.p2 + L*(cos(theta2),sin(theta2))
   vec2 M.p4 = M.p3 + L*(cos(theta3),sin(theta3))

   shape M.icon = Polyline {
      points: [ M.p0, M.p1, M.p2, M.p3, M.p4 ]
   }
}

--------------------------------------------------
-- Circles ---------------------------------------
--------------------------------------------------

forall Circle c {
   -- center
   scalar x0 = ? in shapeStage
   scalar x1 = ? in shapeStage
   vec2 c.x = ( x0, x1 )

   -- radius
   scalar c.r = ? in shapeStage

   shape c.icon = Circle {
      center: c.x
      r: c.r
      fillColor: none()
      strokeColor: colors.black
      strokeWidth: global.lineWidth
   }
}

forall Point p; Circle c
where OnCircle( p, c ) {
   override p.x = c.icon.r*circleRandom() + c.icon.center
}

--------------------------------------------------
-- Disks -----------------------------------------
--------------------------------------------------

forall Disk d {
   -- center
   scalar x0 = ? in shapeStage
   scalar x1 = ? in shapeStage
   vec2 c.x = ( x0, x1 )

   -- radius
   scalar c.r = ? in shapeStage

   shape d.icon = Circle {
      center: c.x
      r: c.r
   }
}

forall Point p; Disk d
where InDisk( p, d ) {
   override p.x = d.icon.r*diskRandom() + d.icon.center
}

--------------------------------------------------
-- Chords ----------------------------------------
--------------------------------------------------

forall Chord c {
   shape c.icon = Line {
      strokeWidth: .25*global.lineWidth
      strokeColor: colors.black
      fill: "none"
   }
}

forall Chord c; Disk d
where OfDisk(c,d) {
   scalar t0 = 2.*random(0,MathPI())
   scalar t1 = 2.*random(0,MathPI())
   c.icon.start = d.icon.r*(cos(t0),sin(t0)) + d.icon.center
   c.icon.end = d.icon.r*(cos(t1),sin(t1)) + d.icon.center
}

--------------------------------------------------
-- Rays ------------------------------------------
--------------------------------------------------

forall Ray r {
   r.icon = Line {
      strokeColor: colors.lightGray
      style: "dashed"
      strokeDasharray: "4,4"
      ensureOnCanvas: false
      fill: "none"
   }
}

forall Ray r; Point p; Vector v
where r := RayFrom(p,v) {

   override v.x = p.x
   override v.icon.ensureOnCanvas = false

   -- draw the ray as a line whose length
   -- is equal to the diameter of the canvas,
   -- so that it always goes "off screen" (but
   -- isn't so crazy long that it causes problems
   -- for display, editing, etc.)
   vec2 d = (canvas.width,canvas.height)
   scalar L = norm(d)
   vec2 r.x = p.x
   vec2 r.v = unit(v.v)
   r.icon.start = r.x
   r.icon.end = r.x + L*r.v

   layer r.icon below v.icon
}

--------------------------------------------------
-- Ray intersections -----------------------------
--------------------------------------------------

forall Point q; Ray r; Point p; Vector v; Segment s; Point a; Point b
where q := RaySegmentIntersection(r,s); r := RayFrom(p,v); s := Segment(a,b) {

   -- put the intersection point at some random point in the segment
   scalar u = random(.2,.8)
   override q.x = (1-u)*a.x + u*b.x

   -- make the ray point toward the intersection point
   override v.v = unit(q.x - p.x)

   -- make the ray origin reasonably far from the intersection point
   ensure norm(p.x-q.x) > 1.5*global.arrowLength in shapeStage

   -- make sure the angle of intersection is not too shallow
   vec2 w = unit(b.x-a.x)
   ensure sqr(dot(v.v,w)) < .25 in shapeStage

   -- draw the intersection point as a white dot
   override q.icon.fillColor = colors.white
   override q.icon.strokeColor = colors.black
   override q.icon.strokeWidth = global.dotStroke
}

forall Point q; Ray r; Set s
where q := RayIntersection(r,s) {

   override q.x = rayIntersect( s.icon, r.x, r.v )

   -- since the intersection point could be off the
   -- canvas, we shouldn't try to enforce a constraint
   -- keeping it on the canvas
   override q.icon.ensureOnCanvas = false

   -- draw the intersection point as a white dot
   override q.icon.fillColor = colors.white
   override q.icon.strokeColor = colors.black
   override q.icon.strokeWidth = global.dotStroke
}


--------------------------------------------------
-- Closest points --------------------------------
--------------------------------------------------

forall Point q; Set s; Point p
where q := ClosestPoint( s, p ) {
   override q.x = closestPoint( s.icon, p.x )
   override q.icon.fillColor = colors.white
   override q.icon.strokeColor = colors.black
   override q.icon.strokeWidth = global.dotStroke
   override q.icon.ensureOnCanvas = false
}

forall Segment t; Set s; Point p
where t := ClosestSegment( s, p ) {
   override t.a = p.x
   override t.b = closestPoint( s.icon, p.x )
   override t.icon.strokeColor = colors.lightGray
   override t.icon.ensureOnCanvas = false
   override t.icon.strokeStyle = "dashed"
   override t.icon.strokeDasharray = "4,3"
}

--------------------------------------------------
-- Overlap constraints ---------------------------
--------------------------------------------------

forall Set X
where X has label {
   constraint X.labelDisjoint = ensure disjoint( X.icon, X.labelText, global.padding ) in labelStage
}

forall Length l
where l has label {
   delete l.labelDisjoint
}

forall Set X; Point p
where X has label {
   ensure disjoint( X.labelText, p.icon, global.padding ) in labelStage
}
forall Set X; Segment s
where X has label {
   ensure disjoint( X.labelText, s.icon, global.padding ) in labelStage
}
forall Set X; Length l
where X has label {
   ensure disjoint( X.labelText, l.icon, global.padding ) in labelStage
}

forall Set X; Set Y
where X has label; Y has label {
   ensure disjoint( X.labelText, Y.labelText, global.padding ) in labelStage
}

--------------------------------------------------
-- Layering --------------------------------------
--------------------------------------------------

forall Point p; Set X {
   layer p.icon above X.icon
}

forall Point p; Set X
where p has label {
   layer p.labelText above X.icon
}

-- AtomicClosestPoint.style ----------------------

-- forall Segment \`ab\` {
--    ensure norm( \`ab\`.a - \`ab\`.b ) == canvas.width in shapeStage
-- }
-- forall Segment \`s\` {
--    ensure norm( \`s\`.a - \`s\`.b ) == canvas.width * .5 in shapeStage
-- }
-- forall Length \`l\` {
--    ensure norm( \`l\`.a - \`l\`.b ) > canvas.width * .2 in shapeStage
--    ensure norm( \`l\`.a - \`l\`.b ) < canvas.width * .8 in shapeStage
-- }

-- ClosestPoint.style ----------------------------

-- forall Point xi; Point xj; Point yi; Point yj; Polyline M
-- where yi := ClosestPoint(M,xi); yj := ClosestPoint(M,xj) {
--    encourage 100/norm(xi.x-xj.x) == 0
-- }

-- FirstRay.style ----------------------------

-- forall Point p {
--    override p.x0 = -canvas.width/2 + 10
-- }
-- 
-- forall Point p; Polyline M {
-- 
--    scalar y0 = min( M.p0[1], min( M.p1[1], min( M.p2[1], min( M.p3[1], M.p4[1] ))))
--    scalar y1 = max( M.p0[1], max( M.p1[1], max( M.p2[1], max( M.p3[1], M.p4[1] ))))
-- 
--    scalar t = random(0,1)
--    override p.x1 = (1-t)*y0 + t*y1
-- }
-- 
-- forall Vector v {
--    override v.v0 = 1000
-- }

-- AtomicClosestSilhouette.style

-- resizeCanvas
-- {
--    override canvas.width = 150
-- }
-- 
-- forall Point \`a\` {
--    scalar w = canvas.width
--    scalar h = canvas.height
--    override \`a\`.x = (-w/2 + 10,0)
-- }
-- forall Point \`b\` {
--    scalar w = canvas.width
--    scalar h = canvas.height
--    override \`b\`.x = (0,h/6)
-- }
-- forall Point \`c\` {
--    scalar w = canvas.width
--    scalar h = canvas.height
--    override \`c\`.x = (w/2 - 10,-h/6)
-- }
-- forall Segment \`ab\` {
--    override \`ab\`.icon.strokeWidth = 1.5*global.lineWidth
-- }
-- forall Segment \`bc\` {
--    override \`bc\`.icon.strokeWidth = 1.5*global.lineWidth
-- }
-- 
-- -- Conditionally label diagram as "silhouette" or
-- -- "not silhouette" depending on whether \`b\` is a silhouette
-- -- point relative to \`x\`.
-- forall Triangle t1; Triangle t2 {
-- 
--    -- compute the signed area of each triangle
--    scalar A1 = cross2D( t1.b - t1.a, t1.c - t1.a )
--    scalar A2 = cross2D( t2.b - t2.a, t2.c - t2.a )
-- 
--    -- compute the sign of the product, which will
--    -- be positive if the triangles have the same
--    -- orientation and negative otherwise
--    scalar s = A1*A2
-- 
--    -- emulate the statement
--    --    alpha = s < 0 ? 0 : 1
--    -- using a steep sigmoid function to
--    -- approximate a step function
--    scalar alpha = 1/(1 + exp(-1000*s))
-- 
--    shape isSilhouette = Text {
--       string: "silhouette"
--       center: (0,-canvas.height/2 + 2*global.labelHeight) - 1.5*global.labelHeight*(0,alpha)
--       fontFamily: global.labelFamily
--       fontSize: global.labelSize
--       fillColor: rgba( alpha, alpha, alpha, 1-alpha )
--       opacity: 1 - alpha
--       ensureOnCanvas: false
--    }
-- 
--    shape notSilhouette = Text {
--       string: "not silhouette"
--       center: (0,-canvas.height/2 + 2*global.labelHeight) - 1.5*global.labelHeight*(0,1-alpha)
--       fontFamily: global.labelFamily
--       fontSize: global.labelSize
--       fillColor: rgba( 1-alpha, 1-alpha, 1-alpha, alpha )
--       opacity: alpha
--       ensureOnCanvas: false
--    }
-- }

-- AtomicClosestSilhouetteExample1.style
-- forall Point \`x\` {
--    override \`x\`.x = (0,canvas.height/2 - 2*global.labelHeight)
-- }

-- -- AtomicClosestSilhouetteExample2.style
-- forall Point \`x\` {
--    override \`x\`.x = (-canvas.width/4,-canvas.height/2 + 4*global.labelHeight)
-- }

-- -- AtomicClosestSilhouetteExample3.style
-- forall Point \`x\` {
--    override \`x\`.x = (canvas.width/2 - global.labelHeight, global.labelHeight*2)
-- }

-- subtended-angles.style

-- -- Shade signed angles blue if they're positive, and red
-- -- if they're negative
-- forall Angle t; Point a, b, c
-- where t := InteriorAngle(a,b,c); IsOriented(t)
-- {
--    -- computes a value v equal to zero if the triangle
--    -- is negatively-oriented, and one if the triangle is
--    -- positively-oriented
--    scalar u = sign( cross2D( b.x - a.x, c.x - a.x ))
--    scalar v = (u + 1)/2
-- 
--    shape subtended = Polygon {
--       points: ( b.x, a.x, c.x )
--       fillColor: rgba( 1-v, 0, v, .2 )
--       strokeColor: #0002
--       strokeWidth: .5
--       ensureOnCanvas: false
--    }
-- 
--    override t.icon.strokeColor = rgba( 1-v, 0, v, .4 )
-- }

-- AtomicSignedAngle.style

-- AtomicSignedAngle {
--    scalar segmentSize = toRadians(50)
--    override canvas.width = 200
-- }
-- 
-- forall Point \`x\` {
--    override \`x\`.x = (0,0)
-- }
-- 
-- forall Point \`a\`, \`b\` {
--    scalar theta = unitRandom() * 2*MathPI()
--    scalar dTheta = AtomicSignedAngle.segmentSize
--    scalar r = .9 * canvas.height/2
--    override \`a\`.x = r*(cos(theta-dTheta/2),sin(theta-dTheta/2))
--    override \`b\`.x = r*(cos(theta+dTheta/2),sin(theta+dTheta/2))
-- }
-- 
-- forall Point \`c\`, \`d\` {
--    scalar theta = unitRandom() * 2*MathPI()
--    scalar dTheta = 1.5*AtomicSignedAngle.segmentSize
--    scalar r = .9 * canvas.height/2
--    override \`c\`.x = r*(cos(theta+dTheta/2),sin(theta+dTheta/2))
--    override \`d\`.x = r*(cos(theta-dTheta/2),sin(theta-dTheta/2))
-- }

-- SignedAngle.style

-- SignedAngle {
--    override canvas.width = 200
-- }
-- 
-- -- put the points on the boundary of the letter "A"
-- forall Point \`a1\`, \`a2\`, \`a3\`, \`a4\`, \`a5\`, \`a6\`, \`a7\`, \`a8\`, \`b1\`, \`b2\`, \`b3\` {
-- 
--    scalar s = canvas.height - 10 -- scale
-- 
--    -- outer boundary
--    override \`a1\`.x = s * (-.169, -.287)
--    override \`a2\`.x = s * ( .173, -.287)
--    override \`a3\`.x = s * ( .249, -.500)
--    override \`a4\`.x = s * ( .500, -.500)
--    override \`a5\`.x = s * ( .119,  .500)
--    override \`a6\`.x = s * (-.123,  .500)
--    override \`a7\`.x = s * (-.500, -.500)
--    override \`a8\`.x = s * (-.242, -.500)
-- 
--    -- hole
--    override \`b1\`.x = s * ( 0.00,  .202)
--    override \`b2\`.x = s * ( .105, -.094)
--    override \`b3\`.x = s * (-.102, -.094)
-- }
-- 
-- forall Angle a {
--    override a.radius = 20 + 60*(match_id - 1)/(match_total - 1)
-- }

-- SignedAngleInside.style

-- forall Point \`x\` {
--    override \`x\`.x = (50,-30)
-- }

-- SignedAngleOutside.style

-- forall Point \`x\` {
--    override \`x\`.x = (90,50)
-- }

-- AtomicClosestRay.style

-- forall Point \`a\`, \`b\`, \`x\` {
--    scalar w = canvas.width
--    scalar h = canvas.height
--    scalar p = 20
--    override \`a\`.x = ( -w/2 + p, -h/2 + p )
--    override \`b\`.x = ( w/2 - p, h/2 - p )
--    override \`x\`.x = ( -w/4 + p, h/2 - p )
-- }

`,o=n("walk-on-spheres"),l=`-- Shade signed angles blue if they're positive, and red
-- if they're negative
forall Angle t; Point a, b, c
where t := InteriorAngle(a,b,c); IsOriented(t)
{
   -- computes a value v equal to zero if the triangle
   -- is negatively-oriented, and one if the triangle is
   -- positively-oriented
   scalar u = sign( cross2D( b.x - a.x, c.x - a.x ))
   scalar v = (u + 1)/2

   shape subtended = Polygon {
      points: ( b.x, a.x, c.x )
      fillColor: rgba( 1-v, 0, v, .2 )
      strokeColor: #0002
      strokeWidth: .5
      ensureOnCanvas: false
   }

   override t.icon.strokeColor = rgba( 1-v, 0, v, .4 )
}
`,r=n("walk-on-spheres"),i='SignedAngle {\n   override canvas.width = 200\n}\n\n-- put the points on the boundary of the letter "A"\nforall Point `a1`, `a2`, `a3`, `a4`, `a5`, `a6`, `a7`, `a8`, `b1`, `b2`, `b3` {\n\n   scalar s = canvas.height - 10 -- scale\n\n   -- outer boundary\n   override `a1`.x = s * (-.169, -.287)\n   override `a2`.x = s * ( .173, -.287)\n   override `a3`.x = s * ( .249, -.500)\n   override `a4`.x = s * ( .500, -.500)\n   override `a5`.x = s * ( .119,  .500)\n   override `a6`.x = s * (-.123,  .500)\n   override `a7`.x = s * (-.500, -.500)\n   override `a8`.x = s * (-.242, -.500)\n\n   -- hole\n   override `b1`.x = s * ( 0.00,  .202)\n   override `b2`.x = s * ( .105, -.094)\n   override `b3`.x = s * (-.102, -.094)\n}\n\nforall Angle a {\n   override a.radius = 20 + 60*(match_id - 1)/(match_total - 1)\n}\n\n',s=n("walk-on-spheres"),c="forall Point `x` {\n   override `x`.x = (90,50)\n}\n",h=`type Set
type Point <: Set
type Segment <: Set
type Ray <: Set
type Vector <: Set
type Triangle <: Set
type Circle <: Set
type Disk <: Set
type Chord <: Set
type Length <: Set
type Polyline <: Set
type Angle <: Set

constructor Segment( Point p, Point q )
constructor LineSegment( Point p, Point q ) -> Segment
constructor ClosestPoint( Set s, Point p ) -> Point
constructor ClosestSegment( Set s, Point p ) -> Segment
constructor RayFrom( Point p, Vector v ) -> Ray
constructor RayIntersection( Ray r, Set s ) -> Point
constructor RaySegmentIntersection( Ray r, Segment s ) -> Point
constructor Triangle( Point p1, Point p2, Point p3 )
constructor LengthBetween( Point x, Point y ) -> Length
constructor LengthOf( Segment s ) -> Length
constructor Normal( Set s ) -> Vector
constructor InteriorAngle( Point a, Point b, Point c ) -> Angle

predicate RootedAt( Point p, Vector v )
predicate InTri( Point p, Triangle t )
predicate OnCircle( Point p, Circle c )
predicate InDisk( Point p, Disk d )
predicate OfDisk( Chord c, Disk d )
predicate IsDashed( Set s )
predicate IsOriented( Set s )

`,p={substance:e,style:[{contents:t,resolver:a},{contents:l,resolver:o},{contents:i,resolver:r},{contents:c,resolver:s}],domain:h,variation:"BollywoodKangaroo140",excludeWarnings:[]};export{p as default};
//# sourceMappingURL=SignedAngleOutside.trio-62cdb90c.js.map
