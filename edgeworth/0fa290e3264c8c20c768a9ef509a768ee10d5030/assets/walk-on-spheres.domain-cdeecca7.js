import{a as n}from"./index-82fd0888.js";const a=n("walk-on-spheres"),l=`layout = [ walkStage, nestStage, labelStage, legendStage ]

-- diagram dimensions (in px; multiply by 96/72 to convert to pt)
canvas {
   width = 200 -- ==150pt
   height = 200 -- ==150pt
}

-- some colors re-used throughout
Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color darkGray = #b3b3b3
   color lightGray = #e5e5e5
   color deepGreen = #93c697
}

-- global shapes and constants
Global {
   scalar toPt = 96/72 -- constant for converting sizes from pt to px

   -- default appearance for label text
   string labelFont = "Linux Libertine"
   string labelSize = "13.333px" -- equivalent to 10pt (multiply by 96/72)
   scalar labelHeight = toPt * 10
   color labelColor = Colors.black
   color subdomainLabelColor = Colors.deepGreen
   color labelStrokeColor = Colors.white
   scalar labelStrokeWidth = toPt * 1.5

   -- appearance of dots/points
   scalar dotSize = toPt * 1.5
   scalar openDotWidth = toPt * 0.5
   scalar dotLabelDistance = 9

   -- style of domains/subdomains
   color domainColor = Colors.lightGray
   color ballColor = rgba(154/255.,204/255.,160/255.,1)
   color sphereColor = rgba(27/255.,138/255.,31/255.,1)
   scalar subdomainOpacity = .35
   scalar sphereStrokeWidth = toPt * .5
   scalar domainStrokeWidth = toPt * 1
   scalar dirichletStrokeWidth = domainStrokeWidth
   scalar neumannStrokeWidth = .65*domainStrokeWidth
   string neumannDash = "2.66,2.66" -- "2,2"
   scalar labelPadding = 6

   -- style of lines along a walk
   -- (set color to none() to disable)
   color walkColor = #0066004c
   scalar walkStrokeWidth = toPt * 1

}

forall Domain D {
   -- draw the domain as a polygon with some random variation, by
   -- perturbing the vertices of a hexagon by some bounded amount
   -- Note: if the domain shape is changed to something else (like
   -- an ellipse or a rectangle), things _should_ still work as
   -- expected, since later methods just make a call to \`signedDistance\`
   -- to determine ball radii.  However, signed distance may not yet
   -- be supported for all shapes (like arbitrary Bézier curves).
   scalar maxPerturbation = 15
   scalar w = .8*canvas.width/2
   scalar h = .8*canvas.height/2
   vec2 D.p0 = ( w, -h) + maxPerturbation*diskRandom()
   vec2 D.p1 = ( 0, -h) + maxPerturbation*diskRandom()
   vec2 D.p2 = (-w, -h) + maxPerturbation*diskRandom()
   vec2 D.p3 = (-w,  h) + maxPerturbation*diskRandom()
   vec2 D.p4 = ( 0,  h) + maxPerturbation*diskRandom()
   vec2 D.p5 = ( w,  h) + maxPerturbation*diskRandom()
   shape D.geometry = Polygon {
      points: [ D.p0, D.p1, D.p2, D.p3, D.p4, D.p5 ]
      fillColor: Colors.lightGray
      strokeColor: Colors.darkGray
      strokeWidth: Global.domainStrokeWidth
      ensureOnCanvas: false
   }
}

-- label the domain if it was assigned a label in the Substance program
forall Domain D
where D has label {

   scalar x = ? in labelStage
   scalar y = ? in labelStage
   shape D.labelText = Equation {
      string: D.label
      center: (x,y)
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fillColor: Colors.black
      ensureOnCanvas: false
   }

   ensure lessThan( signedDistance( D.geometry, D.labelText.center ), -9. ) in labelStage
   layer D.labelText above D.geometry -- make sure the label doesn't get covered by the domain shape
}

forall Domain D
where hasMixedBoundaryConditions( D ) {
   override D.geometry.strokeColor = none()

   shape D.poly0 = Polyline {
      points: ( D.p5, D.p0 )
      strokeColor: Colors.black
      strokeWidth: Global.dirichletStrokeWidth
      strokeLinecap: "round"
      ensureOnCanvas: false
   }
   shape D.poly1 = Polyline {
      points: ( D.p0, D.p1, D.p2 )
      strokeColor: Colors.black
      strokeWidth: Global.neumannStrokeWidth
      ensureOnCanvas: false
      style: "dashed"
      strokeDasharray: Global.neumannDash
   }
   shape D.poly2 = Polyline {
      points: ( D.p2, D.p3 )
      strokeColor: Colors.black
      strokeWidth: Global.dirichletStrokeWidth
      strokeLinecap: "round"
      ensureOnCanvas: false
   }
   shape D.poly3 = Polyline {
      points: ( D.p3, D.p4, D.p5 )
      strokeColor: Colors.black
      strokeWidth: Global.neumannStrokeWidth
      ensureOnCanvas: false
      style: "dashed"
      strokeDasharray: Global.neumannDash
   }

   shape D.dirichletBoundary = Group {
      shapes: ( D.poly0, D.poly2 )
      ensureOnCanvas: false
   }
   shape D.neumannBoundary = Group {
      shapes: ( D.poly1, D.poly3 )
      ensureOnCanvas: false
   }

   layer D.dirichletBoundary above D.geometry
   layer D.neumannBoundary above D.dirichletBoundary
}

-- draw each point as a small dot
forall Point p {
   scalar x = ? in walkStage
   scalar y = ? in walkStage
   vec2 p.location = (x,y)

   -- The \`dot\` shape represents the sample point associated
   -- with the ball.  Ordinarily this point will be at the
   -- center of the ball, but in general it could be at a
   -- different location (e.g., for an off-center walk).
   shape p.dot = Circle {
      center: p.location
      r: Global.dotSize
      fillColor: Colors.black
      ensureOnCanvas: false
   }
}

-- draw each Ball as the largest ball tangent to the domain boundary
forall Ball B; Domain D {

   -- Make sure the ball is inside the problem domain by
   -- setting its radius to (minus) the signed distance to
   -- the domain boundary.  This value is negated since, by
   -- convention, points inside the domain have negative
   -- signed distance.
   scalar x = ? in walkStage
   scalar y = ? in walkStage
   vec2 B.center = (x,y)
   scalar B.radius = -signedDistance( D.geometry, B.center ) - Global.sphereStrokeWidth/2 - Global.domainStrokeWidth/2.

   -- The \`ball\` shape is a flat-shaded semi-transparent
   -- disk that determines the color of the ball.
   -- The ball radius equals the distance from the
   -- ball center to the closest point on the domain boundary
   -- (just as in the WoS algorithm!).  We also subtract half
   -- the stroke widths so that visually the ball makes perfect
   -- tangential contact with the domain boundary.
   -- Note that we don't need to
   -- explicitly enforce that the ball remains on the canvas,
   -- since the domain shape already has an onCanvas constraint,
   -- and the balls are always contained in the domain shape, by
   -- construction.
   shape B.ball = Circle {
      center: B.center
      r: B.radius
      fillColor: Global.ballColor
      strokeColor: Global.sphereColor
      strokeWidth: Global.sphereStrokeWidth
      opacity: Global.subdomainOpacity
      ensureOnCanvas: false
   }

   shape B.shading = Image {
      href: "ball-shading.svg"
      center: B.ball.center
      width: 2*B.ball.r
      height: 2*B.ball.r
      ensureOnCanvas: false
   }

   -- Make sure all components of the ball get drawn in the
   -- proper order, and are not covered up by the domain shape.
   layer B.ball above B.shading
   layer B.shading above D.geometry

   -- This constant determines how much balls shrink from one
   -- step of the walk to the next.  We define it as a named
   -- constant so that specialized types of balls (say, those
   -- coming from a nested walk) can use a different factor.
   scalar B.shrinkFactor = .55
}

-- If a ball is constructed around a point, center it around that point
forall Point p; Ball B
where B := ballAround( p ) {
   override B.center = p.location
   layer p.dot above B.ball
}

-- For a step of an off-centered walk, we want to clearly
-- indicate that the sample point may not be at the ball center.
forall Ball B; Point p
where B := ballAround(p); isOffCenter(B) {

   -- We can no longer just copy the ball center from the point center,
   -- but will instead let the layout engine determine its location.
   scalar x = ? in walkStage
   scalar y = ? in walkStage
   override B.center = (x,y)

   -- Still keep the center within the middle 25–75% of the ball, since
   -- the dot may be hard to see if it gets too close to the boundary
   -- (especially for very small balls).
   scalar r = B.radius * random(.25,.75)
   ensure norm(B.center - p.location) == r in walkStage
}

-- If a point has a label, draw a label string near the dot.
forall Point p
where p has label {
   shape p.labelText = Equation {
      string: p.label
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fontStyle: "italic"
      fillColor: Global.labelColor
      --strokeColor: Global.labelStrokeColor
      --strokeWidth: Global.labelStrokeWidth
      --strokeLinejoin: "round"
      --paintOrder: "stroke"
      ensureOnCanvas: false
   }

   -- We want the label to be near the dot, but don't want it
   -- to overlap the dot (which makes it hard to read).
   scalar theta = ? in labelStage
   p.labelText.center = p.dot.center + Global.dotLabelDistance*( cos(theta), sin(theta) )

   -- If we fail to avoid overlap, we should
   -- at least draw the label on top of the dot
   layer p.labelText above p.dot
}

-- Make sure a point's label doesn't get covered by a ball
-- containing it, by layering it above the topmost ball shape.
forall Point p; Ball B
where B := ballAround( p ); p has label {
   layer p.labelText above B.ball
}

-- If a point p is sampled from the boundary of a ball B,
-- make sure it sits on the corresponding sphere ∂B.
forall Ball B; Point p
where p := sampleBoundary( B ) {
   constraint p.ballConstraint = ensure equal( norm(p.location - B.center), B.radius ) in walkStage

   -- Make sure the dot is not covered by the sphere.
   layer p.dot above B.ball
}

-- If a point p is sampled from the interior of a ball B,
-- make sure it sits somewhere between 20% and 80% away
-- from the center, so that it's clearly visible and doesn't
-- run into the center point or the boundary.
forall Ball B; Point p
where p := sampleInterior( B ) {
   scalar r = B.radius * random(.2,.8)
   constraint p.ballConstraint = ensure norm( p.location - B.center ) == r in walkStage

   -- Make sure the dot is not covered by the ball.
   layer p.dot above B.ball
}


-- Adjust the drawing for consecutive steps x0, x1 along a walk on spheres,
-- where B0, B1 are the associated spheres.
forall Ball B0; Ball B1; Point x0; Point x1
where x1 := sampleBoundary( B0 ); B0 := ballAround( x0 ); B1 := ballAround( x1 ) {
   -- draw later steps of a walk as smaller balls (which is often, but
   -- not always, the behavior of the WoS algorithm)
   ensure equal( B1.radius, B1.shrinkFactor * B0.radius ) in walkStage

   -- It's typically easier to see labels, etc., if later, smaller
   -- balls are drawn on top of earlier, bigger ones.
   layer B1.ball above B0.ball
}

-- Draw a line between the consecutive points on a walk on spheres
forall Point x0; Point x1; Ball B0; Domain D
where x1 := sampleBoundary( B0 ); B0 := ballAround( x0 ) {

   shape x1.walkLine = Line {
      start: x0.location
      end: x1.location
      fill: "none"
      strokeColor: Global.walkColor
      strokeWidth: Global.walkStrokeWidth
      ensureOnCanvas: false
   }
   layer x1.walkLine above D.geometry

   -- Use layering to help clarify the direction of the walk.
   layer x1.walkLine above B0.ball
}

forall Point x0; Point x1; Ball B0; Ball B1; Domain D
where x1 := sampleBoundary( B0 ); B0 := ballAround( x0 ); B1 := ballAround( x1 ) {
   -- Use layering to help clarify the direction of the walk.
   layer x1.walkLine below B1.ball
}

-- Draw sample points that are used to estimate the source term
-- as just dots contained in the ball of interest.  These sample
-- points are different from steps of a walk, since we never
-- need to consider a ball around these points (and hence do not
-- want to draw one).
forall Point p; Ball B
where p := sampleInterior( B ); isSourceSample( p ) {
   scalar x = ? in walkStage
   scalar y = ? in walkStage
   -- Draw source samples in a different style
   override p.location = (x,y)
   override p.dot.r = 2
   override p.dot.fillColor = Colors.white
   override p.dot.strokeColor = Colors.black
   override p.dot.strokeWidth = 1
}

-- Nested walks --------------------------------------------

-- Draw nested points in a different style
forall NestedPoint p {
   scalar x = ? in nestStage
   scalar y = ? in nestStage
   vec2 p.location = (x,y)

   shape p.dot = Circle {
      center: p.location
      r: 2.5
      fillColor: Colors.white
      ensureOnCanvas: false
   }
}

-- Draw nested balls in a different style
forall Ball B; NestedPoint p
where B := nestedBallAround(p) {

   override B.center = p.location

   -- Change to a color palette that is more white & opaque.
   override B.ball.fillColor = none()
   override B.ball.strokeColor = none()
   override B.shading = Circle{
      center: B.center
      r: B.radius
      fillColor: rgba(1,1,1,.2)
      strokeColor: rgba(1,1,1,.5)
      strokeWidth: 1
      ensureOnCanvas: false
   }

   override B.shrinkFactor = .7
}

forall NestedPoint p; Point p0; Ball B; Ball B0; Domain D
where p := startWalkFrom(B0); B := nestedBallAround(p); B0 := ballAround(p0) {
   vec2 y = closestPoint( D.geometry, B0.center )
   scalar u = .75
   override p.location = (1-u)*B0.center + u*y
   override B.center = p.location
   override B.radius = norm( y - p.location )
}

forall NestedPoint p; Ball B
where p := sampleBoundaryNested(B) {
   ensure norm(p.location - B.center) == B.radius in nestStage
}

forall Ball B0; Ball B1; NestedPoint x0; NestedPoint x1
where x1 := sampleBoundaryNested( B0 ); B0 := nestedBallAround( x0 ); B1 := nestedBallAround( x1 ) {
   ensure equal( B1.radius, B1.shrinkFactor * B0.radius ) in nestStage
}

forall Ball B; NestedPoint p; Ball B0; Point p0
where B := nestedBallAround(p); B0 := ballAround(p0) {
   layer B.ball above B0.ball
}

-- Draw a line between the consecutive points on a nested walk
forall NestedPoint x0; NestedPoint x1; Ball B0; Domain D
where x1 := sampleBoundaryNested( B0 ); B0 := nestedBallAround( x0 ) {

   shape x1.walkLine = Line {
      start: x0.location
      end: x1.location
      fill: "none"
      strokeColor: Colors.white
      strokeWidth: Global.walkStrokeWidth
      ensureOnCanvas: false
   }
   layer x1.walkLine above D.geometry

   -- Use layering to help clarify the direction of the walk.
   layer x1.walkLine above B0.ball
}

-- draw nested walks above regular walks
forall NestedPoint p; Ball B {
   layer p.dot above B.ball
}

-- Walk on Stars -------------------------------------------

-- If the domain has mixed boundary conditions (and
-- hence we're using walk on stars), we want to keep
-- track of the normal at point p, which we'll set
-- to zero if p is not on the domain boundary.
forall Domain D; Point p
where hasMixedBoundaryConditions( D ) {

   override p.location = triangleRandom( D.p0, D.p2, D.p3 )

   vec2 p.normal = (0,0)

   -- rays from p will be sampled from baseAngle + [-angleRange,angleRange]
   scalar p.baseAngle = 0
   scalar p.angleRange = MathPI()

   layer p.dot above D.neumannBoundary

   -- -- draw an arrow for the normal at p (transparent if the normal is zero)
   -- scalar eps = 1e-7 -- prevents NaNs for zero-length normals
   -- shape p.normalArrow = Line {
   --    start: p.location
   --    end: p.location + 20*p.normal + eps*(1,1)
   --    fill: "none"
   --    strokeWidth: 1
   --    strokeColor: rgba(1,1,1,.5*norm(p.normal))
   --    ensureOnCanvas: false
   --    endArrowhead: "straight"
   --    endArrowheadSize: .5
   -- }
   -- layer p.normalArrow below p.dot
}

forall Star St; Point p; Domain D
where St := starAround(p) {
   vec2 yDirichlet = closestPoint( D.dirichletBoundary, p.location )
   scalar St.dDirichlet = norm( yDirichlet - p.location )
   scalar St.dNeumann = closestSilhouetteDistance( D.neumannBoundary, p.location )
   scalar rMin = 1
   scalar R = max( rMin, min( St.dDirichlet, St.dNeumann ))

   -- color the boundary of the ball according to whether it's defined
   -- by the closest Dirichlet point or closest Neumann silhouette point
   scalar upsilon = 1e-7 -- to fudge the case where dDirichlet==dNeumann
   scalar t = (St.dDirichlet - R)/(St.dDirichlet - St.dNeumann + upsilon)
   -- vec3 c0 = (.4,.6,.4)
   -- vec3 c1 = (.4,.4,.7)
   vec3 f0 = (154,204,160)/255.
   vec3 f1 = (255,255,255)/255.
   vec3 s0 = (27,138,31)/255.
   vec3 s1 = (27,138,31)/255.
   vec3 s = (1-t)*s0 + t*s1
   vec3 f = (1-t)*f0 + t*f1

   shape St.ball = Circle {
      center: p.location
      r: R
      fillColor: rgba( f[0], f[1], f[2], 1 )
      strokeColor: rgba( s[0], s[1], s[2], 1 )
      strokeWidth: Global.sphereStrokeWidth
      ensureOnCanvas: false
      opacity: .35
   }

   shape St.shading = Image {
      href: "ball-shading.svg"
      center: St.ball.center
      width: 2*St.ball.r
      height: 2*St.ball.r
      ensureOnCanvas: false
   }

   layer St.shading above D.geometry
   layer St.ball above St.shading

   shape St.domainGeometry = Polygon {
      points: D.geometry.points
      fillColor: none()
      ensureOnCanvas: false
   }
   shape St.G = Group {
      shapes: [St.ball,St.shading]
      clipPath: clip(St.domainGeometry)
      ensureOnCanvas: false
   }
   layer St.G above D.geometry
   layer St.G below D.neumannBoundary
}

forall Point p; Star St {
   layer p.dot above St.ball
}

forall Point p0; Point p1; Star St; Domain D
where p1 := sampleBoundary(St); St := starAround(p0) {

   -- sample a random ray from p0
   scalar theta = p0.baseAngle + p0.angleRange * random(-1,1)
   scalar eps = 1e-1 -- offset ray origin to avoid intersecting at x again
   vec2 v = ( cos(theta), sin(theta) )
   vec2 x = p0.location + eps*v

   -- intersect the star boundary
   scalar tBoundary = rayIntersectDistance( D.neumannBoundary, x, v )
   scalar tBall = St.ball.r
   scalar t = min( tBoundary, tBall )
   override p1.location = x + t*v 

   -- set the normal at the intersection point, or set it to zero if
   -- the ray hits the ball boundary first
   vec2 n = rayIntersectNormal( D.neumannBoundary, x, v )
   scalar k = 100. -- steepness parameter for indicator function
   scalar s = 1. - exp(-k*sqr(t - tBall)) -- equals zero near tBall and 1 otherwise
   override p1.normal = s * n -- scales n to zero if t == tBall
   override p1.angleRange = (1-s/2)*MathPI() -- s=0 -> MathPI(); s=1 -> MathPI()/2
   scalar delta = 1e-7 -- to get valid angle if normal is zero
   override p1.baseAngle = atan2( p1.normal[0], p1.normal[1] + delta )

   -- draw the point in white if it's on the Neumann boundary
   override p1.dot.fillColor = rgba(s,s,s,1)
   override p1.dot.strokeWidth = .5*s
   override p1.dot.strokeColor = rgba(0,0,0,1)

   -- draw the walk line from p0 to p1
   shape p1.walkLine = Line {
      start: p0.location
      end: p1.location -- - Global.dotSize*v
      fill: "none"
      strokeColor: Global.walkColor
      strokeWidth: Global.walkStrokeWidth
      --endArrowhead: "straight"
      --endArrowheadSize: .5
      ensureOnCanvas: false
      strokeLinecap: "round"
   }

   layer p0.dot above p1.walkLine
   layer D.dirichletBoundary above p1.walkLine
}

forall Point p0; Point p1; Star St; Domain D; Star St2
where p1 := sampleBoundary(St); St := starAround(p0) {
   layer p1.walkLine above St2.ball
}

-- connect the walk to the closest point on the
-- Dirichlet boundary, and draw the boundary point
-- in a different style
forall Point q; Point p; Domain D
where q := closestBoundaryPoint( D, p ); hasMixedBoundaryConditions( D )
{
   override q.dot.center = closestPoint( D.dirichletBoundary, p.location )
   override q.dot.fillColor = Colors.white
   override q.dot.strokeColor = Colors.black
   override q.dot.strokeWidth = Global.openDotWidth

   shape q.walkLine = Line {
      start: p.dot.center
      end: q.dot.center
      fill: "none"
      strokeColor: Global.walkColor
      strokeWidth: Global.walkStrokeWidth
      ensureOnCanvas: false
   }

   layer q.walkLine above D.geometry
   layer q.dot above q.walkLine
}

-- Rays ---------------------------------------

forall Ray r; Domain D {
   vec2 r.x = (0,0) -- origin
   scalar r.L = 30 -- length
   scalar r.theta = 0 -- angle
   vec2 r.v = ( cos(r.theta), sin(r.theta) ) -- direction

   vec2 s = 2.0 -- size
   vec2 a = r.x -- start
   vec2 b = r.x + r.L*r.v -- end

   scalar w = .75
   vec2 u = unit(b-a)
   vec2 n = rot90(u)
   shape r.taperedStroke = Polygon {
      points: ( a, b + w*n - 3.5*u, b - w*n - 3.5*u )
      fillColor: #ff4411
      ensureOnCanvas: false
   }

   shape r.arrowHead = Line {
      start: b - 6.5*u
      end: b
      strokeColor: #ff4411
      strokeWidth: Global.walkStrokeWidth
      endArrowhead: "straight"
      endArrowheadSize: .5
      ensureOnCanvas: false
   }

   shape r.icon = Group {
      shapes: [ r.taperedStroke, r.arrowHead ]
      ensureOnCanvas: false
      opacity: .75
   }

   layer r.icon above D.geometry
}


forall Ray r; Point p
where p := headOf( r )
{
   override p.location = r.arrowHead.end
   override p.dot.fillColor = Colors.white
   override p.dot.strokeColor = Colors.black
   override p.dot.strokeWidth = Global.openDotWidth
   layer p.dot above r.icon
}


-- Labeling ------------------------------------------------

forall Subdomain S; Domain D
where S has label {
   shape S.labelText = Equation {
      string: S.label
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fontStyle: "italic"
      fillColor: Global.subdomainLabelColor
      ensureOnCanvas: false
   }

   -- encourage near( S.labelText, S.ball ) in labelStage
   encourage norm( S.labelText.center - S.ball.center ) == S.ball.r/2 in labelStage
   encourage lessThan( signedDistance( D.geometry, S.labelText.center ), -2. ) in labelStage
}

-- Make sure the domain's label isn't covered up by a ball.
forall Ball B; Domain D
where D has label {
   scalar padding = 10.
   ensure disjoint( D.labelText, B.ball, padding ) in labelStage
}

-- Put all labels above all balls
forall Point p; Ball B
where p has label {
   layer p.labelText above B.ball
}

-- Put all labels above all dots and
-- encourage labels not to overlap
forall Point p; Point q
where p has label; q has label {
   layer p.labelText above q.dot
   layer q.labelText above p.dot

   vec2 px = p.location
   vec2 qx = q.location
   vec2 pc = p.labelText.center
   vec2 qc = q.labelText.center
   encourage 10000./norm(pc - qc) == 0. in labelStage
}

forall Point p; Point q
where p has label {
   layer p.labelText above q.dot
}

forall Point p; Point q
where p has label; isSourceSample(q) {
   vec2 pc = p.labelText.center
   vec2 qx = q.location
   encourage 1000./norm(pc - qx) == 0. in labelStage
}

forall Point p; Domain D
where p has label; hasMixedBoundaryConditions(D) {
   layer p.labelText above D.dirichletBoundary
}

-- Make sure domain label doesn't overlap stars
forall Subdomain S; Domain D
where D has label {
   constraint S.avoidDomainLabel = ensure disjoint( D.labelText, S.ball ) in labelStage
}

-- Make sure subdomain labels don't overlap points or their labels
forall Subdomain S; Point p
where S has label {
   ensure disjoint( S.labelText, p.dot, Global.labelPadding ) in labelStage
}
forall Subdomain S; Point p
where S has label; p has label {
   ensure disjoint( S.labelText, p.labelText, Global.labelPadding ) in labelStage
}


-- Legend ---------------------------------------

forall Domain D
where hasLegend(D) {
   shape D.legendBox = Rectangle {
      center: ( ? in legendStage, ? in legendStage )
      width: ? in legendStage
      height: ? in legendStage
      fillColor: #fff
      strokeColor: #000
      strokeWidth: 1
   }
   encourage D.legendBox.width == 0 in legendStage
   encourage D.legendBox.height == 0 in legendStage
}

forall Domain D
where hasLegend(D); hasMixedBoundaryConditions(D) {

   scalar itemSpacing = 1.5 * Global.labelHeight
   scalar segmentWidth = 1 * itemSpacing
   scalar textSpacing = 2.5*Global.labelHeight
   scalar padding = 8

   vec2 x0 = ( ? in legendStage, ? in legendStage )
   encourage norm(x0-(canvas.width,-canvas.height)) == 0 in legendStage

   shape dirichletLine = Line {
      start: x0
      end: x0 + (segmentWidth,0)
      fill: "none"
      strokeWidth: Global.dirichletStrokeWidth
      strokeColor: #000
      style: "dashed"
      ensureOnCanvas: false
   }

   shape neumannLine = Line {
      start: x0 + (0,-itemSpacing)
      end: x0 + (segmentWidth,-itemSpacing)
      fill: "none"
      strokeWidth: Global.neumannStrokeWidth
      strokeColor: #000
      style: "dashed"
      strokeDasharray: "2,2"
      ensureOnCanvas: false
   }

   shape dirichletLegend = Text {
      center: dirichletLine.end + (textSpacing,0)
      string: "Dirichlet"
      fontFamily: Global.labelFont
      fontSize: Global.labelSize
      fillColor: Global.labelColor
      ensureOnCanvas: false
   }

   shape neumannLegend = Text {
      center: neumannLine.end + (textSpacing,0)
      string: "Neumann"
      fontFamily: Global.labelFont
      fontSize: Global.labelSize
      fillColor: Global.labelColor
      ensureOnCanvas: false
   }

   ensure contains( D.legendBox, dirichletLegend, padding ) in legendStage
   ensure contains( D.legendBox, dirichletLine, padding ) in legendStage
   ensure contains( D.legendBox, neumannLegend, padding ) in legendStage
   ensure contains( D.legendBox, neumannLine, padding ) in legendStage

   layer D.legendBox below dirichletLegend
   layer D.legendBox below dirichletLine 
   layer D.legendBox below neumannLegend
   layer D.legendBox below neumannLine
}


-- WalkOnStars.style

-- WalkOnStars {
--    -- Make the image wider to accommodate a legend
--    override canvas.width = 333.33
-- }
-- 
-- -- Offset the domain so that it's not covered up
-- -- too much by the legend
-- forall Domain D
-- where hasLegend(D)
-- {
--    override D.offset = (-50,0)
-- }
-- 
-- -- Place the starting point
-- forall Point \`x0\` {
--    override \`x0\`.location = (-85,30)
-- }

-- walk-on-spheres-no-subdomains.style -----------

-- -- hide subdomains
-- forall Ball B; Point p
-- where B := ballAround(p) {
--    override B.ball.visibility = "hidden"
-- }
-- forall Star St; Point p
-- where St := starAround(p) {
--    override St.ball.visibility = "hidden"
-- }
-- 
-- -- hide points sampled from subdomains
-- forall Point p; Ball B
-- where p := sampleBoundary( B ) {
--    override p.dot.visibility = "hidden"
-- }
-- forall Point p; Star St
-- where p := sampleBoundary( St ) {
--    override p.dot.visibility = "hidden"
-- }
-- 
-- -- make trajectories more transparent
-- noSubdomains {
--    override Global.walkColor = #1b1f8a30
-- }
-- 
-- -- don't worry about domain label avoiding subdomains, since
-- -- there will be many hidden subdomains; instead, make sure
-- -- the label avoids the walk lines
-- forall Subdomain S; Domain D
-- where D has label {
--    delete S.avoidDomainLabel
-- }
-- forall Point p; Subdomain S; Domain D
-- where p := sampleBoundary( S ) {
--    ensure disjoint( D.labelText, p.walkLine, Global.labelPadding ) in labelStage
-- }
-- forall NestedPoint p; Subdomain S; Domain D
-- where p := sampleBoundaryNested( S ) {
--    ensure disjoint( D.labelText, p.walkLine, Global.labelPadding ) in labelStage
-- }
-- forall Point q; Point p; Domain D
-- where q := closestBoundaryPoint( D, p ) {
--    ensure disjoint( D.labelText, q.walkLine, Global.labelPadding ) in labelStage
-- }

-- StarShapedRadii.style ------------------------

-- -- Widgets to draw the smallest ball touching the Dirichlet boundary,
-- -- and the smallest ball toughing the Neumann silhouette.
-- forall Star St; Point p; Domain D
-- where St := starAround(p) {
-- 
--    vec2 c = p.location
--    vec2 yDirichlet = closestPoint( D.dirichletBoundary, c )
--    vec2 yNeumann = closestSilhouettePoint( D.neumannBoundary, c )
-- 
--    shape St.dirichletBall = Circle {
--       center: c
--       r: St.dDirichlet
--       fillColor: none()
--       strokeColor: #ff6666
--       strokeWidth: 1.5 * Global.sphereStrokeWidth
--       ensureOnCanvas: false
--    }
-- 
--    scalar thetaD = 0
--    vec2 x0D = c
--    vec2 x1D = c + St.dDirichlet * (cos(thetaD),sin(thetaD))
--    vec2 mD = (x0D + x1D)/2
--    vec2 nD = rot90(unit(x1D - x0D))
--    shape St.dirichletLine = Line {
--       start: x0D
--       end: x1D
--       strokeColor: St.dirichletBall.strokeColor
--       strokeWidth: St.dirichletBall.strokeWidth
--       fill: "none"
--       ensureOnCanvas: false
--    }
--    shape St.dirichletLabel = Equation {
--       center: mD + nD * Global.labelHeight * .75
--       string: "d_{\\text{Dirichlet}}"
--       texContourColor: "white"
--       fillColor: St.dirichletBall.strokeColor
--       ensureOnCanvas: false
--       fontFamily: Global.labelFont
--       fontSize: Global.labelSize
--       -- rotation: toDegrees(-thetaD)
--    }
--    shape St.dirichletPoint = Circle {
--       center: yDirichlet
--       r: Global.dotSize
--       fillColor : Colors.white
--       strokeColor : St.dirichletBall.strokeColor
--       strokeWidth : Global.openDotWidth
--       ensureOnCanvas: false
--    }
--    shape St.closestDirichlet = Group {
--       shapes: [ St.dirichletBall, St.dirichletLine, St.dirichletPoint ]
--       ensureOnCanvas: false
--    }
--    layer p.dot above St.closestDirichlet
--    layer St.closestDirichlet above St.ball
--    layer St.closestDirichlet above D.neumannBoundary
--    layer St.dirichletPoint above St.dirichletBall
-- 
--    shape St.neumannBall = Circle {
--       center: c
--       r: St.dNeumann
--       fillColor: none()
--       strokeColor: #6666ff
--       strokeWidth: 1.5 * Global.sphereStrokeWidth
--       ensureOnCanvas: false
--    }
--    scalar thetaN = MathPI()
--    vec2 x0N = c
--    vec2 x1N = c + St.dNeumann * (cos(thetaN),sin(thetaN))
--    vec2 mN = (x0N + x1N)/2
--    vec2 nN = rot90(unit(x1N - x0N))
--    shape St.neumannLine = Line {
--       start: x0N
--       end: x1N
--       strokeColor: St.neumannBall.strokeColor
--       strokeWidth: St.neumannBall.strokeWidth
--       fill: "none"
--       ensureOnCanvas: false
--    }
--    shape St.neumannLabel = Equation {
--       center: mN - nN * Global.labelHeight * .75
--       string: "d_{\\text{silhouette}}"
--       texContourColor: "white"
--       fillColor: St.neumannBall.strokeColor
--       ensureOnCanvas: false
--       fontFamily: Global.labelFont
--       fontSize: Global.labelSize
--       -- rotation: toDegrees(-thetaN)
--    }
--    shape St.neumannPoint = Circle {
--       center: yNeumann
--       r: Global.dotSize
--       fillColor : Colors.white
--       strokeColor : St.neumannBall.strokeColor
--       strokeWidth : Global.openDotWidth
--       ensureOnCanvas: false
--    }
--    shape St.closestSilhouette = Group {
--       shapes: [ St.neumannBall, St.neumannLine, St.neumannPoint ]
--       ensureOnCanvas: false
--    }
--    layer p.dot above St.closestSilhouette
--    layer St.neumannPoint above St.neumannBall
--    layer St.closestSilhouette above St.ball
--    layer St.closestSilhouette above D.neumannBoundary
-- 
--    layer St.neumannLabel above St.closestSilhouette
--    layer St.neumannLabel above St.closestDirichlet
--    layer St.dirichletLabel above St.closestSilhouette
--    layer St.dirichletLabel above St.closestDirichlet
-- }
-- 
-- forall Star St; Point p; Domain D
-- where p has label; St := starAround(p) {
--    ensure disjoint( p.labelText, St.neumannLabel ) in labelStage
--    ensure disjoint( p.labelText, St.dirichletLabel ) in labelStage
-- }
-- 
-- forall Star St; Point p; Domain D
-- where D has label; St := starAround(p) {
--    ensure disjoint( D.labelText, St.neumannBall ) in labelStage
--    ensure disjoint( D.labelText, St.dirichletBall ) in labelStage
-- }
-- 
-- forall Star St; Point p; Domain D
-- where St has label; St := starAround(p) {
--    ensure disjoint( St.labelText, St.neumannLabel ) in labelStage
--    ensure disjoint( St.labelText, St.dirichletLabel ) in labelStage
-- }

-- StarShapedRegion.style ------------------------

-- -- draw uniformly-spaced rays to the boundary of the star-shaped region
-- forall Ray r; Point p; Star St; Domain D
-- where r := toBoundary(p,St); St := starAround(p) {
-- 
--    override r.x = St.ball.center
--    override r.theta = 2*MathPI() * (match_id - 1)/match_total
--    
--    scalar dBall  = rayIntersectDistance( St.ball, r.x, r.v )
--    scalar dDomain  = rayIntersectDistance( D.neumannBoundary, r.x, r.v )
--    scalar R = min( dBall, dDomain )
--    override r.L = R
-- 
--    override r.icon.ensureOnCanvas = false
-- 
--    layer r.icon above St.ball
--    layer p.dot above r.icon
-- }

-- alternate-domain.style ------------------------

-- forall Domain D {
--    vec2 D.q1 = ((.92,.84) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q2 = ((.84,.88) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q3 = ((.55,.60) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q4 = ((.68,.96) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q5 = ((.59,1.0) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q6 = ((.33,.90) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q7 = ((.41,.58) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q8  = ((.16,.77) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q9  = ((0.0,.49) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q10 = ((.03,.15) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q11 = ((.43,0.0) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q12 = ((.32,.32) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q13 = ((.48,.36) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q14 = ((.46,.06) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q15 = ((.76,0.0) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q16 = ((1.0,.27) - (0.5,0.5)) * D.scale + D.offset
--    vec2 D.q17 = ((.77,.43) - (0.5,0.5)) * D.scale + D.offset
-- 
--    -- domain boundary
--    override D.geometry.points = ( D.q1, D.q2, D.q3, D.q4, D.q5, D.q6, D.q7, D.q8, D.q9, D.q10, D.q11, D.q12, D.q13, D.q14, D.q15, D.q16, D.q17 )
-- 
--    -- Dirichlet boundary
--    override D.poly0.points = ( D.q8, D.q9, D.q10, D.q11 )
-- 
--    -- Neumann boundary
--    override D.poly1.points = ( D.q1, D.q2, D.q3, D.q4, D.q5, D.q6, D.q7, D.q8 )
-- 
--    -- Dirichlet boundary
--    override D.poly2.points = ( D.q16, D.q17, D.q1 )
-- 
--    -- Neumann boundary
--    override D.poly3.points = ( D.q11, D.q12, D.q13, D.q14, D.q15, D.q16 )
-- }
-- 
-- forall Point \`x\` {
--    override \`x\`.location = (0,0)
-- }

-- WalkOnSpheresTrajectories.style -----------

-- forall Point \`x0\` {
--    override \`x0\`.location = (0,0)
-- }

`,o=`type Domain -- a region in ℝⁿ
predicate hasMixedBoundaryConditions( Domain D )
predicate hasLegend( Domain D )

type Point
predicate isSourceSample( Point p )
constructor closestBoundaryPoint( Domain D, Point p ) -> Point q

type Subdomain
type Ball <: Subdomain
type Star <: Subdomain
constructor sampleBoundary( Subdomain S ) -> Point p
constructor sampleInterior( Subdomain S ) -> Point p

constructor ballAround( Point p ) -> Ball
constructor starAround( Point p ) -> Star
predicate isOffCenter( Ball B ) -- is not centered around p

type NestedPoint
constructor startWalkFrom( Subdomain S ) -> NestedPoint p
constructor nestedBallAround( NestedPoint p ) -> Ball
constructor sampleBoundaryNested( Subdomain S ) -> NestedPoint p

type Ray
constructor toBoundary( Point p, Subdomain S ) -> Ray
constructor headOf( Ray r ) -> Point

`;export{o as d,a as r,l as s};
