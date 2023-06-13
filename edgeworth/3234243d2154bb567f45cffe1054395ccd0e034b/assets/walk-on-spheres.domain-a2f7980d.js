import{a as e}from"./index-2b9b0491.js";const a=e("walk-on-spheres"),t=`-- diagram dimensions (in px)
canvas {
   width = 320
   height = 240
}

-- some colors re-used throughout
Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color darkGray = rgba(.5,.5,.5,1)
   color lightGray = rgba(.9,.9,.9,1)
}

-- global shapes and constants
Global {
   -- default appearance for label text
   string labelFont = "Palatino"
   string labelSize = "12px"
   color labelColor = Colors.black

   -- line thickness for basic shapes
   scalar sphereStrokeWidth = 1.5
   scalar domainStrokeWidth = 2


   -- invisible box around the canvas (used for reference/constraints)
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: none()
      strokeColor: none()
   }

   -- draw the domain as a polygon with some random variation, by
   -- perturbing the vertices of a hexagon by some bounded amount
   -- Note: if the domain shape is changed to something else (like
   -- an ellipse or a rectangle), things _should_ still work as
   -- expected, since later methods just make a call to \`signedDistance\`
   -- to determine ball radii.  However, signed distance may not yet
   -- be supported for all shapes (like arbitrary Bézier curves).
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   vec2 p3 = (?,?)
   vec2 p4 = (?,?)
   vec2 p5 = (?,?)
   scalar maxPerturbation = 30.
   ensure lessThan( norm(p0), maxPerturbation )
   ensure lessThan( norm(p1), maxPerturbation )
   ensure lessThan( norm(p2), maxPerturbation )
   ensure lessThan( norm(p3), maxPerturbation )
   ensure lessThan( norm(p4), maxPerturbation )
   ensure lessThan( norm(p5), maxPerturbation )
   shape domain = Polygon {
      points: ( (150,-110)+p0, (0,-110)+p1, (-150,-110)+p2, (-150,110)+p3, (0,110)+p4, (150,110)+p5)
      fillColor: Colors.lightGray
      strokeColor: Colors.darkGray
      strokeWidth: Global.domainStrokeWidth
      ensureOnCanvas: false
   }
   ensure contains( Global.box, Global.domain ) -- make sure the domain shape stays on the canvas
   layer box below domain
}

-- label the domain if it was assigned a label in the Substance program
forall Domain U
where U has label {
   shape U.labelText = Text {
      string: U.label
      center: (?,?)
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fillColor: Colors.darkGray
   }

   ensure lessThan( signedDistance( Global.domain, U.labelText.center ), -10. )

   layer U.labelText above Global.domain -- make sure the label doesn't get covered by the domain shape
}

-- draw each step of a walk as a ball tangent to the domain boundary
forall Step s {

   -- The \`ball\` shape is a flat-shaded semi-transparent
   -- disk that determines the color of the ball; its center
   -- and radius are used as a reference for all other shapes
   -- related to this ball.  Note that we don't need to
   -- explicitly enforce that the ball remains on the canvas,
   -- since the domain shape already has an onCanvas constraint,
   -- and the balls are always contained in the domain shape, by
   -- construction.
   shape s.ball = Circle {
      center: (?,?)
      fillColor: rgba( .2, .8, .2, .15 )
      strokeColor: none()
      ensureOnCanvas: false
   }

   -- Make sure that the ball center is inside the problem domain.
   scalar R = signedDistance( Global.domain, s.ball.center )
   ensure lessThan( R, 0. )

   -- Also set the ball radius equal to the distance from the
   -- ball center to the closest point on the domain boundary
   -- (just as in the WoS algorithm!).  We also subtract half
   -- the stroke widths,
   s.ball.r = -R - Global.sphereStrokeWidth/2 - Global.domainStrokeWidth/2.

   -- The \`sphere\` shape represents the boundary of the ball,
   -- and is drawn as an empty circle with a thick line.
   shape s.sphere = Circle {
      center: s.ball.center
      r: s.ball.r
      fillColor: none()
      strokeColor: rgba( .4, .5, .4, 1 )
      strokeWidth: Global.sphereStrokeWidth
      ensureOnCanvas: false
   }

   -- The \`dot\` shape represents the sample point associated
   -- with the ball.  Ordinarily this point will be at the
   -- center of the ball, but in general it could be at a
   -- different location (e.g., for an off-center walk).
   shape s.dot = Circle {
      center: s.ball.center
      r: 2.5
      fillColor: Colors.black
      ensureOnCanvas: false
   }

   -- To give the balls a three-dimensional appearance, blend
   -- a shading image behind the flat colored disk.
   shape s.shading = Image {
      center: s.ball.center 
      width: s.ball.r * 2.0
      height: s.ball.r * 2.0
      href: "walk-on-spheres-ball.svg"
      ensureOnCanvas: false
   }

   -- Make sure all components of the ball get drawn in the
   -- proper order, and are not covered up by the domain shape.
   layer s.shading above Global.domain
   layer s.ball above s.shading
   layer s.sphere above s.ball
   layer s.dot above s.ball

   -- This constant will be used to determine how much balls
   -- shrink from one step of the walk to the next.  We define
   -- it here so that specialized types of balls (say, those
   -- coming from a nested walk) can use a different factor.
   scalar s.shrinkFactor = .55
}

-- Make sure the domain's label isn't covered up by a ball.
forall Step x; Domain U
where U has label {
   ensure disjoint( U.labelText, x.sphere, 10. )
}


-- For a step of an off-centered walk, we want to clearly
-- indicate that the sample point may not be at the ball center.
forall Step s
where offCenter(s) {

   -- We can no longer just copy the dot center from the ball center,
   -- but will instead let the layout engine figure out its location.
   override s.dot.center = (?,?)

   -- Still keep the center within the middle 75% of the ball, since
   -- the dot may be hard to see if it gets too close to the boundary
   -- (especially for very small balls).
   ensure lessThan( norm(s.dot.center-s.ball.center), .75*s.ball.r )
}

-- If the step has a label, draw a label string near the sample point.
forall Step s
where s has label {
   shape s.labelText = Text {
      string: s.label
      center: (?,?) -- the layout engine will determine the exact placement
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fontStyle: "italic"
      fillColor: Global.labelColor
      strokeColor: Colors.white
      strokeWidth: 2.5
      strokeLinejoin: "round"
      paintOrder: "stroke"
   }

   -- We want the label to be near the dot, but don't want it
   -- to overlap the dot (which makes it hard to read).
   encourage near( s.labelText, s.dot )
   ensure disjoint( s.labelText, s.dot, 2. )

   -- Make sure the label doesn't get covered by the ball or
   -- its boundary by layering it above the topmost shape.
   layer s.labelText above s.sphere
}

-- Draw steps that belong to a nested walk in a different style.
forall Step s
where nested( s ) {
   -- Change to a color palette that is more white & opaque.
   override s.ball.fillColor = rgba( 1, 1, 1, .4 )
   override s.dot.fillColor = Colors.white
   override s.sphere.strokeColor = Colors.white

   -- Since nested walks are usually secondary in the overall
   -- diagram, draw them smaller by making them shrink faster.
   override s.shrinkFactor = .7
}

-- If a step x1 is sampled from the boundary of another step x0,
-- draw the sample point on the sphere around x0.
forall Step x0; Step x1
where x1 := sampleBoundary(x0) {

   ensure equal( norm(x1.dot.center-x0.ball.center), x0.ball.r )

   -- draw later steps of the walk as smaller
   -- balls (which will typically, but not always,
   -- be the behavior of the WoS algorithm)
   ensure equal( x1.ball.r, x1.shrinkFactor * x0.ball.r )

   -- It's typically easier to see labels, etc., if later, smaller
   -- balls are drawn on top of earlier, bigger ones.
   layer x1.shading above x0.sphere

   -- Also draw a line between the sample points, to help
   -- illustrate the path along the walk.
   color walkColor = rgba( 0, 0, .7, 1 )
   shape walkLine = Line {
      start: x0.dot.center
      end: x1.dot.center
      strokeWidth: 2.
      strokeColor: walkColor
   }
   layer walkLine above Global.domain

   -- Use layering to help clarify the direction of the
   -- walk, by always drawing the line above x0 and below x1.
   layer walkLine above x0.shading
   layer walkLine below x1.shading
}

-- The next step in a walk might also be sampled from
-- the ball interior (e.g., to estimate a value expressed
-- in terms of a source term).
forall Step y; Step x
where y := sampleInterior( x ) {

   -- Put the center of the next ball somewhere inside
   -- the current ball, between 20% and 80% of the
   -- radial distance from the center.
   scalar R = norm(y.dot.center-x.ball.center)
   scalar r = x.ball.r
   ensure inRange( R, .2*r, .8*r )

   -- Since these samples tend to be the start of a
   -- secondary, "nested" walk, we'll draw this ball
   -- much smaller than its parent in the walk.
   ensure equal( y.ball.r, .1 * x.ball.r )

   -- Make sure the smaller ball gets drawn on top
   -- of the bigger ball by putting the lowest layer
   -- of the former above the highest layer of the latter.
   layer y.shading above x.sphere
}

-- Draw sample points that are used to estimate the source term
-- as just dots contained in the ball of interest.  These sample
-- points are different from steps of a walk, since we never
-- need to consider a ball around these points (and hence do not
-- want to draw one).
forall Sample p; Step x
where p := sampleSource( x ) {

   shape p.dot = Circle {
      center: (?,?)
      r: 2
      fillColor: Colors.white
      strokeColor: Colors.black
      strokeWidth: 1
   }

   -- Put the sample point somewhere between 20% and
   -- 80% away from the center, so that it's clearly visible
   -- and doesn't run into the center point or the boundary.
   scalar R = norm( p.dot.center - x.ball.center )
   scalar r = x.ball.r
   ensure inRange( R, .2*r, .8*r )

   -- Make sure the dot is not covered by the ball.
   layer p.dot above x.ball
}

-- Label a sample point if it a label string was specified
-- in the Substance program.
forall Sample p
where p has label {
   shape p.labelText = Text {
      string: p.label
      center: (?,?)
      fontSize: Global.labelSize
      fontFamily: Global.labelFont
      fontStyle: "italic"
      fillColor: Global.labelColor
      strokeColor: Colors.white
      strokeWidth: 2.5
      strokeLinejoin: "round"
      paintOrder: "stroke"
   }

   -- Put the label near the dot, but don't allow
   -- it to cover up the dot (or vice versa).
   encourage near( p.labelText, p.dot )
   ensure disjoint( p.labelText, p.dot, 2. )
}

-- If this sample was drawn from a ball around x,
-- make sure the sample's label is drawn on top
-- of that ball.
forall Sample y; Step x
where y has label; y := sampleSource(x) {
   layer y.labelText above x.sphere
}

-- Also make sure the sample point isn't covered
-- by the next or previous shading in the walk
forall Step x0; Step x1; Sample y0
where x1 := sampleBoundary(x0); y0 := sampleSource(x0) {
   ensure disjoint( x1.sphere, y0.dot )
}
forall Step x0; Step x1; Sample y1
where x1 := sampleBoundary(x0); y1 := sampleSource(x1) {
   ensure disjoint( x0.sphere, y1.dot )
}

`,l=`type Domain -- a region in ℝⁿ
type Step -- a step in a walk
type Sample -- a sample point used for an estimator

-- x1 := sampleBoundary(x0) takes a step in the walk by sampling the boundary
constructor sampleBoundary( Step x0 ) -> Step x1

-- y1 := sampleInterior(x0) takes a step in the walk by sampling the interior
constructor sampleInterior( Step x0 ) -> Step y1

-- y1 := sampleSource(x0) samples a source in the interior, but does not start a new walk
constructor sampleSource( Step x0 ) -> Sample y1

-- nested(x) asserts that step x belongs to a nested walk,
-- e.g., a walk that begins at a source sample point and then
-- continues all the way to the boundary
predicate nested( Step x )

-- offCenter(x) asserts that step x is an off-centered step,
-- which need not be placed at the center of the ball
predicate offCenter( Step x )

`;export{l as d,a as r,t as s};
