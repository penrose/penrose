var W=Object.defineProperty;var c=(e,s)=>W(e,"name",{value:s,configurable:!0});import{s as T,a as A,r as O,d as R,m as p}from"./vector-wedge.substance-c51fe7ca.js";import{r as o,j as n,a as d,F as G}from"./jsx-runtime-433938a6.js";import{s as i}from"./styled-components.browser.esm-3d2f5315.js";import{S as Q}from"./Simple-e159516c.js";import{R as z,L as N}from"./Resample-1c2dca26.js";import"./iframe-f6c56226.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-f51ff477.js";import"./svg-ca0c608d.js";const a={substance:T,style:[{contents:A,resolver:O}],domain:R,variation:"SnorlaxIbis743",excludeWarnings:["BBoxApproximationWarning"]},M=`-- name in registry: incenter-triangle
Point J, K, L, P, m
Let JKL := Triangle(J, K, L)
Incenter(P, JKL)
-- Centroid(P, JKL)
-- Circumcenter(P, JKL)
-- Orthocenter(P, JKL)
Let KL := Segment(K, L)
Collinear(K, m, L)
Let PLM := Triangle(P, L, m)
Angle PML := InteriorAngle(P, m, L)
RightMarked(PML)
AutoLabel J, K, L, P, m
`,I=p("geometry-domain"),j=`canvas {
  width = 500
  height = 500
}

Colors {
    -- Keenan palette
    black = #000000
    darkpurple = #8c90c1
    lightpurple = #d0d3e6
    purple2 = rgba(0.106, 0.122, 0.54, 0.2)
    verylightpurple = rgba(0.953, 0.957, 0.977, 1.0)
    purple3 = rgba(0.557, 0.627, 0.769, 1.0)
    midnightblue = rgba(0.14, 0.16, 0.52, 1.0)
    none = none()
}

const {
    arrowheadSize = 0.65
    strokeWidth = 1.75
    textPadding = 10.0
    textPadding2 = 25.0
    repelWeight = 0.7 -- TODO: Reverted from 0.0
    repelWeight2 = 0.5
    fontSize = "20px"
    containPadding = 50.0
    rayLength = 100.0
    pointRadius = 4.0
    pointStroke = 0.0
    thetaRadius = 30.0
    circleRadius = 150.0
    labelPadding = 30.0
    minSegmentLength = 80.0
    minLineLength = 200.0
}

layout = [shape, label]

--Plane
forall Plane p {
  width = canvas.width * .8
  height = canvas.height * .8
  p.text = Equation {
    center : ((width / 2.0) - const.textPadding2, (height / 2.0) - const.textPadding2)
    string : p.label
    fontSize : const.fontSize
  }

  -- inner: #f3f4f9, outer: #8e93c4
  p.icon = Rectangle {
    -- angle : 0.0
    -- fillColor : Colors.purple2
    fillColor : Colors.none -- TODO: arrange angle markers so plane can be opaque
    strokeColor : Colors.purple3
    strokeWidth : 2.0
    center : (0.0, 0.0)
    width : width
    height : height
  }

  p.text above p.icon
}

--Point
forall Point p {
  p.x = ? except label
  p.y = ? except label
  p.vec = (p.x, p.y)
  p.color = Colors.black

  p.icon = Circle {
    center: p.vec
    r : const.pointRadius
    fillColor : Colors.black
    strokeWidth : 0.0
    strokeColor : Colors.black
  }

  p.text = Equation {
    string : p.label
    fillColor : Colors.black
    fontSize : const.fontSize
    center: (? in label, ? in label)
    ensureOnCanvas: false
  }
  ensure onCanvas(p.text, canvas.width, canvas.height) in label
  ensure signedDistance(p.text, p.vec) == const.textPadding + const.pointRadius in label
}

-- default: if \`Point\` is not on a \`Plane\`, the point should be below the plane to stay hidden
forall Point p
with Plane P {
  p.iconOnPlane = ensure disjoint(p.icon, P.icon)
  p.textOnPlane = ensure disjoint(p.text, P.icon) in label
}

forall Point p
with Plane P
where In(p, P) {
  -- TODO: the problem is that this ensures the padding is const? Or is > padding okay?
  -- There's a choice of whether to put padding on the point or the text for containment
  override p.iconOnPlane = ensure contains(P.icon, p.icon, const.containPadding)
  override p.textOnPlane = ensure contains(P.icon, p.text) in label

  p.icon above P.icon
  p.text above P.icon
}

forall Point p, q, r
where Collinear(p, q, r) {
  ensure collinearOrdered(p.icon.center, q.icon.center, r.icon.center) 
  encourage notTooClose(p.icon, r.icon, const.repelWeight)
}

forall Point p
with Linelike l
where On(p, l) {
  ensure signedDistance(l.icon, p.vec) == 0
}

forall Point p
with Linelike l
where On(p, l); p has label {
  ensure disjoint(l.icon, p.text) in label
}

--Linelike
forall Linelike l {
  l.color = Colors.black

  l.icon = Line {
    start : (?, ?)
    end : (?, ?)
    strokeColor : l.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
}

forall Ray r
where r := Ray(base, direction)
with Point base; Point direction {
  r.start = base.vec
  r.end = direction.vec
  r.vec = direction.vec - base.vec

  override r.icon = Line {
    start : base.icon.center
    end : ptOnLine(base.vec, direction.vec, norm(r.vec) + 40.)
    strokeColor : r.color
    strokeWidth : const.strokeWidth
    style : "solid"
    endArrowhead : "straight"
    endArrowheadSize: const.arrowheadSize
  }
  -- labeling
  ensure disjoint(r.icon, base.text) in label
  ensure disjoint(r.icon, direction.text) in label
}

forall Ray r {
    r.length = const.rayLength
}

forall Ray r
with Angle theta; Point x; Point y; Point z
where r := Bisector(theta); theta := InteriorAngle(y, x, z) {
  -- find the vector for the bisector ray
  xy = normalize(y.vec - x.vec)
  xz = normalize(z.vec - x.vec)
  r_vec = xy + xz
  -- change from generic \`Linelike\` shape to a specific shape for \`Ray\`
  override r.icon = Line {
    start: x.vec
    end:  (r.length * normalize(r_vec)) + x.vec
    strokeWidth : const.strokeWidth
    strokeColor : Colors.black
    endArrowhead: "straight"
    endArrowheadSize : const.arrowheadSize
  }
  r.icon below x.icon
}

forall Line l
where l := Line(p, q)
with Point p; Point q {
  l.start = p.vec
  l.end = q.vec
  l.vec = q.vec - p.vec
  override l.icon = Line {
    start : ptOnLine(p.vec,q.vec, -40.)
    end : ptOnLine(p.vec, q.vec, norm(l.vec) + 40.)
    strokeColor : l.color
    strokeWidth : const.strokeWidth
    style : "solid"
    endArrowhead : "straight"
    startArrowhead: "straight"
    startArrowheadSize: const.arrowheadSize
    endArrowheadSize: const.arrowheadSize
  }

  -- edge case
  ensure norm(l.vec) > const.minLineLength

  -- labeling
  ensure disjoint(l.icon, p.text) in label
  ensure disjoint(l.icon, q.text) in label
}

forall Linelike l1, l2 -- should this work with rays and lines?
where ParallelMarker1(l1, l2) {
  l1.tick1 = Path {
    d : pathFromPoints("open", chevron(l1.icon, 20.))
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor : Colors.none
  }
  l2.tick1 = Path {
    d : pathFromPoints("open", chevron(l2.icon, 20.))
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor : Colors.none
  }
}

forall Linelike l1, l2
where Parallel(l1, l2) {
  -- the dot product of the unit vectors of parallel lines is 1
  -- HACK: scaling to 10000s for convergence
  ensure 10000 == dot(normalize(l1.vec), normalize(l2.vec)) * 10000
}
--Segment
forall Segment e
where e := Segment(p, q)
with Point p; Point q {
  e.vec = [q.x - p.x, q.y - p.y]
  e.start = p.vec
  e.end = q.vec

  override e.icon = Line {
    start : p.icon.center
    end : q.icon.center
    strokeColor : e.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }

  p.icon above e.icon
  q.icon above e.icon

  -- edge case
  ensure norm(e.vec) > const.minSegmentLength

  -- labeling
  ensure disjoint(p.text, e.icon) in label
  ensure disjoint(q.text, e.icon) in label
}

forall Segment e; Plane p {
  e.icon above p.icon
}

forall Linelike s, t
where EqualLength(s, t) {
  ensure vdist(s.icon.start, s.icon.end) == vdist(t.icon.start, t.icon.end)
}

--TODO eventually this should also provide an equal length marker since it is bisecting the segment
forall Segment s
where s := PerpendicularBisector(s2, p)
with Segment s2; Point p {
  override s.icon = Line {
    start : p.icon.center
    end : midpoint(s2.icon.start, s2.icon.end)
    strokeColor : s.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  startA = ptOnLine(s.icon.end, s.icon.start, const.thetaRadius)
  endA = ptOnLine(s.icon.end, s2.icon.end, const.thetaRadius)
  sweepA = arcSweepFlag(s.icon.end, startA, endA)

  s.mark = Path {
    d : pathFromPoints("open", [ptOnLine(s.icon.end, s.icon.start, 20.), innerPointOffset(s.icon.end, s.icon.start, s2.icon.end, 20.), ptOnLine(s.icon.end, s2.icon.end, 20.)])
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor : Colors.none
  }

  ensure perpendicular(s.icon.start, s.icon.end, s2.icon.end)
}

forall Segment s
where s := PerpendicularBisectorLabelPts(s2, p1, p2)
with Segment s2; Point p1, p2 {
  override p2.vec = midpoint(s2.icon.start, s2.icon.end)
  override s.icon = Line {
    start : p1.icon.center
    end : p2.vec
    strokeColor : s.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  startA = ptOnLine(s.icon.end, s.icon.start, const.thetaRadius)
  endA = ptOnLine(s.icon.end, s2.icon.end, const.thetaRadius)
  sweepA = arcSweepFlag(s.icon.end, startA, endA)

  s.mark = Path {
    d : pathFromPoints("open", [ptOnLine(s.icon.end, s.icon.start, 20.), innerPointOffset(s.icon.end, s.icon.start, s2.icon.end, 20.), ptOnLine(s.icon.end, s2.icon.end, 20.)])
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor : Colors.none
  }

  ensure perpendicular(s.icon.start, s.icon.end, s2.icon.end)
}

forall Linelike s, t 
where EqualLengthMarker(s, t) as e {
  e.equivGroup = match_id
  override s.tick = Path {
    d : ticksOnLine(s.icon.start, s.icon.end, 15., e.equivGroup, 10.)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
  }
  override t.tick = Path {
    d : ticksOnLine(t.icon.start, t.icon.end, 15., e.equivGroup, 10.)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
  }
  s.tick above s.icon
  t.tick above t.icon
}

-- HACK: set transitive equal marks to have the same amount of ticks. This will not decrement subsequent matches of \`EqualLength\`, so only use this when there are _only_ transitive equal marks in a diagram
forall Linelike s, t, u
where EqualLengthMarker(s, t) as e1; EqualLengthMarker(t, u) as e2 {
  minEquivGroup = min(e1.equivGroup, e2.equivGroup)
  -- NOTE: since we cannot handle transitive predicates and don't allow recursive expressions, we override the shape properties so the tick counts are the same
  override s.tick.d = ticksOnLine(s.icon.start, s.icon.end, 15., minEquivGroup, 10.) 
  override u.tick.d = ticksOnLine(u.icon.start, u.icon.end, 15., minEquivGroup, 10.) 
  override t.tick.d = ticksOnLine(t.icon.start, t.icon.end, 15., minEquivGroup, 10.) 
}

--Angle
forall Angle theta
where theta := InteriorAngle(p, q, r)
with Point p; Point q; Point r {
  theta.p = p.vec
  theta.q = q.vec
  theta.r = r.vec
  theta.color = #000
  theta.side1 = Line {
    start : p.icon.center
    end : q.icon.center
    strokeColor : theta.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  theta.side2 = Line {
    start : q.icon.center
    end : r.icon.center
    strokeColor : theta.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  
  theta.radius = const.thetaRadius
  -- encourage the angle to be bigger than 10
  angle = angleBetween(theta.q - theta.p, theta.q - theta.r) 
  ensure angle > 10/180 * MathPI()
  theta.side1 below p.icon, q.icon
  theta.side2 below q.icon, r.icon
}

forall Angle theta
where theta := InteriorAngle(p, q, r); theta has label
with Point p; Point q; Point r {
  padding = const.textPadding + const.pointRadius + theta.text.width
  labelDir = normalize((p.vec - q.vec) + (r.vec - q.vec))
  theta.text = Equation {
    string : theta.label
    fillColor : Colors.black
    fontSize : const.fontSize
    center: q.vec + labelDir*padding
  }
}

forall Angle a, b
where EqualAngleMarker(a, b) as e {
  e.equivGroup = match_id
  --find points from p->q, then q->r for each vector. draw vectors for each
  a.start = ptOnLine(a.q, a.p, a.radius)
  a.end = ptOnLine(a.q, a.r, a.radius)
  a.sweep = arcSweepFlag(a.q, a.start, a.end)
  a.spacing = 10

  b.start = ptOnLine(b.q, b.p, b.radius)
  b.end = ptOnLine(b.q, b.r, b.radius)
  b.sweep = arcSweepFlag(b.q, b.start, b.end)
  b.spacing = 10

  override a.mark = Path {
    d : repeatedArcs(a.start, a.end, a.p, a.r, (a.radius, a.radius), e.equivGroup, a.spacing, a.sweep)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
 }
  override b.mark = Path {
    d : repeatedArcs(b.start, b.end, b.p, b.r, (b.radius, b.radius), e.equivGroup, b.spacing, b.sweep)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
 }
}

forall Angle a, b, c
where EqualAngleMarker(a, b) as e1; EqualAngleMarker(b, c) as e2 {
  minEquivGroup = min(e1.equivGroup, e2.equivGroup)
  override a.mark.d = repeatedArcs(a.start, a.end, a.p, a.r, (a.radius, a.radius), minEquivGroup, a.spacing, a.sweep)
  override b.mark.d = repeatedArcs(b.start, b.end, b.p, b.r, (b.radius, b.radius), minEquivGroup, b.spacing, b.sweep)
  override c.mark.d = repeatedArcs(c.start, c.end, c.p, c.r, (c.radius, c.radius), minEquivGroup, c.spacing, c.sweep)
}

forall Angle a, b
where EqualAngle(a, b) {
  -- make sure angle a is equal to angle b
  -- HACK: increase the magnitude of angles
  weight = 100
  angleA = angleBetween(a.p - a.q, a.r - a.q) * weight
  angleB = angleBetween(b.p - b.q, b.r - b.q) * weight
  ensure angleA == angleB 
}

forall Angle a
where RightUnmarked(a) {
  -- ensure perpendicular(a.p, a.q, a.r)
  vec2 u = a.p - a.q
  vec2 v = a.r - a.q
  ensure dot(u, v) == 0
}

forall Angle a
where RightMarked(a) {
  --render half square path of size a.radius
  markSize = 10
  a.mark = Path {
    d : pathFromPoints("open", [ptOnLine(a.q, a.p, markSize), innerPointOffset(a.q, a.p, a.r, markSize), ptOnLine(a.q, a.r, markSize)])
    strokeWidth : 2.0
    strokeColor : #000
    fillColor : Colors.none
  }
  vec2 u = a.p - a.q
  vec2 v = a.r - a.q
  ensure dot(u, v) == 0
}

forall Angle a
where Acute(a) {
  ensure inRange(angleBetween(a.p - a.q, a.r - a.q), 0, MathPI()/2)
}

forall Angle a
where Obtuse(a) {
  ensure inRange(angleBetween(a.p - a.q, a.r - a.q), MathPI()/2, MathPI())
}

forall Triangle t; Plane P 
where t := Triangle(p, q, r)
with Point p; Point q; Point r {
  t.PQ above P.icon
  t.QR above P.icon
  t.RP above P.icon
  t.icon above P.icon
}

forall Triangle t
where t := Triangle(p, q, r)
with Point p; Point q; Point r {
  t.color = Colors.black
  t.PQ = Line {
    start : p.icon.center
    end : q.icon.center
    strokeColor : t.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  t.QR = Line {
    start : q.icon.center
    end : r.icon.center
    strokeColor : t.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  t.RP = Line {
    start : r.icon.center
    end : p.icon.center
    strokeColor : t.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  t.icon = Path {
    d: pathFromPoints("closed", [p.vec, q.vec, r.vec])
    fillColor: none()
    strokeColor: none()
  }
  ensure norm(p.vec - q.vec) > const.minSegmentLength
  ensure norm(q.vec - r.vec) > const.minSegmentLength
  ensure norm(r.vec - p.vec) > const.minSegmentLength
  ensure disjoint(t.icon, p.text) in label
  ensure disjoint(t.icon, q.text) in label
  ensure disjoint(t.icon, r.text) in label

  t.PQ below p.icon
  t.PQ below q.icon
  t.QR below q.icon
  t.QR below r.icon
  t.RP below r.icon
  t.RP below p.icon
}

forall Point p
with Triangle T; Point t1, t2, t3
where T := Triangle(t1, t2, t3); Incenter(p, T) {
  override p.vec = incenter(t1.vec, t2.vec, t3.vec)
  clr = setOpacity(Colors.darkpurple, 0.6)
  T.incenterIcon = Circle {
    center : p.vec
    r : inradius(t1.vec, t2.vec, t3.vec) - const.strokeWidth
    strokeWidth : const.strokeWidth
    strokeColor : clr
    strokeStyle: "dashed"
    fillColor : Colors.none
  }
  T.incenterIcon below t1.icon
  T.incenterIcon below t2.icon
  T.incenterIcon below t3.icon
}

forall Point p
where Circumcenter(p, T)
with Triangle T {
  clr = Colors.darkpurple
  override p.icon = Circle {
    center : p.vec
    r : const.pointRadius
    strokeWidth : const.strokeWidth
    strokeColor : clr
    fillColor : clr
  }
  p.icon above T.PQ
  p.icon above T.QR
  p.icon above T.RP
  T.icon = Circle {
    center : p.vec
    r : ?
    strokeWidth : const.strokeWidth
    strokeColor : clr
    fillColor : Colors.none
    strokeStyle: "dashed"
  }
  ensure norm(T.PQ.start - p.vec) == T.icon.r
  ensure norm(T.QR.start - p.vec) == T.icon.r
  ensure norm(T.RP.start - p.vec) == T.icon.r
  encourage repelPt(const.repelWeight, T.PQ.start, T.QR.start)
  encourage repelPt(const.repelWeight, T.QR.start, T.RP.start)
}

forall Point p
where Centroid(p, T)
with Triangle T {
  clr = setOpacity(Colors.darkpurple, 0.6)
  override p.vec = vmul(1/3, T.PQ.start + T.QR.start + T.RP.start)
  override p.icon = Circle {
    center : p.vec
    r : const.pointRadius
    strokeWidth : const.strokeWidth
    strokeColor : clr
    fillColor : clr
  }
  override T.icon = Line {
    start : T.PQ.start
    end : midpoint(T.QR.start, T.QR.end)
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  override T.line2 = Line {
    start : T.QR.start
    end : midpoint(T.RP.start, T.RP.end)
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  override T.icon3 = Line {
    start : T.RP.start
    end : midpoint(T.PQ.start, T.PQ.end)
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }
}

forall Point p
where Orthocenter(p, T)
with Triangle T {
  clr = setOpacity(Colors.darkpurple, 0.6)
  T.icon = Line {
    start : (?, ?)
    end : p.vec
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  T.icon2 = Line {
    start : (?, ?)
    end : p.vec
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  T.icon3 = Line {
    start : (?, ?)
    end : p.vec
    strokeColor : clr
    strokeWidth : const.strokeWidth
    style : "solid"
  }

  -- TODO make it so that predicates can reference other predicates. 3/4 of these are copy-pasted from the Collinear predicate
  ensure collinear(T.PQ.start, T.icon.start, T.PQ.end)
  ensure collinear(T.QR.start, T.icon2.start, T.QR.end)  
  ensure collinear(T.RP.start, T.icon3.start, T.RP.end)

  ensure perpendicular(T.PQ.start, T.icon.start, p.vec)
  ensure perpendicular(T.QR.start, T.icon2.start, p.vec)  
  ensure perpendicular(T.RP.start, T.icon3.start, p.vec)

  encourage repelPt(const.repelWeight, T.PQ.start, T.icon.start)
  encourage repelPt(const.repelWeight, T.PQ.end, T.icon.start)

  encourage repelPt(const.repelWeight, T.QR.start, T.icon2.start)
  encourage repelPt(const.repelWeight, T.QR.end, T.icon2.start)

  encourage repelPt(const.repelWeight, T.RP.start, T.icon3.start)
  encourage repelPt(const.repelWeight, T.RP.end, T.icon3.start)

}

--Rectangle
-- -- Should the rectangle be constructed from the points, or vice versa?
forall Rectangle R
where R := Rectangle(p, q, r, s)
with Point p; Point q; Point r; Point s {
  override R.color = Colors.none
  override R.icon = Path {
      d : pathFromPoints("closed", [p.icon.center, q.icon.center, r.icon.center, s.icon.center])
      strokeWidth : const.strokeWidth
      fillColor : R.color
      strokeColor : Colors.black
    }
  ensure vdist(p.icon.center, q.icon.center) == vdist(r.icon.center, s.icon.center)
  ensure vdist(p.icon.center, s.icon.center) == vdist(q.icon.center, r.icon.center)

  ensure perpendicular(p.icon.center, q.icon.center, r.icon.center)
  ensure perpendicular(q.icon.center, s.icon.center, r.icon.center)

  -- R.icon above P.icon
}

forall Quadrilateral Q
where Q := Quadrilateral(p, q, r, s)
with Point p; Point q; Point r; Point s {
  Q.p = p.icon.center
  Q.q = q.icon.center
  Q.r = r.icon.center
  Q.s = s.icon.center

  override Q.color = Colors.black
  Q.side1 = Line {
    start : Q.p
    end : Q.q
    strokeColor : Q.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  Q.side2 = Line {
    start : Q.r
    end : Q.q
    strokeColor : Q.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  Q.side3 = Line {
    start : Q.s
    end : Q.r
    strokeColor : Q.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  Q.side4 = Line {
    start : Q.p
    end : Q.s
    strokeColor : Q.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }  
  Q.labelContainer = Path {
    d: pathFromPoints("closed", [Q.p, Q.q, Q.r, Q.s])
    fillColor: none()
  }
  -- TODO: check if the points actually have labels
  ensure disjoint(p.text, Q.labelContainer) in label
  ensure disjoint(q.text, Q.labelContainer) in label
  ensure disjoint(r.text, Q.labelContainer) in label
  ensure disjoint(s.text, Q.labelContainer) in label
  -- ensure all sides are visible
  ensure norm(Q.p - Q.q) > const.minSegmentLength
  ensure norm(Q.q - Q.r) > const.minSegmentLength
  ensure norm(Q.r - Q.s) > const.minSegmentLength
  ensure norm(Q.s - Q.p) > const.minSegmentLength
  ensure norm(Q.r - Q.p) > const.minSegmentLength
  ensure norm(Q.s - Q.q) > const.minSegmentLength
}

--FUNCTIONS
forall Segment s
with Triangle T; Point p, q, r, a, b
where s := MidSegment(T, a, b); T := Triangle(p, q, r) {
  override a.vec = midpoint(q.vec, r.vec)
  override b.vec = midpoint(r.vec, p.vec)
  override s.icon = Line {
    start : a.vec
    end : b.vec
    strokeColor : s.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }
}

forall Point p
where p := Midpoint(l)
with Linelike l {
  override p.vec = midpoint(l.icon.start, l.icon.end)
}

forall Point p
where Midpoint(l, p)
with Linelike l {
  override p.vec = midpoint(l.icon.start, l.icon.end)
}

forall Point p
where Midpoint(l, p); p has label
with Linelike l {
  override p.vec = midpoint(l.icon.start, l.icon.end)
  ensure disjoint(p.text, l.icon) in label
}

-- TODO sometimes bisector becomes a scaled version of either side length PQ or QR of angle PQR
forall Linelike s
where AngleBisector(a, s)
with Angle a; Point p {
  weight = 100
  angleA = angleBetween(a.p - a.q, s.end - a.q) * weight
  angleB = angleBetween(s.end - a.q, a.r - a.q) * weight
  bigAngle = angleBetween(a.q - a.p, a.q - a.r) * weight
  ensure angleA == angleB 
  -- HACK: make sure the angle itself is not zero
  ensure bigAngle > 15/180 * MathPI() * weight 
  -- HACK: make sure the bisector end point is between angle end points. Might be too specific
  ensure inRange(s.end[0], a.p[0], a.r[0])
  ensure inRange(s.end[1], a.p[1], a.r[1])
}

forall Circle c {
  c.radius = const.circleRadius
  c.vec = (?, ?)

  c.icon = Circle {
    center : c.vec
    r : c.radius
    strokeWidth : const.strokeWidth
    strokeColor : Colors.black
    fillColor : Colors.none
  }
}

forall Circle c
where c := CircleR(p, q)
with Point p, q {
  override c.radius = vdist(p.vec, q.vec)
  override c.vec = p.icon.center
  override c.icon = Circle {
    center : c.vec
    r : c.radius
    strokeWidth : const.strokeWidth
    strokeColor : Colors.black
    fillColor : Colors.none
  }
}

-- TODO this can be reimplemented when issue #621 is resolved
-- Circle c
-- where c := CircleD(p, q)
-- with Point p, q {
--   override c.radius = vdist(p.vec, q.vec) / 2
-- }

forall Segment s
where s := Chord(c, p, q)
with Circle c; Point p, q {
  override s.vec = q.vec - p.vec
  override s.icon = Line {
    start : p.icon.center
    end : q.icon.center
    strokeColor : s.color
    strokeWidth : const.strokeWidth
    style : "solid"
  }

  p.icon above c.icon
  q.icon above c.icon
  ensure norm(p.vec - c.vec) == c.radius
  ensure norm(q.vec - c.vec) == c.radius
  ensure norm(q.vec - p.vec) > const.minSegmentLength
}

forall Segment s
where s := Chord(c, p, q); p has label; q has label 
with Circle c; Point p, q {
  ensure disjoint(c.icon, p.text) in label
  ensure disjoint(c.icon, q.text) in label
}

forall Segment s
where s := Radius(c, p)
with Circle c; Point p {
  override s.vec = p.vec - c.vec
  override s.icon = Line {
    start : c.vec
    end : p.icon.center
    strokeColor : Colors.black
    strokeWidth : const.strokeWidth
    style : "solid"
  }
  p.icon above c.icon
  ensure norm(c.vec - p.vec) == c.radius
}

forall Segment s
where s := Radius(c, p); p has label 
with Circle c; Point p {
  ensure disjoint(p.text, s.icon) in label
}

forall Segment s
where s := Radius(c, p); p has label
with Circle c; Point p {
  ensure disjoint(p.text, c.icon) in label
}

forall Segment s
where s := Diameter(c, p, q)
with Circle c; Point p, q {
  override s.vec = q.vec - p.vec
  override s.icon = Line {
    start : p.icon.center
    end : q.icon.center
    strokeWidth : const.strokeWidth
    style : "solid"
    strokeColor : Colors.black
  }

  p.icon above c.icon
  q.icon above c.icon
  ensure norm(midpoint(p.vec, q.vec) - c.icon.center) == 0
  ensure c.icon.r * 2 == norm(s.vec)
}

forall Point p
where OnCircle(c, p)
with Circle c {
  ensure norm(c.vec - p.vec) == c.radius
}

forall Point p
where OnCircle(c, p); p has label
with Circle c {
  ensure disjoint(p.text, c.icon) in label
}

forall Point p
where CircleCenter(c, p)
with Circle c {
  override p.vec = c.vec
}

forall Shape s1, s2
where s1 has label; s2 has label {
  encourage notTooClose(s1.text, s2.text)
}`,F=p("geometry-domain/textbook_problems"),E=`global {
    shape background = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: #DDEDF4
    } 
}

forall Point p
with Triangle T; Point t1, t2, t3
where T := Triangle(t1, t2, t3); Incenter(p, T) {
  override T.incenterIcon.strokeColor = setOpacity(Colors.darkpurple, 0.8)
}`,_=`-- ~~~~~~~~~~~~~~~~ TYPES ~~~~~~~~~~~~~~~~
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
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"`,r={substance:M,style:[{contents:j,resolver:I},{contents:E,resolver:F}],domain:_,variation:"RationalityZebra567",excludeWarnings:["BBoxApproximationWarning"]},$=`-- A short walk used to estimate the solution to
-- a Laplace equation Δu = 0 with pure Dirichlet
-- boundary conditions, via WoS.

Domain U

Point x0
Ball B0 := ballAround( x0 )
Point x1 := sampleBoundary( B0 )
Ball B1 := ballAround( x1 )
Point x2 := sampleBoundary( B1 )
Ball B2 := ballAround( x2 )
Point x3 := sampleBoundary( B2 )
Ball B3 := ballAround( x3 )
Point x4 := sampleBoundary( B3 )
Ball B4 := ballAround( x4 )

Label U  $\\Omega$
Label x0 $x_0$
Label x1 $x_1$
Label x2 $x_2$
Label x3 $\\ldots$
Label x4 $x_k$

`,K=p("walk-on-spheres"),H=`layout = [ walkStage, nestStage, labelStage, legendStage ]

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

`,U=`type Domain -- a region in ℝⁿ
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

`,l={substance:$,style:[{contents:H,resolver:K}],domain:U,variation:"LilyDunlin3394",excludeWarnings:[]},J=i.div`
  position: relative;
  border-radius: 10px;
  border: 0.5px solid rgba(0, 0, 0, 0.2);
  box-shadow: 0 5px 8px 0 rgba(0, 0, 0, 0.2);
  background-color: #fff;
  overflow: hidden;
  min-height: 320px;
`,V=i.div`
  display: flex;
  justify-content: center;
  align-items: center;
  position: absolute;
  background-color: #0001;
  font-size: 20px;
  color: #000;
  left: 0;
  top: 0;
  right: 0;
  bottom: 0;
  cursor: pointer;
`,b=i.div`
  border-radius: 5px;
  background-color: ${e=>e.$active?"#40b4f7":"#bbb"};
  padding: 1px 3px;
  align-self: start;
  color: white;
`,Y=i.div`
  display: flex;
  margin-right: auto;
  font-family: "Open Sans", sans-serif;
  font-size: 14px;
  width: 100%;
  flex-wrap: wrap;
  justify-content: center;
  margin: 10px 0px;
`,g=c(e=>{const{trio:s,imageResolver:m}=e,{variation:v,substance:f,style:k,domain:y}=s,[S,w]=o.useState(v),[P,D]=o.useState(0),[C,x]=o.useState(!1),[B,q]=o.useState([""]);return n(J,{children:C?d(G,{children:[n(Q,{name:"embed",domain:y,substance:f,style:k,variation:S,interactive:!1,animate:!0,stepSize:5,imageResolver:m,onFrame:t=>{q(t.optStages),D(t.currentStageIndex)}}),d(Y,{children:[B.map((t,L)=>{const u=t===""?"default":t;return L===P?n(b,{$active:!0,children:u}):n(b,{children:u})}),n("div",{onClick:()=>w(Math.random().toString()),style:{cursor:"pointer"},children:n(z,{size:28,color:"black"})})]})]}):d("div",{style:{display:"flex",alignItems:"center",justifyContent:"center"},children:[n(V,{onClick:()=>x(!0),children:"Click to lay out the diagram"}),n(N,{width:350,color:"#0001"})]})})},"StagedDiagram$1");try{StagedDiagram.displayName="StagedDiagram",StagedDiagram.__docgenInfo={description:"",displayName:"StagedDiagram",props:{trio:{defaultValue:null,description:"",name:"trio",required:!0,type:{name:"{ substance: string; domain: string; style: string; variation: string; }"}},imageResolver:{defaultValue:null,description:"",name:"imageResolver",required:!0,type:{name:"PathResolver"}}}}}catch{}const hn={parameters:{storySource:{source:`import vector from "@penrose/examples/dist/exterior-algebra/vector-wedge.trio";
import geometry from "@penrose/examples/dist/geometry-domain/textbook_problems/c05p13.trio";
import laplace from "@penrose/examples/dist/walk-on-spheres/laplace-estimator.trio.js";

import { ComponentMeta, ComponentStory } from "@storybook/react";
import StagedDiagram from "../StagedDiagram";

export default {
  title: "Example/StagedDiagram Component",
  component: StagedDiagram,
} as ComponentMeta<typeof StagedDiagram>;

const Template: ComponentStory<typeof StagedDiagram> = (args) => (
  <div style={{ width: "50%" }}>
    <StagedDiagram {...args} />
  </div>
);

export const WalkOnSphere = Template.bind({});
WalkOnSphere.args = {
  trio: {
    substance: laplace.substance,
    style: laplace.style[0].contents,
    domain: laplace.domain,
    variation: "test3",
  },
  imageResolver: laplace.style[0].resolver,
};
export const Geometry = Template.bind({});
Geometry.args = {
  trio: {
    substance: geometry.substance,
    style: geometry.style[0].contents,
    domain: geometry.domain,
    variation: "test",
  },
  imageResolver: geometry.style[0].resolver,
};
export const Vector = Template.bind({});
Vector.args = {
  trio: {
    substance: vector.substance,
    style: vector.style[0].contents,
    domain: vector.domain,
    variation: "test",
  },
  imageResolver: vector.style[0].resolver,
};
`,locationsMap:{"walk-on-sphere":{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}},geometry:{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}},vector:{startLoc:{col:55,line:13},endLoc:{col:1,line:17},startBody:{col:55,line:13},endBody:{col:1,line:17}}}}},title:"Example/StagedDiagram Component",component:g},h=c(e=>n("div",{style:{width:"50%"},children:n(g,{...e})}),"Template"),Z=h.bind({});Z.args={trio:{substance:l.substance,style:l.style[0].contents,domain:l.domain,variation:"test3"},imageResolver:l.style[0].resolver};const X=h.bind({});X.args={trio:{substance:r.substance,style:r.style[0].contents,domain:r.domain,variation:"test"},imageResolver:r.style[0].resolver};const nn=h.bind({});nn.args={trio:{substance:a.substance,style:a.style[0].contents,domain:a.domain,variation:"test"},imageResolver:a.style[0].resolver};const un=["WalkOnSphere","Geometry","Vector"];export{X as Geometry,nn as Vector,Z as WalkOnSphere,un as __namedExportsOrder,hn as default};
//# sourceMappingURL=StagedDiagram.stories-295aee5b.js.map
