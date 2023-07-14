import{m as n}from"./resolver-84c3bc8f.js";import{d as e}from"./geometry.domain-952a3e66.js";import"./iframe-38af56f5.js";const t=`Plane P
Point p, q, r, s
In(p, P)
In(q, P)
In(r, P)
In(s, P)
Let a := Segment(p, q)
Let b := Segment(p, r)
Point m := Midpoint(a)
In(m, P)
Angle theta := InteriorAngle(q, p, r)
Let t := Triangle(p, r, s)
Ray w := Bisector(theta)
Segment h := PerpendicularBisector(a, m)
AutoLabel p, q, r, s, m
Label P $E^2$`,o=n("geometry-domain"),r=`-- Original Style: https://github.com/penrose/penrose/blob/90e88c5a075d6a75698c49c2feb101275ae64af8/examples/geometry-domain/euclidean.sty
-- Original comp. functions: https://github.com/penrose/penrose/blob/90e88c5a075d6a75698c49c2feb101275ae64af8/src/Penrose/Functions.hs

canvas {
  width = 800
  height = 800
}

Colors {
    -- Keenan palette
    black = #000000
    white = #fff
    darkpurple = #8c90c1
    lightpurple = #d0d3e6
    purple2 = rgba(0.106, 0.122, 0.54, 0.2)
    verylightpurple = rgba(0.953, 0.957, 0.977, 1.0)
    purple3 = rgba(0.557, 0.627, 0.769, 1.0)
    midnightblue = rgba(0.14, 0.16, 0.52, 1.0)
    none = none()
}

const {
    pi = 3.14159
    arrowheadSize = 0.65
    strokeWidth = 1.75
    textPadding = 7.0
    textPadding2 = 40.0
    repelWeight = 0.7 -- TODO: Reverted from 0.0
    repelWeight2 = 0.5
    fontSize = "35px"
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

--------------------------------------------------------------------------------
-- START of "euclidean.style"

--Plane
forall Plane p {
  width = canvas.width - 1 
  height = canvas.height - 1
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
  p.x = ?
  p.y = ?
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
  }
  ensure equal(signedDistance(p.text, p.vec), const.textPadding + const.pointRadius)
}

-- default: if \`Point\` is not on a \`Plane\`, the point should be below the plane to stay hidden
forall Point p
with Plane P {
  p.iconOnPlane = ensure disjoint(p.icon, P.icon)
  p.textOnPlane = ensure disjoint(p.text, P.icon)
}

forall Point p
with Plane P
where In(p, P) {
  -- TODO: the problem is that this ensures the padding is const? Or is > padding okay?
  -- There's a choice of whether to put padding on the point or the text for containment
  override p.iconOnPlane = ensure contains(P.icon, p.icon, const.containPadding)
  override p.textOnPlane = ensure contains(P.icon, p.text, 0.0)

  p.icon above P.icon
  p.text above P.icon
}

forall Point p, q, r
where Collinear(p, q, r) {
  ensure collinear(p.icon.center, q.icon.center, r.icon.center)
  -- HACK: make sure q is between p and r
  -- TODO: should this be in Substance instead?
  ensure inRange(q.icon.center[0], p.icon.center[0], r.icon.center[0])
  ensure inRange(q.icon.center[1], p.icon.center[1], r.icon.center[1])
  encourage notTooClose(p.icon, q.icon, const.repelWeight)
  encourage notTooClose(q.icon, r.icon, const.repelWeight)
}

forall Point p
with Linelike l
where On(p, l) {
  ensure equal(signedDistance(l.icon, p.vec), 0)
}

forall Point p
with Linelike l
where On(p, l); p has label {
  ensure disjoint(l.icon, p.text)
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
  ensure disjoint(r.icon, base.text)
  ensure disjoint(r.icon, direction.text)
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
  ensure lessThan(const.minLineLength, norm(l.vec))

  -- labeling
  ensure disjoint(l.icon, p.text)
  ensure disjoint(l.icon, q.text)
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
  ensure equal(10000, dot(normalize(l1.vec), normalize(l2.vec)) * 10000)
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
  ensure lessThan(const.minSegmentLength, norm(e.vec))

  -- labeling
  ensure disjoint(p.text, e.icon)
  ensure disjoint(q.text, e.icon)
}

forall Segment e; Plane p {
  e.icon above p.icon
}

forall Linelike s, t
where EqualLength(s, t) {
  ensure equal(vdist(s.icon.start, s.icon.end), vdist(t.icon.start, t.icon.end))
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

-- HACK: since we cannot handle transitive predicates and don't allow recursive expressions, we redeclare the shapes so the tick counts are the same
forall Linelike s, t, u
where EqualLengthMarker(s, t) as e1; EqualLengthMarker(t, u) as e2 {
  minEquivGroup = min(e1.equivGroup, e2.equivGroup)
  -- minEquivGroup = e2.equivGroup
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
  encourage nonDegenerateAngle(p.icon, q.icon, r.icon)
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
where EqualAngleMarker(a, b) {
  --find points from p->q, then q->r for each vector. draw vectors for each
  startA = ptOnLine(a.q, a.p, a.radius)
  endA = ptOnLine(a.q, a.r, a.radius)
  sweepA = arcSweepFlag(a.q, startA, endA)

  startB = ptOnLine(b.q, b.p, b.radius)
  endB = ptOnLine(b.q, b.r, b.radius)
  sweepB = arcSweepFlag(b.q, startB, endB)

  spacing = 10

  override a.mark = Path {
    d : repeatedArcs(startA, endA, a.p, a.r, (a.radius, a.radius), match_id, spacing, sweepA)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
 }
  override b.mark = Path {
    d : repeatedArcs(startB, endB, b.p, b.r, (a.radius, a.radius), match_id, spacing, sweepB)
    strokeWidth : 2.0
    strokeColor : Colors.black
    fillColor: Colors.none
 }
}

forall Angle a, b
where EqualAngle(a, b) {
  -- make sure angle a is equal to angle b
  -- HACK: increase the magnitude of angles
  weight = 100
  angleA = angleBetween(a.p - a.q, a.r - a.q) * weight
  angleB = angleBetween(b.p - b.q, b.r - b.q) * weight
  ensure equal(angleA, angleB) 
}

forall Angle a
where RightUnmarked(a) {
  -- ensure perpendicular(a.p, a.q, a.r)
  vec2 u = a.p - a.q
  vec2 v = a.r - a.q
  ensure equal(0, dot(u, v))
}

forall Angle a
where RightMarked(a) {
  --render half square path of size a.radius
  markSize = 10
  override a.mark = Path {
    d : pathFromPoints("open", [ptOnLine(a.q, a.p, markSize), innerPointOffset(a.q, a.p, a.r, markSize), ptOnLine(a.q, a.r, markSize)])
    strokeWidth : 2.0
    strokeColor : #000
    fillColor : Colors.none
  }
  vec2 u = a.p - a.q
  vec2 v = a.r - a.q
  ensure equal(0, dot(u, v))
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
  }
  ensure lessThan(const.minSegmentLength, length(t.PQ))
  ensure lessThan(const.minSegmentLength, length(t.QR))
  ensure lessThan(const.minSegmentLength, length(t.RP))
  ensure disjoint(t.icon, p.text)
  ensure disjoint(t.icon, q.text)
  ensure disjoint(t.icon, r.text)
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
  ensure equal(norm(T.PQ.start - p.vec), T.icon.r)
  ensure equal(norm(T.QR.start - p.vec), T.icon.r)
  ensure equal(norm(T.RP.start - p.vec), T.icon.r)
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
  ensure equal(vdist(p.icon.center, q.icon.center), vdist(r.icon.center, s.icon.center))
  ensure equal(vdist(p.icon.center, s.icon.center), vdist(q.icon.center, r.icon.center))

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
  ensure disjoint(p.text, Q.labelContainer)
  ensure disjoint(q.text, Q.labelContainer)
  ensure disjoint(r.text, Q.labelContainer)
  ensure disjoint(s.text, Q.labelContainer)
  -- ensure all sides are visible
  ensure lessThan(const.minSegmentLength, norm(Q.p - Q.q))
  ensure lessThan(const.minSegmentLength, norm(Q.q - Q.r))
  ensure lessThan(const.minSegmentLength, norm(Q.r - Q.s))
  ensure lessThan(const.minSegmentLength, norm(Q.s - Q.p))
  ensure lessThan(const.minSegmentLength, norm(Q.r - Q.p))
  ensure lessThan(const.minSegmentLength, norm(Q.s - Q.q))
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
  ensure disjoint(p.text, l.icon)
}

-- TODO sometimes bisector becomes a scaled version of either side length PQ or QR of angle PQR
forall Linelike s
where AngleBisector(a, s)
with Angle a; Point p {
  weight = 10000
  angleA = angleBetween(a.p - a.q, s.end - a.q) * weight
  angleB = angleBetween(s.end - a.q, a.r - a.q) * weight
  ensure equal(angleA, angleB) 
  -- HACK: make sure the bisector end point is between angle end points. Might be too specific
  ensure inRange(s.end[0], a.p[0], a.r[0])
  ensure inRange(s.end[1], a.p[1], a.r[1])
  -- FIXME: NaN when using the following
  -- v1 = normalize(a.p - a.q) 
  -- v2 = normalize(s.end - a.q)
  -- v3 = normalize(a.r - a.q)
  -- angleA = angleFrom(v1, v2) * weight
  -- angleB = angleFrom(v2, v3) * weight

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
  ensure equal(norm(p.vec - c.vec), c.radius)
  ensure equal(norm(q.vec - c.vec), c.radius)
}

forall Segment s
where s := Chord(c, p, q); p has label; q has label 
with Circle c; Point p, q {
  ensure disjoint(c.icon, p.text)
  ensure disjoint(c.icon, q.text)
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
  ensure equal(norm(c.vec - p.vec), c.radius)
}

forall Segment s
where s := Radius(c, p); p has label
with Circle c; Point p {
  ensure disjoint(p.text, c.icon)
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
  ensure equal(norm(midpoint(p.vec, q.vec) - c.icon.center), 0)
  ensure equal(c.icon.r * 2, norm(s.vec))
}

forall Point p
where OnCircle(c, p)
with Circle c {
  ensure equal(norm(c.vec - p.vec), c.radius)
}

forall Point p
where OnCircle(c, p); p has label
with Circle c {
  ensure disjoint(p.text, c.icon)
}

forall Point p
where CircleCenter(c, p)
with Circle c {
  override p.vec = c.vec
}
-- END of "euclidean.style"
--------------------------------------------------------------------------------

--Plane
forall Plane p {
  dim = 700.0
  override p.text.center = ((dim / 2.0) - const.textPadding2, (dim / 2.0) - const.textPadding2)
  -- inner: 
  override p.icon = Rectangle {
    fillColor : #f3f4f9
    strokeColor : #8e93c4
    strokeWidth : 2.0
    center : (0.0, 0.0)
    width : dim
    height : dim
  }

  p.text above p.icon
}

forall Triangle t
where t := Triangle(p, q, r)
with Point p; Point q; Point r {
  t.strokeWidth = 0.0
  override t.PQ = Line {
    start : p.icon.center
    end : q.icon.center
    strokeWidth : t.strokeWidth
  }
  override t.QR = Line {
    start : q.icon.center
    end : r.icon.center
    strokeWidth : t.strokeWidth
  }
  override t.RP = Line {
    start : r.icon.center
    end : p.icon.center
    strokeWidth : t.strokeWidth
  }
  override t.icon = Path {
    d : pathFromPoints("closed", [p.icon.center, r.icon.center, q.icon.center])
    fillColor: Colors.purple2
    strokeWidth: 0
  }
  ensure lessThan(const.minSegmentLength, length(t.PQ))
  ensure lessThan(const.minSegmentLength, length(t.QR))
  ensure lessThan(const.minSegmentLength, length(t.RP))
  ensure disjoint(t.icon, p.text)
  ensure disjoint(t.icon, q.text)
  ensure disjoint(t.icon, r.text)
  t.PQ below p.icon
  t.PQ below q.icon
  t.QR below q.icon
  t.QR below r.icon
  t.RP below r.icon
  t.RP below p.icon
}

--Angle
forall Angle theta
where theta := InteriorAngle(p, q, r)
with Point p; Point q; Point r {
  override theta.p = p.vec
  override theta.q = q.vec
  override theta.r = r.vec

  override theta.radius = const.thetaRadius
  encourage nonDegenerateAngle(p.icon, q.icon, r.icon)
  startA = ptOnLine(theta.q, theta.p, theta.radius)
  endA = ptOnLine(theta.q, theta.r, theta.radius)
  sweepA = arcSweepFlag(theta.q, startA, endA)
  theta.mark = Path {
    d : arc("open", startA, endA, (theta.radius, theta.radius), 0, 0, sweepA)
    strokeWidth : 2.0
    strokeColor : Colors.darkpurple
    -- fillColor : #ffffff40
  }
  theta.mark below theta.side1, theta.side2
}

forall Segment s
where s := PerpendicularBisector(s2, p)
with Segment s2; Point p {
  override s.icon = Line {
    end : p.icon.center
    startArrowhead: "straight"
    startArrowheadSize: const.arrowheadSize
    strokeColor : Colors.darkpurple
    strokeWidth : const.strokeWidth
  }
  startA = ptOnLine(s.icon.end, s.icon.start, const.thetaRadius)
  endA = ptOnLine(s.icon.end, s2.icon.end, const.thetaRadius)
  sweepA = arcSweepFlag(s.icon.end, startA, endA)

  markSize = 10
  override s.mark = Path {
    d : pathFromPoints("open", [ptOnLine(s.icon.end, s.icon.start, markSize), innerPointOffset(s.icon.end, s.icon.start, s2.icon.end, markSize), ptOnLine(s.icon.end, s2.icon.end, markSize)])
    strokeWidth : 2.0
    strokeColor : Colors.black
  }

  ensure disjoint(s.icon, p.text)
  ensure perpendicular(s.icon.start, s.icon.end, s2.icon.end)
  ensure equal(norm(s.icon.end - s.icon.start), const.rayLength)
}

forall Point p
where p := Midpoint(l)
with Linelike l {
  override p.vec = midpoint(l.icon.start, l.icon.end)
  override p.icon.fillColor = Colors.white
  override p.icon.strokeColor = Colors.black
  override p.icon.strokeWidth = 1
  layer p.icon above l.icon
  ensure disjoint(l.icon, p.text)
}

forall Ray r
with Angle theta
where r := Bisector(theta) {
  override r.icon.strokeColor = Colors.darkpurple
}
`,a={substance:t,style:[{contents:r,resolver:o}],domain:e,variation:"StromboliCat61816",excludeWarnings:["BBoxApproximationWarning"]};export{a as default};
//# sourceMappingURL=siggraph-teaser.trio-8586c499.js.map
