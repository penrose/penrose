import{a as n}from"./index-b40072f4.js";const e=`-- For each shape, draw the result of each geometric query (closest ray
-- intersection, closest point, and closest silhouette point).

-- Circle
Circle C
Point pC
Ray rC
rC := rayFrom(pC)
Point aC := rayIntersect(C,rC)
Point bC := closestPoint(C,pC)
Point cC := closestSilhouettePoint(C,pC)

-- Rectangle
Rectangle R
Point pR
Ray rR
rR := rayFrom(pR)
Point aR := rayIntersect(R,rR)
Point bR := closestPoint(R,pR)
Point cR := closestSilhouettePoint(R,pR)

-- Ellipse
Ellipse E
Point pE
Ray rE
rE := rayFrom(pE)
Point aE := rayIntersect(E,rE)
Point bE := closestPoint(E,pE)
Point cE := closestSilhouettePoint(E,pE)

-- Polygon
Polygon P
Point pP
Ray rP
rP := rayFrom(pP)
Point aP := rayIntersect(P,rP)
Point bP := closestPoint(P,pP)
Point cP := closestSilhouettePoint(P,pP)

-- Line
Line L
Point pL
Ray rL
rL := rayFrom(pL)
Point aL := rayIntersect(L,rL)
Point bL := closestPoint(L,pL)
Point cL := closestSilhouettePoint(L,pL)

-- Polyline
Polyline M
Point pM
Ray rM
rM := rayFrom(pM)
Point aM := rayIntersect(M,rM)
Point bM := closestPoint(M,pM)
Point cM := closestSilhouettePoint(M,pM)

`,o=n("geometric-queries"),r=`canvas {
   width = 800
   height = 700
}

global {
   color shapeColor = #0003
   color closestColor = #66d
   color silhouetteColor = #0a0
   color rayColor = #ff6600
   color labelColor = #000
   color labelStrokeColor = #fff
   color labelStrokeWidth = 6
   color labelFont = "monospace"
   color labelFontSize = "24px"
   scalar labelFontSizeS = 24

   scalar strokeWidth = 2
   scalar arrowSize = 40
   scalar arrowWidth = 2.5
   scalar perpSize = 14

   scalar boxWidth = canvas.width/3
   scalar boxHeight = (canvas.height - 50)/2
   scalar boxPadding = 20
}

legend {
   scalar H = canvas.height
   scalar W = canvas.width
   scalar pad = global.boxPadding

   scalar x0 = -W/2+pad
   scalar y0 = H/2 - pad

   shape queryShape = Rectangle {
      center: (x0,y0)
      width: 30
      height: 30
      fillColor: global.shapeColor
      ensureOnCanvas: false
   }
   shape queryShapeLabel = Text {
      string: "shape"
      center: queryShape.center + (25,0)
      fontSize: "18px"
      fillColor: #000
      ensureOnCanvas: false
      fontWeight: "bold"
      fontFamily: "sans-serif"
      textAnchor: "start"
   }

   scalar x1 = x0 + 110
   shape closestLine = Line {
      start: (x1,y0)
      end: (x1 + 40,y0)
      fill: "none"
      strokeColor: global.closestColor
      strokeWidth: 2*global.strokeWidth
      ensureOnCanvas: false
   }
   shape closestLineLabel = Text {
      string: "closest point"
      center: closestLine.end + (10,0)
      fontSize: "18px"
      fillColor: global.closestColor
      ensureOnCanvas: false
      fontWeight: "bold"
      fontFamily: "sans-serif"
      textAnchor: "start"
   }

   scalar x2 = x1 + 195
   shape silhouetteLine = Line {
      start: (x2,y0)
      end: (x2 + 40,y0)
      fill: "none"
      strokeColor: global.silhouetteColor
      strokeWidth: 2*global.strokeWidth
      ensureOnCanvas: false
   }
   shape silhouetteLineLabel = Text {
      string: "closest silhouette point"
      center: silhouetteLine.end + (10,0)
      fontSize: "18px"
      fillColor: global.silhouetteColor
      ensureOnCanvas: false
      fontWeight: "bold"
      fontFamily: "sans-serif"
      textAnchor: "start"
   }

   scalar x3 = x2 + 290
   shape rayLine = Line {
      start: (x3,y0)
      end: (x3 + 40,y0)
      fill: "none"
      strokeColor: global.rayColor
      strokeWidth: 2*global.strokeWidth
      ensureOnCanvas: false
   }
   shape rayLineLabel = Text {
      string: "ray intersection"
      center: rayLine.end + (10,0)
      fontSize: "18px"
      fillColor: global.rayColor
      ensureOnCanvas: false
      fontWeight: "bold"
      fontFamily: "sans-serif"
      textAnchor: "start"
   }
}

-- Rules for drawing each type of shape

forall Point p {
   vec2 p.x = (?,?) -- location

   shape p.icon = Circle {
      center: p.x
      r: 5
      strokeWidth: 1
      fillColor: #000000ff
      ensureOnCanvas: false
   } 
}

forall Ray r {
   vec2 r.o = (?,?) -- origin
   vec2 r.d = circleRandom() -- direction

   r.icon = Line {
      start: r.o
      end: r.o + global.arrowSize*r.d
      strokeColor: #000
      strokeWidth: global.arrowWidth
      endArrowhead: "straight"
      endArrowheadSize: .5
      ensureOnCanvas: false
   }
}

forall Point p; Ray r
where r := rayFrom(p) {
   override r.o = p.x
}

forall Shape S {
   shape S.box = Rectangle {
      center: (?,?)
      width: global.boxWidth
      height: global.boxHeight
      fillColor: rgba(.9,.9,.9,1)
      strokeColor: #fff
      strokeWidth: 4
      ensureOnCanvas: false
   }

   shape S.boxLabel = Text {
      string: "Shape"
      center: ( S.box.center[0], S.box.center[1] + global.boxHeight/2 - global.labelFontSizeS/2 - global.boxPadding )
      ensureOnCanvas: false
      fillColor: global.labelColor
      fontFamily: global.labelFont
      fontSize: global.labelFontSize
      strokeWidth: global.labelStrokeWidth
      strokeColor: global.labelStrokeColor
      paintOrder: "stroke"
   }
}

forall Line L {
   shape L.icon = Line {
      start: (?,?)
      end: (?,?)
      strokeWidth: 5
      strokeColor: global.shapeColor
      fill: "none"
   }

   override L.boxLabel.string = "Line"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override L.box.center = (-W+w,-H+h)/2 + (w,h)
}

forall Rectangle R {
   shape R.icon = Rectangle {
      center: (?, ?)
      width: random(40,150)
      height: random(40,150)
      strokeWidth: 1
      fillColor: global.shapeColor
   }

   override R.boxLabel.string = "Rectangle"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override R.box.center = (-W+w,-H+h)/2 + (w,0)
}

forall Polyline M {
   vec2 p0 = (?,?)
   vec2 p1 = p0 + 50*circleRandom()
   vec2 p2 = p1 + 50*rotateBy(unit(p1-p0),random(0,1.5))
   vec2 p3 = p2 + 50*rotateBy(unit(p2-p1),random(0,1.5))
   vec2 p4 = p3 + 50*rotateBy(unit(p3-p2),random(-1.5,0))
   
   shape M.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: "round"
      strokeColor: global.shapeColor
   }

   override M.boxLabel.string = "Polyline"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override M.box.center = (-W+w,-H+h)/2 + (2*w,h)
}

forall Polygon P {
   vec2 c = (?,?)
   scalar d = random(50,100)
   scalar u = circleRandom()
   scalar v = rot90(u)
   vec2 p0 = c + d*u
   vec2 p2 = c - d*u
   vec2 p1 = c + random(-50,50)*u + random(30,80)*v
   vec2 p3 = c + random(-50,50)*u - random(30,80)*v

   shape P.icon = Polygon {
      points: [ p0, p1, p2, p3 ]
      fillColor: global.shapeColor
   }

   override P.boxLabel.string = "Polygon"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override P.box.center = (-W+w,-H+h)/2 + (0,h)
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: random(30,80)
      fillColor: global.shapeColor
   }

   override C.boxLabel.string = "Circle"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override C.box.center = (-W+w,-H+h)/2
}

forall Point q; Shape S; Point p
where q := closestPoint(S,p) {
   ensure contains( S.box, p.icon, global.boxPadding )
}

forall Ellipse E {
   shape E.icon = Ellipse {
      center: (?,?)
      rx: random(20,100)
      ry: random(20,100)
      fillColor: global.shapeColor
   }

   override E.boxLabel.string = "Ellipse"
   scalar W = canvas.width
   scalar H = canvas.height
   scalar w = global.boxWidth
   scalar h = global.boxHeight
   override E.box.center = (-W+w,-H+h)/2 + (2*w,0)
}

forall Shape S {
   ensure contains( S.box, S.icon, global.boxPadding )
   override S.icon.ensureOnCanvas = false
}

-- Rules for geometric queries

forall Point q; Shape S; Point p
where q := closestPoint(S,p)
{
   vec2 y = closestPoint( S.icon, p.x )

   override q.x = y
   override q.icon.fillColor = #fff
   override q.icon.strokeColor = global.closestColor
   override q.icon.strokeWidth = global.strokeWidth
   override q.icon.ensureOnCanvas = false

   shape q.querySegment = Line {
      start: p.x
      end: y
      strokeColor: global.closestColor
      strokeWidth: global.strokeWidth
      ensureOnCanvas: false
   }
}

forall Point q; Shape S; Point p
where q := closestSilhouettePoint(S,p)
{
   vec2 y = closestSilhouettePoint( S.icon, p.x )

   override q.x = y
   override q.icon.fillColor = #fff
   override q.icon.strokeColor = global.silhouetteColor
   override q.icon.strokeWidth = global.strokeWidth
   override q.icon.ensureOnCanvas = false

   shape q.querySegment = Line {
      start: p.x
      end: y
      strokeColor: global.silhouetteColor
      strokeWidth: global.strokeWidth
      ensureOnCanvas: false
   }
}

forall Point q; Shape S; Ray r
where q := rayIntersect(S,r)
{
   -- encourage more frequent hits by pointing the ray into the box
   override r.d = unit(S.box.center - r.o)

   vec2 y = rayIntersect( S.icon, r.o, r.d )
   vec2 q.n = rayIntersectNormal( S.icon, r.o, r.d )

   override q.x = y
   override q.icon.fillColor = #fff
   override q.icon.strokeColor = global.rayColor
   override q.icon.strokeWidth = global.strokeWidth
   override q.icon.ensureOnCanvas = false

   shape q.raySegment = Line {
      start: r.o
      end: y
      fill: "none"
      strokeColor: global.rayColor
      strokeWidth: global.strokeWidth
      style: "dashed"
      strokeDasharray: "8,6"
      ensureOnCanvas: false
   }

   shape q.normalArrow = Line {
      start: y
      end: y + global.arrowSize*q.n
      fill: "none"
      strokeColor: global.rayColor
      strokeWidth: global.arrowWidth
      endArrowhead: "straight"
      endArrowheadSize: .5 * norm(q.n)
      ensureOnCanvas: false
   }

   vec2 u = global.perpSize*q.n
   vec2 v = global.perpSize*rot90(q.n)
   vec2 r.h = (0,0)
   shape perpMark = Polyline {
      points: [ y+u+r.h, y+u+v+r.h, y+v+r.h ]
      ensureOnCanvas: false
      fill: "none"
      strokeColor: global.rayColor
      strokeWidth: global.strokeWidth
   }
}

forall Point q; Conic C; Ray r
where q := rayIntersect(C,r) {
   override r.h = -.15*global.perpSize*q.n
}

-- Layering

forall Shape S {
   layer S.icon above S.box
}

forall Point p; Shape S {
   layer p.icon above S.icon
}

forall Ray r; Shape S {
   layer r.icon above S.box
}

forall Point q; Shape S; Point p
where q := closestPoint(S,p) {
   layer p.icon above q.querySegment
   layer q.icon above q.querySegment
}

forall Point q; Shape S; Point p
where q := closestSilhouettePoint(S,p) {
   layer p.icon above q.querySegment
   layer q.icon above q.querySegment
}

forall Point p; Ray r {
   layer p.icon above r.icon
}

forall Point p; Point q; Ray r; Shape S
where q := rayIntersect(S,r) {
   layer q.icon above q.normalArrow
   layer q.icon above q.raySegment
}

forall Point q; Ray r; Shape S
where q := rayIntersect(S,r) {
   layer r.icon above q.raySegment
}

`,a=`type Point
type Ray

type Shape
type Conic <: Shape
type Line <: Shape
type Polyline <: Shape
type Polygon <: Shape
type Rectangle <: Shape
type Circle <: Conic
type Ellipse <: Conic

function rayFrom(Point p) -> Ray r
constructor rayIntersect(Shape S, Ray r) -> Point q
constructor closestPoint(Shape S, Point p) -> Point q
constructor closestSilhouettePoint(Shape S, Point p) -> Point q

`,t={substance:e,style:[{contents:r,resolver:o}],domain:a,variation:"MettwurstCattle35745"};export{t as default};
