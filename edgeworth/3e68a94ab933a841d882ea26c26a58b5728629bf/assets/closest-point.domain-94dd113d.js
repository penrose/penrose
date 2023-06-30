import{a as n}from"./index-d0dd7cba.js";const s=n("geometric-queries/closest-point"),o=`canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Point x {
   vec2 x.pos = (?,?)
   shape x.icon = Circle {
      center: x.pos
      r: 5
      strokeWidth: 1
      fillColor: #000000ff
   } 
}

forall Line L {
   shape L.icon = Line {
      start: (?,?)
      end: (?,?)
      strokeWidth: 5
   }
}

forall Rectangle R {
   shape R.icon = Rectangle {
      center: (?, ?)
      width: ?
      height: ?
      strokeWidth: 1
   }

   ensure R.icon.width > 50
   ensure R.icon.height > 50
}

forall Polyline P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   vec2 p3 = (?,?)
   vec2 p4 = (?,?)
   
   shape P.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: "round"
      strokeColor: sampleColor(1.0,"rgb")
   }

   scalar L0 = norm(p1-p0)
   scalar L1 = norm(p2-p1)
   scalar L2 = norm(p3-p2)
   scalar L3 = norm(p4-p3)
   ensure L0 > 30
   ensure L1 > 30
   ensure L2 > 30
   ensure L3 > 30
   ensure L0 < 80
   ensure L1 < 80
   ensure L2 < 80
   ensure L3 < 80

   scalar theta0 = angleBetween(p1-p0,p2-p1)
   scalar theta1 = angleBetween(p2-p1,p3-p2)
   scalar theta2 = angleBetween(p3-p2,p4-p3)
   ensure theta0 > MathPI()/6
   ensure theta1 > MathPI()/6
   ensure theta2 > MathPI()/6
}

forall Polygon P {
   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   
   shape P.icon = Polygon {
      points: [ p0, p1, p2 ]
   }

   vec2 e01 = p1-p0
   vec2 e12 = p2-p1
   vec2 e20 = p0-p2

   ensure angleBetween( e01, -e12 ) > toRadians(30)
   ensure angleBetween( e12, -e20 ) > toRadians(30)
   ensure angleBetween( e20, -e01 ) > toRadians(30)

   ensure abs(cross2D( e01, -e12 )) > 50
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: ?
   }

   ensure C.icon.r > 50
}

forall Ellipse E {
   shape E.icon = Ellipse {
      rx: ?
      ry: ?
   }

   ensure E.icon.rx > 50
   ensure E.icon.ry > 50
}

forall Group G {

   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)

   shape s1 = Circle {
      r: random(30,70)
   }
   shape s2 = Rectangle {
      width: random(50,150)
      height: random(50,150)
   }
   shape s3 = Polygon {
      points: [p0,p1,p2]
   }

   vec2 u = p1-p0
   vec2 v = p2-p1
   vec2 w = p0-p2
   ensure angleBetween(-u,v) > MathPI()/6
   ensure angleBetween(-v,w) > MathPI()/6
   ensure angleBetween(-w,u) > MathPI()/6

   shape G.icon = Group {
      shapes: [s1,s2,s3]
   }

   shape bbox = Rectangle {
      fillColor: none()
      strokeColor: #ddd
      strokeDasharray: "10 8"
      strokeWidth: 2.0
      center: (0,0)
      width: random(300,500)
      height: random(300,500)
   }
   ensure contains( bbox, s1 )
   ensure contains( bbox, s2 )
   ensure contains( bbox, s3 )
   ensure contains( G.icon, bbox )

}

-- Rules for drawing closest points

forall Point x; Shape S
{
   vec2 p = closestPoint( S.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

forall Point x; Polyline L
{
   vec2 p = closestPoint( L.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }
}

-- Keep shapes from overlapping
forall Shape S1; Shape S2
{
   ensure disjoint( S1.icon, S2.icon )
}

`,r=`type Point

type Shape
type Line <: Shape
type Polyline
type Polygon <: Shape
type Rectangle <: Shape
type Circle <: Shape
type Ellipse <: Shape

type Group <: Shape
`;export{r as d,s as r,o as s};
