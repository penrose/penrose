import{m as n}from"./resolver-0c99df2e.js";import"./iframe-b95d2f91.js";const e=`-- For each shape, finds the closest silhouette point relative to the query point x.

Point x
Line L
Polyline M
Polygon P
Rectangle R
Circle C
Ellipse E
-- Group G

`,o=n("geometric-queries/closest-silhouette-point"),t=`canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Point x {
   vec2 x.pos = (0,canvas.height/2 - 10)
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
      strokeColor: sampleColor(1.0, "rgb")
   }
}

forall Polygon P {
   vec2 c = (?,?)
   scalar d = random(100,200)
   scalar u = circleRandom()
   scalar v = rot90(u)
   vec2 p0 = c + d*u
   vec2 p2 = c - d*u
   vec2 p1 = c + random(-150,150)*u + random(60,150)*v
   vec2 p3 = c + random(-150,150)*u - random(60,150)*v

   shape P.icon = Polygon {
      points: [ p0, p1, p2, p3 ]
   }
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: random(30,80)
   }
}

forall Ellipse E {
   shape E.icon = Ellipse {
      center: (0,0)
      rx: random(30,80)
      ry: random(30,80)
   }
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

-- Rules for drawing closest silhouette points

forall Point x; Shape S
{
   vec2 p = closestSilhouettePoint( S.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   scalar D = norm( (canvas.width, canvas.height) )
   shape continuedSegment = Line {
      start: p
      end: p + D*unit(p-x.pos)
      strokeColor: #00000033
      strokeWidth: 2
      ensureOnCanvas: false
      strokeDasharray: "10 8"
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
   vec2 p = closestSilhouettePoint( L.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   scalar D = norm( (canvas.width, canvas.height) )
   shape continuedSegment = Line {
      start: p
      end: p + D*unit(p-x.pos)
      strokeColor: #00000033
      strokeWidth: 2
      ensureOnCanvas: false
      strokeDasharray: "10 8"
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

`,s=`type Point

type Shape
type Line <: Shape
type Polyline
type Polygon <: Shape
type Rectangle <: Shape
type Circle <: Shape
type Ellipse <: Shape

type Group <: Shape
`,p={substance:e,style:[{contents:t,resolver:o}],domain:s,variation:"CatfishDonkey135"};export{p as default};
//# sourceMappingURL=test.trio-437c7972.js.map
