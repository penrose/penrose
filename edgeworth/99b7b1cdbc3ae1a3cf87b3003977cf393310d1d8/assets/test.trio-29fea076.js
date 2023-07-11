import{a as n}from"./index-3083d8fe.js";import{d as e}from"./ray-intersect.domain-6b8fb535.js";const r=`-- For each ray, finds the first intersection with each shape.

Ray r1
Ray r2
Ray r3
Ray r4
Ray r5

Ellipse E1
Ellipse E2
Ellipse E3
Circle C1
Circle C2
Circle C3
Polygon P1
Polygon P2
Polygon P3
Rectangle R1
Rectangle R2
Rectangle R3
Line L1
Line L2
Line L3
Line L4
Line L5
Polyline M
Group G1
Group G2
Group G3

`,a=n("geometric-queries/ray-intersect"),o=`canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Ray r {
   scalar W = canvas.width
   scalar H = canvas.height
   scalar x = -W/2 + 10
   scalar y = .95*H*random(-1,1)/2
   vec2 r.pos = (x,y)
   vec2 r.vec = (1000,?)

   shape r.icon = Circle {
      center: r.pos
      r: 5
      fillColor: #000000ff
      ensureOnCanvas: false
   } 
   shape r.arrow = Line {
      start: r.pos
      end: r.pos + 75*unit(r.vec)
      strokeWidth: 3
      strokeColor: #000
      endArrowhead: "straight"
      endArrowheadSize: .5
      ensureOnCanvas: false
   } 
   shape r.line = Line {
      start: r.pos
      end: r.pos + norm((canvas.width,canvas.height))*unit(r.vec)
      strokeColor: #000
      strokeWidth: 1.5
      style: "dashed"
      strokeDasharray: "8,6"
      ensureOnCanvas: false
   }

   layer r.arrow above r.line
   layer r.icon above r.arrow
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
      width: random(30,80)
      height: random(30,80)
      strokeWidth: 1
   }
}

forall Polyline P {
   scalar w = canvas.width/2
   scalar h = .9*canvas.height/2
   vec2 p0 = (w - random(0,100),h)
   vec2 p1 = (w - random(0,100),h/2)
   vec2 p2 = (w - random(0,100),0)
   vec2 p3 = (w - random(0,100),-h/2)
   vec2 p4 = (w - random(0,100),-h)
   
   shape P.icon = Polyline {
      points: [ p0, p1, p2, p3, p4 ]
      strokeWidth: 5.0
      strokeLinejoin: "round"
      strokeColor: sampleColor(1.0,"rgb")
      ensureOnCanvas: false
   }
}

forall Polygon P {

   vec2 c = (?,?)
   scalar t0 = 0*MathPI()/3 + random(-1,1)
   scalar t1 = 2*MathPI()/3 + random(-1,1)
   scalar t2 = 4*MathPI()/3 + random(-1,1)
   scalar r0 = 60 + random(-5,5)
   scalar r1 = 60 + random(-5,5)
   scalar r2 = 60 + random(-5,5)

   vec2 p0 = c + r0*(cos(t0),sin(t0))
   vec2 p1 = c + r1*(cos(t1),sin(t1))
   vec2 p2 = c + r2*(cos(t2),sin(t2))
   
   shape P.icon = Polygon {
      points: [ p0, p1, p2 ]
   }
}

forall Circle C {
   shape C.icon = Circle {
      center: (?,?)
      r: random(40,80)
   }
}

forall Ellipse E {
   shape E.icon = Ellipse {
      rx: random(5,80)
      ry: random(5,80)
   }
}

forall Group G {
   shape c1 = Circle {
      center: (?,?)
      r: random(20,40)
      fillColor: sampleColor(1,"rgb")
      ensureOnCanvas: false
   }

   shape c2 = Circle {
      center: c1.center + 40*diskRandom()
      r: random(20,40)
      fillColor: c1.fillColor
      ensureOnCanvas: false
   }

   shape c3 = Circle {
      center: c1.center + 40*diskRandom()
      r: random(20,40)
      fillColor: c1.fillColor
      ensureOnCanvas: false
   }

   shape c4 = Circle {
      center: c1.center + 40*diskRandom()
      r: random(20,40)
      fillColor: c1.fillColor
      ensureOnCanvas: false
   }

   shape G.icon = Group {
      shapes: [c1,c2,c3,c4]
   }
}

-- Rules for drawing ray intersections

forall Ray r; Shape S
{
   vec2 p = rayIntersect( S.icon, r.pos, r.vec )
   vec2 n = rayIntersectNormal( S.icon, r.pos, r.vec )

   shape intersectDot = Circle {
      r: r.icon.r * norm(n)
      center: p
      fillColor: #ffffffff
      strokeColor: #000
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape intersectNormal = Line {
      start: p
      end: p + 30*n
      strokeColor: #888
      strokeWidth: 2
      endArrowhead: "straight"
      endArrowheadSize: .5 * norm(n)
      ensureOnCanvas: false
   }

   layer intersectDot above r.arrow
   layer intersectDot below r.icon
   layer intersectDot above S.icon
   layer intersectNormal below intersectDot
}

forall Ray r; Ellipse E
{
   vec2 p = rayIntersect( E.icon, r.pos, r.vec )
   vec2 n = rayIntersectNormal( E.icon, r.pos, r.vec )

   shape intersectDot = Circle {
      r: r.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape intersectNormal = Line {
      start: p
      end: p + 30*n
      strokeColor: #888
      strokeWidth: 2
      endArrowhead: "straight"
      endArrowheadSize: .5
      ensureOnCanvas: false
   }

   layer intersectDot above r.arrow
   layer intersectDot below r.icon
   layer intersectDot above E.icon
   layer intersectNormal below intersectDot
}

forall Ray r; Polyline M
{
   vec2 p = rayIntersect( M.icon, r.pos, r.vec )
   vec2 n = rayIntersectNormal( M.icon, r.pos, r.vec )

   shape intersectDot = Circle {
      r: r.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
      ensureOnCanvas: false
   }

   shape intersectNormal = Line {
      start: p
      end: p + 30*n
      strokeColor: #888
      strokeWidth: 2
      endArrowhead: "straight"
      endArrowheadSize: .5
      ensureOnCanvas: false
   }

   layer intersectDot above r.arrow
   layer intersectDot below r.icon
   layer intersectDot above M.icon
   layer intersectNormal below intersectDot
}

forall Shape S; Ray r
{
   layer r.icon above S.icon
}

-- Keep shapes from overlapping
forall Shape S1; Shape S2
{
   ensure disjoint( S1.icon, S2.icon )
}

`,l={substance:r,style:[{contents:o,resolver:a}],domain:e,variation:"SnowflakeQuetzal3561",excludeWarnings:[]};export{l as default};
