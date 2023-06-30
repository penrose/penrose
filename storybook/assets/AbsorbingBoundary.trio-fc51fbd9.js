import{m as n}from"./resolver-33472b42.js";import"./iframe-39c020d4.js";const e=`Point x0, x1
RandomWalk A

startsAt( A, x1 )
endsAt( A, x0 )

Boundary D := AbsorbingBoundary( x0, x1 )

Label D $\\partial \\Omega$
Label x0 $x_k$
Label x1 $\\overline{x}_k$
`,a=n("stochastic-process/epsilon-shell"),r=`canvas {
    width = 100
    height = 100
}

global {
   color labelColor = #000
   string labelSize = "9px"
}

forall Point p {
    vec2 p.x = (?,?) -- location

    shape p.icon = Circle {
        center: p.x
        r: 1
        fillColor: #000
    }
}

forall Point p
where p has label {
   shape p.labelText = Equation {
      string: p.label
      center: p.x + 5*circleRandom()
      fillColor: global.labelColor
      fontSize: global.labelSize
   }
}

forall RandomWalk X {
    vec2 X.start = (?,?) -- start point
    mat2x2 A = .5*((1,0),(0,1)) -- covariance matrix
    vec2 omega = .2*circleRandom() -- drift
    X.curve = diffusionProcess(150, X.start, A, omega)

    shape X.icon = Path {
        d: interpolatingSpline("open", X.curve)
        strokeWidth: .5
        strokeColor: #1b1f8a66
        ensureOnCanvas: false
    }
}

forall RandomWalk X; Point p
where startsAt( X, p ) {
    override X.start = p.x
}

forall RandomWalk X; Point p
where endsAt( X, p ) {
    override p.x = lastPoint(X.curve)
}

forall Boundary B {

   vec2 B.x = (?,?)
   scalar theta = random(0,1) * MathPI()
   vec2 B.e1 = ( cos(theta), sin(theta) )
   vec2 B.e2 = (-sin(theta), cos(theta) )
   scalar w = 2*canvas.width
   scalar h = 2*canvas.width

   vec2 B.p0 = B.x - w*B.e1
   vec2 B.p1 = B.x + w*B.e1
   vec2 B.p2 = B.x + w*B.e1 - h*B.e2
   vec3 B.p3 = B.x - w*B.e1 - h*B.e2

   B.icon = Polygon{
      points: [ B.p0, B.p1, B.p2, B.p3 ]
      ensureOnCanvas: false
      fillColor: #0002
      ensureOnCanvas: false
   }
   B.boundary = Line {
      start: B.p0
      end: B.p1
      fillColor: none()
      strokeColor: #000
      strokeWidth: .5
      ensureOnCanvas: false
   }
}

forall Boundary B; Point a, b, c
where B := ReflectingBoundary( a, b, c ) {

   override B.x = b.x

   override B.e1 = unit(c.x - a.x)
   vec2 ab = b.x - a.x
   override B.e2 = unit(ab - dot(ab, B.e1)*B.e1)

   override B.boundary.strokeStyle = "dashed"
   override B.boundary.strokeDasharray = "2 2"
}

forall Boundary B; Point a, b, c
where a has label; B := ReflectingBoundary( a, b, c ) {
   override a.labelText.center = a.x - 6*B.e2
}
forall Boundary B; Point a, b, c
where b has label; B := ReflectingBoundary( a, b, c ) {
   override a.labelText.center = b.x + 6*B.e2
}
forall Boundary B; Point a, b, c
where c has label; B := ReflectingBoundary( a, b, c ) {
   override c.labelText.center = c.x - 6*B.e2
}

forall Boundary B; Point a, b
where B := AbsorbingBoundary( a, b ) {

   override B.x = b.x

   vec2 u1 = unit( b.x - a.x )
   vec2 u2 = ( -u1[1], u1[0] )

   override B.e1 = u2
   override B.e2 = u1

   scalar eps = norm( b.x - a.x )*1.1
   B.epsilonShell = Polygon {
      points: [ B.p0 - eps*B.e2, B.p1 - eps*B.e2, B.p1, B.p0 ]
      fillColor: #0003
      ensureOnCanvas: false
   }

   scalar markerOffset = 20
   scalar markerPadding = 2
   scalar markerCapWidth = 5
   vec2 m0 = B.x - markerPadding*B.e2 - markerOffset*B.e1
   vec2 m1 = B.x - (eps-markerPadding)*B.e2 - markerOffset*B.e1
   shape epsilonMarker = Line {
      start: m0
      end: m1
      strokeWidth: .5
      strokeColor: #888
   }
   shape epsilonMarkerStart = Line {
      start: m0 - markerCapWidth*B.e1/2
      end: m0 + markerCapWidth*B.e1/2
      strokeWidth: epsilonMarker.strokeWidth
      strokeColor: epsilonMarker.strokeColor
   }
   shape epsilonMarkerEnd = Line {
      start: m1 - markerCapWidth*B.e1/2
      end: m1 + markerCapWidth*B.e1/2
      strokeWidth: epsilonMarker.strokeWidth
      strokeColor: epsilonMarker.strokeColor
   }
   shape markerGroup = Group {
      shapes: [ epsilonMarker, epsilonMarkerStart, epsilonMarkerEnd ]
   }

   shape epsilonLabel = Equation {
      center: (m0+m1)/2 + 3*B.e1
      string: "\\varepsilon"
      fillColor: epsilonMarker.strokeColor
      fontSize: global.labelSize
   }
}

forall Boundary B; Point a, b
where a has label; B := AbsorbingBoundary( a, b ) {
   override a.labelText.center = a.x - 6*B.e2
}
forall Boundary B; Point a, b
where b has label; B := AbsorbingBoundary( a, b ) {
   override b.labelText.center = b.x + 6*B.e2
}

forall Boundary B
where B has label {
   shape B.labelText = Equation {
      string: B.label
      center: B.x + 8*B.e2 + 20*B.e1
      fillColor: global.labelColor
      fontSize: global.labelSize
   }
}

`,o=`type Point
type RandomWalk
type Boundary

predicate startsAt( RandomWalk X, Point p )
predicate endsAt( RandomWalk X, Point p )
constructor ReflectingBoundary( Point a, Point b, Point c ) -> Boundary
constructor AbsorbingBoundary( Point a, Point b ) -> Boundary
`,s={substance:e,style:[{contents:r,resolver:a}],domain:o,variation:"BookstoneShrew61995",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=AbsorbingBoundary.trio-fc51fbd9.js.map
