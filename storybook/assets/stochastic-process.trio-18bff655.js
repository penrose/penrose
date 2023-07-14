import{m as n}from"./resolver-b9429209.js";import"./iframe-8d1c39a4.js";const e=`Point X0
StochasticProcess Xt
startsAt( Xt, X0 )
Point XT := endPoint( Xt )
-- Let s := Step( X0, XT )

Label Xt $X_t$
Label X0 $x_k$
Label XT $x_{k+1}$

`,a=n("stochastic-process"),l=`canvas {
   width = 288
   height = 162

   shape background = Rectangle {
      center: (0,0)
      width: width
      height: height
      fillColor: #0002
      ensureOnCanvas: false
   }
}

forall Point p {
   vec2 p.x = (?,?)

   shape p.icon = Circle {
      center: p.x
      r: 2
      fillColor: #000
   }
}

forall Point p
where p has label {

   scalar theta = ?
   vec2 c = p.x + 10*(cos(theta),sin(theta))

   p.labelText = Equation {
      center: c
      string: p.label
      fillColor: #000
      fontSize: "12px"
   }
}

forall StochasticProcess X {

   vec2 X.start = (0,0)
   mat2x2 A = 3*( (1,0), (0,1) )
   vec2 omega = .5*circleRandom()

   X.curve = diffusionProcess( 190, X.start, A, omega )

   shape X.path = Path {
      d: interpolatingSpline( "open", X.curve )
      strokeColor: #1b1f8a99
      strokeWidth: 1
   }

   shape X.ball = Circle {
      center: X.start
      r: norm( X.start - lastPoint(X.curve) )
      fillColor: #1b1f8a33
      strokeColor: #888
      strokeWidth: 1.5
      ensureOnCanvas: false
   }

   shape X.shading = Image {
      href: "ball-shading.svg"
      center: X.ball.center
      width: 2*X.ball.r
      height: 2*X.ball.r
      opacity: .75
      ensureOnCanvas: false
   }

   shape X.canvasRect = Rectangle {
      center: canvas.background.center
      width: canvas.background.width
      height: canvas.background.height
   }
   shape X.clippedBall = Group {
      shapes: [ X.ball, X.shading ]
      clipPath: clip( X.canvasRect )
      ensureOnCanvas: false
   }

   shape X.clipBall = Circle {
      center: X.ball.center
      r: X.ball.r
      ensureOnCanvas: false
   }
   shape X.clippedPath = Group {
      shapes: [ X.path ]
      clipPath: clip( X.clipBall )
      ensureOnCanvas: false
   }

   layer X.ball above X.shading
   layer X.path above X.ball
}

forall StochasticProcess X
where X has label {

   shape X.labelText = Equation {
      string: X.label
      center: averagePoint( X.curve )
      fillColor: #fff
      fontSize: "12px"
      ensureOnCanvas: false
   }

   -- Fake equation stroking by "stamping"
   -- many copies of the label
   scalar a = .5
   shape X.labelStroke0 = Equation {
      string: X.label
      center: X.labelText.center + a*(1,0)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke1 = Equation {
      string: X.label
      center: X.labelText.center + a*(1,1)/sqrt(2)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke2 = Equation {
      string: X.label
      center: X.labelText.center + a*(0,1)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke3 = Equation {
      string: X.label
      center: X.labelText.center + a*(-1,1)/sqrt(2)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke4 = Equation {
      string: X.label
      center: X.labelText.center + a*(-1,0)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke5 = Equation {
      string: X.label
      center: X.labelText.center + a*(-1,-1)/sqrt(2)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke6 = Equation {
      string: X.label
      center: X.labelText.center + a*(0,-1)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }
   shape X.labelStroke7 = Equation {
      string: X.label
      center: X.labelText.center + a*(1,-1)/sqrt(2)
      fillColor: #000
      fontSize: "12px"
      ensureOnCanvas: false
   }

   layer X.labelText above X.labelStroke0
   layer X.labelText above X.labelStroke1
   layer X.labelText above X.labelStroke2
   layer X.labelText above X.labelStroke3
   layer X.labelText above X.labelStroke4
   layer X.labelText above X.labelStroke5
   layer X.labelText above X.labelStroke6
   layer X.labelText above X.labelStroke7
   layer X.labelText above X.path
}

forall Point p; StochasticProcess X {
   layer p.icon above X.path
}

forall Point p; StochasticProcess X
where p has label
{
   ensure disjoint( p.labelText, X.path )
}

forall Point p; StochasticProcess X
where startsAt( X, p ) {
   override X.start = p.x
}

forall Point p; StochasticProcess X
where p := endPoint( X ) {
   override p.x = lastPoint( X.curve )
}

forall Step s; Point p0, p1
where s := Step( p0, p1 ) {

   vec2 a = p0.x
   vec2 b = p1.x
   vec2 m = (a+b)/2
   scalar l  = norm(b-a)
   vec2 n = rot90((b-a)/l)
   vec2 c = m + l*n/5

   shape s.icon = Path {
      d: quadraticCurveFromPoints( "open", [a, c, b] )
      fillColor: none()
      strokeColor: #1b1f8a
      strokeWidth: 1.5
      ensureOnCanvas: false
   }
   vec2 u = unit(b-c)
   shape s.arrowhead = Line {
      start: b - 6.25*u
      end: b - .5*u
      endArrowhead: "straight"
      endArrowheadSize: .5
      fillColor: none()
      strokeColor: #1b1f8a
      strokeWidth: 1.5
      ensureOnCanvas: false
   }

   layer s.arrowhead above s.icon
   layer p1.icon above s.arrowhead
   layer p0.icon above s.arrowhead
}

`,t=`type StochasticProcess
type Point
type Step

constructor Step( Point p0, Point p1 )
predicate startsAt( StochasticProcess X, Point p )
function endPoint( StochasticProcess X ) -> Point

`,s={substance:e,style:[{contents:l,resolver:a}],domain:t,variation:"ChambrayGoose54176",excludeWarnings:["BBoxApproximationWarning"]};export{s as default};
//# sourceMappingURL=stochastic-process.trio-18bff655.js.map
