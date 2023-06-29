import{m as n}from"./resolver-16fd6907.js";const a=`type Scalar

-- define vectors and bivectors as subtypes of a base k-vector type
type kVector
type Vector <: kVector
type Bivector <: kVector

-- some basic operations on k-vectors
function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector

-- (more can be added here ) --
`,o=n("exterior-algebra"),t=`-- define the size of the drawing
canvas {
   width = 240
   height = 180
}

-- define some colors re-used throughout
Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color clearGray = rgba(0,0,0,.2)
}

Global {

   -- draw a box around the canvas (this box will 
   -- also be used to constrain shapes to the canvas)
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: none()
      strokeColor: Colors.clearGray
      strokeWidth: 1
   }

   -- some additional parameters to get consistent styling throughout
   scalar lineThickness = 1.5
   scalar fontSize = "9px"
   string fontFamily = "Linux Libertine"
}

-- for each Vector declared in the .substance program, this match will
-- get executed once to draw a little widget for that vector, and
-- set up any associated constraints
forall Vector v {

   -- declare a point whose location will be
   -- determined via optimization
   v.p = (?,?)

   -- draw an arrow to p
   v.icon = Line {
      start: (0,0)
      end: v.p
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      strokeLinecap: "round"
      endArrowhead: "straight"
      endArrowheadSize: .5
   }
   -- keep the arrow on the canvas
   ensure contains( Global.box, v.icon)
}

-- draw a label for the vector if it has one
forall Vector v
where v has label {
   v.labelText = Equation {
      string: v.label
      center: v.p + 4.*unit(v.p)
      fillColor: Colors.black
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
   }
   -- keep the label on the canvas
   ensure contains( Global.box, v.labelText )
}

-- draw a bivector that has been defined as the wedge of two
-- vectors as a little parallelogram
forall Bivector w; Vector u; Vector v
where w := Wedge(u,v) {

   -- pick a random brightly-saturated color for this bivector
   -- (these colors will be re-used in subsequent rules, which
   -- why we define them up-front and associate them with w)
   scalar w.hue = ?
   color w.solidColor = hsva( w.hue, 100, 80, 1 )
   color w.clearColor = hsva( w.hue, 100, 80, .2 )

   -- draw a parallelogram with sides u,v
   shape w.icon = Path {
      d: pathFromPoints("closed", [ (0,0), u.p, u.p+v.p, v.p ])
      fillColor: w.clearColor
      strokeColor: none()
   }
   -- keep the parallelogram on the canvas
   ensure contains( Global.box, w.icon )

   -- try to make sure the parallelogram is a reasonable size
   scalar area = abs( cross2D( u.p, v.p ))
   scalar canvasArea = canvas.width * canvas.height
   encourage greaterThan( area, canvasArea/10. )

   -- compute the minimum width of the parallelogram
   -- by projecting each vector onto the unit normal
   -- of the other
   vec2 nu = unit( rot90(u.p) )
   vec2 nv = unit( rot90(v.p) )
   scalar wu = abs( dot( nu, v.p ))
   scalar wv = abs( dot( nv, u.p ))
   scalar minWidth = min( wu, wv )

   -- draw an orientation marker
   scalar w.c = (u.p+v.p)/2. -- center
   scalar R = .75 * minWidth/2. -- radius
   vec2 x0 = w.c + R*unit(u.p-v.p) -- arc start
   vec2 x1 = w.c + R*unit(v.p-u.p) -- arc end
   scalar cw = .5*sign( cross2D(u.p,v.p) ) + .5 -- clockwise (1) or not (0)
   shape w.marker = Path {
      d: arc( "open", x0, x1, (R,R), 0., 0, cw )
      fillColor: none()
      strokeColor: w.solidColor
      strokeWidth: .75*Global.lineThickness
      startArrowhead: "straight"
      startArrowheadSize: .5
   }
}

-- draw a label for the bivector if it has one
forall Bivector w
where w has label {
   w.labelText = Equation {
      string: w.label
      center: w.c -- marker center
      fillColor: w.solidColor
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
   }
}

`,r=`Vector a, b, c
Bivector u := Wedge(a,b)
Bivector v := Wedge(b,c)

AutoLabel All
Label u $a \\wedge b$
Label v $b \\wedge c$
`;export{t as a,a as d,o as r,r as s};
//# sourceMappingURL=vector-wedge.substance-d627cac9.js.map
