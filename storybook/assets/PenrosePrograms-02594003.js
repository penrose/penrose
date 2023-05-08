import{m as e}from"./index-67a4734b.js";const t="type Object",o=`canvas {
    width = 400
    height = 400
}

layout = [centerX, centerY, shrink]

forall Object o {
    x = ? 
    y = ?
    r = ?
    o.shape = Circle {
      center: (x, y)
      r: r
    }
    o.text = Equation {
        string: o.label
    }
    ensure contains(o.shape, o.text)
    ensure equal(x, 0)
    ensure equal(y, 0) except centerX
    ensure equal(r, 30) in shrink
}`,r=`Object o 
AutoLabel All`,a={"animation.domain":t,"animation.style":o,"center-shrink-circle.substance":r},l=`type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate SBond( Atom a1, Atom a2 )
symmetric predicate WBond( Atom a1, Atom a2 )`,i=`canvas {
    width = 800
    height = 700
}

forall Atom a {
    a.center = (?, ?)
    a.symbol = Equation {
        center: a.center
        string: "Atom"
    }
    a.background = Circle {
        r: 10
        center: a.center
        fillColor: rgba(1., 1., 1., 1.)
    }
    a.shape = Group {
        shapes: [a.symbol, a.background]
    }
    layer a.symbol above a.background
}

forall Hydrogen h {
    override h.symbol.string = "H"
}

forall Oxygen o {
    override o.symbol.string = "O"
}

forall Atom a1; Atom a2
where SBond(a1, a2) as b {
    b.symbol = Line {
        start: a1.center
        end: a2.center
        strokeColor: rgba(0., 0., 0., 1.)
    }
    b.v = a2.center - a1.center
    ensure equal(norm(b.v), 100)

    layer b.symbol below a1.shape
    layer b.symbol below a2.shape
}

forall Hydrogen h1; Hydrogen h2; Oxygen o
where SBond(o, h1) as b1; SBond(o, h2) as b2 {
    ensure equal(angleBetween(b1.v, b2.v), toRadians(104.5))
}

forall Atom a1; Atom a2
where WBond(a1, a2) as wb {
    wb.symbol = Line {
        start: a1.center
        end: a2.center
        strokeColor: rgba(0., 0., 0., 1)
        strokeWidth: 5
        strokeStyle: "dashed"
    }
    wb.v = a2.center - a1.center
    ensure equal(norm(wb.v), 200)
    layer wb.symbol below a1.shape
    layer wb.symbol below a2.shape
}

forall Hydrogen h; Oxygen o; Hydrogen h1; Hydrogen h2; Oxygen o1
where WBond(h, o); SBond(h, o1); SBond(o, h1); SBond(o, h2) {
    ensure collinearOrdered(o.symbol.center, h.symbol.center, o1.symbol.center)
    ensure collinearOrdered(midpoint(h1.symbol.center, h2.symbol.center), o.symbol.center, h.symbol.center)
}`,s=`Hydrogen H1, H2
Oxygen O1
SBond(H1, O1)
SBond(H2, O1)`,c=`-- center
Hydrogen H1, H2
Oxygen O1
SBond(H1, O1)
SBond(H2, O1)

Hydrogen H3, H4
Oxygen O2
SBond(H3, O2)
SBond(H4, O2)

Hydrogen H5, H6
Oxygen O3
SBond(H5, O3)
SBond(H6, O3)

Hydrogen H7, H8
Oxygen O4
SBond(H7, O4)
SBond(H8, O4)

WBond(O1, H3)
WBond(H1, O3)
WBond(H2, O4)`,d={"atoms-and-bonds.domain":l,"atoms-and-bonds.style":i,"one-water-molecule.substance":s,"wet-floor.substance":c},p=`type Point
type Rectangle
type Polygon
type Circle
type Ellipse
`,g=`canvas {
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

-- Keep shapes from overlapping

forall Rectangle R; Polygon P
{
   ensure disjoint( R.icon, P.icon )
}

forall Rectangle R; Circle C
{
   ensure disjoint( R.icon, C.icon )
}

forall Rectangle R; Ellipse E
{
   ensure disjoint( R.icon, E.icon )
}

forall Polygon P; Circle C
{
   ensure disjoint( P.icon, C.icon )
}

forall Polygon P; Ellipse E
{
   ensure disjoint( P.icon, E.icon )
}

forall Circle C; Ellipse E
{
   ensure disjoint( C.icon, E.icon )
}

-- Rules for drawing closest points

forall Point x; Rectangle R
{
   vec2 p = closestPoint( R.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
   }
}

forall Point x; Polygon P
{
   vec2 p = closestPoint( P.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
   }
}

forall Point x; Circle C
{
   vec2 p = closestPoint( C.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
   }
}

forall Point x; Ellipse E
{
   vec2 p = closestPoint( E.icon, x.pos )

   shape closestSegment = Line {
      start: x.pos
      end: p
      strokeColor: #000000ff
      strokeWidth: 2
   }

   shape closestDot = Circle {
      r: x.icon.r
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 2
   }
}

`,f=`Point x
Rectangle R
Polygon P
Circle C
Ellipse E
`,u={"closest-point.domain":p,"closest-point.style":g,"test.substance":f},h=`type Point
type Ellipse
type VectorSpace
type Vector
predicate In(Vector, VectorSpace V)
predicate ClosestPoint(Point p, Ellipse e)
`,b=`canvas {
    width = 800
    height = 700
}

const {
  scalar vectorSpaceSize = 350.0
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  scalar arrowThickness = 1.5
  color gray = rgba(0.6, 0.6, 0.6, 1.)
  color lightBlue = rgba(0.2, 0.4, 0.8, 1.0)
  color lightGray = rgba(252, 252, 252, 0.015)
  color green = rgba(0., 0.8, 0., 1.)
  color none = rgba(0., 0., 0., 0.)
}
forall VectorSpace U {
    scalar axisSize = const.vectorSpaceSize / 2.0
    vec2 U.origin = (0., 0.)
    vec2 o = U.origin /* just so we don't need to type U.origin everytime */
    U.axisColor = const.gray

    U.background = Rectangle {
        center : U.origin
        width : const.vectorSpaceSize
        height : const.vectorSpaceSize
        fillColor : const.lightGray
        strokeColor : const.none
    }

    U.xAxis = Line {
        start : (o[0] - axisSize, o[1])
        end : (o[0] + axisSize, o[1])
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: true
        endArrowhead: true
        arrowheadSize : const.arrowheadSize * 2.
    }

    U.yAxis = Line {
        start : (o[0], o[1] - axisSize)
        end : (o[0], o[1] + axisSize)
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: true
        endArrowhead: true
        arrowheadSize : const.arrowheadSize * 2.
    }

    U.text = Equation {
        string : U.label
        center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
        fillColor : U.axisColor
    }
}


forall Point p; Ellipse e1; Ellipse e2
    {
    rxval = ?
    ryval = ?
    e1.shape = Ellipse{
        strokeWidth: 3
        rx: rxval
        ry: ryval
        center: (?,?)        
    }
    e2.shape = Ellipse{
        strokeWidth: 3
        rx: rxval
        ry: ryval
        center: (0,0)        
    }
    ensure inRange(rxval,10, 500)
    ensure inRange(ryval,10, 500)
    
    p.Point = (?, ?)
    p.shape = Circle{
        center: p.Point
        r: 10
        strokeWidth: 1
    } 
    p.line = Line{
        start: p.Point
        end: closestPoint(p.Point, e1.shape)
        strokeWidth: 1.0
    }
}

forall Vector u; VectorSpace U; Point p; Ellipse e
where In(u,U) {

  u.shape = Line {
    start: p.Point - e.shape.center
    end : U.origin + closestPoint( p.Point,e.shape)-e.shape.center
    strokeWidth : 3.0
    strokeColor : const.lightBlue
    endArrowhead : true
    arrowheadSize : const.arrowheadSize
  }

  u.text = Equation {
    string : u.label
    fillColor : u.shape.strokeColor
  }

  ensure contains(U.background, u.shape)
  ensure contains(U.background, u.text)
  ensure atDist(u.shape, u.text, 15.0)
  ensure minSize(u.shape)

  layer u.text above U.xAxis
  layer u.text above U.yAxis
}
`,k=`VectorSpace U
Vector v
Vector w
In(v, U)
In(w, U)
Point p
Ellipse e1
Ellipse e2
`,x={"closest-point-ellipse.domain":h,"closest-point-ellipse.style":b,"closest-point-ellipse.substance":k},v=`canvas {
    width = 600
    height = 400
}

global {
    scalar t = 0.4
}

forall Curve c {
    vec2 c.p1 = (?, ?)
    vec2 c.p2 = (?, ?)
    vec2 c.p3 = (?, ?)
    vec2 c.p4 = (?, ?)

    points = [c.p1, c.p2, c.p3, c.p4]
    shape curve = Path {
        d: cubicCurveFromPoints("open", points)
        strokeWidth: 3.5
        ensureOnCanvas: true
    }

    ensure equal(perimeter(points, false), 900)
    ensure equal(signedArea(points, false), 1e5)
}

forall Point p {
    vec2 p.p = (?, ?) 

    shape p.point = Circle {
        center: p.p
        r: 4
        fillColor: rgba(0,0,0,1)
    }

    shape p.text = Equation {
        string: p.label
        center: (?, ?)
    }

    encourage near(p.text, p.point)
}

forall Curve c; Point p1; Point p2; Point p3; Point p4
where c := CurveFromPoints(p1, p2, p3, p4) {
    override p1.p = c.p1
    override p2.p = c.p2
    override p3.p = c.p3
    override p4.p = c.p4
}

forall Point p; Point p1; Point p2 
where p := Lerp(p1, p2) {
    vec2 p3 = global.t * p1.p + (1 - global.t) * p2.p
    override p.p = p3

    shape line = Line {
        start: p1.p
        end: p2.p
        strokeWidth: 2
        strokeColor: rgba(0,0,0,1)
    }

    ensure disjoint(line, p.text, -3)
    ensure disjoint(line, p1.text, -3)
    ensure disjoint(line, p2.text, -3)
}
`,m=`Point p1, p2, p3, p4

Label p1 $P_1$
Label p2 $P_2$
Label p3 $P_3$
Label p4 $P_4$

Curve c := CurveFromPoints( p1, p2, p3, p4 )

Point p12 := Lerp( p1, p2 )
Point p23 := Lerp( p2, p3 )
Point p34 := Lerp( p3, p4 )

Label p12 $P_{12}$
Label p23 $P_{23}$
Label p34 $P_{34}$

Point p123 := Lerp( p12, p23 )
Point p234 := Lerp( p23, p34 )

Label p123 $P_{123}$
Label p234 $P_{234}$

Point p1234 := Lerp( p123, p234 )

Label p1234 $P_{1234}$

Label c $\\gamma$
`,y=`type Curve
type Point

constructor CurveFromPoints( Point p1, Point p2, Point p3, Point p4 ) -> Curve
constructor Lerp( Point p1, Point p2 ) -> Point
`,w=`canvas {
    width = 600
    height = 400
}

forall Curve c {
    vec2 c.p1 = (?, ?)
    vec2 c.p2 = (?, ?)
    vec2 c.p3 = (?, ?)
    vec2 c.p4 = (?, ?)

    points = [c.p1, c.p2, c.p3, c.p4]
    shape curve = Path {
        d: cubicCurveFromPoints("open", points)
        strokeWidth: 2.5
        ensureOnCanvas: true
        strokeColor: rgba(0, 0, 0, 1)
    }
    
    ensure equal(vdist(c.p1, c.p2), vdist(c.p2, c.p3))
    ensure equal(vdist(c.p2, c.p3), vdist(c.p3, c.p4))
    ensure equal(perimeter(points), 500)
}

forall Point p {
    vec2 p.p = (?, ?) 
}


forall Point p; Point p1; Point p2 
where p := Lerp(p1, p2) {
    vec2 p3 = 0.5 * (p1.p + p2.p)
    override p.p = p3
    ensure lessThan(vdist(p1.p, p2.p), 200)
}

forall Curve c; Point p1; Point p2; Point p3; Point p4
where c := CurveFromPoints(p1, p2, p3, p4) {
    override p1.p = c.p1
    override p2.p = c.p2
    override p3.p = c.p3
    override p4.p = c.p4
}
`,C=`Point a1, b1, d1
Point a2, b2, d2

Point c1 := Lerp( b1, d1 )
Point c2 := Lerp( b2, d2 )

Curve curve1 := CurveFromPoints( a1, b1, c1, d1 )
Curve curve2 := CurveFromPoints( a2, b2, c2, d2 )

Point a12 := Lerp( a1, a2 )
Point b12 := Lerp( b1, b2 )
Point c12 := Lerp( c1, c2 )
Point d12 := Lerp( d1, d2 )

Curve curve12 := CurveFromPoints( a12, b12, c12, d12 )

Point a112 := Lerp( a1, a12 )
Point b112 := Lerp( b1, b12 )
Point c112 := Lerp( c1, c12 )
Point d112 := Lerp( d1, d12 )

Curve curve112 := CurveFromPoints( a112, b112, c112, d112 )

Point a122 := Lerp( a12, a2 )
Point b122 := Lerp( b12, b2 )
Point c122 := Lerp( c12, c2 )
Point d122 := Lerp( d12, d2 )

Curve curve122 := CurveFromPoints( a122, b122, c122, d122 )

Label curve1 $\\gamma_1$
Label curve2 $\\gamma_2$
`,L={"cubic-bezier.style":v,"cubic-bezier.substance":m,"curves.domain":y,"homotopy.style":w,"homotopy.substance":C},A=`type Graph
type Variable

predicate not(Graph g1)
predicate and(Graph g1, Graph g2)
function if(Graph, Graph) -> Graph
function or(Graph, Graph) -> Graph

predicate equal(Variable v1, Variable v2)
predicate invisibleGraph(Graph p1)
predicate some(Variable, Graph, Graph)
predicate all(Variable, Graph, Graph)
`,S=`/*Graph a
Graph aInv
invisibleGraph(aInv)
Variable x
some(x, a, aInv)

Graph b
Graph bInv
invisibleGraph(bInv)
Variable y
some(y, b, bInv)

equal(x, y)*/

Graph p
Graph q

Graph r := if(p, q)

Variable x
some(x, p, q)

AutoLabel All`,P=`variable {
    primary = sampleColor(1.0, "rgb")
    secondary = sampleColor(1.0, "rgb")
}

forall Graph A
{
    A.icon = Circle {
        strokeWidth: 0.0
        fillColor: rgba(0.0, 0.0, 0.0, 0.0)
    }

    A.text = Equation{
        string: A.label
    }

    A.level = 0

    ensure minSize(A.icon)
    ensure contains(A.icon, A.text)
    ensure contains(A.icon, A.text)

    A.text above A.icon
}

forall Variable V
{
    V.icon = Path {
        fillColor: rgba(0.0, 0.0, 0.0, 0.0)
        strokeColor: rgba(0.0, 0.0, 0.0, 0.0)
        strokeWidth: 2.0
    }
}



forall Variable v1; Variable v2 where equal(v1,v2) {
    --Overriding (v2.icon.d or v2.icon) has no effect, creating new object
    v2.icon2 = Path {
        d: makePath(v1.start, v2.start, 50, 10)
        fillColor: rgba(0.0, 0.0, 0.0, 0.0)
        strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 2.0
    }

    override v1.icon.strokeWidth = 0.0
    override v2.icon.strokeWidth = 0.0
}



forall Variable V; Graph A; Graph B where some(V, A, B)
{
    override V.icon.d = makePath(A.text.center, B.text.center, 50, 10)
    override V.icon.strokeColor = rgba(0.0, 0.0, 0.0, 1.0)

    V.start = A.text.center
    V.end = B.text.center

    V.icon above A.text
    V.icon above B.text
}

forall Graph A; Graph B; Graph Result 
where Result := or(A, B) 
{
    override A.icon.fillColor = variable.primary
    override B.icon.fillColor = variable.primary

    override Result.level = A.level + 1

    Result.icon2 = Circle {
        strokeWidth: 0.0
        center: Result.icon.center
        r: Result.icon.r - 1.0
        fillColor: selectColor(variable.primary, variable.secondary, Result.level)
    }

    override Result.text = Circle{
        strokeWidth: 0.0
        r : 0.0
    }

    ensure smallerThan(A.icon, Result.icon)
    ensure smallerThan(B.icon, Result.icon)
    ensure contains(Result.icon2, A.icon, 5.0)
    ensure contains(Result.icon2, B.icon, 5.0)
    ensure disjoint(A.icon, B.icon)

    A.icon above Result.icon2
    B.icon above Result.icon2
    A.text above Result.icon2
    B.text above Result.icon2
}


forall Graph a where invisibleGraph(a){
    override a.text.fillColor = rgba(1.0, 0.0, 0.0, 0.0)
}



forall Graph A; Graph B; Graph Result 
where Result := if(A, B) 
{
    override Result.level = A.level + 1

    override Result.icon = Circle{
        strokeWidth: 0.0
        fillColor: selectColor(variable.primary, variable.secondary, Result.level)
    }

    override Result.icon2 = Circle{
        strokeWidth: 0.0
        fillColor: selectColor(variable.primary, variable.secondary, Result.level + 1)
    }

    override Result.text.string = ""

    override Result.icon2.r = Result.icon.r / 1.6

    ensure disjoint(A.text, Result.icon2)
    ensure contains(Result.icon, A.text)
    ensure contains(Result.icon2, B.text)

    ensure lessThan(20, Result.icon.r)
    ensure lessThan(20, Result.icon2.r)
    ensure contains(Result.icon, Result.icon2, 5.0)

    ensure contains(Result.icon2, B.icon, 5.0)


    Result.icon2 above Result.icon
    Result.icon2 below A.icon
    Result.icon2 below B.icon

}`,E={"existential-graph.domain":A,"graph.substance":S,"style.style":P},T=`type Scalar

-- define vectors and bivectors as subtypes of a base k-vector type
type kVector
type Vector <: kVector
type Bivector <: kVector

-- some basic operations on k-vectors
function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector

-- (more can be added here ) --
`,O=`-- define the size of the drawing
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
   scalar fontSize = "4.5px"
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

`,B=`Vector a, b, c
Bivector u := Wedge(a,b)
Bivector v := Wedge(b,c)

AutoLabel All
Label u $a \\wedge b$
Label v $b \\wedge c$
`,M={"exterior-algebra.domain":T,"exterior-algebra.style":O,"vector-wedge.substance":B},G=`type AmbientSpace
type Hyperplane
type Vector

predicate HyperplaneInAmbientSpace(Hyperplane, AmbientSpace)
predicate VectorInAmbientSpace(Vector, AmbientSpace)
predicate VectorInHyperplane(Vector, Hyperplane, AmbientSpace)
predicate VectorOrthogonalToHyperplane(Vector, Hyperplane, AmbientSpace)
predicate VectorOrthogonalToVector(Vector, Vector, AmbientSpace)

function addV(Vector, Vector) -> Vector
`,I=`canvas {
    width = 5
    height = 5
}

forall AmbientSpace S {
    S.origin = (0., 0., 0.)
    S.vp = (0.5, 1, 1) -- viewpoint
    S.origin_proj_vp = S.origin - (dot(S.origin, S.vp) / normsq(S.vp)) * S.vp
    S.origin_proj_2d = (S.origin_proj_vp[0], S.origin_proj_vp[1])
    S.shape = Circle {
      center : S.origin_proj_2d
      r : .02
      fillColor : rgba(0, 0, 0, 255)
    }
}

forall Hyperplane P; AmbientSpace S
where HyperplaneInAmbientSpace(P, S) {
    P.normal_vector = (0, 0, 1)

    vec3 vertex1 = (-2, -2, 0)
    vec3 vertex2 = (-2, 2, 0)
    vec3 vertex3 = (2, 2, 0)
    vec3 vertex4 = (2, -2, 0)

    vec3 vertex1_o = vertex1 - (dot(vertex1, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex2_o = vertex2 - (dot(vertex2, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex3_o = vertex3 - (dot(vertex3, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex4_o = vertex4 - (dot(vertex4, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector

    vec3 vertex1_proj_vp = vertex1_o - (dot(vertex1_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex2_proj_vp = vertex2_o - (dot(vertex2_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex3_proj_vp = vertex3_o - (dot(vertex3_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex4_proj_vp = vertex4_o - (dot(vertex4_o, S.vp) / normsq(S.vp)) * S.vp

  P.shape = Polygon {
    strokeWidth : .01
    strokeColor : rgba(0., 0., 0., 255.)
    points : [(vertex1_proj_vp[0], vertex1_proj_vp[1]), (vertex2_proj_vp[0], vertex2_proj_vp[1]), (vertex3_proj_vp[0], vertex3_proj_vp[1]), (vertex4_proj_vp[0], vertex4_proj_vp[1])]
  }
}

forall Vector v; AmbientSpace S
where VectorInAmbientSpace(v, S) {
  v.vec = (?, ?, ?)

  vec3 vec_endpoint_proj_vp = v.vec - (dot(v.vec, S.vp) / normsq(S.vp)) * S.vp

  v.shape = Line {
    start: S.origin_proj_2d
    end: (vec_endpoint_proj_vp[0], vec_endpoint_proj_vp[1])
    strokeWidth : 0.01
    endArrowhead: "straight"
    strokeColor : rgba(0, 0, 0, 255.)
  }
}

forall Vector v; Hyperplane P; AmbientSpace S
where VectorInHyperplane(v, P, S) {
  ensure inRange(v.vec[0], -2, 2)
  ensure inRange(v.vec[1], -2, 2)
  ensure equal(v.vec[2], 0)
  ensure equal(dot(v.vec, P.normal_vector), 0)
}

forall Vector v; Hyperplane P; AmbientSpace S
where VectorOrthogonalToHyperplane(v, P, S) {
  scalar multiplier = ?
  override v.vec = multiplier * P.normal_vector
  encourage notTooClose(v.shape, S.shape, 0.01)
}

forall Vector v; Vector u; AmbientSpace S
where VectorOrthogonalToVector(v, u, S) {
  encourage nonDegenerateAngle(v.shape, S.shape, u.shape, 500)
}

forall Vector u; Vector v; Vector w; AmbientSpace S
where w := addV(u, v); VectorInAmbientSpace(u, S); VectorInAmbientSpace(v, S); VectorInAmbientSpace(w, S) {
  override w.vec = u.vec + v.vec

  w.dashed_u = Line {
    start: v.shape.end
    end: w.shape.end
    strokeStyle : "dashed"
    strokeWidth : .01
  }
  w.dashed_v = Line {
    start: u.shape.end
    end: w.shape.end
    strokeStyle : "dashed"
    strokeWidth : .01
  }
}
`,V=`AmbientSpace S

Hyperplane P

HyperplaneInAmbientSpace(P, S)

Vector v

VectorInAmbientSpace(v, S)
VectorInHyperplane(v, P, S)

Vector u

VectorInAmbientSpace(u, S)
VectorOrthogonalToHyperplane(u, P, S)
VectorOrthogonalToVector(u, v, S)

Vector w := addV(v, u)
VectorInAmbientSpace(w, S)


AutoLabel All
`,N={"fake3d.domain":G,"fake3d.style":I,"projection.substance":V},_=`type Text
type Point
`,Q=`canvas {
   scalar width  = 533.
   scalar height = 300.
}

Global {

   shape bbox = Rectangle {
      center: (0.,0.)
      width: canvas.width
      height: canvas.height
      fillColor: rgba( .8,.95,.9,1. )
      strokeColor: rgba(0.,0.,0.,1.)
      strokeWidth: 2.
   }

}

-- Make some fun-looking text by
-- layering it on top of a couple
-- stroked copies
forall Text t {

   string t.titleString = t.label
   vec2 t.titleCenter = (0.,0.)

   shape t.text = Text {
      string: t.titleString
      center: t.titleCenter
      fillColor: rgba(1.,1.,1.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "38px"
   }
   shape t.innerStroke = Text {
      string: t.titleString
      center: t.titleCenter
      fillColor: rgba(0.,0.,0.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "38px"
      style: "stroke:#FF0303;stroke-width:4;stroke-miterlimit:2;"
   }
   shape t.outerStroke = Text {
      string: t.titleString
      center: t.titleCenter
      fillColor: rgba(0.,0.,0.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "38px"
      style: "stroke:#FF6603;stroke-width:8;stroke-miterlimit:2;"
   }

   -- make sure the text and strokes
   -- appear in the right order
   layer t.text above t.innerStroke
   layer t.innerStroke above t.outerStroke
   layer t.outerStroke above Global.bbox
}


-- draw each point as a circular disk,
-- whose size is allowed to vary
forall Point p {
   shape p.icon = Circle {
      center: (?,?)
      r: ?
      fillColor: rgba(0.,0.,1.,.15)
   }
   layer p.icon above Global.bbox

   shape p.text = Equation {
      center: p.icon.center
      string: p.label
      fillColor: rgba(.25,.5,1.,.5)
      fontSize: "28px"
   }
   layer p.text above Global.bbox

   ensure inRange( p.icon.r, 30., 60. )

   -- keep all disks on the canvas
   ensure contains( Global.bbox, p.icon )
}

-- repel all pairs of points
forall Point p; Point q {
   vec2 x = p.icon.center
   vec2 y = q.icon.center
   Δ = x-y
   encourage equal( 200000./normsq(Δ), 0. )
}

`,q=`Text t
Label t "Hello, Penrose!"
Point p1, p2, p3, p4, p5
Point p6, p7, p8, p9, p10
Label p1 $\\alpha$
Label p2 $\\beta$
Label p3 $\\gamma$
Label p4 $\\delta$
Label p5 $\\epsilon$
Label p6 $\\zeta$
Label p7 $\\eta$
Label p8 $\\theta$
Label p9 $\\iota$
Label p10 $\\kappa$

`,R={"fancy-text.domain":_,"fancy-text.style":Q,"fancy-text.substance":q},D=`type Frame
type Moon
type Crater
type Star
type Branch

predicate AreSubbranches(Branch left, Branch right, Branch base)
predicate HasCrater(Moon m, Crater c)
`,$=`canvas {
  width = 800
  height = 700
}

-- Frame

forall Frame f {
    f.ground = -200.0
    f.foreground = Rectangle {
        strokeWidth : 0.0
        center: (0.0, 0.5 * (f.ground - 300.0))
        width: 600.0
        height: f.ground + 300.0
        fillColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    f.background = Rectangle {
        strokeWidth : 0.0
        center: (0.0, 0.0)
        width: 600.0
        height: 600.0
        fillColor : rgba(0.05, 0.05, 0.45, 1.0)
    }
    f.foreground above f.background
}

forall Frame f; Moon m { 
    m.icon above f.background 
    ensure contains(f.background, m.icon)
    ensure disjoint(f.foreground, m.icon)
}

forall Frame f; Branch b { 
    b.icon above f.foreground
    ensure contains(f.background, b.icon)
    ensure disjoint(f.foreground, b.icon)
}

-- Moon

forall Moon m {
    m.icon = Circle {
        strokeWidth : 0.0
        fillColor : rgba(0.2, 0.2, 0.3, 1.0)
    }
    ensure lessThan(30.0, m.icon.r)
    ensure lessThan(m.icon.r, 50.0)
}

forall Crater c {
    c.icon = Circle {
        strokeWidth : 0.0
        fillColor : rgba(0.3, 0.3, 0.4, 1.0)
    }
    ensure lessThan(5.0, c.icon.r)
}

forall Crater c1; Crater c2 {
    ensure disjoint(c1.icon, c2.icon, 5.0)
}

forall Moon m; Crater c
where HasCrater(m, c) {
    c.icon above m.icon
    ensure contains(m.icon, c.icon, 5.0)
}

-- Stars

forall Star s {
    s.icon = Circle {
        strokeWidth : 0.0
        fillColor : rgba(0.9, 0.8, 0.0, 1.0)
    }
    ensure lessThan(3.0, s.icon.r)
    ensure lessThan(s.icon.r, 10.0)
}

forall Moon m; Star s {
    ensure disjoint(m.icon, s.icon)
}

forall Star s1; Star s2 {
    ensure disjoint(s1.icon, s2.icon)
}

forall Branch b; Star s {
    ensure disjoint(b.icon, s.icon, 10.0)
}

forall Frame f; Star s {
    s.icon above f.background
    ensure contains(f.background, s.icon)
    ensure disjoint(f.foreground, s.icon, 10.0)
}

-- Tree

forall Branch b {
    b.baseX = ? -- -20.0
    b.baseY = -200.0
    b.base = ( b.baseX, b.baseY )
    b.baseLength = 150.0
    b.baseAngle = 0.0
    b.scalingFactorL = 0.7
    b.scalingFactorR = 0.6
    b.angleL = ?
    b.angleR = ?
    b.tipX = b.baseX + sin(b.baseAngle) * b.baseLength
    b.tipY = b.baseY + cos(b.baseAngle) * b.baseLength
    b.strokeWidth = 8.0

    b.icon = Line {
        start: (b.baseX, b.baseY)
        end: (b.tipX, b.tipY)
        strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: b.strokeWidth
        strokeLinecap: "round"
    }

    ensure lessThanSq(-1.0, b.angleL)
    ensure lessThanSq(-1.0, b.angleR)
    ensure lessThanSq(b.angleL, 1.0)
    ensure lessThanSq(b.angleR, 1.0)
}

forall Branch b; Moon m {
    ensure disjoint(b.icon, m.icon, 20.0)
}

forall Branch left; Branch right; Branch base
where AreSubbranches(left, right, base) {

    override left.baseX = base.tipX
    override left.baseY = base.tipY
    override left.baseLength = base.scalingFactorL * base.baseLength
    override left.baseAngle = base.baseAngle + base.angleL
    override left.strokeWidth = 0.5 * base.strokeWidth
    override left.scalingFactorL = base.scalingFactorL
    override left.scalingFactorR = base.scalingFactorR
    override left.angleL = base.angleL
    override left.angleR = base.angleR
    override left.tipX = left.baseX + sin(left.baseAngle) * left.baseLength
    override left.tipY = left.baseY + cos(left.baseAngle) * left.baseLength

    override left.icon = Line {
      start: (left.baseX, left.baseY)
      end: (left.tipX, left.tipY)
      strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
      strokeWidth: left.strokeWidth
      strokeLinecap: "round"
    }

    override right.baseX = base.tipX
    override right.baseY = base.tipY
    override right.baseLength = base.scalingFactorR * base.baseLength
    override right.baseAngle = base.baseAngle + base.angleR
    override right.strokeWidth = 0.5 * base.strokeWidth
    override right.scalingFactorL = base.scalingFactorL
    override right.scalingFactorR = base.scalingFactorR
    override right.angleL = base.angleL
    override right.angleR = base.angleR
    override right.tipX = right.baseX + sin(right.baseAngle) * right.baseLength
    override right.tipY = right.baseY + cos(right.baseAngle) * right.baseLength

    override right.icon = Line {
      start: (right.baseX, right.baseY)
      end: (right.tipX, right.tipY)
      strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
      strokeWidth: right.strokeWidth
      strokeLinecap: "round"
    }

}
`,W=`-- Frame
Frame F

-- Moon
Moon M

Crater C1, C2, C3
HasCrater(M, C1)
HasCrater(M, C2)
HasCrater(M, C3)

-- Stars
Star S1, S2, S3, S4, S5, S6, S7

-- Tree
Branch B

Branch BL, BR
AreSubbranches(BL, BR, B)

Branch BLL, BLR, BRL, BRR
AreSubbranches(BLL, BLR, BL)
AreSubbranches(BRL, BRR, BR)

Branch BLLL, BLLR, BLRL, BLRR, BRLL, BRLR, BRRL, BRRR
AreSubbranches(BLLL, BLLR, BLL)
AreSubbranches(BLRL, BLRR, BLR)
AreSubbranches(BRLL, BRLR, BRL)
AreSubbranches(BRRL, BRRR, BRR)
`,F={"full-moon.domain":D,"full-moon.style":$,"full-moon.substance":W},j=`-- Original Style: https://github.com/penrose/penrose/blob/90e88c5a075d6a75698c49c2feb101275ae64af8/examples/geometry-domain/euclidean.sty
-- Original comp. functions: https://github.com/penrose/penrose/blob/90e88c5a075d6a75698c49c2feb101275ae64af8/src/Penrose/Functions.hs

canvas {
  width = 800
  height = 700
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
    textPadding2 = 25.0
    repelWeight = 0.7 -- TODO: Reverted from 0.0
    repelWeight2 = 0.5
    fontSize = "22pt"
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
  p.iconOnPlane = ensure contains(P.icon, p.icon, const.containPadding)
  p.textOnPlane = ensure contains(P.icon, p.text, 0.0)

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
where r := MkRay(base, direction)
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
where l := MkLine(p, q)
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
where e := MkSegment(p, q)
with Point p; Point q {
  override e.vec = [q.x - p.x, q.y - p.y]
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
where t := MkTriangle(p, q, r)
with Point p; Point q; Point r {
  t.PQ above P.icon
  t.QR above P.icon
  t.RP above P.icon
  t.icon above P.icon
}

forall Triangle t
where t := MkTriangle(p, q, r)
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
where T := MkTriangle(t1, t2, t3); Incenter(p, T) {
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
where R := MkRectangle(p, q, r, s)
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
where Q := MkQuadrilateral(p, q, r, s)
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
where s := MidSegment(T, a, b); T := MkTriangle(p, q, r) {
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
where p := MkMidpoint(l)
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
where c := MkCircleR(p, q)
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
-- where c := MkCircleD(p, q)
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
  p.icon = Rectangle {
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
where t := MkTriangle(p, q, r)
with Point p; Point q; Point r {
  t.strokeWidth = 0.0
  t.PQ = Line {
    start : p.icon.center
    end : q.icon.center
    strokeWidth : t.strokeWidth
  }
  t.QR = Line {
    start : q.icon.center
    end : r.icon.center
    strokeWidth : t.strokeWidth
  }
  t.RP = Line {
    start : r.icon.center
    end : p.icon.center
    strokeWidth : t.strokeWidth
  }
  t.icon = Path {
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
  theta.p = p.vec
  theta.q = q.vec
  theta.r = r.vec

  theta.radius = const.thetaRadius
  encourage nonDegenerateAngle(p.icon, q.icon, r.icon)
  startA = ptOnLine(theta.q, theta.p, theta.radius)
  endA = ptOnLine(theta.q, theta.r, theta.radius)
  sweepA = arcSweepFlag(theta.q, startA, endA)
  override theta.mark = Path {
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
  s.mark = Path {
    d : pathFromPoints("open", [ptOnLine(s.icon.end, s.icon.start, markSize), innerPointOffset(s.icon.end, s.icon.start, s2.icon.end, markSize), ptOnLine(s.icon.end, s2.icon.end, markSize)])
    strokeWidth : 2.0
    strokeColor : Colors.black
  }

  ensure disjoint(s.icon, p.text)
  ensure perpendicular(s.icon.start, s.icon.end, s2.icon.end)
  ensure equal(norm(s.icon.end - s.icon.start), const.rayLength)
}

forall Point p
where p := MkMidpoint(l)
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
`,H=`canvas {
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
    fontSize = "13.5px"
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
  p.iconOnPlane = ensure contains(P.icon, p.icon, const.containPadding)
  p.textOnPlane = ensure contains(P.icon, p.text) in label

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
where r := MkRay(base, direction)
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
where l := MkLine(p, q)
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
where e := MkSegment(p, q)
with Point p; Point q {
  override e.vec = [q.x - p.x, q.y - p.y]
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
  override a.mark = Path {
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
where t := MkTriangle(p, q, r)
with Point p; Point q; Point r {
  t.PQ above P.icon
  t.QR above P.icon
  t.RP above P.icon
  t.icon above P.icon
}

forall Triangle t
where t := MkTriangle(p, q, r)
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
where T := MkTriangle(t1, t2, t3); Incenter(p, T) {
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
where R := MkRectangle(p, q, r, s)
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
where Q := MkQuadrilateral(p, q, r, s)
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
where s := MidSegment(T, a, b); T := MkTriangle(p, q, r) {
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
where p := MkMidpoint(l)
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
where c := MkCircleR(p, q)
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
-- where c := MkCircleD(p, q)
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
}`,z=`-- right triangle
Point A, B, C
Triangle ABC
ABC := MkTriangle(A, B, C) 

Angle CAB
CAB := InteriorAngle(C, A, B)

AutoLabel All`,U=`-- ~~~~~~~~~~~~~~~~ TYPES ~~~~~~~~~~~~~~~~
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
constructor MkSegment(Point p, Point q) -> Segment
constructor MkRay(Point base, Point direction) -> Ray
constructor MkLine(Point p, Point q) -> Line
constructor MkMidpoint(Linelike l) -> Point

-- Angles
constructor InteriorAngle(Point p, Point q, Point r) -> Angle

-- Polygons/Shapes
constructor MkTriangle(Point p, Point q, Point r) -> Triangle
constructor MkRectangle(Point p, Point q, Point r, Point s) -> Rectangle
constructor MkQuadrilateral(Point p, Point q, Point r, Point s) -> Quadrilateral
constructor MkCircleR(Point center, Point radius) -> Circle
-- constructor MkCircleD(Point diam1, Point diam2) -> Circle  -- TODO can be reimplemented when #621 is resolved

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

-- notation "{p, q}" ~ "MkSegment(p, q)"
-- notation "{p, q, r}" ~ "MkTriangle(p, q, r)"
-- notation "{p, q, r, s}" ~ "MkRectangle(p, q, r, s)"
-- notation "∠(p, q, r)" ~ "InteriorAngle(p, q, r)"`,K=`-- First construct the right triangle
-- Byrne (1)
Point A, B, C
Triangle ABC := {A, B, C} 
Angle CAB := ∠(C, A, B)
Right(CAB)
-- Triangle won't be visible unless you show the segments

-- And draw the area of each side of the right triangle as a square
-- Byrne (2)
Point D, E, F, G, H, I
Square CBDE := [C, B, D, E]
Disjoint(CBDE, ABC)
Square BAGF := [B, A, G, F]
Disjoint(BAGF, ABC)
Square ACIH := [A, C, I, H]
Disjoint(ACIH, ABC)

-- -- Construct two rectangles that split the hypotenuse's area by dropping an altitude from the right angle through to the square
-- Byrne (3)
Segment AK := Altitude(ABC, CAB)
Point K := Endpoint(AK)
Segment DE := {D, E}
Point L
On(L, DE)
Segment KL := {K, L}
Perpendicular(KL, DE)
Rectangle BDLK := {B, D, L, K}
Rectangle CKLE := {C, K, L, E}
-- -- Drop two lines that describe the triangles used to prove area equality between BAGF~BDKL and ACIH~CKLE
Segment CF := {C, F}
Segment AD := {A, D} 

-- -- These objects are only present/displayed in the Byrne style
-- Byrne (1)
Segment AB := {A, B}
Segment BC := {B, C}
Segment CA := {C, A}

-- Byrne (3)
Segment AL := {A, L}
Segment BF := {B, F}
Segment DB := {D, B}
Angle FBA := ∠(F, B, A)
Angle CBA := ∠(C, B, A)
Angle CBD := ∠(C, B, D)

-- These are part of the proof, but not shown in the construction
-- Triangle BCF := {B, C, F}
-- Triangle BDA := {B, D, A}
-- Angle CAB := ∠(C, A, B)

AutoLabel All
`,X=`-- First construct the right triangle
Point A, B, C
Triangle ABC
ABC := MkTriangle(A, B, C) 

Angle CAB
CAB := InteriorAngle(C, A, B)

Right(CAB)

-- And draw the area of each side of the right triangle as a square
Point D, E, F, G, H, I
Square CBDE
CBDE := MkSquare(C, B, D, E)

Disjoint(CBDE, ABC)
Square BAGF
BAGF := MkSquare(B, A, G, F)

Disjoint(BAGF, ABC)
Square ACIH
ACIH := MkSquare(A, C, I, H)

Disjoint(ACIH, ABC)

-- Construct two rectangles that split the hypotenuse's area by dropping an altitude from the right angle through to the square
Segment AK
AK := Altitude(ABC, CAB)

Point K
K := Endpoint(AK)

Segment DE
DE := MkSegment(D, E)

Point L
On(L, DE)
Segment KL
KL := MkSegment(K, L)

Perpendicular(KL, DE)

Rectangle BDLK
BDLK := MkRectangle(B, D, L, K)

Rectangle CKLE
CKLE := MkRectangle(C, K, L, E)


-- Drop two lines that describe the triangles used to prove area equality between BAGF~BDKL and ACIH~CKLE
Segment CF
CF := MkSegment(C, F)

Segment AD
AD := MkSegment(A, D) 


-- These objects are only present in the Byrne style
Segment AB
AB := MkSegment(A, B)

Segment BC
BC := MkSegment(B, C)

Segment CA
CA := MkSegment(C, A)

Segment AL
AL := MkSegment(A, L)

Segment BF
BF := MkSegment(B, F)

Segment DB
DB := MkSegment(D, B)

Angle FBA
FBA := InteriorAngle(F, B, A)

Angle CBA
CBA := InteriorAngle(C, B, A)

Angle CBD
CBD := InteriorAngle(C, B, D)


-- These are part of the proof, but not shown in the construction
Triangle BCF
BCF := MkTriangle(B, C, F)

Triangle BDA
BDA := MkTriangle(B, D, A)

Angle CAB
CAB := InteriorAngle(C, A, B)


AutoLabel All
`,Y=`Plane P
Point p, q, r, s
In(p, P)
In(q, P)
In(r, P)
In(s, P)
Segment a := MkSegment(p, q)
Segment b := MkSegment(p, r)
Point m := MkMidpoint(a)
In(m, P)
Angle theta := InteriorAngle(q, p, r)
Triangle t := MkTriangle(p, r, s)
Ray w := Bisector(theta)
Segment h := PerpendicularBisector(a, m)
AutoLabel p, q, r, s, m
Label P $E^2$`,Z=`-- substance name in registry: collinear
Plane R
Point A, B, C, D, E
In(A, R)
In(B, R)
In(C, R)
In(D,R)
In(E,R)
Ray BE := MkRay(B, E)
Line BA := MkLine(B, A)
Collinear(B,D,E)
AutoLabel A, B, C, D, E, R`,J=`Point M, N, Q, P
Segment NQ := MkSegment(N, Q)
Angle QMN := InteriorAngle(Q, M, N)
Angle QPN := InteriorAngle(Q, P, N)
RightMarked(QMN)
RightMarked(QPN)
Segment MN := MkSegment(M, N)
Segment MQ := MkSegment(M, Q)
Segment QP := MkSegment(Q, P)
Segment PN := MkSegment (P, N)
EqualLengthMarker(MQ, PN)
EqualLength(MQ, PN)
EqualLengthMarker(MN, QP)
EqualLength(MN, QP)
AutoLabel M, N, Q, P`,nn=`Point A, B, C, D
Ray DA := MkRay(D, A)
Ray DB := MkRay(D, B)
Ray DC := MkRay(D, C)
Angle ADB := InteriorAngle(A, D, B)
Angle BDC := InteriorAngle(B, D, C)
Angle ADC := InteriorAngle(A, D, C)
AngleBisector(ADC, DB)
EqualAngleMarker(ADB, BDC)
AutoLabel A, B, C, D`,en=`-- name in registry: parallel-lines
Point J, K, L, M, N
Line JK := MkLine(J, K)
Line NL := MkLine(N, L)
Line KM := MkLine(K, M)
ParallelMarker1(JK, NL)
Parallel(JK, NL)
Collinear(L, M, N)
Angle JKM := InteriorAngle(J, K, M)
-- Acute(JKM) 
AutoLabel J, K, L, M, N`,tn=`Point A, B, C, D, E
Segment AE := MkSegment(A, E)
Segment EC := MkSegment(E, C)
Segment AB := MkSegment(A, B)
Segment BC := MkSegment(B, C)
Segment CD := MkSegment(C, D)
Segment DA := MkSegment(D, A)
Segment ED := MkSegment(E, D)
Segment EB := MkSegment(E, B)
Collinear(A,E,C)
Collinear(B,E,D) 
Angle r := InteriorAngle(B,E,C)
EqualLength(ED, EB)
EqualLength(AE, EC)
EqualLengthMarker(AE, EC)
RightMarked(r)
AutoLabel A, B, C, D, E
`,on=`Point A, B, C, D, E
Collinear(A, C, E)
Collinear(B, C, D)
Segment BC := MkSegment(B, C)
Segment AC := MkSegment(A, C)
Angle aBAC := InteriorAngle(B, A, C)
Angle aABC := InteriorAngle(A, B, C)
Angle aDEC := InteriorAngle(D, E, C)
Angle aDCE := InteriorAngle(D, C, E)
Angle aCDE := InteriorAngle(C, D, E)
Triangle ABC := MkTriangle(A, B, C)
Triangle DCE := MkTriangle(C, D, E)
EqualLength(AC, BC)
EqualLengthMarker(AC, BC)
EqualAngle(aABC, aBAC)
EqualAngleMarker(aABC, aBAC)
EqualAngle(aDCE, aDEC)
EqualAngleMarker(aDCE, aDEC)
AutoLabel A, B, C, D, E
Label aCDE $x$
`,rn=`-- name in registry: midsegment-triangles
Point D, E, F, G, H, J, K
Triangle DEF := MkTriangle(D, E, F)
Triangle GJF := MkTriangle(G, J, F)
Segment GJ := MidSegment(DEF, G, J)  -- TODO add markers when attribute changes are implemented
Segment HK := MidSegment(GJF, H, K)
Angle aGFJ := InteriorAngle(G, F, J)
AutoLabel D, E, F, G, H, J, K`,an=`-- name in registry: incenter-triangle
Point J, K, L, P, m
Triangle JKL := MkTriangle(J, K, L)
Incenter(P, JKL)
-- Centroid(P, JKL)
-- Circumcenter(P, JKL)
-- Orthocenter(P, JKL)
Segment KL := MkSegment(K, L)
Collinear(K, m, L)
Triangle PLM := MkTriangle(P, L, m)
Angle PML := InteriorAngle(P, m, L)
RightMarked(PML)
AutoLabel J, K, L, P, m
`,ln=`Point J, K, L, M, N
Quadrilateral JKLM := MkQuadrilateral(J, K, L, M)
Segment JK := MkSegment(J, K)
Segment ML := MkSegment(M, L)
Segment KM := MkSegment(K, M)
Segment JL := MkSegment(J, L)
Midpoint(JL, N)
Midpoint(KM, N)
ParallelMarker1(JK, ML)
Parallel(JK, ML)
EqualLength(JK, ML)
EqualLengthMarker(JK, ML)
AutoLabel J, K, L, M, N`,sn=`Point A, B, C, X, Y, Z, P
Triangle ABC := MkTriangle(A, B, C)
Segment AB := MkSegment(A, B)
Segment BC := MkSegment(B, C)
Segment AC := MkSegment(A, C)

Segment AP := MkSegment(A, P)
Segment BP := MkSegment(B, P)
Segment CP := MkSegment(C, P)

Segment YP := PerpendicularBisectorLabelPts(AB, P, X)
Segment ZP := PerpendicularBisectorLabelPts(BC, P, Y)
Segment XP := PerpendicularBisectorLabelPts(AC, P, Z)

AutoLabel A, B, C, X, Y, Z, P`,cn=`Point A, B, C, D, E
Line AB := MkLine(A, B)
Line CD := MkLine(C, D)
Parallel(AB, CD)
ParallelMarker1(AB, CD)
Line AC := MkLine(A, C)
Line BD := MkLine(B, D)
Collinear(A, E, D)
Collinear(C, E, B)
Segment AD := MkSegment(A, D)
Segment CB := MkSegment(C, B)
EqualLengthMarker(AB, CD)
EqualLength(AB, CD)
AutoLabel A, B, C, D, E`,dn=`Point F, G, H, J, K, L
Triangle FGH := MkTriangle(F, G, H)
Angle aFGH := InteriorAngle(F, G, H)
Segment KH := MkSegment(K, H)
Segment FL := MkSegment(F, L)
Collinear(F, J, L)
Collinear(K, J, H)
Collinear(F, K, G)
Collinear(G, L, H)
RightMarked(aFGH)

-- should be angle bisectors
Angle GFL := InteriorAngle(G, F, L)
Angle LFH := InteriorAngle(L, F, H)
Angle GHK := InteriorAngle(G, H, K)
Angle KHF := InteriorAngle(K, H, F)
EqualAngleMarker(GHK, KHF)
EqualAngle(GHK, KHF)
EqualAngleMarker(GFL, LFH)
EqualAngle(GFL, LFH)
AutoLabel F, G, H, J, K, L`,pn=`Point R, S, T, U, V, W
Triangle RST := MkTriangle(R, S, T)
Triangle UVW := MkTriangle(U, V, W)
Segment SR := MkSegment(S, R)
Segment ST := MkSegment(S, T)
EqualLengthMarker(SR, ST)
EqualLength(SR, ST)

Angle aUWV := InteriorAngle(U, W, V)
Angle aSRT := InteriorAngle(S, R, T)
EqualAngle(aUWV, aSRT)
EqualAngleMarker(aUWV, aSRT)

Angle aWUV := InteriorAngle(W, U, V)
Angle aSTR := InteriorAngle(S, T, R)
EqualAngle(aWUV, aSTR)
EqualAngleMarker(aWUV, aSTR)

Segment UV := MkSegment(U, V)
Segment VW := MkSegment(V, W)
EqualLengthMarker(UV, VW)
EqualLength(UV, VW)
AutoLabel R, S, T, U, V, W`,gn=`Point P, Q, R, S, T
Triangle tPQR := MkTriangle(P, Q, R)
Triangle tQTR := MkTriangle(Q, T, R)
Triangle tTSR := MkTriangle(T, S, R)
Angle PQR := InteriorAngle(P, Q, R)
Angle QTR := InteriorAngle(Q, T, R)
Angle TSR := InteriorAngle(T, S, R)
Segment PR := MkSegment(P, R)
Segment QR := MkSegment(Q, R)
On(T, PR)
On(S, QR)
RightMarked(PQR)
RightMarked(QTR)
RightMarked(TSR)

AutoLabel P, Q, R, S, T`,fn=`Point A, B, C, D
Triangle ABC := MkTriangle(A, B, C)
Segment BD := MkSegment(B, D)
Collinear(A, D, C)
AutoLabel All`,un=`Point J, K, L
Triangle JKL := MkTriangle(J, K, L)
Segment KL := MkSegment(K, L)
Segment j := PerpendicularBisector(KL, J)
AutoLabel All`,hn=`-- TODO one of the triangles crosses with the other segments on a regular basis
Point A, B, C, D, E, F
Triangle ABC := MkTriangle(A, B, C)
Triangle CDE := MkTriangle(C, D, E)
Triangle DEF := MkTriangle(D, E, F)
Collinear(B, C, E)
Collinear(A, C, D)
Angle aCEF := InteriorAngle(C, E, F)
Segment DE := MkSegment(D, E)
Segment CA := MkSegment(C, A)
Segment BC := MkSegment(B, C)
Segment CE := MkSegment(C, E)
Segment CD := MkSegment(C, D)
Segment EF := MkSegment(E, F)
Segment DF := MkSegment(D, F)
EqualLengthMarker(CA, CD)
EqualLength(CA, CD)
EqualLength(BC, CE)
EqualLengthMarker(BC, CE)
EqualLength(CE, EF)
EqualLengthMarker(CE, EF)
AngleBisector(aCEF, DE)
Angle aCED := InteriorAngle(C, E, D)
Angle aDEF := InteriorAngle(D, E, F)
EqualAngle(aCED, aDEF)
EqualAngleMarker(aCED, aDEF)
AutoLabel A, B, C, D, E, F`,bn=`-- name in registry: circle-example
Point P, Q, U, S, T, R
Circle c
Segment PR := MkSegment(P, R)
Segment PS := MkSegment(P, S)
Segment RT := Diameter(c, R, T)
Collinear(P, Q, R)
Collinear(P, U, S)
OnCircle(c, Q)
OnCircle(c, U)
Segment ST := Chord(c, S, T)
AutoLabel P, Q, U, S, T, R`,kn=`-- TODO full example can sometimes throw error "2147482504"
Point A, B, C, D, E, F, d, b
Circle c := MkCircleR(F, C)
Segment AC := Chord(c, A, C)
Segment EC := Chord(c, E, C)
Segment FB := Radius(c, B)
Segment FD := Radius(c, D)
Segment FC := Radius(c, C)
Segment Fd := PerpendicularBisectorLabelPts(EC, F, d)
Segment Fb := PerpendicularBisectorLabelPts(AC, F, b)
On(b, FB)
On(d, FD)
Collinear(F, b, B)
Collinear(F, d, D)
Segment EA := Diameter(c, E, A)
AutoLabel A, B, C, D, E, F, d, b`,xn=`Point J, K, L, M, Y
Circle c
Segment LM := Chord(c, L, M)
Segment JK := Chord(c, J, K)
Collinear(J, Y, K)
Collinear(L, Y, M)
AutoLabel All`,vn=`Point A, B, C, D, O
Circle c
CircleCenter(c, O)
Segment AO := MkSegment(A, O)
Segment CO := MkSegment(C, O)
EqualLengthMarker(AO, CO)
Segment AB := Chord(c, A, B)
Segment DC := Chord(c, D, C)
Segment CB := Chord(c, B, C)
Segment DA := Chord(c, D, A)
Segment DB := Diameter(c, D, B)
Segment AC := Diameter(c, A, C)
Quadrilateral ABCD := MkQuadrilateral(A, B, C, D)
ParallelMarker1(AB, DC)
Parallel(AB, DC)
AutoLabel A, B, C, D, O`,mn=`Point T, V, S, P
Circle c := MkCircleR(P, V)
Segment PV := Radius(c, V)
Segment PT := Radius(c, T)
Segment ST := Chord(c, S, T)
Segment SV := Chord(c, V, S)
Segment TV := Chord(c, T, V)
AutoLabel All`,yn=`Point A, B, C, D
Triangle ABC := MkTriangle(A, B, C)
Triangle BCD := MkTriangle(B, C, D)
Segment AB := MkSegment(A, B)
Segment BC := MkSegment(B, C)
Segment DC := MkSegment(D, C)
Segment AC := MkSegment(A, C)
Segment BD := MkSegment(B, D)
EqualLength(AB, BC)
EqualLength(BC, DC)
EqualLengthMarker(AB, BC)
EqualLengthMarker(BC, DC)
Parallel(AC, BD)
EqualLength(AC, BD)
AutoLabel A, B, C, D`,wn=`-- TODO simplify after attributes are implemented
-- name in registry: congruent-triangles
Point U, S, T,  X, Y, Z

Triangle UTS := MkTriangle(U, T, S)
Segment US := MkSegment(U, S)
Segment UT := MkSegment(U, T)
Segment ST := MkSegment(S, T)
Angle aTUS := InteriorAngle(T, U, S)
Angle aUST := InteriorAngle(U, S, T)
Angle aUTS := InteriorAngle(U, T, S)

Triangle XYZ := MkTriangle(X, Y, Z)
Segment XZ := MkSegment(X, Z)
Segment XY := MkSegment(X, Y)
Segment ZY := MkSegment(Z, Y)
Angle aXYZ := InteriorAngle(X, Y, Z)
Angle aYZX := InteriorAngle(Y, Z, X)
Angle aZXY := InteriorAngle(Z, X, Y)

EqualLengthMarker(ST, XY)
EqualLength(ST, XY)

EqualLengthMarker(XZ, US)
EqualLength(XZ, US)

EqualLengthMarker(ZY, UT)
EqualLength(ZY, UT)

EqualAngleMarker(aTUS, aXYZ)
EqualAngleMarker(aUST, aYZX)
EqualAngleMarker(aUTS, aZXY)
EqualAngle(aTUS, aXYZ)
EqualAngle(aUST, aYZX)
EqualAngle(aUTS, aZXY)

AutoLabel U, S, T, X, Y, Z`,Cn={"c01p01.substance":Z,"c01p10.substance":J,"c02p01.substance":nn,"c03p01.substance":en,"c04p01.substance":tn,"c04p12.substance":on,"c05p01.substance":rn,"c05p13.substance":an,"c06p06.substance":ln,"c07p02.substance":sn,"c07p06.substance":cn,"c07p10.substance":dn,"c07p22.substance":pn,"c08p08.substance":gn,"c08p18.substance":fn,"c09p02.substance":un,"c10p08.substance":hn,"c11p07.substance":bn,"c11p12.substance":kn,"c11p21.substance":xn,"c11p25.substance":vn,"c12p12.substance":mn,"c12p20.substance":yn,"ex.substance":wn},Ln={"euclidean-teaser.style":j,"euclidean.style":H,"general-triangle.substance":z,"geometry.domain":U,"pythagorean-theorem-sugared.substance":K,"pythagorean-theorem-unsugared.substance":X,"teaser.substance":Y,textbook_problems:Cn},An=`type Vertex
type Link
constructor Arc(Vertex u, Vertex v) -> Link
`,Sn=`canvas {
  width = 400
  height = 400
}

layout = [dots, loops, arrows, text]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
  offset = 10
  loopRadius = 15
  pointerX = 6
  pointerY = 4
}

forall Vertex v {
  v.dot = Circle {
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black
  }

  v.text = Text {
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v) {
  a = u.dot.center
  b = v.dot.center
  t = normalize(b - a) -- tangent
  n = rot90(t) -- normal
  m = (a + b) / 2 -- midpoint

  e.start = a
  e.end = b
  e.theta = angleOf(t)
  e.offset = ? in dots
  e.arrow = Path {
    d: quadraticCurveFromPoints("open", [a, m + e.offset * n, b])
    strokeColor: color.black
  }

  e.step = ? in arrows
  e.pointerCenter = m + (e.offset / 2) * n + e.step * t
  p = e.pointerCenter
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path {
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot
  e.pointer below e.arrow

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
  encourage minimal(sqr(e.offset)) in dots
  encourage minimal(sqr(e.step)) 
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(u, v) {
  ensure abs(e2.offset - e1.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(v, u) {
  ensure abs(e1.offset + e2.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v); u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v); v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w; Link e where e := Arc(u, v); w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}

forall Vertex v; Link e where e := Arc(v, v) {
  e.theta = ? in loops
  n = (cos(e.theta), sin(e.theta)) -- normal
  delta = num.loopRadius * n

  e.arrow = Circle {
    center: v.dot.center + delta
    r: num.loopRadius
    strokeWidth: 1
    strokeColor: color.black
    fillColor: none()
  }

  t = rot90(n)
  p = v.dot.center + 2 * delta
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path {
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black
  }

  e.arrow below v.dot
  e.pointer below e.arrow
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(v, v) {
  encourage maximal(cos(e2.theta - e1.theta)) in loops
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(v, u); e2 := Arc(v, v) {
  encourage minimal(cos(e2.theta - e1.theta)) in loops
}

forall Link e1, e2
where e1 := Arc(a, b); e2 := Arc(c, d)
with Vertex a, b, c, d {
  ensure norm(e2.pointerCenter - e1.pointerCenter) > max(num.pointerX, num.pointerY)*3 in arrows
  encourage e1.step == e2.step
}
`,Pn=`type Vertex
type Link
constructor Edge(Vertex u, Vertex v) -> Link
`,En=`canvas {
  width = 400
  height = 400
}

layout = [dots, loops, text]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
  offset = 10
  loopRadius = 15
  pointerX = 6
  pointerY = 4
}

forall Vertex v {
  v.dot = Circle {
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black
  }

  v.text = Text {
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v; Link e where e := Edge(u, v) {
  a = u.dot.center
  b = v.dot.center
  t = normalize(b - a) -- tangent
  n = rot90(t) -- normal
  m = (a + b) / 2 -- midpoint

  e.start = a
  e.end = b
  e.theta = angleOf(t)
  e.offset = ? in dots
  e.arrow = Path {
    d: quadraticCurveFromPoints("open", [a, m + e.offset * n, b])
    strokeColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
  encourage minimal(sqr(e.offset)) in dots
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Edge(u, v); e2 := Edge(u, v) {
  ensure abs(e2.offset - e1.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Edge(u, v); e2 := Edge(v, u) {
  ensure abs(e1.offset + e2.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e where e := Edge(u, v); u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Link e where e := Edge(u, v); v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w; Link e where e := Edge(u, v); w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}

forall Vertex v; Link e where e := Edge(v, v) {
  e.theta = ? in loops
  n = (cos(e.theta), sin(e.theta)) -- normal
  delta = num.loopRadius * n

  e.arrow = Circle {
    center: v.dot.center + delta
    r: num.loopRadius
    strokeWidth: 1
    strokeColor: color.black
    fillColor: none()
  }

  e.arrow below v.dot
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Edge(u, v); e2 := Edge(v, v) {
  encourage maximal(cos(e2.theta - e1.theta)) in loops
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Edge(v, u); e2 := Edge(v, v) {
  encourage minimal(cos(e2.theta - e1.theta)) in loops
}
`,Tn=`type Vertex
predicate Arc(Vertex, Vertex)
`,On=`canvas {
  width = 400
  height = 400
}

layout = [dots, arrows, text]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
  offset = 10
  loopRadius = 15
  pointerX = 6
  pointerY = 4
}

forall Vertex v {
  v.dot = Circle {
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black
  }

  v.text = Text {
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v where Arc(u, v) as e {
  a = u.dot.center
  b = v.dot.center
  t = normalize(b - a) -- tangent
  n = rot90(t) -- normal
  m = (a + b) / 2 -- midpoint

  e.start = a
  e.end = b
  e.offset = ? in dots
  e.arrow = Path {
    d: quadraticCurveFromPoints("open", [a, m + e.offset * n, b])
    strokeColor: color.black
  }

  e.step = ? in arrows
  e.pointerCenter = m + (e.offset / 2) * n + e.step * t
  p = e.pointerCenter
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path {
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot
  e.pointer below e.arrow

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
  encourage minimal(sqr(e.offset)) in dots
  encourage minimal(sqr(e.step)) 
}

forall Vertex u; Vertex v where Arc(u, v) as e1; Arc(u, v) as e2 {
  ensure abs(e2.offset - e1.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v where Arc(u, v) as e1; Arc(v, u) as e2 {
  ensure abs(e1.offset + e2.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v where Arc(u, v) as e; u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v where Arc(u, v) as e; v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w where Arc(u, v) as e; w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}

forall Vertex a, b, c, d where Arc(a, b) as e1; Arc(c, d) as e2 {
  ensure norm(e2.pointerCenter - e1.pointerCenter) > max(num.pointerX, num.pointerY)*3 in arrows
  encourage e1.step == e2.step
}
`,Bn=`type Vertex
symmetric predicate Edge(Vertex, Vertex)
`,Mn=`canvas {
  width = 400
  height = 400
}

layout = [dots, text]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
  offset = 10
  loopRadius = 15
  pointerX = 6
  pointerY = 4
}

forall Vertex v {
  v.dot = Circle {
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black
  }

  v.text = Text {
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v where Edge(u, v) as e {
  a = u.dot.center
  b = v.dot.center

  e.start = a
  e.end = b
  e.arrow = Line {
    start: a
    end: b
    strokeColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
}

forall Vertex u; Vertex v where Edge(u, v) as e; u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v where Edge(u, v) as e; v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w where Edge(u, v) as e; w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}
`,Gn=`Vertex a, b, c, d

Edge(a, b)
Edge(a, c)
Edge(b, c)
Edge(b, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,In=`Vertex a, b, c, d

Link e1m1 := Edge(a, b)
Link e1m2 := Edge(a, b)
Link e2 := Edge(a, c)
Link e3m1 := Edge(b, d)
Link e3m2 := Edge(b, d)
Link e3m3 := Edge(b, d)
Link e4 := Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Vn=`Vertex a, b, c, d

Link e1m1 := Edge(a, b)
Link e1m2 := Edge(a, b)
Link e2 := Edge(a, c)
Link e3m1 := Edge(b, d)
Link e3m2 := Edge(b, d)
Link e4m1 := Edge(c, d)
Link e4m2 := Edge(c, d)

Link e5 := Edge(a, a)
Link e6 := Edge(b, b)
Link e7 := Edge(d, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Nn=`Vertex a, b, c, d, e

Link e1 := Edge(a, b)
Link e2m1 := Edge(a, c)
Link e2m2 := Edge(a, c)
Link e3m1 := Edge(b, d)
Link e3m2 := Edge(b, d)
Link e4 := Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,_n=`Vertex a, b, c, d, e

Link e1 := Arc(a, b)
Link e2 := Arc(b, e)
Link e3 := Arc(c, b)
Link e4 := Arc(c, c)
Link e5 := Arc(c, d)
Link e6 := Arc(d, c)
Link e7 := Arc(e, a)
Link e8 := Arc(e, d)
Link e9 := Arc(e, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Qn=`Vertex a, b, c, d, e

Link e1m1 := Arc(a, b)
Link e1m2 := Arc(a, b)
Link e2 := Arc(a, e)
Link e3m1 := Arc(b, c)
Link e3m2 := Arc(b, c)
Link e4m1 := Arc(c, d)
Link e4m2 := Arc(c, d)
Link e4m3 := Arc(c, d)
Link e5 := Arc(c, e)
Link e6 := Arc(d, d)
Link e7 := Arc(e, a)
Link e8 := Arc(e, d)
Link e9 := Arc(e, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,qn=`Vertex a, b, c, d, e, f

Link e1 := Arc(a, b)
Link e2 := Arc(b, a)
Link e3m1 := Arc(b, c)
Link e3m2 := Arc(b, c)
Link e4 := Arc(c, d)
Link e5 := Arc(d, d)
Link e6m1 := Arc(e, d)
Link e6m2 := Arc(e, d)
Link e7 := Arc(f, a)
Link e8 := Arc(f, e)
Link e9 := Arc(f, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,Rn=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Edge(Chicago, Denver)
Edge(Chicago, Detroit)
Edge(Chicago, NYC)
Edge(Chicago, Washington)
Edge(Denver, LA)
Edge(Denver, SF)
Edge(Detroit, NYC)
Edge(LA, SF)
Edge(NYC, Washington)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,Dn=`Vertex s1, s2, s3, s4, s5, s6

Arc(s1, s3)
Arc(s1, s4)
Arc(s1, s6)
Arc(s2, s4)
Arc(s2, s5)
Arc(s2, s6)
Arc(s3, s6)
Arc(s4, s5)
Arc(s4, s6)

Label s1 "𝑆₁"
Label s2 "𝑆₂"
Label s3 "𝑆₃"
Label s4 "𝑆₄"
Label s5 "𝑆₅"
Label s6 "𝑆₆"
`,$n=`Vertex Raccoon, Hawk, Owl
Vertex Opossum, Squirrel, Crow
Vertex Shrew, Mouse, Woodpecker

Edge(Raccoon, Hawk)
Edge(Raccoon, Owl)
Edge(Raccoon, Squirrel)
Edge(Hawk, Owl)
Edge(Hawk, Crow)
Edge(Owl, Crow)
Edge(Opossum, Squirrel)
Edge(Opossum, Shrew)
Edge(Opossum, Woodpecker)
Edge(Squirrel, Crow)
Edge(Squirrel, Woodpecker)
Edge(Shrew, Mouse)
Edge(Shrew, Woodpecker)

AutoLabel All
`,Wn=`Vertex a, b, c, d, e, f, g, h, i

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(a, e)
Edge(b, c)
Edge(b, d)
Edge(b, e)
Edge(c, d)
Edge(c, e)
Edge(d, e)
Edge(d, f)
Edge(d, h)
Edge(d, i)
Edge(e, f)
Edge(e, g)
Edge(e, h)
Edge(e, i)
Edge(g, h)
Edge(h, i)

Label a "Q9Y3A5"
Label b "RRP43"
Label c "RRP42"
Label d "RRP4"
Label e "RRP41"
Label f "RRP44"
Label g "RRP40"
Label h "RRP46"
Label i "PM/Sci2"
`,Fn=`Vertex team1, team2, team3, team4, team5, team6

Arc(team1, team2)
Arc(team1, team3)
Arc(team1, team4)
Arc(team1, team5)
Arc(team1, team6)
Arc(team2, team3)
Arc(team2, team4)
Arc(team4, team3)
Arc(team5, team2)
Arc(team5, team3)
Arc(team5, team4)
Arc(team5, team6)
Arc(team6, team2)
Arc(team6, team3)
Arc(team6, team4)

Label team1 "Team 1"
Label team2 "Team 2"
Label team3 "Team 3"
Label team4 "Team 4"
Label team5 "Team 5"
Label team6 "Team 6"
`,jn=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Link e1m1 := Edge(Chicago, Denver)
Link e1m2 := Edge(Chicago, Denver)
Link e1m3 := Edge(Chicago, Denver)
Link e2 := Edge(Chicago, Detroit)
Link e3m1 := Edge(Chicago, NYC)
Link e3m2 := Edge(Chicago, NYC)
Link e4 := Edge(Chicago, Washington)
Link e5m1 := Edge(Denver, LA)
Link e5m2 := Edge(Denver, LA)
Link e6 := Edge(Denver, SF)
Link e7 := Edge(Detroit, NYC)
Link e8 := Edge(LA, SF)
Link e9m1 := Edge(NYC, Washington)
Link e9m2 := Edge(NYC, Washington)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,Hn=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Link e1m1 := Edge(Chicago, Denver)
Link e1m2 := Edge(Chicago, Denver)
Link e1m3 := Edge(Chicago, Denver)
Link e2 := Edge(Chicago, Detroit)
Link e3m1 := Edge(Chicago, NYC)
Link e3m2 := Edge(Chicago, NYC)
Link e4 := Edge(Chicago, Washington)
Link e5m1 := Edge(Denver, LA)
Link e5m2 := Edge(Denver, LA)
Link e6 := Edge(Denver, SF)
Link e7 := Edge(Detroit, NYC)
Link e8 := Edge(LA, SF)
Link e9m1 := Edge(NYC, Washington)
Link e9m2 := Edge(NYC, Washington)

Link e10 := Edge(Chicago, Chicago)
Link e11 := Edge(Denver, Denver)
Link e12 := Edge(Detroit, Detroit)
Link e13 := Edge(LA, LA)
Link e14 := Edge(NYC, NYC)
Link e15 := Edge(SF, SF)
Link e16 := Edge(Washington, Washington)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,zn=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Arc(Chicago, Denver)
Arc(Chicago, Detroit)
Arc(Chicago, NYC)
Arc(Chicago, Washington)
Arc(Denver, Chicago)
Arc(Denver, LA)
Arc(Detroit, Chicago)
Arc(Detroit, NYC)
Arc(LA, Denver)
Arc(SF, Denver)
Arc(SF, LA)
Arc(Washington, Chicago)
Arc(Washington, NYC)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,Un=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Link e1m1 := Arc(Chicago, Denver)
Link e1m2 := Arc(Chicago, Denver)
Link e2 := Arc(Chicago, Detroit)
Link e3m1 := Arc(Chicago, NYC)
Link e3m2 := Arc(Chicago, NYC)
Link e4 := Arc(Chicago, Washington)
Link e5m1 := Arc(Denver, Chicago)
Link e5m2 := Arc(Denver, Chicago)
Link e6 := Arc(Denver, LA)
Link e7 := Arc(Denver, SF)
Link e8 := Arc(Detroit, NYC)
Link e9 := Arc(LA, Denver)
Link e10 := Arc(LA, SF)
Link e11m1 := Arc(SF, Denver)
Link e11m2 := Arc(SF, Denver)
Link e12 := Arc(SF, LA)
Link e13 := Arc(Washington, Chicago)
Link e14m1 := Arc(Washington, NYC)
Link e14m2 := Arc(Washington, NYC)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,Kn=`Vertex Eduardo
Vertex Jan, Paula, Todd, Kamlesh
Vertex Kamini
Vertex Amy, Ching
Vertex Lila, Liz, Steve
Vertex Joel, Gail
Vertex Koko
Vertex Kari, Shaquira

Edge(Eduardo, Paula)
Edge(Kamini, Jan)
Edge(Jan, Joel)
Edge(Jan, Lila)
Edge(Jan, Paula)
Edge(Paula, Lila)
Edge(Paula, Joel)
Edge(Paula, Liz)
Edge(Paula, Amy)
Edge(Paula, Todd)
Edge(Todd, Amy)
Edge(Todd, Steve)
Edge(Todd, Kamlesh)
Edge(Kamlesh, Ching)
Edge(Amy, Liz)
Edge(Amy, Steve)
Edge(Liz, Steve)
Edge(Lila, Joel)
Edge(Lila, Liz)
Edge(Steve, Shaquira)
Edge(Steve, Koko)
Edge(Joel, Kari)
Edge(Joel, Gail)
Edge(Gail, Shaquira)

AutoLabel All
`,Xn=`Vertex Linda, Brian
Vertex Deborah, Fred, Yvonne

Arc(Brian, Linda)
Arc(Brian, Yvonne)
Arc(Deborah, Linda)
Arc(Deborah, Brian)
Arc(Deborah, Fred)
Arc(Fred, Brian)
Arc(Yvonne, Brian)
Arc(Yvonne, Fred)

AutoLabel All
`,Yn=`Vertex a, b, c, d, e, f, g

Link e1 := Arc(a, b)
Link e2m1 := Arc(a, d)
Link e2m2 := Arc(a, d)
Link e3m1 := Arc(a, g)
Link e3m2 := Arc(a, g)
Link e4m1 := Arc(b, e)
Link e4m2 := Arc(b, e)
Link e4m3 := Arc(b, e)
Link e5 := Arc(c, f)
Link e6m1 := Arc(d, a)
Link e6m2 := Arc(d, a)
Link e7 := Arc(d, g)
Link e8m1 := Arc(e, b)
Link e8m2 := Arc(e, b)
Link e9m1 := Arc(f, g)
Link e9m2 := Arc(f, g)
Link e10 := Arc(g, f)

Label a "732-555-1001"
Label b "732-555-1234"
Label c "732-555-4444"
Label d "732-555-0069"
Label e "732-555-9876"
Label f "732-555-0011"
Label g "732-555-6666"
`,Zn=`Vertex a, b, c, d, e, f, g

Link e1 := Edge(a, b)
Link e2m1 := Edge(a, d)
Link e2m2 := Edge(a, d)
Link e3m1 := Edge(a, g)
Link e3m2 := Edge(a, g)
Link e4m1 := Edge(b, e)
Link e4m2 := Edge(b, e)
Link e4m3 := Edge(b, e)
Link e5 := Edge(c, f)
Link e6m1 := Edge(d, a)
Link e6m2 := Edge(d, a)
Link e7 := Edge(d, g)
Link e8m1 := Edge(e, b)
Link e8m2 := Edge(e, b)
Link e9m1 := Edge(f, g)
Link e9m2 := Edge(f, g)
Link e10 := Edge(g, f)

Label a "732-555-1001"
Label b "732-555-1234"
Label c "732-555-4444"
Label d "732-555-0069"
Label e "732-555-9876"
Label f "732-555-0011"
Label g "732-555-6666"
`,Jn=`Vertex main
Vertex display, parser, protocol
Vertex AST, page, network

Arc(main, display)
Arc(main, parser)
Arc(main, protocol)
Arc(main, AST)
Arc(display, AST)
Arc(parser, AST)
Arc(parser, page)
Arc(protocol, page)
Arc(protocol, network)

AutoLabel All
`,ne={"ex3.substance":Gn,"ex4.substance":In,"ex5.substance":Vn,"ex6.substance":Nn,"ex7.substance":_n,"ex8.substance":Qn,"ex9.substance":qn,"fig1.substance":Rn,"fig10.substance":Dn,"fig11.substance":$n,"fig12.substance":Wn,"fig13.substance":Fn,"fig2.substance":jn,"fig3.substance":Hn,"fig4.substance":zn,"fig5.substance":Un,"fig6.substance":Kn,"fig7.substance":Xn,"fig8a.substance":Yn,"fig8b.substance":Zn,"fig9.substance":Jn},ee=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(b, c)
Edge(c, d)
Edge(d, e)
Edge(e, f)
Edge(f, a)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,te=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, f)
Edge(b, c)
Edge(b, e)
Edge(b, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,oe=`Vertex a, b, c, d, e

Link e1 := Edge(a, a)
Link e2m1 := Edge(a, b)
Link e2m2 := Edge(a, b)
Link e2m3 := Edge(a, b)
Link e3 := Edge(a, e)
Link e4 := Edge(b, c)
Link e5 := Edge(b, d)
Link e6 := Edge(b, e)
Link e7 := Edge(c, c)
Link e8m1 := Edge(c, d)
Link e8m2 := Edge(c, d)
Link e8m3 := Edge(c, d)
Link e9 := Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,re=`Vertex a, b, c, d, e

Edge(a, e)
Edge(b, e)
Edge(c, e)
Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,ae=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, d)
Edge(a, e)
Edge(b, c)
Edge(c, d)
Edge(c, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,le=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, d)
Edge(a, e)
Edge(b, c)
Edge(b, f)
Edge(c, d)
Edge(c, e)
Edge(c, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,ie=`Vertex a, b, c, d, e, f

Edge(a, c)
Edge(a, f)
Edge(b, c)
Edge(b, f)
Edge(c, d)
Edge(c, e)
Edge(d, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,se=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, f)
Edge(b, d)
Edge(b, e)
Edge(c, d)
Edge(c, f)
Edge(d, e)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,ce=`Vertex a, b, c, d, e, f, g, h, i

Link e1 := Edge(a, c)
Link e2 := Edge(a, e)
Link e3 := Edge(a, i)
Link e4 := Edge(b, e)
Link e5 := Edge(b, h)
Link e6 := Edge(c, e)
Link e7 := Edge(c, i)
Link e8m1 := Edge(e, g)
Link e8m2 := Edge(e, g)
Link e8m3 := Edge(e, g)
Link e9 := Edge(h, i)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
Label h "ℎ"
Label i "𝑖"
`,de=`Vertex a, b, c, d

Edge(a, b)
Edge(a, c)
Edge(a, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,pe=`Vertex a, b, c, d

Link e1 := Arc(a, a)
Link e2 := Arc(b, a)
Link e3 := Arc(b, c)
Link e4 := Arc(c, d)
Link e5 := Arc(d, a)
Link e6 := Arc(d, b)
Link e7 := Arc(d, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,ge=`Vertex a, b, c, d

Link e1m1 := Arc(a, b)
Link e1m2 := Arc(a, b)
Link e2 := Arc(b, b)
Link e3m1 := Arc(b, c)
Link e3m2 := Arc(b, c)
Link e4 := Arc(c, a)
Link e5 := Arc(d, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,fe=`Vertex a, b, c, d, e

Link e1 := Arc(a, b)
Link e2m1 := Arc(b, a)
Link e2m2 := Arc(b, a)
Link e3m1 := Arc(b, c)
Link e3m2 := Arc(b, c)
Link e4 := Arc(b, d)
Link e5m1 := Arc(c, a)
Link e5m2 := Arc(c, a)
Link e6m1 := Arc(c, d)
Link e6m2 := Arc(c, d)
Link e6m3 := Arc(c, d)
Link e7m1 := Arc(d, a)
Link e7m2 := Arc(d, a)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,ue=`Vertex Alvarez, Berkowitz, Chen, Davis
Vertex requirements, architecture, implementation, testing

Edge(Alvarez, requirements)
Edge(Alvarez, testing)
Edge(Berkowitz, architecture)
Edge(Berkowitz, implementation)
Edge(Berkowitz, testing)
Edge(Chen, requirements)
Edge(Chen, architecture)
Edge(Chen, implementation)
Edge(Davis, requirements)

AutoLabel All
`,he=`Vertex Washington, Xuan, Ybarra, Ziegler
Vertex requirements, architecture, implementation, testing

Edge(Washington, architecture)
Edge(Xuan, requirements)
Edge(Xuan, implementation)
Edge(Xuan, testing)
Edge(Ybarra, architecture)
Edge(Ziegler, requirements)
Edge(Ziegler, architecture)
Edge(Ziegler, testing)

AutoLabel All
`,be=`Vertex a, b, c, d, e, f, g, h, i

Edge(a, e)
Edge(b, e)
Edge(c, e)
Edge(d, e)
Edge(e, f)
Edge(e, g)
Edge(e, h)
Edge(e, i)
`,ke=`Vertex a, b, c, d, e, f, g, h

Edge(a, b)
Edge(b, c)
Edge(c, d)
Edge(d, e)
Edge(e, f)
Edge(f, g)
Edge(g, h)
Edge(h, a)
`,xe=`Vertex a, b, c, d, e, f, g, h, i

Edge(a, b)
Edge(b, c)
Edge(c, d)
Edge(d, e)
Edge(e, f)
Edge(f, g)
Edge(g, h)
Edge(h, a)

Edge(a, i)
Edge(b, i)
Edge(c, i)
Edge(d, i)
Edge(e, i)
Edge(f, i)
Edge(g, i)
Edge(h, i)
`,ve=`Vertex p1, p2, p3, p4, p5, p6

Edge(p1, p2)
Edge(p2, p3)
Edge(p3, p4)
Edge(p4, p5)
Edge(p5, p6)

Label p1 "𝑃₁"
Label p2 "𝑃₂"
Label p3 "𝑃₃"
Label p4 "𝑃₄"
Label p5 "𝑃₅"
Label p6 "𝑃₆"
`,me=`Vertex p00, p01, p02, p03
Vertex p10, p11, p12, p13
Vertex p20, p21, p22, p23
Vertex p30, p31, p32, p33

Edge(p00, p01)
Edge(p01, p02)
Edge(p02, p03)

Edge(p10, p11)
Edge(p11, p12)
Edge(p12, p13)

Edge(p20, p21)
Edge(p21, p22)
Edge(p22, p23)

Edge(p30, p31)
Edge(p31, p32)
Edge(p32, p33)

Edge(p00, p10)
Edge(p10, p20)
Edge(p20, p30)

Edge(p01, p11)
Edge(p11, p21)
Edge(p21, p31)

Edge(p02, p12)
Edge(p12, p22)
Edge(p22, p32)

Edge(p03, p13)
Edge(p13, p23)
Edge(p23, p33)

Label p00 "𝑃(0, 0)"
Label p01 "𝑃(0, 1)"
Label p02 "𝑃(0, 2)"
Label p03 "𝑃(0, 3)"

Label p10 "𝑃(1, 0)"
Label p11 "𝑃(1, 1)"
Label p12 "𝑃(1, 2)"
Label p13 "𝑃(1, 3)"

Label p20 "𝑃(2, 0)"
Label p21 "𝑃(2, 1)"
Label p22 "𝑃(2, 2)"
Label p23 "𝑃(2, 3)"

Label p30 "𝑃(3, 0)"
Label p31 "𝑃(3, 1)"
Label p32 "𝑃(3, 2)"
Label p33 "𝑃(3, 3)"
`,ye=`Vertex p0, p1, p2, p3, p4, p5, p6, p7

Edge(p0, p1)
Edge(p0, p2)
Edge(p0, p4)
Edge(p1, p3)
Edge(p1, p5)
Edge(p2, p3)
Edge(p2, p6)
Edge(p3, p7)
Edge(p4, p5)
Edge(p4, p6)
Edge(p5, p7)
Edge(p6, p7)

Label p0 "𝑃₀"
Label p1 "𝑃₁"
Label p2 "𝑃₂"
Label p3 "𝑃₃"
Label p4 "𝑃₄"
Label p5 "𝑃₅"
Label p6 "𝑃₆"
Label p7 "𝑃₇"
`,we=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, d)
Edge(b, c)
Edge(b, d)
Edge(b, e)
Edge(b, f)
Edge(c, e)
Edge(c, f)
Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,Ce=`Vertex a, b, c, d, e, f, g

Edge(a, b)
Edge(a, f)
Edge(b, c)
Edge(b, e)
Edge(b, f)
Edge(c, d)
Edge(c, e)
Edge(c, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
`,Le=`Vertex a, b, c, d, e

Link e1 := Edge(a, b)
Link e2 := Edge(a, d)
Link e3m1 := Edge(a, e)
Link e3m2 := Edge(a, e)
Link e4 := Edge(b, b)
Link e5 := Edge(b, c)
Link e6 := Edge(b, d)
Link e7 := Edge(b, e)
Link e8m1 := Edge(d, e)
Link e8m2 := Edge(d, e)
Link e8m3 := Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Ae=`Vertex a, b, c, d, e, f

Link e1 := Arc(a, a)
Link e2 := Arc(a, b)
Link e3 := Arc(a, c)
Link e4 := Arc(a, e)
Link e5 := Arc(b, d)
Link e6 := Arc(c, b)
Link e7 := Arc(c, c)
Link e8 := Arc(d, c)
Link e9 := Arc(d, e)
Link e10 := Arc(e, a)
Link e11 := Arc(e, d)
Link e12 := Arc(e, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,Se=`Vertex k11
Vertex k21, k22
Vertex k31, k32, k33
Vertex k41, k42, k43, k44
Vertex k51, k52, k53, k54, k55
Vertex k61, k62, k63, k64, k65, k66

Edge(k21, k22)

Edge(k31, k32)
Edge(k31, k33)
Edge(k32, k33)

Edge(k41, k42)
Edge(k41, k43)
Edge(k41, k44)
Edge(k42, k43)
Edge(k42, k44)
Edge(k43, k44)

Edge(k51, k52)
Edge(k51, k53)
Edge(k51, k54)
Edge(k51, k55)
Edge(k52, k53)
Edge(k52, k54)
Edge(k52, k55)
Edge(k53, k54)
Edge(k53, k55)
Edge(k54, k55)

Edge(k61, k62)
Edge(k61, k63)
Edge(k61, k64)
Edge(k61, k65)
Edge(k61, k66)
Edge(k62, k63)
Edge(k62, k64)
Edge(k62, k65)
Edge(k62, k66)
Edge(k63, k64)
Edge(k63, k65)
Edge(k63, k66)
Edge(k64, k65)
Edge(k64, k66)
Edge(k65, k66)
`,Pe=`Vertex c31, c32, c33
Vertex c41, c42, c43, c44
Vertex c51, c52, c53, c54, c55
Vertex c61, c62, c63, c64, c65, c66

Edge(c31, c32)
Edge(c32, c33)
Edge(c33, c31)

Edge(c41, c42)
Edge(c42, c43)
Edge(c43, c44)
Edge(c44, c41)

Edge(c51, c52)
Edge(c52, c53)
Edge(c53, c54)
Edge(c54, c55)
Edge(c55, c51)

Edge(c61, c62)
Edge(c62, c63)
Edge(c63, c64)
Edge(c64, c65)
Edge(c65, c66)
Edge(c66, c61)
`,Ee=`Vertex c30, c31, c32, c33
Vertex c40, c41, c42, c43, c44
Vertex c50, c51, c52, c53, c54, c55
Vertex c60, c61, c62, c63, c64, c65, c66

Edge(c31, c32)
Edge(c32, c33)
Edge(c33, c31)
Edge(c30, c31)
Edge(c30, c32)
Edge(c30, c33)

Edge(c41, c42)
Edge(c42, c43)
Edge(c43, c44)
Edge(c44, c41)
Edge(c40, c41)
Edge(c40, c42)
Edge(c40, c43)
Edge(c40, c44)

Edge(c51, c52)
Edge(c52, c53)
Edge(c53, c54)
Edge(c54, c55)
Edge(c55, c51)
Edge(c50, c51)
Edge(c50, c52)
Edge(c50, c53)
Edge(c50, c54)
Edge(c50, c55)

Edge(c61, c62)
Edge(c62, c63)
Edge(c63, c64)
Edge(c64, c65)
Edge(c65, c66)
Edge(c66, c61)
Edge(c60, c61)
Edge(c60, c62)
Edge(c60, c63)
Edge(c60, c64)
Edge(c60, c65)
Edge(c60, c66)
`,Te=`Vertex q10, q11
Vertex q200, q201, q210, q211
Vertex q3000, q3001, q3010, q3011, q3100, q3101, q3110, q3111

Edge(q10, q11)

Edge(q200, q201)
Edge(q200, q210)
Edge(q201, q211)
Edge(q210, q211)

Edge(q3000, q3001)
Edge(q3000, q3010)
Edge(q3000, q3100)
Edge(q3001, q3011)
Edge(q3001, q3101)
Edge(q3010, q3011)
Edge(q3010, q3110)
Edge(q3011, q3111)
Edge(q3100, q3101)
Edge(q3100, q3110)
Edge(q3101, q3111)
Edge(q3110, q3111)

Label q10 "0"
Label q11 "1"

Label q200 "00"
Label q201 "01"
Label q210 "10"
Label q211 "11"

Label q3000 "000"
Label q3001 "001"
Label q3010 "010"
Label q3011 "011"
Label q3100 "100"
Label q3101 "101"
Label q3110 "110"
Label q3111 "111"
`,Oe=`Vertex a, b, c, d, e, f, g

Edge(a, c)
Edge(a, e)
Edge(a, f)
Edge(a, g)
Edge(b, c)
Edge(b, e)
Edge(b, f)
Edge(c, d)
Edge(d, e)
Edge(d, f)
Edge(d, g)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
`,Be=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, e)
Edge(a, f)
Edge(b, c)
Edge(b, d)
Edge(b, e)
Edge(b, f)
Edge(c, d)
Edge(c, f)
Edge(d, e)
Edge(d, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,Me=`Vertex k23t1, k23t2
Vertex k23b1, k23b2, k23b3

Edge(k23t1, k23b1)
Edge(k23t1, k23b2)
Edge(k23t1, k23b3)

Edge(k23t2, k23b1)
Edge(k23t2, k23b2)
Edge(k23t2, k23b3)

Vertex k33t1, k33t2, k33t3
Vertex k33b1, k33b2, k33b3

Edge(k33t1, k33b1)
Edge(k33t1, k33b2)
Edge(k33t1, k33b3)

Edge(k33t2, k33b1)
Edge(k33t2, k33b2)
Edge(k33t2, k33b3)

Edge(k33t3, k33b1)
Edge(k33t3, k33b2)
Edge(k33t3, k33b3)

Vertex k35t1, k35t2, k35t3
Vertex k35b1, k35b2, k35b3, k35b4, k35b5

Edge(k35t1, k35b1)
Edge(k35t1, k35b2)
Edge(k35t1, k35b3)
Edge(k35t1, k35b4)
Edge(k35t1, k35b5)

Edge(k35t2, k35b1)
Edge(k35t2, k35b2)
Edge(k35t2, k35b3)
Edge(k35t2, k35b4)
Edge(k35t2, k35b5)

Edge(k35t3, k35b1)
Edge(k35t3, k35b2)
Edge(k35t3, k35b3)
Edge(k35t3, k35b4)
Edge(k35t3, k35b5)

Vertex k26t1, k26t2
Vertex k26b1, k26b2, k26b3, k26b4, k26b5, k26b6

Edge(k26t1, k26b1)
Edge(k26t1, k26b2)
Edge(k26t1, k26b3)
Edge(k26t1, k26b4)
Edge(k26t1, k26b5)
Edge(k26t1, k26b6)

Edge(k26t2, k26b1)
Edge(k26t2, k26b2)
Edge(k26t2, k26b3)
Edge(k26t2, k26b4)
Edge(k26t2, k26b5)
Edge(k26t2, k26b6)
`,Ge={"eg9.substance":ee,"ex1.substance":te,"ex2.substance":oe,"ex21.substance":re,"ex22.substance":ae,"ex23.substance":le,"ex24.substance":ie,"ex25.substance":se,"ex3.substance":ce,"ex51.substance":de,"ex7.substance":pe,"ex8.substance":ge,"ex9.substance":fe,"fig10a.substance":ue,"fig10b.substance":he,"fig11a.substance":be,"fig11b.substance":ke,"fig11c.substance":xe,"fig12.substance":ve,"fig13.substance":me,"fig14.substance":ye,"fig16b.substance":we,"fig1G.substance":Ce,"fig1H.substance":Le,"fig2.substance":Ae,"fig3.substance":Se,"fig4.substance":Pe,"fig5.substance":Ee,"fig6.substance":Te,"fig8G.substance":Oe,"fig8H.substance":Be,"fig9.substance":Me},Ie=`Vertex a, b, c, d

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, d)
Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Ve=`Vertex a, b, c, d

Link e1 := Edge(a, c)
Link e2 := Edge(b, c)
Link e3m1 := Edge(b, d)
Link e3m2 := Edge(b, d)
Link e4 := Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Ne=`Vertex a, b, c, d

Link e1m1 := Edge(a, b)
Link e1m2 := Edge(a, b)
Link e1m3 := Edge(a, b)
Link e2 := Edge(a, d)
Link e3 := Edge(b, c)
Link e4m1 := Edge(c, d)
Link e4m2 := Edge(c, d)
Link e4m3 := Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,_e=`Vertex a, b, c, d

Link e1 := Edge(a, a)
Link e2m1 := Edge(a, c)
Link e2m2 := Edge(a, c)
Link e3 := Edge(a, d)
Link e4 := Edge(b, b)
Link e5 := Edge(b, c)
Link e6m1 := Edge(b, d)
Link e6m2 := Edge(b, d)
Link e7 := Edge(c, c)
Link e8 := Edge(d, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Qe=`Vertex a, b, c, d

Link e1 := Arc(a, b)
Link e2 := Arc(b, b)
Link e3 := Arc(b, c)
Link e4 := Arc(c, b)
Link e5 := Arc(c, c)
Link e6 := Arc(c, d)
Link e7 := Arc(d, a)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,qe=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, d)
Edge(b, d)
Edge(b, e)
Edge(c, d)
Edge(c, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Re=`Vertex a, b, c, d

Link e1 := Arc(a, a)
Link e2 := Arc(a, b)
Link e3 := Arc(a, c)
Link e4 := Arc(a, d)
Link e5 := Arc(b, b)
Link e6 := Arc(b, d)
Link e7 := Arc(c, a)
Link e8 := Arc(c, c)
Link e9 := Arc(d, a)
Link e10 := Arc(d, b)
Link e11 := Arc(d, c)
Link e12 := Arc(d, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,De=`Vertex a, b, c, d

Link e1 := Arc(a, a)
Link e2 := Arc(a, b)
Link e3m1 := Arc(a, c)
Link e3m2 := Arc(a, c)
Link e4 := Arc(a, d)
Link e5 := Arc(b, a)
Link e6m1 := Arc(b, d)
Link e6m2 := Arc(b, d)
Link e7 := Arc(c, a)
Link e8 := Arc(c, c)
Link e9 := Arc(c, d)
Link e10m1 := Arc(d, b)
Link e11m1 := Arc(d, b)
Link e12 := Arc(d, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,$e=`Vertex a, b, c, d

Link e1 := Arc(a, a)
Link e2 := Arc(a, b)
Link e3 := Arc(a, c)
Link e4 := Arc(a, d)
Link e5 := Arc(b, d)
Link e6 := Arc(c, a)
Link e7 := Arc(c, b)
Link e8 := Arc(d, b)
Link e9 := Arc(d, c)
Link e10 := Arc(d, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,We=`Vertex u1, u2, u3, u4, u5

Edge(u1, u2)
Edge(u2, u3)
Edge(u3, u4)
Edge(u4, u5)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"

Vertex v1, v2, v3, v4, v5

Edge(v1, v2)
Edge(v2, v4)
Edge(v3, v5)
Edge(v4, v5)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
`,Fe=`Vertex u1, u2, u3, u4, u5

Edge(u1, u2)
Edge(u2, u3)
Edge(u3, u4)
Edge(u4, u5)
Edge(u5, u1)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"

Vertex v1, v2, v3, v4, v5

Edge(v1, v3)
Edge(v1, v4)
Edge(v2, v4)
Edge(v2, v5)
Edge(v3, v5)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
`,je=`Vertex u1, u2, u3, u4, u5

Edge(u1, u2)
Edge(u1, u4)
Edge(u1, u5)
Edge(u2, u3)
Edge(u3, u4)
Edge(u3, u5)
Edge(u4, u5)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"

Vertex v1, v2, v3, v4, v5

Edge(v1, v3)
Edge(v1, v5)
Edge(v2, v3)
Edge(v2, v4)
Edge(v2, v5)
Edge(v3, v4)
Edge(v4, v5)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
`,He=`Vertex u1, u2, u3, u4, u5, u6, u7

Edge(u1, u2)
Edge(u2, u3)
Edge(u3, u4)
Edge(u4, u5)
Edge(u5, u6)
Edge(u6, u7)
Edge(u7, u1)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"
Label u7 "𝑢₇"

Vertex v1, v2, v3, v4, v5, v6, v7

Edge(v1, v3)
Edge(v1, v6)
Edge(v2, v4)
Edge(v2, v7)
Edge(v3, v5)
Edge(v4, v6)
Edge(v5, v7)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
Label v7 "𝑣₇"
`,ze=`Vertex u1, u2, u3, u4, u5

Edge(u1, u2)
Edge(u1, u4)
Edge(u1, u5)
Edge(u2, u3)
Edge(u2, u4)
Edge(u2, u5)
Edge(u3, u4)
Edge(u4, u5)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"

Vertex v1, v2, v3, v4, v5

Edge(v1, v3)
Edge(v1, v4)
Edge(v1, v5)
Edge(v2, v3)
Edge(v2, v5)
Edge(v3, v4)
Edge(v3, v5)
Edge(v4, v5)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
`,Ue=`Vertex u1, u2, u3, u4, u5, u6

Edge(u1, u2)
Edge(u1, u4)
Edge(u1, u6)
Edge(u2, u3)
Edge(u2, u6)
Edge(u3, u4)
Edge(u3, u5)
Edge(u4, u5)
Edge(u5, u6)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"

Vertex v1, v2, v3, v4, v5, v6

Edge(v1, v2)
Edge(v1, v4)
Edge(v1, v5)
Edge(v2, v3)
Edge(v2, v5)
Edge(v3, v4)
Edge(v3, v6)
Edge(v4, v6)
Edge(v5, v6)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
`,Ke=`Vertex a, b, c, d, e

Link e1 := Arc(a, b)
Link e2 := Arc(a, d)
Link e3 := Arc(b, a)
Link e4 := Arc(b, c)
Link e5 := Arc(b, d)
Link e6 := Arc(b, e)
Link e7 := Arc(c, b)
Link e8 := Arc(c, c)
Link e9 := Arc(d, a)
Link e10 := Arc(d, e)
Link e11 := Arc(e, c)
Link e12 := Arc(e, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Xe=`Vertex u1, u2, u3, u4, u5, u6

Edge(u1, u2)
Edge(u1, u5)
Edge(u1, u6)
Edge(u2, u3)
Edge(u2, u6)
Edge(u3, u4)
Edge(u4, u5)
Edge(u5, u6)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"

Vertex v1, v2, v3, v4, v5, v6

Edge(v1, v2)
Edge(v1, v6)
Edge(v2, v5)
Edge(v2, v3)
Edge(v2, v6)
Edge(v3, v4)
Edge(v3, v6)
Edge(v4, v5)
Edge(v5, v6)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
`,Ye=`Vertex u1, u2, u3, u4, u5, u6, u7, u8

Edge(u1, u2)
Edge(u2, u3)
Edge(u3, u4)
Edge(u3, u5)
Edge(u5, u6)
Edge(u6, u7)
Edge(u6, u8)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"
Label u7 "𝑢₇"
Label u8 "𝑢₈"

Vertex v1, v2, v3, v4, v5, v6, v7, v8

Edge(v1, v2)
Edge(v2, v3)
Edge(v2, v4)
Edge(v4, v5)
Edge(v5, v6)
Edge(v6, v7)
Edge(v6, v8)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
Label v7 "𝑣₇"
Label v8 "𝑣₈"
`,Ze=`Vertex u1, u2, u3, u4, u5, u6, u7, u8, u9, u10

Edge(u1, u2)
Edge(u2, u3)
Edge(u2, u6)
Edge(u2, u8)
Edge(u3, u4)
Edge(u3, u7)
Edge(u3, u9)
Edge(u4, u5)
Edge(u4, u10)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"
Label u7 "𝑢₇"
Label u8 "𝑢₈"
Label u9 "𝑢₉"
Label u10 "𝑢₁₀"

Vertex v1, v2, v3, v4, v5, v6, v7, v8, v9, v10

Edge(v1, v2)
Edge(v2, v3)
Edge(v2, v6)
Edge(v2, v8)
Edge(v3, v4)
Edge(v3, v9)
Edge(v4, v5)
Edge(v4, v7)
Edge(v4, v10)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
Label v7 "𝑣₇"
Label v8 "𝑣₈"
Label v9 "𝑣₉"
Label v10 "𝑣₁₀"
`,Je=`Vertex u1, u2, u3, u4, u5, u6, u7, u8, u9, u10

Edge(u1, u2)
Edge(u1, u5)
Edge(u1, u10)
Edge(u2, u3)
Edge(u2, u9)
Edge(u3, u4)
Edge(u3, u8)
Edge(u4, u5)
Edge(u4, u7)
Edge(u5, u6)
Edge(u6, u8)
Edge(u6, u9)
Edge(u7, u9)
Edge(u7, u10)
Edge(u8, u10)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"
Label u7 "𝑢₇"
Label u8 "𝑢₈"
Label u9 "𝑢₉"
Label u10 "𝑢₁₀"

Vertex v1, v2, v3, v4, v5, v6, v7, v8, v9, v10

Edge(v1, v2)
Edge(v1, v6)
Edge(v1, v9)
Edge(v2, v3)
Edge(v2, v8)
Edge(v3, v4)
Edge(v3, v7)
Edge(v4, v5)
Edge(v4, v9)
Edge(v5, v6)
Edge(v5, v8)
Edge(v6, v7)
Edge(v7, v10)
Edge(v8, v10)
Edge(v9, v10)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
Label v7 "𝑣₇"
Label v8 "𝑣₈"
Label v9 "𝑣₉"
Label v10 "𝑣₁₀"
`,nt=`Vertex u1, u2, u3, u4, u5, u6, u7, u8

Edge(u1, u2)
Edge(u1, u4)
Edge(u1, u5)
Edge(u1, u6)
Edge(u1, u8)
Edge(u2, u3)
Edge(u2, u5)
Edge(u2, u6)
Edge(u2, u7)
Edge(u3, u4)
Edge(u3, u6)
Edge(u3, u7)
Edge(u3, u8)
Edge(u4, u5)
Edge(u4, u7)
Edge(u4, u8)
Edge(u5, u6)
Edge(u5, u8)
Edge(u6, u7)
Edge(u7, u8)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"
Label u7 "𝑢₇"
Label u8 "𝑢₈"

Vertex v1, v2, v3, v4, v5, v6, v7, v8

Edge(v1, v2)
Edge(v1, v3)
Edge(v1, v5)
Edge(v1, v7)
Edge(v2, v3)
Edge(v2, v6)
Edge(v2, v8)
Edge(v3, v4)
Edge(v3, v7)
Edge(v4, v5)
Edge(v4, v8)
Edge(v5, v6)
Edge(v6, v7)
Edge(v7, v8)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
Label v7 "𝑣₇"
Label v8 "𝑣₈"
`,et=`Vertex a, b, c, d

Edge(a, b)
Edge(a, d)
Edge(b, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,tt=`Vertex u1, u2, u3, u4

Link eu1 := Arc(u1, u3)
Link eu2 := Arc(u2, u1)
Link eu3 := Arc(u2, u2)
Link eu4 := Arc(u2, u4)
Link eu5 := Arc(u3, u1)
Link eu6 := Arc(u3, u2)
Link eu7 := Arc(u3, u4)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"

Vertex v1, v2, v3, v4

Link ev1 := Arc(v2, v1)
Link ev2 := Arc(v2, v3)
Link ev3 := Arc(v2, v4)
Link ev4 := Arc(v3, v2)
Link ev5 := Arc(v4, v1)
Link ev6 := Arc(v4, v3)
Link ev7 := Arc(v4, v4)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
`,ot=`Vertex u1, u2, u3, u4

Arc(u1, u3)
Arc(u1, u4)
Arc(u3, u2)
Arc(u4, u2)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"

Vertex v1, v2, v3, v4

Arc(v1, v2)
Arc(v2, v4)
Arc(v3, v1)
Arc(v3, v4)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
`,rt=`Vertex u1, u2, u3, u4

Arc(u1, u3)
Arc(u1, u4)
Arc(u2, u1)
Arc(u2, u3)
Arc(u3, u4)
Arc(u4, u2)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"

Vertex v1, v2, v3, v4

Arc(v1, v3)
Arc(v1, v4)
Arc(v2, v1)
Arc(v3, v2)
Arc(v3, v4)
Arc(v4, v2)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
`,at=`Vertex u1, u2, u3, u4, u5, u6

Arc(u1, u4)
Arc(u2, u5)
Arc(u3, u4)
Arc(u3, u6)
Arc(u4, u2)
Arc(u5, u1)
Arc(u5, u3)
Arc(u6, u1)
Arc(u6, u2)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"

Vertex v1, v2, v3, v4, v5, v6

Arc(v1, v2)
Arc(v1, v6)
Arc(v2, v5)
Arc(v3, v2)
Arc(v3, v4)
Arc(v4, v1)
Arc(v5, v4)
Arc(v6, v3)
Arc(v6, v5)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
`,lt=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, c)
Edge(a, e)
Edge(c, d)
Edge(c, e)
Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,it=`Vertex a, b, c, d, e, f, g, h

Edge(a, b)
Edge(a, d)
Edge(b, c)
Edge(b, f)
Edge(c, d)
Edge(d, h)
Edge(e, f)
Edge(e, h)
Edge(f, g)
Edge(g, h)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
Label h "ℎ"

Vertex s, t, u, v, w, x, y, z

Edge(s, t)
Edge(s, v)
Edge(s, w)
Edge(t, u)
Edge(u, v)
Edge(v, z)
Edge(w, x)
Edge(w, z)
Edge(x, y)
Edge(y, z)

Label s "𝑠"
Label t "𝑡"
Label u "𝑢"
Label v "𝑣"
Label w "𝑤"
Label x "𝑥"
Label y "𝑦"
Label z "𝑧"
`,st=`Vertex b, d, f, h, s, v, w, z

Edge(b, f)
Edge(d, h)
Edge(s, v)
Edge(s, w)
Edge(v, z)
Edge(w, z)

Label b "𝑏"
Label d "𝑑"
Label f "𝑓"
Label h "ℎ"
Label s "𝑠"
Label v "𝑣"
Label w "𝑤"
Label z "𝑧"
`,ct=`Vertex u1, u2, u3, u4, u5, u6

Edge(u1, u2)
Edge(u1, u4)
Edge(u2, u3)
Edge(u2, u6)
Edge(u3, u4)
Edge(u4, u5)
Edge(u5, u6)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"
Label u5 "𝑢₅"
Label u6 "𝑢₆"

Vertex v1, v2, v3, v4, v5, v6

Edge(v1, v2)
Edge(v1, v5)
Edge(v2, v3)
Edge(v3, v4)
Edge(v3, v6)
Edge(v4, v5)
Edge(v5, v6)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
Label v5 "𝑣₅"
Label v6 "𝑣₆"
`,dt=`Vertex a, b, c, d, e

Link e1 := Arc(a, b)
Link e2 := Arc(a, c)
Link e3 := Arc(a, d)
Link e4 := Arc(a, e)
Link e5 := Arc(b, b)
Link e7 := Arc(b, d)
Link e8 := Arc(c, a)
Link e9 := Arc(c, c)
Link e10 := Arc(c, e)
Link e11 := Arc(e, b)
Link e12 := Arc(e, c)
Link e13 := Arc(e, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,pt=`Vertex a, b, c, d

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,gt=`Vertex a, b, c, d

Edge(a, b)
Edge(a, c)
Edge(b, d)
Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,ft=`Vertex a, b, c, d

Link e1m1 := Edge(a, b)
Link e1m2 := Edge(a, b)
Link e1m3 := Edge(a, b)
Link e2m1 := Edge(a, d)
Link e2m2 := Edge(a, d)
Link e3 := Edge(b, c)
Link e4 := Edge(b, d)
Link e5 := Edge(c, c)
Link e6m1 := Edge(c, d)
Link e6m2 := Edge(c, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,ut=`Vertex u1, u2, u3, u4

Edge(u1, u2)
Edge(u1, u3)
Edge(u2, u4)
Edge(u3, u4)

Label u1 "𝑢₁"
Label u2 "𝑢₂"
Label u3 "𝑢₃"
Label u4 "𝑢₄"

Vertex v1, v2, v3, v4

Edge(v1, v3)
Edge(v1, v4)
Edge(v2, v3)
Edge(v2, v4)

Label v1 "𝑣₁"
Label v2 "𝑣₂"
Label v3 "𝑣₃"
Label v4 "𝑣₄"
`,ht={"ex1.substance":Ie,"ex13.substance":Ve,"ex14.substance":Ne,"ex15.substance":_e,"ex19.substance":Qe,"ex2.substance":qe,"ex20.substance":Re,"ex21.substance":De,"ex3.substance":$e,"ex34.substance":We,"ex35.substance":Fe,"ex36.substance":je,"ex37.substance":He,"ex38.substance":ze,"ex39.substance":Ue,"ex4.substance":Ke,"ex40.substance":Xe,"ex41.substance":Ye,"ex42.substance":Ze,"ex43.substance":Je,"ex44.substance":nt,"ex50.substance":et,"ex61.substance":tt,"ex62.substance":ot,"ex63.substance":rt,"ex64.substance":at,"fig1.substance":lt,"fig10.substance":it,"fig11.substance":st,"fig12.substance":ct,"fig2.substance":dt,"fig3.substance":pt,"fig4.substance":gt,"fig5.substance":ft,"fig8.substance":ut},bt=`Vertex a, b, c, d, e

Arc(b, a)
Arc(b, c)
Arc(b, e)
Arc(c, d)
Arc(d, b)
Arc(e, a)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,kt=`Vertex a, b, c, d, e

Arc(a, e)
Arc(a, d)
Arc(b, a)
Arc(b, c)
Arc(d, b)
Arc(d, c)
Arc(e, b)
Arc(e, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,xt=`Vertex a, b, c, d, e, f, g

Arc(a, c)
Arc(a, g)
Arc(b, f)
Arc(d, c)
Arc(e, b)
Arc(f, e)
Arc(g, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
`,vt=`Vertex a, b, c, d, e, f

Arc(a, f)
Arc(d, c)
Arc(e, b)
Arc(e, c)
Arc(f, b)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,mt=`Vertex a, b, c, d, e, f

Arc(a, b)
Arc(b, c)
Arc(c, a)
Arc(c, d)
Arc(d, e)
Arc(e, b)
Arc(e, f)
Arc(f, a)
Arc(f, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,yt=`Vertex a, b, c, d, e, f, g

Arc(a, g)
Arc(b, f)
Arc(c, d)
Arc(e, b)
Arc(f, e)
Arc(g, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
`,wt={"ex11a.substance":bt,"ex11b.substance":kt,"ex11c.substance":xt,"ex12a.substance":vt,"ex12b.substance":mt,"ex12c.substance":yt},Ct=`Vertex a, b, c, d

Arc(a, b)
Arc(a, d)
Arc(b, d)
Arc(b, c)
Arc(c, a)
Arc(c, d)
Arc(d, b)
Arc(d, c)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
`,Lt=`Vertex a, b, c, d, e

Arc(b, a)
Arc(a, e)
Arc(a, d)
Arc(d, b)
Arc(b, c)
Arc(c, b)
Arc(b, e)
Arc(e, b)
Arc(c, e)
Arc(e, c)
Arc(d, e)
Arc(e, d)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,At=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, c)
Edge(b, c)
Edge(c, f)
Edge(d, e)
Edge(d, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,St=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, e)
Edge(b, c)
Edge(b, d)
Edge(b, e)
Edge(c, d)
Edge(d, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Pt=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, c)
Edge(b, e)
Edge(c, e)
Edge(d, e)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,Et=`Vertex a, b, c, d, e, f, g

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, c)
Edge(b, d)
Edge(b, g)
Edge(c, d)
Edge(c, e)
Edge(d, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
`,Tt=`Vertex a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q

Edge(a, b)
Edge(a, d)
Edge(b, c)
Edge(b, j)
Edge(c, h)
Edge(d, e)
Edge(d, o)
Edge(e, f)
Edge(f, g)
Edge(f, m)
Edge(g, h)
Edge(h, q)
Edge(i, j)
Edge(i, o)
Edge(j, k)
Edge(j, p)
Edge(k, q)
Edge(l, m)
Edge(l, q)
Edge(m, n)
Edge(m, p)
Edge(n, o)
Edge(o, p)
Edge(p, q)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
Label h "ℎ"
Label i "𝑖"
Label j "𝑗"
Label k "𝑘"
Label l "𝑙"
Label m "𝑚"
Label n "𝑛"
Label o "𝑜"
Label p "𝑝"
Label q "𝑞"
`,Ot=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, c)
Edge(b, d)
Edge(b, e)
Edge(c, d)
Edge(c, e)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
`,Bt=`Vertex a, b, c, d, e, f, g, h, i

Edge(a, b)
Edge(a, d)
Edge(a, e)
Edge(b, c)
Edge(b, e)
Edge(c, e)
Edge(c, f)
Edge(d, e)
Edge(d, g)
Edge(e, f)
Edge(e, g)
Edge(e, h)
Edge(e, i)
Edge(f, i)
Edge(g, h)
Edge(h, i)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
Label h "ℎ"
Label i "𝑖"
`,Mt=`Vertex a, b, c, d, e, f, g, h, i, j

Edge(a, b)
Edge(a, e)
Edge(a, f)
Edge(b, c)
Edge(b, g)
Edge(c, d)
Edge(c, h)
Edge(c, e)
Edge(d, i)
Edge(e, j)
Edge(f, h)
Edge(f, i)
Edge(g, i)
Edge(g, j)
Edge(h, j)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
Label g "𝑔"
Label h "ℎ"
Label i "𝑖"
Label j "𝑗"
`,Gt=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, c)
Edge(b, c)
Edge(b, d)
Edge(c, e)
Edge(d, e)
`,It=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, c)
Edge(b, c)
Edge(b, d)
Edge(b, e)
Edge(c, e)
Edge(d, e)
`,Vt=`Vertex a, b, c, d, e

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, c)
Edge(b, e)
Edge(c, d)
Edge(c, e)
Edge(d, e)
`,Nt=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, c)
Edge(a, e)
Edge(b, c)
Edge(b, f)
Edge(c, d)
Edge(d, e)
Edge(d, f)
Edge(e, f)
`,_t={"ex18.substance":Ct,"ex21.substance":Lt,"ex30.substance":At,"ex31.substance":St,"ex32.substance":Pt,"ex33.substance":Et,"ex34.substance":Tt,"ex35.substance":Ot,"ex36.substance":Bt,"ex46.substance":Mt,"ex47a.substance":Gt,"ex47b.substance":It,"ex47c.substance":Vt,"ex47d.substance":Nt},Qt=`Vertex a, b, c, d, e, f

Edge(a, c)
Edge(a, d)
Edge(a, e)
Edge(a, f)
Edge(b, c)
Edge(b, d)
Edge(b, f)
Edge(c, e)
Edge(c, f)
Edge(d, f)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,qt=`Vertex a, b, c, d, e, f, g, h

Edge(a, b)
Edge(a, c)
Edge(a, e)
Edge(b, d)
Edge(b, f)
Edge(c, d)
Edge(c, g)
Edge(d, h)
Edge(e, f)
Edge(h, f)
Edge(h, g)
Edge(e, g)
`,Rt={"ex5.substance":Qt,"fig4.substance":qt},Dt={sec1:ne,sec2:Ge,sec3:ht,sec4:wt,sec5:_t,sec7:Rt},$t={"directed-multigraph.domain":An,"directed-multigraph.style":Sn,"pseudograph.domain":Pn,"pseudograph.style":En,"simple-directed-graph.domain":Tn,"simple-directed-graph.style":On,"simple-graph.domain":Bn,"simple-graph.style":Mn,textbook:Dt},Wt=`-- Draws a Cayley graph for a group G, assuming that
--   (i) all elements g of G have been declared as an Element,
--   (ii) all generator s of G have been tagged, via IsGenerator(), and
--   (iii) the group multiplication table has been specified, via IsProduct.
-- For drawing the Cayley graph, it is not strictly necessary to specify the
-- entire multiplication table: one can instead just specify the subset of
-- the table corresponding to right-multiplication by any generator.  However,
-- for other Styles in the domain Group, it may be helpful to generate Substance
-- files that specify the full table.

canvas {
    width = 240
    height = 200
}

colors {
   white = rgba( 1., 1., 1., 1. )
   lightGray = rgba( .8, .8, .8, 1. )
   mediumGray = rgba( .6, .6, .6, 1. )
   darkGray = rgba( .4, .4, .4, 1. )
}

global {
   scalar targetEdgeLength = 40.
   scalar pathWidth = 1.
   scalar pathOutlineWidth = 3.
}

-- draw each group element as a dot
forall Element g
{
   shape g.icon = Circle {
      r: 5.0 -- radius
      fillColor: colors.lightGray
      strokeColor: colors.mediumGray
      strokeWidth: .55
      ensureOnCanvas: true
   }

   shape g.labelText = Equation {
       string: g.label
       center: g.icon.center
       fontSize: "0.35px"
       fillColor: colors.darkGray
   }
}

-- draw a circle around the identity element
forall Element e
where IsIdentity(e)
{
   shape identityMarker = Circle {
      center: e.icon.center
      r: 1.5*e.icon.r
      fillColor: rgba(0,0,0,.1)
   }
}

 -- highlight any generating element by changing its color
 forall Element s
 where IsGenerator(s)
 {
    scalar r = ?
    scalar g = ?
    scalar b = ?
    ensure inRange(r,.25,1.0)
    ensure inRange(g,.25,1.0)
    ensure inRange(b,.25,1.0)

    s.icon.fillColor = rgba(r,g,b,1.0)
    s.icon.strokeColor = rgba(.7*r,.7*g,.7*b,1.0)
    s.labelText.fillColor = s.icon.strokeColor
 }

-- encourage all nodes to avoid each other
forall Element g1; Element g2
{
   vec2 x1 = g1.icon.center
   vec2 x2 = g2.icon.center
   scalar d = norm( x1 - x2 )

   -- minimize a Coulomb potential
   encourage equal( 0., 2.*sqr(1000./d) )

   -- minimize IPC potential
   -- scalar dhat = 10.*g1.icon.r
   --encourage equal( 0., max(0, -sqr(d - dhat)*log(d/dhat))) 
}

-- rule for any two elements g1, g2 related by a generator s
forall Element g1; Element g2; Element s
where IsGenerator(s); IsProduct( g2, g1, s )
{
   -- draw an arrow from g1 to g2
   vec2 x0 = g1.icon.center
   vec2 x2 = g2.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = g1.icon.center
   vec2 x2 = g2.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}

-- same rule as above, but catches the special case where g1 is the identity
-- (since we don't currently support matching on non-distinct tuples)
forall Element s; Element e
where IsGenerator(s); IsProduct( s, e, s )
{
   -- draw an arrow from e to s
   vec2 x0 = e.icon.center
   vec2 x2 = s.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = e.icon.center
   vec2 x2 = s.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}

-- same rule as above, but catches the special case where g1 = s
-- (since we don't currently support matching on non-distinct tuples)
forall Element g; Element s
where IsGenerator(s); IsProduct( g, s, s )
{
   -- draw an arrow from s to g
   vec2 x0 = s.icon.center
   vec2 x2 = g.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = s.icon.center
   vec2 x2 = g.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}
`,Ft=`type Element -- an element of the group
predicate IsIdentity( Element e ) -- asserts that e is the identity element
predicate IsGenerator( Element g ) -- asserts that g is a generator of the group
predicate IsProduct( Element a, Element b, Element c ) -- asserts that a = b*c
`,jt=`canvas {
    width = 240
    height = 200
}

colors {
   lightGray = rgba( .8, .8, .8, 1. )
   mediumGray = rgba( .6, .6, .6, 1. )
   darkGray = rgba( .4, .4, .4, 1. )
}

global {
   scalar tableWidth = 180
   scalar tableHeight = 180

   scalar boxPadding = 2.0
}

-- draw each group element as a label for a row and column
forall Element g
{
   scalar m = match_id
   scalar n = match_total

   -- associate each group element with a value t ∈ [0,1]
   scalar g.t = m / n

   -- compute horizontal/vertical coordinates u/v for each element
   scalar g.u = ( g.t - 0.5 ) * global.tableHeight
   scalar g.v = ( (1.-g.t) - 0.5 ) * global.tableWidth

   -- compute box widths/heights (for later use by cascading rules)
   scalar g.width = global.tableWidth / n
   scalar g.height= global.tableHeight / n

   -- define a color associated with this element

   -- -- Random colors
   -- scalar g.R = ?
   -- scalar g.G = ?
   -- scalar g.B = ?
   -- ensure inRange( g.R, 0, 1 )
   -- ensure inRange( g.G, 0, 1 )
   -- ensure inRange( g.B, 0, 1 )

   -- -- Rainbow colors
   -- scalar π = MathPI()
   -- scalar θ = 2.*π * g.t
   -- scalar φ = 2.*π/3.
   -- scalar g.R = .5 + .5*cos(θ + 0.*φ)
   -- scalar g.G = .5 + .5*cos(θ + 2.*φ)
   -- scalar g.B = .5 + .5*cos(θ + 4.*φ)
   
     -- "Hot" colors
     scalar T = .1 + .8*(1.-g.t)
     scalar s = max(0,min(1,T)) -- clamp to [0,1]
     scalar g.R = 1-sqr(1-max(0,min(1,3*T)))
     scalar g.G = 3.*s*s - 2.*s*s*s
     scalar g.B = max(0,min(1,3*T - 2))

   color g.boxColor = rgba( g.R, g.G, g.B, .75 )
   color g.labelColor= rgba( .75*g.R, .75*g.G, .75*g.B, 1. )

   shape g.rowLabel = Equation {
       string: g.label
       center: (-global.tableWidth/2, g.v )
       fontSize: "2.5px"
       fillColor: colors.darkGray
   }

   shape g.colLabel = Equation {
       string: g.label
       center: (g.u, global.tableHeight/2)
       fontSize: "2.5px"
       fillColor: colors.darkGray
   }
}

-- -- if colors are chosen randomly, encourage them to be far apart in RGB space
-- forall Element a; Element b
-- {
--    encourage equal( 0, 1./abs(a.R-b.R) )
--    encourage equal( 0, 1./abs(a.G-b.G) )
--    encourage equal( 0, 1./abs(a.B-b.B) )
-- }

-- draw a box for each product in the multiplication table
forall Element a; Element b; Element c
where IsProduct( a, b, c )
{
   shape productShape = Rectangle {
      center: ( b.u, c.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "2.5px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element e
where IsProduct( a, e, a )
{
   shape productShape = Rectangle {
      center: ( e.u, a.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "2.5px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element e
where IsProduct( a, a, e )
{
   shape productShape = Rectangle {
      center: ( a.u, e.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "2.5px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element b
where IsProduct( a, b, b )
{
   shape productShape = Rectangle {
      center: ( b.u, b.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "2.5px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element e
where IsProduct( e, e, e )
{
   shape productShape = Rectangle {
      center: ( e.u, e.v )
      width: e.width - global.boxPadding
      height: e.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: e.boxColor
   }

   shape productText = Equation {
      string: e.label
      center: productShape.center
      fontSize: "2.5px"
      fillColor: e.labelColor
   }
}
`,Ht=`Element g1
Element g2
IsGenerator(g2)
IsProduct(g1,g1,g1)
IsProduct(g2,g2,g1)
IsProduct(g2,g1,g2)
IsProduct(g1,g2,g2)

Label g1 $g_{1}$
Label g2 $g_{2}$
`,zt=`Element g1
Element g2
Element g3
Element g4
Element g5
Element g6
Element g7
Element g8
Element g9
Element g10
IsGenerator(g2)
IsGenerator(g4)
IsProduct(g1,g1,g1)
IsProduct(g2,g2,g1)
IsProduct(g3,g3,g1)
IsProduct(g4,g4,g1)
IsProduct(g5,g5,g1)
IsProduct(g6,g6,g1)
IsProduct(g7,g7,g1)
IsProduct(g8,g8,g1)
IsProduct(g9,g9,g1)
IsProduct(g10,g10,g1)
IsProduct(g2,g1,g2)
IsProduct(g1,g2,g2)
IsProduct(g9,g3,g2)
IsProduct(g10,g4,g2)
IsProduct(g8,g5,g2)
IsProduct(g7,g6,g2)
IsProduct(g6,g7,g2)
IsProduct(g5,g8,g2)
IsProduct(g3,g9,g2)
IsProduct(g4,g10,g2)
IsProduct(g3,g1,g3)
IsProduct(g4,g2,g3)
IsProduct(g1,g3,g3)
IsProduct(g2,g4,g3)
IsProduct(g9,g5,g3)
IsProduct(g10,g6,g3)
IsProduct(g8,g7,g3)
IsProduct(g7,g8,g3)
IsProduct(g5,g9,g3)
IsProduct(g6,g10,g3)
IsProduct(g4,g1,g4)
IsProduct(g3,g2,g4)
IsProduct(g5,g3,g4)
IsProduct(g6,g4,g4)
IsProduct(g7,g5,g4)
IsProduct(g8,g6,g4)
IsProduct(g10,g7,g4)
IsProduct(g9,g8,g4)
IsProduct(g1,g9,g4)
IsProduct(g2,g10,g4)
IsProduct(g5,g1,g5)
IsProduct(g6,g2,g5)
IsProduct(g4,g3,g5)
IsProduct(g3,g4,g5)
IsProduct(g1,g5,g5)
IsProduct(g2,g6,g5)
IsProduct(g9,g7,g5)
IsProduct(g10,g8,g5)
IsProduct(g7,g9,g5)
IsProduct(g8,g10,g5)
IsProduct(g6,g1,g6)
IsProduct(g5,g2,g6)
IsProduct(g7,g3,g6)
IsProduct(g8,g4,g6)
IsProduct(g10,g5,g6)
IsProduct(g9,g6,g6)
IsProduct(g2,g7,g6)
IsProduct(g1,g8,g6)
IsProduct(g4,g9,g6)
IsProduct(g3,g10,g6)
IsProduct(g7,g1,g7)
IsProduct(g8,g2,g7)
IsProduct(g6,g3,g7)
IsProduct(g5,g4,g7)
IsProduct(g4,g5,g7)
IsProduct(g3,g6,g7)
IsProduct(g1,g7,g7)
IsProduct(g2,g8,g7)
IsProduct(g10,g9,g7)
IsProduct(g9,g10,g7)
IsProduct(g8,g1,g8)
IsProduct(g7,g2,g8)
IsProduct(g10,g3,g8)
IsProduct(g9,g4,g8)
IsProduct(g2,g5,g8)
IsProduct(g1,g6,g8)
IsProduct(g3,g7,g8)
IsProduct(g4,g8,g8)
IsProduct(g6,g9,g8)
IsProduct(g5,g10,g8)
IsProduct(g9,g1,g9)
IsProduct(g10,g2,g9)
IsProduct(g2,g3,g9)
IsProduct(g1,g4,g9)
IsProduct(g3,g5,g9)
IsProduct(g4,g6,g9)
IsProduct(g5,g7,g9)
IsProduct(g6,g8,g9)
IsProduct(g8,g9,g9)
IsProduct(g7,g10,g9)
IsProduct(g10,g1,g10)
IsProduct(g9,g2,g10)
IsProduct(g8,g3,g10)
IsProduct(g7,g4,g10)
IsProduct(g6,g5,g10)
IsProduct(g5,g6,g10)
IsProduct(g4,g7,g10)
IsProduct(g3,g8,g10)
IsProduct(g2,g9,g10)
IsProduct(g1,g10,g10)

Label g1 $g_{1}$
Label g2 $g_{2}$
Label g3 $g_{3}$
Label g4 $g_{4}$
Label g5 $g_{5}$
Label g6 $g_{6}$
Label g7 $g_{7}$
Label g8 $g_{8}$
Label g9 $g_{9}$
Label g10 $g_{10}$
`,Ut=`-- variation: LeverkaasAlbatross41817
Element g1
Element g2
Element g3
Element g4
Element g5
Element g6
Element g7
Element g8
IsIdentity(g1)
IsGenerator(g2)
IsGenerator(g3)
IsProduct(g1,g1,g1)
IsProduct(g2,g1,g2)
IsProduct(g3,g1,g3)
IsProduct(g4,g1,g4)
IsProduct(g5,g1,g5)
IsProduct(g6,g1,g6)
IsProduct(g7,g1,g7)
IsProduct(g8,g1,g8)
IsProduct(g2,g2,g1)
IsProduct(g5,g2,g2)
IsProduct(g4,g2,g3)
IsProduct(g7,g2,g4)
IsProduct(g6,g2,g5)
IsProduct(g1,g2,g6)
IsProduct(g8,g2,g7)
IsProduct(g3,g2,g8)
IsProduct(g3,g3,g1)
IsProduct(g8,g3,g2)
IsProduct(g5,g3,g3)
IsProduct(g2,g3,g4)
IsProduct(g7,g3,g5)
IsProduct(g4,g3,g6)
IsProduct(g1,g3,g7)
IsProduct(g6,g3,g8)
IsProduct(g4,g4,g1)
IsProduct(g3,g4,g2)
IsProduct(g6,g4,g3)
IsProduct(g5,g4,g4)
IsProduct(g8,g4,g5)
IsProduct(g7,g4,g6)
IsProduct(g2,g4,g7)
IsProduct(g1,g4,g8)
IsProduct(g5,g5,g1)
IsProduct(g6,g5,g2)
IsProduct(g7,g5,g3)
IsProduct(g8,g5,g4)
IsProduct(g1,g5,g5)
IsProduct(g2,g5,g6)
IsProduct(g3,g5,g7)
IsProduct(g4,g5,g8)
IsProduct(g6,g6,g1)
IsProduct(g1,g6,g2)
IsProduct(g8,g6,g3)
IsProduct(g3,g6,g4)
IsProduct(g2,g6,g5)
IsProduct(g5,g6,g6)
IsProduct(g4,g6,g7)
IsProduct(g7,g6,g8)
IsProduct(g7,g7,g1)
IsProduct(g4,g7,g2)
IsProduct(g1,g7,g3)
IsProduct(g6,g7,g4)
IsProduct(g3,g7,g5)
IsProduct(g8,g7,g6)
IsProduct(g5,g7,g7)
IsProduct(g2,g7,g8)
IsProduct(g8,g8,g1)
IsProduct(g7,g8,g2)
IsProduct(g2,g8,g3)
IsProduct(g1,g8,g4)
IsProduct(g4,g8,g5)
IsProduct(g3,g8,g6)
IsProduct(g6,g8,g7)
IsProduct(g5,g8,g8)

Label g1 $1$
Label g2 $i$
Label g3 $j$
Label g4 $k$
Label g5 $-1$
Label g6 $-i$
Label g7 $-j$
Label g8 $-k$
`,Kt={"dihedral-1.substance":Ht,"dihedral-5.substance":zt,"quaternions.substance":Ut},Xt={"CayleyGraph.style":Wt,"Group.domain":Ft,"MultiplicationTable.style":jt,groups:Kt},Yt={},Zt=`canvas {
   scalar width  = 533.
   scalar height = 300.
}

Global {
   scalar width = 800.0
   scalar height = 700.0
   scalar pointSize = width/100.0
   scalar thinStroke = width/200.0
   scalar planeSize = 0.9*height
}

Colors {
   color none = none()
   color black = rgba( 0.0, 0.0, 0.0, 1.0 )
   color blue = rgba( 0.8, 0.7, 1.0, 0.2 )
   color red = rgba( 1.0, 0.0, 0.0, 0.5 )
}

forall HyperbolicPlane H {
   shape H.diskShape = Circle {
      center : (0.0, 0.)
      r : Global.planeSize/2.0
      fillColor : Colors.blue
      strokeWidth : Global.thinStroke
      strokeColor : Colors.black
      strokeStyle : "dashed"
      -- strokeDasharray : "20,10,5,5,5,10"
   }
}

forall Point p; HyperbolicPlane H
where In( p, H ) {
   shape p.dotShape = Circle {
      r : Global.pointSize/2.0
      strokeWidth : 0.0
      fillColor : Colors.black
   }

   --ensure contains( H.diskShape, p.dotShape )
}

-- TODO: Fix that we can't add new functions with new names w/o backend complaining

forall IdealPoint p; HyperbolicPlane H
where In( p, H ) {
   scalar p.angle = ?

   scalar R = H.diskShape.r

   override p.dotShape = Circle {
      center : R*(cos(p.angle), sin(p.angle))
      r : Global.pointSize
      strokeWidth : 0.0
      fillColor : Colors.black
   }
}

forall Horocycle h; HyperbolicPlane H {
  shape h.circleShape = Circle {
    fillColor : Colors.none
    strokeWidth : 2.0
    strokeColor : Colors.black
  }

  scalar R = H.diskShape.r

  ensure inRange(h.circleShape.r, R/8., R/2.)
  constraint h.inDisk      = ensure contains(H.diskShape, h.circleShape)
  constraint h.tangentDisk = ensure touching(H.diskShape, h.circleShape)
}

forall IdealPoint p; Horocycle h; HyperbolicPlane H
where IsCenter( p, h ); In( p, H ) {

   scalar R = H.diskShape.r
   scalar r = h.circleShape.r
   vec2 x = p.dotShape.center

   -- -- set the center of the horocycle h so that it is
   -- -- tangent to the hyperbolic plane H at ideal point p
   scalar a = R - r
   h.circleShape.center = a*unit(x)

   -- since we've computed the location of the center
   -- explicitly, we should no longer need constraints to
   -- ensure containment/tangency in the Poincaré disk
   delete h.inDisk
   delete h.tangentDisk
}


`,Jt=`HyperbolicPlane H
IdealPoint p, q
Horocycle g, h

In(p, H)
In(q, H)

IsCenter( p, g )
IsCenter( q, h )
`,no=`HyperbolicPlane H
IdealPoint p, q
Horocycle g, h

p ∈ H
q ∈ H

IsCenter( p, g )
IsCenter( q, h )
`,eo=`type HyperbolicPlane
type Point
type IdealPoint <: Point
type Segment
type Horocycle

predicate In(Point, HyperbolicPlane)
predicate IsCenter(IdealPoint, Horocycle)

constructor MakeSegment(Point endpoint1, Point endpoint2) -> Segment

notation "{ a, b }" ~ "MakeSegment( a, b )"
notation "p ∈ H" ~ "In( p, H )"

`,to={Notes:Yt,"PoincareDisk.style":Zt,"hyperbolic-example-unsugared.substance":Jt,"hyperbolic-example.substance":no,"hyperbolic.domain":eo},oo=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 700">
  <rect
    x="291.702230696695"
    y="345.9981178395118"
    width="20"
    height="22.222169668815994"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 291.702230696695, 345.9981178395118)"
  >
    <title>\`E11\`.rect</title>
  </rect>
  <rect
    x="391.7022471804507"
    y="334.96215024153094"
    width="20"
    height="23.11891152129036"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 391.7022471804507, 334.96215024153094)"
  >
    <title>\`E24\`.rect</title>
  </rect>
  <rect
    x="291.70223031930436"
    y="307.9980573161733"
    width="20"
    height="18.000057856856788"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 291.70223031930436, 307.9980573161733)"
  >
    <title>\`E12\`.rect</title>
  </rect>
  <rect
    x="291.7022305079997"
    y="268.5581329338428"
    width="20"
    height="19.439892015733896"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 291.7022305079997, 268.5581329338428)"
  >
    <title>\`E15\`.rect</title>
  </rect>
  <rect
    x="491.70224401877664"
    y="404.07972601006026"
    width="20"
    height="27.285037337848408"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 491.70224401877664, 404.07972601006026)"
  >
    <title>\`E31\`.rect</title>
  </rect>
  <rect
    x="391.7022471804506"
    y="258.9621664570621"
    width="20"
    height="17.999991611872588"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 391.7022471804506, 258.9621664570621)"
  >
    <title>\`E21\`.rect</title>
  </rect>
  <rect
    x="391.7022471804507"
    y="296.9621544589236"
    width="20"
    height="17.999996009181736"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 391.7022471804507, 296.9621544589236)"
  >
    <title>\`E22\`.rect</title>
  </rect>
  <rect
    x="491.70224401877664"
    y="277.3667606570609"
    width="20"
    height="37.44204766512864"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 491.70224401877664, 277.3667606570609)"
  >
    <title>\`E32\`.rect</title>
  </rect>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="286.36677167809034"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot3</title>
  </circle>
  <rect
    x="291.70223050799984"
    y="388.221814800541"
    width="20"
    height="20.999462668882472"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 291.70223050799984, 388.221814800541)"
  >
    <title>\`E14\`.rect</title>
  </rect>
  <rect
    x="291.70223031930436"
    y="429.27300444310015"
    width="20"
    height="17.999999881609497"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 291.70223031930436, 429.27300444310015)"
  >
    <title>\`E13\`.rect</title>
  </rect>
  <rect
    x="391.70224718045074"
    y="378.0811264184068"
    width="20"
    height="19.909447932829735"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 391.70224718045074, 378.0811264184068)"
  >
    <title>\`E23\`.rect</title>
  </rect>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="305.96215066709806"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot2</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="316.998061721732"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot1</title>
  </circle>
  <rect
    x="491.7022440187774"
    y="353.539271468655"
    width="20"
    height="30.540316554667207"
    fill="#ffffff"
    fill-opacity="0"
    stroke="none"
    rx="0"
    transform="rotate(0, 491.7022440187774, 353.539271468655)"
  >
    <title>\`E33\`.rect</title>
  </rect>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="343.9621465692021"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot2</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="375.0795866313984"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot3</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="289.874635643035"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot3</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="354.9981164477696"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot1</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="278.9980261614254"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot1</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="372.96416058670474"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot3</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="438.2730044458995"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot1</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="388.99056040095866"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot2</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="387.0811853217614"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot2</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="277.5581317358143"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot1</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="305.9621538035543"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot2</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="400.2212785643029"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot1</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="355.978741360198"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot1</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="422.36476388661225"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot3</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="349.08106297970113"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot2</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="363.4716832079031"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot3</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="267.96216322770044"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot2</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="343.9621674648398"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot2</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="413.07972811640843"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot3</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="359.22028506144795"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot1</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="397.2218184956848"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot1</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="348.53351257941665"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot2</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="305.8087888844103"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot3</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="362.53927405867086"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot3</title>
  </circle>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="501.70224401877664"
    cy="417.72224467898445"
    stroke="none"
    rx="10"
    ry="13.642518668924204"
  >
    <title>\`E31\`.ellipse</title>
  </ellipse>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="433.6867779201014"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dotl</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="413.07972811640843"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot3P</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="442.92127431040296"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dotrP</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="267.96216322770044"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot2P</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="355.978741360198"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dot1P</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="442.92127431040296"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dotr</title>
  </circle>
  <circle
    fill="#672551"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="433.6867779201014"
    stroke="none"
    r="5"
  >
    <title>\`N1\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N1\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#672551"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 413.07972811640843 Q 545.9831533000591 447.1653642013924 601.7022376566715 442.92127431040296"
    ></path>
    <title>\`N1\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N1\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#672551"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 355.978741360198 Q 364.91620090379325 326.98348464558194 401.70224509665695 267.96216322770044"
    ></path>
    <title>\`N1\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N1\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#672551"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 267.96216322770044 Q 435.2336922222399 351.86937172661595 501.70224187312004 413.07972811640843"
    ></path>
    <title>\`N1\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N1\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#672551"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 433.6867779201014 Q 263.9741856084683 410.62513544717467 301.702232119186 355.978741360198"
    ></path>
    <title>\`N1\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="401.7022471804506"
    cy="267.9621622629984"
    stroke="none"
    rx="10"
    ry="8.999995805936294"
  >
    <title>\`E21\`.ellipse</title>
  </ellipse>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="501.70224401877664"
    cy="296.0877844896252"
    stroke="none"
    rx="10"
    ry="18.72102383256432"
  >
    <title>\`E32\`.ellipse</title>
  </ellipse>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="418.333826016555"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dotl</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="286.36677167809034"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot3P</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="262.91673649559726"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dotrP</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="305.9621538035543"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot2P</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="359.22028506144795"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dot1P</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="262.91673649559726"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dotr</title>
  </circle>
  <circle
    fill="#24c4c9"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="418.333826016555"
    stroke="none"
    r="5"
  >
    <title>\`N2\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N2\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#24c4c9"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 286.36677167809034 Q 556.2683845211727 294.1135348009342 601.7022376566715 262.91673649559726"
    ></path>
    <title>\`N2\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N2\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#24c4c9"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 359.22028506144795 Q 361.1036690490424 350.24378397849654 401.70224509665695 305.9621538035543"
    ></path>
    <title>\`N2\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N2\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#24c4c9"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 305.9621538035543 Q 455.54818217786635 315.7911964747614 501.70224187312004 286.36677167809034"
    ></path>
    <title>\`N2\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N2\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#24c4c9"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 418.333826016555 Q 261.8797115422948 405.99387511377165 301.702232119186 359.22028506144795"
    ></path>
    <title>\`N2\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="301.702230696695"
    cy="357.1092026739198"
    stroke="none"
    rx="10"
    ry="11.111084834407997"
  >
    <title>\`E11\`.ellipse</title>
  </ellipse>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="401.7022471804507"
    cy="305.9621524635144"
    stroke="none"
    rx="10"
    ry="8.999998004590868"
  >
    <title>\`E22\`.ellipse</title>
  </ellipse>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="501.7022440187774"
    cy="368.8094297459886"
    stroke="none"
    rx="10"
    ry="15.270158277333604"
  >
    <title>\`E33\`.ellipse</title>
  </ellipse>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="373.9476161180909"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dotl</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="289.874635643035"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot3P</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="258.9697932905605"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dotrP</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="305.96215066709806"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot2P</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="354.9981164477696"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dot1P</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="258.9697932905605"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dotr</title>
  </circle>
  <circle
    fill="#6f41d4"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="373.9476161180909"
    stroke="none"
    r="5"
  >
    <title>\`N3\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N3\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#6f41d4"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 289.874635643035 Q 557.6076285873627 293.530492945572 601.7022376566715 258.9697932905605"
    ></path>
    <title>\`N3\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N3\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#6f41d4"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 354.9981164477696 Q 360.50775399145675 348.4373835418093 401.70224509665695 305.96215066709806"
    ></path>
    <title>\`N3\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N3\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#6f41d4"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 305.96215066709806 Q 454.8789065458484 317.66449993115606 501.70224187312004 289.874635643035"
    ></path>
    <title>\`N3\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N3\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#6f41d4"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 373.9476161180909 Q 255.42587313861145 384.1231698649264 301.702232119186 354.9981164477696"
    ></path>
    <title>\`N3\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="301.70223031930436"
    cy="316.99808624460167"
    stroke="none"
    rx="10"
    ry="9.000028928428394"
  >
    <title>\`E12\`.ellipse</title>
  </ellipse>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="401.70224718045074"
    cy="388.0358503848217"
    stroke="none"
    rx="10"
    ry="9.954723966414868"
  >
    <title>\`E23\`.ellipse</title>
  </ellipse>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="347.5243031937014"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dotl</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="305.8087888844103"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot3P</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="290.61360635442713"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dotrP</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="387.0811853217614"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot2P</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="316.998061721732"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dot1P</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="290.61360635442713"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dotr</title>
  </circle>
  <circle
    fill="#8f997b"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="347.5243031937014"
    stroke="none"
    r="5"
  >
    <title>\`N4\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N4\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8f997b"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 305.8087888844103 Q 554.706792444756 317.98422429976944 601.7022376566715 290.61360635442713"
    ></path>
    <title>\`N4\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N4\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8f997b"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 316.998061721732 Q 340.2238625021769 368.4178656881744 401.70224509665695 387.0811853217614"
    ></path>
    <title>\`N4\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N4\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8f997b"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 387.0811853217614 Q 464.31619283113554 361.9655611842218 501.70224187312004 305.8087888844103"
    ></path>
    <title>\`N4\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N4\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8f997b"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 347.5243031937014 Q 257.5414800642656 351.3897769067113 301.702232119186 316.998061721732"
    ></path>
    <title>\`N4\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="301.70223031930436"
    cy="438.2730043839049"
    stroke="none"
    rx="10"
    ry="8.999999940804749"
  >
    <title>\`E13\`.ellipse</title>
  </ellipse>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="401.7022471804507"
    cy="346.5216060021761"
    stroke="none"
    rx="10"
    ry="11.55945576064518"
  >
    <title>\`E24\`.ellipse</title>
  </ellipse>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="387.3262563238399"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dotl</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="422.36476388661225"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot3P</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="447.27312908081234"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dotrP</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="388.99056040095866"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot2P</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="438.2730044458995"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dot1P</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="447.27312908081234"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dotr</title>
  </circle>
  <circle
    fill="#66c690"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="387.3262563238399"
    stroke="none"
    r="5"
  >
    <title>\`N5\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N5\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#66c690"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 422.36476388661225 Q 546.868271695883 454.2259729195084 601.7022376566715 447.27312908081234"
    ></path>
    <title>\`N5\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N5\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#66c690"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 438.2730044458995 Q 360.5433829423219 431.57151751342934 401.70224509665695 388.99056040095866"
    ></path>
    <title>\`N5\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N5\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#66c690"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 388.99056040095866 Q 445.3707154985986 424.6490004590838 501.70224187312004 422.36476388661225"
    ></path>
    <title>\`N5\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N5\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#66c690"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 387.3262563238399 Q 242.62324887855996 430.6201792821891 301.702232119186 438.2730044458995"
    ></path>
    <title>\`N5\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="301.70223050799984"
    cy="398.72154613498225"
    stroke="none"
    rx="10"
    ry="10.499731334441236"
  >
    <title>\`E14\`.ellipse</title>
  </ellipse>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="409.33131884676925"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dotl</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="362.53927405867086"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot3P</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="399.3572537347592"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dotrP</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="348.53351257941665"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot2P</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="397.2218184956848"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dot1P</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="399.3572537347592"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dotr</title>
  </circle>
  <circle
    fill="#7c88a1"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="409.33131884676925"
    stroke="none"
    r="5"
  >
    <title>\`N6\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N6\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#7c88a1"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 362.53927405867086 Q 544.7921246506696 399.71659451067353 601.7022376566715 399.3572537347592"
    ></path>
    <title>\`N6\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N6\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#7c88a1"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 397.2218184956848 Q 360.457321724823 390.8595577541281 401.70224509665695 348.53351257941665"
    ></path>
    <title>\`N6\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N6\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#7c88a1"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 348.53351257941665 Q 448.9281725163938 375.34307024969206 501.70224187312004 362.53927405867086"
    ></path>
    <title>\`N6\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N6\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#7c88a1"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 409.33131884676925 Q 254.10657415722417 423.13151937962414 301.702232119186 397.2218184956848"
    ></path>
    <title>\`N6\`.linel1</title>
  </g>
  <ellipse
    fill="#000000"
    fill-opacity="0.2"
    cx="301.7022305079997"
    cy="278.27807894170974"
    stroke="none"
    rx="10"
    ry="9.719946007866948"
  >
    <title>\`E15\`.ellipse</title>
  </ellipse>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="447.2731279895004"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dotl</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="375.0795866313984"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot3P</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="331.97134603082964"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dotrP</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="349.08106297970113"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot2P</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="400.2212785643029"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dot1P</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="331.97134603082964"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dotr</title>
  </circle>
  <circle
    fill="#8c8b1a"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="447.2731279895004"
    stroke="none"
    r="5"
  >
    <title>\`N7\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N7\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8c8b1a"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 375.0795866313984 Q 559.6195731167817 371.89162431612823 601.7022376566715 331.97134603082964"
    ></path>
    <title>\`N7\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N7\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8c8b1a"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 400.2212785643029 Q 360.80857134662426 392.457760361974 401.70224509665695 349.08106297970113"
    ></path>
    <title>\`N7\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N7\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8c8b1a"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 349.08106297970113 Q 446.6698392991308 381.43684388014384 501.70224187312004 375.0795866313984"
    ></path>
    <title>\`N7\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N7\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#8c8b1a"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 447.2731279895004 Q 260.2171451351314 441.8440596661263 301.702232119186 400.2212785643029"
    ></path>
    <title>\`N7\`.linel1</title>
  </g>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="313.01092190374663"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dotl</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="363.4716832079031"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot3P</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="416.038277840749"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dotrP</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="343.9621465692021"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot2P</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="277.5581317358143"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dot1P</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="416.038277840749"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dotr</title>
  </circle>
  <circle
    fill="#36baba"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="313.01092190374663"
    stroke="none"
    r="5"
  >
    <title>\`N8\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N8\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#36baba"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 363.4716832079031 Q 542.3963260007058 407.4580847210786 601.7022376566715 416.038277840749"
    ></path>
    <title>\`N8\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N8\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#36baba"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 277.5581317358143 Q 340.6385551481731 327.4213168317068 401.70224509665695 343.9621465692021"
    ></path>
    <title>\`N8\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N8\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#36baba"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 343.9621465692021 Q 447.8725441802816 373.3468238215224 501.70224187312004 363.4716832079031"
    ></path>
    <title>\`N8\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N8\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#36baba"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 313.01092190374663 Q 258.38523097933626 314.1349239562963 301.702232119186 277.5581317358143"
    ></path>
    <title>\`N8\`.linel1</title>
  </g>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="259.49233285032085"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dotl</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="501.70224187312004"
    cy="372.96416058670474"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot3P</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="379.13510090631127"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dotrP</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="401.70224509665695"
    cy="343.9621674648398"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot2P</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="301.702232119186"
    cy="278.9980261614254"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dot1P</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="601.7022376566715"
    cy="379.13510090631127"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dotr</title>
  </circle>
  <circle
    fill="#c0c263"
    fill-opacity="0.5"
    cx="201.70223531695788"
    cy="259.49233285032085"
    stroke="none"
    r="5"
  >
    <title>\`N9\`.dotlP</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N9\`.line3r-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#c0c263"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 501.70224187312004 372.96416058670474 Q 550.4703999976422 396.0116569698084 601.7022376566715 379.13510090631127"
    ></path>
    <title>\`N9\`.line3r</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N9\`.line12-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#c0c263"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 301.702232119186 278.9980261614254 Q 340.8067058706174 328.25171865730425 401.70224509665695 343.9621674648398"
    ></path>
    <title>\`N9\`.line12</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N9\`.line23-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#c0c263"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 401.70224509665695 343.9621674648398 Q 446.1314065038017 377.6716438848415 501.70224187312004 372.96416058670474"
    ></path>
    <title>\`N9\`.line23</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`N9\`.linel1-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#c0c263"
      fill="none"
      stroke-width="3"
      stroke-opacity="0.5"
      d="M 201.70223531695788 259.49233285032085 Q 247.87326120121574 288.87523021760046 301.702232119186 278.9980261614254"
    ></path>
    <title>\`N9\`.linel1</title>
  </g>
</svg>
`,ro=`
canvas {
  width = 800
  height = 700
}

forall Graph g {
  g.xl = ?
  g.x1 = ?
  g.x2 = ?
  g.x3 = ?
  g.xr = ?
  g.yb = ?
  g.yt = ?
  g.infty = (?, ?)
  encourage equal(g.xl, g.xr)
  encourage equal(g.yb, g.yt)
  ensure lessThan(100, g.x1 - g.xl)
  ensure lessThan(100, g.x2 - g.x1)
  ensure lessThan(100, g.x3 - g.x2)
  ensure lessThan(100, g.xr - g.x3)
  encourage equal(g.xl + g.xr, 0)
  encourage equal(g.yb + g.yt, 0)
  ensure lessThan(g.infty[0], -700)
  ensure lessThan(g.infty[1], 700)
  ensure lessThan(-1000, g.infty[0])
  ensure lessThan(500, g.infty[1])
}

forall Node n {
  n.r = 5
  n.w = 3
  n.xl = ?
  n.x1 = ?
  n.x2 = ?
  n.x3 = ?
  n.xr = ?
  n.yl = ?
  n.y1 = ?
  n.y2 = ?
  n.y3 = ?
  n.yr = ?
  n.dotl = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
  }
  n.dotlP = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
    fillColor: n.dotl.fillColor
  }
  n.dot1 = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot1P = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot2 = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot2P = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot3 = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dot3P = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dotr = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.dotrP = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.linel1 = Path{
    d: makePath((n.xl, n.yl), (n.x1, n.y1) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line12 = Path{ 
    d: makePath((n.x1, n.y1), (n.x2, n.y2) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line23 = Path{ 
    d: makePath((n.x2, n.y2), (n.x3, n.y3) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line3r = Path{
    d: makePath((n.x3, n.y3), (n.xr, n.yr) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  encourage equal(abs(n.y1 - n.yl), -100)
  encourage equal(abs(n.y1 - n.y2), -100)
  encourage equal(abs(n.y3 - n.y2), -100)
  encourage equal(abs(n.y3 - n.yr), -100)
}

forall Layer l {
  l.x = ?
  l.yb = ?
  l.yt = ?
  l.infty = (?, ?)

  l.r1 = (l.x - 20.0, (l.x - 20.0 - l.infty[0]) * (l.yb - 50.0 - l.infty[1]) / (l.x + 20.0 - l.infty[0]) + l.infty[1] )
  l.r2 = (l.x + 20.0, l.yb - 50.0)
  l.r3 = (l.x + 20.0, (l.x + 20.0 - l.infty[0]) * (l.yt + 50.0 - l.infty[1]) / (l.x - 20.0 - l.infty[0]) + l.infty[1] )
  l.r4 = (l.x - 20.0, l.yt + 50.0)

  l.rect = Polygon {
    points: [ l.r1, l.r2, l.r3, l.r4 ]
  }

}

Graph g; Layer1 l
where GraphHasLayer1(g, l) {
  override l.x = g.x1
  override l.yb = g.yb
  override l.yt = g.yt
  override l.infty = g.infty
}

Graph g; Layer2 l
where GraphHasLayer2(g, l) {
  override l.x = g.x2
  override l.yb = g.yb
  override l.yt = g.yt
  override l.infty = g.infty
}

Graph g; Layer3 l
where GraphHasLayer3(g, l) {
  override l.x = g.x3
  override l.yb = g.yb
  override l.yt = g.yt
  override l.infty = g.infty
}

Graph g; Node n
where GraphHasNode(g, n) {
  override n.xl = g.xl
  override n.x1 = g.x1
  override n.x2 = g.x2
  override n.x3 = g.x3
  override n.xr = g.xr
  ensure lessThan(g.yb, n.yr)
  ensure lessThan(n.yr, g.yt)
  ensure lessThan(g.yb, n.yl)
  ensure lessThan(n.yl, g.yt)
}

forall Node n1; Node n2 {
  encourage lessThan(50.0, abs(n2.yl - n1.yl))
  encourage lessThan(50.0, abs(n2.y1 - n1.y1))
  encourage lessThan(50.0, abs(n2.y2 - n1.y2))
  encourage lessThan(50.0, abs(n2.y3 - n1.y3))
  encourage lessThan(50.0, abs(n2.yr - n1.yr))
}

forall Edge e {
  e.x = ?
  e.yt = ?
  e.yb = ?
  e.rectx = ?
  e.rect = Rectangle {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    width: 20.0
    height: e.yt - e.yb
    fillColor: rgba(1.0, 1.0, 1.0, 0.0)
  }
  e.ellipse = Ellipse {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    rx: 10.0
    ry: 0.5 * (e.yt - e.yb)
    fillColor: rgba(0.0, 0.0, 0.0, 0.2)
  }
  encourage equal(e.yt, e.yb)
  ensure equal(e.rectx, e.x)
}

forall Layer1 l; Edge e
where Layer1HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Layer2 l; Edge e
where Layer2HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Layer3 l; Edge e
where Layer3HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Edge e; Node n
where EdgeHasNodeInLayer1(e, n) {
  ensure contains(e.rect, n.dot1, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer2(e, n) {
  ensure contains(e.rect, n.dot2, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer3(e, n) {
  ensure contains(e.rect, n.dot3, 4)
}

forall Edge e1; Edge e2 {
  ensure disjoint(e1.rect, e2.rect, 20)
  encourage lessThan(abs(e1.yt - e1.yb) + abs(e2.yt - e2.yb), abs(e1.yt + e1.yb - e2.yt - e2.yb))
}
`,ao=`
type Graph

type Layer

type Layer1
type Layer2
type Layer3

Layer1 <: Layer
Layer2 <: Layer
Layer3 <: Layer

type Node
type Edge

predicate GraphHasLayer1(Graph g, Layer1 l)
predicate GraphHasLayer2(Graph g, Layer2 l)
predicate GraphHasLayer3(Graph g, Layer3 l)

predicate GraphHasNode(Graph g, Node n)

predicate Layer1HasEdge(Layer1 l, Edge e)
predicate Layer2HasEdge(Layer2 l, Edge e)
predicate Layer3HasEdge(Layer3 l, Edge e)

predicate EdgeHasNodeInLayer1(Edge e, Node n)
predicate EdgeHasNodeInLayer2(Edge e, Node n)
predicate EdgeHasNodeInLayer3(Edge e, Node n)
`,lo=`
canvas {
  width = 800
  height = 700
}

forall Graph g {
  g.xl = ?
  g.x1 = ?
  g.x2 = ?
  g.x3 = ?
  g.xr = ?
  g.yb = ?
  g.yt = ?
  encourage equal(g.xl, g.xr)
  encourage equal(g.yb, g.yt)
  ensure lessThan(100, g.x1 - g.xl)
  ensure lessThan(100, g.x2 - g.x1)
  ensure lessThan(100, g.x3 - g.x2)
  ensure lessThan(100, g.xr - g.x3)
  encourage equal(g.xl + g.xr, 0)
  encourage equal(g.yb + g.yt, 0)
}

forall Node n {
  n.r = 5
  n.w = 3
  n.xl = ?
  n.x1 = ?
  n.x2 = ?
  n.x3 = ?
  n.xr = ?
  n.yl = ?
  n.y1 = ?
  n.y2 = ?
  n.y3 = ?
  n.yr = ?
  n.dotl = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
  }
  n.dotlP = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
    fillColor: n.dotl.fillColor
  }
  n.dot1 = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot1P = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot2 = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot2P = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot3 = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dot3P = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dotr = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.dotrP = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.linel1 = Path{
    d: makePath((n.xl, n.yl), (n.x1, n.y1) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line12 = Path{ 
    d: makePath((n.x1, n.y1), (n.x2, n.y2) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line23 = Path{ 
    d: makePath((n.x2, n.y2), (n.x3, n.y3) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line3r = Path{
    d: makePath((n.x3, n.y3), (n.xr, n.yr) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  encourage equal(abs(n.y1 - n.yl), 50)
  encourage equal(abs(n.y1 - n.y2), 50)
  encourage equal(abs(n.y3 - n.y2), 50)
  encourage equal(abs(n.y3 - n.yr), 50)
}

forall Layer1 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Layer2 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Layer3 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Graph g; Layer1 l
where GraphHasLayer1(g, l) {
  override l.x = g.x1
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Layer2 l
where GraphHasLayer2(g, l) {
  override l.x = g.x2
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Layer3 l
where GraphHasLayer3(g, l) {
  override l.x = g.x3
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Node n
where GraphHasNode(g, n) {
  override n.xl = g.xl
  override n.x1 = g.x1
  override n.x2 = g.x2
  override n.x3 = g.x3
  override n.xr = g.xr
  ensure lessThan(g.yb, n.yr)
  ensure lessThan(n.yr, g.yt)
  ensure lessThan(g.yb, n.yl)
  ensure lessThan(n.yl, g.yt)
}

forall Node n1; Node n2 {
  encourage lessThan(50.0, abs(n2.yl - n1.yl))
  encourage lessThan(50.0, abs(n2.y1 - n1.y1))
  encourage lessThan(50.0, abs(n2.y2 - n1.y2))
  encourage lessThan(50.0, abs(n2.y3 - n1.y3))
  encourage lessThan(50.0, abs(n2.yr - n1.yr))
}

forall Edge e {
  e.x = ?
  e.yt = ?
  e.yb = ?
  e.rectx = ?
  e.rect = Rectangle {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    width: 20.0
    height: e.yt - e.yb
    fillColor: rgba(1.0, 1.0, 1.0, 0.0)
  }
  e.ellipse = Ellipse {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    rx: 10.0
    ry: 0.5 * (e.yt - e.yb)
    fillColor: rgba(0.0, 0.0, 0.0, 0.2)
  }
  encourage equal(e.yt, e.yb)
  ensure equal(e.rectx, e.x)
}

forall Layer1 l; Edge e
where Layer1HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Layer2 l; Edge e
where Layer2HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Layer3 l; Edge e
where Layer3HasEdge(l, e) {
  override e.x = l.x
  ensure lessThan(l.yb, e.yb)
  ensure lessThan(e.yt, l.yt)
}

forall Edge e; Node n
where EdgeHasNodeInLayer1(e, n) {
  ensure contains(e.rect, n.dot1, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer2(e, n) {
  ensure contains(e.rect, n.dot2, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer3(e, n) {
  ensure contains(e.rect, n.dot3, 4)
}

forall Edge e1; Edge e2 {
  ensure disjoint(e1.rect, e2.rect, 20)
  encourage lessThan(abs(e1.yt - e1.yb) + abs(e2.yt - e2.yb), abs(e1.yt + e1.yb - e2.yt - e2.yb))
}
`,io=`
-- Graph
Graph G
Node N1, N2, N3, N4, N5, N6, N7, N8, N9

GraphHasNode(G, N1)
GraphHasNode(G, N2)
GraphHasNode(G, N3)
GraphHasNode(G, N4)
GraphHasNode(G, N5)
GraphHasNode(G, N6)
GraphHasNode(G, N7)
GraphHasNode(G, N8)
GraphHasNode(G, N9)

-- First layer
Layer1 L1
GraphHasLayer1(G, L1)
Edge E11, E12, E13, E14, E15

Layer1HasEdge(L1, E11)
Layer1HasEdge(L1, E12)
Layer1HasEdge(L1, E13)
Layer1HasEdge(L1, E14)
Layer1HasEdge(L1, E15)

EdgeHasNodeInLayer1(E11, N1)
EdgeHasNodeInLayer1(E11, N2)
EdgeHasNodeInLayer1(E11, N3)
EdgeHasNodeInLayer1(E12, N4)
EdgeHasNodeInLayer1(E13, N5)
EdgeHasNodeInLayer1(E14, N6)
EdgeHasNodeInLayer1(E14, N7)
EdgeHasNodeInLayer1(E15, N8)
EdgeHasNodeInLayer1(E15, N9)

-- Second layer
Layer2 L2
GraphHasLayer2(G, L2)
Edge E21, E22, E23, E24

Layer2HasEdge(L2, E21)
Layer2HasEdge(L2, E22)
Layer2HasEdge(L2, E23)
Layer2HasEdge(L2, E24)

EdgeHasNodeInLayer2(E21, N1)
EdgeHasNodeInLayer2(E22, N2)
EdgeHasNodeInLayer2(E22, N3)
EdgeHasNodeInLayer2(E23, N4)
EdgeHasNodeInLayer2(E23, N5)
EdgeHasNodeInLayer2(E24, N6)
EdgeHasNodeInLayer2(E24, N7)
EdgeHasNodeInLayer2(E24, N8)
EdgeHasNodeInLayer2(E24, N9)

-- Third layer
Layer3 L3
GraphHasLayer3(G, L3)
Edge E31, E32, E33

Layer3HasEdge(L3, E31)
Layer3HasEdge(L3, E32)
Layer3HasEdge(L3, E33)

EdgeHasNodeInLayer3(E31, N1)
EdgeHasNodeInLayer3(E32, N2)
EdgeHasNodeInLayer3(E32, N3)
EdgeHasNodeInLayer3(E32, N4)
EdgeHasNodeInLayer3(E31, N5)
EdgeHasNodeInLayer3(E33, N6)
EdgeHasNodeInLayer3(E33, N7)
EdgeHasNodeInLayer3(E33, N8)
EdgeHasNodeInLayer3(E33, N9)
`,so={"example.svg":oo,"hypergraph-3d.style":ro,"hypergraph.domain":ao,"hypergraph.style":lo,"hypergraph.substance":io},co=`Node a, b, c, d
Node u, v, x, y, z
Element t2 := QuadraticTriangle( a, b, c, x, y, z )
Element e := QuadraticTriangle( b, a, d, x, u, v )

Label a $p_i$
Label b $p_j$
Label c $p_k$
Label d $p_l$
Label x $m_{ij}$
Label y $m_{jk}$
Label z $m_{ki}$
Label u $m_{il}$
Label v $m_{jl}$

`,po=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 240 180">
  <rect
    x="0"
    y="0"
    width="240"
    height="180"
    fill="#e6ffe6"
    fill-opacity="1"
    stroke="none"
    rx="0"
    transform="rotate(0, 0, 0)"
  >
    <title>Global.box</title>
  </rect>
  <circle
    fill="#ffffff"
    fill-opacity="1"
    cx="169.91699909605111"
    cy="131.25862214556537"
    stroke="#000000"
    stroke-opacity="1"
    stroke-width="1"
    stroke-linecap="butt"
    r="2"
  >
    <title>\`u\`.icon</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="13.723584318905637"
    cy="140.85761123012145"
    stroke="none"
    r="2.5"
  >
    <title>\`c\`.icon</title>
  </circle>
  <g endArrowheadSize="1" startArrowheadSize="1">
    <filter id="\`e\`.curveIJ-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 93.69344457008361 54.084827290071644 Q 131.08506575423095 147.77292483988063 150.76202870142595 163.44697952849714"
    ></path>
    <title>\`e\`.curveIJ</title>
  </g>
  <g endArrowheadSize="1" startArrowheadSize="1">
    <filter id="\`e\`.curveJK-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 150.76202870142595 163.44697952849714 Q 167.32747819299092 151.09027314825838 194.25101129679666 59.40696275724758"
    ></path>
    <title>\`e\`.curveJK</title>
  </g>
  <g endArrowheadSize="1" startArrowheadSize="1">
    <filter id="\`e\`.curveKI-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 194.25101129679666 59.40696275724758 Q 169.8018073976805 87.28820706286994 93.69344457008361 54.084827290071644"
    ></path>
    <title>\`e\`.curveKI</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="93.69344457008361"
    cy="54.084827290071644"
    stroke="none"
    r="2.5"
  >
    <title>\`b\`.icon</title>
  </circle>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`t2\`.curveIJ-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 150.76202870142595 163.44697952849714 Q 131.08506575423095 147.77292483988063 93.69344457008361 54.084827290071644"
    ></path>
    <title>\`t2\`.curveIJ</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`t2\`.curveJK-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 93.69344457008361 54.084827290071644 Q 15.761541135265418 84.8210328318795 13.723584318905637 140.85761123012145"
    ></path>
    <title>\`t2\`.curveJK</title>
  </g>
  <g startArrowheadSize="1" endArrowheadSize="1">
    <filter id="\`t2\`.curveKI-shadow" x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5"></feOffset>
      <feGaussianBlur
        result="blurOut"
        in="offOut"
        stdDeviation="4"
      ></feGaussianBlur>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5"></feFuncA>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode></feMergeNode>
        <feMergeNode in="SourceGraphic"></feMergeNode>
      </feMerge>
    </filter>
    <path
      stroke="#000000"
      fill="none"
      stroke-width="1.5"
      stroke-opacity="1"
      d="M 13.723584318905637 140.85761123012145 Q 58.369537264230765 120.05763826147903 150.76202870142595 163.44697952849714"
    ></path>
    <title>\`t2\`.curveKI</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="150.76202870142595"
    cy="163.44697952849714"
    stroke="none"
    r="2.5"
  >
    <title>\`a\`.icon</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="194.25101129679666"
    cy="59.40696275724758"
    stroke="none"
    r="2.5"
  >
    <title>\`d\`.icon</title>
  </circle>
  <circle
    fill="#ffffff"
    fill-opacity="1"
    cx="156.88701766556034"
    cy="72.01705104326479"
    stroke="#000000"
    stroke-opacity="1"
    stroke-width="1"
    stroke-linecap="butt"
    r="2"
  >
    <title>\`v\`.icon</title>
  </circle>
  <circle
    fill="#ffffff"
    fill-opacity="1"
    cx="126.65640119499287"
    cy="128.2694141245825"
    stroke="#000000"
    stroke-opacity="1"
    stroke-width="1"
    stroke-linecap="butt"
    r="2"
  >
    <title>\`x\`.icon</title>
  </circle>
  <circle
    fill="#ffffff"
    fill-opacity="1"
    cx="70.30617188719827"
    cy="136.10496682039417"
    stroke="#000000"
    stroke-opacity="1"
    stroke-width="1"
    stroke-linecap="butt"
    r="2"
  >
    <title>\`z\`.icon</title>
  </circle>
  <circle
    fill="#ffffff"
    fill-opacity="1"
    cx="34.73502778988002"
    cy="91.14612604598801"
    stroke="#000000"
    stroke-opacity="1"
    stroke-width="1"
    stroke-linecap="butt"
    r="2"
  >
    <title>\`y\`.icon</title>
  </circle>
  <g
    transform="rotate(0, 146.24287425387206, 167.61180698722924)translate(146.24287425387206, 167.61180698722924)"
    string="p_i"
  >
    <title>\`a\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="9.6778"
      height="11.66"
      role="img"
      focusable="false"
      viewBox="-0.45474000000000003 -5.15372 9.19407324 7.41576"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.26818000000000003-3.34642Q0.27984-3.3814 0.2915-3.4397T0.3498-3.6962200000000003 0.46640000000000004-4.05768 0.6413-4.4424600000000005 0.8745-4.79226 1.17766-5.04878 1.56244-5.15372Q2.43694-5.15372 2.6818-4.4074800000000005L2.7984-4.51242Q3.5213200000000002-5.15372 4.17428-5.15372 4.93218-5.15372 5.3636-4.6057T5.79502-3.27646Q5.79502-2.01718 4.90886-0.95612T2.90334 0.11660000000000001Q2.64682 0.11660000000000001 2.4486 0.04664 2.32034-0.01166 2.1804200000000002-0.12826T1.95888-0.32648L1.87726-0.41976Q1.8656000000000001-0.4081 1.62074 0.59466T1.37588 1.60908Q1.37588 1.67904 1.46916 1.6907T1.90058 1.72568H2.1920800000000003Q2.26204 1.8073000000000001 2.26204 1.8306200000000001T2.2270600000000003 2.0405Q2.1920800000000003 2.1804200000000002 2.1571000000000002 2.2154000000000003T2.00552 2.26204Q1.9822 2.26204 1.87726 2.26204T1.48082 2.25038 0.7579 2.23872Q-0.058300000000000005 2.23872-0.27984 2.26204H-0.37312Q-0.45474000000000003 2.1804200000000002-0.45474000000000003 2.1337800000000002-0.43142 1.8189600000000001-0.30316 1.72568H-0.06996Q0.32648 1.71402 0.38478 1.58576 0.41976 1.5158 1.0960400000000001-1.20098T1.8073000000000001-4.081Q1.8189600000000001-4.1393 1.8189600000000001-4.2442400000000005 1.8189600000000001-4.7223 1.52746-4.7223 1.27094-4.7223 1.0960400000000001-4.3958200000000005T0.82786-3.6845600000000003 0.68794-3.2648Q0.66462-3.24148 0.50138-3.24148H0.33814Q0.26818000000000003-3.31144 0.26818000000000003-3.34642ZM2.07548-1.18932Q2.332-0.30316 2.93832-0.30316 3.28812-0.30316 3.6146000000000003-0.57134T4.15096-1.24762Q4.3608400000000005-1.64406 4.57072-2.5069T4.79226-3.7895V-3.85946Q4.79226-4.7223 4.081-4.7223 3.95274-4.7223 3.82448-4.68732T3.5679600000000002-4.58238 3.33476-4.4308000000000005 3.13654-4.2559000000000005 2.96164-4.081 2.83338-3.91776 2.7401-3.80116L2.70512-3.75452Q2.70512-3.74286 2.67014-3.5912800000000002T2.54188-3.07824 2.37864-2.47192Q2.07548-1.23596 2.07548-1.18932ZM7.76658608-3.197172Q7.76658608-3.3950188800000003 7.92321486-3.54340404T8.28593414-3.70003282Q8.4343193-3.70003282 8.53324274-3.6011093800000005T8.6404098-3.35380078Q8.6404098-3.16419752 8.4755374-3.0075687400000004T8.11281812-2.8426963400000003Q7.989163820000001-2.8426963400000003 7.88199676-2.92513254T7.76658608-3.197172ZM6.4228760199999995-0.61691894Q6.4228760199999995-0.6828679 6.4970686-0.8724711599999999T6.69491548-1.29289578 7.057634759999999-1.7133204 7.55225196-1.8946800400000001Q7.87375314-1.8946800400000001 8.08808726-1.7050767800000002T8.310665-1.19397234Q8.310665-1.0538308 8.19525432-0.73232962T7.86550952 0.13325048 7.51927748 1.06477954Q7.4780593799999995 1.2461391800000001 7.4780593799999995 1.32857538 7.4780593799999995 1.53466588 7.61820092 1.53466588 7.6923935000000006 1.53466588 7.7748297 1.5099350200000001T7.964432960000001 1.39452434 8.1870107 1.10599764 8.3931012 0.61962406Q8.41783206 0.52070062 8.4343193 0.50421338T8.57446084 0.48772614000000003Q8.73933324 0.48772614000000003 8.73933324 0.57016234 8.73933324 0.6361113 8.66514066 0.8257145600000001T8.45905016 1.2461391800000001 8.08808726 1.6583201800000003 7.5769828200000005 1.83967982Q7.31318698 1.83967982 7.090609240000001 1.6665638T6.85978788 1.13897212Q6.85978788 0.99883058 6.90100598 0.8751762800000001T7.25548164-0.0645964Q7.5687392000000004-0.89720202 7.61820092-1.06207442T7.6759062600000005-1.3835756Q7.6759062600000005-1.5814224799999999 7.53576472-1.5814224799999999H7.51927748Q7.27196888-1.5814224799999999 7.065878380000001-1.30938302T6.75262082-0.61691894Q6.7443772-0.60867532 6.73613358-0.59218808T6.72788996-0.56745722 6.71140272-0.55096998 6.686671860000001-0.54272636 6.6536973800000005-0.54272636 6.58774842-0.54272636H6.47233774Q6.4228760199999995-0.59218808 6.4228760199999995-0.61691894Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 81.03159864826523, 41.45186219127309)translate(81.03159864826523, 41.45186219127309)"
    string="p_j"
  >
    <title>\`b\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="10.216402987933344"
      height="12.16"
      role="img"
      focusable="false"
      viewBox="-0.45416586860754643 -5.404573836429803 10.014019689714614 8.57358023367171"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.26784140969162995-3.599555640681862Q0.2794866883738747-3.6344914767285963 0.2911319670561195-3.69271787013982T0.3493583604673434-3.948914001149205 0.4658111472897912-4.309917640298793 0.6404903275234629-4.694211836812871 0.8733959011683585-5.043570197280214 1.1761731469067227-5.2997663282896 1.5604673434208005-5.404573836429803Q2.433863244589159-5.404573836429803 2.6784140969162995-4.659276000766137L2.7948668837387474-4.76408350890634Q3.5168741620379236-5.404573836429803 4.169009768243631-5.404573836429803 4.925952882589542-5.404573836429803 5.356828193832599-4.857245738364298T5.787703505075656-3.529683968588393Q5.787703505075656-2.2719938709059564 4.9026623252250525-1.2122735108216816T2.8996743918789503-0.14090787205516184Q2.643478260869565-0.14090787205516184 2.4455085232714038-0.21077954414863054 2.3174104577667114-0.2690059375598544 2.177667113579774-0.3854587243823022T1.956406818617123-0.5834284619804635L1.8748898678414097-0.6765906914384218Q1.863244589159165-0.6649454127561769 1.6186937368320244 0.3365485539168741T1.374142884504884 1.34968779927217Q1.374142884504884 1.4195594713656388 1.4673051139628424 1.4312047500478835T1.8981804252058991 1.466140586094618H2.1893123922620186Q2.2591840643554875 1.5476575368703314 2.2591840643554875 1.5709480942348208T2.224248228308753 1.7805631105152269Q2.1893123922620186 1.9203064547021642 2.154376556215284 1.9552422907488987T2.0029879333461023 2.001823405477878Q1.9796973759816126 2.001823405477878 1.8748898678414097 2.001823405477878T1.478950392645087 1.9901781267956329 0.7569431143459107 1.9785328481133881Q-0.0582263934112239 1.9785328481133881-0.2794866883738747 2.001823405477878H-0.37264891783183296Q-0.45416586860754643 1.9203064547021642-0.45416586860754643 1.8737253399731852-0.43087531124305684 1.559302815552576-0.3027772457383643 1.466140586094618H-0.06987167209346867Q0.3260678031028538 1.454495307412373 0.38429419651407776 1.3263972419076806 0.4192300325608121 1.256525569814212 1.0946561961310093-1.456824363148822T1.805018195747941-4.333208197663283Q1.8166634744301857-4.391434591074507 1.8166634744301857-4.49624209921471 1.8166634744301857-4.973698525186745 1.5255315073740663-4.973698525186745 1.269335376364681-4.973698525186745 1.0946561961310093-4.647630722083892T0.8268147864393793-3.9372687224669605 0.687071442252442-3.5180386899061484Q0.6637808848879525-3.494748132541659 0.5007469833365256-3.494748132541659H0.3377130817850986Q0.26784140969162995-3.5646198046351274 0.26784140969162995-3.599555640681862ZM2.0728596054395707-1.445179084466577Q2.329055736448956-0.5601379046159739 2.9346102279256847-0.5601379046159739 3.283968588393028-0.5601379046159739 3.610036391495882-0.8279793143076037T4.145719210879141-1.503405477877801Q4.355334227159548-1.8993449530741235 4.564949243439954-2.7610955755602373T4.786209538402605-4.042076230607163V-4.111947902700632Q4.786209538402605-4.973698525186745 4.075847538785673-4.973698525186745 3.9477494732809806-4.973698525186745 3.8196514077762878-4.938762689140011T3.5634552767669025-4.8339551809998085 3.3305497031220073-4.682566558130627 3.132579965523846-4.507887377896955 2.957900785290174-4.333208197663283 2.8298027197854814-4.170174296111856 2.7366404903275234-4.053721509289408L2.701704654280789-4.007140394560429Q2.701704654280789-3.9954951158781844 2.6667688182340545-3.844106493009002T2.538670752729362-3.3317142309902317 2.3756368511779353-2.726159739513503Q2.0728596054395707-1.4917601991955562 2.0728596054395707-1.445179084466577ZM8.687133346102279-3.4175632254357398Q8.687133346102279-3.6727927983144992 8.860030798697567-3.8127574027963993T9.21405891591649-3.9527220072782994Q9.35402352039839-3.9527220072782994 9.444588852710208-3.8703898869948286T9.559853821107067-3.6398599502011106Q9.559853821107067-3.4093300134073927 9.403422792568474-3.252898984868799T9.041161463321203-3.096467956330205Q8.892963646810955-3.096467956330205 8.79416510247079-3.1788000766136753T8.687133346102279-3.4175632254357398ZM8.613034437847155-1.6062565791993868Q8.613034437847155-1.8450197280214518 8.398970925110133-1.8450197280214518 8.217840260486495-1.8450197280214518 8.053176019919555-1.7462211836812869T7.765013598927408-1.490991610802528 7.567416510247079-1.186362765753687 7.427451905765179-0.9228999808465811L7.36981942156675-0.8076350124497221Q7.353352997510056-0.7994018004213752 7.238088029113197-0.7994018004213752H7.122823060716337Q7.073423788546256-0.8488010725914575 7.073423788546256-0.8652674966481517T7.106356636659643-0.9722992530166633Q7.279254089254931-1.3757266424056693 7.59211614633212-1.729754759624593T8.316638804826662-2.141415361041946Q8.324872016855007-2.141415361041946 8.382504501053438-2.141415361041946T8.481303045393602-2.149648573070293Q8.678900134073933-2.141415361041946 8.843564374640874-2.067316452786822 9.04939467534955-1.9520514843899635 9.156426431718062-1.8120868799080634T9.263458188086574-1.3757266424056693V-1.2439952499521163L8.860030798697567 0.39441394368894844Q8.440136985251867 2.0410563493583602 8.415437349166826 2.106922045585137 8.267239532656578 2.5185826470024897 7.839112507182532 2.8396779161080254T6.859360275809231 3.1690063972419074Q6.447699674391878 3.1690063972419074 6.299501857881631 2.996108944646619T6.143070829343037 2.6173811913426546Q6.143070829343037 2.3950844665772837 6.299501857881631 2.2386534380386895T6.678229611185596 2.073989197471749Q6.818194215667496 2.073989197471749 6.916992760007662 2.156321317755219T7.024024516376172 2.411550890633978Q7.024024516376172 2.707946523654472 6.760561731469068 2.864377552193066 6.80996100363915 2.872610764221413 6.875826699865925 2.872610764221413 7.155755908829725 2.872610764221413 7.386285845623443 2.6420808274276957T7.740313962842366 2.0986888335567895L7.830879295154185 1.7528939283662133Q7.921444627466002 1.3988658111472898 8.044942807891207 0.896639877418119T8.308405592798314-0.13251162612526335 8.530702317563685-1.046398161271787 8.621267649875502-1.5156912468875694Q8.621267649875502-1.5733237310859989 8.613034437847155-1.6062565791993868Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 0.06362310635416435, 140.89759460417585)translate(0.06362310635416435, 140.89759460417585)"
    string="p_k"
  >
    <title>\`c\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="11.128304"
      height="11.66"
      role="img"
      focusable="false"
      viewBox="-0.45474000000000003 -5.15372 10.85104086 7.41576"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.26818000000000003-3.34642Q0.27984-3.3814 0.2915-3.4397T0.3498-3.6962200000000003 0.46640000000000004-4.05768 0.6413-4.4424600000000005 0.8745-4.79226 1.17766-5.04878 1.56244-5.15372Q2.43694-5.15372 2.6818-4.4074800000000005L2.7984-4.51242Q3.5213200000000002-5.15372 4.17428-5.15372 4.93218-5.15372 5.3636-4.6057T5.79502-3.27646Q5.79502-2.01718 4.90886-0.95612T2.90334 0.11660000000000001Q2.64682 0.11660000000000001 2.4486 0.04664 2.32034-0.01166 2.1804200000000002-0.12826T1.95888-0.32648L1.87726-0.41976Q1.8656000000000001-0.4081 1.62074 0.59466T1.37588 1.60908Q1.37588 1.67904 1.46916 1.6907T1.90058 1.72568H2.1920800000000003Q2.26204 1.8073000000000001 2.26204 1.8306200000000001T2.2270600000000003 2.0405Q2.1920800000000003 2.1804200000000002 2.1571000000000002 2.2154000000000003T2.00552 2.26204Q1.9822 2.26204 1.87726 2.26204T1.48082 2.25038 0.7579 2.23872Q-0.058300000000000005 2.23872-0.27984 2.26204H-0.37312Q-0.45474000000000003 2.1804200000000002-0.45474000000000003 2.1337800000000002-0.43142 1.8189600000000001-0.30316 1.72568H-0.06996Q0.32648 1.71402 0.38478 1.58576 0.41976 1.5158 1.0960400000000001-1.20098T1.8073000000000001-4.081Q1.8189600000000001-4.1393 1.8189600000000001-4.2442400000000005 1.8189600000000001-4.7223 1.52746-4.7223 1.27094-4.7223 1.0960400000000001-4.3958200000000005T0.82786-3.6845600000000003 0.68794-3.2648Q0.66462-3.24148 0.50138-3.24148H0.33814Q0.26818000000000003-3.31144 0.26818000000000003-3.34642ZM2.07548-1.18932Q2.332-0.30316 2.93832-0.30316 3.28812-0.30316 3.6146000000000003-0.57134T4.15096-1.24762Q4.3608400000000005-1.64406 4.57072-2.5069T4.79226-3.7895V-3.85946Q4.79226-4.7223 4.081-4.7223 3.95274-4.7223 3.82448-4.68732T3.5679600000000002-4.58238 3.33476-4.4308000000000005 3.13654-4.2559000000000005 2.96164-4.081 2.83338-3.91776 2.7401-3.80116L2.70512-3.75452Q2.70512-3.74286 2.67014-3.5912800000000002T2.54188-3.07824 2.37864-2.47192Q2.07548-1.23596 2.07548-1.18932ZM7.24723802-3.5846221399999996Q7.24723802-3.6670583400000005 7.2802125-3.7742254T7.37913594-3.88139246Q7.38737956-3.88139246 7.972676580000001-3.92261056T8.57446084-3.9720722800000003Q8.673384279999999-3.9720722800000003 8.673384279999999-3.9061233200000003 8.673384279999999-3.8484179800000002 8.26120328-2.18320674 7.849022280000001-0.55096998 7.849022280000001-0.49326464000000003 8.00565106-0.57570084 8.08808726-0.64989342 8.28593414-0.79827858 8.65689704-1.1692414800000002T9.23395044-1.6721023000000002Q9.563695240000001-1.8946800400000001 9.860465560000002-1.8946800400000001 10.10777416-1.8946800400000001 10.2479157-1.7380512600000002T10.39630086-1.29289578Q10.39630086-1.08680528 10.33859552-0.94666374T10.18196674-0.74057324 10.008850720000002-0.64989342 9.860465560000002-0.62516256Q9.69559316-0.62516256 9.596669720000001-0.71584238T9.49774628-0.95490736Q9.49774628-1.16099786 9.6296442-1.29289578T9.89344004-1.4660118L10.02533796-1.49074266Q9.92641452-1.5896661 9.82749108-1.5896661H9.7945166Q9.53072076-1.56493524 9.275168540000001-1.3835756T8.673384279999999-0.8559839200000001 8.12930536-0.3531231Q8.1457926-0.34487948 8.25295966-0.32839224T8.45080654-0.27893052 8.66514066-0.21298156 8.8877184-0.09757088000000001 9.06907804 0.050814279999999996 9.20921958 0.2651484 9.2586813 0.53718786Q9.2586813 0.6773294000000001 9.2174632 0.8751762800000001T9.168001480000001 1.2049210799999999Q9.168001480000001 1.53466588 9.39057922 1.53466588 9.78627298 1.53466588 10.03358158 0.5536751 10.05006882 0.48772614000000003 10.19845398 0.48772614000000003H10.23142846Q10.36332638 0.48772614000000003 10.36332638 0.56191872 10.36332638 0.60313682 10.33859552 0.6773294000000001 10.0006071 1.83967982 9.36584836 1.83967982 8.99488546 1.83967982 8.7640641 1.6253457000000002T8.53324274 1.0070742000000001Q8.53324274 0.8586890400000001 8.5579736 0.75152198T8.58270446 0.5536751Q8.58270446 0.37231546000000004 8.46729378 0.24041754T8.17876708 0.050814279999999996 7.898484-0.03986554 7.75009884-0.0645964H7.7336116Q7.63468816 0.28163564 7.56049558 0.60313682T7.4450849 1.08126678 7.37089232 1.38628072 7.31318698 1.5841276000000002 7.25548164 1.6912946599999998 7.16480182 1.76548724Q7.057634759999999 1.83967982 6.933980460000001 1.83967982 6.793838920000001 1.83967982 6.71964634 1.75724362T6.6454537600000005 1.61710208Q6.6454537600000005 1.53466588 6.950467700000001 0.29812288000000003T7.55225196-2.13374502L7.857265900000001-3.3290699200000002Q7.86550952-3.43623698 7.799560560000001-3.4609678400000004T7.4780593799999995-3.50218594H7.43684128Q7.354405080000001-3.50218594 7.329674219999999-3.50218594T7.27196888-3.5269168000000004 7.24723802-3.5846221399999996Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 196.68101222449195, 46.50966992251917)translate(196.68101222449195, 46.50966992251917)"
    string="p_l"
  >
    <title>\`d\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="9.289522"
      height="11.66"
      role="img"
      focusable="false"
      viewBox="-0.45474000000000003 -5.15372 8.89730292 7.41576"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.26818000000000003-3.34642Q0.27984-3.3814 0.2915-3.4397T0.3498-3.6962200000000003 0.46640000000000004-4.05768 0.6413-4.4424600000000005 0.8745-4.79226 1.17766-5.04878 1.56244-5.15372Q2.43694-5.15372 2.6818-4.4074800000000005L2.7984-4.51242Q3.5213200000000002-5.15372 4.17428-5.15372 4.93218-5.15372 5.3636-4.6057T5.79502-3.27646Q5.79502-2.01718 4.90886-0.95612T2.90334 0.11660000000000001Q2.64682 0.11660000000000001 2.4486 0.04664 2.32034-0.01166 2.1804200000000002-0.12826T1.95888-0.32648L1.87726-0.41976Q1.8656000000000001-0.4081 1.62074 0.59466T1.37588 1.60908Q1.37588 1.67904 1.46916 1.6907T1.90058 1.72568H2.1920800000000003Q2.26204 1.8073000000000001 2.26204 1.8306200000000001T2.2270600000000003 2.0405Q2.1920800000000003 2.1804200000000002 2.1571000000000002 2.2154000000000003T2.00552 2.26204Q1.9822 2.26204 1.87726 2.26204T1.48082 2.25038 0.7579 2.23872Q-0.058300000000000005 2.23872-0.27984 2.26204H-0.37312Q-0.45474000000000003 2.1804200000000002-0.45474000000000003 2.1337800000000002-0.43142 1.8189600000000001-0.30316 1.72568H-0.06996Q0.32648 1.71402 0.38478 1.58576 0.41976 1.5158 1.0960400000000001-1.20098T1.8073000000000001-4.081Q1.8189600000000001-4.1393 1.8189600000000001-4.2442400000000005 1.8189600000000001-4.7223 1.52746-4.7223 1.27094-4.7223 1.0960400000000001-4.3958200000000005T0.82786-3.6845600000000003 0.68794-3.2648Q0.66462-3.24148 0.50138-3.24148H0.33814Q0.26818000000000003-3.31144 0.26818000000000003-3.34642ZM2.07548-1.18932Q2.332-0.30316 2.93832-0.30316 3.28812-0.30316 3.6146000000000003-0.57134T4.15096-1.24762Q4.3608400000000005-1.64406 4.57072-2.5069T4.79226-3.7895V-3.85946Q4.79226-4.7223 4.081-4.7223 3.95274-4.7223 3.82448-4.68732T3.5679600000000002-4.58238 3.33476-4.4308000000000005 3.13654-4.2559000000000005 2.96164-4.081 2.83338-3.91776 2.7401-3.80116L2.70512-3.75452Q2.70512-3.74286 2.67014-3.5912800000000002T2.54188-3.07824 2.37864-2.47192Q2.07548-1.23596 2.07548-1.18932ZM7.214263540000001 1.2626264200000001Q7.214263540000001 1.53466588 7.42035404 1.53466588 7.72536798 1.53466588 7.9397021 0.6690857800000001 7.989163820000001 0.50421338 8.0221383 0.49596976000000004 8.03862554 0.48772614000000003 8.1045745 0.48772614000000003H8.13754898Q8.21174156 0.48772614000000003 8.23647242 0.48772614000000003T8.27769052 0.50421338 8.29417776 0.56191872Q8.28593414 0.61138044 8.2694469 0.69381664T8.17876708 1.0070742000000001 8.01389468 1.39452434 7.75834246 1.69953828 7.37913594 1.83967982Q7.082365620000001 1.83967982 6.8268134 1.6583201800000003T6.56301756 1.0482923Q6.56301756 0.9493688600000001 6.57126118 0.90815076L7.10709648-1.2187032Q7.62644454-3.3208263000000002 7.62644454-3.3867752600000003 7.62644454-3.41150612 7.61820092-3.42799336T7.5852264400000005-3.4609678400000004 7.54400834-3.47745508 7.4780593799999995-3.4856987 7.41211042-3.49394232 7.337917839999999-3.50218594 7.25548164-3.50218594Q7.17304544-3.50218594 7.14831458-3.50218594T7.082365620000001-3.51042956 7.0329039-3.5351604200000004 7.02466028-3.5846221399999996Q7.02466028-3.6011093800000005 7.04114752-3.70003282 7.082365620000001-3.8566616 7.13182734-3.87314884T7.72536798-3.92261056Q7.849022280000001-3.93085418 8.00565106-3.9390978T8.25295966-3.9638286600000003 8.34363948-3.9720722800000003Q8.44256292-3.9720722800000003 8.44256292-3.9061233200000003 8.44256292-3.8154435 7.840778660000001-1.43303732T7.222507160000001 1.06477954Q7.222507160000001 1.08126678 7.222507160000001 1.1307285T7.214263540000001 1.2131647V1.2626264200000001Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 172.56214675076802, 130.4490884258122)translate(172.56214675076802, 130.4490884258122)"
    string="m_{il}"
  >
    <title>\`u\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="16.507062"
      height="11.66"
      role="img"
      focusable="false"
      viewBox="0.24486 -5.15372 15.414251820000002 6.99339982"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.24486-3.34642Q0.25652-3.41638 0.27984-3.5329800000000002T0.41976-3.97606 0.65296-4.52408 1.02608-4.9555 1.53912-5.15372 2.0405-5.0721 2.3903-4.86222 2.57686-4.6057 2.67014-4.3841600000000005L2.69346-4.3025400000000005Q2.69346-4.2792200000000005 2.70512-4.2792200000000005L2.83338-4.4074800000000005Q3.5329800000000002-5.15372 4.47744-5.15372 4.67566-5.15372 4.8389-5.1304T5.14206-5.04878 5.3636-4.93218 5.5385-4.79226 5.6551-4.64068 5.74838-4.4891 5.79502-4.3491800000000005 5.83-4.2442400000000005 5.85332-4.16262L5.9466-4.2792200000000005Q6.68118-5.15372 7.68394-5.15372 8.31358-5.15372 8.698360000000001-4.8389T9.0948-3.91776Q9.0948-3.3231 8.651720000000001-2.07548T8.20864-0.583Q8.2203-0.41976 8.26694-0.36146T8.441840000000001-0.30316Q8.768320000000001-0.30316 9.04816-0.65296T9.5029-1.60908Q9.53788-1.73734 9.57286-1.7606600000000001T9.75942-1.7839800000000001Q9.99262-1.7839800000000001 9.99262-1.6907 9.99262-1.67904 9.94598-1.5158 9.8527-1.17766 9.68946-0.85118T9.1531-0.19822 8.34856 0.11660000000000001Q7.80054 0.11660000000000001 7.55568-0.19822T7.3108200000000005-0.85118Q7.3108200000000005-1.0727200000000001 7.73058-2.25038T8.162-4.0227Q8.162-4.71064 7.64896-4.71064H7.59066Q6.5879-4.71064 5.89996-3.5329800000000002L5.81834-3.39306 5.43356-1.8306200000000001Q5.04878-0.30316 4.99048-0.18656 4.8389 0.12826 4.4891 0.12826 4.3375200000000005 0.12826 4.2442400000000005 0.04664T4.11598-0.09328 4.081-0.20988Q4.081-0.33814 4.47744-1.87726L4.8972-3.5796200000000002Q4.93218-3.75452 4.93218-4.0227 4.93218-4.71064 4.4191400000000005-4.71064H4.3608400000000005Q3.35808-4.71064 2.67014-3.5329800000000002L2.58852-3.39306 2.2037400000000003-1.8306200000000001Q1.8189600000000001-0.30316 1.7606600000000001-0.18656 1.60908 0.12826 1.25928 0.12826 1.1077000000000001 0.12826 1.01442 0.058300000000000005T0.8861600000000001-0.08162 0.86284-0.19822Q0.86284-0.3498 1.30592-2.1104600000000002 1.7606600000000001-3.9061 1.7606600000000001-3.98772 1.7956400000000001-4.16262 1.7956400000000001-4.3025400000000005 1.7956400000000001-4.7223 1.50414-4.7223 1.24762-4.7223 1.0727200000000001-4.3958200000000005T0.80454-3.6845600000000003 0.66462-3.2648Q0.6413-3.24148 0.47806-3.24148H0.31482Q0.24486-3.31144 0.24486-3.34642ZM12.13908608-3.197172Q12.13908608-3.3950188800000003 12.29571486-3.54340404T12.658434139999999-3.70003282Q12.8068193-3.70003282 12.905742739999999-3.6011093800000005T13.0129098-3.35380078Q13.0129098-3.16419752 12.8480374-3.0075687400000004T12.485318119999999-2.8426963400000003Q12.361663819999999-2.8426963400000003 12.254496760000002-2.92513254T12.13908608-3.197172ZM10.79537602-0.61691894Q10.79537602-0.6828679 10.869568600000001-0.8724711599999999T11.06741548-1.29289578 11.43013476-1.7133204 11.92475196-1.8946800400000001Q12.24625314-1.8946800400000001 12.46058726-1.7050767800000002T12.683165-1.19397234Q12.683165-1.0538308 12.56775432-0.73232962T12.238009519999999 0.13325048 11.89177748 1.06477954Q11.85055938 1.2461391800000001 11.85055938 1.32857538 11.85055938 1.53466588 11.990700920000002 1.53466588 12.064893499999998 1.53466588 12.1473297 1.5099350200000001T12.33693296 1.39452434 12.5595107 1.10599764 12.765601199999999 0.61962406Q12.79033206 0.52070062 12.8068193 0.50421338T12.946960840000001 0.48772614000000003Q13.11183324 0.48772614000000003 13.11183324 0.57016234 13.11183324 0.6361113 13.037640660000001 0.8257145600000001T12.83155016 1.2461391800000001 12.46058726 1.6583201800000003 11.94948282 1.83967982Q11.68568698 1.83967982 11.463109240000001 1.6665638T11.23228788 1.13897212Q11.23228788 0.99883058 11.27350598 0.8751762800000001T11.62798164-0.0645964Q11.941239199999998-0.89720202 11.990700920000002-1.06207442T12.04840626-1.3835756Q12.04840626-1.5814224799999999 11.90826472-1.5814224799999999H11.89177748Q11.64446888-1.5814224799999999 11.438378380000001-1.30938302T11.12512082-0.61691894Q11.1168772-0.60867532 11.10863358-0.59218808T11.10038996-0.56745722 11.08390272-0.55096998 11.059171860000001-0.54272636 11.026197380000001-0.54272636 10.96024842-0.54272636H10.844837740000001Q10.79537602-0.59218808 10.79537602-0.61691894ZM14.43081244 1.2626264200000001Q14.43081244 1.53466588 14.63690294 1.53466588 14.94191688 1.53466588 15.156251 0.6690857800000001 15.205712720000001 0.50421338 15.238687200000001 0.49596976000000004 15.255174440000001 0.48772614000000003 15.321123400000001 0.48772614000000003H15.35409788Q15.428290460000001 0.48772614000000003 15.45302132 0.48772614000000003T15.49423942 0.50421338 15.51072666 0.56191872Q15.502483040000001 0.61138044 15.485995800000001 0.69381664T15.395315980000001 1.0070742000000001 15.23044358 1.39452434 14.97489136 1.69953828 14.595684839999999 1.83967982Q14.298914519999999 1.83967982 14.0433623 1.6583201800000003T13.77956646 1.0482923Q13.77956646 0.9493688600000001 13.787810080000002 0.90815076L14.32364538-1.2187032Q14.842993439999999-3.3208263000000002 14.842993439999999-3.3867752600000003 14.842993439999999-3.41150612 14.83474982-3.42799336T14.80177534-3.4609678400000004 14.76055724-3.47745508 14.69460828-3.4856987 14.62865932-3.49394232 14.55446674-3.50218594 14.472030540000002-3.50218594Q14.38959434-3.50218594 14.36486348-3.50218594T14.298914519999999-3.51042956 14.2494528-3.5351604200000004 14.24120918-3.5846221399999996Q14.24120918-3.6011093800000005 14.25769642-3.70003282 14.298914519999999-3.8566616 14.34837624-3.87314884T14.94191688-3.92261056Q15.065571180000001-3.93085418 15.222199960000001-3.9390978T15.46950856-3.9638286600000003 15.56018838-3.9720722800000003Q15.659111820000001-3.9720722800000003 15.659111820000001-3.9061233200000003 15.659111820000001-3.8154435 15.057327560000001-1.43303732T14.439056059999999 1.06477954Q14.439056059999999 1.08126678 14.439056059999999 1.1307285T14.43081244 1.2131647V1.2626264200000001Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 148.66241350874378, 77.93324859253272)translate(148.66241350874378, 77.93324859253272)"
    string="m_{jl}"
  >
    <title>\`v\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="17.037042712124112"
      height="12.16"
      role="img"
      focusable="false"
      viewBox="0.24455085232714038 -5.404573836429803 15.946415782417159 8.57358023367171"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.24455085232714038-3.599555640681862Q0.25619613100938515-3.6694273127753303 0.2794866883738747-3.785880099597778T0.4192300325608121-4.22840068952308 0.6521356062057077-4.7757287875885845 1.0247845240375406-5.206604098831641 1.537176786056311-5.404573836429803 2.0379237693928367-5.323056885654089 2.38728212986018-5.113441869373683 2.573606588776096-4.857245738364298 2.6667688182340545-4.635985443401647L2.690059375598544-4.554468492625934Q2.690059375598544-4.531177935261444 2.701704654280789-4.531177935261444L2.8298027197854814-4.659276000766137Q3.5285194407201685-5.404573836429803 4.471787013981995-5.404573836429803 4.669756751580157-5.404573836429803 4.832790653131584-5.381283279065313T5.135567898869948-5.2997663282896 5.356828193832599-5.183313541467152 5.531507374066271-5.043570197280214 5.647960160888719-4.892181574411032 5.741122390346677-4.7407929515418505 5.787703505075656-4.601049607354913 5.82263934112239-4.49624209921471 5.8459298984868795-4.414725148438996L5.939092127944837-4.531177935261444Q6.672744684926259-5.404573836429803 7.67423865159931-5.404573836429803 8.303083700440528-5.404573836429803 8.687377896954606-5.090151312009193T9.083317372150928-4.170174296111856Q9.083317372150928-3.5762650833173724 8.640796782225626-2.3302202643171803T8.198276192300325-0.8396245929898486Q8.20992147098257-0.6765906914384218 8.25650258571155-0.6183642980271978T8.43118176594522-0.5601379046159739Q8.757249569048074-0.5601379046159739 9.03673625742195-0.9094962650833173T9.490902126029496-1.8644091170273893Q9.52583796207623-1.9925071825320817 9.560773798122964-2.015797739896571T9.74709825703888-2.039088297261061Q9.980003830683776-2.039088297261061 9.980003830683776-1.9459260678031027 9.980003830683776-1.934280789120858 9.933422715954798-1.771246887569431 9.84026048649684-1.4335338057843323 9.677226584945412-1.1074660026814784T9.141543765562153-0.4553303964757709 8.338019536487263-0.14090787205516184Q7.790691438421757-0.14090787205516184 7.546140586094618-0.4553303964757709T7.301589733767477-1.1074660026814784Q7.301589733767477-1.3287262976441294 7.720819766328289-2.504899444550852T8.151695077571347-4.274981804252059Q8.151695077571347-4.962053246504501 7.6393028155525755-4.962053246504501H7.581076422141352Q6.579582455468301-4.962053246504501 5.8925110132158585-3.785880099597778L5.8109940624401455-3.646136755410841 5.426699865926068-2.08566941199004Q5.042405669411989-0.5601379046159739 4.9841792760007655-0.44368511779352615 4.832790653131584-0.12926259337291707 4.48343229266424-0.12926259337291707 4.332043669795058-0.12926259337291707 4.2388814403371-0.21077954414863054T4.110783374832407-0.3505228883355679 4.075847538785673-0.4669756751580157Q4.075847538785673-0.5950737406627082 4.471787013981995-2.132250526719019L4.8910170465428076-3.8324612143267576Q4.925952882589542-4.007140394560429 4.925952882589542-4.274981804252059 4.925952882589542-4.962053246504501 4.413560620570772-4.962053246504501H4.355334227159548Q3.3538402604864967-4.962053246504501 2.6667688182340545-3.785880099597778L2.585251867458341-3.646136755410841 2.2009576709442635-2.08566941199004Q1.8166634744301857-0.5601379046159739 1.7584370810189618-0.44368511779352615 1.6070484581497797-0.12926259337291707 1.2576900976824363-0.12926259337291707 1.106301474813254-0.12926259337291707 1.0131392453552959-0.19913426546638577T0.8850411798506033-0.33887760965332314 0.8617506224861137-0.4553303964757709Q0.8617506224861137-0.6067190193449531 1.3042712124114153-2.3651561003639148 1.7584370810189618-4.158529017429611 1.7584370810189618-4.240045968205325 1.793372917065696-4.414725148438996 1.793372917065696-4.554468492625934 1.793372917065696-4.973698525186745 1.5022409500095766-4.973698525186745 1.2460448190001914-4.973698525186745 1.0713656387665198-4.647630722083892T0.8035242290748899-3.9372687224669605 0.6637808848879525-3.5180386899061484Q0.6404903275234629-3.494748132541659 0.47745642597203597-3.494748132541659H0.3144225244206091Q0.24455085232714038-3.5646198046351274 0.24455085232714038-3.599555640681862ZM13.054112851944073-3.4175632254357398Q13.054112851944073-3.6727927983144992 13.227010304539359-3.8127574027963993T13.581038421758285-3.9527220072782994Q13.721003026240185-3.9527220072782994 13.811568358552-3.8703898869948286T13.92683332694886-3.6398599502011106Q13.92683332694886-3.4093300134073927 13.770402298410266-3.252898984868799T13.408140969162996-3.096467956330205Q13.259943152652747-3.096467956330205 13.161144608312584-3.1788000766136753T13.054112851944073-3.4175632254357398ZM12.980013943688947-1.6062565791993868Q12.980013943688947-1.8450197280214518 12.765950430951923-1.8450197280214518 12.58481976632829-1.8450197280214518 12.420155525761347-1.7462211836812869T12.131993104769201-1.490991610802528 11.93439601608887-1.186362765753687 11.79443141160697-0.9228999808465811L11.736798927408541-0.8076350124497221Q11.720332503351848-0.7994018004213752 11.605067534954989-0.7994018004213752H11.48980256655813Q11.440403294388048-0.8488010725914575 11.440403294388048-0.8652674966481517T11.473336142501436-0.9722992530166633Q11.646233595096724-1.3757266424056693 11.959095652173913-1.729754759624593T12.683618310668454-2.141415361041946Q12.691851522696801-2.141415361041946 12.74948400689523-2.141415361041946T12.848282551235396-2.149648573070293Q13.045879639915723-2.141415361041946 13.210543880482666-2.067316452786822 13.416374181191342-1.9520514843899635 13.523405937559854-1.8120868799080634T13.630437693928366-1.3757266424056693V-1.2439952499521163L13.227010304539359 0.39441394368894844Q12.807116491093659 2.0410563493583602 12.782416855008616 2.106922045585137 12.634219038498372 2.5185826470024897 12.206092013024323 2.8396779161080254T11.226339781651024 3.1690063972419074Q10.81467918023367 3.1690063972419074 10.666481363723424 2.996108944646619T10.510050335184829 2.6173811913426546Q10.510050335184829 2.3950844665772837 10.666481363723424 2.2386534380386895T11.045209117027389 2.073989197471749Q11.185173721509289 2.073989197471749 11.283972265849455 2.156321317755219T11.391004022217965 2.411550890633978Q11.391004022217965 2.707946523654472 11.12754123731086 2.864377552193066 11.176940509480941 2.872610764221413 11.242806205707717 2.872610764221413 11.522735414671518 2.872610764221413 11.753265351465236 2.6420808274276957T12.107293468684158 2.0986888335567895L12.197858800995977 1.7528939283662133Q12.288424133307796 1.3988658111472898 12.411922313733001 0.896639877418119T12.675385098640108-0.13251162612526335 12.897681823405478-1.046398161271787 12.988247155717296-1.5156912468875694Q12.988247155717296-1.5733237310859989 12.980013943688947-1.6062565791993868ZM14.964218042520589 1.003671633786631Q14.964218042520589 1.2753676307220838 15.170048343229267 1.2753676307220838 15.474677188278106 1.2753676307220838 15.688740701015131 0.41088036774564257 15.738139973185213 0.2462161271787014 15.7710728212986 0.23798291515035433 15.787539245355294 0.22974970312200726 15.85340494158207 0.22974970312200726H15.886337789695459Q15.960436697950582 0.22974970312200726 15.985136334035625 0.22974970312200726T16.026302394177357 0.2462161271787014 16.04276881823405 0.30384861137713076Q16.034535606205708 0.35324788354721315 16.018069182149013 0.4355800038306838T15.927503849837194 0.7484420609078719 15.762839609270255 1.1354030262401837 15.507610036391494 1.440031871289025 15.128882283087531 1.5799964757709248Q14.832486650067036 1.5799964757709248 14.577257077188277 1.3988658111472898T14.313794292281173 0.7896081210496074Q14.313794292281173 0.6908095767094425 14.322027504309519 0.6496435165677074L14.857186286152077-1.474525186745834Q15.375878643937943-3.573994253974334 15.375878643937943-3.6398599502011106 15.375878643937943-3.664559586286152 15.367645431909594-3.681026010342846T15.334712583796206-3.713958858456234 15.29354652365447-3.7304252825129285 15.227680827427694-3.7386584945412755 15.16181513120092-3.7468917065696226 15.087716222945794-3.7551249185979696 15.005384102662324-3.7551249185979696Q14.923051982378855-3.7551249185979696 14.898352346293812-3.7551249185979696T14.832486650067036-3.7633581306263166 14.783087377896955-3.7880577667113573 14.774854165868605-3.83745703888144Q14.774854165868605-3.853923462938134 14.7913205899253-3.9527220072782994 14.832486650067036-4.1091530358168935 14.88188592223712-4.125619459873588T15.474677188278106-4.17501873204367Q15.598175368703313-4.183251944072016 15.754606397241906-4.191485156100364T16.001602758092318-4.2161847921854045 16.092168090404137-4.224418004213752Q16.1909666347443-4.224418004213752 16.1909666347443-4.158552307986975 16.1909666347443-4.067986975675158 15.589942156674965-1.6885886994828576T14.972451254548936 0.8060745451063014Q14.972451254548936 0.8225409691629955 14.972451254548936 0.8719402413330779T14.964218042520589 0.9542723616165484V1.003671633786631Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 107.46839882915899, 128.0414465940402)translate(107.46839882915899, 128.0414465940402)"
    string="m_{ij}"
  >
    <title>\`x\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="17.424830492242865"
      height="12.16"
      role="img"
      focusable="false"
      viewBox="0.24455085232714038 -5.404573836429803 16.522740624401454 8.57358023367171"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.24455085232714038-3.599555640681862Q0.25619613100938515-3.6694273127753303 0.2794866883738747-3.785880099597778T0.4192300325608121-4.22840068952308 0.6521356062057077-4.7757287875885845 1.0247845240375406-5.206604098831641 1.537176786056311-5.404573836429803 2.0379237693928367-5.323056885654089 2.38728212986018-5.113441869373683 2.573606588776096-4.857245738364298 2.6667688182340545-4.635985443401647L2.690059375598544-4.554468492625934Q2.690059375598544-4.531177935261444 2.701704654280789-4.531177935261444L2.8298027197854814-4.659276000766137Q3.5285194407201685-5.404573836429803 4.471787013981995-5.404573836429803 4.669756751580157-5.404573836429803 4.832790653131584-5.381283279065313T5.135567898869948-5.2997663282896 5.356828193832599-5.183313541467152 5.531507374066271-5.043570197280214 5.647960160888719-4.892181574411032 5.741122390346677-4.7407929515418505 5.787703505075656-4.601049607354913 5.82263934112239-4.49624209921471 5.8459298984868795-4.414725148438996L5.939092127944837-4.531177935261444Q6.672744684926259-5.404573836429803 7.67423865159931-5.404573836429803 8.303083700440528-5.404573836429803 8.687377896954606-5.090151312009193T9.083317372150928-4.170174296111856Q9.083317372150928-3.5762650833173724 8.640796782225626-2.3302202643171803T8.198276192300325-0.8396245929898486Q8.20992147098257-0.6765906914384218 8.25650258571155-0.6183642980271978T8.43118176594522-0.5601379046159739Q8.757249569048074-0.5601379046159739 9.03673625742195-0.9094962650833173T9.490902126029496-1.8644091170273893Q9.52583796207623-1.9925071825320817 9.560773798122964-2.015797739896571T9.74709825703888-2.039088297261061Q9.980003830683776-2.039088297261061 9.980003830683776-1.9459260678031027 9.980003830683776-1.934280789120858 9.933422715954798-1.771246887569431 9.84026048649684-1.4335338057843323 9.677226584945412-1.1074660026814784T9.141543765562153-0.4553303964757709 8.338019536487263-0.14090787205516184Q7.790691438421757-0.14090787205516184 7.546140586094618-0.4553303964757709T7.301589733767477-1.1074660026814784Q7.301589733767477-1.3287262976441294 7.720819766328289-2.504899444550852T8.151695077571347-4.274981804252059Q8.151695077571347-4.962053246504501 7.6393028155525755-4.962053246504501H7.581076422141352Q6.579582455468301-4.962053246504501 5.8925110132158585-3.785880099597778L5.8109940624401455-3.646136755410841 5.426699865926068-2.08566941199004Q5.042405669411989-0.5601379046159739 4.9841792760007655-0.44368511779352615 4.832790653131584-0.12926259337291707 4.48343229266424-0.12926259337291707 4.332043669795058-0.12926259337291707 4.2388814403371-0.21077954414863054T4.110783374832407-0.3505228883355679 4.075847538785673-0.4669756751580157Q4.075847538785673-0.5950737406627082 4.471787013981995-2.132250526719019L4.8910170465428076-3.8324612143267576Q4.925952882589542-4.007140394560429 4.925952882589542-4.274981804252059 4.925952882589542-4.962053246504501 4.413560620570772-4.962053246504501H4.355334227159548Q3.3538402604864967-4.962053246504501 2.6667688182340545-3.785880099597778L2.585251867458341-3.646136755410841 2.2009576709442635-2.08566941199004Q1.8166634744301857-0.5601379046159739 1.7584370810189618-0.44368511779352615 1.6070484581497797-0.12926259337291707 1.2576900976824363-0.12926259337291707 1.106301474813254-0.12926259337291707 1.0131392453552959-0.19913426546638577T0.8850411798506033-0.33887760965332314 0.8617506224861137-0.4553303964757709Q0.8617506224861137-0.6067190193449531 1.3042712124114153-2.3651561003639148 1.7584370810189618-4.158529017429611 1.7584370810189618-4.240045968205325 1.793372917065696-4.414725148438996 1.793372917065696-4.554468492625934 1.793372917065696-4.973698525186745 1.5022409500095766-4.973698525186745 1.2460448190001914-4.973698525186745 1.0713656387665198-4.647630722083892T0.8035242290748899-3.9372687224669605 0.6637808848879525-3.5180386899061484Q0.6404903275234629-3.494748132541659 0.47745642597203597-3.494748132541659H0.3144225244206091Q0.24455085232714038-3.5646198046351274 0.24455085232714038-3.599555640681862ZM12.123759892740853-3.4504960735491284Q12.123759892740853-3.6480931622294572 12.280190921279447-3.7962909787397052T12.642452250526716-3.9527220072782994Q12.790650067036966-3.9527220072782994 12.88944861137713-3.853923462938134T12.996480367745642-3.6069271020877225Q12.996480367745642-3.4175632254357398 12.831816127178701-3.2611321968971456T12.46955479793143-3.096467956330205Q12.346056617506223-3.096467956330205 12.239024861137713-3.1788000766136753T12.123759892740853-3.4504960735491284ZM10.781746332120283-0.8735007086764988Q10.781746332120283-0.9393664049032753 10.855845240375407-1.1287302815552576T11.053442329055736-1.5486240950009575 11.415703658303006-1.9685179084466575 11.90969638000383-2.149648573070293Q12.230791649109365-2.149648573070293 12.44485516184639-1.9602846964183105T12.667151886611759-1.4498255506607929Q12.667151886611759-1.3098609461788928 12.551886918214901-0.9887656770733575T12.222558437081016-0.1242784140969163 11.876763531890441 0.8060745451063014Q11.835597471748706 0.9872052097299366 11.835597471748706 1.0695373300134075 11.835597471748706 1.2753676307220838 11.975562076230608 1.2753676307220838 12.04966098448573 1.2753676307220838 12.131993104769201 1.2506679946370427T12.321356981421184 1.1354030262401837 12.543653706186554 0.8472406052480368 12.74948400689523 0.36148109557556024Q12.774183642980272 0.26268255123539547 12.790650067036966 0.2462161271787014T12.930614671518866 0.22974970312200726Q13.095278912085805 0.22974970312200726 13.095278912085805 0.31208182340547785 13.095278912085805 0.3779475196322543 13.021180003830684 0.5673113962842368T12.815349703122008 0.9872052097299366 12.44485516184639 1.3988658111472898 11.93439601608887 1.5799964757709248Q11.670933231181765 1.5799964757709248 11.448636506416396 1.4070990231756368T11.218106569622677 0.8801734533614249Q11.218106569622677 0.740208848879525 11.259272629764412 0.616710668454319T11.613300746983336-0.32187550277724575Q11.926162804060523-1.1534299176402987 11.975562076230608-1.3180941582072399T12.033194560429035-1.6391894273127752Q12.033194560429035-1.8367865159931047 11.893229955947136-1.8367865159931047H11.876763531890441Q11.62976717104003-1.8367865159931047 11.423936870331355-1.5650905190576516T11.111074813254165-0.8735007086764988Q11.102841601225817-0.8652674966481517 11.09460838919747-0.8488010725914575T11.086375177169124-0.8241014365064163 11.06990875311243-0.8076350124497221 11.045209117027389-0.7994018004213752 11.012276268914-0.7994018004213752 10.946410572687224-0.7994018004213752H10.831145604290366Q10.781746332120283-0.8488010725914575 10.781746332120283-0.8735007086764988ZM15.894571001723806-3.4175632254357398Q15.894571001723806-3.6727927983144992 16.067468454319094-3.8127574027963993T16.421496571538018-3.9527220072782994Q16.561461176019918-3.9527220072782994 16.652026508331737-3.8703898869948286T16.767291476728595-3.6398599502011106Q16.767291476728595-3.4093300134073927 16.61086044819-3.252898984868799T16.24859911894273-3.096467956330205Q16.100401302432484-3.096467956330205 16.001602758092318-3.1788000766136753T15.894571001723806-3.4175632254357398ZM15.820472093468682-1.6062565791993868Q15.820472093468682-1.8450197280214518 15.606408580731658-1.8450197280214518 15.425277916108024-1.8450197280214518 15.260613675541082-1.7462211836812869T14.972451254548936-1.490991610802528 14.774854165868605-1.186362765753687 14.634889561386705-0.9228999808465811L14.577257077188277-0.8076350124497221Q14.560790653131582-0.7994018004213752 14.445525684734724-0.7994018004213752H14.330260716337866Q14.280861444167781-0.8488010725914575 14.280861444167781-0.8652674966481517T14.313794292281173-0.9722992530166633Q14.48669174487646-1.3757266424056693 14.799553801953648-1.729754759624593T15.524076460448189-2.141415361041946Q15.532309672476536-2.141415361041946 15.589942156674965-2.141415361041946T15.688740701015131-2.149648573070293Q15.886337789695459-2.141415361041946 16.0510020302624-2.067316452786822 16.256832330971076-1.9520514843899635 16.36386408733959-1.8120868799080634T16.4708958437081-1.3757266424056693V-1.2439952499521163L16.067468454319094 0.39441394368894844Q15.647574640873394 2.0410563493583602 15.622875004788355 2.106922045585137 15.474677188278106 2.5185826470024897 15.04655016280406 2.8396779161080254T14.06679793143076 3.1690063972419074Q13.655137330013405 3.1690063972419074 13.50693951350316 2.996108944646619T13.350508484964566 2.6173811913426546Q13.350508484964566 2.3950844665772837 13.50693951350316 2.2386534380386895T13.885667266807124 2.073989197471749Q14.025631871289024 2.073989197471749 14.124430415629188 2.156321317755219T14.2314621719977 2.411550890633978Q14.2314621719977 2.707946523654472 13.967999387090593 2.864377552193066 14.017398659260678 2.872610764221413 14.083264355487454 2.872610764221413 14.363193564451255 2.872610764221413 14.593723501244973 2.6420808274276957T14.947751618463894 2.0986888335567895L15.038316950775712 1.7528939283662133Q15.128882283087531 1.3988658111472898 15.252380463512736 0.896639877418119T15.515843248419843-0.13251162612526335 15.738139973185213-1.046398161271787 15.828705305497031-1.5156912468875694Q15.828705305497031-1.5733237310859989 15.820472093468682-1.6062565791993868Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 16.157167136422707, 77.29210986581441)translate(16.157167136422707, 77.29210986581441)"
    string="m_{jk}"
  >
    <title>\`y\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="18.873503160314115"
      height="12.16"
      role="img"
      focusable="false"
      viewBox="0.24455085232714038 -5.404573836429803 17.897687033135412 8.57358023367171"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.24455085232714038-3.599555640681862Q0.25619613100938515-3.6694273127753303 0.2794866883738747-3.785880099597778T0.4192300325608121-4.22840068952308 0.6521356062057077-4.7757287875885845 1.0247845240375406-5.206604098831641 1.537176786056311-5.404573836429803 2.0379237693928367-5.323056885654089 2.38728212986018-5.113441869373683 2.573606588776096-4.857245738364298 2.6667688182340545-4.635985443401647L2.690059375598544-4.554468492625934Q2.690059375598544-4.531177935261444 2.701704654280789-4.531177935261444L2.8298027197854814-4.659276000766137Q3.5285194407201685-5.404573836429803 4.471787013981995-5.404573836429803 4.669756751580157-5.404573836429803 4.832790653131584-5.381283279065313T5.135567898869948-5.2997663282896 5.356828193832599-5.183313541467152 5.531507374066271-5.043570197280214 5.647960160888719-4.892181574411032 5.741122390346677-4.7407929515418505 5.787703505075656-4.601049607354913 5.82263934112239-4.49624209921471 5.8459298984868795-4.414725148438996L5.939092127944837-4.531177935261444Q6.672744684926259-5.404573836429803 7.67423865159931-5.404573836429803 8.303083700440528-5.404573836429803 8.687377896954606-5.090151312009193T9.083317372150928-4.170174296111856Q9.083317372150928-3.5762650833173724 8.640796782225626-2.3302202643171803T8.198276192300325-0.8396245929898486Q8.20992147098257-0.6765906914384218 8.25650258571155-0.6183642980271978T8.43118176594522-0.5601379046159739Q8.757249569048074-0.5601379046159739 9.03673625742195-0.9094962650833173T9.490902126029496-1.8644091170273893Q9.52583796207623-1.9925071825320817 9.560773798122964-2.015797739896571T9.74709825703888-2.039088297261061Q9.980003830683776-2.039088297261061 9.980003830683776-1.9459260678031027 9.980003830683776-1.934280789120858 9.933422715954798-1.771246887569431 9.84026048649684-1.4335338057843323 9.677226584945412-1.1074660026814784T9.141543765562153-0.4553303964757709 8.338019536487263-0.14090787205516184Q7.790691438421757-0.14090787205516184 7.546140586094618-0.4553303964757709T7.301589733767477-1.1074660026814784Q7.301589733767477-1.3287262976441294 7.720819766328289-2.504899444550852T8.151695077571347-4.274981804252059Q8.151695077571347-4.962053246504501 7.6393028155525755-4.962053246504501H7.581076422141352Q6.579582455468301-4.962053246504501 5.8925110132158585-3.785880099597778L5.8109940624401455-3.646136755410841 5.426699865926068-2.08566941199004Q5.042405669411989-0.5601379046159739 4.9841792760007655-0.44368511779352615 4.832790653131584-0.12926259337291707 4.48343229266424-0.12926259337291707 4.332043669795058-0.12926259337291707 4.2388814403371-0.21077954414863054T4.110783374832407-0.3505228883355679 4.075847538785673-0.4669756751580157Q4.075847538785673-0.5950737406627082 4.471787013981995-2.132250526719019L4.8910170465428076-3.8324612143267576Q4.925952882589542-4.007140394560429 4.925952882589542-4.274981804252059 4.925952882589542-4.962053246504501 4.413560620570772-4.962053246504501H4.355334227159548Q3.3538402604864967-4.962053246504501 2.6667688182340545-3.785880099597778L2.585251867458341-3.646136755410841 2.2009576709442635-2.08566941199004Q1.8166634744301857-0.5601379046159739 1.7584370810189618-0.44368511779352615 1.6070484581497797-0.12926259337291707 1.2576900976824363-0.12926259337291707 1.106301474813254-0.12926259337291707 1.0131392453552959-0.19913426546638577T0.8850411798506033-0.33887760965332314 0.8617506224861137-0.4553303964757709Q0.8617506224861137-0.6067190193449531 1.3042712124114153-2.3651561003639148 1.7584370810189618-4.158529017429611 1.7584370810189618-4.240045968205325 1.793372917065696-4.414725148438996 1.793372917065696-4.554468492625934 1.793372917065696-4.973698525186745 1.5022409500095766-4.973698525186745 1.2460448190001914-4.973698525186745 1.0713656387665198-4.647630722083892T0.8035242290748899-3.9372687224669605 0.6637808848879525-3.5180386899061484Q0.6404903275234629-3.494748132541659 0.47745642597203597-3.494748132541659H0.3144225244206091Q0.24455085232714038-3.5646198046351274 0.24455085232714038-3.599555640681862ZM13.054112851944073-3.4175632254357398Q13.054112851944073-3.6727927983144992 13.227010304539359-3.8127574027963993T13.581038421758285-3.9527220072782994Q13.721003026240185-3.9527220072782994 13.811568358552-3.8703898869948286T13.92683332694886-3.6398599502011106Q13.92683332694886-3.4093300134073927 13.770402298410266-3.252898984868799T13.408140969162996-3.096467956330205Q13.259943152652747-3.096467956330205 13.161144608312584-3.1788000766136753T13.054112851944073-3.4175632254357398ZM12.980013943688947-1.6062565791993868Q12.980013943688947-1.8450197280214518 12.765950430951923-1.8450197280214518 12.58481976632829-1.8450197280214518 12.420155525761347-1.7462211836812869T12.131993104769201-1.490991610802528 11.93439601608887-1.186362765753687 11.79443141160697-0.9228999808465811L11.736798927408541-0.8076350124497221Q11.720332503351848-0.7994018004213752 11.605067534954989-0.7994018004213752H11.48980256655813Q11.440403294388048-0.8488010725914575 11.440403294388048-0.8652674966481517T11.473336142501436-0.9722992530166633Q11.646233595096724-1.3757266424056693 11.959095652173913-1.729754759624593T12.683618310668454-2.141415361041946Q12.691851522696801-2.141415361041946 12.74948400689523-2.141415361041946T12.848282551235396-2.149648573070293Q13.045879639915723-2.141415361041946 13.210543880482666-2.067316452786822 13.416374181191342-1.9520514843899635 13.523405937559854-1.8120868799080634T13.630437693928366-1.3757266424056693V-1.2439952499521163L13.227010304539359 0.39441394368894844Q12.807116491093659 2.0410563493583602 12.782416855008616 2.106922045585137 12.634219038498372 2.5185826470024897 12.206092013024323 2.8396779161080254T11.226339781651024 3.1690063972419074Q10.81467918023367 3.1690063972419074 10.666481363723424 2.996108944646619T10.510050335184829 2.6173811913426546Q10.510050335184829 2.3950844665772837 10.666481363723424 2.2386534380386895T11.045209117027389 2.073989197471749Q11.185173721509289 2.073989197471749 11.283972265849455 2.156321317755219T11.391004022217965 2.411550890633978Q11.391004022217965 2.707946523654472 11.12754123731086 2.864377552193066 11.176940509480941 2.872610764221413 11.242806205707717 2.872610764221413 11.522735414671518 2.872610764221413 11.753265351465236 2.6420808274276957T12.107293468684158 2.0986888335567895L12.197858800995977 1.7528939283662133Q12.288424133307796 1.3988658111472898 12.411922313733001 0.896639877418119T12.675385098640108-0.13251162612526335 12.897681823405478-1.046398161271787 12.988247155717296-1.5156912468875694Q12.988247155717296-1.5733237310859989 12.980013943688947-1.6062565791993868ZM14.997150890633977-3.83745703888144Q14.997150890633977-3.9197891591649108 15.030083738747367-4.026820915533423T15.128882283087531-4.133852671901934Q15.137115495115877-4.133852671901934 15.72167354912852-4.17501873204367T16.322698027197852-4.224418004213752Q16.421496571538018-4.224418004213752 16.421496571538018-4.158552307986975 16.421496571538018-4.100919823788546 16.009835970120665-2.43781099406244 15.598175368703313-0.8076350124497221 15.598175368703313-0.7500025282512928 15.754606397241906-0.8323346485347635 15.836938517525377-0.906433556789887 16.034535606205708-1.054631373300134 16.405030147481327-1.4251259145757518T16.98135498946562-1.9273518483049223Q17.3106834705995-2.149648573070293 17.607079103619995-2.149648573070293 17.85407546447041-2.149648573070293 17.99404006895231-1.9932175445316989T18.142237885462553-1.5486240950009575Q18.142237885462553-1.3427937942922812 18.084605401264124-1.2028291898103811T17.92817437272553-0.9969988891017046 17.755276920130243-0.906433556789887 17.607079103619995-0.8817339207048457Q17.442414863053052-0.8817339207048457 17.34361631871289-0.9722992530166633T17.244817774372727-1.2110624018387282Q17.244817774372727-1.4168927025474047 17.376549166826276-1.5486240950009575T17.640011951733385-1.721521547596246L17.771743344186934-1.7462211836812869Q17.67294479984677-1.8450197280214518 17.57414625550661-1.8450197280214518H17.54121340739322Q17.277750622486113-1.8203200919364104 17.022521049607352-1.6391894273127752T16.421496571538018-1.1122638574985635 15.878104577667113-0.6100379237693928Q15.894571001723806-0.6018047117410457 16.001602758092318-0.5853382876843517T16.19919984677265-0.5359390155142693 16.41326335950967-0.4700733192874928 16.635560084275042-0.35480835089063395 16.816690748898676-0.2066105343803869 16.956655353380576 0.007452978356636659 17.00605462555066 0.2791489752920896Q17.00605462555066 0.41911357977398966 16.964888565408927 0.616710668454319T16.915489293238842 0.9460391495882015Q16.915489293238842 1.2753676307220838 17.137786018004213 1.2753676307220838 17.53298019536487 1.2753676307220838 17.779976556215285 0.2956153993487838 17.796442980271976 0.22974970312200726 17.944640796782224 0.22974970312200726H17.977573644895614Q18.109305037349166 0.22974970312200726 18.109305037349166 0.30384861137713076 18.109305037349166 0.3450146715188661 18.084605401264124 0.41911357977398966 17.747043708101895 1.5799964757709248 17.11308638191917 1.5799964757709248 16.742591840643552 1.5799964757709248 16.512061903849837 1.3659329630339014T16.281531967056118 0.7484420609078719Q16.281531967056118 0.6002442443976249 16.30623160314116 0.49321248802911316T16.3309312392262 0.2956153993487838Q16.3309312392262 0.11448473472514843 16.21566627082934-0.01724665772840452T15.927503849837194-0.2066105343803869 15.647574640873394-0.2971758666922045 15.499376824363148-0.32187550277724575H15.482910400306455Q15.384111855966289 0.023919402413330775 15.310012947711165 0.3450146715188661T15.194747979314306 0.8225409691629955 15.120649071059182 1.1271698142118367 15.063016586860755 1.3247669028921663 15.005384102662324 1.431798659260678 14.914818770350506 1.5058975675158015Q14.807787013981994 1.5799964757709248 14.684288833556788 1.5799964757709248 14.544324229074888 1.5799964757709248 14.470225320819766 1.4976643554874545T14.396126412564643 1.3576997510055544Q14.396126412564643 1.2753676307220838 14.700755257613482 0.040385826470024894T15.30177973568282-2.388411721892358L15.606408580731658-3.5822274660026814Q15.614641792760006-3.689259222371193 15.548776096533231-3.713958858456234T15.227680827427694-3.7551249185979696H15.18651476728596Q15.104182647002489-3.7551249185979696 15.079483010917448-3.7551249185979696T15.02185052671902-3.7798245546830107 14.997150890633977-3.83745703888144Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 62.592367277494006, 118.36430241971944)translate(62.592367277494006, 118.36430241971944)"
    string="m_{ki}"
  >
    <title>\`z\`.labelText</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="18.345844000000003"
      height="11.66"
      role="img"
      focusable="false"
      viewBox="0.24486 -5.15372 17.161899260000002 6.99339982"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M0.24486-3.34642Q0.25652-3.41638 0.27984-3.5329800000000002T0.41976-3.97606 0.65296-4.52408 1.02608-4.9555 1.53912-5.15372 2.0405-5.0721 2.3903-4.86222 2.57686-4.6057 2.67014-4.3841600000000005L2.69346-4.3025400000000005Q2.69346-4.2792200000000005 2.70512-4.2792200000000005L2.83338-4.4074800000000005Q3.5329800000000002-5.15372 4.47744-5.15372 4.67566-5.15372 4.8389-5.1304T5.14206-5.04878 5.3636-4.93218 5.5385-4.79226 5.6551-4.64068 5.74838-4.4891 5.79502-4.3491800000000005 5.83-4.2442400000000005 5.85332-4.16262L5.9466-4.2792200000000005Q6.68118-5.15372 7.68394-5.15372 8.31358-5.15372 8.698360000000001-4.8389T9.0948-3.91776Q9.0948-3.3231 8.651720000000001-2.07548T8.20864-0.583Q8.2203-0.41976 8.26694-0.36146T8.441840000000001-0.30316Q8.768320000000001-0.30316 9.04816-0.65296T9.5029-1.60908Q9.53788-1.73734 9.57286-1.7606600000000001T9.75942-1.7839800000000001Q9.99262-1.7839800000000001 9.99262-1.6907 9.99262-1.67904 9.94598-1.5158 9.8527-1.17766 9.68946-0.85118T9.1531-0.19822 8.34856 0.11660000000000001Q7.80054 0.11660000000000001 7.55568-0.19822T7.3108200000000005-0.85118Q7.3108200000000005-1.0727200000000001 7.73058-2.25038T8.162-4.0227Q8.162-4.71064 7.64896-4.71064H7.59066Q6.5879-4.71064 5.89996-3.5329800000000002L5.81834-3.39306 5.43356-1.8306200000000001Q5.04878-0.30316 4.99048-0.18656 4.8389 0.12826 4.4891 0.12826 4.3375200000000005 0.12826 4.2442400000000005 0.04664T4.11598-0.09328 4.081-0.20988Q4.081-0.33814 4.47744-1.87726L4.8972-3.5796200000000002Q4.93218-3.75452 4.93218-4.0227 4.93218-4.71064 4.4191400000000005-4.71064H4.3608400000000005Q3.35808-4.71064 2.67014-3.5329800000000002L2.58852-3.39306 2.2037400000000003-1.8306200000000001Q1.8189600000000001-0.30316 1.7606600000000001-0.18656 1.60908 0.12826 1.25928 0.12826 1.1077000000000001 0.12826 1.01442 0.058300000000000005T0.8861600000000001-0.08162 0.86284-0.19822Q0.86284-0.3498 1.30592-2.1104600000000002 1.7606600000000001-3.9061 1.7606600000000001-3.98772 1.7956400000000001-4.16262 1.7956400000000001-4.3025400000000005 1.7956400000000001-4.7223 1.50414-4.7223 1.24762-4.7223 1.0727200000000001-4.3958200000000005T0.80454-3.6845600000000003 0.66462-3.2648Q0.6413-3.24148 0.47806-3.24148H0.31482Q0.24486-3.31144 0.24486-3.34642ZM11.61973802-3.5846221399999996Q11.61973802-3.6670583400000005 11.6527125-3.7742254T11.75163594-3.88139246Q11.75987956-3.88139246 12.345176579999999-3.92261056T12.946960840000001-3.9720722800000003Q13.04588428-3.9720722800000003 13.04588428-3.9061233200000003 13.04588428-3.8484179800000002 12.63370328-2.18320674 12.221522279999999-0.55096998 12.221522279999999-0.49326464000000003 12.378151059999999-0.57570084 12.46058726-0.64989342 12.658434139999999-0.79827858 13.02939704-1.1692414800000002T13.60645044-1.6721023000000002Q13.93619524-1.8946800400000001 14.23296556-1.8946800400000001 14.48027416-1.8946800400000001 14.6204157-1.7380512600000002T14.76880086-1.29289578Q14.76880086-1.08680528 14.71109552-0.94666374T14.55446674-0.74057324 14.38135072-0.64989342 14.23296556-0.62516256Q14.068093160000002-0.62516256 13.96916972-0.71584238T13.87024628-0.95490736Q13.87024628-1.16099786 14.002144199999998-1.29289578T14.265940039999998-1.4660118L14.39783796-1.49074266Q14.298914519999999-1.5896661 14.19999108-1.5896661H14.1670166Q13.90322076-1.56493524 13.647668540000002-1.3835756T13.04588428-0.8559839200000001 12.501805359999999-0.3531231Q12.518292599999999-0.34487948 12.625459659999999-0.32839224T12.82330654-0.27893052 13.037640660000001-0.21298156 13.260218400000001-0.09757088000000001 13.441578040000001 0.050814279999999996 13.581719580000001 0.2651484 13.631181300000001 0.53718786Q13.631181300000001 0.6773294000000001 13.5899632 0.8751762800000001T13.54050148 1.2049210799999999Q13.54050148 1.53466588 13.76307922 1.53466588 14.158772980000002 1.53466588 14.406081579999999 0.5536751 14.422568819999999 0.48772614000000003 14.57095398 0.48772614000000003H14.60392846Q14.735826379999999 0.48772614000000003 14.735826379999999 0.56191872 14.735826379999999 0.60313682 14.71109552 0.6773294000000001 14.3731071 1.83967982 13.738348360000002 1.83967982 13.367385460000001 1.83967982 13.1365641 1.6253457000000002T12.905742739999999 1.0070742000000001Q12.905742739999999 0.8586890400000001 12.930473600000001 0.75152198T12.95520446 0.5536751Q12.95520446 0.37231546000000004 12.83979378 0.24041754T12.55126708 0.050814279999999996 12.270984000000002-0.03986554 12.12259884-0.0645964H12.1061116Q12.007188160000002 0.28163564 11.93299558 0.60313682T11.8175849 1.08126678 11.74339232 1.38628072 11.68568698 1.5841276000000002 11.62798164 1.6912946599999998 11.53730182 1.76548724Q11.43013476 1.83967982 11.306480460000001 1.83967982 11.166338920000001 1.83967982 11.09214634 1.75724362T11.017953760000001 1.61710208Q11.017953760000001 1.53466588 11.322967700000001 0.29812288000000003T11.92475196-2.13374502L12.2297659-3.3290699200000002Q12.238009519999999-3.43623698 12.17206056-3.4609678400000004T11.85055938-3.50218594H11.80934128Q11.726905080000002-3.50218594 11.70217422-3.50218594T11.64446888-3.5269168000000004 11.61973802-3.5846221399999996ZM16.4340121-3.197172Q16.4340121-3.3950188800000003 16.59064088-3.54340404T16.953360160000003-3.70003282Q17.10174532-3.70003282 17.20066876-3.6011093800000005T17.30783582-3.35380078Q17.30783582-3.16419752 17.14296342-3.0075687400000004T16.78024414-2.8426963400000003Q16.65658984-2.8426963400000003 16.54942278-2.92513254T16.4340121-3.197172ZM15.09030204-0.61691894Q15.09030204-0.6828679 15.164494620000001-0.8724711599999999T15.362341500000001-1.29289578 15.725060780000002-1.7133204 16.21967798-1.8946800400000001Q16.54117916-1.8946800400000001 16.755513280000002-1.7050767800000002T16.97809102-1.19397234Q16.97809102-1.0538308 16.86268034-0.73232962T16.53293554 0.13325048 16.1867035 1.06477954Q16.145485400000002 1.2461391800000001 16.145485400000002 1.32857538 16.145485400000002 1.53466588 16.28562694 1.53466588 16.35981952 1.53466588 16.442255720000002 1.5099350200000001T16.63185898 1.39452434 16.85443672 1.10599764 17.06052722 0.61962406Q17.08525808 0.52070062 17.10174532 0.50421338T17.24188686 0.48772614000000003Q17.40675926 0.48772614000000003 17.40675926 0.57016234 17.40675926 0.6361113 17.33256668 0.8257145600000001T17.12647618 1.2461391800000001 16.755513280000002 1.6583201800000003 16.24440884 1.83967982Q15.980613 1.83967982 15.75803526 1.6665638T15.5272139 1.13897212Q15.5272139 0.99883058 15.568432000000001 0.8751762800000001T15.922907660000002-0.0645964Q16.23616522-0.89720202 16.28562694-1.06207442T16.34333228-1.3835756Q16.34333228-1.5814224799999999 16.20319074-1.5814224799999999H16.1867035Q15.939394900000002-1.5814224799999999 15.7333044-1.30938302T15.42004684-0.61691894Q15.411803220000001-0.60867532 15.4035596-0.59218808T15.395315980000001-0.56745722 15.378828740000001-0.55096998 15.35409788-0.54272636 15.321123400000001-0.54272636 15.255174440000001-0.54272636H15.13976376Q15.09030204-0.59218808 15.09030204-0.61691894Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
</svg>
`,go=`type Node
type Element

constructor LinearTriangle( Node vi, Node vj, Node vk ) -> Element
constructor LinearQuad( Node vi, Node vj, Node vk, Node vl ) -> Element
constructor QuadraticTriangle( Node vi, Node vj, Node vk, Node mij, Node mjk, Node mki ) -> Element
`,fo=`canvas {
   width = 240
   height = 180
}

Global {
   -- background color
   shape box = Rectangle {
      fillColor: rgba( .9, 1, .9, 1 )
      center: (0,0)
      width: canvas.width
      height: canvas.height
      ensureOnCanvas: false
   }

   scalar dotSize = 2.5
   scalar smallDotSize = 2.
   scalar edgeWidth = 1.5
   string labelSize = "6."
}

Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color lightGray = rgba(0,0,0,.1)
}

forall Node x {
   shape x.icon = Circle {
      center: (?,?)
      r: Global.dotSize
      fillColor: Colors.black
   }

   layer x.icon above Global.box
}

-- put text near a node if it has a label
forall Node x
where x has math label {

   -- direction of label relative to the node location
   -- (as an angle in radians)
   scalar theta = ?

   x.labelText = Equation {
      center: x.icon.center + 10.*unitVector(theta)
      string: x.label
      fontSize: Global.labelSize
      fillColor: Colors.black
   }
}

forall Element e; Node vi; Node vj; Node vk
where e := LinearTriangle( vi, vj, vk ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center

   -- draw edges as straight lines
   shape e.curveIJ = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveJK = Line {
      start: pj
      end: pk
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveKI = Line {
      start: pk
      end: pi
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }

   -- make sure triangles are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any triangle sum to π
   scalar minAngle = toRadians( 30. )
   ensure greaterThan( angleFrom(pj-pi,pk-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
}

forall Element e; Node vi; Node vj; Node vk; Node vl
where e := LinearQuad( vi, vj, vk, vl ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center
   vec2 pl = vl.icon.center

   -- draw edges as straight lines
   shape e.curveIJ = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveJK = Line {
      start: pj
      end: pk
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveKL = Line {
      start: pk
      end: pl
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveLI = Line {
      start: pl
      end: pi
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }

   -- make sure quads are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any quad sum to 2π
   scalar minAngle = toRadians( 30. )
   ensure greaterThan( angleFrom(pj-pi,pl-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
   ensure greaterThan( angleFrom(pl-pk,pj-pk), minAngle )
}

forall Element e; Node vi; Node vj; Node vk; Node mij; Node mjk; Node mki
where e := QuadraticTriangle( vi, vj, vk, mij, mjk, mki ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center
   vec2 pij = mij.icon.center
   vec2 pjk = mjk.icon.center
   vec2 pki = mki.icon.center

   -- offset the middle nodes in some random direction from the
   -- geometric midpoints between the triangle vertices
   scalar offsetSize = 20.
   scalar thetaIJ = ?
   scalar thetaJK = ?
   scalar thetaKI = ?
   override mij.icon.center = (pi+pj)/2. + offsetSize * unitVector(thetaIJ)
   override mjk.icon.center = (pj+pk)/2. + offsetSize * unitVector(thetaJK)
   override mki.icon.center = (pk+pi)/2. + offsetSize * unitVector(thetaKI)

   -- offset the midpoint labels so they don't cross the edges
   -- (since we don't yet support disjoint constraints for Bézier
   -- curves, just push the vertex away from both segments to the midpoint)
   vec2 cij = mij.icon.center
   vec2 nij = unit(unit(cij-pi) + unit(cij-pj))
   override mij.labelText.center = cij + 12.*nij

   vec2 cjk = mjk.icon.center
   vec2 njk = unit(unit(cjk-pj) + unit(cjk-pk))
   override mjk.labelText.center = cjk + 12.*njk

   vec2 cki = mki.icon.center
   vec2 nki = unit(unit(cki-pk) + unit(cki-pi))
   override mki.labelText.center = cki + 12.*nki

   -- similarly, offset the vertex labels so they don't cross the edges
   vec2 ni = unit(unit(pi-pij) + unit(pi-pki))
   override vi.labelText.center = pi + 10.*ni

   vec2 nj = unit(unit(pj-pjk) + unit(pj-pij))
   override vj.labelText.center = pj + 10.*nj

   vec2 nk = unit(unit(pk-pki) + unit(pk-pjk))
   override vk.labelText.center = pk + 10.*nk

   -- draw the midpoints in a different style
   override mij.icon.r = Global.smallDotSize
   override mij.icon.fillColor = Colors.white
   override mij.icon.strokeColor = Colors.black
   override mij.icon.strokeWidth = 1.

   override mjk.icon.r = Global.smallDotSize
   override mjk.icon.fillColor = Colors.white
   override mjk.icon.strokeColor = Colors.black
   override mjk.icon.strokeWidth = 1.

   override mki.icon.r = Global.smallDotSize
   override mki.icon.fillColor = Colors.white
   override mki.icon.strokeColor = Colors.black
   override mki.icon.strokeWidth = 1.

   -- draw edges as quadratic Bézier curves
   shape e.curveIJ = Path {
      d: interpolateQuadraticFromPoints("open", pi, pij, pj)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }
   shape e.curveJK = Path {
      d: interpolateQuadraticFromPoints("open", pj, pjk, pk)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }
   shape e.curveKI = Path {
      d: interpolateQuadraticFromPoints("open", pk, pki, pi)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }

   layer e.curveIJ above Global.box
   layer e.curveJK above Global.box
   layer e.curveKI above Global.box

   -- make sure triangles are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any triangle sum to π
   scalar minAngle = toRadians( 50. )
   ensure greaterThan( angleFrom(pj-pi,pk-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
}

`,uo={"example.substance":co,"example.svg":po,"lagrange-bases.domain":go,"lagrange-bases.style":fo},ho=`VectorSpace V
Vector u 
Vector v 
In(u, V)
In(v, V)
Dependent(u, v)
AutoLabel All
`,bo=`canvas {
  width = 800
  height = 700
}

const { -- 0
  scalar perpLen = 20.0
  -- For unit mark
  scalar markerPadding = 15.0
  scalar barSize = 5.0
  scalar vectorSpaceSize = 350.0
  scalar repelWeight = 0.7
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  int intForTesting = 1
  bool boolForTesting = true
}

C { -- 1
    -- black = #000000
    color black = rgba(0.,0.,0.,1.)
    white = rgba(1., 1., 1., 1.)
    lightBlue = rgba(1e-1, 0.1, 0.9, 1.0)
    darkBlue = rgba(lightBlue.r / 2., lightBlue.g / 2., lightBlue.b / 2., 0.5)
    darkGray = rgba(0.4, 0.4, 0.4, 1.)
    gray = rgba(0.6, 0.6, 0.6, 1.)
    green = rgba(0., 0.8, 0., 1.)
    -- blue = #0000ff
    none = none()
}

testing { -- 2

} 

-------- broken selectors -- should throw errors

forall Thing U {
}

-- TODO(error): fails but the error message is hard to interpret due to disambiguation, and throws fatal error rather than adding to list

-- forall VectorSpace U; Vector u
-- where u := Nonexistent(U) {
-- }

forall VectorSpace U; Vector U {
}

forall VectorSpace \`x1\` {
}

forall Vector v; VectorSpace U
where Orthogonal(v, U) {
}

forall Vector v; Vector w
where Orthogonal(v, e) {
}

forall Scalar c
with Vector v; Vector w; Vector u
where u := scale(v, w) {
}

forall Scalar c
with Scalar d; Vector w
where c := scale(w, d) {
}

--------- working selectors w/ tests for translation

forall VectorSpace U { -- 3

       U.f = ?

       shape U.xAxis = Circle {
             center: (0., 0.)
       }

      -- TODO(error): if there are multiple matches, override errors behave oddly
      U.xAxis.fillColor = C.black
      -- U.xAxis.fillColor = C.green -- test override -- throws error if uncommented

      delete U.xAxis.fillColor

      delete U.xAxis.stroke -- TODO(err): this should throw an error, but doesn't
}

forall Vector u; VectorSpace U -- 4
where In(u,U) {

      override U.f = 1. -- test field overrides

      -- TODO(error): if there are multiple matches, (non) override errors behave oddly
      -- U.xAxis.fillColor = C.black

       shape u.arrow = Line {
       }

}

forall Vector u; Vector v -- 5
with VectorSpace U
where Orthogonal(u, v); In(u, U); In(v, U) {

}

forall Vector v -- 6
with VectorSpace U; Vector w
where In(v, U); Unit(v); Orthogonal(v, w) {

}

forall Vector \`x2\` { -- 7

}`,ko=`VectorSpace V
Vector u
Vector v
In(u, V)
In(v, V)
Independent(u, v)
AutoLabel All
`,xo=`canvas {
  width = 400
  height = 500
}

const {
  scalar perpLen = 20.0
  -- For unit mark
  scalar markerPadding = 15.0
  scalar barSize = 5.0
  scalar vectorSpaceSize = 350.0
  scalar repelWeight = 0.7
  scalar arrowheadSize = 0.7
  scalar lineThickness = 2.
  string fontFamily = "Palatino"
  string fontSize = "22.5px"
}

C {
  color black = rgba(0.,0.,0.,1.)
  color white = rgba(1., 1., 1., 1.)
  color lightBlue = rgba(1e-1, 0.1, 0.9, 1.0)
  -- Note: we don't currently support color accessors r,g,b
  -- darkBlue = rgba(lightBlue.r / 2., lightBlue.g / 2., lightBlue.b / 2., 0.5)
  color darkGray = rgba(0.4, 0.4, 0.4, 1.)
  color gray = rgba(0.6, 0.6, 0.6, 1.)
  color green = rgba(0., 0.8, 0., 1.)
  color none = none()
}

forall VectorSpace U {
  scalar axisSize = const.vectorSpaceSize / 2.0 -- This should get promoted to float
  vec2 U.origin = (0., 0.)
  vec2 o = U.origin
  color U.axisColor = C.gray

  shape U.background = Rectangle {
    center : U.origin
    width : const.vectorSpaceSize
    height : const.vectorSpaceSize
    fillColor : rgba(.95,.95,.95,1.)
    strokeColor : C.none
  }

  shape U.xAxis = Line {
    start : (o[0] - 1.1*axisSize, o[1]) -- TODO This gets mis-parsed as a matrix access
    end : (o[0] + 1.1*axisSize, o[1])
    strokeWidth : const.lineThickness
    style : "solid"
    strokeColor : U.axisColor

  }

  shape U.yAxis = Line {
    start : (o[0], o[1] - 1.1*axisSize)
    end : (o[0], o[1] + 1.1*axisSize)
    strokeWidth : const.lineThickness
    style : "solid"
    strokeColor : U.axisColor
  }

  shape U.text = Text {
    fontSize : const.fontSize
    fontFamily : const.fontFamily
    fontStyle : "italic"
    string : U.label
    center : (U.origin[0] + .8*axisSize, U.origin[1] + .8*axisSize)
    fillColor : U.axisColor
  }

  layer U.xAxis above U.yAxis
  layer U.background below U.yAxis
  layer U.text above U.background
  layer U.text below U.yAxis
}

forall Vector u; VectorSpace U
where In(u,U) {

  shape u.arrow = Line {
    start : U.origin
    end : (?, ?)
    strokeWidth : 3.0
    strokeColor : C.lightBlue
    endArrowhead: "straight"
    endArrowheadSize : const.arrowheadSize
  }

  shape u.text = Text {
    center: u.arrow.end * 1.15
    fontSize: const.fontSize
    fontFamily: const.fontFamily
    fontWeight: "bold"
    string: u.label
    fillColor: u.arrow.strokeColor
  }

  shape u.stroke = Text {
    center: u.text.center
    fontSize: u.text.fontSize
    fontFamily: u.text.fontFamily
    fontWeight: "bold"
    string: u.label
    fillColor: C.white
    strokeColor: C.white
    strokeWidth: 8.
  }

  vec2 u.vector = u.arrow.end - u.arrow.start -- Vector sugar for subtraction

  ensure contains(U.background, u.arrow)
  ensure contains(U.background, u.text)
  ensure minSize(u.arrow)

  layer u.text above u.stroke
  layer u.stroke above u.arrow
  layer u.arrow above U.xAxis
  layer u.text above U.xAxis
  layer u.text above U.yAxis
  layer u.text above U.xAxis
}

-- draw all labels above all arrows, to improve legibility
forall Vector u; Vector v {
  layer u.stroke above v.arrow
  layer v.stroke above u.arrow
}

forall Vector u; Vector v
with VectorSpace U
where Orthogonal(u, v); In(u, U); In(v, U) {
  startR = u.arrow.start
  endR = u.arrow.end
  startL = v.arrow.start
  endL = v.arrow.end
  dirR = normalize(endR - startR)  
  dirL = normalize(endL - startL)
  ptL = startR + const.perpLen * dirL
  ptR = startR + const.perpLen * dirR
  ptLR = ptL + const.perpLen * dirR
  pts = [startR, ptL, ptLR, ptR]

  -- Draw perpendicular mark -- NOTE: local shapes should still be drawn
  perpMark = Path {
    d : pathFromPoints("closed", pts)
    strokeWidth : 2.0
    strokeColor : C.black
    fillColor : C.white
  }

  ensure equal(dot(u.vector, v.vector), 0.0) 

  layer v.arrow above perpMark
  layer u.arrow above perpMark
  layer perpMark above U.xAxis

  shape labelText = Text {
    string: "orthogonal"
    center: (0.,-.6*U.background.width)
    fontFamily: const.fontFamily
    fontSize: "18px"
    fontWeight: "bold"
    fillColor: C.black
  }
}

forall Vector v
with VectorSpace U; Vector w
where In(v, U); Unit(v); Orthogonal(v, w) {
  -- Usually, the unit vector shouldn't need to know about orthogonal vectors
  -- but we need to position the unit mark so it doesn't overlap with the "inside" of the two vectors

  strokeWidth = 2.0
  padding = 15.0 

  -- The start and end of the body of the unit marker line
  -- NOTE: We need to have lists of vectors
  dir = normalize(w.arrow.end - w.arrow.start)
  normal = -dir
  markStart = v.arrow.start + padding * normal
  markEnd = v.arrow.end + padding * normal
  v.markerLine = [markStart, markEnd]

  v.unitMarkerLine = Path {
    d : pathFromPoints("open", v.markerLine)
    strokeColor : C.black
    fillColor : C.none
  }

  -- Could use normal instead, just doing this to demonstrate how to use matrices
  mat2x2 rot90CW = ((0., 1.), (-1., 0.))
  vec2 markNormal = mul(rot90CW, normalize(v.arrow.end - v.arrow.start)) 
  scalar c = const.barSize
  vec2 halfvec = c * markNormal

  v.unitMarkerEnd1 = Path {
    d : pathFromPoints("open", [markStart - halfvec, markStart + halfvec]) 
    strokeColor : C.black
    fillColor : C.none
  }

  v.unitMarkerEnd2 = Path {
    d : pathFromPoints("open", [markEnd - halfvec, markEnd + halfvec])
    strokeColor : C.black
    fillColor : C.none
  }

  vec2 midpointLoc = (v.markerLine[0] + v.markerLine[1]) / 2.
  vec2 labelPos = midpointLoc + const.markerPadding * normal

  v.unitMarkerText = Equation {
    fontSize : const.fontSize
    string : "1"
    center : labelPos
    fillColor : C.black
  }

  layer v.unitMarkerLine above U.xAxis
  layer v.unitMarkerLine above U.yAxis
}

-- If two vectors are linearly dependent, make
-- sure they are parallel by minimizing the
-- signed are of the parallelogram with sides u,v
forall Vector u; Vector v
where Dependent(u,v) {
  vec2 x = u.arrow.end
  vec2 y = v.arrow.end
  scalar A = cross2D(x,y)
  ensure equal( A, 0. )

  shape labelText = Text {
    string: "linearly dependent"
    center: (0.,-.6*V.background.width)
    fontFamily: const.fontFamily
    fontSize: "18px"
    fontWeight: "bold"
    fillColor: C.black
  }
}

-- If two vectors are linearly independent, make
-- sure they're not parallel
forall Vector u; Vector v; VectorSpace V
where Independent(u, v); In(v,V) {
  vec2 x = u.arrow.end
  vec2 y = v.arrow.end
  ensure inRange( angleBetween(x,y), .25*MathPI(), .75*MathPI() )

  shape labelText = Text {
    string: "linearly independent"
    center: (0.,-.6*V.background.width)
    fontFamily: const.fontFamily
    fontSize: "18px"
    fontWeight: "bold"
    fillColor: C.black
  }
}

forall Vector \`u\` {
  override \`u\`.arrow.strokeColor = C.green
}

forall Vector \`x2\` {
  override \`x2\`.arrow.strokeColor = C.green
}



`,vo=`-- Types
type Scalar
type VectorSpace
type Vector
type LinearMap

-- Operators
function neg(Vector v) -> Vector
function scale(Scalar c, Vector v) -> Vector cv
function addV(Vector, Vector) -> Vector
function addS(Scalar s1, Scalar s2) -> Scalar
function norm(Vector v) -> Scalar
function innerProduct(Vector, Vector) -> Scalar
function determinant(Vector, Vector) -> Scalar
function apply(LinearMap f, Vector) -> Vector

-- Predicates
predicate In(Vector, VectorSpace V)
predicate From(LinearMap V, VectorSpace domain, VectorSpace codomain)
predicate Not(Prop p1)
predicate Orthogonal(Vector v1, Vector v2)
predicate Unit(Vector v)

-- Syntactic sugar
notation "det(v1, v2)" ~ "determinant(v1, v2)"
notation "LinearMap f : U → V" ~ "LinearMap f; From(f, U, V)"
notation "v1 + v2" ~ "addV(v1, v2)"
notation "-v1" ~ "neg(v1)"
notation "Vector a ∈ U" ~ "Vector a; In(a, U)"
notation "|y1|" ~ "norm(y1)"
notation "<v1,v2>" ~ "innerProduct(v1, v2)"
notation "s * v1" ~ "scale(s, v1)"
notation "Scalar c := " ~ "Scalar c; c := "
notation "f(v)" ~ "apply(f, v)"

-- Examples for prelude, just for reproducing (Should be removed)

--value T : VectorSpace
-- value T1 : VectorSpace
`,mo=`-- Types
type Scalar
type VectorSpace
type Vector
type LinearMap

-- Operators
function neg(Vector v) -> Vector
function scale(Scalar c, Vector v) -> Vector cv
function addV(Vector, Vector) -> Vector
function addS(Scalar s1, Scalar s2) -> Scalar
function norm(Vector v) -> Scalar
function innerProduct(Vector, Vector) -> Scalar
function determinant(Vector, Vector) -> Scalar
function apply(LinearMap f, Vector) -> Vector

-- Predicates
predicate In(Vector, VectorSpace V)
predicate From(LinearMap V, VectorSpace domain, VectorSpace codomain)
predicate Not(Prop p1)
predicate Orthogonal(Vector v1, Vector v2)
predicate Independent(Vector v1, Vector v2)
predicate Dependent(Vector v1, Vector v2)
predicate Unit(Vector v)

-- Syntactic sugar
notation "det(v1, v2)" ~ "determinant(v1, v2)"
notation "LinearMap f : U → V" ~ "LinearMap f; From(f, U, V)"
notation "v1 + v2" ~ "addV(v1, v2)"
notation "-v1" ~ "neg(v1)"
notation "Vector a ∈ U" ~ "Vector a; In(a, U)"
notation "|y1|" ~ "norm(y1)"
notation "<v1,v2>" ~ "innerProduct(v1, v2)"
notation "s * v1" ~ "scale(s, v1)"
notation "Scalar c := " ~ "Scalar c; c := "
notation "f(v)" ~ "apply(f, v)"

-- Examples for prelude, just for reproducing (Should be removed)

--value T : VectorSpace
-- value T1 : VectorSpace
`,yo=`canvas {
  width = 800
  height = 700
}

const { -- 0
  scalar perpLen = 20.0
  -- For unit mark
  scalar markerPadding = 15.0
  scalar barSize = 5.0
  scalar vectorSpaceSize = 350.0
  scalar repelWeight = 0.7
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  int intForTesting = 1
  bool boolForTesting = true
}

C { -- 1
    -- black = #000000
    color black = rgba(0.,0.,0.,1.)
    white = rgba(1., 1., 1., 1.)
    lightBlue = rgba(1e-1, 0.1, 0.9, 1.0)
    -- Note: we don't currently support color accessors r,g,b
    -- darkBlue = rgba(lightBlue.r / 2., lightBlue.g / 2., lightBlue.b / 2., 0.5)
    darkGray = rgba(0.4, 0.4, 0.4, 1.)
    gray = rgba(0.6, 0.6, 0.6, 1.)
    green = rgba(0., 0.8, 0., 1.)
    -- blue = #0000ff
    none = none()
}

-- Just some weird definitions to test parser. Not used in rest of program
testing { -- 2
  -- COMBAK: Test that plugins still run
  -- pluginVar = "ddg"["a"]["length"]
        x = { 1, 2 }
        y = [-2., const.perpLen, const.markerPadding + 3]
        a = (-1.0, ?)
        a1 = (-1.0, 2.) + (1e5, 2.0)
        m = (a, (-1., 2.))
        v = (a + (2., 900.))  / (4.0 + 3.)
        -- z = Colors.black.g
        asum = a[1] + a[0]
        -- Currently not supported: indexing a vector or list by a variable
        -- c = 0
        -- b = 1
        -- msum = m[1][0] + m[c][b]
        nv = -v
        -- t1 = x[1][b]

        -- test parser access of matrix
        -- test0 = f(0)[1]
        -- test1 = makeMatrix((1, 0.0, 5.0), (-1, -9., -4.))[2][0]
        -- test2 = (makeVector(-1, 3.0) + (3.4, 2.1))[0]
}

forall VectorSpace U { -- 3
    scalar axisSize = const.vectorSpaceSize / 2.0 -- This should get promoted to float
    vec2 U.origin = (0., 0.)
    vec2 o = U.origin
    U.axisColor = C.gray

    shape U.background = Rectangle {
        center : U.origin
        width : const.vectorSpaceSize
        height : const.vectorSpaceSize
        fillColor : C.none
        strokeColor : C.none
        -- strokeWidth : 2.0
    }

    shape U.xAxis = Line {
        start : (o[0] - axisSize, o[1]) -- TODO This gets mis-parsed as a matrix access
        end : (o[0] + axisSize, o[1])
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.yAxis = Line {
        start : (o[0], o[1] - axisSize)
        end : (o[0], o[1] + axisSize)
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.text = Equation {
        string : U.label
        center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
        fillColor : U.axisColor
    }
}

forall Vector u; VectorSpace U -- 4
where In(u,U) {
  u.text = Equation {
    -- center : (?, ?) -- This should be done automatically
    string : u.label
    fillColor : u.arrow.strokeColor
  }

  u.arrow = Line {
    start : U.origin
    end : (?, ?)
    strokeWidth : 3.0
    strokeColor : C.lightBlue
    endArrowhead: "straight"
    endArrowheadSize : const.arrowheadSize
    strokeDasharray : "4 1 2"
  }

   u.vector = u.arrow.end - u.arrow.start -- Vector sugar for subtraction

   ensure contains(U.background, u.arrow)
   ensure contains(U.background, u.text)
   ensure atDist(u.arrow, u.text, 15.0)
   ensure lessThan(20, length(u.arrow))

  layer u.text above U.xAxis
  layer u.text above U.yAxis
}

forall Vector u; Vector v -- 5
with VectorSpace U
where Orthogonal(u, v); In(u, U); In(v, U) {
      startR = u.arrow.start -- TODO: Do we want destructuring syntax like vec2[] [startR, endR] = [u.arrow.start, u.arrow.end]
      endR = u.arrow.end
      startL = v.arrow.start
      endL = v.arrow.end
      dirR = normalize(endR - startR)  -- Syntax sugar for vectors (better in Style because JS doesn't allow it!)
      dirL = normalize(endL - startL)
      ptL = startR + const.perpLen * dirL
      ptR = startR + const.perpLen * dirR
      ptLR = ptL + const.perpLen * dirR
      pts = [startR, ptL, ptLR, ptR]

      -- toPath = functions.pathFromPoints -- COMBAK: Add ability to alias function names

      -- Draw perpendicular mark -- NOTE: local shapes should still be drawn
      perpMark = Path {
           d : pathFromPoints("closed", pts)
           -- strokeWidth : 2.0
           strokeColor : C.black
           fillColor : C.white
      }

      -- Make sure vectors are orthogonal (use ensure?)
      -- eq = functions.equal
      encourage equal(dot(u.vector, v.vector), 0.0) -- NOTE: Have to import Penrose fns

-- COMBAK: Test parsing the expressions that involve local vars 
      layer v.arrow above perpMark
      layer u.arrow above perpMark
}

forall Vector v -- 6
with VectorSpace U; Vector w
where In(v, U); Unit(v); Orthogonal(v, w) {
      -- Usually, the unit vector shouldn't need to know about orthogonal vectors
      -- but we need to position the unit mark so it doesn't overlap with the "inside" of the two vectors

      strokeWidth = 2.0
      padding = 15.0 -- COMBAK: What is this?
      -- toPath = functions.pathFromPoints -- COMBAK

      -- The start and end of the body of the unit marker line
      -- NOTE: We need to have lists of vectors
      dir = normalize(w.arrow.end - w.arrow.start)
      normal = -dir
      markStart = v.arrow.start + padding * normal
      markEnd = v.arrow.end + padding * normal
      v.markerLine = [markStart, markEnd]

      v.unitMarkerLine = Path {
          d : pathFromPoints("open", v.markerLine)
          -- strokeWidth : strokeWidth
          strokeColor : C.black
          fillColor : C.none
      }

      -- Could use normal instead, just doing this to demonstrate how to use matrices
      mat2x2 rot90CW = ((0., 1.), (-1., 0.))
      vec2 markNormal = mul(rot90CW, normalize(v.arrow.end - v.arrow.start)) -- TODO: Do we want syntactic sugar for matrix-vector multiplication? Or a better name?
      scalar c = const.barSize
      vec2 halfvec = c * markNormal

      v.unitMarkerEnd1 = Path {
          d : pathFromPoints("open", [markStart - halfvec, markStart + halfvec]) -- TODO: Can we infer this type if it's written anonymously?
          -- strokeWidth : strokeWidth
          strokeColor : C.black
          fillColor : C.none
      }

      v.unitMarkerEnd2 = Path {
          d : pathFromPoints("open", [markEnd - halfvec, markEnd + halfvec])
          -- strokeWidth : strokeWidth
          strokeColor : C.black
          fillColor : C.none
      }

      vec2 midpointLoc = (v.markerLine[0] + v.markerLine[1]) / 2.
      vec2 labelPos = midpointLoc + const.markerPadding * normal

      v.unitMarkerText = Equation {
          string : "1"
          center : labelPos
          fillColor : C.black
      }

      layer v.unitMarkerLine above U.xAxis
      layer v.unitMarkerLine above U.yAxis
}

forall Vector \`x2\` { -- 7
       override \`x2\`.arrow.strokeColor = C.green
}`,wo=`VectorSpace V
Vector u 
In(u, V)
Vector v 
In(v, V)
Unit(u)
Orthogonal(v, u)
AutoLabel All
`,Co=`VectorSpace X
Vector x1 ∈ X
Vector x2 ∈ X
Unit(x1)
Orthogonal(x1, x2)
AutoLabel All
`,Lo={"dependent.substance":ho,"error-test.style":bo,"independent.substance":ko,"linear-algebra-paper-simple.style":xo,"linear-algebra-test.domain":vo,"linear-algebra.domain":mo,"linearalgebra-paper-dashes.style":yo,"twoVectorsPerp-unsugared.substance":wo,"twoVectorsPerp.substance":Co},Ao=`canvas {
    width = 800
    height = 700
}

background {
    shape icon = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: rgba(1.0, 1.0, 1.0, 1.0)
    }
}

-- Nodes
forall Node x {
    vec2 x.center = (?, ?)
}

forall InputNode x {
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 1.0
        r : 6.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }

    shape x.symbol = Equation {
       string : x.label
       rotation : 0.0
       center : (x.icon.center[0] - 30.0, x.icon.center[1])
    }

    x.icon above background.icon
    x.symbol above background.icon
}

forall OutputNode x {
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 1.0
        r : 6.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape x.symbol = Equation {
       string : x.label
       rotation : 0.0
       center : (x.icon.center[0] + 35.0, x.icon.center[1])
    }

    x.icon above background.icon
    x.symbol above background.icon
}

forall InputNode A; InputNode B {
    ensure equal(A.center[0], B.center[0])
}

forall OutputNode A; OutputNode B {
    ensure equal(A.center[0], B.center[0])
}

-- Gates

forall XORGate G
where G := MakeXORGate(IN1, IN2, OUT)
with Node IN1; Node IN2; Node OUT {
    vec2 G.center = (?, ?)
    scalar G.width = 100.0
    scalar G.height = 60.0

    shape G.part1 = Rectangle {
        center : G.center
        width : 40.0
        height : 50.0
        strokeWidth: 1.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part2 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 25.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part3 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 24.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
    }

    scalar G.GG = 50.0
    shape G.part4 = Circle {
        center : (G.part1.center[0] - G.GG + 3.0, G.part1.center[1])
        r : sqrt(25.0 * 25.0 + (G.GG - 20.0) * (G.GG  - 20.0))
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part5 = Rectangle {
        center : (G.part1.center[0] - G.GG - 30.0+ 3.0, G.part1.center[1])
        width : 2 * (G.GG - 20.0) + 60.0
        height : 3 * (G.GG - 20.0)
        strokeWidth: 0.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part5 above G.part4

    shape G.part6 = Circle {
        center : (G.part1.center[0] - G.GG  - 6.0, G.part1.center[1])
        r : sqrt(25.0 * 25.0 + (G.GG - 20.0) * (G.GG  - 20.0))
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part7 = Rectangle {
        center : (G.part1.center[0] - G.GG - 30.0 - 6.0, G.part1.center[1])
        width : 2 * (G.GG - 20.0) + 60.0
        height : 3 * (G.GG - 20.0)
        strokeWidth: 0.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part4 above G.part3
    G.part6 above G.part5
    G.part7 above G.part6

    G.part1 above G.part2
    G.part3 above G.part1

    G.part1 above background.icon
    G.part2 above background.icon
    G.part3 above background.icon
    G.part4 above background.icon
    G.part5 above background.icon
    G.part6 above background.icon
    G.part7 above background.icon

    ensure equal(vdist(IN1.center, (G.part1.center[0] - 20.0, G.part1.center[1] + 17.0)), 0.0)
    ensure equal(vdist(IN2.center, (G.part1.center[0] - 20.0, G.part1.center[1] - 17.0)), 0.0)
    ensure equal(vdist(OUT.center, (G.part1.center[0] + 45.0, G.part1.center[1])), 0.0)
}

forall ANDGate G
where G := MakeANDGate(IN1, IN2, OUT)
with Node IN1; Node IN2; Node OUT {
    vec2 G.center = (?, ?)
    scalar G.width = 100.0
    scalar G.height = 60.0

    shape G.part1 = Rectangle {
        center : G.center
        width : 40.0
        height : 50.0
        strokeWidth: 1.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part2 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 25.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part3 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 24.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part1 above G.part2
    G.part3 above G.part1

    G.part1 above background.icon
    G.part2 above background.icon
    G.part3 above background.icon

    ensure equal(vdist(IN1.center, (G.part1.center[0] - 20.0, G.part1.center[1] + 17.0)), 0.0)
    ensure equal(vdist(IN2.center, (G.part1.center[0] - 20.0, G.part1.center[1] - 17.0)), 0.0)
    ensure equal(vdist(OUT.center, (G.part1.center[0] + 45.0, G.part1.center[1])), 0.0)
}

forall SplitComponent x
where x := MakeSplitComponent(IN, OUT1, OUT2)
with Node IN; Node OUT1; Node OUT2 {
    vec2 x.center = IN.center
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 0.0
        r : 6.0
        fillColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape x.line = Line {
        start : OUT1.center
        end : OUT2.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    x.icon above x.line

    x.icon above background.icon
    x.line above background.icon

    ensure equal(IN.center[0], OUT1.center[0])
    ensure equal(IN.center[0], OUT2.center[0])
    ensure lessThan(0.0, -(OUT1.center[1] - IN.center[1]) * (-IN.center[1] + OUT2.center[1]))
}

forall Gate G; Gate H {
    ensure lessThan(0.6 * (G.height + H.height), abs(G.part1.center[1] - H.part1.center[1]))
    ensure lessThan(0.6 * (G.width + H.width), abs(G.part1.center[0] - H.part1.center[0]))
}

forall Connection x; InputNode y {
    x.line1 below y.icon
    x.line2 below y.icon
    x.line3 below y.icon
}

forall SplitComponent x; XORGate y {
    x.line above y.part7
}

forall Connection x; XORGate y {
    x.line1 above y.part7
    x.line2 above y.part7
    x.line3 above y.part7
}

-- Connections

forall Connection C
where C := MakeConnection(A, B)
with Node A; Node B {
    scalar C.pivot = ?
    shape C.line1 = Line {
        start : A.center
        end : (C.pivot, A.center[1])
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    shape C.line2 = Line {
        start : (C.pivot, A.center[1])
        end : (C.pivot, B.center[1])
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    shape C.line3 = Line {
        start : (C.pivot, B.center[1])
        end : B.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }

    C.line1 above background.icon
    C.line2 above background.icon
    C.line3 above background.icon

    encourage equal(A.center[1], B.center[1])
    ensure lessThan(A.center[0], B.center[0])
    ensure lessThan(A.center[0], C.pivot)
    ensure lessThan(C.pivot, B.center[0])
}

forall SplitComponent x; SplitComponent y {
    ensure lessThan(10.0, abs(x.center[0] - y.center[0]))
}`,So=`AutoLabel All

InputNode IN1, IN2, INCARRY
OutputNode SUM, CARRY

Node XOR1IN1, XOR1IN2, XOR1OUT
XORGate XOR1 := MakeXORGate(XOR1IN1, XOR1IN2, XOR1OUT)

Node XOR2IN1, XOR2IN2, XOR2OUT
XORGate XOR2 := MakeXORGate(XOR2IN1, XOR2IN2, XOR2OUT)

Node XOR3IN1, XOR3IN2, XOR3OUT
XORGate XOR3 := MakeXORGate(XOR3IN1, XOR3IN2, XOR3OUT)

Node AND1IN1, AND1IN2, AND1OUT
ANDGate AND1 := MakeANDGate(AND1IN1, AND1IN2, AND1OUT)

Node AND2IN1, AND2IN2, AND2OUT
ANDGate AND2 := MakeANDGate(AND2IN1, AND2IN2, AND2OUT)

Node S1IN, S1OUT1, S1OUT2
SplitComponent S1 := MakeSplitComponent(S1IN, S1OUT1, S1OUT2)

Node S2IN, S2OUT1, S2OUT2
SplitComponent S2 := MakeSplitComponent(S2IN, S2OUT1, S2OUT2)

Node S3IN, S3OUT1, S3OUT2
SplitComponent S3 := MakeSplitComponent(S3IN, S3OUT1, S3OUT2)

Node S4IN, S4OUT1, S4OUT2
SplitComponent S4 := MakeSplitComponent(S4IN, S4OUT1, S4OUT2)

Connection C1 := MakeConnection(IN1, S1IN)
Connection C2 := MakeConnection(IN2, S2IN)
Connection C3 := MakeConnection(INCARRY, S3IN)
Connection C4 := MakeConnection(S1OUT1, XOR1IN1)
Connection C5 := MakeConnection(S1OUT2, AND1IN1)
Connection C6 := MakeConnection(S2OUT1, XOR1IN2)
Connection C7 := MakeConnection(S2OUT2, AND1IN2)
Connection C8 := MakeConnection(S3OUT1, XOR2IN2)
Connection C9 := MakeConnection(S3OUT2, AND2IN2)
Connection C10 := MakeConnection(XOR1OUT, S4IN)
Connection C11 := MakeConnection(S4OUT1, XOR2IN1)
Connection C12 := MakeConnection(S4OUT2, AND2IN1)
Connection C13 := MakeConnection(AND1OUT, XOR3IN2)
Connection C14 := MakeConnection(AND2OUT, XOR3IN1)
Connection C15 := MakeConnection(XOR2OUT, SUM)
Connection C16 := MakeConnection(XOR3OUT, CARRY)`,Po=`AutoLabel All

InputNode IN1, IN2
OutputNode SUM, CAR

Node XORIN1, XORIN2, XOROUT
XORGate XOR := MakeXORGate(XORIN1, XORIN2, XOROUT)

Node ANDIN1, ANDIN2, ANDOUT 
ANDGate AND := MakeANDGate(ANDIN1, ANDIN2, ANDOUT)

Node S1IN, S1OUT1, S1OUT2
SplitComponent S1 := MakeSplitComponent(S1IN, S1OUT1, S1OUT2)

Node S2IN, S2OUT1, S2OUT2
SplitComponent S2 := MakeSplitComponent(S2IN, S2OUT1, S2OUT2)

Connection C1 := MakeConnection(IN1, S1IN)
Connection C2 := MakeConnection(IN2, S2IN)
Connection C3 := MakeConnection(S1OUT1, XORIN1)
Connection C4 := MakeConnection(S2OUT1, XORIN2)
Connection C5 := MakeConnection(S1OUT2, ANDIN1)
Connection C6 := MakeConnection(S2OUT2, ANDIN2)
Connection C7 := MakeConnection(XOROUT, SUM)
Connection C8 := MakeConnection(ANDOUT, CAR)`,Eo=`-- Nodes

type Node

type InputNode <: Node
type OutputNode <: Node

-- Gates

type Component

type SplitComponent <: Component
type Gate <: Component

type OneInputGate <: Gate
type TwoInputGate <: Gate

constructor MakeSplitComponent(Node IN, Node OUT1, Node OUT2) -> SplitComponent

type Buffer <: OneInputGate
type NOTGate <: OneInputGate

constructor MakeBuffer(Node IN, Node OUT) -> Buffer
constructor MakeNOTGate(Node IN, Node OUT) -> NOTGate

type ORGate <: TwoInputGate
type NORGate <: TwoInputGate
type ANDGate <: TwoInputGate
type NANDGate <: TwoInputGate
type XORGate <: TwoInputGate
type XNORGate <: TwoInputGate

constructor MakeORGate(Node IN1, Node IN2, Node OUT) -> ORGate
constructor MakeNORGate(Node IN1, Node IN2, Node OUT) -> NORGate
constructor MakeANDGate(Node IN1, Node IN2, Node OUT) -> ANDGate
constructor MakeNANDGate(Node IN1, Node IN2, Node OUT) -> NANDGate
constructor MakeXORGate(Node IN1, Node IN2, Node OUT) -> XORGate
constructor MakeXNORGate(Node IN1, Node IN2, Node OUT) -> XNORGate

-- Connections

type Connection

constructor MakeConnection(Node A, Node B) -> Connection`,To={"distinctive-shape.style":Ao,"full-adder.substance":So,"half-adder.substance":Po,"logic-gates.domain":Eo},Oo=`type Scalar
type Vector
type Matrix

function transpose( Matrix A ) -> Matrix
function smmul( Scalar c, Matrix A ) -> Matrix
function smmulr( Matrix A, Scalar c ) -> Matrix
function msdiv( Matrix A, Scalar c ) -> Matrix
function mvmul( Matrix A, Vector v ) -> Vector
function vmmul( Vector v, Matrix A ) -> Vector
function outer( Vector u, Vector v ) -> Matrix
function vadd( Vector u, Vector v ) -> Vector
function vsub( Vector u, Vector v ) -> Vector
function lvmul( Scalar c, Vector u ) -> Vector
function rvmul( Vector u, Scalar c ) -> Vector
function vdiv( Vector u, Scalar c ) -> Vector
function mmmul( Matrix A, Matrix B ) -> Matrix
function mmadd( Matrix A, Matrix B ) -> Matrix
function mmsub( Matrix A, Matrix B ) -> Matrix
function ewvvmul( Vector u, Vector v ) -> Vector
function ewvvdiv( Vector u, Vector v ) -> Vector
function ewmmmul( Matrix A, Matrix B ) -> Matrix
function ewmmdiv( Matrix A, Matrix B ) -> Matrix

`,Bo=`canvas {
    width = 240
    height = 200
}

global {
   scalar scale = 18.0
   string labelSize = "7px"

   shape box = Rectangle {
      center: (0,0)
      width: scale*10
      height: scale*10
      fillColor: #1b1f8a11
      strokeWidth: 2.0
   }

   shape xAxis = Line {
      start: (-scale*5.,0)
      end: (scale*5.,0)
      strokeColor: #888888ff
      strokeWidth: 0.65
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape yAxis = Line {
      start: (0,-scale*5.)
      end: (0,scale*5.)
      strokeColor: #888888ff
      strokeWidth: 0.65
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   scalar gridStrokeWidth = 0.35
   color gridStrokeColor = #88888844

   shape gridX1 = Line {
      start: scale*(1.,-5.)
      end: scale*(1.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX2 = Line {
      start: scale*(2.,-5.)
      end: scale*(2.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX3 = Line {
      start: scale*(3.,-5.)
      end: scale*(3.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridX4 = Line {
      start: scale*(4.,-5.)
      end: scale*(4.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn4 = Line {
      start: scale*(-4.,-5.)
      end: scale*(-4.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn3 = Line {
      start: scale*(-3.,-5.)
      end: scale*(-3.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn2 = Line {
      start: scale*(-2.,-5.)
      end: scale*(-2.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridXn1 = Line {
      start: scale*(-1.,-5.)
      end: scale*(-1.,5.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }

   shape gridY1 = Line {
      start: scale*(-5.,1.)
      end: scale*(5.,1.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY2 = Line {
      start: scale*(-5.,2.)
      end: scale*(5.,2.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY3 = Line {
      start: scale*(-5.,3.)
      end: scale*(5.,3.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridY4 = Line {
      start: scale*(-5.,4.)
      end: scale*(5.,4.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn4 = Line {
      start: scale*(-5.,-4.)
      end: scale*(5.,-4.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn3 = Line {
      start: scale*(-5.,-3.)
      end: scale*(5.,-3.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn2 = Line {
      start: scale*(-5.,-2.)
      end: scale*(5.,-2.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
   shape gridYn1 = Line {
      start: scale*(-5.,-1.)
      end: scale*(5.,-1.)
      strokeColor: gridStrokeColor
      strokeWidth: gridStrokeWidth
   }
}

forall Matrix M {

   vec2 M.r1 = (1,0)
   vec2 M.r2 = (0,1)

   mat2x2 M.mat = ( M.r1, M.r2 )

   shape M.row1 = Line {
      start: (0,0)
      end: global.scale * M.r1
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape M.row2 = Line {
      start: (0,0)
      end: global.scale * M.r2
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.5
   }

   shape M.r1Label = Equation {
      center: global.scale*M.r1 + unit(M.r1)*7
      string: M.label + "_1"
      fillColor: M.row1.strokeColor
      fontSize: global.labelSize
   }

   shape M.r2Label = Equation {
      center: global.scale*M.r2 + unit(M.r2)*7
      string: M.label + "_2"
      fillColor: M.row2.strokeColor
      fontSize: global.labelSize
   }
}

forall Vector v {

   scalar θ = ?
   vec2 v.vec = (cos(θ),sin(θ))

   shape v.icon = Line {
      start: (0,0)
      end: global.scale * v.vec
      strokeColor: #aaaaaaff
      strokeWidth: 1.0
      endArrowhead: "straight"
      endArrowheadSize: 0.75
   }

   shape v.labelText = Equation {
      center: global.scale*v.vec + unit(v.vec)*7
      string: v.label
      fillColor: v.icon.strokeColor
      fontSize: global.labelSize
   }
}

forall Matrix A; Matrix B
where B := transpose(A)
{
   B.mat = A.mat'
   B.r1 = (B.mat[0][0],B.mat[0][1])
   B.r2 = (B.mat[1][0],B.mat[1][1])
}

forall Matrix A; Vector u; Vector v
where v := mvmul( A, u )
{
   v.vec = A.mat * u.vec
   v.icon.strokeColor = #0000aaff
}

forall Matrix A; Vector u; Vector v
where v := vmmul( u, A )
{
   v.vec = u.vec * A.mat
   v.icon.strokeColor = #ff6600ff
}

forall Scalar c
{
   c.val = ?

   shape c.icon = Circle {
      center: (0,0)
      r: global.scale * c.val
      fillColor : #ff660033
   }

   shape c.labelText = Equation {
      center: (global.scale*c.val + 5)*sqrt(2.)*(1,1)/2.
      string: c.label
      fillColor : c.icon.fillColor
      fontSize: global.labelSize
   }
}

forall Scalar c; Matrix A; Matrix B
where B := smmul( c, A )
{
   override B.mat = c.val * A.mat
   override B.r1 = c.val * A.r1
   override B.r2 = c.val * A.r2
}

forall Scalar c; Matrix A; Matrix B
where B := smmulr( A, c )
{
   override B.mat = A.mat * c.val
   override B.r1 = A.r1 * c.val 
   override B.r2 = A.r2 * c.val 
}

forall Scalar c; Matrix A; Matrix B
where B := msdiv( A, c )
{
   override B.mat = A.mat / c.val
   override B.r1 = A.r1 / c.val
   override B.r2 = A.r2 / c.val
}

forall Matrix A; Vector u; Vector v
where A := outer( u, v )
{
   override A.mat = outerProduct( u.vec, v.vec )
   override A.r1 = ( A.mat[0][0], A.mat[0][1] )
   override A.r2 = ( A.mat[1][0], A.mat[1][1] )
}

forall Vector u; Vector v; Vector w
where w := vadd(u,v)
{
   override w.vec = u.vec + v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := vsub(u,v)
{
   override w.vec = u.vec - v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := ewvvmul(u,v)
{
   override w.vec = u.vec .* v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Vector u; Vector v; Vector w
where w := ewvvdiv(u,v)
{
   override w.vec = u.vec ./ v.vec
   layer u.icon above w.icon
   layer v.icon above w.icon
}

forall Scalar c; Vector u; Vector v
where v := lvmul(c,u)
{
   override v.vec = c.val * u.vec
   layer u.icon above v.icon
}

forall Scalar c; Vector u; Vector v
where v := rvmul(u,c)
{
   override v.vec = u.vec * c.val
   layer u.icon above v.icon
}

forall Scalar c; Vector u; Vector v
where v := vdiv(u,c)
{
   override v.vec = u.vec / c.val
   layer u.icon above v.icon
}

forall Matrix A; Matrix B; Matrix C
where C := mmadd(A,B)
{
   override C.mat = A.mat + B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "+" + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := mmsub(A,B)
{
   override C.mat = A.mat - B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "-" + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := mmmul(A,B)
{
   override C.mat = A.mat * B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := ewmmmul(A,B)
{
   override C.mat = A.mat .* B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "\\odot " + B.label + ")"
}

forall Matrix A; Matrix B; Matrix C
where C := ewmmdiv(A,B)
{
   override C.mat = A.mat ./ B.mat
   override C.r1 = ( C.mat[0][0], C.mat[0][1] )
   override C.r2 = ( C.mat[1][0], C.mat[1][1] )

   override C.label = "(" + A.label + "\\oslash " + B.label + ")"
}

---- set constants (for debugging)

forall Scalar \`c\` {
   override \`c\`.val = 2
}

forall Vector \`u\` {
   override \`u\`.vec = (-3,2)
   override \`u\`.icon.strokeColor = #00aa00ff
}

forall Vector \`v\` {
   override \`v\`.vec = (2,1)
   override \`v\`.icon.strokeColor = #0000aaff
}

forall Matrix \`A\` {
   override \`A\`.mat = ( (-1,1), (0,-1) )
   override \`A\`.r1 = ( \`A\`.mat[0][0], \`A\`.mat[0][1] )
   override \`A\`.r2 = ( \`A\`.mat[1][0], \`A\`.mat[1][1] )
   override \`A\`.row1.strokeColor = #ff0000ff
   override \`A\`.row2.strokeColor = #ff0000ff
}

forall Matrix \`B\` {
   override \`B\`.mat = ( (4,2), (1,4) )
   override \`B\`.r1 = ( \`B\`.mat[0][0], \`B\`.mat[0][1] )
   override \`B\`.r2 = ( \`B\`.mat[1][0], \`B\`.mat[1][1] )
   override \`B\`.row1.strokeColor = #00aa00ff
   override \`B\`.row2.strokeColor = #00aa00ff
}

forall Matrix \`AT\` {
   override \`AT\`.r1 = ( \`AT\`.mat[0][0], \`AT\`.mat[0][1] )
   override \`AT\`.r2 = ( \`AT\`.mat[1][0], \`AT\`.mat[1][1] )
   override \`AT\`.row1.strokeColor = #ff000077
   override \`AT\`.row2.strokeColor = #ff000077
}

forall Matrix \`X\` {
   override \`X\`.mat = ( (-1,2), (0,-2) )
   override \`X\`.r1 = ( \`X\`.mat[0][0], \`X\`.mat[0][1] )
   override \`X\`.r2 = ( \`X\`.mat[1][0], \`X\`.mat[1][1] )
   override \`X\`.row1.strokeColor = #33dd00ff
   override \`X\`.row2.strokeColor = #33dd00ff
}

forall Matrix \`Y\` {
   override \`Y\`.mat = ( (2,1), (1,2) )
   override \`Y\`.r1 = ( \`Y\`.mat[0][0], \`Y\`.mat[0][1] )
   override \`Y\`.r2 = ( \`Y\`.mat[1][0], \`Y\`.mat[1][1] )
   override \`Y\`.row1.strokeColor = #4400ccff
   override \`Y\`.row2.strokeColor = #4400ccff
}

`,Mo=`AutoLabel All
Matrix X, Y

-- matrix-matrix addition
Matrix Z := mmadd(X,Y)

`,Go=`AutoLabel All
Matrix X, Y

-- elementwise matrix-matrix division
Matrix Z := ewmmdiv(X,Y)

`,Io=`AutoLabel All
Matrix X, Y

-- elementwise matrix-matrix multiplication
Matrix Z := ewmmmul(X,Y)

`,Vo=`AutoLabel All
Matrix X, Y

-- matrix-matrix multiplication
Matrix Z := mmmul(X,Y)

`,No=`AutoLabel All
Matrix X, Y

-- matrix-matrix subtraction
Matrix Z := mmsub(X,Y)

`,_o=`AutoLabel All
Matrix A, AT

-- matrix transpose
AT := transpose(A)
Label AT $A^\\mathsf{T}$

`,Qo=`AutoLabel All
Matrix A
Vector u

-- matrix-vector left multiplication
Vector Au := mvmul(A,u)
Label Au $Au$

`,qo=`AutoLabel All
Matrix A
Vector u

-- matrix-vector right multiplication
Vector uA := vmmul(u,A)
Label uA $u^\\mathsf{T}A$

`,Ro=`AutoLabel All
Scalar c
Vector u

-- scalar-vector division
Vector r := vdiv(u,c)
Label r $\\vec{u}/c$

`,Do=`AutoLabel All
Scalar c
Vector u

-- left scalar-vector multiplication
Vector r := lvmul(c,u)
Label r $c\\vec{u}$
`,$o=`AutoLabel All
Scalar c
Vector u

-- right scalar-vector multiplication
Vector r := rvmul(u,c)
Label r $\\vec{u}c$

`,Wo=`AutoLabel All
Vector u, v

-- vector-vector addition
Vector a := vadd(u,v)
Label a $u+v$

`,Fo=`AutoLabel All
Vector u, v

-- elementwise vector-vector division
Vector a := ewvvdiv(u,v)
Label a $u/v$

`,jo=`AutoLabel All
Vector u, v

-- elementwise vector-vector multiplication
Vector a := ewvvmul(u,v)
Label a $u*v$
`,Ho=`AutoLabel All
Vector u, v

-- vector-vector outer product
Matrix uv
uv := outer(u,v)
Label uv $(uv)^\\mathsf{T}$

`,zo=`AutoLabel All
Vector u, v

-- vector-vector subtraction
Vector a := vsub(u,v)
Label a $u-v$

`,Uo={"matrix-matrix-addition.substance":Mo,"matrix-matrix-division-elementwise.substance":Go,"matrix-matrix-multiplication-elementwise.substance":Io,"matrix-matrix-multiplication.substance":Vo,"matrix-matrix-subtraction.substance":No,"matrix-transpose.substance":_o,"matrix-vector-left-multiplication.substance":Qo,"matrix-vector-right-multiplication.substance":qo,"scalar-vector-division.substance":Ro,"scalar-vector-left-multiplication.substance":Do,"scalar-vector-right-multiplication.substance":$o,"vector-vector-addition.substance":Wo,"vector-vector-division-elementwise.substance":Fo,"vector-vector-multiplication-elementwise.substance":jo,"vector-vector-outerproduct.substance":Ho,"vector-vector-subtraction.substance":zo},Ko={"matrix-ops.domain":Oo,"matrix-ops.style":Bo,tests:Uo},Xo=`-- Clashing notations with \\in symbol
-- Vertex v1, v2, v3, v4 -- ∈ K (can't say this)

SimplicialComplex K
Vertex v1
InVS(v1, K)
Vertex v2
InVS(v2, K)
Vertex v3
InVS(v3, K)
Vertex v4
InVS(v4, K)

Edge e1
InES(e1, K)
Edge e2
InES(e2, K)
Edge e3
InES(e3, K)
Edge e4
InES(e4, K)
Face f 
InFS(f, K)

e1 := MkEdge(v2, v3)
e2 := MkEdge(v3, v1)
e3 := MkEdge(v1, v2)
e4 := MkEdge(v3, v4)
f := MkFace(e1, e2, e3)

AutoLabel All

Label v1 $v_1$
Label v2 $v_2$
Label v3 $v_3$
Label v4 $v_4$
Label e1 $e_1$
Label e2 $e_2$
Label e3 $e_3$
Label e4 $e_4$
Label f  $f$

---------------------------------

Set A
Set B1, B2, B3, B4
Point p1, p2, p3, p4
IsSubset(B1, A)
IsSubset(B2, A)
IsSubset(B3, A)

PointIn(B2, p1)
PointIn(B3, p1)
PointNotIn(B1, p1)
PointNotIn(B4, p1)

PointIn(B3, p2)
PointIn(B1, p2)
PointNotIn(B2, p2)
PointNotIn(B4, p2)

PointIn(B1, p3)
PointIn(B2, p3)
PointIn(B4, p3)
PointNotIn(B3, p3)

PointIn(B4, p4)
PointNotIn(A, p4)

Label p1 $v_1$
Label p2 $v_2$
Label p3 $v_3$
Label p4 $v_4$
Label B1 $e_1$
Label B2 $e_2$
Label B3 $e_3$
Label B4 $e_4$
Label A  $f$

---------------------------------

-- Identify points with vertices
Identified(p1,v1)
Identified(p2,v2)
Identified(p3,v3)
Identified(p4,v4)

-- Identify edges with sets
Identified(e1, B1)
Identified(e2, B2)
Identified(e3, B3)
Identified(e4, B4)

-- Also identify faces with sets
Identified(f, A)
`,Yo=`type Type

-- | Set domain program

type Set <: Type
type Point <: Type
type Map

constructor Singleton(Point p) -> Set

function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set

predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Nonempty(Set s)
predicate Intersect(Set s1, Set s2)
predicate NotIntersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate NotSubset(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate PointNotIn(Set s, Point p)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)

-- predicate Not(Predicate p)

-- These are new, and should go back in the Set domain
notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"

------------------------------
-- | Mesh domain program

type Vertex <: Type
type Edge <: Type
type Face <: Type
type SimplicialSubset -- Subset of a mesh; might not be a simplicial complex
type SimplicialComplex <: SimplicialSubset -- Mesh := SimplicialComplex(2); simplicial complex
type Subcomplex <: SimplicialSubset -- (V, E, F) linked to a mesh; is a simplicial complex

Vertex <: Subcomplex -- TODO: plugin doesn't deal w/ this
Edge <: SimplicialSubset
Face <: SimplicialSubset
-- Subcomplex <: SimplicialComplex
-- TODO: Technically true, but messes up our Style matching

constructor MkEdge(Vertex v1, Vertex v2) -> Edge
constructor MkFace(Edge e1, Edge e2, Edge e3) -> Face

function Star(SimplicialSubset s) -> SimplicialSubset
function Closure(SimplicialSubset s) -> Subcomplex
function Link(SimplicialSubset s) -> SimplicialSubset
function SetMinus(SimplicialSubset s, SimplicialSubset t) -> SimplicialSubset
function Boundary(SimplicialSubset s) -> SimplicialSubset
-- function Union(SimplicialSubset s, SimplicialSubset t) -> SimplicialSubset

-- Generic connectivity and selection predicates
predicate InVE(Vertex v, Edge e)
predicate InEF(Edge e, Face f)

predicate InVS(Vertex v, SimplicialSubset s)
predicate InES(Edge e, SimplicialSubset s)
predicate InFS(Face f, SimplicialSubset s)

-- For plugin use
predicate DeclaredV(Vertex v)
predicate DeclaredE(Edge e)
predicate DeclaredF(Face f)

type Object
Vertex <: Object
Edge <: Object
Face <: Object
SimplicialSubset <: Object
SimplicialComplex <: Object
Subcomplex <: Object

predicate Result(Object o) -- The Style only draws objects that are declared as results

-- Syntactic sugar
notation "Vertex v ∈ K" ~ "Vertex v; InVS(v, K)"
notation "Edge e ∈ K" ~ "Edge e; InES(e, K)"
notation "Face f ∈ K" ~ "Face f; InFS(f, K)"
-- notation "SimplicialSubset S ⊆ K" ~ "SimplicialSubset S; SubsetOf(S, K)"
-- notation "Subcomplex S ⊆ K" ~ "Subcomplex S; SubsetOf(S, K)"

-- These are new, from geometry.domain
notation "{p, q}" ~ "MkEdge(p, q)"
notation "{p, q, r}" ~ "MkFace(p, q, r)"
-- | above this line, concatenate the two .dom files for Sets and Meshes

------------------------------

-- predicate Identified(Point, Vertex)
predicate Identified(Type, Type)`,Zo=`-- Random seeds; first seed controls the vertex selection, second seed controls the mesh topology
-- plugin "ddgjs" (11.0, 9.0)

-- | Set Style program

canvas {
  width = 800
  height = 700
}

const {
      subsetColor = sampleColor(0.2, "rgb")
      repelWeight = 0.0
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }

    x.text = Equation {
        string : x.label
	fillCcolor : setOpacity(x.icon.fillColor, 1.0)
    }

    ensure contains(x.icon, x.text)
    ensure contains(x.icon, x.text)
    ensure lessThan(20, x.icon.r)
    -- encourage sameCenter(x.text, x.icon)
    x.icon below x.text
}

forall Set x; Set y
where IsSubset(x, y) {
    ensure contains(y.icon, x.icon, 5.0)
    ensure smallerThan(x.icon, y.icon)
    ensure disjoint(y.text, x.icon) -- Is this not working?
    x.icon above y.icon
    -- layering1  = y.text below x.icon
}

forall Set x; Set y
where NotIntersecting(x, y) {
    ensure disjoint(x.icon, y.icon)
}

forall Set x; Set y
where Intersect(x, y) {
    ensure overlapping(x.icon, y.icon)
    ensure disjoint(y.text, x.icon)
    ensure disjoint(x.text, y.icon)
}

forall Point p {
    p.offset = 10.0
    p.icon = Circle {
        strokeWidth : 0.0
        fillColor : rgba(0.0, 0.0, 0.0, 1.0)
        r : 3.0
    }

    p.text = Equation {
        string : p.label
        center : p.icon.center + (p.offset, p.offset)
    }
}

forall Point p
with Set A
where PointIn(A, p) {
    ensure contains(A.icon, p.icon, 0.3 * A.icon.r)
    p.icon above A.icon
    p.text above A.icon
}

forall Point p
with Set A
where PointNotIn(A, p) {
-- where Not(PointIn(A, p)) {
    ensure disjoint(A.icon, p.icon)
}

-- Put all the text on top of everything
forall Set x; Set y {
    encourage notTooClose(x.text, y.text, const.repelWeight)
    x.icon below y.text
}

forall Point p; Point q {
     -- encourage notTooClose(p.icon, q.icon, G.repelWeight2)
     encourage notTooClose(p.text, q.text, const.repelWeight)
}

forall Set x; Point p {
    p.icon above x.icon
    p.text above x.icon

     encourage notTooClose(p.text, x.text, const.repelWeight)
}

--------------------------------------
-- | Mesh Style program

global {
    -- TODO: Behaves badly w/ padding=0.0, 1.0
    global.labelPadding2 = 10.0
    global.padding = 20.0
    global.stroke = 2.0
    global.spacing = 150.0

    global.sc_rect_stroke = 2.0
    global.edgeStroke = 1.5
    global.selectedEdgeStroke = 3.0

    global.selectedFaceColor = global.selectedColor

    global.selectedRadius = 3.6
    global.selectedThickness = 3.1

    global.selectedColor = Colors.midnightblue
    global.selectedColor2 = Colors.lightslategray

    global.starColor = global.selectedColor
    global.starColor2 = global.selectedColor2

    global.closureColor = global.selectedColor
    global.closureColor2 = global.selectedColor2

    global.linkColor = global.selectedColor
    global.linkColor2 = global.selectedColor2

    global.setminusColor = global.selectedColor
    global.setminusColor2 = global.selectedColor2

    global.boundaryColor = global.selectedColor
    global.boundaryColor2 = global.selectedColor2

    -- For conversion from math space to screen space
    -- TODO: compute these more automatically
    -- These numbers (weights on inRange and the range itself) have been heavily tweaked
    -- And it makes convergence hard
    -- global.scaleFactor = ?
    ensure inRange(global.scaleFactor, 75.0, 150.0)
    global.scaleFactor = 70.0
    -- global.offset = 150.0
}

Colors {
    -- Keenan palette
    Colors.black = rgba(0.0, 0.0, 0.0, 1.0)
    Colors.midnightblue = rgba(0.14, 0.16, 0.52, 1.0)
    Colors.lightslategray = rgba(0.50, 0.51, 0.69, 1.0)
    Colors.silver = rgba(0.71, 0.72, 0.79, 1.0)
    Colors.gainsboro = rgba(0.87, 0.87, 0.87, 1.0)

    Colors.darkgray = rgba(0.1, 0.1, 0.1, 1.0)
    Colors.gray = rgba(0.8, 0.8, 0.8, 1.0)
    Colors.red = rgba(1.0, 0.0, 0.0, 1.0)
    Colors.pink = rgba(1.0, 0.4, 0.7, 1.0)
    Colors.yellow = rgba(1.0, 1.0, 0.0, 1.0)
    Colors.orange = rgba(1.0, 0.6, 0.0, 1.0)
    Colors.lightorange = rgba(1.0, 0.6, 0.0, 0.25)
    Colors.green = rgba(0.0, 1.0, 0.0, 1.0)
    Colors.blue = rgba(0.0, 0.0, 1.0, 1.0)
    Colors.sky = rgba(0.325, 0.718, 0.769, 1.0)
    Colors.lightsky = rgba(0.325, 0.718, 0.769, 0.25)
    Colors.lightblue = rgba(0.0, 0.0, 1.0, 0.25)
    Colors.cyan = rgba(0.0, 1.0, 1.0, 1.0)
    Colors.purple = rgba(0.5, 0.0, 0.5, 1.0)
    Colors.lightpurple = rgba(0.5, 0.0, 0.5, 0.25)
    Colors.white = rgba(1.0, 1.0, 1.0, 1.0)
    Colors.none = none()
    Colors.bluegreen = rgba(0.44, 0.68, 0.60, 1.0)
}

-- TODO: this is going to match Subcomplex too... how to fix that?
forall SimplicialComplex K {
       -- No longer necessary, as box size is being computed
       -- K.x_offset = ?
       -- K.y_offset = ?

       -- Plugin computes mesh's bbox center and dimensions; Style scales it and adds padding
       -- K.box_padding = 50.0
       K.center_x = -200.0
       K.center_y = 0.0
       K.size_x = 250.0
       K.size_y = 300.0

       -- K.center_x = ddg[K.name]["center_x"] * global.scaleFactor
       -- K.center_y = ddg[K.name]["center_y"] * global.scaleFactor
       -- K.size_x = ddg[K.name]["size_x"] * global.scaleFactor + K.box_padding
       -- K.size_y = ddg[K.name]["size_y"] * global.scaleFactor + K.box_padding

       K.icon = Rectangle {
           center : (K.center_x, K.center_y)
	       width : K.size_x
	       height : K.size_y
	       -- rotation : 0.0
	       fillColor : setOpacity(Colors.gainsboro, 0.2)
	       strokeColor : Colors.black
	       strokeWidth : global.sc_rect_stroke
       }

       K.padding = 25.0

       K.text = Equation {
         center : (K.icon.center[0] - K.icon.width / 2.0 + K.padding, K.icon.center[1] - K.icon.height / 2.0 + K.padding)
         string : K.label
         -- rotation : 0.0
         fillColor : Colors.black
       }

     encourage centerLabel(K.icon, K.text)

       K.text above K.icon

       -- Expression label
       K.expr_padding = 25.0

       K.const_text = Equation {
       	 center : (K.icon.center[0], K.icon.center[1] - (K.size_y / 2.0) - K.expr_padding)
       	 string : "\\text{ }"
       	 -- rotation : 0.0
       	 fillColor : global.starColor
       }

       K.const_text above K.icon
}

-- TODO: this generates a (K1, K2) and (K2, K1) match
forall SimplicialComplex K1; SimplicialComplex K2 {
	 padding = 30.0

	 -- TODO: improve this for rectangles by not just using the x size
	 ensure disjoint(K1.icon, K2.icon, padding)
         -- COMBAK: Put \`sameHeight\` back in?
	 -- ensure sameHeight(K1.icon, K2.icon)

	 -- distFn = ensure distBetween(K1.icon, K2.icon, padding)
}

forall Vertex v 
where InVS(v, K)
with SimplicialComplex K {
     v.xpos = ?
     v.ypos = ?
       -- v.xpos = ddg[v.name]["x"] * global.scaleFactor
       -- v.ypos = ddg[v.name]["y"] * global.scaleFactor

       v.icon = Circle { 
         center : (v.xpos, v.ypos) -- avoid "x <- f(x)" in override
         r : global.selectedRadius
	 fillColor : Colors.black
	 strokeWidth : 0.0
       }

       -- NOTE: by default, this starts with an empty string, so we only label user-declared vertices
       v.text = Equation {
	 center : (?, ?) -- v.icon.x + global.padding
	                 -- v.icon.y + global.padding
	 string : v.label
	 -- string : " " -- TODO: the frontend does not deal with empty strings well! Doesn't seem to generate a label with dimensions. See above for how to get around this
	 -- rotation : 0.0
	 fillColor : v.icon.fillColor
       }

       v.icon above K.icon
       v.text above K.icon

     ensure contains(K.icon, v.icon, v.icon.r) -- padding

     ensure atDist(v.icon, v.text, global.labelPadding2)
     ensure contains(K.icon, v.text, 0.0)
}

-- Style a distinguished vertex (only if it's a result)
forall Vertex v
where DeclaredV(v); InVS(v, K); Result(v)
with SimplicialComplex K {
      -- Don't label the vertex because then we need to position it...
      -- override v.text.string = v.label

      override v.icon.r = global.selectedRadius
      override v.icon.fillColor = global.closureColor

      v.offset = 10.0
     encourage near(v.text, v.icon, v.offset)

      /*
       -- Optimize the label padding, only for the distinguished vertex
      v.padding_x = ?
      v.padding_y = ?
      override v.text.x = v.icon.x + v.padding_x
      override v.text.y = v.icon.y + v.padding_y

      v.offset = 30.0
      -- This is trying to place the labels but it's very slow, goes from 40s to 3min
     ensure contains(K.icon, v.text)
      -- Label's color might need to be programmatically set depending on its location

      v.padding_range = 20.0
     ensure inRange(v.padding_x, v.icon.x - v.padding_range, v.icon.x + v.padding_range)
     ensure inRange(v.padding_y, v.icon.y - v.padding_range, v.icon.y + v.padding_range) */
}

forall Vertex v; Edge e
where InVS(v, K); InES(e, K)
with SimplicialComplex K {
     offset = 5.0
     -- Make sure the label doesn't overlap with any edge
     -- TODO: this is NaNing
     ensure disjoint(v.text, e.icon, offset)
}

forall Vertex v; Edge e
where DeclaredV(v); InVS(v, K); InES(e, K)
with SimplicialComplex K {
     offset = 5.0
     -- Make sure the label doesn't overlap with any edge
     -- TODO: this is NaNing
     ensure disjoint(v.text, e.icon, offset)
}

forall Edge e
where e := MkEdge(v1, v2); InES(e, K)
with Vertex v1; Vertex v2; SimplicialComplex K {
     e.icon = Line { 
     	     start : v1.icon.center
     	     end : v2.icon.center
	     strokeColor : Colors.black
	     strokeWidth : global.edgeStroke
     }

     e.text = Equation {
       center : (?, ?)
       string : e.label
       -- rotation : 0.0
       strokeColor : Colors.black
     }

     encourage nearPt(e.text, average2(v1.xpos, v2.xpos), average2(v1.ypos, v2.ypos))
     ensure disjoint(e.text, e.icon, 5.0)

     v1.icon above e.icon
     v2.icon above e.icon
     e.icon above K.icon
}

-- Style a distinguished edge (only if it's declared to be a result)
forall Edge e
where DeclaredE(e); e := MkEdge(v1, v2); InES(e, K); Result(e)
with Vertex v1; Vertex v2; SimplicialComplex K {
     override e.icon.strokeWidth = global.selectedEdgeStroke
     override e.icon.strokeColor = global.closureColor

     e.text = Equation {
     -- TODO: Vectorize this
       center : (average2(e.icon.start[0], e.icon.end[0]) + global.padding, average2(e.icon.start[1], e.icon.end[1]) + global.padding)
       -- string : e.label
       string : "\\\\text{ }" 
       -- rotation : 0.0
       fillColor : e.icon.strokeColor
     }

     e.text above K.icon
}

forall Face f -- 255,552 substitutions = 22 e * 22 e * 22 e * 12 f * 2 sc
where f := MkFace(e1, e2, e3); InFS(f, K)
with Edge e1; Edge e2; Edge e3; SimplicialComplex K {
     f.color = Colors.silver

     f.icon = Path { 
        -- As temp hack around furthestFrom, web-runtime assumes triangle is drawn in a consistent order (first point of each line)
     	     d : triangle(e1.icon, e2.icon, e3.icon)
	     strokeWidth : 0.0
	     fillColor : setOpacity(f.color, 0.8)
	     strokeColor : setOpacity(f.color, 0.8)
	     -- rotation : 0.0
     }

     f.text = Equation {
       -- Makes assumptions about vertex order in constructor
       center : (average([e1.icon.start[0], e2.icon.start[0], e3.icon.start[0]]), average([e1.icon.start[1], e2.icon.start[1], e3.icon.start[1]]))
       string : f.label
       -- rotation : 0.0
       fillColor : Colors.black
     }

     f.text above K.icon

     f.icon above K.icon
}

-- Style and label a distinguished face
forall Face f
where DeclaredF(f); InFS(f, K)
with SimplicialComplex K {
     -- Need to pick a color that doesn't "override" the selected edges and vertices!
     override f.icon.fillColor = global.selectedFaceColor
     override f.icon.strokeColor = global.selectedFaceColor
}

-- Relative layerings within a simplicial complex
forall Vertex v; Edge e; Face f
where InVS(v, K); InES(e, K); InFS(f, K)
with SimplicialComplex K {
      v.text above f.icon 
      v.text above e.icon
}

-- Only label the (last) result of an operation
forall Object e
with SimplicialComplex K
where Result(e) {
      override K.const_text.string = e.label
}

forall Edge e; Face f {
     e.icon above f.icon
     e.text above f.icon
}

forall Vertex v; Vertex w {
     encourage notTooClose(v.icon, w.icon, 1.0)
}

--------------------------------------
-- | above this line, concatenate the two .style files for Sets and Meshes

forall Point p; Vertex v
where Identified(p, v) {

   icon = Line {
      start : p.icon.center
      end : v.icon.center
      strokeColor: setOpacity(Colors.black, 0.25)
      strokeWidth : 2.0
      style : "dashed"
   }

   layer icon above p.icon
   layer icon above v.icon
   
   -- rather than worry about the two diagrams colliding, just add some penalty that tries to make all the connecting lines fairly long
   -- p.vector = (p.icon.x, p.icon.y)
   -- v.vector = (v.icon.x, v.icon.y)
   vec = p.icon.center - v.icon.center

   -- TODO: encourage or ensure?
   -- TODO: make this better
   -- lenFn = encourage equal(norm_(p.icon.x - v.icon.x, p.icon.y - v.icon.y), 100.0)
}

forall Set s; Edge e
where Identified(e, s) {
      -- Make all edge subsets the same random color
      s.icon.fillColor = setOpacity(Colors.purple, 0.2) -- const.subsetColor
}

-- TODO: hack for position; assuming there's only one face
forall Set s; Face f
where Identified(f, s) {
    s.icon.center = (200.0, 0.0)
}`,Jo=`-- Clashing notations with \\in symbol
-- Vertex v1, v2, v3, v4 -- ∈ K (can't say this)

SimplicialComplex K
Vertex v1 ∈ K
Vertex v2 ∈ K
Vertex v3 ∈ K
Vertex v4 ∈ K

Edge e1 ∈ K
Edge e2 ∈ K
Edge e3 ∈ K
Edge e4 ∈ K
Face f ∈ K

e1 := {v2, v3}
e2 := {v3, v1}
e3 := {v1, v2}
e4 := {v3, v4}
f := {e1, e2, e3}

AutoLabel All

Label v1 $v_1$
Label v2 $v_2$
Label v3 $v_3$
Label v4 $v_4$
Label e1 $e_1$
Label e2 $e_2$
Label e3 $e_3$
Label e4 $e_4$
Label f  $f$

---------------------------------

Set A
Set B1, B2, B3, B4
Point p1, p2, p3, p4
B1 ⊂ A
B2 ⊂ A
B3 ⊂ A

p1 ∈ B2
p1 ∈ B3
p1 ∉ B1
p1 ∉ B4

p2 ∈ B3
p2 ∈ B1
p2 ∉ B2
p2 ∉ B4

p3 ∈ B1
p3 ∈ B2
p3 ∈ B4
p3 ∉ B3

p4 ∈ B4
p4 ∉ A

Label p1 $v_1$
Label p2 $v_2$
Label p3 $v_3$
Label p4 $v_4$
Label B1 $e_1$
Label B2 $e_2$
Label B3 $e_3$
Label B4 $e_4$
Label A  $f$

---------------------------------

-- Identify points with vertices
Identified(p1,v1)
Identified(p2,v2)
Identified(p3,v3)
Identified(p4,v4)

-- Identify edges with sets
Identified(e1, B1)
Identified(e2, B2)
Identified(e3, B3)
Identified(e4, B4)

-- Also identify faces with sets
Identified(f, A)
`,n0={"DomainInterop-unsugared.substance":Xo,"DomainInterop.domain":Yo,"DomainInterop.style":Zo,"DomainInterop.substance":Jo},e0=`--- This .style file is intended for debugging purposes only

canvas {
  width = 800
  height = 700
}

forall Maze x {
    x.icon = Ellipse {
        center: [0.0, 0.0]
        rx: 200.0
        ry: 100.0
    }
}

forall Triangle x {
    x.x = ? 
    x.y = ?
    x.a = (x.x - 100.0, x.y)
    x.b = (x.x, x.y + 150.0)
    x.c = (x.x + 100.0, x.y)

    x.icon = Polygon {
        points: [ x.a, x.b, x.c ]
    }

    encourage lessThan(x.x, 400.0)
    encourage lessThan(-400.0, x.x)
    encourage lessThan(x.y, 400.0)
    encourage lessThan(-400.0, x.y)
}

forall Triangle x; Maze y {
    ensure disjoint(x.icon, y.icon)
}
`,t0=`--- This .style file is intended for debugging purposes only

canvas {
  width = 800
  height = 700
}

forall Maze x {
    x.icon = Ellipse {
        center: [0.0, 0.0]
        rx: 150.0
        ry: 80.0
    }
}

forall Triangle x {
    x.icon = Ellipse{}
    ensure lessThan(x.icon.rx, 300)
    ensure lessThan(50.0, x.icon.rx)
    ensure lessThan(x.icon.ry, 300)
    ensure lessThan(50.0, x.icon.ry)
}

forall Triangle x; Maze y {
    ensure disjoint(x.icon, y.icon)
}
`,o0=`
type Maze
type Triangle
`,r0=`
canvas {
  width = 800
  height = 700
}

forall Maze x {
    x.a1 = (-25.0, 70.0)
    x.a2 = (0.0, -30.0)
    x.a3 = (-50.0, -30.0)
    x.a4 = (-50.0, 5.0)
    x.a5 = (-110.0, -55.0)
    x.a6 = (-140.0, -160.0)
    x.a7 = (100.0, -80.0)
    x.a8 = (0.0, 170.0)
    x.a9 = (-320.0, 80.0)
    x.icon = Polygon {
        points: [x.a1, x.a2, x.a3, x.a4, x.a5, x.a6, x.a7, x.a8, x.a9]
    }
}

forall Triangle x {
    x.a = (?, ?)
    x.b = (?, ?)
    x.c = (?, ?)
    x.icon = Polygon {
        points: [ x.a, x.b, x.c ]
    }
    
    -- Compute area
    x.ab = sqrt((x.b[0] - x.a[0]) * (x.b[0] - x.a[0]) + (x.b[1] - x.a[1]) * (x.b[1] - x.a[1]))
    x.bc = sqrt((x.c[0] - x.b[0]) * (x.c[0] - x.b[0]) + (x.c[1] - x.b[1]) * (x.c[1] - x.b[1]))
    x.ca = sqrt((x.a[0] - x.c[0]) * (x.a[0] - x.c[0]) + (x.a[1] - x.c[1]) * (x.a[1] - x.c[1]))
    x.area = 0.25 * sqrt((x.ab + x.bc + x.ca) * (-x.ab + x.bc + x.ca) * (x.ab - x.bc + x.ca) * (x.ab + x.bc - x.ca))
    
    -- Prescribed area
    encourage equal(x.area, 1000.0)
    
    -- Try to be equilateral
    encourage equal(x.ab, x.bc)
    encourage equal(x.bc, x.ca)
    encourage equal(x.ca, x.ab)

    -- Try getting close to the center
    encourage equal(0.0, x.a[0])
    encourage equal(0.0, x.a[1])
}

forall Triangle x; Triangle y {
   ensure disjoint(x.icon, y.icon)
}

forall Triangle x; Maze y {
    ensure disjoint(x.icon, y.icon)
}
`,a0=`
Maze M
Triangle T1, T2, T3
`,l0={"ellipse-maze.style":e0,"ellipses-maze.style":t0,"maze.domain":o0,"maze.style":r0,"maze.substance":a0},i0=`
type Shape
`,s0=`
canvas {
  width = 800
  height = 700
}

forall Shape x {
  x.x = ?
  x.y = ?
  x.angle = ?
  
  x.c = cos(x.angle)
  x.s = sin(x.angle)

  x.aa1 = (-50.0, 0.0)
  x.aa2 = (100.0, 100.0)
  x.aa3 = (-100.0, 100.0)
  x.aa4 = (-100.0, -100.0)
  x.aa5 = (50.0, 0.0)
  x.aa6 = (0.0, 0.0)

  x.a1 = (x.c * x.aa1[0] - x.s * x.aa1[1] + x.x, x.s * x.aa1[0] + x.c * x.aa1[1] + x.y)
  x.a2 = (x.c * x.aa2[0] - x.s * x.aa2[1] + x.x, x.s * x.aa2[0] + x.c * x.aa2[1] + x.y)
  x.a3 = (x.c * x.aa3[0] - x.s * x.aa3[1] + x.x, x.s * x.aa3[0] + x.c * x.aa3[1] + x.y)
  x.a4 = (x.c * x.aa4[0] - x.s * x.aa4[1] + x.x, x.s * x.aa4[0] + x.c * x.aa4[1] + x.y)
  x.a5 = (x.c * x.aa5[0] - x.s * x.aa5[1] + x.x, x.s * x.aa5[0] + x.c * x.aa5[1] + x.y)
  x.a6 = (x.c * x.aa6[0] - x.s * x.aa6[1] + x.x, x.s * x.aa6[0] + x.c * x.aa6[1] + x.y)

  x.icon = Polygon {
    points: [x.a1, x.a2, x.a3, x.a4, x.a5, x.a6]
  }

  encourage equal(0.0, x.a6[0])
  encourage equal(0.0, x.a6[1])
}

forall Shape x; Shape y {
  ensure disjoint(x.icon, y.icon)
}
`,c0=`
Shape Yin, Yang
`,d0={"yin-yang.domain":i0,"yin-yang.style":s0,"yin-yang.substance":c0},p0={maze:l0,"yin-yang":d0},g0=`type Center
type Shape

type Point
type Circle

Point <: Shape
Circle <: Shape

constructor Inversion( Shape original, Center center ) -> Shape`,f0=`canvas {
    width = 800
    height = 700
}

forall Shape s {
    shape s.eq = Equation {
        center: s.center
        string: s.label
        fontSize: "15"
    }
}

forall Center c {
    vec2 c.center = (?, ?)
    scalar c.r = ?
    shape c.circle = Circle {
        center: c.center
        r: c.r
        fillColor: rgba(0, 0, 0, 0)
        strokeWidth: 2
        strokeStyle: "dashed"
        strokeColor: rgba(0, 0, 0, 1)
    }
    shape c.point = Circle {
        center: c.center
        fillColor: rgba(0, 0, 0, 1)
        r: 3
    }
    shape c.eq = Equation {
        string: c.label
        fontSize: "15"
    }
    ensure lessThan(100, c.r)
    ensure touching(c.eq, c.circle, 5)
}

forall Circle c {
    vec2 c.center = (?, ?)
    scalar c.r = ?
    shape c.circle = Circle {
        center: c.center
        r: c.r
    }
    ensure lessThan(30, c.r)
}

forall Point p {
    vec2 p.pos = (?, ?)
    shape p.point = Circle {
      r: 5
      center: p.pos
    }
}

forall Shape s; Point p; Center r
where s := Inversion( p, r ) {
    scalar s.d2 = vdistsq(p.pos, r.center)
    scalar s.coef = r.r * r.r / s.d2
    vec2 s.pos = r.center + s.coef * (p.pos - r.center)
    shape s.point = Circle {
        r: 5
        fillColor: p.point.fillColor
        center: s.pos
    }
}

forall Shape s; Circle c; Center r
where s := Inversion( c, r ) {
    scalar s.d = vdist( c.center, r.center )

    scalar s.coef1 = ( s.d - c.r ) / s.d
    vec2 s.p1 = r.center + s.coef1 * (c.center - r.center)

    scalar s.i1coef = r.r * r.r / vdistsq( r.center, s.p1 )
    vec2 s.ip1 = r.center + s.i1coef * ( s.p1  - r.center)

    scalar s.coef2 = (s.d + c.r) / s.d
    vec2 s.p2 = r.center + s.coef2 * (c.center - r.center)

    scalar s.i2coef = r.r * r.r / vdistsq( r.center, s.p2 )
    vec2 s.ip2 = r.center + s.i2coef * ( s.p2  - r.center )

    scalar s.r = vdist(s.ip1, s.ip2) / 2
    vec2 s.center = 0.5 * (s.ip1 + s.ip2)

    shape s.circle = Circle {
        r: s.r
        fillColor: c.circle.fillColor
        center: s.center
    }

    ensure lessThan(30, s.r)
}

forall Circle c1; Circle c2 {
    ensure touching(c1.circle, c2.circle)
}

forall Center r; Circle c {
    ensure contains(r.circle, c.circle, 30)
}

forall Shape s1; Shape s2 {
    s1.eq above s2.circle
}`,u0=`Center r

Circle c1, c2, c3

Shape ic1 := Inversion(c1, r)
Shape ic2 := Inversion(c2, r)
Shape ic3 := Inversion(c3, r)

Label r $S^1$

Label c1 $c_1$
Label c2 $c_2$
Label c3 $c_3$

Label ic1 $c'_1$
Label ic2 $c'_2$
Label ic3 $c'_3$`,h0={"mobius.domain":g0,"mobius.style":f0,"mobius.substance":u0},b0=`type Object
type Morphism

function tensor(Object, Object) -> Object
-- notation “a * b” ~ “tensor(a, b)” -- Could use unicode

predicate ChildOf(Object, Morphism)
predicate NotChildOf(Object, Morphism)

constructor join(Object first, Object second) -> Morphism`,k0=`Colors {
    black = rgba(0.0, 0.0, 0.0, 1.0)
    darkgray = rgba(0.1, 0.1, 0.1, 1.0)
    gray = rgba(0.8, 0.8, 0.8, 1.0)
    red = rgba(1.0, 0.0, 0.0, 1.0)
    pink = rgba(1.0, 0.4, 0.7, 1.0)
    yellow = rgba(1.0, 1.0, 0.0, 1.0)
    orange = rgba(1.0, 0.6, 0.0, 1.0)
    lightorange = rgba(1.0, 0.6, 0.0, 0.25)
    green = rgba(0.0, 1.0, 0.0, 1.0)
    blue = rgba(0.0, 0.0, 1.0, 1.0)
    sky = rgba(0.325, 0.718, 0.769, 1.0)
    lightsky = rgba(0.325, 0.718, 0.769, 0.25)
    lightblue = rgba(0.0, 0.0, 1.0, 0.25)
    cyan = rgba(0.0, 1.0, 1.0, 1.0)
    purple = rgba(0.5, 0.0, 0.5, 1.0)
    white = rgba(1.0, 1.0, 1.0, 1.0)
    none = none()
    bluegreen = rgba(0.44, 0.68, 0.60, 1.0)
}

canvas {
    width = 1000.
    height = 600.

    shape box = Rectangle {
        width: width
        height: height
        center: (0., 0.)
        strokeColor: rgba(0., 0., 0., 1.)
        strokeWidth: 3.
        fillColor: rgba(1., 1., 1., 0.)
    }

    -- TODO(bug): file issue as the local var \`box\` doesn't seem to work here
         left = canvas.box.center[0] - canvas.box.width/2.
         right = canvas.box.center[0] + canvas.box.width/2.
         top = canvas.box.center[1] + canvas.box.height/2.
         bot = canvas.box.center[1] - canvas.box.height/2.
}

forall Object o {
       scalar o.minY = canvas.bot
       scalar o.maxY = canvas.top

       scalar o.minX = canvas.left
       scalar o.maxX = canvas.right

    scalar o.y = ?
    vec2 o.v1 = (o.minX, o.y)
    vec2 o.v2 = (o.maxX, o.y)

    shape o.shape = Line {
        start: o.v1
        end: o.v2
        -- strokeColor: rgba(0., 0., 0., 1.)
        strokeWidth: 5.
        endArrowhead: "straight"
        endArrowheadSize: 0.5
    }

    shape o.text = Equation {
           center: (o.shape.start + o.shape.end) / 2.
           string: o.label
           fontSize: "15px"
    }

    ensure inRange(o.y, o.minY, o.maxY)

    ensure lessThan(o.shape.start[0], o.shape.end[0], 50.)

    -- ensure minSize(o.shape)
    o.shape above canvas.box
}

forall Morphism m {
         -- The floating vars are the rect centers
         shape m.shape = Rectangle { 
                  center: (?, ?)
                  width: 100. 
                  height: 300.
               fillColor: rgba(1.,1.,1.,0.)
               strokeColor: rgba(0., 0., 0., 1.)
               strokeWidth: 3.
         }

         shape m.text = Equation {
               center: m.shape.center
               string: m.label
                  fontSize: "15px"
         }

         m.left = m.shape.center[0] - m.shape.width/2.
         m.right = m.shape.center[0] + m.shape.width/2.
         m.top = m.shape.center[1] + m.shape.height/2.
         m.bot = m.shape.center[1] - m.shape.height/2.

         ensure contains(canvas.box, m.shape)

         m.shape above canvas.box
         m.text above m.shape
}

forall Object p
with Object a, b
where p := tensor(a, b) {
      override p.shape.strokeColor = rgba(1.0, 0.0, 0.0, 0.1)
      override p.shape.strokeStyle = "dashed"

      -- bottom-up
      override p.y = (a.y + b.y)/2.

      -- top-down
      override a.minY = p.minY
      override a.maxY = p.maxY

      override b.minY = p.minY
      override b.maxY = p.maxY

      override a.minX = p.minX
      override a.maxX = p.maxX

      override b.minX = p.minX
      override b.maxX = p.maxX

      -- Repel the \`y\`s of the input objects from each other? TODO: Will this work for "base" objects and not "phantom' ones? And not be applied too many times? Problem is that "child" objects don't all know each other
      -- encourage repelScalar(a.y, b.y)
}

Morphism m
with Object a, b
forall where m := join(a, b) {
      -- override m.shape.fillColor = Colors.green

      -- TODO: overriding nonexistent fields should throw an error. like \`override a.sdjfsadfjhks = 0\`
      override a.maxX = m.left
      override b.minX = m.right

      -- top-down?
      override a.minY = m.bot
      override a.maxY = m.top

      override b.minY = m.bot
      override b.maxY = m.top

      -- Instead of doing this, every object has its own local range that it imposes, and the info moves from top-down
      -- ensure inRange(a.y, m.bot, m.top)
      -- ensure inRange(b.y, m.bot, m.top)

      -- TODO: Repel the \`y\` from both top and bot of shape, on each side

      -- The "invisible object" is placed near the center 
      encourage nearScalar(a.y, m.shape.center[1])
      encourage nearScalar(b.y, m.shape.center[1])

      a.shape above m.shape
      b.shape above m.shape
}

forall Object o; Morphism m
where NotChildOf(o, m) {
       ensure disjointRectLineAA(m.shape, o.shape)
}

forall Morphism m1; Morphism m2 {
         ensure disjoint(m1.shape, m2.shape)
}

-- TODO: each object's \`y\` should be attracted to the center of the morphism, and repel all other objects
-- TODO: each object's \`y\` should be out of the range of all morphisms' \`y\`s
-- TODO: an object between the morphisms should have a \`y\` that's in the range of both morphisms

-- TODO: Maybe these are conflicting with \`nearScalar\`?
-- Object o; Object p; Morphism m
-- where ChildOf(o, m); ChildOf(p, m) {
--       encourage repelScalar(o.y, p.y)

-- }

-- Object o; Morphism m
-- where ChildOf(o, m) {
--       encourage repelScalar(o.y, m.top)
--       encourage repelScalar(o.y, m.bot)
-- }

-- Problem is that "child" objects don't all know each other
-- So, it's hard (impossible?) to select the every object/morphism pair where the object is not a child of the morphism

-- TODO: how would you state this predicate? "NotChildOf" is not unique to this case
-- Object o; Morphism m
-- where NotChildOf(o, m) {
       -- Would want \`o.y\` disjoint from \`[m.bot, m.top]\`
       -- ensure disjointScalar(o.y, m.bot, m.top)
-- }
`,x0=`-- (1)

-- Object A, B, C
-- C := tensor(A, B)

-- (2)

-- Object A
-- Object B
-- Morphism F := join(A, B)

-- (3)

-- Object A, B, C, D
-- C := tensor(A, B)
-- Morphism F := join(C, D)

-- AutoLabel All

------

-- (4)

Object A, B, C , D
Object E, F

Object ab := tensor(A, B)
Object abc := tensor(ab, C)
Morphism F2 := join(abc, D)

Object de := tensor(D, E)
Morphism G := join(de, F)

-- TODO: Might need a plugin. Is there any way to naturally state this in the math, anyway?
ChildOf(A, F2)
ChildOf(B, F2)
ChildOf(C, F2)

ChildOf(D, G)
ChildOf(E, G)

NotChildOf(E, F2)
-- OutputOf(E, F2)
-- Presumably you would generate these by analyzing the "chain"? What would be the right thing to state here?

AutoLabel All
Label F2 $F^2$
Label G $G$

-- Even this simple example freezes up when the Object initially intersects the Morphism
-- Anything more complicated makes things crash for me

------

-- Object A
-- Morphism F

-- AutoLabel All`,v0={"monoidal-test.domain":b0,"monoidal-test.style":k0,"test.substance":x0},m0=`<!-- <svg width="100%" height="100%" version="1.2" viewBox="0 0 240 180" xmlns="http://www.w3.org/2000/svg"> -->
<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" version="1.2" viewBox="0 0 240 180">
	<defs>
		<script>
			function $(sel) { return document.querySelector(sel); }
		<\/script>
		<audio xmlns="http://www.w3.org/1999/xhtml">
			<source src="pop.wav" type="audio/x-wav" />
		</audio>
	</defs>
	<rect x="0" y="0" width="240" height="180" fill="#ffe6e6" fill-opacity="1" stroke="none" rx="0" transform="rotate(0, 0, 0)">
		<title>
			Global.box
		</title>
	</rect>
	<g>
		<marker id="j.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="j.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 171.28779116743544 143.32329886578188 L 62.521225288793005 90.13350059726976" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			j.outline
		</title>
	</g>
	<g>
		<marker id="j.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="j.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 171.28779116743544 143.32329886578188 L 62.521225288793005 90.13350059726976" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			j.stroke
		</title>
	</g>
	<g>
		<marker id="i.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="i.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 171.28779116743544 143.32329886578188 L 117.06541974841754 31.414824169199107" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			i.outline
		</title>
	</g>
	<g>
		<marker id="i.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="i.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 171.28779116743544 143.32329886578188 L 117.06541974841754 31.414824169199107" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			i.stroke
		</title>
	</g>
	<g>
		<marker id="h.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="h.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 17.204995922577496 114.97648936029539 L 171.28779116743544 143.32329886578188" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			h.outline
		</title>
	</g>
	<g>
		<marker id="h.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="h.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 17.204995922577496 114.97648936029539 L 171.28779116743544 143.32329886578188" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			h.stroke
		</title>
	</g>
	<g>
		<marker id="g.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="g.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 117.06541974841754 31.414824169199107 L 17.204995922577496 114.97648936029539" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			g.outline
		</title>
	</g>
	<g>
		<marker id="g.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="g.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 117.06541974841754 31.414824169199107 L 17.204995922577496 114.97648936029539" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			g.stroke
		</title>
	</g>
	<g>
		<marker id="f.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="f.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 14.34058311067551 40.340010938977656 L 121.34936311055273 5.964125901676425" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			f.outline
		</title>
	</g>
	<g>
		<marker id="f.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="f.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 14.34058311067551 40.340010938977656 L 121.34936311055273 5.964125901676425" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			f.stroke
		</title>
	</g>
	<g>
		<marker id="e.outline-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<marker id="e.outline-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#ffe6e6" fill-opacity="1">
			</path>
		</marker>
		<path d="M 217.923600256671 82.30931119704456 L 14.34058311067551 40.340010938977656" stroke-opacity="1" stroke-width="6" stroke="#ffe6e6" stroke-linecap="butt">
		</path>
		<title>
			e.outline
		</title>
	</g>
	<g>
		<marker id="e.stroke-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<marker id="e.stroke-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000ff" fill-opacity="1">
			</path>
		</marker>
		<path d="M 217.923600256671 82.30931119704456 L 14.34058311067551 40.340010938977656" stroke-opacity="1" stroke-width="2" stroke="#0000ff" stroke-linecap="butt">
		</path>
		<title>
			e.stroke
		</title>
	</g>
	<circle fill="#000000" fill-opacity="1" cx="156.96611313247527" cy="5.8885495366826035" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			X.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="77.97974068329465" cy="166.42047946465038" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			z.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="10.281173366820184" cy="168.2935933644608" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			y.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="199.20639389131603" cy="40.323023185181285" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			x.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="62.521225288793005" cy="90.13350059726976" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			d.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="171.28779116743544" cy="143.32329886578188" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			c.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="17.204995922577496" cy="114.97648936029539" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			b.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="117.06541974841754" cy="31.414824169199107" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			a.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="121.34936311055273" cy="5.964125901676425" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			r.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="14.34058311067551" cy="40.340010938977656" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			q.icon
		</title>
	</circle>
	<circle fill="#000000" fill-opacity="1" cx="217.923600256671" cy="82.30931119704456" stroke="none" r="4" onmouseover="try { $('audio').currentTime=0; } catch(e) {} $('audio').play()">
		<title>
			p.icon
		</title>
	</circle>
</svg>
`,y0=`Point p, q, r
Segment e := MakeSegment(p,q)
Segment f := MakeSegment(q,r)

Point a, b, c, d
Segment g := MakeSegment(a,b)
Segment h := MakeSegment(b,c)
Segment i := MakeSegment(c,a)
Segment j := MakeSegment(c,d)

Point x, y, z
Point X
`,w0=`type Point
type Segment

constructor MakeSegment(Point p, Point q) -> Segment

`,C0=`-- define the size of the drawing
canvas {
   width = 240
   height = 180
}

-- define some colors re-used throughout
Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color lightPink = rgba(1,.9,.9,1)
}

Global {

   -- draw a box around the canvas (this box will 
   -- also be used to constrain shapes to the canvas)
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: Colors.lightPink
      strokeWidth: 1
   }

   -- some additional parameters to get consistent styling throughout
   scalar lineThickness = 1.5
   scalar fontSize = "6px"
   string fontFamily = "Linux Libertine"
}

forall Point p {

   vec2 p.c = (?,?)

   shape p.icon = Circle {
      center: p.c
      r: 4.
      fillColor: Colors.black
      onmouseover: "try { $('audio').currentTime=0; } catch(e) {} $('audio').play()"
   }
   -- keep the arrow on the canvas
   ensure contains( Global.box, p.icon)

   layer p.icon above Global.box
}

forall Segment s; Point a; Point b
where s := MakeSegment(a, b) {
   s.stroke = Line {
      start: a.c
      end: b.c
      strokeWidth: 2.
      strokeColor: rgba(0,0,1,1)
   }
   s.outline = Line {
      start: a.c
      end: b.c
      strokeWidth: 6.
      strokeColor: Colors.lightPink
   }

   layer s.stroke above s.outline
   layer s.outline above Global.box
}

`,L0={"penrose-sound-example.svg":m0,"sound-test.substance":y0,"sound.domain":w0,"sound.style":C0},A0=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 700">
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="151.91237570890337"
    cy="418.2396102075062"
    stroke="none"
    r="7.693592674327508"
  >
    <title>\`P3\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="151.70908154886214"
    cy="375.50937549979625"
    stroke="none"
    r="7.693595683615132"
  >
    <title>\`P2\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="7.66175077014708"
    cy="495.9277324217652"
    stroke="none"
    r="7.677545068106613"
  >
    <title>\`P9\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="183.5999546288961"
    cy="506.4530254156093"
    stroke="none"
    r="7.693590506776686"
  >
    <title>\`P4\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="16.05037452639573"
    cy="402.19862490809135"
    stroke="none"
    r="7.693589001330443"
  >
    <title>\`P10\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="73.9834392307825"
    cy="496.34935585622543"
    stroke="none"
    r="7.6935871423983775"
  >
    <title>\`P8\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="109.36042596686434"
    cy="391.72706592280224"
    stroke="none"
    r="7.693586883204146"
  >
    <title>\`P1\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="7.682583319506193"
    cy="562.6070537729172"
    stroke="none"
    r="7.6877921604031085"
  >
    <title>\`P7\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="167.98534976303642"
    cy="554.0633609063029"
    stroke="none"
    r="7.693591360915477"
  >
    <title>\`P5\`.circle1</title>
  </circle>
  <circle
    fill="#339933"
    fill-opacity="0.3"
    cx="74.38869157234552"
    cy="562.7821634515711"
    stroke="none"
    r="7.693588137647046"
  >
    <title>\`P6\`.circle1</title>
  </circle>
  <g
    transform="rotate(0, 144.21878303457586, 410.54601753317866)translate(144.21878303457586, 410.54601753317866)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387185348655017"
      height="15.387185348655017"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="427.83060826463463"
    cy="418.2396102075062"
    stroke="none"
    r="28.54473292094079"
  >
    <title>\`P3\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="427.62730898171094"
    cy="375.50937549979625"
    stroke="none"
    r="28.49863920021401"
  >
    <title>\`P2\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="283.5639378530473"
    cy="495.9277324217652"
    stroke="none"
    r="28.29484011675723"
  >
    <title>\`P9\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="459.5589950296462"
    cy="506.4530254156093"
    stroke="none"
    r="28.53894017408397"
  >
    <title>\`P4\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="291.9686114679162"
    cy="402.19862490809135"
    stroke="none"
    r="28.498640886545655"
  >
    <title>\`P10\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="349.90166922673893"
    cy="496.34935585622543"
    stroke="none"
    r="28.213096376892757"
  >
    <title>\`P8\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="385.27865782399886"
    cy="391.72706592280224"
    stroke="none"
    r="28.544744265846216"
  >
    <title>\`P1\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="283.5950091805091"
    cy="562.6070537729172"
    stroke="none"
    r="28.419118718389175"
  >
    <title>\`P7\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="443.9035780218949"
    cy="554.0633609063029"
    stroke="none"
    r="28.53894079452196"
  >
    <title>\`P5\`.circle2</title>
  </circle>
  <circle
    fill="#ff3333"
    fill-opacity="0.3"
    cx="350.3069193132663"
    cy="562.7821634515711"
    stroke="none"
    r="28.337373890675416"
  >
    <title>\`P6\`.circle2</title>
  </circle>
  <g
    transform="rotate(0, 399.2858753436939, 389.69487728656543)translate(399.2858753436939, 389.69487728656543)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="57.08946584188158"
      height="57.08946584188158"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="717.9676036501098"
    cy="418.2396102075062"
    stroke="none"
    r="50.390545779037524"
  >
    <title>\`P3\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="717.7250737469469"
    cy="375.50937549979625"
    stroke="none"
    r="50.372812442978535"
  >
    <title>\`P2\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="573.7783097234297"
    cy="495.9277324217652"
    stroke="none"
    r="50.60382511147532"
  >
    <title>\`P9\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="749.7100875543792"
    cy="506.4530254156093"
    stroke="none"
    r="50.31044896769036"
  >
    <title>\`P4\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="582.0665476059057"
    cy="402.19862490809135"
    stroke="none"
    r="50.41624714946476"
  >
    <title>\`P10\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="639.8313528581905"
    cy="496.34935585622543"
    stroke="none"
    r="50.1237940506439"
  >
    <title>\`P8\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="675.3372608881709"
    cy="391.72706592280224"
    stroke="none"
    r="50.390808692435655"
  >
    <title>\`P1\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="573.7271963170124"
    cy="562.6070537729172"
    stroke="none"
    r="50.42156556936715"
  >
    <title>\`P7\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="733.9892343601778"
    cy="554.0633609063029"
    stroke="none"
    r="50.37607887136012"
  >
    <title>\`P5\`.circle3</title>
  </circle>
  <circle
    fill="#3333ff"
    fill-opacity="0.3"
    cx="640.3594959416766"
    cy="562.7821634515711"
    stroke="none"
    r="50.59379277464286"
  >
    <title>\`P6\`.circle3</title>
  </circle>
  <g
    transform="rotate(0, 667.5770578710723, 367.84906442846864)translate(667.5770578710723, 367.84906442846864)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.78109155807505"
      height="100.78109155807505"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 101.6668390836602, 384.0334790395981)translate(101.6668390836602, 384.0334790395981)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387173766408292"
      height="15.387173766408292"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 356.73391355815266, 363.18232165695605)translate(356.73391355815266, 363.18232165695605)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="57.08948853169243"
      height="57.08948853169243"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 624.9464521957352, 341.3362572303666)translate(624.9464521957352, 341.3362572303666)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.78161738487131"
      height="100.78161738487131"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 717.9676036501098 418.2396102075062 L 675.3372608881709 391.72706592280224"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P3P1\`.line</title>
  </g>
  <g
    transform="rotate(0, 66.28985208838412, 488.65576871382706)translate(66.28985208838412, 488.65576871382706)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387174284796755"
      height="15.387174284796755"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 321.6885728498462, 468.1362594793327)translate(321.6885728498462, 468.1362594793327)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.426192753785514"
      height="56.426192753785514"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 589.7075588075465, 446.22556180558155)translate(589.7075588075465, 446.22556180558155)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.2475881012878"
      height="100.2475881012878"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, -0.015794297959533132, 488.2501873536586)translate(-0.015794297959533132, 488.2501873536586)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.355090136213226"
      height="15.355090136213226"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 255.2690977362901, 467.632892305008)translate(255.2690977362901, 467.632892305008)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.58968023351446"
      height="56.58968023351446"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 523.1744846119544, 445.3239073102899)translate(523.1744846119544, 445.3239073102899)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="101.20765022295063"
      height="101.20765022295063"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 639.8313528581905 496.34935585622543 L 573.7783097234297 495.9277324217652"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P8P9\`.line</title>
  </g>
  <g
    transform="rotate(0, 8.356785525065286, 394.5050359067609)translate(8.356785525065286, 394.5050359067609)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387178002660885"
      height="15.387178002660885"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 263.46997058137055, 373.6999840215457)translate(263.46997058137055, 373.6999840215457)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.99728177309131"
      height="56.99728177309131"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 531.650300456441, 351.7823777586266)translate(531.650300456441, 351.7823777586266)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.83249429892952"
      height="100.83249429892952"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 582.0665476059057 402.19862490809135 L 675.3372608881709 391.72706592280224"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P10P1\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="151.91237570890337"
    cy="418.2396102075062"
    stroke="none"
    r="3"
  >
    <title>\`P3\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="427.83060826463463"
    cy="418.2396102075062"
    stroke="none"
    r="3"
  >
    <title>\`P3\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="717.9676036501098"
    cy="418.2396102075062"
    stroke="none"
    r="3"
  >
    <title>\`P3\`.dot3</title>
  </circle>
  <g
    transform="rotate(0, -0.005208840896915312, 554.9192616125141)translate(-0.005208840896915312, 554.9192616125141)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.375584320806217"
      height="15.375584320806217"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 255.17589046211995, 534.187935054528)translate(255.17589046211995, 534.187935054528)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.83823743677835"
      height="56.83823743677835"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 523.3056307476452, 512.18548820355)translate(523.3056307476452, 512.18548820355)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.8431311387343"
      height="100.8431311387343"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 573.7271963170124 562.6070537729172 L 573.7783097234297 495.9277324217652"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P7P9\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="167.98534976303642"
    cy="554.0633609063029"
    stroke="none"
    r="3"
  >
    <title>\`P5\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="443.9035780218949"
    cy="554.0633609063029"
    stroke="none"
    r="3"
  >
    <title>\`P5\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="733.9892343601778"
    cy="554.0633609063029"
    stroke="none"
    r="3"
  >
    <title>\`P5\`.dot3</title>
  </circle>
  <g>
    <path
      d="M 573.7783097234297 495.9277324217652 L 582.0665476059057 402.19862490809135"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P9P10\`.line</title>
  </g>
  <g>
    <path
      d="M 573.7271963170124 562.6070537729172 L 639.8313528581905 496.34935585622543"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P7P8\`.line</title>
  </g>
  <g>
    <path
      d="M 427.83060826463463 418.2396102075062 L 385.27865782399886 391.72706592280224"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C2P3P1\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="183.5999546288961"
    cy="506.4530254156093"
    stroke="none"
    r="3"
  >
    <title>\`P4\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="459.5589950296462"
    cy="506.4530254156093"
    stroke="none"
    r="3"
  >
    <title>\`P4\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="749.7100875543792"
    cy="506.4530254156093"
    stroke="none"
    r="3"
  >
    <title>\`P4\`.dot3</title>
  </circle>
  <g
    transform="rotate(0, 144.015485865247, 367.81577981618113)translate(144.015485865247, 367.81577981618113)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387191367230264"
      height="15.387191367230264"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 399.1286697814969, 347.0107362995822)translate(399.1286697814969, 347.0107362995822)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.99727840042802"
      height="56.99727840042802"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 667.3522613039684, 325.1365630568177)translate(667.3522613039684, 325.1365630568177)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.74562488595707"
      height="100.74562488595707"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 385.27865782399886 391.72706592280224 L 427.62730898171094 375.50937549979625"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C2P1P2\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="16.05037452639573"
    cy="402.19862490809135"
    stroke="none"
    r="3"
  >
    <title>\`P10\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="291.9686114679162"
    cy="402.19862490809135"
    stroke="none"
    r="3"
  >
    <title>\`P10\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="582.0665476059057"
    cy="402.19862490809135"
    stroke="none"
    r="3"
  >
    <title>\`P10\`.dot3</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="73.9834392307825"
    cy="496.34935585622543"
    stroke="none"
    r="3"
  >
    <title>\`P8\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="349.90166922673893"
    cy="496.34935585622543"
    stroke="none"
    r="3"
  >
    <title>\`P8\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="639.8313528581905"
    cy="496.34935585622543"
    stroke="none"
    r="3"
  >
    <title>\`P8\`.dot3</title>
  </circle>
  <g
    transform="rotate(0, 160.29175840212093, 546.3697695453875)translate(160.29175840212093, 546.3697695453875)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387182721830953"
      height="15.387182721830953"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 415.3646372273729, 525.524420111781)translate(415.3646372273729, 525.524420111781)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="57.07788158904392"
      height="57.07788158904392"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 683.6131554888176, 503.6872820349428)translate(683.6131554888176, 503.6872820349428)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.75215774272024"
      height="100.75215774272024"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 66.69510343469848, 555.0885753139241)translate(66.69510343469848, 555.0885753139241)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387176275294092"
      height="15.387176275294092"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 321.9695454225909, 534.4447895608957)translate(321.9695454225909, 534.4447895608957)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="56.67474778135083"
      height="56.67474778135083"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 589.7657031670337, 512.1883706769282)translate(589.7657031670337, 512.1883706769282)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="101.18758554928571"
      height="101.18758554928571"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 733.9892343601778 554.0633609063029 L 640.3594959416766 562.7821634515711"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P5P6\`.line</title>
  </g>
  <g>
    <path
      d="M 640.3594959416766 562.7821634515711 L 573.7783097234297 495.9277324217652"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P6P9\`.line</title>
  </g>
  <g
    transform="rotate(0, 175.90636412211944, 498.75943490883265)translate(175.90636412211944, 498.75943490883265)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="15.387181013553372"
      height="15.387181013553372"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 431.02005485556225, 477.91408524152536)translate(431.02005485556225, 477.91408524152536)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="57.07788034816794"
      height="57.07788034816794"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g
    transform="rotate(0, 699.3996385866889, 456.14257644791894)translate(699.3996385866889, 456.14257644791894)"
  >
    <!--?xml version="1.0" encoding="utf-8"?-->
    <!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
    <svg
      version="1.1"
      id="Layer_1"
      xmlns="http://www.w3.org/2000/svg"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      x="0px"
      y="0px"
      viewBox="0 0 92.9 92.9"
      style="enable-background: new 0 0 92.9 92.9"
      xml:space="preserve"
      width="100.62089793538073"
      height="100.62089793538073"
    >
      <style type="text/css">
        .st0 {
          opacity: 0.3;
          fill: url(#SVGID_1_);
        }
      </style>
      <radialGradient
        id="SVGID_1_"
        cx="46.4444"
        cy="46.4444"
        r="46.4444"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="9.860697e-02" style="stop-color: #ffffff"></stop>
        <stop offset="0.4259" style="stop-color: #fdfdfd"></stop>
        <stop offset="0.5475" style="stop-color: #f6f6f6"></stop>
        <stop offset="0.6348" style="stop-color: #eaeaea"></stop>
        <stop offset="0.7058" style="stop-color: #d9d9d9"></stop>
        <stop offset="0.7668" style="stop-color: #c3c3c3"></stop>
        <stop offset="0.8209" style="stop-color: #a8a8a8"></stop>
        <stop offset="0.87" style="stop-color: #878787"></stop>
        <stop offset="0.9152" style="stop-color: #616161"></stop>
        <stop offset="0.9572" style="stop-color: #353535"></stop>
        <stop offset="0.9947" style="stop-color: #070707"></stop>
        <stop offset="1" style="stop-color: #000000"></stop>
      </radialGradient>
      <circle class="st0" cx="46.4" cy="46.4" r="46.4"></circle>
    </svg>
  </g>
  <g>
    <path
      d="M 459.5589950296462 506.4530254156093 L 443.9035780218949 554.0633609063029"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C2P4P5\`.line</title>
  </g>
  <g>
    <path
      d="M 717.7250737469469 375.50937549979625 L 717.9676036501098 418.2396102075062"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P2P3\`.line</title>
  </g>
  <g>
    <path
      d="M 675.3372608881709 391.72706592280224 L 717.7250737469469 375.50937549979625"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P1P2\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="7.682583319506193"
    cy="562.6070537729172"
    stroke="none"
    r="3"
  >
    <title>\`P7\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="283.5950091805091"
    cy="562.6070537729172"
    stroke="none"
    r="3"
  >
    <title>\`P7\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="573.7271963170124"
    cy="562.6070537729172"
    stroke="none"
    r="3"
  >
    <title>\`P7\`.dot3</title>
  </circle>
  <g>
    <path
      d="M 427.62730898171094 375.50937549979625 L 427.83060826463463 418.2396102075062"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C2P2P3\`.line</title>
  </g>
  <g>
    <path
      d="M 717.9676036501098 418.2396102075062 L 749.7100875543792 506.4530254156093"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P3P4\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="151.70908154886214"
    cy="375.50937549979625"
    stroke="none"
    r="3"
  >
    <title>\`P2\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="427.62730898171094"
    cy="375.50937549979625"
    stroke="none"
    r="3"
  >
    <title>\`P2\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="717.7250737469469"
    cy="375.50937549979625"
    stroke="none"
    r="3"
  >
    <title>\`P2\`.dot3</title>
  </circle>
  <g>
    <path
      d="M 640.3594959416766 562.7821634515711 L 573.7271963170124 562.6070537729172"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P6P7\`.line</title>
  </g>
  <g>
    <path
      d="M 749.7100875543792 506.4530254156093 L 733.9892343601778 554.0633609063029"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P4P5\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="109.36042596686434"
    cy="391.72706592280224"
    stroke="none"
    r="3"
  >
    <title>\`P1\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="385.27865782399886"
    cy="391.72706592280224"
    stroke="none"
    r="3"
  >
    <title>\`P1\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="675.3372608881709"
    cy="391.72706592280224"
    stroke="none"
    r="3"
  >
    <title>\`P1\`.dot3</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="7.66175077014708"
    cy="495.9277324217652"
    stroke="none"
    r="3"
  >
    <title>\`P9\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="283.5639378530473"
    cy="495.9277324217652"
    stroke="none"
    r="3"
  >
    <title>\`P9\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="573.7783097234297"
    cy="495.9277324217652"
    stroke="none"
    r="3"
  >
    <title>\`P9\`.dot3</title>
  </circle>
  <g>
    <path
      d="M 640.3594959416766 562.7821634515711 L 639.8313528581905 496.34935585622543"
      stroke-opacity="1"
      stroke-width="3"
      stroke="#000000"
      stroke-linecap="butt"
    ></path>
    <title>\`C3P6P8\`.line</title>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="74.38869157234552"
    cy="562.7821634515711"
    stroke="none"
    r="3"
  >
    <title>\`P6\`.dot1</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="350.3069193132663"
    cy="562.7821634515711"
    stroke="none"
    r="3"
  >
    <title>\`P6\`.dot2</title>
  </circle>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="640.3594959416766"
    cy="562.7821634515711"
    stroke="none"
    r="3"
  >
    <title>\`P6\`.dot3</title>
  </circle>
  <g>
    <marker
      id="\`F\`.axis-endArrowId"
      markerUnits="strokeWidth"
      markerWidth="6.97"
      markerHeight="5.68"
      viewBox="0 0 9.95 8.12"
      refX="2.36"
      refY="4.06"
      orient="auto-start-reverse"
    >
      <path
        d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z"
        fill="#000000"
        fill-opacity="0.5"
      ></path>
    </marker>
    <path
      d="M 28.862413264629367 645.0774249341332 L 760.5115867353707 645.0774249341332"
      stroke-opacity="0.5"
      stroke-width="2"
      stroke="#000000"
      stroke-linecap="butt"
      marker-end="url(#\`F\`.axis-endArrowId)"
    ></path>
    <title>\`F\`.axis</title>
  </g>
  <g
    transform="rotate(0, 78.39325464859681, 229.68704412051477)translate(78.39325464859681, 229.68704412051477)"
    string="X \\oplus B_{\\varepsilon_{1}}"
  >
    <title>\`F\`.lab1</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="105.00219858860947"
      height="29.72"
      role="img"
      focusable="false"
      viewBox="0.7680349865818507 -20.26430772289037 100.63114969044825 27.73935592883411"
    >
      <path
        stroke="none"
        fill="#1a4d1a"
        stroke-width="0"
        d="M1.2406719014014511-0.08861942152867508H1.181592287049001Q0.7680349865818507-0.08861942152867508 0.7680349865818507-0.41355730046715033 0.7680349865818507-0.5317165291720505 0.8566544081105257-0.8861942152867508 0.9748136368154259-1.299751515753901 1.063433058344101-1.3588311301063511T1.6246893946923764-1.4474505516350262Q4.165112811847728-1.5360699731637013 5.612563363482755-2.9835205247987275 5.907961435245005-3.278918596560978 9.039180995924857-6.705536229003081T12.140860749428485-10.191233475797635Q8.921021767219958-18.403299870788192 8.773322731338833-18.550998906669317 8.507464466752808-18.87593678560779 6.912314879236656-18.905476592784016H6.085200278302355Q5.907961435245005-19.082715435841365 5.907961435245005-19.141795050193817T5.967041049597455-19.703051386542093Q6.085200278302355-20.087068879833016 6.262439121359705-20.26430772289037H6.675996421826856Q7.680349865818506-20.205228108537916 10.250313090150083-20.205228108537916 11.22512672696551-20.205228108537916 12.05224132789981-20.205228108537916T13.381532650829936-20.23476791571414 13.972328794354437-20.23476791571414Q14.474505516350263-20.23476791571414 14.474505516350263-19.909830036775666 14.474505516350263-19.880290229599442 14.415425901997812-19.52581254348474 14.297266673292912-19.082715435841365 14.208647251764237-18.994096014312692T13.736010336944636-18.905476592784016Q12.820276314481662-18.816857171255343 12.140860749428485-18.403299870788192L14.415425901997812-12.672577278600535 15.981035682337739-14.415425901997812Q19.082715435841365-17.75342411291124 19.082715435841365-18.107901799025942 19.082715435841365-18.63961832819799 18.373760063611964-18.846396978431567 18.22606102773084-18.846396978431567 17.98974257032104-18.905476592784016 17.546645462677667-18.905476592784016 17.546645462677667-19.23041447172249 17.546645462677667-19.289494086074942 17.605725077030115-19.703051386542093 17.723884305735016-20.087068879833016 17.901123148792365-20.26430772289037H18.255600834907067Q18.28514064208329-20.26430772289037 18.994096014312692-20.26430772289037T20.589245601828843-20.205228108537916 21.80037769605407-20.175688301361692Q24.45896034191432-20.175688301361692 24.724818606500346-20.26430772289037H24.961137063910147Q25.16791571414372-20.05752907265679 25.16791571414372-19.93936984395189 25.108836099791272-19.200874664546266 24.813438028029022-18.905476592784016H24.34080111320942Q23.33644766921777-18.87593678560779 22.538872875459695-18.63961832819799T21.327740781234468-18.137441606202167 20.618785409005067-17.605725077030115L20.293847530066593-17.33986681244409Q20.293847530066593-17.369406619620314 17.487565848325215-14.267726866116687L14.917602623993638-11.431905377199085Q14.917602623993638-11.40236557002286 15.833336646456614-9.068720803101083T17.75342411291124-4.283272040552629 18.846396978431567-1.7428486233972764Q19.141795050193817-1.4769903588112512 20.64832521618129-1.4474505516350262 21.68221846734917-1.4474505516350262 21.68221846734917-1.181592287049001 21.68221846734917-1.1225126726965509 21.623138852996718-0.7680349865818507 21.504979624291817-0.29539807176225025 21.416360202763144-0.20677865023357517T21.002802902295993-0.1181592287049001Q20.914183480767317-0.1181592287049001 20.027989265480567-0.1181592287049001T17.39894642679654-0.14769903588112512Q15.597018189046814-0.14769903588112512 14.651744359407612-0.14769903588112512T13.617851108239737-0.1181592287049001Q13.115674386243912-0.1181592287049001 13.115674386243912-0.3840174932909253 13.115674386243912-0.41355730046715033 13.174754000596362-0.8271146009343007 13.23383361494881-1.1225126726965509 13.292913229301261-1.2406719014014511T13.440612265182386-1.388370937282576 13.706470529768412-1.4474505516350262 14.179107444588013-1.4769903588112512 14.947142431169864-1.6837690090448265Q15.449319153165689-1.9200874664546266 15.449319153165689-1.9791670808070767 15.419779345989463-1.9791670808070767 14.060948215883112-5.435324520425405L12.672577278600535-8.921021767219958Q7.1190935294702316-2.8949011032700525 6.971394493589106-2.5699632243315773 6.853235264884206-2.333644766921777 6.853235264884206-2.215485538216877 6.853235264884206-1.6542292018686013 7.7098896729947315-1.4769903588112512 7.739429480170957-1.4769903588112512 7.887128516052082-1.4769903588112512T8.064367359109433-1.4474505516350262Q8.152986780638107-1.4474505516350262 8.182526587814332-1.4474505516350262T8.271146009343006-1.4179107444588013 8.359765430871683-1.3292913229301262 8.389305238047907-1.1225126726965509Q8.389305238047907-0.8566544081105257 8.330225623695457-0.6498757578769505 8.241606202166782-0.26585826458602524 8.152986780638107-0.20677865023357517T7.7098896729947315-0.1181592287049001Q7.621270251466057-0.1181592287049001 7.178173143822681-0.1181592287049001T5.93750124242123-0.14769903588112512 4.1946526190239535-0.14769903588112512Q1.8905476592784016-0.14769903588112512 1.2406719014014511-0.08861942152867508ZM33.38589007056952-7.473571215584932Q33.38589007056952-11.727303448961335 36.33987078819203-14.504045323526487T43.07494682437133-17.310327005267865Q47.38775867210019-17.310327005267865 50.22358016101779-14.415425901997812T53.059401649935396-7.473571215584932Q53.059401649935396-3.337998210913428 50.194040353841565-0.5021767219958254T43.252185667428684 2.363184574098002Q39.087072855580956 2.363184574098002 36.251251366663354-0.5021767219958254T33.38589007056952-7.473571215584932ZM42.48415068084683-16.099194911042638Q40.82992147897823-16.010575489513965 39.146152469933405-15.124381274227213T36.10355233078223-12.436258821190735 34.56748235761852-8.300685816519232V-8.064367359109433H42.631849716727956V-16.099194911042638H42.48415068084683ZM51.84826955571017-8.300685816519232Q51.671030712652815-10.073074247092734 50.93253553324719-11.490984991551535T49.21922671702614-13.736010336944636 47.15144021469039-15.124381274227213 45.260892555411985-15.892416260809064 43.90206142530563-16.099194911042638H43.813442003776956V-8.064367359109433H51.84826955571017V-8.300685816519232ZM34.56748235761852-6.6464566146506305Q34.803800815028325-4.519590497962429 35.8376940661962-2.8949011032700525T38.200878640294206-0.4430971076433754 40.65268263592088 0.7680349865818507 42.54323029519928 1.152052479872776H42.631849716727956V-6.882775072060431H34.56748235761852V-6.6464566146506305ZM51.84826955571017-6.6464566146506305V-6.882775072060431H43.813442003776956V1.152052479872776H43.90206142530563Q44.40423814730146 1.152052479872776 45.201812941059536 0.9748136368154259T47.12190040751416 0.20677865023357517 49.16014710267369-1.181592287049001 50.90299572607096-3.426617632442103 51.84826955571017-6.6464566146506305ZM68.10107146406918-18.905476592784016Q67.3034966703111-18.905476592784016 67.15579763442997-18.93501639996024T67.00809859854886-19.259954278898718Q67.00809859854886-20.05752907265679 67.33303647748733-20.23476791571414 67.36257628466355-20.26430772289037 71.17321141039658-20.26430772289037 78.82402146903885-20.26430772289037 79.23757876950602-20.205228108537916 81.09858662160819-19.909830036775666 82.33925852300963-18.87593678560779T83.60947023158731-16.15827452539509Q83.60947023158731-14.267726866116687 81.89616141536627-12.761196700129211T77.96736706092834-10.722950004969684L77.67196898916609-10.63433058344101Q79.56251664844449-10.368472318854984 80.7736487426697-9.275499453334659T82.01432064407116-6.557837193121956Q82.01432064407116-4.401431269257529 79.88745452738296-2.392724381274227T74.65890865719113-0.1181592287049001Q74.45213000695756-0.08861942152867508 68.42600934300765-0.08861942152867508 62.51804790776264-0.08861942152867508 62.429428486233974-0.14769903588112512 62.31126925752907-0.2363184574098002 62.31126925752907-0.3840174932909253 62.31126925752907-0.5907961435245005 62.37034887188152-0.7975747937580757 62.51804790776264-1.3588311301063511 62.665746943643775-1.4179107444588013 62.78390617234867-1.4474505516350262 63.10884405128715-1.4474505516350262H63.286082894344496Q64.08365768810258-1.4474505516350262 65.058471324918-1.5360699731637013 65.47202862538515-1.6246893946923764 65.61972766126627-1.8905476592784016 65.70834708279494-2.0087068879833017 67.74659377795447-10.102614054268958T69.784840473114-18.63961832819799Q69.784840473114-18.846396978431567 68.10107146406918-18.905476592784016ZM80.44871086373124-16.15827452539509Q80.44871086373124-17.04446874068184 80.00561375608787-17.81250372726369T78.55816320445284-18.816857171255343Q78.35138455421927-18.87593678560779 75.84050094424013-18.905476592784016 75.24970480071563-18.905476592784016 74.59982904283868-18.905476592784016T73.56593579167081-18.87593678560779H73.18191829837988Q72.7388211907365-18.846396978431567 72.6206619620316-18.58053871384554 72.56158234767915-18.46237948514064 71.6753881323924-15.035761852698538 71.6753881323924-14.947142431169864 71.64584832521618-14.858523009641187L70.73011430275321-11.136507305436835H73.12283868402743Q75.51556306530166-11.136507305436835 75.87004075141635-11.195586919789285 77.64242918198985-11.490984991551535 79.03080011927244-12.908895736010336T80.44871086373124-16.15827452539509ZM78.85356127621509-6.853235264884206Q78.85356127621509-8.152986780638107 78.17414571116191-9.009641188748633T76.40175728058841-10.013994632740284Q76.22451843753106-10.043534439916508 73.94995328496174-10.043534439916508 70.46425603816718-10.043534439916508 70.43471623099096-10.013994632740284 70.43471623099096-9.954915018387833 69.93253950899513-7.8575887088758565T68.89864625782725-3.692475897028128L68.36692972865521-1.6246893946923764Q68.36692972865521-1.5065301659874764 68.7214074147699-1.5065301659874764T71.11413179604413-1.4474505516350262Q73.74317463472816-1.4474505516350262 73.94995328496174-1.4769903588112512 75.78142132988768-1.6837690090448265 77.31749130305138-3.190299175032303T78.85356127621509-6.853235264884206ZM88.64098558791372 4.801813815724083Q87.26259910545672 4.801813815724083 86.26013620912434 4.112620574495577T85.23678866911837 2.1076947818308316Q85.23678866911837 0.7084236557002286 86.69871372626974-0.5028856773680549L86.90756016300566-0.6490781830831924 86.74048301361694-0.8370399761455123Q86.26013620912434-1.3591560679852897 86.26013620912434-2.0065800218666134 86.26013620912434-3.259658642282079 87.68029197892854-4.178582963920086T90.72944995527283-5.097507285558095H90.98006567935593Q92.1913750124242-5.097507285558095 93.13118397773582-4.450083331676771 93.6115307822284-4.115929032899314 93.6115307822284-3.84442866514263 93.6115307822284-3.6146975847331277 93.3817997018189-3.3640818606500345T92.83879896630553-3.09258149289335Q92.65083717324322-3.09258149289335 92.42110609283371-3.2805432859556705T91.7319128516052-3.65646687208031 90.58325744955769-3.84442866514263Q89.20487096710067-3.84442866514263 88.16063878342112-3.3431972169764435T87.09552195606797-2.090118596560978Q87.09552195606797-1.6933103667627472 87.45056089851903-1.3382714243116987 87.63852269158134-1.1503096312493788 87.70117662260212-1.1294249875757876T87.95179234668521-1.17119427492297Q88.59921630056654-1.463579286353245 89.6434484842461-1.463579286353245H89.74787170261405Q91.10537354139747-1.463579286353245 91.10537354139747-0.8161553324719213 91.10537354139747 0.04011505814531359 89.33017882914223 0.04011505814531359 88.36948522015705 0.04011505814531359 87.63852269158134-0.23138530961137063L87.42967625484545-0.08519280389623297Q86.09305905973561 0.812846874068184 86.09305905973561 2.0032715634628766 86.09305905973561 3.5278505516350265 88.87071666832323 3.5278505516350265 89.95671813934996 3.5278505516350265 90.64591138057847 3.319004114899115T91.52306641486929 2.83865731040652 91.87810535732034 2.379195149587516 92.23314429977138 2.149464069178014Q92.52552931120167 2.191233356525196 92.52552931120167 2.5045030116290627 92.52552931120167 2.6924648046913826 92.29579823079216 3.047503747142431T91.64837427691084 3.7993509193917103 90.41618030016896 4.509428804293807 88.64098558791372 4.801813815724083ZM98.23937985850313-1.059377892694563L98.04742909849915-0.9855506773084187Q97.84071289541794-0.9117234619222742 97.45681137540998-0.8378962465361295T96.60041567693072-0.7345381449955272H96.31987225846338V-1.413748526548057H96.60041567693072Q97.29439150156048-1.4432794127025146 97.88500922464962-1.6352301727064904T98.71187403697445-1.9896008065599842 99.12530644313686-2.31444055425902Q99.15483732929133-2.3587368834907068 99.3024917600636-2.3587368834907068 99.43538074775867-2.3587368834907068 99.5535042923765-2.2701442250273334V2.144723255064109L99.56826973545373 6.5743561782327795Q99.67162783699433 6.677714279773382 99.74545505238046 6.70724516592784T100.09982568623397 6.766306938236755 101.01528315702217 6.795837824391214H101.3991846770301V7.475048205943743H101.23676480318059Q100.92669049855878 7.430751876712056 98.88905935390119 7.430751876712056 96.88095909539805 7.430751876712056 96.57088479077626 7.475048205943743H96.39369947384951V6.795837824391214H96.77760099385746Q97.1024407415565 6.795837824391214 97.33868783079215 6.795837824391214T97.70782390772288 6.781072381313984 97.94407099695854 6.7367760520822975 98.06219454157637 6.70724516592784 98.15078720003976 6.633417950541695 98.23937985850313 6.5743561782327795V-1.059377892694563Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 354.31122762986206, 229.68704412051477)translate(354.31122762986206, 229.68704412051477)"
    string="X \\oplus B_{\\varepsilon_{2}}"
  >
    <title>\`F\`.lab2</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="105.00219858860947"
      height="29.72"
      role="img"
      focusable="false"
      viewBox="0.7680349865818507 -20.26430772289037 100.95598943814728 27.73935592883411"
    >
      <path
        stroke="none"
        fill="#801a1a"
        stroke-width="0"
        d="M1.2406719014014511-0.08861942152867508H1.181592287049001Q0.7680349865818507-0.08861942152867508 0.7680349865818507-0.41355730046715033 0.7680349865818507-0.5317165291720505 0.8566544081105257-0.8861942152867508 0.9748136368154259-1.299751515753901 1.063433058344101-1.3588311301063511T1.6246893946923764-1.4474505516350262Q4.165112811847728-1.5360699731637013 5.612563363482755-2.9835205247987275 5.907961435245005-3.278918596560978 9.039180995924857-6.705536229003081T12.140860749428485-10.191233475797635Q8.921021767219958-18.403299870788192 8.773322731338833-18.550998906669317 8.507464466752808-18.87593678560779 6.912314879236656-18.905476592784016H6.085200278302355Q5.907961435245005-19.082715435841365 5.907961435245005-19.141795050193817T5.967041049597455-19.703051386542093Q6.085200278302355-20.087068879833016 6.262439121359705-20.26430772289037H6.675996421826856Q7.680349865818506-20.205228108537916 10.250313090150083-20.205228108537916 11.22512672696551-20.205228108537916 12.05224132789981-20.205228108537916T13.381532650829936-20.23476791571414 13.972328794354437-20.23476791571414Q14.474505516350263-20.23476791571414 14.474505516350263-19.909830036775666 14.474505516350263-19.880290229599442 14.415425901997812-19.52581254348474 14.297266673292912-19.082715435841365 14.208647251764237-18.994096014312692T13.736010336944636-18.905476592784016Q12.820276314481662-18.816857171255343 12.140860749428485-18.403299870788192L14.415425901997812-12.672577278600535 15.981035682337739-14.415425901997812Q19.082715435841365-17.75342411291124 19.082715435841365-18.107901799025942 19.082715435841365-18.63961832819799 18.373760063611964-18.846396978431567 18.22606102773084-18.846396978431567 17.98974257032104-18.905476592784016 17.546645462677667-18.905476592784016 17.546645462677667-19.23041447172249 17.546645462677667-19.289494086074942 17.605725077030115-19.703051386542093 17.723884305735016-20.087068879833016 17.901123148792365-20.26430772289037H18.255600834907067Q18.28514064208329-20.26430772289037 18.994096014312692-20.26430772289037T20.589245601828843-20.205228108537916 21.80037769605407-20.175688301361692Q24.45896034191432-20.175688301361692 24.724818606500346-20.26430772289037H24.961137063910147Q25.16791571414372-20.05752907265679 25.16791571414372-19.93936984395189 25.108836099791272-19.200874664546266 24.813438028029022-18.905476592784016H24.34080111320942Q23.33644766921777-18.87593678560779 22.538872875459695-18.63961832819799T21.327740781234468-18.137441606202167 20.618785409005067-17.605725077030115L20.293847530066593-17.33986681244409Q20.293847530066593-17.369406619620314 17.487565848325215-14.267726866116687L14.917602623993638-11.431905377199085Q14.917602623993638-11.40236557002286 15.833336646456614-9.068720803101083T17.75342411291124-4.283272040552629 18.846396978431567-1.7428486233972764Q19.141795050193817-1.4769903588112512 20.64832521618129-1.4474505516350262 21.68221846734917-1.4474505516350262 21.68221846734917-1.181592287049001 21.68221846734917-1.1225126726965509 21.623138852996718-0.7680349865818507 21.504979624291817-0.29539807176225025 21.416360202763144-0.20677865023357517T21.002802902295993-0.1181592287049001Q20.914183480767317-0.1181592287049001 20.027989265480567-0.1181592287049001T17.39894642679654-0.14769903588112512Q15.597018189046814-0.14769903588112512 14.651744359407612-0.14769903588112512T13.617851108239737-0.1181592287049001Q13.115674386243912-0.1181592287049001 13.115674386243912-0.3840174932909253 13.115674386243912-0.41355730046715033 13.174754000596362-0.8271146009343007 13.23383361494881-1.1225126726965509 13.292913229301261-1.2406719014014511T13.440612265182386-1.388370937282576 13.706470529768412-1.4474505516350262 14.179107444588013-1.4769903588112512 14.947142431169864-1.6837690090448265Q15.449319153165689-1.9200874664546266 15.449319153165689-1.9791670808070767 15.419779345989463-1.9791670808070767 14.060948215883112-5.435324520425405L12.672577278600535-8.921021767219958Q7.1190935294702316-2.8949011032700525 6.971394493589106-2.5699632243315773 6.853235264884206-2.333644766921777 6.853235264884206-2.215485538216877 6.853235264884206-1.6542292018686013 7.7098896729947315-1.4769903588112512 7.739429480170957-1.4769903588112512 7.887128516052082-1.4769903588112512T8.064367359109433-1.4474505516350262Q8.152986780638107-1.4474505516350262 8.182526587814332-1.4474505516350262T8.271146009343006-1.4179107444588013 8.359765430871683-1.3292913229301262 8.389305238047907-1.1225126726965509Q8.389305238047907-0.8566544081105257 8.330225623695457-0.6498757578769505 8.241606202166782-0.26585826458602524 8.152986780638107-0.20677865023357517T7.7098896729947315-0.1181592287049001Q7.621270251466057-0.1181592287049001 7.178173143822681-0.1181592287049001T5.93750124242123-0.14769903588112512 4.1946526190239535-0.14769903588112512Q1.8905476592784016-0.14769903588112512 1.2406719014014511-0.08861942152867508ZM33.38589007056952-7.473571215584932Q33.38589007056952-11.727303448961335 36.33987078819203-14.504045323526487T43.07494682437133-17.310327005267865Q47.38775867210019-17.310327005267865 50.22358016101779-14.415425901997812T53.059401649935396-7.473571215584932Q53.059401649935396-3.337998210913428 50.194040353841565-0.5021767219958254T43.252185667428684 2.363184574098002Q39.087072855580956 2.363184574098002 36.251251366663354-0.5021767219958254T33.38589007056952-7.473571215584932ZM42.48415068084683-16.099194911042638Q40.82992147897823-16.010575489513965 39.146152469933405-15.124381274227213T36.10355233078223-12.436258821190735 34.56748235761852-8.300685816519232V-8.064367359109433H42.631849716727956V-16.099194911042638H42.48415068084683ZM51.84826955571017-8.300685816519232Q51.671030712652815-10.073074247092734 50.93253553324719-11.490984991551535T49.21922671702614-13.736010336944636 47.15144021469039-15.124381274227213 45.260892555411985-15.892416260809064 43.90206142530563-16.099194911042638H43.813442003776956V-8.064367359109433H51.84826955571017V-8.300685816519232ZM34.56748235761852-6.6464566146506305Q34.803800815028325-4.519590497962429 35.8376940661962-2.8949011032700525T38.200878640294206-0.4430971076433754 40.65268263592088 0.7680349865818507 42.54323029519928 1.152052479872776H42.631849716727956V-6.882775072060431H34.56748235761852V-6.6464566146506305ZM51.84826955571017-6.6464566146506305V-6.882775072060431H43.813442003776956V1.152052479872776H43.90206142530563Q44.40423814730146 1.152052479872776 45.201812941059536 0.9748136368154259T47.12190040751416 0.20677865023357517 49.16014710267369-1.181592287049001 50.90299572607096-3.426617632442103 51.84826955571017-6.6464566146506305ZM68.10107146406918-18.905476592784016Q67.3034966703111-18.905476592784016 67.15579763442997-18.93501639996024T67.00809859854886-19.259954278898718Q67.00809859854886-20.05752907265679 67.33303647748733-20.23476791571414 67.36257628466355-20.26430772289037 71.17321141039658-20.26430772289037 78.82402146903885-20.26430772289037 79.23757876950602-20.205228108537916 81.09858662160819-19.909830036775666 82.33925852300963-18.87593678560779T83.60947023158731-16.15827452539509Q83.60947023158731-14.267726866116687 81.89616141536627-12.761196700129211T77.96736706092834-10.722950004969684L77.67196898916609-10.63433058344101Q79.56251664844449-10.368472318854984 80.7736487426697-9.275499453334659T82.01432064407116-6.557837193121956Q82.01432064407116-4.401431269257529 79.88745452738296-2.392724381274227T74.65890865719113-0.1181592287049001Q74.45213000695756-0.08861942152867508 68.42600934300765-0.08861942152867508 62.51804790776264-0.08861942152867508 62.429428486233974-0.14769903588112512 62.31126925752907-0.2363184574098002 62.31126925752907-0.3840174932909253 62.31126925752907-0.5907961435245005 62.37034887188152-0.7975747937580757 62.51804790776264-1.3588311301063511 62.665746943643775-1.4179107444588013 62.78390617234867-1.4474505516350262 63.10884405128715-1.4474505516350262H63.286082894344496Q64.08365768810258-1.4474505516350262 65.058471324918-1.5360699731637013 65.47202862538515-1.6246893946923764 65.61972766126627-1.8905476592784016 65.70834708279494-2.0087068879833017 67.74659377795447-10.102614054268958T69.784840473114-18.63961832819799Q69.784840473114-18.846396978431567 68.10107146406918-18.905476592784016ZM80.44871086373124-16.15827452539509Q80.44871086373124-17.04446874068184 80.00561375608787-17.81250372726369T78.55816320445284-18.816857171255343Q78.35138455421927-18.87593678560779 75.84050094424013-18.905476592784016 75.24970480071563-18.905476592784016 74.59982904283868-18.905476592784016T73.56593579167081-18.87593678560779H73.18191829837988Q72.7388211907365-18.846396978431567 72.6206619620316-18.58053871384554 72.56158234767915-18.46237948514064 71.6753881323924-15.035761852698538 71.6753881323924-14.947142431169864 71.64584832521618-14.858523009641187L70.73011430275321-11.136507305436835H73.12283868402743Q75.51556306530166-11.136507305436835 75.87004075141635-11.195586919789285 77.64242918198985-11.490984991551535 79.03080011927244-12.908895736010336T80.44871086373124-16.15827452539509ZM78.85356127621509-6.853235264884206Q78.85356127621509-8.152986780638107 78.17414571116191-9.009641188748633T76.40175728058841-10.013994632740284Q76.22451843753106-10.043534439916508 73.94995328496174-10.043534439916508 70.46425603816718-10.043534439916508 70.43471623099096-10.013994632740284 70.43471623099096-9.954915018387833 69.93253950899513-7.8575887088758565T68.89864625782725-3.692475897028128L68.36692972865521-1.6246893946923764Q68.36692972865521-1.5065301659874764 68.7214074147699-1.5065301659874764T71.11413179604413-1.4474505516350262Q73.74317463472816-1.4474505516350262 73.94995328496174-1.4769903588112512 75.78142132988768-1.6837690090448265 77.31749130305138-3.190299175032303T78.85356127621509-6.853235264884206ZM88.64098558791372 4.801813815724083Q87.26259910545672 4.801813815724083 86.26013620912434 4.112620574495577T85.23678866911837 2.1076947818308316Q85.23678866911837 0.7084236557002286 86.69871372626974-0.5028856773680549L86.90756016300566-0.6490781830831924 86.74048301361694-0.8370399761455123Q86.26013620912434-1.3591560679852897 86.26013620912434-2.0065800218666134 86.26013620912434-3.259658642282079 87.68029197892854-4.178582963920086T90.72944995527283-5.097507285558095H90.98006567935593Q92.1913750124242-5.097507285558095 93.13118397773582-4.450083331676771 93.6115307822284-4.115929032899314 93.6115307822284-3.84442866514263 93.6115307822284-3.6146975847331277 93.3817997018189-3.3640818606500345T92.83879896630553-3.09258149289335Q92.65083717324322-3.09258149289335 92.42110609283371-3.2805432859556705T91.7319128516052-3.65646687208031 90.58325744955769-3.84442866514263Q89.20487096710067-3.84442866514263 88.16063878342112-3.3431972169764435T87.09552195606797-2.090118596560978Q87.09552195606797-1.6933103667627472 87.45056089851903-1.3382714243116987 87.63852269158134-1.1503096312493788 87.70117662260212-1.1294249875757876T87.95179234668521-1.17119427492297Q88.59921630056654-1.463579286353245 89.6434484842461-1.463579286353245H89.74787170261405Q91.10537354139747-1.463579286353245 91.10537354139747-0.8161553324719213 91.10537354139747 0.04011505814531359 89.33017882914223 0.04011505814531359 88.36948522015705 0.04011505814531359 87.63852269158134-0.23138530961137063L87.42967625484545-0.08519280389623297Q86.09305905973561 0.812846874068184 86.09305905973561 2.0032715634628766 86.09305905973561 3.5278505516350265 88.87071666832323 3.5278505516350265 89.95671813934996 3.5278505516350265 90.64591138057847 3.319004114899115T91.52306641486929 2.83865731040652 91.87810535732034 2.379195149587516 92.23314429977138 2.149464069178014Q92.52552931120167 2.191233356525196 92.52552931120167 2.5045030116290627 92.52552931120167 2.6924648046913826 92.29579823079216 3.047503747142431T91.64837427691084 3.7993509193917103 90.41618030016896 4.509428804293807 88.64098558791372 4.801813815724083ZM96.70377377847132 1.1406731258125433Q96.30510681538614 1.1406731258125433 96.06885972615048 0.8748951504224232T95.83261263691482 0.22521565502435142Q95.83261263691482-0.8231308034589007 96.61518112000795-1.5909338434748037T98.56421960620216-2.3587368834907068Q99.90787492623-2.3587368834907068 100.80856695394095-1.531872071165888T101.72402442472914 0.609117175032303Q101.72402442472914 1.2440312273531458 101.42871556318457 1.819883507365073T100.71997429547757 2.8239336366166383 99.53873884929926 3.916576424331577Q99.00718289851903 4.374305159725672 98.06219454157637 5.274997187436636L97.19103340001988 6.1018619997614545 98.31320707388926 6.116627442838684Q100.63138163701421 6.116627442838684 100.77903606778649 6.042800227452539 100.8823941693271 6.0132693412980815 101.13340670163998 4.728675793579167V4.68437946434748H101.72402442472914V4.728675793579167Q101.70925898165191 4.772972122810853 101.53207366472517 6.072331113606997T101.31059201856674 7.430751876712056V7.475048205943743H95.83261263691482V7.194504787476393 7.017319470549646Q95.83261263691482 6.913961369009044 95.92120529537819 6.795837824391214T96.36416858769506 6.279047316688201Q96.7923664369347 5.806553138216876 97.1024407415565 5.452182504363383 97.23532972925156 5.304528073591094 97.60446580618229 4.905861110505914T98.10649087080807 4.359539716648444 98.5346887200477 3.872280095099891 98.94812112621011 3.3702550304741075 99.28772631698637 2.9272917381572405 99.59780062160819 2.4547975596859155 99.81928226776661 2.0265997104462774 100.0112330277706 1.5541055319749528 100.1145911293112 1.1111422396580857 100.15888745854289 0.6238826181095318Q100.15888745854289-0.30634029575588906 99.65686239391711-0.9855506773084187T98.2246144154259-1.664761058860948Q97.73735479387734-1.664761058860948 97.36821871694661-1.413748526548057T96.85142820924361-0.926488904999503 96.70377377847132-0.6459454865321538Q96.70377377847132-0.631180043454925 96.77760099385746-0.631180043454925 97.04337896924758-0.631180043454925 97.32392238771493-0.4244638403737203T97.60446580618229 0.25474654117880924Q97.60446580618229 0.6238826181095318 97.36821871694661 0.8748951504224232T96.70377377847132 1.1406731258125433Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 644.7496082733619, 229.62204412051477)translate(644.7496082733619, 229.62204412051477)"
    string="X \\oplus B_{\\varepsilon_{3}}"
  >
    <title>\`F\`.lab3</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="104.32092222986924"
      height="29.85"
      role="img"
      focusable="false"
      viewBox="0.7630518139809263 -20.29424343722348 100.41832286997347 27.882109269786646"
    >
      <path
        stroke="none"
        fill="#1a1a80"
        stroke-width="0"
        d="M1.2326221610461117-0.24945924687837973H1.1739258676629634Q0.7630518139809263-0.24945924687837973 0.7630518139809263-0.5722888604856946 0.7630518139809263-0.689681447251991 0.8510962540556485-1.04185920755088 0.9684888408219448-1.4527332612329171 1.056533280896667-1.5114295546160654T1.6141480680365747-1.5994739946907877Q4.138088683511946-1.68751843476551 5.576147871399076-3.12557762265264 5.869629338314817-3.419059089568381 8.98053288762167-6.8234441057909745T12.062088290236948-10.286525415396717Q8.863140300855374-18.44531019565431 8.716399567397502-18.592050929112183 8.452266247173336-18.914880542719498 6.867466325828336-18.944228689411073H6.045718218464262Q5.869629338314817-19.120317569560516 5.869629338314817-19.179013862943663T5.928325631697965-19.73662865008357Q6.045718218464262-20.118154557074035 6.221807098613706-20.29424343722348H6.632681152295743Q7.630518139809262-20.235547143840332 10.183806901976208-20.235547143840332 11.152295742798152-20.235547143840332 11.974043850162227-20.235547143840332T13.294710451283061-20.264895290531907 13.881673385114542-20.264895290531907Q14.380591878871302-20.264895290531907 14.380591878871302-19.942065676924592 14.380591878871302-19.912717530233017 14.321895585488154-19.560539769934127 14.204502998721857-19.120317569560516 14.116458558647135-19.032273129485795T13.646888211581949-18.944228689411073Q12.737095664143153-18.856184249336348 12.062088290236948-18.44531019565431L14.321895585488154-12.75176973748894 15.877347360141579-14.483310392291811Q18.95890276275686-17.799650968439682 18.95890276275686-18.151828728738572 18.95890276275686-18.680095369186905 18.25454724215908-18.885532396027923 18.10780650870121-18.885532396027923 17.873021335168616-18.944228689411073 17.432799134795005-18.944228689411073 17.432799134795005-19.267058303018388 17.432799134795005-19.325754596401534 17.491495428178155-19.73662865008357 17.60888801494445-20.118154557074035 17.784976895093894-20.29424343722348H18.137154655392784Q18.16650280208436-20.29424343722348 18.870858322682135-20.29424343722348T20.455658244027138-20.235547143840332 21.658932258381675-20.206198997148757Q24.300265460623343-20.206198997148757 24.564398780847508-20.29424343722348H24.7991839543801Q25.00462098122112-20.08880641038246 25.00462098122112-19.971413823616164 24.945924687837973-19.237710156326813 24.65244322092223-18.944228689411073H24.182872873857047Q23.185035886343528-18.914880542719498 22.392635925671026-18.680095369186905T21.18936191131649-18.181176875430147 20.485006390718713-17.65291023498181L20.162176777111398-17.388776914757646Q20.162176777111398-17.41812506144922 17.374102841411858-14.336569658833941L14.820814079244913-11.519147576442828Q14.820814079244913-11.489799429751255 15.73060662668371-9.171295841116901T17.638236161636026-4.4168960770818995 18.724117589224267-1.8929554616065285Q19.017599056140007-1.6288221413823618 20.514354537410284-1.5994739946907877 21.54153967161538-1.5994739946907877 21.54153967161538-1.3353406744666207 21.54153967161538-1.2766443810834727 21.48284337823223-0.9244666207845836 21.365450791465932-0.45489627371939834 21.27740635139121-0.36685183364467605T20.866532297709174-0.2788073935699538Q20.778487857634452-0.2788073935699538 19.89804345688723-0.2788073935699538T17.286058401337137-0.3081555402615279Q15.495821453151116-0.3081555402615279 14.556680759020747-0.3081555402615279T13.529495624815652-0.2788073935699538Q13.030577131058894-0.2788073935699538 13.030577131058894-0.5429407137941206 13.030577131058894-0.5722888604856946 13.089273424442041-0.9831629141677318 13.14796971782519-1.2766443810834727 13.206666011208338-1.394036967849769T13.35340674466621-1.5407777013076394 13.617540064890376-1.5994739946907877 14.08711041195556-1.6288221413823618 14.850162225936486-1.8342591682233802Q15.349080719693246-2.069044341755973 15.349080719693246-2.127740635139121 15.319732573001673-2.127740635139121 13.969717825189264-5.561473798053289L12.590354930685283-9.024555107659031Q7.072903352669354-3.037533182577918 6.926162619211484-2.7147035689706027 6.808770032445188-2.47991839543801 6.808770032445188-2.362525808671714 6.808770032445188-1.8049110215318063 7.6598662865008365-1.6288221413823618 7.689214433192411-1.6288221413823618 7.8359551666502805-1.6288221413823618T8.012044046799724-1.5994739946907877Q8.100088486874448-1.5994739946907877 8.129436633566021-1.5994739946907877T8.217481073640744-1.5701258479992135 8.305525513715466-1.4820814079244913 8.33487366040704-1.2766443810834727Q8.33487366040704-1.012511060859306 8.276177367023893-0.8070740340182874 8.18813292694917-0.42554812702782424 8.100088486874448-0.36685183364467605T7.6598662865008365-0.2788073935699538Q7.571821846426114-0.2788073935699538 7.131599646052503-0.2788073935699538T5.898977485006391-0.3081555402615279 4.16743683020352-0.3081555402615279Q1.8782813882607414-0.3081555402615279 1.2326221610461117-0.24945924687837973ZM33.169275390817035-7.586495919771901Q33.169275390817035-11.81262904335857 36.10409005997444-14.571354832366533T42.795467505653335-17.35942876806607Q47.08029692262315-17.35942876806607 49.89771900501426-14.483310392291811T52.71514108740537-7.586495919771901Q52.71514108740537-3.477755382951529 49.868370858322685-0.660333300560417T42.97155638580278 2.1864369285222693Q38.83346770229083 2.1864369285222693 36.01604561989972-0.660333300560417T33.169275390817035-7.586495919771901ZM42.20850457182185-16.156154753711533Q40.5650083570937-16.06811031363681 38.892163995673975-15.187665912889589T35.869304886441846-12.516984563956347 34.34320125847999-8.408244027135975V-8.173458853603382H42.35524530527972V-16.156154753711533H42.20850457182185ZM51.51186707305084-8.408244027135975Q51.335778192901394-10.16913282863042 50.60207452561204-11.577843869825976T48.899882017500744-13.808303018385606 46.845511749090555-15.187665912889589 44.96723036082982-15.950717726870515 43.61721561301741-16.156154753711533H43.52917117294268V-8.173458853603382H51.51186707305084V-8.408244027135975ZM34.34320125847999-6.764747812407826Q34.57798643201259-4.651681250614493 35.60517156621768-3.037533182577918T37.95302330154361-0.6016370071772688 40.38891947694426 0.6016370071772688 42.267200865205 0.9831629141677318H42.35524530527972V-6.999532985940419H34.34320125847999V-6.764747812407826ZM51.51186707305084-6.764747812407826V-6.999532985940419H43.52917117294268V0.9831629141677318H43.61721561301741Q44.11613410677417 0.9831629141677318 44.908534067446666 0.8070740340182874T46.81616360239898 0.04402222003736113 48.841185724117594-1.3353406744666207 50.572726378920464-3.5657998230262513 51.51186707305084-6.764747812407826ZM67.6592173827549-18.944228689411073Q66.86681742208239-18.944228689411073 66.72007668862453-18.973576836102644T66.57333595516666-19.29640644970996Q66.57333595516666-20.08880641038246 66.89616556877397-20.264895290531907 66.92551371546554-20.29424343722348 70.71142463867861-20.29424343722348 78.31259463179629-20.29424343722348 78.72346868547832-20.235547143840332 80.5724019270475-19.942065676924592 81.8050240880936-18.914880542719498T83.06699439583129-16.214851047094683Q83.06699439583129-14.336569658833941 81.36480188771999-12.839814177563662T77.46149837774064-10.81479205584505L77.1680169108249-10.726747615770329Q79.04629829908565-10.462614295546162 80.24957231344018-9.37673286795792T81.48219447448629-6.676703372333105Q81.48219447448629-4.534288663848196 79.36912791269296-2.5386146888211583T74.17450594828435-0.2788073935699538Q73.96906892144332-0.24945924687837973 67.98204699636221-0.24945924687837973 62.1124176580474-0.24945924687837973 62.02437321797267-0.3081555402615279 61.90698063120638-0.39619998033625015 61.90698063120638-0.5429407137941206 61.90698063120638-0.7483777406351392 61.96567692458952-0.9538147674761578 62.1124176580474-1.5114295546160654 62.25915839150527-1.5701258479992135 62.37655097827156-1.5994739946907877 62.69938059187888-1.5994739946907877H62.87546947202832Q63.66786943270082-1.5994739946907877 64.63635827352277-1.68751843476551 65.04723232720481-1.7755628748402321 65.19397306066267-2.039696195064399 65.2820175007374-2.157088781830695 67.307039622456-10.198480975321994T69.33206174417462-18.680095369186905Q69.33206174417462-18.885532396027923 67.6592173827549-18.944228689411073ZM79.92674269983287-16.214851047094683Q79.92674269983287-17.095295447841906 79.48652049945926-17.858347261822832T78.04846131157213-18.856184249336348Q77.8430242847311-18.914880542719498 75.34843181594731-18.944228689411073 74.76146888211582-18.944228689411073 74.1158096549012-18.944228689411073T73.0886245206961-18.914880542719498H72.70709861370564Q72.26687641333203-18.885532396027923 72.14948382656573-18.621399075803758 72.09078753318258-18.50400648903746 71.21034313243536-15.099621472814867 71.21034313243536-15.011577032740144 71.1809949857438-14.923532592665422L70.271202438305-11.225666109527088H72.64840232032249Q75.02560220234-11.225666109527088 75.37777996263888-11.284362402910235 77.13866876413333-11.577843869825976 78.51803165863731-12.986554911021532T79.92674269983287-16.214851047094683ZM78.34194277848786-6.970184839248845Q78.34194277848786-8.261503293678105 77.66693540458166-9.112599547733753T75.90604660308722-10.110436535247272Q75.72995772293777-10.139784681938846 73.47015042768656-10.139784681938846 70.00706911808082-10.139784681938846 69.97772097138925-10.110436535247272 69.97772097138925-10.051740241864124 69.47880247763248-7.968021826762364T68.4516173434274-3.8299331432504182L67.92335070297906-1.7755628748402321Q67.92335070297906-1.6581702880739357 68.27552846327795-1.6581702880739357T70.65272834529546-1.5994739946907877Q73.26471340084555-1.5994739946907877 73.47015042768656-1.6288221413823618 75.28973552256416-1.8342591682233802 76.81583915052602-3.3310146494936586T78.34194277848786-6.970184839248845ZM88.0658642218071 4.609243830498476Q86.69642100088488 4.609243830498476 85.70046229475962 3.9245222200373617T84.68375444892341 1.932604807786845Q84.68375444892341 0.5424124471536722 86.1361942286894-0.6610376560810147L86.34368562579884-0.8062816340576148 86.1776925081113-0.9930238914561007Q85.70046229475962-1.5117523842296727 85.70046229475962-2.1549757152689017 85.70046229475962-3.3999240979254743 87.11140379510374-4.312886245206961T90.14077819290138-5.225848392488448H90.38976786943272Q91.59321797266739-5.225848392488448 92.52692925965982-4.582625061449218 93.00415947301151-4.250638826074133 93.00415947301151-3.9809000098318754 93.00415947301151-3.7526594730115037 92.77591893619113-3.503669796480189T92.23644130370663-3.2339309802379312Q92.04969904630813-3.2339309802379312 91.82145850948777-3.4206732376364175T91.13673689902664-3.7941577524333896 89.9955342149248-3.9809000098318754Q88.62609099400257-3.9809000098318754 87.58863400845541-3.482920656769246T86.53042788319732-2.2379722741126735Q86.53042788319732-1.8437386196047587 86.88316325828336-1.4910032445187298 87.06990551568184-1.304260987120244 87.13215293481467-1.283511847409301T87.38114261134598-1.325010126831187Q88.02436594238522-1.615498082784387 89.06182292793237-1.615498082784387H89.16556862648707Q90.51426270769836-1.615498082784387 90.51426270769836-0.9722747517451578 90.51426270769836-0.12156002359649987 88.75058583226823-0.12156002359649987 87.79612540556485-0.12156002359649987 87.06990551568184-0.3912988398387573L86.86241411857242-0.24605486186215714Q85.53446917707207 0.6461581457083866 85.53446917707207 1.8288591092321307 85.53446917707207 3.343546308130961 88.29410475862748 3.343546308130961 89.37306002359651 3.343546308130961 90.05778163405762 3.136054911021532T90.92924550191722 2.658824697669846 91.28198087700325 2.2023436240291026 91.63471625208928 1.9741030872087308Q91.92520420804247 2.0156013666306167 91.92520420804247 2.3268384622947598 91.92520420804247 2.5135807196932456 91.69696367122211 2.8663160947792745T91.05374034018288 3.613285124373218 89.82954109723724 4.3187558745452765 88.0658642218071 4.609243830498476ZM96.3403928979943 0.4730895713794121Q95.94431257005212 0.4730895713794121 95.72426794341757 0.22370566119358964T95.48955367500739-0.4217585769344214Q95.48955367500739-1.2285888745944353 96.19369648023795-1.8593834709468096T97.89537492621177-2.4901780672991842Q98.40881238835907-2.4901780672991842 98.5408391643398-2.475508425523547 99.62639265573691-2.299472724215908 100.19850868498673-1.698017411414807T100.7852943560122-0.3924192933831482Q100.7852943560122 0.37040207894995575 100.28652653564056 1.0892145259561499T98.9075802087307 2.1307590920263495L98.8635712834038 2.160098375577623Q98.8635712834038 2.174768017353259 98.99559805938452 2.204107300904533T99.42101767087799 2.336134076885262 99.97846405835219 2.644196554173631Q101.18137468395439 3.4216875682823717 101.18137468395439 4.697946402762757 101.18137468395439 5.8568481030380495 100.2718568938649 6.72235696780061T97.96872313508997 7.587865832563169Q96.79515179303904 7.587865832563169 95.94431257005212 6.957071236210796T95.0934733470652 5.358080282666404Q95.0934733470652 4.94733031294858 95.35752689902665 4.697946402762757T96.0176607789303 4.433892850801298Q96.42841074864812 4.433892850801298 96.69246430060959 4.697946402762757T96.95651785257104 5.358080282666404Q96.95651785257104 5.519446342198408 96.91250892724413 5.651473118179137T96.80982143481468 5.886187386589323 96.64845537528267 6.062223087896963 96.47241967397504 6.179580222102055 96.32572325621867 6.238258789204602 96.20836612201357 6.282267714531511L96.14968755491103 6.296937356307148Q96.89783928546849 6.957071236210796 97.96872313508997 6.957071236210796 98.77555343274997 6.957071236210796 99.18630340246781 6.179580222102055 99.43568731265363 5.695482043506047 99.43568731265363 4.697946402762757V4.404553567250025Q99.43568731265363 3.0109375985645466 98.49683023901288 2.541509061744175 98.27678561237833 2.453491211090355 97.60198209069905 2.4388215693147184L96.98585713612232 2.4241519275390817 96.94184821079541 2.394812643987809Q96.91250892724413 2.3508037186608988 96.91250892724413 2.160098375577623 96.91250892724413 1.8960448236161638 97.02986606144923 1.8960448236161638 97.44061603116705 1.8960448236161638 97.88070528443615 1.8226966147379806 98.37947310480779 1.7493484058597977 98.79022307452563 1.2065716601612428T99.20097304424344-0.43642821871005805V-0.5537853529151509Q99.20097304424344-1.389954934126438 98.68753558209616-1.7126870531904435 98.36480346303216-1.918062038049356 97.99806241864124-1.918062038049356 97.52863388182087-1.918062038049356 97.13255355387868-1.7566959785173533T96.57510716640448-1.4192942176777112 96.4137411068725-1.2432585163700718H96.4577500321994Q96.5017589575263-1.2285888745944353 96.57510716640448-1.2139192328187987T96.72180358416087-1.1405710239406157 96.89783928546849-1.0378835315111592 97.04453570322487-0.8765174719791565 97.16189283742996-0.6564728453446073 97.20590176275687-0.36308000983187494Q97.20590176275687-0.04034789076786944 97.00052677789795 0.20903601941795302T96.3403928979943 0.4730895713794121Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="28.862413264629367"
    cy="645.0774249341332"
    stroke="none"
    r="4"
  >
    <title>\`F\`.dot0</title>
  </circle>
  <g
    transform="rotate(0, 1.4474132646293674, 630.2474249341332)translate(1.4474132646293674, 630.2474249341332)"
    string="0"
  >
    <title>\`F\`.eps0</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="14.83"
      height="29.66"
      role="img"
      focusable="false"
      viewBox="1.1567399999999999 -19.75356 12.48686 20.40608"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M2.84736-17.3511Q4.508319999999999-19.75356 7.385339999999999-19.75356 8.80902-19.75356 10.2327-18.9824T12.54618-16.25368Q13.6436-13.7919 13.6436-9.4912 13.6436-4.8938999999999995 12.368219999999999-2.46178 11.77502-1.21606 10.73692-0.47456T8.92766 0.44489999999999996 7.415 0.65252Q6.64384 0.65252 5.87268 0.47456T4.06342-0.47456 2.43212-2.46178Q1.1567399999999999-4.8938999999999995 1.1567399999999999-9.4912 1.1567399999999999-14.65204 2.84736-17.3511ZM9.520859999999999-17.70702Q8.63106-18.65614 7.415-18.65614 6.16928-18.65614 5.2794799999999995-17.70702 4.53798-16.935859999999998 4.3007-15.571499999999999T4.06342-9.87678Q4.06342-5.1905 4.3007-3.7075T5.36846-1.36436Q6.1989399999999995-0.47456 7.415-0.47456 8.6014-0.47456 9.43188-1.36436 10.292019999999999-2.2541599999999997 10.49964-3.8558T10.73692-9.87678Q10.73692-14.17748 10.49964-15.541839999999999T9.520859999999999-17.70702Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="130.89435394290155"
    cy="645.0774249341332"
    stroke="none"
    r="4"
  >
    <title>\`F\`.dot1</title>
  </circle>
  <g
    transform="rotate(0, 784.2268067353706, 630.2474249341332)translate(784.2268067353706, 630.2474249341332)"
    string="\\varepsilon"
  >
    <title>\`F\`.eps</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="13.82156"
      height="29.66"
      role="img"
      focusable="false"
      viewBox="0.80082 -13.40632 11.893659999999999 14.05884"
    >
      <path
        stroke="none"
        fill="#000000"
        stroke-width="0"
        d="M5.6354 0.65252Q3.6778399999999998 0.65252 2.2541599999999997-0.32626T0.80082-3.1736199999999997Q0.80082-5.160839999999999 2.87702-6.88112L3.1736199999999997-7.08874 2.93634-7.3556799999999996Q2.2541599999999997-8.09718 2.2541599999999997-9.016639999999999 2.2541599999999997-10.79624 4.27104-12.10128T8.6014-13.40632H8.95732Q10.6776-13.40632 12.0123-12.48686 12.694479999999999-12.0123 12.694479999999999-11.626719999999999 12.694479999999999-11.30046 12.368219999999999-10.94454T11.597059999999999-10.558959999999999Q11.330119999999999-10.558959999999999 11.00386-10.825899999999999T10.025079999999999-11.359779999999999 8.39378-11.626719999999999Q6.43622-11.626719999999999 4.95322-10.91488T3.44056-9.13528Q3.44056-8.57174 3.9447799999999997-8.06752 4.21172-7.80058 4.3007-7.770919999999999T4.65662-7.83024Q5.57608-8.245479999999999 7.05908-8.245479999999999H7.20738Q9.13528-8.245479999999999 9.13528-7.32602 9.13528-6.10996 6.61418-6.10996 5.24982-6.10996 4.21172-6.49554L3.91512-6.28792Q2.01688-5.0125399999999996 2.01688-3.32192 2.01688-1.1567399999999999 5.96166-1.1567399999999999 7.503979999999999-1.1567399999999999 8.482759999999999-1.4533399999999999T9.72848-2.13552 10.2327-2.78804 10.73692-3.1143Q11.15216-3.05498 11.15216-2.61008 11.15216-2.34314 10.825899999999999-1.8389199999999999T9.90644-0.77116 8.1565 0.23728 5.6354 0.65252Z"
        fill-opacity="1"
      ></path>
    </svg>
  </g>
  <g
    transform="rotate(0, 137.50879594290154, 650.2474249341332)translate(137.50879594290154, 650.2474249341332)"
    string="\\varepsilon_1"
  >
    <title>\`F\`.eps1</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="26.771116"
      height="29.66"
      role="img"
      focusable="false"
      viewBox="0.80082 -13.40632 22.953547739999998 17.85532"
    >
      <path
        stroke="none"
        fill="#1a4d1a"
        stroke-width="0"
        d="M5.6354 0.65252Q3.6778399999999998 0.65252 2.2541599999999997-0.32626T0.80082-3.1736199999999997Q0.80082-5.160839999999999 2.87702-6.88112L3.1736199999999997-7.08874 2.93634-7.3556799999999996Q2.2541599999999997-8.09718 2.2541599999999997-9.016639999999999 2.2541599999999997-10.79624 4.27104-12.10128T8.6014-13.40632H8.95732Q10.6776-13.40632 12.0123-12.48686 12.694479999999999-12.0123 12.694479999999999-11.626719999999999 12.694479999999999-11.30046 12.368219999999999-10.94454T11.597059999999999-10.558959999999999Q11.330119999999999-10.558959999999999 11.00386-10.825899999999999T10.025079999999999-11.359779999999999 8.39378-11.626719999999999Q6.43622-11.626719999999999 4.95322-10.91488T3.44056-9.13528Q3.44056-8.57174 3.9447799999999997-8.06752 4.21172-7.80058 4.3007-7.770919999999999T4.65662-7.83024Q5.57608-8.245479999999999 7.05908-8.245479999999999H7.20738Q9.13528-8.245479999999999 9.13528-7.32602 9.13528-6.10996 6.61418-6.10996 5.24982-6.10996 4.21172-6.49554L3.91512-6.28792Q2.01688-5.0125399999999996 2.01688-3.32192 2.01688-1.1567399999999999 5.96166-1.1567399999999999 7.503979999999999-1.1567399999999999 8.482759999999999-1.4533399999999999T9.72848-2.13552 10.2327-2.78804 10.73692-3.1143Q11.15216-3.05498 11.15216-2.61008 11.15216-2.34314 10.825899999999999-1.8389199999999999T9.90644-0.77116 8.1565 0.23728 5.6354 0.65252ZM19.26686906-7.67144036L18.994263999999998-7.566592259999999Q18.70068932-7.461744159999999 18.1554792-7.3568960599999995T16.93924124-7.21010872H16.54081846V-8.174711239999999H16.93924124Q17.92481338-8.21665048 18.763598180000002-8.48925554T19.937896900000002-8.99252642 20.52504626-9.45385806Q20.566985499999998-9.51676692 20.7766817-9.51676692 20.96540828-9.51676692 21.13316524-9.3909492V-3.12103282L21.15413486 3.16985318Q21.3009222 3.31664052 21.4057703 3.35857976T21.90904118 3.4424582399999997 23.20915762 3.4843974799999997H23.75436774V4.449H23.523701919999997Q23.0833399 4.3860911399999996 20.18953234 4.3860911399999996 17.337664020000002 4.3860911399999996 16.897302 4.449H16.64566656V3.4843974799999997H17.19087668Q17.65220832 3.4843974799999997 17.98772224 3.4843974799999997T18.51196274 3.46342786 18.84747666 3.400519 19.01523362 3.35857976 19.14105134 3.2537316599999997 19.26686906 3.16985318V-7.67144036Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="406.8123269241668"
    cy="645.0774249341332"
    stroke="none"
    r="4"
  >
    <title>\`F\`.dot2</title>
  </circle>
  <g
    transform="rotate(0, 413.4267689241668, 650.2474249341332)translate(413.4267689241668, 650.2474249341332)"
    string="\\varepsilon_2"
  >
    <title>\`F\`.eps2</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="26.771116"
      height="29.66"
      role="img"
      focusable="false"
      viewBox="0.80082 -13.40632 23.41487938 17.85532"
    >
      <path
        stroke="none"
        fill="#801a1a"
        stroke-width="0"
        d="M5.6354 0.65252Q3.6778399999999998 0.65252 2.2541599999999997-0.32626T0.80082-3.1736199999999997Q0.80082-5.160839999999999 2.87702-6.88112L3.1736199999999997-7.08874 2.93634-7.3556799999999996Q2.2541599999999997-8.09718 2.2541599999999997-9.016639999999999 2.2541599999999997-10.79624 4.27104-12.10128T8.6014-13.40632H8.95732Q10.6776-13.40632 12.0123-12.48686 12.694479999999999-12.0123 12.694479999999999-11.626719999999999 12.694479999999999-11.30046 12.368219999999999-10.94454T11.597059999999999-10.558959999999999Q11.330119999999999-10.558959999999999 11.00386-10.825899999999999T10.025079999999999-11.359779999999999 8.39378-11.626719999999999Q6.43622-11.626719999999999 4.95322-10.91488T3.44056-9.13528Q3.44056-8.57174 3.9447799999999997-8.06752 4.21172-7.80058 4.3007-7.770919999999999T4.65662-7.83024Q5.57608-8.245479999999999 7.05908-8.245479999999999H7.20738Q9.13528-8.245479999999999 9.13528-7.32602 9.13528-6.10996 6.61418-6.10996 5.24982-6.10996 4.21172-6.49554L3.91512-6.28792Q2.01688-5.0125399999999996 2.01688-3.32192 2.01688-1.1567399999999999 5.96166-1.1567399999999999 7.503979999999999-1.1567399999999999 8.482759999999999-1.4533399999999999T9.72848-2.13552 10.2327-2.78804 10.73692-3.1143Q11.15216-3.05498 11.15216-2.61008 11.15216-2.34314 10.825899999999999-1.8389199999999999T9.90644-0.77116 8.1565 0.23728 5.6354 0.65252ZM17.08602858-4.54696698Q16.51984884-4.54696698 16.18433492-4.92442014T15.848821000000001-5.84708342Q15.848821000000001-7.33592644 16.96021086-8.42634668T19.7282007-9.51676692Q21.63643612-9.51676692 22.91558294-8.342468199999999T24.21569938-5.3018733Q24.21569938-4.40017964 23.79630698-3.58236446T22.78976522-2.1564303 21.11219562-0.6046784199999999Q20.3572893 0.0453798 19.01523362 1.3245266199999999L17.77802604 2.49882534 19.37171716 2.51979496Q22.6639475 2.51979496 22.873643700000002 2.41494686 23.02043104 2.37300762 23.37691458 0.5486506800000001V0.48574181999999994H24.21569938V0.5486506800000001Q24.194729759999998 0.61155954 23.943094319999997 2.4568860999999997T23.628550020000002 4.3860911399999996V4.449H15.848821000000001V4.05057722 3.79894178Q15.848821000000001 3.65215444 15.97463872 3.4843974799999997T16.60372732 2.75046078Q17.211846299999998 2.07943294 17.65220832 1.57616206 17.8409349 1.36646586 18.365175400000002 0.80028612T19.078142479999997 0.024410179999999997 19.68626146-0.6675872799999999 20.273410820000002-1.3805543599999999 20.75571208-2.00964296 21.1960741-2.6806707999999997 21.5106184-3.2887897799999997 21.783223460000002-3.95981762 21.930010799999998-4.58890622 21.99291966-5.28090368Q21.99291966-6.60198974 21.27995258-7.566592259999999T19.24589944-8.53119478Q18.55390198-8.53119478 18.02966148-8.174711239999999T17.29572478-7.482713779999999 17.08602858-7.0842909999999994Q17.08602858-7.06332138 17.19087668-7.06332138 17.568329839999997-7.06332138 17.966752619999998-6.7697467T18.365175400000002-5.80514418Q18.365175400000002-5.28090368 18.02966148-4.92442014T17.08602858-4.54696698Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
  <circle
    fill="#000000"
    fill-opacity="1"
    cx="696.9100693882965"
    cy="645.0774249341332"
    stroke="none"
    r="4"
  >
    <title>\`F\`.dot3</title>
  </circle>
  <g
    transform="rotate(0, 703.5245113882966, 650.2474249341332)translate(703.5245113882966, 650.2474249341332)"
    string="\\varepsilon_3"
  >
    <title>\`F\`.eps3</title>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="26.771116"
      height="29.66"
      role="img"
      focusable="false"
      viewBox="0.80082 -13.40632 23.58263634 18.31665164"
    >
      <path
        stroke="none"
        fill="#1a1a80"
        stroke-width="0"
        d="M5.6354 0.65252Q3.6778399999999998 0.65252 2.2541599999999997-0.32626T0.80082-3.1736199999999997Q0.80082-5.160839999999999 2.87702-6.88112L3.1736199999999997-7.08874 2.93634-7.3556799999999996Q2.2541599999999997-8.09718 2.2541599999999997-9.016639999999999 2.2541599999999997-10.79624 4.27104-12.10128T8.6014-13.40632H8.95732Q10.6776-13.40632 12.0123-12.48686 12.694479999999999-12.0123 12.694479999999999-11.626719999999999 12.694479999999999-11.30046 12.368219999999999-10.94454T11.597059999999999-10.558959999999999Q11.330119999999999-10.558959999999999 11.00386-10.825899999999999T10.025079999999999-11.359779999999999 8.39378-11.626719999999999Q6.43622-11.626719999999999 4.95322-10.91488T3.44056-9.13528Q3.44056-8.57174 3.9447799999999997-8.06752 4.21172-7.80058 4.3007-7.770919999999999T4.65662-7.83024Q5.57608-8.245479999999999 7.05908-8.245479999999999H7.20738Q9.13528-8.245479999999999 9.13528-7.32602 9.13528-6.10996 6.61418-6.10996 5.24982-6.10996 4.21172-6.49554L3.91512-6.28792Q2.01688-5.0125399999999996 2.01688-3.32192 2.01688-1.1567399999999999 5.96166-1.1567399999999999 7.503979999999999-1.1567399999999999 8.482759999999999-1.4533399999999999T9.72848-2.13552 10.2327-2.78804 10.73692-3.1143Q11.15216-3.05498 11.15216-2.61008 11.15216-2.34314 10.825899999999999-1.8389199999999999T9.90644-0.77116 8.1565 0.23728 5.6354 0.65252ZM17.46348174-5.25993406Q16.897302-5.25993406 16.582757700000002-5.6164176T16.24724378-6.539080879999999Q16.24724378-7.69240998 17.25378554-8.59410364T19.68626146-9.4957973Q20.420198159999998-9.4957973 20.608924740000003-9.474827679999999 22.16067662-9.22319224 22.9784918-8.36343782T23.8172766-6.49714164Q23.8172766-5.4067213999999995 23.104309519999997-4.3792100199999995T21.13316524-2.890367L21.07025638-2.84842776Q21.07025638-2.8274581399999996 21.258982959999997-2.7855189T21.86710194-2.59679232 22.6639475-2.1564303Q24.383456340000002-1.04504044 24.383456340000002 0.7793165 24.383456340000002 2.43591648 23.0833399 3.6731240599999997T19.79110956 4.91033164Q18.11353996 4.91033164 16.897302 4.00863798T15.681064039999999 1.7229494Q15.681064039999999 1.13580004 16.058517199999997 0.7793165T17.0021501 0.40186333999999996Q17.58929946 0.40186333999999996 17.966752619999998 0.7793165T18.34420578 1.7229494Q18.34420578 1.95361522 18.28129692 2.1423418T18.13450958 2.47785572 17.903843759999997 2.72949116 17.65220832 2.89724812 17.44251212 2.9811266 17.27475516 3.04403546L17.19087668 3.0650050799999997Q18.2603273 4.00863798 19.79110956 4.00863798 20.94443866 4.00863798 21.53158802 2.89724812 21.88807156 2.20525066 21.88807156 0.7793165V0.35992409999999997Q21.88807156-1.6321898 20.54601588-2.3032176399999997 20.23147158-2.42903536 19.26686906-2.4500049799999997L18.38614502-2.4709746 18.323236159999997-2.51291384Q18.28129692-2.5758227 18.28129692-2.84842776 18.28129692-3.22588092 18.44905388-3.22588092 19.03620324-3.22588092 19.66529184-3.3307290199999997 20.37825892-3.4355771199999996 20.96540828-4.21145306T21.55255764-6.5600505V-6.727807459999999Q21.55255764-7.9230757999999994 20.81862094-8.38440744 20.3572893-8.67798212 19.833048799999997-8.67798212 19.16202096-8.67798212 18.59584122-8.4473163T17.79899566-7.965015039999999 17.568329839999997-7.7133796H17.6312387Q17.69414756-7.69240998 17.79899566-7.67144036T18.00869186-7.566592259999999 18.2603273-7.41980492 18.4700235-7.189139099999999 18.63778046-6.8745948 18.70068932-6.455202399999999Q18.70068932-5.99387076 18.40711464-5.63738722T17.46348174-5.25993406Z"
        fill-opacity="0.8"
      ></path>
    </svg>
  </g>
</svg>
`,S0=`canvas {
  width = 800
  height = 700
}

forall Frames f {
  f.d = ?
  f.r1 = ?
  f.r2 = ?
  f.r3 = ?
  ensure lessThan(200, f.d)
  ensure lessThan(f.d, 400)
  ensure lessThan(3, f.r1)
  ensure lessThan(f.r1, f.r2)
  ensure lessThan(10, f.r2)
  ensure lessThan(f.r2, f.r3)
  ensure lessThan(30, f.r3)
}

forall Point p {
  p.x = ?
  p.y = ?
  p.d = ?
  p.r1 = ?
  p.r2 = ?
  p.r3 = ?

  p.dot1 = Circle {
    center: (p.x - p.d, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot2 = Circle {
    center: (p.x, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot3 = Circle {
    center: (p.x + p.d, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.circle1 = Circle {
    center: (p.x - p.d, p.y)
    r: p.r1
    fillColor: rgba(0.2, 0.2, 1.0, 0.3)
  }
  p.circle2 = Circle {
    center: (p.x, p.y)
    r: p.r2
    fillColor: rgba(0.2, 0.2, 1.0, 0.3)
  }
  p.circle3 = Circle {
    center: (p.x + p.d, p.y)
    r: p.r3
    fillColor: rgba(0.2, 0.2, 1.0, 0.3)
  }
  p.dot1 above p.circle1
  p.dot2 above p.circle2
  p.dot3 above p.circle3

}

forall Point p; Frames f {
  ensure equal(p.d, f.d)
  ensure equal(p.r1, f.r1)
  ensure equal(p.r2, f.r2)
  ensure equal(p.r3, f.r3)
}

forall Point p1; Point p2 {
  ensure lessThan(abs(p1.x - p2.x) + 150.0, p1.d)
}

forall Point p1; Point p2 
where ConnectedOnFrame1(p1, p2) {
  ensure overlapping(p1.circle1, p2.circle1, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame2(p1, p2) {
  ensure overlapping(p1.circle2, p2.circle2, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame3(p1, p2) {
  ensure overlapping(p1.circle3, p2.circle3, 7)
}

forall Point p1; Point p2 
where NotConnectedOnFrame1(p1, p2) {
  ensure disjoint(p1.circle1, p2.circle1, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame2(p1, p2) {
  ensure disjoint(p1.circle2, p2.circle2, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame3(p1, p2) {
  ensure disjoint(p1.circle3, p2.circle3, 10)
}

forall Connection c; Point p1; Point p2
where c := MakeConnection1(p1, p2) {
  c.line = Line {
    start: p1.dot1.center
    end: p2.dot1.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
}

forall Connection c; Point p1; Point p2
where c := MakeConnection2(p1, p2) {
  c.line = Line {
    start: p1.dot2.center
    end: p2.dot2.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
}

forall Connection c; Point p1; Point p2
where c := MakeConnection3(p1, p2) {
  c.line = Line {
    start: p1.dot3.center
    end: p2.dot3.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
}
`,P0=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 92.9 92.9" style="enable-background:new 0 0 92.9 92.9;" xml:space="preserve">
<style type="text/css">
	.st0{opacity:0.3;fill:url(#SVGID_1_);}
</style>
<radialGradient id="SVGID_1_" cx="46.4444" cy="46.4444" r="46.4444" gradientUnits="userSpaceOnUse">
	<stop  offset="9.860697e-02" style="stop-color:#FFFFFF"/>
	<stop  offset="0.4259" style="stop-color:#FDFDFD"/>
	<stop  offset="0.5475" style="stop-color:#F6F6F6"/>
	<stop  offset="0.6348" style="stop-color:#EAEAEA"/>
	<stop  offset="0.7058" style="stop-color:#D9D9D9"/>
	<stop  offset="0.7668" style="stop-color:#C3C3C3"/>
	<stop  offset="0.8209" style="stop-color:#A8A8A8"/>
	<stop  offset="0.87" style="stop-color:#878787"/>
	<stop  offset="0.9152" style="stop-color:#616161"/>
	<stop  offset="0.9572" style="stop-color:#353535"/>
	<stop  offset="0.9947" style="stop-color:#070707"/>
	<stop  offset="1" style="stop-color:#000000"/>
</radialGradient>
<circle class="st0" cx="46.4" cy="46.4" r="46.4"/>
</svg>
`,E0=`
type Point
type Frames
type Connection

constructor MakeConnection1(Point p1, Point p2) -> Connection
constructor MakeConnection2(Point p1, Point p2) -> Connection
constructor MakeConnection3(Point p1, Point p2) -> Connection

predicate ConnectedOnFrame1(Point p1, Point p2)
predicate ConnectedOnFrame2(Point p1, Point p2)
predicate ConnectedOnFrame3(Point p1, Point p2)

predicate NotConnectedOnFrame1(Point p1, Point p2)
predicate NotConnectedOnFrame2(Point p1, Point p2)
predicate NotConnectedOnFrame3(Point p1, Point p2)
`,T0=`canvas {
  width = 800
  height = 700
}

forall Frames f {
  f.d = ?
  f.r1 = ?
  f.r2 = ?
  f.r3 = ?
  f.y = ?
  f.ylab = ?
  ensure lessThan(200, f.d)
  ensure lessThan(f.d, 500)
  ensure lessThan(8, f.r1)
  ensure lessThan(f.r1, f.r2)
  ensure lessThan(15, f.r2)
  ensure lessThan(f.r2, f.r3)
  ensure lessThan(25, f.r3)
  encourage lessThan(f.ylab - f.y, 400)

  f.axis = Line {
    strokeColor: rgba(0.0, 0.0, 0.0, 0.5)
    strokeWidth: 2
    endArrowhead: "straight"
    start: (-1.5 * f.d, f.y)
    end: (1.5 * f.d, f.y)
    endArrowheadSize: 0.7
  }

  f.dot0 = Circle {
    center: (-1.5 * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot1 = Circle {
    center: ((-1.5 + 2.7 * f.r1 / f.r3) * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot2 = Circle {
    center: ((-1.5 + 2.7 * f.r2 / f.r3) * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot3 = Circle {
    center: (1.2 * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }

  f.eps0 = Equation {
    string: "0"
    center: (f.dot0.center[0] - 20, f.dot0.center[1])
    fontSize: "24p"
  }
  f.eps = Equation {
    string: "\\varepsilon"
    center: (1.5 * f.d + 20, f.dot0.center[1])
    fontSize: "24p"
  }
  f.eps1 = Equation {
    string: "\\varepsilon_1"
    center: (f.dot1.center[0] + 20, f.dot1.center[1] - 20)
    fillColor: rgba(0.1, 0.3, 0.1, 0.8)
    fontSize: "24p"
  }
  f.eps2 = Equation {
    string: "\\varepsilon_2"
    center: (f.dot2.center[0] + 20, f.dot2.center[1] - 20)
    fillColor: rgba(0.5, 0.1, 0.1, 0.8)
    fontSize: "24p"
  }
  f.eps3 = Equation {
    string: "\\varepsilon_3"
    center: (f.dot3.center[0] + 20, f.dot3.center[1] - 20)
    fillColor: rgba(0.1, 0.1, 0.5, 0.8)
    fontSize: "24p"
  }

  f.lab1 = Equation {
    string: "X \\oplus B_{\\varepsilon_{1}}"
    fillColor: rgba(0.1, 0.3, 0.1, 0.8)
    center: (f.dot1.center[0], f.ylab)
    fontSize: "24p"
  }
  f.lab2 = Equation {
    string: "X \\oplus B_{\\varepsilon_{2}}"
    fillColor: rgba(0.5, 0.1, 0.1, 0.8)
    center: (f.dot2.center[0], f.ylab)
    fontSize: "24p"
  }
  f.lab3 = Equation {
    string: "X \\oplus B_{\\varepsilon_{3}}"
    fillColor: rgba(0.1, 0.1, 0.5, 0.8)
    center: (f.dot3.center[0], f.ylab)
    fontSize: "24p"
  }
}

forall Point p {
  p.x = ?
  p.d12 = ?
  p.d23 = ?
  p.y = ?
  p.r1 = ?
  p.r2 = ?
  p.r3 = ?

  p.dot1 = Circle {
    center: (p.x - p.d12, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot2 = Circle {
    center: (p.x, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot3 = Circle {
    center: (p.x + p.d23, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.circle1 = Circle {
    center: (p.x - p.d12, p.y)
    r: p.r1
    fillColor: rgba(0.2, 0.6, 0.2, 0.3)
  }
  p.circle2 = Circle {
    center: (p.x, p.y)
    r: p.r2
    fillColor: rgba(1.0, 0.2, 0.2, 0.3)
  }
  p.circle3 = Circle {
    center: (p.x + p.d23, p.y)
    r: p.r3
    fillColor: rgba(0.2, 0.2, 1.0, 0.3)
  }

  p.shade1 = Image {
    center: p.circle1.center 
    width: p.circle1.r * 2.0
    height: p.circle1.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
   }
  p.shade2 = Image {
    center: p.circle2.center 
    width: p.circle2.r * 2.0
    height: p.circle2.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
  }
  p.shade3 = Image {
    center: p.circle3.center 
    width: p.circle3.r * 2.0
    height: p.circle3.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
  }

  p.circle1 below p.shade1
  p.circle2 below p.shade2
  p.circle3 below p.shade3
  
  p.dot1 above p.circle1
  p.dot2 above p.circle2
  p.dot3 above p.circle3

}

forall Point p; Frames f {
  ensure equal(p.d12, f.dot2.center[0] - f.dot1.center[0])
  ensure equal(p.d23, f.dot3.center[0] - f.dot2.center[0])
  ensure equal(p.r1, f.r1)
  ensure equal(p.r2, f.r2)
  ensure equal(p.r3, f.r3)
  ensure lessThan(f.y, p.y - p.r3 - 30)
  ensure lessThan(p.y + p.r3 + 30, f.ylab)
}

forall Point p1; Point p2 {
  p1.circle1 below p2.shade1
  p1.circle2 below p2.shade2
  p1.circle3 below p2.shade3
  p2.circle1 below p1.shade1
  p2.circle2 below p1.shade2
  p2.circle3 below p1.shade3
  ensure lessThan(abs(p1.x - p2.x) + 100.0, p1.d12)
  ensure lessThan(abs(p1.x - p2.x) + 100.0, p1.d23)
}

forall Point p1; Point p2 
where NotConnectedOnFrame1(p1, p2) {
  ensure disjoint(p1.circle1, p2.circle1, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame2(p1, p2) {
  ensure disjoint(p1.circle2, p2.circle2, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame3(p1, p2) {
  ensure disjoint(p1.circle3, p2.circle3, 10)
}

forall Point p1; Point p2 
where ConnectedOnFrame1(p1, p2) {
  ensure overlapping(p1.circle1, p2.circle1, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame2(p1, p2) {
  ensure overlapping(p1.circle2, p2.circle2, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame3(p1, p2) {
  ensure overlapping(p1.circle3, p2.circle3, 7)
}

forall Connection c; Point p1; Point p2
where c := MakeConnection1(p1, p2) {
  c.line = Line {
    start: p1.dot1.center
    end: p2.dot1.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}

forall Connection c; Point p1; Point p2
where c := MakeConnection2(p1, p2) {
  c.line = Line {
    start: p1.dot2.center
    end: p2.dot2.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}

forall Connection c; Point p1; Point p2
where c := MakeConnection3(p1, p2) {
  c.line = Line {
    start: p1.dot3.center
    end: p2.dot3.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}
`,O0=`
Frames F
Point P1, P2, P3, P4, P5, P6, P7, P8, P9, P10

ConnectedOnFrame2(P1, P2)
ConnectedOnFrame2(P2, P3)
ConnectedOnFrame2(P3, P1)
ConnectedOnFrame2(P4, P5)
Connection C2P1P2 := MakeConnection2(P1, P2)
Connection C2P2P3 := MakeConnection2(P2, P3)
Connection C2P3P1 := MakeConnection2(P3, P1)
Connection C2P4P5 := MakeConnection2(P4, P5)

ConnectedOnFrame3(P1, P2)
ConnectedOnFrame3(P2, P3)
ConnectedOnFrame3(P3, P1)
ConnectedOnFrame3(P3, P4)
ConnectedOnFrame3(P4, P5)
ConnectedOnFrame3(P5, P6)
ConnectedOnFrame3(P6, P7)
ConnectedOnFrame3(P6, P8)
ConnectedOnFrame3(P6, P9)
ConnectedOnFrame3(P7, P8)
ConnectedOnFrame3(P7, P9)
ConnectedOnFrame3(P8, P9)
ConnectedOnFrame3(P9, P10)
ConnectedOnFrame3(P10, P1)
Connection C3P1P2 := MakeConnection3(P1, P2)
Connection C3P2P3 := MakeConnection3(P2, P3)
Connection C3P3P1 := MakeConnection3(P3, P1)
Connection C3P3P4 := MakeConnection3(P3, P4)
Connection C3P4P5 := MakeConnection3(P4, P5)
Connection C3P5P6 := MakeConnection3(P5, P6)
Connection C3P6P7 := MakeConnection3(P6, P7)
Connection C3P6P8 := MakeConnection3(P6, P8)
Connection C3P6P9 := MakeConnection3(P6, P9)
Connection C3P7P8 := MakeConnection3(P7, P8)
Connection C3P7P9 := MakeConnection3(P7, P9)
Connection C3P8P9 := MakeConnection3(P8, P9)
Connection C3P9P10 := MakeConnection3(P9, P10)
Connection C3P10P1 := MakeConnection3(P10, P1)

-- This can be deleted in the future (after issue #775 is resolved)

NotConnectedOnFrame1(P1, P2)
NotConnectedOnFrame1(P1, P3)
NotConnectedOnFrame1(P1, P4)
NotConnectedOnFrame1(P1, P5)
NotConnectedOnFrame1(P1, P6)
NotConnectedOnFrame1(P1, P7)
NotConnectedOnFrame1(P1, P8)
NotConnectedOnFrame1(P1, P9)
NotConnectedOnFrame1(P1, P10)
NotConnectedOnFrame1(P2, P3)
NotConnectedOnFrame1(P2, P4)
NotConnectedOnFrame1(P2, P5)
NotConnectedOnFrame1(P2, P6)
NotConnectedOnFrame1(P2, P7)
NotConnectedOnFrame1(P2, P8)
NotConnectedOnFrame1(P2, P9)
NotConnectedOnFrame1(P2, P10)
NotConnectedOnFrame1(P3, P4)
NotConnectedOnFrame1(P3, P5)
NotConnectedOnFrame1(P3, P6)
NotConnectedOnFrame1(P3, P7)
NotConnectedOnFrame1(P3, P8)
NotConnectedOnFrame1(P3, P9)
NotConnectedOnFrame1(P3, P10)
NotConnectedOnFrame1(P4, P5)
NotConnectedOnFrame1(P4, P6)
NotConnectedOnFrame1(P4, P7)
NotConnectedOnFrame1(P4, P8)
NotConnectedOnFrame1(P4, P9)
NotConnectedOnFrame1(P4, P10)
NotConnectedOnFrame1(P5, P6)
NotConnectedOnFrame1(P5, P7)
NotConnectedOnFrame1(P5, P8)
NotConnectedOnFrame1(P5, P9)
NotConnectedOnFrame1(P5, P10)
NotConnectedOnFrame1(P6, P7)
NotConnectedOnFrame1(P6, P8)
NotConnectedOnFrame1(P6, P9)
NotConnectedOnFrame1(P6, P10)
NotConnectedOnFrame1(P7, P8)
NotConnectedOnFrame1(P7, P9)
NotConnectedOnFrame1(P7, P10)
NotConnectedOnFrame1(P8, P9)
NotConnectedOnFrame1(P8, P10)
NotConnectedOnFrame1(P9, P10)

NotConnectedOnFrame2(P1, P4)
NotConnectedOnFrame2(P1, P5)
NotConnectedOnFrame2(P1, P6)
NotConnectedOnFrame2(P1, P7)
NotConnectedOnFrame2(P1, P8)
NotConnectedOnFrame2(P1, P9)
NotConnectedOnFrame2(P1, P10)
NotConnectedOnFrame2(P2, P4)
NotConnectedOnFrame2(P2, P5)
NotConnectedOnFrame2(P2, P6)
NotConnectedOnFrame2(P2, P7)
NotConnectedOnFrame2(P2, P8)
NotConnectedOnFrame2(P2, P9)
NotConnectedOnFrame2(P2, P10)
NotConnectedOnFrame2(P3, P4)
NotConnectedOnFrame2(P3, P5)
NotConnectedOnFrame2(P3, P6)
NotConnectedOnFrame2(P3, P7)
NotConnectedOnFrame2(P3, P8)
NotConnectedOnFrame2(P3, P9)
NotConnectedOnFrame2(P3, P10)
NotConnectedOnFrame2(P4, P6)
NotConnectedOnFrame2(P4, P7)
NotConnectedOnFrame2(P4, P8)
NotConnectedOnFrame2(P4, P9)
NotConnectedOnFrame2(P4, P10)
NotConnectedOnFrame2(P5, P6)
NotConnectedOnFrame2(P5, P7)
NotConnectedOnFrame2(P5, P8)
NotConnectedOnFrame2(P5, P9)
NotConnectedOnFrame2(P5, P10)
NotConnectedOnFrame2(P6, P7)
NotConnectedOnFrame2(P6, P8)
NotConnectedOnFrame2(P6, P9)
NotConnectedOnFrame2(P6, P10)
NotConnectedOnFrame2(P7, P8)
NotConnectedOnFrame2(P7, P9)
NotConnectedOnFrame2(P7, P10)
NotConnectedOnFrame2(P8, P9)
NotConnectedOnFrame2(P8, P10)
NotConnectedOnFrame2(P9, P10)

NotConnectedOnFrame3(P1, P4)
NotConnectedOnFrame3(P1, P5)
NotConnectedOnFrame3(P1, P6)
NotConnectedOnFrame3(P1, P7)
NotConnectedOnFrame3(P1, P8)
NotConnectedOnFrame3(P1, P9)
NotConnectedOnFrame3(P2, P4)
NotConnectedOnFrame3(P2, P5)
NotConnectedOnFrame3(P2, P6)
NotConnectedOnFrame3(P2, P7)
NotConnectedOnFrame3(P2, P8)
NotConnectedOnFrame3(P2, P9)
NotConnectedOnFrame3(P2, P10)
NotConnectedOnFrame3(P3, P5)
NotConnectedOnFrame3(P3, P6)
NotConnectedOnFrame3(P3, P7)
NotConnectedOnFrame3(P3, P8)
NotConnectedOnFrame3(P3, P9)
NotConnectedOnFrame3(P3, P10)
NotConnectedOnFrame3(P4, P6)
NotConnectedOnFrame3(P4, P7)
NotConnectedOnFrame3(P4, P8)
NotConnectedOnFrame3(P4, P9)
NotConnectedOnFrame3(P4, P10)
NotConnectedOnFrame3(P5, P7)
NotConnectedOnFrame3(P5, P8)
NotConnectedOnFrame3(P5, P9)
NotConnectedOnFrame3(P5, P10)
NotConnectedOnFrame3(P6, P10)
NotConnectedOnFrame3(P7, P10)
NotConnectedOnFrame3(P8, P10)
`,B0={"example.svg":A0,"persistent-homology-basic.style":S0,"persistent-homology-shading.svg":P0,"persistent-homology.domain":E0,"persistent-homology.style":T0,"persistent-homology.substance":O0},M0=`canvas {
  width = 800
  height = 700
}

Const {
  strokeWidth = 1.5
  padding = 20.0
}

Colors {
  black = #000000
  lightBlue = #1a1ae633
  lightYellow = setOpacity(#f2f5eb, 0.5)
}

forall Set x {
    x.icon = Circle {
        fillColor : Colors.lightBlue
        strokeColor : Colors.black
        strokeStyle : "solid"
        strokeWidth : 1.0
        -- rotation : 0.0
    }

    x.text    = Equation {
      string : x.label
      -- rotation : 0.0
    }

    x.labelFn = ensure contains(x.icon, x.text)
    x.icon below x.text
}

-- Selector ordering matters!
forall Set x; Set y
where IsSubset(x, y) {
  ensure contains(y.icon, x.icon, 10.0)
  -- y.sizeFn    = ensure smallerThan(x.icon, y.icon)
  y.outsideFn = ensure disjoint(y.text, x.icon, 1.0)
  x.icon above y.icon
}

forall Map f
where From(f, X, Y); IsSubset(X, R1); IsSubset(Y, R2)
with Set X; Set Y; Set R1; Set R2 {
  f.padding = 20.0

    f.icon = Line {
      start : (R1.icon.center[0] + R1.icon.width / 2.0 + f.padding, R1.icon.center[1])
      end : (R2.icon.center[0] - R2.icon.width / 2.0 - f.padding, R2.icon.center[1])
      strokeWidth : 2.0
      strokeColor : Colors.black
      endArrowhead: "straight"
        -- style : "curved"
    }

    f.text = Equation {
      -- Doesn't seem to work after the first resample. Is the server updating f.text.height on resample?
      -- x : (f.icon.startX + f.icon.endX) / 2.0
      -- y : (f.icon.startY + f.icon.endY) / 2.0 + 1.1 * f.text.height
      string : f.label
    }

    encourage centerLabelAbove(f.icon, f.text, 5.0)
}

forall Set \`U\` {
    override \`U\`.icon.strokeStyle = "dashed"
    override \`U\`.icon.strokeWidth = Const.strokeWidth
}

forall Set \`V\` {
    override \`V\`.icon.strokeStyle = "dashed"
    override \`V\`.icon.strokeWidth = Const.strokeWidth
}

-- TODO: use subtyping for reals?
forall Set \`Rn\` {
    \`Rn\`.iconSize = canvas.height / 3

    override \`Rn\`.icon = Rectangle {
      -- Works but is slow
      -- x : -100.0
      -- y = 0.0
      width : \`Rn\`.iconSize
      height : \`Rn\`.iconSize
      fillColor : Colors.lightYellow
      -- rotation : 0.0
      strokeWidth : Const.strokeWidth
      strokeColor : Colors.black
    }

    override \`Rn\`.text.center = (\`Rn\`.icon.center[0] + \`Rn\`.icon.width / 2.0 - Const.padding, \`Rn\`.icon.center[1] + \`Rn\`.icon.width / 2.0 - Const.padding)

    delete \`Rn\`.labelFn
    delete \`Rn\`.outsideFn

}

forall Set \`Rm\`
with Set \`Rn\` {
    -- TODO: factor this block out
    override \`Rm\`.icon = Rectangle {
        fillColor : Colors.lightYellow
        center : (\`Rn\`.icon.center[0] + 400.0, \`Rn\`.icon.center[1])
        width : \`Rn\`.iconSize
        height : \`Rn\`.iconSize
        -- rotation : 0.0
        strokeWidth : 1.0
        strokeColor : Colors.black
    }

     override \`Rm\`.text.center = (\`Rm\`.icon.center[0] + \`Rm\`.icon.width / 2.0 - Const.padding, \`Rm\`.icon.center[1] + \`Rm\`.icon.width / 2.0 - Const.padding)

    delete \`Rm\`.labelFn
    delete \`Rm\`.outsideFn

    -- This doesn't seem to work
    --    \`Rm\`.posFn = encourage topRightOf(\`Rm\`.text, \`Rm\`.icon)
}`,G0=`AutoLabel All

Set A
Set U
Label U $f^{-1}(V)$
Set Rn
Label Rn $\\mathbb{R}^n$
IsSubset(U, A)
IsSubset(A, Rn)

Set B
Set V
Set Rm
Label Rm $\\mathbb{R}^m$
IsSubset(V, B)
IsSubset(B, Rm)

Map f
From(f, A, B)`,I0=`type Set
type Point
type Map

constructor Singleton(Point p) -> Set

function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set

predicate Not(Prop p1)
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate Equal(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)

notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"
notation "A ∩ B = ∅" ~ "Not(Intersecting(A, B))"
notation "f: A -> B" ~ "Map f; From(f, A, B)"
`,V0=`Set A, B, C, D, E
AutoLabel All`,N0=`Set A
Set B
Set C
Set D
Set E
Set F
Set G
IsSubset(B, A)
IsSubset(C, B)
IsSubset(D, C)
IsSubset(E, D)
IsSubset(F, E)
IsSubset(G, F)

AutoLabel All
`,_0=`<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 874.58 820.84">
  <defs>
    <linearGradient id="linear-gradient" x1="214.17" y1="30.72" x2="996.08" y2="1361.33" gradientUnits="userSpaceOnUse">
      <stop offset="0.14" stop-color="#dbdbdb" stop-opacity="0"/>
      <stop offset="0.88"/>
    </linearGradient>
  </defs>
  <title>shadow</title>
  <g id="Layer_2" data-name="Layer 2">
    <g id="Layer_1-2" data-name="Layer 1">
      <ellipse cx="437.29" cy="410.42" rx="437.29" ry="410.42" opacity="0.3" fill="url(#linear-gradient)"/>
    </g>
  </g>
</svg>
`,Q0=`type Set

predicate Not(Prop p1)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`,q0=`<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 829.57 829.57">
  <defs>
    <radialGradient id="radial-gradient" cx="388.77" cy="378.93" fx="227.19844299493337" fy="199.3018850052363" r="571.68" gradientUnits="userSpaceOnUse">
      <stop offset="0.14" stop-color="#dbdbdb"/>
      <stop offset="0.29" stop-color="#bdbdbd"/>
      <stop offset="0.6" stop-color="#707070"/>
      <stop offset="0.72" stop-color="#525252"/>
      <stop offset="0.76" stop-color="#656565"/>
      <stop offset="0.84" stop-color="#959595"/>
      <stop offset="0.96" stop-color="#e2e2e2"/>
      <stop offset="1" stop-color="#fff"/>
    </radialGradient>
  </defs>
  <title>shading</title>
  <g id="Layer_2" data-name="Layer 2">
    <g id="Layer_1-2" data-name="Layer 1">
      <circle cx="414.78" cy="414.78" r="414.78" fill="url(#radial-gradient)"/>
    </g>
  </g>
</svg>
`,R0=`canvas {
  width = 800
  height = 700
}


Colors {
   color black = rgba(0.,0.,0.,1.)
   color red = rgba(1.,0.,0.,1.)
   color green = rgba(0.,.7,0.,1.)
   color blue = rgba(0.,0,1.,1.)
   color white = rgba(1.,1.,1.,1.)
   color lightGray = rgba(.8,.8,.8,1.)
}

Global {
   shape box = Rectangle {
      center: (0.,0.)
      fillColor: none()
      strokeColor: Colors.lightGray
      strokeWidth: 2.
      width: canvas.width
      height: canvas.height
   }

   scalar setRadius = 18.
}

forall Set x {

   vec2 x.center = (?,?)

   x.icon = Text {
      center: x.center
      string: x.label
      fontFamily: "Courier"
      fontSize: "20px"
      fontWeight: "bold"
      fillColor: Colors.black
   }

   x.bounds = Circle {
      center: x.center
      r: Global.setRadius
      fillColor: none()
   }
}

forall Set x; Set y {
   -- Try to make sure no labels overlap
   encourage notTooClose(x.bounds, y.bounds, 5.0)
}


forall Set x; Set y
where IsSubset(x, y) {

   vec2 p = x.center
   vec2 q = y.center
   vec2 u = unit(q-p)
   scalar r = Global.setRadius

   arrow = Line {
      start: p + r*u
      end: q - r*u
      strokeWidth : 4.0
      strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
      endArrowhead: "straight"
      endArrowheadSize: .5
   }

   -- Position y above x
   encourage above(y.bounds, x.bounds)

   -- Have sets 'fight' to be aligned with the superset's x-position
   encourage x.bounds.center[0] == y.bounds.center[0]
}

/*
-- TODO: This one currently causes convergence to become very slow but works eventually

Set x1; Set x2
where IsSubset(x1, S); IsSubset(x2, S)
with Set S {
   -- If two sets are direct subsets of the same set, give them the same y-position
   heightFn = ensure sameHeight(x1.bounds, x2.bounds)
}
*/
`,D0=`Set A, B, C, D, E, F, G

IsSubset(B, A)
IsSubset(C, A)
IsSubset(D, B)
IsSubset(E, B)
IsSubset(F, C)
IsSubset(G, C)

Not(Intersecting(E, D))
Not(Intersecting(F, G))
Not(Intersecting(B, C))

AutoLabel All
`,$0=`Set A, B
IsSubset(B, A)
AutoLabel All `,W0=`canvas {
  width = 800
  height = 700
}

forall Set x {
    x.shape = Circle {
        strokeWidth : 0.
    }

    x.shading = Image {
        center : x.shape.center 
        width : x.shape.r * 2.0
        height : x.shape.r * 2.0
        href : "https://raw.githubusercontent.com/penrose/penrose/main/packages/examples/src/set-theory-domain/shading.svg"
    }

    x.shadow = Image {
        href : "set-theory-domain-shadow.svg"
        width : x.shape.r * 2.15
        height : x.shape.r * 2.22
        center : (x.shape.center[0] + 0.03 * x.shading.width, x.shape.center[1] - 0.051 * x.shading.height)
    }

    x.text = Equation {
        string : x.label
        fillColor: rgba(1.0, 1.0, 1.0, 1.0)
        width: 0.4 * x.shape.r
        height: 0.4 * x.shape.r
    }

    ensure contains(x.shape, x.text)
    ensure lessThan(20, x.shape.r)
    encourage sameCenter(x.text, x.shape)

    x.shape below x.text
    x.shading below x.shape
    x.shadow below x.shading
}

forall Set x; Set y
where IsSubset(x, y) {
    ensure smallerThan(x.shape, y.shape)
    ensure disjoint(y.text, x.shape, 5.0)
    ensure contains(y.shape, x.shape, 5.0)
    x.shape above y.shape
    y.text below x.shape
    x.shadow above y.shape
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.shape, y.shape)
}

forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.shape, y.shape)
    ensure disjoint(y.text, x.shape)
    ensure disjoint(x.text, y.shape)
}
`,F0=`-- Fork of venn-opt-test to verify doing optimization through an (external) computation

canvas {
  width = 800
  height = 700
}

Colors {
   Colors.none = none()
   Colors.black = rgba( 0.0, 0.0, 0.0, 1.0 )
   Colors.blue = rgba( 0.8, 0.7, 1.0, 0.2 )
   Colors.red = rgba( 1.0, 0.0, 0.0, 0.5 )
}

const {
      const.circle = Circle {
                   center : (0., 0.)
                   r : 100.0
                   fillColor : Colors.none
                   strokeWidth : 5.0
                   strokeColor : Colors.red
      }
}

forall Set x {
       x.r = 100.0
       x.angle = ?       

    x.icon = Circle {
            center : (x.r * cos(x.angle), x.r * sin(x.angle))
            strokeWidth : 0.0
    }

    x.text = Equation {
        string : x.label
    }

    ensure contains(x.icon, x.text)
    ensure lessThan(20, x.icon.r)
    encourage sameCenter(x.text, x.icon)
    x.textLayering = x.text above x.icon
}

-- forall Set x; Set y
-- where IsSubset(x, y) {

--     ensure smallerThan(x.icon, y.icon)
--     ensure disjoint(y.text, x.icon)
--     ensure contains(y.icon, x.icon, 5.0)
--     x.icon above y.icon
-- }
`,j0=`canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }

    x.text = Equation {
        string : x.label
    }

    ensure contains(x.icon, x.text)
    ensure lessThan(20, x.icon.r)
    encourage sameCenter(x.text, x.icon)
    x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {

    ensure smallerThan(x.icon, y.icon)
    ensure disjoint(y.text, x.icon)
    ensure contains(y.icon, x.icon, 5.0)
    x.icon above y.icon
}

-- TODO: Fix that the resample hack breaks on switching examples since it saves the cached functions...
-- TOOD: Also breaks if you resample without generating the function on first sample. Clearly this should be part of the state

---

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.icon, y.icon)
}

-- --------- NEW


forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.icon, y.icon)
    ensure disjoint(y.text, x.icon)
    ensure disjoint(x.text, y.icon)
}

forall Point p {
    p.offset = 20.0
    p.icon = Rectangle {
        strokeWidth : 0.0
        fillColor : rgba(0.0, 0.0, 0.0, 1.0)
        r : 3.0
    }

    p.text = Equation {
        string : p.label
        center : p.icon.center + (p.offset, p.offset)
    }

    p.textLayering = p.text above p.icon
}

forall Point p
with Set A
where PointIn(A, p) {
    ensure contains(A.icon, p.icon, 0.3 * A.icon.r)
    p.layering = p.icon above A.icon
}

forall Point p
with Set A
where Not(PointIn(A, p)) {
    ensure disjoint(A.icon, p.icon)
}
`,H0=`canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Circle {
        -- center : (?, ?)
        r : ?
        -- r : 100.0
        strokeWidth : 0.0
    }

    x.text = Equation {
        string : x.label
    }

    c = 20.0

    d = 20.0

    x.containFn = ensure contains(x.icon, x.text)
    ensure contains(x.icon, x.text)
    encourage sameCenter(x.text, x.icon)

    ensure lessThan(20, x.icon.r)

    layer x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
    ensure smallerThan(x.icon, y.icon)
    ensure disjoint(y.text, x.icon)

    ensure contains(y.icon, x.icon, 5.0)
    layer x.icon above y.icon
}
`,z0=`forall Set x {
       x.test0 = 5.3
       x.test1 = 1.0 + 2.5
       x.test2 = -0.9 * x.test1
       x.test3 = -0.9 * x.icon.center

    x.icon = Circle {
        strokeWidth : 0.0
    }

    -- x.text = Equation {
    --     string : x.label
    -- }

    -- ensure contains(x.icon, x.text)
    -- ensure minSize(x.icon)
    -- ensure maxSize(x.icon)
    -- encourage sameCenter(x.text, x.icon)
    -- x.textLayering = x.text above x.icon
}
`,U0=`canvas {
  width = 800
  height = 700
}

forall Set x {
  x.icon = Circle {
    strokeWidth : 0
  }

  x.text = Equation {
    string : x.label
    fontSize : "25px"
  }

  ensure contains(x.icon, x.text)
  encourage sameCenter(x.text, x.icon)
  x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
  ensure smallerThan(x.icon, y.icon)
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  x.icon above y.icon
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
  ensure disjoint(x.icon, y.icon)
}

forall Set x; Set y
where Intersecting(x, y) {
  ensure overlapping(x.icon, y.icon)
  ensure disjoint(y.text, x.icon)
  ensure disjoint(x.text, y.icon)
}
`,K0={"continuousmap.style":M0,"continuousmap.substance":G0,"functions.domain":I0,"multisets.substance":V0,"nested.substance":N0,"set-theory-domain-shadow.svg":_0,"setTheory.domain":Q0,"shading.svg":q0,"tree.style":R0,"tree.substance":D0,"twosets-simple.substance":$0,"venn-3d.style":W0,"venn-comp-test.style":F0,"venn-polygon.style":j0,"venn-small.style":H0,"venn-test.style":z0,"venn.style":U0},X0=`canvas {
  width = 800
  height = 700
}

forall Node n {
    shape n.icon = Rectangle {
        height: 400
        width: 200
        strokeDasharray : "4 1 2"
    }
}

forall Node from; Node to; Edge e1 where e1 := MkEdge(from, to) {
    shape e1.icon = Line {
        strokeWidth : 5
        endArrowhead: "straight"
    }
    ensure equal(signedDistance(from.icon, e1.icon.start), 5)
    ensure equal(signedDistance(to.icon, e1.icon.end), 5)
    ensure disjoint(from.icon, e1.icon)
    ensure disjoint(to.icon, e1.icon)
    ensure disjoint(to.icon, from.icon, 400)

    encourage minimal(length(e1.icon))
}

forall Edge e1; Edge e2 {
    ensure greaterThan(signedDistance(e2.icon, e1.icon.start), 30)
    ensure greaterThan(signedDistance(e2.icon, e1.icon.end), 30)
    encourage minimal(signedDistance(e2.icon, e1.icon.start))
    encourage minimal(signedDistance(e2.icon, e1.icon.end))
}`,Y0=`Point p1, p2, p3, p4, p5, p6, p7, p8
Polygon g

Around(p1,g)
Around(p2,g)
Around(p3,g)
Around(p4,g)
Around(p5,g)
Around(p6,g)
Around(p7,g)
Around(p8,g)`,Z0=`Text g

Point p0
Around(p0,g)
Point p1
Around(p1,g)
Point p2
Around(p2,g)
Point p3
Around(p3,g)
Point p4
Around(p4,g)
Point p5
Around(p5,g)
Point p6
Around(p6,g)
Point p7
Around(p7,g)
Point p8
Around(p8,g)
Point p9
Around(p9,g)
Point p10
Around(p10,g)
Point p11
Around(p11,g)
Point p12
Around(p12,g)
Point p13
Around(p13,g)
Point p14
Around(p14,g)
Point p15
Around(p15,g)
Point p16
Around(p16,g)
Point p17
Around(p17,g)
Point p18
Around(p18,g)
Point p19
Around(p19,g)
Point p20
Around(p20,g)
Point p21
Around(p21,g)
Point p22
Around(p22,g)
Point p23
Around(p23,g)
Point p24
Around(p24,g)
Point p25
Around(p25,g)
Point p26
Around(p26,g)
Point p27
Around(p27,g)
Point p28
Around(p28,g)
Point p29
Around(p29,g)
Point p30
Around(p30,g)
Point p31
Around(p31,g)
Point p32
Around(p32,g)
Point p33
Around(p33,g)
Point p34
Around(p34,g)
Point p35
Around(p35,g)
Point p36
Around(p36,g)
Point p37
Around(p37,g)
Point p38
Around(p38,g)
Point p39
Around(p39,g)
Point p40
Around(p40,g)
Point p41
Around(p41,g)
Point p42
Around(p42,g)
Point p43
Around(p43,g)
Point p44
Around(p44,g)
Point p45
Around(p45,g)
Point p46
Around(p46,g)
Point p47
Around(p47,g)
Point p48
Around(p48,g)
Point p49
Around(p49,g)
Point p50
Around(p50,g)
Point p51
Around(p51,g)
Point p52
Around(p52,g)
Point p53
Around(p53,g)
Point p54
Around(p54,g)
Point p55
Around(p55,g)
Point p56
Around(p56,g)
Point p57
Around(p57,g)
Point p58
Around(p58,g)
Point p59
Around(p59,g)
Point p60
Around(p60,g)
Point p61
Around(p61,g)
Point p62
Around(p62,g)
Point p63
Around(p63,g)
Point p64
Around(p64,g)
Point p65
Around(p65,g)
Point p66
Around(p66,g)
Point p67
Around(p67,g)
Point p68
Around(p68,g)
Point p69
Around(p69,g)
Point p70
Around(p70,g)
Point p71
Around(p71,g)
Point p72
Around(p72,g)
Point p73
Around(p73,g)
Point p74
Around(p74,g)
Point p75
Around(p75,g)
Point p76
Around(p76,g)
Point p77
Around(p77,g)
Point p78
Around(p78,g)
Point p79
Around(p79,g)
Point p80
Around(p80,g)`,J0=`Ellipse e1
Point p0
Around(p0,e1)
Point p1
Around(p1,e1)
-- Point p2
-- Around(p2,e1)
-- Point p3
-- Around(p3,e1)
-- Point p4
-- Around(p4,e1)
-- Point p5
-- Around(p5,e1)
-- Point p6
-- Around(p6,e1)
-- Point p7
-- Around(p7,e1)
-- Point p8
-- Around(p8,e1)
-- Point p9
-- Around(p9,e1)
-- Point p10
-- Around(p10,e1)
-- Point p11
-- Around(p11,e1)
-- Point p12
-- Around(p12,e1)
-- Point p13
-- Around(p13,e1)
-- Point p14
-- Around(p14,e1)
-- Point p15
-- Around(p15,e1)
-- Point p16
-- Around(p16,e1)
-- Point p17
-- Around(p17,e1)
-- Point p18
-- Around(p18,e1)
-- Point p19
-- Around(p19,e1)
-- Point p20
-- Around(p20,e1)
AutoLabel All`,nr=`Line g

Point p0
Around(p0,g)
Point p1
Around(p1,g)
Point p2
Around(p2,g)
Point p3
Around(p3,g)
Point p4
Around(p4,g)
Point p5
Around(p5,g)
Point p6
Around(p6,g)
Point p7
Around(p7,g)
Point p8
Around(p8,g)
Point p9
Around(p9,g)
Point p10
Around(p10,g)
Point p11
Around(p11,g)
Point p12
Around(p12,g)
Point p13
Around(p13,g)
Point p14
Around(p14,g)
Point p15
Around(p15,g)
Point p16
Around(p16,g)
Point p17
Around(p17,g)
Point p18
Around(p18,g)
Point p19
Around(p19,g)
Point p20
Around(p20,g)
Point p21
Around(p21,g)
Point p22
Around(p22,g)
Point p23
Around(p23,g)
Point p24
Around(p24,g)
Point p25
Around(p25,g)
Point p26
Around(p26,g)
Point p27
Around(p27,g)
Point p28
Around(p28,g)
Point p29
Around(p29,g)
Point p30
Around(p30,g)
Point p31
Around(p31,g)
Point p32
Around(p32,g)
Point p33
Around(p33,g)
Point p34
Around(p34,g)
Point p35
Around(p35,g)
Point p36
Around(p36,g)
Point p37
Around(p37,g)
Point p38
Around(p38,g)
Point p39
Around(p39,g)
Point p40
Around(p40,g)
Point p41
Around(p41,g)
Point p42
Around(p42,g)
Point p43
Around(p43,g)
Point p44
Around(p44,g)
Point p45
Around(p45,g)
Point p46
Around(p46,g)
Point p47
Around(p47,g)
Point p48
Around(p48,g)
Point p49
Around(p49,g)
Point p50
Around(p50,g)
Point p51
Around(p51,g)
Point p52
Around(p52,g)
Point p53
Around(p53,g)
Point p54
Around(p54,g)
Point p55
Around(p55,g)
Point p56
Around(p56,g)
Point p57
Around(p57,g)
Point p58
Around(p58,g)
Point p59
Around(p59,g)
Point p60
Around(p60,g)
Point p61
Around(p61,g)
Point p62
Around(p62,g)
Point p63
Around(p63,g)
Point p64
Around(p64,g)
Point p65
Around(p65,g)
Point p66
Around(p66,g)
Point p67
Around(p67,g)
Point p68
Around(p68,g)
Point p69
Around(p69,g)
Point p70
Around(p70,g)
Point p71
Around(p71,g)
Point p72
Around(p72,g)
Point p73
Around(p73,g)
Point p74
Around(p74,g)
Point p75
Around(p75,g)
Point p76
Around(p76,g)
Point p77
Around(p77,g)
Point p78
Around(p78,g)
Point p79
Around(p79,g)
Point p80
Around(p80,g)
Point p81
Around(p81,g)
Point p82
Around(p82,g)
Point p83
Around(p83,g)
Point p84
Around(p84,g)
Point p85
Around(p85,g)
Point p86
Around(p86,g)
Point p87
Around(p87,g)
Point p88
Around(p88,g)
Point p89
Around(p89,g)
Point p90
Around(p90,g)
Point p91
Around(p91,g)
Point p92
Around(p92,g)
Point p93
Around(p93,g)
Point p94
Around(p94,g)
Point p95
Around(p95,g)
Point p96
Around(p96,g)
Point p97
Around(p97,g)
Point p98
Around(p98,g)
Point p99
Around(p99,g)`,er=`Star g

Point p0
Around(p0,g)
Point p1
Around(p1,g)
Point p2
Around(p2,g)
Point p3
Around(p3,g)
Point p4
Around(p4,g)
Point p5
Around(p5,g)
Point p6
Around(p6,g)
Point p7
Around(p7,g)
Point p8
Around(p8,g)
Point p9
Around(p9,g)
Point p10
Around(p10,g)
Point p11
Around(p11,g)
Point p12
Around(p12,g)
Point p13
Around(p13,g)
Point p14
Around(p14,g)
Point p15
Around(p15,g)
Point p16
Around(p16,g)
Point p17
Around(p17,g)
Point p18
Around(p18,g)
Point p19
Around(p19,g)
Point p20
Around(p20,g)
Point p21
Around(p21,g)
Point p22
Around(p22,g)
Point p23
Around(p23,g)
Point p24
Around(p24,g)
Point p25
Around(p25,g)
Point p26
Around(p26,g)
Point p27
Around(p27,g)
Point p28
Around(p28,g)
Point p29
Around(p29,g)
Point p30
Around(p30,g)
Point p31
Around(p31,g)
Point p32
Around(p32,g)
Point p33
Around(p33,g)
Point p34
Around(p34,g)
Point p35
Around(p35,g)
Point p36
Around(p36,g)
Point p37
Around(p37,g)
Point p38
Around(p38,g)
Point p39
Around(p39,g)
Point p40
Around(p40,g)
Point p41
Around(p41,g)
Point p42
Around(p42,g)
Point p43
Around(p43,g)
Point p44
Around(p44,g)
Point p45
Around(p45,g)
Point p46
Around(p46,g)
Point p47
Around(p47,g)
Point p48
Around(p48,g)
Point p49
Around(p49,g)
Point p50
Around(p50,g)
Point p51
Around(p51,g)
Point p52
Around(p52,g)
Point p53
Around(p53,g)
Point p54
Around(p54,g)
Point p55
Around(p55,g)
Point p56
Around(p56,g)
Point p57
Around(p57,g)
Point p58
Around(p58,g)
Point p59
Around(p59,g)
Point p60
Around(p60,g)
Point p61
Around(p61,g)
Point p62
Around(p62,g)
Point p63
Around(p63,g)
Point p64
Around(p64,g)
Point p65
Around(p65,g)
Point p66
Around(p66,g)
Point p67
Around(p67,g)
Point p68
Around(p68,g)
Point p69
Around(p69,g)
Point p70
Around(p70,g)
Point p71
Around(p71,g)
Point p72
Around(p72,g)
Point p73
Around(p73,g)
Point p74
Around(p74,g)
Point p75
Around(p75,g)
Point p76
Around(p76,g)
Point p77
Around(p77,g)
Point p78
Around(p78,g)
Point p79
Around(p79,g)
Point p80
Around(p80,g)
Point p81
Around(p81,g)
Point p82
Around(p82,g)
Point p83
Around(p83,g)
Point p84
Around(p84,g)
Point p85
Around(p85,g)
Point p86
Around(p86,g)
Point p87
Around(p87,g)
Point p88
Around(p88,g)
Point p89
Around(p89,g)
Point p90
Around(p90,g)
Point p91
Around(p91,g)
Point p92
Around(p92,g)
Point p93
Around(p93,g)
Point p94
Around(p94,g)
Point p95
Around(p95,g)
Point p96
Around(p96,g)
Point p97
Around(p97,g)
Point p98
Around(p98,g)
Point p99
Around(p99,g)
`,tr=`Text g
Line l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15
`,or=`Polyline g

Point p0
Around(p0,g)
Point p1
Around(p1,g)
Point p2
Around(p2,g)
Point p3
Around(p3,g)
Point p4
Around(p4,g)
Point p5
Around(p5,g)
Point p6
Around(p6,g)
Point p7
Around(p7,g)
Point p8
Around(p8,g)
Point p9
Around(p9,g)
Point p10
Around(p10,g)
Point p11
Around(p11,g)
Point p12
Around(p12,g)
Point p13
Around(p13,g)
Point p14
Around(p14,g)
Point p15
Around(p15,g)
Point p16
Around(p16,g)
Point p17
Around(p17,g)
Point p18
Around(p18,g)
Point p19
Around(p19,g)
Point p20
Around(p20,g)
Point p21
Around(p21,g)
Point p22
Around(p22,g)
Point p23
Around(p23,g)
Point p24
Around(p24,g)
Point p25
Around(p25,g)
Point p26
Around(p26,g)
Point p27
Around(p27,g)
Point p28
Around(p28,g)
Point p29
Around(p29,g)
Point p30
Around(p30,g)
Point p31
Around(p31,g)
Point p32
Around(p32,g)
Point p33
Around(p33,g)
Point p34
Around(p34,g)
Point p35
Around(p35,g)
Point p36
Around(p36,g)
Point p37
Around(p37,g)
Point p38
Around(p38,g)
Point p39
Around(p39,g)
Point p40
Around(p40,g)
Point p41
Around(p41,g)
Point p42
Around(p42,g)
Point p43
Around(p43,g)
Point p44
Around(p44,g)
Point p45
Around(p45,g)
Point p46
Around(p46,g)
Point p47
Around(p47,g)
Point p48
Around(p48,g)
Point p49
Around(p49,g)
Point p50
Around(p50,g)
Point p51
Around(p51,g)
Point p52
Around(p52,g)
Point p53
Around(p53,g)
Point p54
Around(p54,g)
Point p55
Around(p55,g)
Point p56
Around(p56,g)
Point p57
Around(p57,g)
Point p58
Around(p58,g)
Point p59
Around(p59,g)
Point p60
Around(p60,g)
Point p61
Around(p61,g)
Point p62
Around(p62,g)
Point p63
Around(p63,g)
Point p64
Around(p64,g)
Point p65
Around(p65,g)
Point p66
Around(p66,g)
Point p67
Around(p67,g)
Point p68
Around(p68,g)
Point p69
Around(p69,g)
Point p70
Around(p70,g)
Point p71
Around(p71,g)
Point p72
Around(p72,g)
Point p73
Around(p73,g)
Point p74
Around(p74,g)
Point p75
Around(p75,g)
Point p76
Around(p76,g)
Point p77
Around(p77,g)
Point p78
Around(p78,g)
Point p79
Around(p79,g)
Point p80
Around(p80,g)
Point p81
Around(p81,g)
Point p82
Around(p82,g)
Point p83
Around(p83,g)
Point p84
Around(p84,g)
Point p85
Around(p85,g)
Point p86
Around(p86,g)
Point p87
Around(p87,g)
Point p88
Around(p88,g)
Point p89
Around(p89,g)
Point p90
Around(p90,g)`,rr=`canvas {
  width = 800
  height = 700
}

forall Text \`g\` {
  \`g\`.shape = Equation {
    string : "e^{i \\pi} = -1"
    fontSize : "50pt"
  }
  \`g\`.box = Rectangle {
    center: \`g\`.shape.center
    width : \`g\`.shape.width
    height : \`g\`.shape.height
    fillColor: none()
    strokeWidth: 1
    strokeColor: #000
  }
}

forall Line l {
  l.shape = Line {
    strokeColor: #000
  }

  ensure touching(\`g\`.shape, l.shape)
}
`,ar=`canvas {
  width = 800
  height = 700
}

forall Point p {
    p.icon = Circle {
        strokeWidth : 0
        fillColor: rgba(0,0,0,1)
        r: 3
    }
}

forall ReverseL g {
    g.icon = Polygon {
        strokeWidth : 1
        fillColor: rgba(68/255,114/255,148/255,1)
        points: [(-200,-200), (-200,0), (0,0), (0,200), (200,200), (200,-200)]
    }
}

forall Star s {
    s.icon = Polygon {
        strokeWidth : 1
        fillColor: rgba(68/255,114/255,148/255,1)
        points: [(-200,-200), (0,200), (200,-300), (-200,150), (250,150)]
        fillRule : "evenodd"
    }
}

forall Line l {
    l.icon = Line {
        strokeWidth : 8
        start : (-200,-100)
        end : (250, 100)
    }
}

forall Polyline l {
    l.icon = Polyline {
        strokeWidth : 8
        points : [(-200,-200), (0,200), (200,-300), (-200,150), (250,150)]
    }
}

forall Text t {
    shape t.icon = Text {
      string: "Hello Nimo"
      center: (0,0)
      fillColor: rgba(0.,0.,0.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "128px"
   }
}

forall Ellipse e {
    shape e.icon = Ellipse {
      center: (10,10)
      rx : 100
      ry : 50
      
   }
}

forall Point p; Shape s
where Around(p, s) {
    ensure equal(signedDistance(s.icon, p.icon.center), 20)
}
`,lr=`type Point

type Shape
type ReverseL <: Shape
type Star <: Shape
type Line <: Shape
type Polyline <: Shape
type Text <: Shape
type Ellipse <: Shape

predicate Around(Point p, Shape s)
`,ir=`type Node
type Edge
constructor MkEdge(Node from, Node to) -> Edge
`,sr=`Node b1, b2
Edge e1, e2, e3, e4
e1 := MkEdge(b1, b2)
e2 := MkEdge(b1, b2)
e3 := MkEdge(b1, b2)
e4 := MkEdge(b1, b2)`,cr={"box-arrow.style":X0,"eight-points-around-polygon.substance":Y0,"eighty-text.substance":Z0,"ellipse.substance":J0,"hundred-points-around-line.substance":nr,"hundred-points-around-star.substance":er,"lines-around-rect.substance":tr,"ninety-points-around-polyline.substance":or,"rect-line-dist.style":rr,"shape-distance.style":ar,"shapes.domain":lr,"supersimplegraph.domain":ir,"two-nodes-two-edges.substance":sr},dr=`type Shape
type Circle <: Shape
type Ellipse <: Shape
type Rectangle <: Shape
type Polygon <: Shape
type Polyline <: Shape
type Image <: Shape
type Equation <: Shape
type Text <: Shape
type Line <: Shape
type Path <: Shape
`,pr=`-- fill shapes
Rectangle Rectangle
Circle Circle
Ellipse Ellipse
Polygon Polygon
Image Image
-- outline shapes
Path Path
Line Line 
Equation Equation
Text Text
Polyline Polyline

AutoLabel All
`,gr=`canvas {
  width = 400
  height = 600
}
const {
  ch2 = canvas.height/2
  cw2 = canvas.width/2
  bg = Rectangle {
    center: (0, 0)
    width: canvas.width
    height: canvas.height
    fillColor: #fff
  }
  vguide1 = Line {
    start: (0, ch2)
    end: (0, -ch2)
    strokeColor: #f00
    strokeWidth: 1
  }
  vguide2 = Line {
    start: (-cw2 + 10, ch2)
    end: (-cw2 + 10, -ch2)
    strokeColor: #f00
    strokeWidth: 1
  }

}

forall Line l {
  rowPadding = 70
  rowY = -match_id * rowPadding + canvas.height/2
  l.icon = Line {
    start: (-const.cw2 + 10, rowY)
    end: (0, rowY)
    startArrowhead: l.label
    endArrowhead: l.label
    strokeColor: #000
    strokeWidth: 3.0
  }
  l.text = Equation {
    string: "\\textbf{" + l.label + "}"
    center: (?, rowY)
    fillColor: #000
  }
  ensure equal(l.text.center[0], l.text.height/2 + rowPadding)
}

`,fr=`-- https://tikz.dev/tikz-arrows
Line none, concave, straight, line, doubleLine, loopup, loopdown, loop
AutoLabel All`,ur=`canvas {
  width = 800
  height = 1200
}

const {
  ch = canvas.height
  cw = canvas.width
  -- grid
  gh = 200
  gw = 200
}

forall Shape s {
    s.center = (?, ?)
    s.text = Equation {
        center: s.center - (0, 70)
        string: "\\text{" + s.label + "}"
    }
    s.container = Rectangle {
        center: s.center
        width: const.gw - 20
        height: const.gh - 20
        strokeWidth: 1
        strokeColor: none()
        fillColor: none()
    }
}

forall Circle e {
    e.center = (-const.cw / 6, 2*const.gh)
    e.icon = Circle {
        center: e.center
        r: const.gh/4
    }
}

forall Rectangle r {
    r.center = (-const.cw / 6, const.gh)
    r.icon = Rectangle { 
        center: r.center
        width: 200
        height: 100
    }
}

forall Ellipse e {
    e.center = (-const.cw / 6, 0)
    e.icon = Ellipse { 
        center: e.center
        rx: const.gw/2
        ry: const.gh/4
    }
}


forall Polygon j {
    c = (-const.cw / 6, -const.gh)
    u = 50
    j.center = c
	j.icon = Polygon { 
        points: ( c+(-u, 0), c+(0, u), c+(u, 0), c+(u/2, -u), c+(-u/2, -u) )
	}
}

forall Image i {
    i.center = (-const.cw / 6, -2*const.gh)
    i.icon = Image {
       href: "https://penrose.cs.cmu.edu/img/logo.svg"
       height: 100
       width: 100
       center: i.center
    }
}

forall Path a {
    c = (const.cw / 6, 2*const.gh)
    u = 50
    a.center = c
    a.icon = Path {
        d: pathFromPoints("closed", [ c+(-u, 0), c+(0, u), c+(u, 0), c+(u/2, -u), c+(-u/2, -u) ])
    }
}

forall Line l {
    l.center = (const.cw / 6, const.gh)
    l.icon = Line { 
        start: l.center - (50, 50) 
        end: l.center + (50, 50) 
    }
}

forall Polyline k {
    c = (const.cw / 6, 0)
    u = 50
    k.center = c
	k.icon = Polyline { 
        points: ( c+(-u, 0), c+(0, u), c+(u, 0), c+(u/2, -u), c+(-u/2, -u), c+(-u + 10, 0) )	
    }
}

forall Equation h {
    h.center = (const.cw / 6, -const.gh)
    h.icon = Equation { 
        string: "E = mc^2"
        center: h.center
    }
}

forall Text h {
    h.center = (const.cw / 6, -2*const.gh)
    h.icon = Text { 
        string: "Diagrams, not words"
        fontFamily: "Palatino"
        fontStyle: "italic"
        center: h.center
    }
}
`,hr={"all-shapes.domain":dr,"all-shapes.substance":pr,"arrows.style":gr,"arrows.substance":fr,"shape-spec.style":ur},br=`type Node
`,kr=`-- Visualize the construction of callout boxes, as in Callout.ts

canvas {
  width = 800
  height = 700
}

global {
       nodeR = 6.

       lineThickness = 3.

       calloutPadding = 30. -- Padding around the text for rect
       calloutThickness = 30. -- Thickness of base of stem
       calloutEndPadding = 40. -- Space between the external anchor point and the stem
       maxCalloutDist = 200.
}

colors {
       black = rgba(0., 0., 0., 1.)
       red = rgba(1., 0., 0., 1.)
       green = rgba(0., 1., 0., 1.)
       blue = rgba(0., 0., 1., 1.)
       none = none()
}

forall Node n {
     n.shape = Circle { 
             r: global.nodeR
             strokeWidth: 0.
             fillColor: colors.black
     }

     n.text = Equation {
            string: n.label
     }

     n.callout = Rectangle {
               strokeWidth: global.lineThickness
               strokeColor: colors.black
               fillColor: colors.none
               center: n.text.center
               width: n.text.width + global.calloutPadding
               height: n.text.height + global.calloutPadding
     }

     -- Parallel to callout direction
     vec = normalize(n.callout.center - n.shape.center)

     n.anchorStart = n.shape.center + global.calloutEndPadding * vec

     -- Thick line
     n.line = Line {
            start: n.anchorStart
            end: n.callout.center
            strokeWidth: global.calloutThickness
            strokeColor: rgba(0.5, 0.5, 0.5, 0.5)
     }

     -- Thin line
     n.line1 = Line {
             start: n.anchorStart
             end: n.callout.center
             strokeWidth: global.lineThickness
             strokeColor: colors.black
     }

     n.line1 above n.line

     -- Perpendicular location - start of segment
     segStart = n.anchorStart + global.calloutThickness / 2. * rot90(vec)
     n.circle1 = Circle {
               fillColor: colors.red
               r: 5.
               strokeWidth: 0.
               center: segStart
     }

     -- Seg endpt
     t = norm(n.callout.center - n.anchorStart)
     segEnd = segStart + t * vec

     n.circle2 = Circle {
               fillColor: colors.red
               r: 5.
               strokeWidth: 0.
               center: segEnd
     }

     -- hit point 1
     n.circle3 = Circle {
               fillColor: colors.blue
               r: 5.
               strokeWidth: 0.
               -- center: 
     }

     dist = norm(n.callout.center - n.shape.center)
     ensure lessThan(dist, 200.)

     -- TODO: Doesn't work so well
     -- ensure disjoint(n.callout, n.shape)
}
`,xr=`Node n1
Node n2
Node ldfhskjfhsdsdks
Node a

AutoLabel All
Label n1 $\\int_0^\\infty e^x + 3x^2 + \\frac{1}{2}$
`,vr=`canvas {
  width = 800
  height = 700
}

global {
       nodeR = 6.
}

colors {
       black = rgba(0., 0., 0., 1.)
       gray = rgba(0.8, 0.8, 0.8, 1.)
       red = rgba(1., 0., 0., 1.)
       green = rgba(0., 1., 0., 1.)
       blue = rgba(0., 0., 1., 1.)
       none = none()
}

forall Node n {
     n.shape = Circle { 
             r: global.nodeR
             strokeWidth: 0.
             fillColor: colors.black
     }

     n.text = Equation {
            string: n.label
     }

--      n.callout = Callout {
--              anchor: n.shape.center
--              center: n.text.center
--              w: n.text.width
--              h: n.text.height
--              color: colors.gray
--              padding: 30.
--              strokeWidth: 2.
--              strokeColor: colors.black
--      }

     dist = norm(n.callout.center - n.shape.center)
     ensure lessThan(dist, 300.)
}`,mr={"shape.domain":br,"shape.style":kr,"shape.substance":xr,"shape2.style":vr},yr={},wr=`-- acetic acid, expressed via structural-formula DSL
Hydrogen H1, H2, H3, H4
Carbon C1, C2
Oxygen O1, O2

SingleBond(C1, H1)
SingleBond(C1, H2)
SingleBond(C1, H3)
SingleBond(C1, C2)
DoubleBond(C2, O1)
SingleBond(C2, O2)
SingleBond(O2, H4)

AutoLabel All
`,Cr=`-- caffeine molecule, expressed via structural-formula DSL
Carbon C2, C4, C5, C6, C8
Nitrogen N1, N2, N3, N4
Oxygen O1, O2
Hydrogen H1
FunctionalGroup methyl1, methyl2, methyl3

SingleBond(methyl1, N2)
SingleBond(N2, C4)
SingleBond(C4, C5)
DoubleBond(C5, C6)
SingleBond(C6, N1)
SingleBond(N1, C2)
SingleBond(N2, C2)
DoubleBond(C2, O2)
DoubleBond(C4, O1)
SingleBond(N3, methyl2)
SingleBond(N3, C5)
SingleBond(C8, N3)
DoubleBond(C8, N4)
SingleBond(N4, C6)
SingleBond(C8, H1)
SingleBond(N1, methyl3)

AutoLabel All
Label methyl1 $\\mathrm{H}_3\\mathrm{C}$
Label methyl2 $\\mathrm{CH}_3$
Label methyl3 $\\mathrm{CH}_3$

`,Lr=`-- caffeine molecule, expressed via structural-formula DSL
Carbon C1, C2, C3, C4, C5, C6, C7, C8
Nitrogen N1, N2, N3, N4
Oxygen O1, O2
Hydrogen H1, H2, H3, H4, H5, H6, H7, H8, H9, H10

SingleBond(N2, C4)
SingleBond(C4, C5)
DoubleBond(C5, C6)
SingleBond(C6, N1)
SingleBond(N1, C2)
SingleBond(N2, C2)
DoubleBond(C2, O2)
SingleBond(N2, C1)
SingleBond(C1, H7)
SingleBond(C1, H6)
SingleBond(C1, H5)
DoubleBond(C4, O1)
SingleBond(C7, H4)
SingleBond(C7, H3)
SingleBond(H2, C7)
SingleBond(N3, C7)
SingleBond(N3, C5)
SingleBond(C8, N3)
DoubleBond(C8, N4)
SingleBond(N4, C6)
SingleBond(C8, H1)
SingleBond(C3, H10)
SingleBond(C3, H8)
SingleBond(C3, H9)
SingleBond(N1, C3)

AutoLabel All
`,Ar=`-- ethanol molecule, expressed via structural-formula DSL
Carbon C1, C2
Hydrogen H1, H2, H3, H4, H5, H6
Oxygen O1

SingleBond(C1, H1)
SingleBond(H2, C1)
SingleBond(C1, H3)
SingleBond(C1, C2)
SingleBond(C2, H4)
SingleBond(C2, H5)
SingleBond(O1, C2)
SingleBond(H6, O1)

AutoLabel All
`,Sr=`-- glucose molecule, expressed via structural-formula DSL
Oxygen O1
Hydrogen H1, H2, H3, H4
Carbon C1, C2, C3, C4, C5
FunctionalGroup hydroxy_anion1, hydroxy_anion2, hydroxy_anion3
FunctionalGroup hydroxy_cation
FunctionalGroup hydroxy_methyl

SingleBond(hydroxy_methyl, C1)
SingleBond(C1, O1)
SingleBond(C1, C2)
SingleBond(C2, C3)
SingleBond(C3, C4)
SingleBond(C4, C5)
SingleBond(C5, O1)
SingleBond(C2, H1)
SingleBond(C2, hydroxy_cation)
SingleBond(C3, hydroxy_anion1)
SingleBond(C3, H2)
SingleBond(C4, H3)
SingleBond(C4, hydroxy_anion2)
SingleBond(C5, H4)
SingleBond(C5, hydroxy_anion3)

AutoLabel All
Label hydroxy_cation $\\mathrm{HO}$
Label hydroxy_anion1 $\\mathrm{OH}$
Label hydroxy_anion2 $\\mathrm{OH}$
Label hydroxy_anion3 $\\mathrm{OH}$
Label hydroxy_methyl $\\mathrm{CH}_2\\mathrm{OH}$
`,Pr=`-- methane molecule, expressed via structural-formula DSL
Carbon C1
Hydrogen H1, H2, H3, H4

SingleBond(C1, H1)
SingleBond(C1, H2)
SingleBond(C1, H3)
SingleBond(C1, H4)

AutoLabel All
`,Er=`-- photosynthesis, expressed via structural-formula DSL
Carbon C1, C2, C3

Oxygen O1, O2, O3, O4, O5, O6
Oxygen O7, O8, O9

Hydrogen H1, H2, H3, H4, H5, H6

-- carbon dioxide
DoubleBond(C1, O1)
DoubleBond(C1, O2)

-- carbon dioxide
DoubleBond(C2, O3)
DoubleBond(C2, O4)

-- carbon dioxide
DoubleBond(C3, O5)
DoubleBond(C3, O6)

-- water
SingleBond(O7, H1)
SingleBond(O7, H2)

-- water
SingleBond(O8, H3)
SingleBond(O8, H4)

-- water
SingleBond(O9, H5)
SingleBond(O9, H6)

AutoLabel All
`,Tr=`-- photosynthesis, expressed via structural-formula DSL
Carbon C1, C2, C3, C4, C5, C6

Oxygen O1, O2, O3, O4, O5, O6
Oxygen O7, O8, O9, O10, O11, O12
Oxygen O13, O14, O15, O16, O17, O18

Hydrogen H1, H2, H3, H4, H5, H6
Hydrogen H7, H8, H9, H10, H11, H12

-- water
SingleBond(O13, H1)
SingleBond(O13, H2)

-- water
SingleBond(O14, H3)
SingleBond(O14, H4)

-- water
SingleBond(O15, H5)
SingleBond(O15, H6)

-- water
SingleBond(O16, H7)
SingleBond(O16, H8)

-- water
SingleBond(O17, H9)
SingleBond(O17, H10)

-- water
SingleBond(O18, H11)
SingleBond(O18, H12)


-- carbon dioxide
DoubleBond(C1, O1)
DoubleBond(C1, O2)

-- carbon dioxide
DoubleBond(C2, O3)
DoubleBond(C2, O4)

-- carbon dioxide
DoubleBond(C3, O5)
DoubleBond(C3, O6)

-- carbon dioxide
DoubleBond(C4, O7)
DoubleBond(C4, O8)

-- carbon dioxide
DoubleBond(C5, O9)
DoubleBond(C5, O10)

-- carbon dioxide
DoubleBond(C6, O11)
DoubleBond(C6, O12)

AutoLabel All
`,Or=`-- water molecule, expressed via structural-formula DSL
Hydrogen H1, H2
Oxygen O1

SingleBond( O1, H1 )
SingleBond( O1, H2 )

AutoLabel All
`,Br={"acetic-acid.substance":wr,"caffeine-reduced.substance":Cr,"caffeine.substance":Lr,"ethanol.substance":Ar,"glucose.substance":Sr,"methane.substance":Pr,"photosynthesis-half.substance":Er,"photosynthesis.substance":Tr,"water.substance":Or},Mr=`-- TODO would be really helpful to be able to join multiple
-- .style files---currently we are maintaining parallel edits
-- across both pseudo-3d.style and pseudo-3d-reaction.style

canvas {
   scalar width  = 1248.
   scalar height = 702.
}

Colors {
   vec4 clear     = rgba( 0., 0., 0., 0. )
   vec4 black     = rgba( 0., 0., 0., 1. )
   vec4 gray      = rgba( .5, .5, .5, 1. )
   vec4 lightGray = rgba( .9, .9, .9, 1. )
   vec4 white     = rgba( 1., 1., 1., 1. )
   vec4 red       = rgba( 1., 0., 0., 1. )
   vec4 green     = rgba( 0., .7, 0., 1. )
   vec4 blue      = rgba( 0., 0., 1., 1. )
   vec4 darkRed   = rgba( .7, 0., 0., 1. )
   vec4 darkBlue  = rgba( 0., 0., .7, 1. )
   vec4 purple    = rgba( .66, .36, .95, 1. )
   vec4 turquoise  = rgba( .1, .7, .6, 1. )
}

Global {
   scalar atomRadius = 25.
   scalar bondLength = 60.

   scalar padding = 50.

   -- specify the typeface(s) that should
   -- be used, in order of availability
   string fontFamily = "Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif"

   -- box around the whole canvas
   shape bbox = Rectangle {
      width : canvas.width
      height : canvas.height
      center : (0.,0.)
      fillColor : Colors.clear
      strokeColor : Colors.gray
      strokeWidth : 8
   }

   scalar reactionBoxSize = .75 * canvas.width/2.
   scalar reactionBoxTop = reactionBoxSize/2.
   scalar reactionBoxBottom = -reactionBoxSize/2.

   -- box around reactants
   scalar reactantCenter = -canvas.width/4.
   shape reactantBox = Rectangle {
      width : reactionBoxSize
      height : reactionBoxSize
      center : (reactantCenter,0.)
      fillColor : rgba( 0., 0., 0., .1 )
      strokeColor : Colors.clear
      strokeWidth : 12
      cornerRadius : 20
   }
   shape reactantText = Text {
      string : "reactants"
      center : (reactantCenter,-24.+reactionBoxBottom) -- TODO make issue about non-commutativity of + here
      fillColor : Colors.black
      fontSize : "18px"
      fontFamily: Global.fontFamily
      fontWeight: "bold"
   }

   -- box around products
   scalar productCenter = canvas.width/4.
   scalar productTop = .75*canvas.width/4.
   shape productBox = Rectangle {
      width : reactionBoxSize
      height : reactionBoxSize
      center : (productCenter,0.)
      fillColor : rgba( 0., 0., 0., .1 )
      strokeColor : Colors.clear
      strokeWidth : 12
      cornerRadius : 20
   }
   shape productText = Text {
      string : "products"
      center : (productCenter,-24.+reactionBoxBottom)
      fillColor : Colors.black
      fontSize : "18px"
      fontFamily: Global.fontFamily
      fontWeight: "bold"
   }
}

forall Node n {

   scalar cx = ?
   scalar cy = ?
   vec2 n.center = (cx,cy)

   scalar R = Global.atomRadius

   shape n.icon = Circle {
      r : R
      center : n.center
      fillColor : Colors.white
      strokeColor : Colors.black
      strokeWidth : 3.
   }

   shape n.shadow = Ellipse {
      rx : 2.*Global.atomRadius
      ry : Global.atomRadius
      center : (cx,cy) + (0.,-2.*R)
      fillColor : rgba( .95, .95, .95, 1. )
      strokeColor : Colors.clear
      strokeWidth : 3.
   }

   shape n.text = Text {
      string : n.label
      center : n.center
      fillColor : Colors.black
      fontSize : "10.5px"
   }

   ensure contains( Global.bbox, n.icon )

   layer n.icon below n.text
}

-- make sure shadows are drawn below all molecules
forall Node n1; Node n2 {
   layer n1.shadow below n2.icon
   layer n2.shadow below n1.icon
}

-- draw functional groups as boxes
forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.text.fillColor = Colors.black
   override g.shadow.fillColor = Colors.clear

   g.box = Rectangle {
      center : g.center
      width : 3.*Global.atomRadius
      height : 1.5*Global.atomRadius
      fillColor : Colors.lightGray
      strokeColor : Colors.gray
      strokeWidth : 3.
      cornerRadius : 10.
   }

   layer g.shadow below g.box
}

forall Oxygen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.red
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Carbon a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.black
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.blue
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.black
   override a.text.string = ""

   -- make hydrogen atoms (and their shadows) smaller
   override a.icon.r = .75*Global.atomRadius
   override a.shadow.rx = .75*2.*Global.atomRadius
   override a.shadow.ry = .75*Global.atomRadius
}

forall Chlorine a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.green
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Sodium a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.purple
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}


forall Node n1; Node n2
where SingleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line above n2.icon
   layer innerLine above line
   layer innerLine below n1.icon
}

forall Node n1; Node n2
where DoubleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   vec2 v = ( -u[1], u[0] )
   scalar r = Global.atomRadius

   shape line1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   shape line2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 above n2.icon
   layer innerLine1 above line1
   layer innerLine1 below n1.icon

   layer line2 above n2.icon
   layer innerLine2 above line2
   layer innerLine2 below n1.icon

   layer line2 above innerLine1
}

forall Node n1; Node n2
where IonicBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 2.5
      strokeColor : Colors.turquoise
      strokeLinecap: "butt"
      style: "dashed"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line below n1.icon
   layer line below n2.icon
}


-- make bonds with hydrogen shorter
forall Node n; Hydrogen h
where SingleBond(n,h) {
   vec2 x1 = n.center
   vec2 x2 = h.center
   encourage equal( 2.*norm(x1-x2), .5*Global.bondLength )
}

-- give common molecules a physical bond angle
forall Oxygen o; Hydrogen h1; Hydrogen h2 -- water (H2O)
where SingleBond(o,h1); SingleBond(o,h2) { -- TODO make an issue about symmetry of predicate matches
   vec2 a = o.center
   vec2 b = h1.center
   vec2 c = h2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(104.5) )
}
forall Carbon c0; Oxygen o1; Oxygen o2 -- carbon dioxide (CO2)
where DoubleBond(c0,o1); DoubleBond(c0,o2) {
   vec2 a = c0.center
   vec2 b = o1.center
   vec2 c = o2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(180.) )
}

-- use a Coulomb-like force to prevent nodes from overlapping
-- (but only worry about nodes in the same reactant/product box)
forall Molecule m; Node n1; Node n2
where IsReactant(m); Contains(m,n1); Contains(m,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m1; Molecule m2; Node n1; Node n2
where IsReactant(m1); IsReactant(m2); Contains(m1,n1); Contains(m2,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m; Node n1; Node n2
where IsProduct(m); Contains(m,n1); Contains(m,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m1; Molecule m2; Node n1; Node n2
where IsProduct(m1); IsProduct(m2); Contains(m1,n1); Contains(m2,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}


-- place a label near the Nodes in each Molecule
forall Molecule m {

   -- m.box = Rectangle {
   --    center : (?,?)
   --    fillColor : Colors.clear
   -- }

   m.labelCenter = (?,?)

   m.text = Equation {
      string : m.label
      fillColor : Colors.black
      fontSize : "13.5px"
      center : m.labelCenter
      --center : m.box.center - (0.,3.*Global.atomRadius)
      -- TODO add stroke (have to expose SVG text stroke in Penrose renderer)
   }

   -- used to prevent overlap with molecules
   -- (since "ensure disjoint" isn't currently supported for Equation-Circle pairs)
   scalar R = 20.
   m.textPhantom1 = Circle {
      center : m.labelCenter
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
   m.textPhantom2 = Circle {
      center : m.labelCenter + (2.*R,0.)
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
   m.textPhantom3 = Circle {
      center : m.labelCenter - (2.*R,0.)
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
}
forall Molecule m; Node n
where Contains(m,n) {
   --override m.box.center = n.icon.center
   encourage near( m.text, n.icon )
   ensure disjoint( m.textPhantom1, n.icon )
   ensure disjoint( m.textPhantom2, n.icon )
   ensure disjoint( m.textPhantom3, n.icon )
   layer m.text above n.icon
}

forall Reaction r {

   scalar h = (Global.reactionBoxTop + canvas.height/2.)/2.

   r.text = Text {
      string : r.label
      center : (0.,h)
      fontSize : "30px"
      fontFamily: Global.fontFamily
      fontVariant: "small-caps"
      fillColor : Colors.black
   }
}

-- put all reactants on the left
forall Molecule m; Node n
where IsReactant(m); Contains(m,n) {
   ensure contains( Global.reactantBox, n.icon, Global.padding )
   ensure contains( Global.reactantBox, m.text, Global.padding )
   layer n.icon above Global.reactantBox
   layer n.shadow below Global.reactantBox
}

-- put all products on the right
forall Molecule m; Node n
where IsProduct(m); Contains(m,n) {
   ensure contains( Global.productBox, n.icon, Global.padding )
   ensure contains( Global.productBox, m.text, Global.padding )
   layer n.icon above Global.productBox
   layer n.shadow below Global.productBox
}

forall Reaction r
where IsNetForward(r) {

   vec2 Rc = Global.reactantCenter
   vec2 Pc = Global.productCenter
   scalar s = Global.reactionBoxSize
   scalar p = 10.

   shape reactionArrow = Line {
      start : (Rc+(0.5*s)+p,0.)
      end : (Pc-(0.5*s)-p,0.)
      strokeColor : Colors.black
      strokeWidth : 5.
      endArrowhead : "straight"
      endArrowheadSize : .5
   }
}

`,Gr=`canvas {
   scalar width  = 1440.
   scalar height = 810.
}

Colors {
   vec4 clear     = rgba( 0., 0., 0., 0. )
   vec4 black     = rgba( 0., 0., 0., 1. )
   vec4 gray      = rgba( .5, .5, .5, 1. )
   vec4 lightGray = rgba( .9, .9, .9, 1. )
   vec4 white     = rgba( 1., 1., 1., 1. )
   vec4 red       = rgba( 1., 0., 0., 1. )
   vec4 green     = rgba( 0., .7, 0., 1. )
   vec4 blue      = rgba( 0., 0., 1., 1. )
   vec4 darkRed   = rgba( .7, 0., 0., 1. )
   vec4 darkBlue  = rgba( 0., 0., .7, 1. )
   vec4 purple    = rgba( .66, .36, .95, 1. )
   vec4 turquoise  = rgba( .1, .7, .6, 1. )
}

Global {
   scalar atomRadius = 25.
   scalar bondLength = 60.

   scalar padding = 100.

   -- box around the whole canvas
   shape bbox = Rectangle {
      width : canvas.width
      height : canvas.height
      center : (0.,0.)
      fillColor : Colors.clear
      strokeColor : Colors.gray
      strokeWidth : 8
   }
}

forall Node n {

   scalar cx = ?
   scalar cy = ?
   vec2 n.center = (cx,cy)

   scalar R = Global.atomRadius

   shape n.icon = Circle {
      r : R
      center : n.center
      fillColor : Colors.white
      strokeColor : Colors.black
      strokeWidth : 3.
   }

   shape n.shadow = Ellipse {
      rx : 2.*Global.atomRadius
      ry : Global.atomRadius
      center : (cx,cy) + (0.,-2.*R)
      fillColor : rgba( .95, .95, .95, 1. )
      strokeColor : Colors.clear
      strokeWidth : 3.
   }

   shape n.text = Text {
      string : n.label
      center : n.center
      fillColor : Colors.black
      fontSize : "10.5px"
   }

   ensure contains( Global.bbox, n.icon )

   layer n.icon below n.text
}

forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.text.fillColor = Colors.black
   override g.shadow.fillColor = Colors.clear

   g.box = Rectangle {
      center : g.center
      width : 3.*Global.atomRadius
      height : 1.5*Global.atomRadius
      fillColor : Colors.lightGray
      strokeColor : Colors.gray
      strokeWidth : 3.
      cornerRadius : 10.
   }

   layer g.shadow below g.box
}

forall Oxygen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.red
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Carbon a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.black
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.blue
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.black
   override a.text.string = ""

   -- make hydrogen atoms (and their shadows) smaller
   override a.icon.r = .75*Global.atomRadius
   override a.shadow.rx = .75*2.*Global.atomRadius
   override a.shadow.ry = .75*Global.atomRadius
}

forall Chlorine a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.green
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Sodium a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.purple
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}


forall Node n1; Node n2
where SingleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line above n2.icon
   layer innerLine above line
   layer innerLine below n1.icon
}

forall Node n1; Node n2
where DoubleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   vec2 v = ( -u[1], u[0] )
   scalar r = Global.atomRadius

   shape line1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   shape line2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 above n2.icon
   layer innerLine1 above line1
   layer innerLine1 below n1.icon

   layer line2 above n2.icon
   layer innerLine2 above line2
   layer innerLine2 below n1.icon

   layer line2 above innerLine1
}

forall Node n1; Node n2
where IonicBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 2.5
      strokeColor : Colors.turquoise
      strokeLinecap: "butt"
      style: "dashed"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line below n1.icon
   layer line below n2.icon
}


-- make bonds with hydrogen shorter
forall Node n; Hydrogen h
where SingleBond(n,h) {
   vec2 x1 = n.center
   vec2 x2 = h.center
   encourage equal( 2.*norm(x1-x2), .5*Global.bondLength )
}

-- give water molecules a physical bond angle
forall Oxygen o; Hydrogen h1; Hydrogen h2
where SingleBond(o,h1); SingleBond(o,h2) {
   vec2 a = o.center
   vec2 b = h1.center
   vec2 c = h2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(104.5) )
}

-- use a Coulomb-like force to prevent nodes from overlapping
forall Node n1; Node n2 {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )

   -- make sure shadows are drawn below all molecules
   layer n1.shadow below n2.icon
   layer n2.shadow below n1.icon
}

-- place a label near the Nodes in each Molecule
forall Molecule m {

   m.box = Rectangle {
      center : (?,?)
      fillColor : Colors.clear
   }

   m.text = Text {
      string : m.label
      fillColor : Colors.black
      fontSize : "13.5px"
      center : m.box.center - (0.,2.*Global.atomRadius)
      -- TODO add stroke (have to expose SVG text stroke in Penrose renderer)
   }
}
forall Molecule m; Node n
where Contains(m,n) {
   encourage near( m.box, n.icon )
   layer m.text above n.icon
}

`,Ir=`AutoLabel All

-- combustion reaction, expressed via structural-formula DSL

Reaction combustion
Label combustion "Methane Combustion Reaction"
IsNetForward( combustion )

-- reactants: CH4 + 2O2 ====================================
Carbon C1
Hydrogen H1, H2, H3, H4
Oxygen O1, O2, O3, O4

Molecule methane
Label methane $\\text{methane}\\ (\\mathrm{CH}_4)$
Contains( methane, C1 )
Contains( methane, H1 )
Contains( methane, H2 )
Contains( methane, H3 )
Contains( methane, H4 )
SingleBond( C1, H1 )
SingleBond( C1, H2 )
SingleBond( C1, H3 )
SingleBond( C1, H4 )

Molecule oxygen1, oxygen2
Label oxygen1 $\\text{oxygen}\\ (\\mathrm{O}_2)$
Label oxygen2 $\\text{oxygen}\\ (\\mathrm{O}_2)$
Contains( oxygen1, O1 )
Contains( oxygen1, O2 )
DoubleBond( O1, O2 )
Contains( oxygen2, O3 )
Contains( oxygen2, O4 )
DoubleBond( O3, O4 )

IsReactant( methane )
IsReactant( oxygen1 )
IsReactant( oxygen2 )

-- products: CO2 + 2H20 ====================================
Carbon C2
Hydrogen H5, H6, H7, H8
Oxygen O5, O6, O7, O8

Molecule carbonDioxide
Label carbonDioxide $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide, C2 )
Contains( carbonDioxide, O5 )
Contains( carbonDioxide, O6 )
DoubleBond( C2, O5 )
DoubleBond( C2, O6 )

Molecule water1, water2
Label water1 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Label water2 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water1, O7 )
Contains( water1, H5 )
Contains( water1, H6 )
SingleBond( O7, H5 )
SingleBond( O7, H6 )
Contains( water2, O8 )
Contains( water2, H7 )
Contains( water2, H8 )
SingleBond( O8, H7 )
SingleBond( O8, H8 )

IsProduct( carbonDioxide )
IsProduct( water1 )
IsProduct( water2 )

`,Vr=`AutoLabel All

-- photosynthesis, expressed via structural-formula DSL

Reaction photosynthesis
Label photosynthesis "Photosynthesis"
IsNetForward( photosynthesis )

-- reactants: 6H20 + 6CO2 ====================================
Molecule water0, water1, water2, water3, water4, water5
Molecule carbonDioxide0, carbonDioxide1, carbonDioxide2, carbonDioxide3, carbonDioxide4, carbonDioxide5

Hydrogen H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB
Oxygen O0, O1, O2, O3, O4, O5
Carbon C0, C1, C2, C3, C4, C5, C6
Oxygen O6, O7, O8, O9, OA, OB
Oxygen OC, OD, OE, OF, OG, OH

Label water0 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water0, H0 )
Contains( water0, H1 )
Contains( water0, O0 )
SingleBond( O0, H0 )
SingleBond( O0, H1 )

Label water1 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water1, H2 )
Contains( water1, H3 )
Contains( water1, O1 )
SingleBond( O1, H2 )
SingleBond( O1, H3 )

Label water2 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water2, H4 )
Contains( water2, H5 )
Contains( water2, O2 )
SingleBond( O2, H4 )
SingleBond( O2, H5 )

Label water3 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water3, H6 )
Contains( water3, H7 )
Contains( water3, O3 )
SingleBond( O3, H6 )
SingleBond( O3, H7 )

Label water4 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water4, H8 )
Contains( water4, H9 )
Contains( water4, O4 )
SingleBond( O4, H8 )
SingleBond( O4, H9 )

Label water5 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water5, HA )
Contains( water5, HB )
Contains( water5, O5 )
SingleBond( O5, HA )
SingleBond( O5, HB )

Label carbonDioxide0 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide0, C0 )
Contains( carbonDioxide0, O6 )
Contains( carbonDioxide0, O7 )
DoubleBond( C0, O6 )
DoubleBond( C0, O7 )

Label carbonDioxide1 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide1, C1 )
Contains( carbonDioxide1, O8 )
Contains( carbonDioxide1, O9 )
DoubleBond( C1, O8 )
DoubleBond( C1, O9 )

Label carbonDioxide2 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide2, C2 )
Contains( carbonDioxide2, OA )
Contains( carbonDioxide2, OB )
DoubleBond( C2, OA )
DoubleBond( C2, OB )

Label carbonDioxide3 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide3, C3 )
Contains( carbonDioxide3, OC )
Contains( carbonDioxide3, OD )
DoubleBond( C3, OC )
DoubleBond( C3, OD )

Label carbonDioxide4 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide4, C4 )
Contains( carbonDioxide4, OE )
Contains( carbonDioxide4, OF )
DoubleBond( C4, OE )
DoubleBond( C4, OF )

Label carbonDioxide5 $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide5, C5 )
Contains( carbonDioxide5, OG )
Contains( carbonDioxide5, OH )
DoubleBond( C5, OG )
DoubleBond( C5, OH )

IsReactant( water0 )
IsReactant( water1 )
IsReactant( water2 )
IsReactant( water3 )
IsReactant( water4 )
IsReactant( water5 )

IsReactant( carbonDioxide0 )
IsReactant( carbonDioxide1 )
IsReactant( carbonDioxide2 )
IsReactant( carbonDioxide3 )
IsReactant( carbonDioxide4 )
IsReactant( carbonDioxide5 )

-- -- products: C6H12O6 + 6O2 =====================================================
-- Molecule glucose
-- Molecule oxygen0, oxygen1, oxygen2, oxygen3, oxygen4, oxygen5
-- Carbon c0, c1, c2, c3, c4, c5, c6
-- Hydrogen h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, ha, hb
-- Oxygen o0, o1, o2, o3, o4, o5
-- Oxygen o6, o7, o8, o9, oa, ob
-- Oxygen oc, od, oe, of, og, oh
-- 
-- Label glucose $\\text{glucose}\\ (\\mathrm{C}_6\\mathrm{H}_{12}\\mathrm{O}_6)$
-- -- TODO finish me!

`,Nr=`AutoLabel All

-- sodium chlorine gas reaction, expressed via structural-formula DSL

Reaction sodium_chlorine
Label sodium_chlorine "Sodium and Chlorine Gas Reaction"
IsNetForward( sodium_chlorine )

-- reactants: 2Na + Cl2 ====================================
Sodium Na1, Na2
Chlorine Cl1, Cl2

Molecule chlorine
Label chlorine $\\text{chlorine gas}\\ (\\mathrm{Cl}_2)$
Contains( chlorine, Cl1 )
Contains( chlorine, Cl2 )
SingleBond( Cl1, Cl2 )

Molecule sodium1, sodium2
Label sodium1 $\\text{sodium}\\ (\\mathrm{Na})$
Label sodium2 $\\text{sodium}\\ (\\mathrm{Na})$
Contains( sodium1, Na1 )
Contains( sodium2, Na2 )

IsReactant( chlorine )
IsReactant( sodium1 )
IsReactant( sodium2 )

-- products: 2NaCl =========================================
Sodium Na3, Na4
Chlorine Cl3, Cl4

Molecule sodiumChloride1, sodiumChloride2
Label sodiumChloride1 $\\text{salt}\\ (\\mathrm{NaCl})$
Label sodiumChloride2 $\\text{salt}\\ (\\mathrm{NaCl})$
Contains( sodiumChloride1, Na3 )
Contains( sodiumChloride1, Cl3 )
Contains( sodiumChloride2, Na4 )
Contains( sodiumChloride2, Cl4 )
IonicBond( Na3, Cl3 )
IonicBond( Na4, Cl4 )

IsProduct( sodiumChloride1 )
IsProduct( sodiumChloride2 )

`,_r={"methane-combustion.substance":Ir,"photosynthesis.substance":Vr,"sodium-chlorine.substance":Nr},Qr=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 24.2.3, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 432 432" style="enable-background:new 0 0 432 432;" xml:space="preserve">
<style type="text/css">
	.st0{fill:url(#SVGID_1_);}
</style>
<radialGradient id="SVGID_1_" cx="216" cy="216" r="216" gradientUnits="userSpaceOnUse">
	<stop  offset="0" style="stop-color:#000000"/>
	<stop  offset="0.2365" style="stop-color:#020202"/>
	<stop  offset="0.3732" style="stop-color:#0B0B0B"/>
	<stop  offset="0.4846" style="stop-color:#191919"/>
	<stop  offset="0.5822" style="stop-color:#2D2D2D"/>
	<stop  offset="0.6709" style="stop-color:#474747"/>
	<stop  offset="0.7531" style="stop-color:#666666"/>
	<stop  offset="0.8303" style="stop-color:#8C8C8C"/>
	<stop  offset="0.9036" style="stop-color:#B8B8B8"/>
	<stop  offset="0.9714" style="stop-color:#E8E8E8"/>
	<stop  offset="1" style="stop-color:#FFFFFF"/>
</radialGradient>
<circle class="st0" cx="216" cy="216" r="216"/>
</svg>
`,qr=`-- structural-formula.domain
--
-- This Penrose Domain schema is used to encode molecular
-- structures in a format suitable for drawing a variety
-- of different kinds of molecular structure diagrams.

-- a Node is any collection of atoms that is treated
-- as a single logical unit, such as a functional
-- group (or just a single atom)
type Node

-- a FunctionalGroup represents a collection of atoms
-- in the same molecule (such as an alcohol or ester)
type FunctionalGroup <: Node

-- an Atom represents a single atom within a larger
-- molecule (or as an isolated ion)
type Atom <: Node

-- specific types of atoms (more could be added here)
type Hydrogen <: Atom
type   Carbon <: Atom
type Nitrogen <: Atom
type   Oxygen <: Atom
type   Sodium <: Atom
type Chlorine <: Atom

-- predicates used to specify bonds between Nodes
predicate SingleBond(Node n1, Node n2)
predicate DoubleBond(Node n1, Node n2)
predicate  IonicBond(Node n1, Node n2)

-- a Molecule is a collection of Atoms, or more generally,
-- Nodes, held together by bonds.  It is not essential to
-- annotate which collections of Nodes are connected, but
-- grouping Nodes is helpful for, e.g., labeling molecules
-- and/or grouping reactants/products.
type Molecule

predicate Contains(Molecule m, Node n)

-- these predicates are used to delineate reactants and
-- produces in a chemical equation
predicate IsReactant(Molecule m)
predicate IsProduct(Molecule m)

-- a reaction involving all reactants and products
type Reaction

-- predicates to mark the type of reaction
predicate     IsNetForward(Reaction r)
predicate IsStoichiometric(Reaction r)
predicate    IsEquilibrium(Reaction r)
predicate  IsBidirectional(Reaction r)

`,Rr=`canvas {
   scalar width  = 1066.5
   scalar height = 600.0
}

Colors {
   vec4 clear     = rgba( 0., 0., 0., 0. )
   vec4 black     = rgba( 0., 0., 0., 1. )
   vec4 gray      = rgba( .5, .5, .5, 1. )
   vec4 white     = rgba( 1., 1., 1., 1. )
   vec4 red       = rgba( 1., 0., 0., 1. )
   vec4 blue      = rgba( 0., 0., 1., 1. )
   vec4 darkRed   = rgba( .7, 0., 0., 1. )
   vec4 darkBlue  = rgba( 0., 0., .7, 1. )
}

Global {
   scalar atomRadius = 20.
   scalar bondLength = 60.

   shape bbox = Rectangle {
      width : canvas.width
      height : canvas.height
      center : (0.,0.)
      fillColor : Colors.clear
      strokeColor : Colors.gray
   }
}

forall Node n {

   vec2 n.center = (?,?)

   shape n.icon = Circle {
      r : Global.atomRadius
      center : n.center
      fillColor : Colors.white
      strokeColor : Colors.black
      strokeWidth : 2.
   }

   shape n.background = Image {
      href : "structural-formula-atom.svg"
      center : n.center
      width : 4.*Global.atomRadius
      height : 4.*Global.atomRadius
   }

   shape n.text = Text {
      string: n.label
      center: n.center
      fillColor: Colors.black
      fontSize: "12px"
      fontFamily: "HelveticaNeue-CondensedBold, Helvetica Neue, Helvetica, Arial, sans-serif"
      fontWeight: "Bold"
   }

   ensure contains( Global.bbox, n.icon, 0. )

   layer n.icon below n.text
   layer n.background below n.icon
   layer n.icon above Global.bbox
   layer n.background above Global.bbox
   layer n.text above Global.bbox
}

forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.text.fillColor = Colors.black
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.red
   override a.icon.strokeColor = Colors.darkRed
   override a.text.fillColor = Colors.white
   override a.text.string = "H"
}

forall Carbon a {
   override a.icon.fillColor = Colors.black
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.white
   override a.text.string = "C"
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.blue
   override a.icon.strokeColor = Colors.darkBlue
   override a.text.fillColor = Colors.white
   override a.text.string = "N"
}

forall Oxygen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.black
   override a.text.string = "O"
}

forall Node n1; Node n2
where SingleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - 1.3*r*u
        end : x2 + 1.3*r*u
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line below n1.icon
   layer line below n2.icon
   layer n1.background below line
   layer n2.background below line
}

forall Node n1; Node n2
where DoubleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   vec2 v = ( -u[1], u[0] )
   scalar r = Global.atomRadius

   shape line1 = Line {
      start : x1 - 1.3*r*u - .25*r*v
        end : x2 + 1.3*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   shape line2 = Line {
      start : x1 - 1.3*r*u + .25*r*v
        end : x2 + 1.3*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 below n1.icon
   layer line2 below n1.icon
   layer line1 below n2.icon
   layer line2 below n2.icon
   layer n1.background below line1
   layer n2.background below line1
   layer n1.background below line2
   layer n2.background below line2
}

forall Node n1; Node n2 {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
   layer n1.background below n2.icon
   layer n2.background below n1.icon
}

`,Dr={images:yr,molecules:Br,"pseudo-3d-reaction.style":Mr,"pseudo-3d.style":Gr,reactions:_r,"structural-formula-atom.svg":Qr,"structural-formula.domain":qr,"structural-formula.style":Rr},$r=`-- angle-equivalence.substance
-- 
-- This example re-diagrams Figure 1 from
-- Crane, "Conformal Geometry of Simplicial Surfaces"

-- Initial mesh

Vertex i, j, k, l -- for the two "inner" triangles
Vertex a, b, c -- for the three "ears"

Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge lj := MakeEdge( l, j )
Edge ka := MakeEdge( k, a )
Edge ja := MakeEdge( j, a )
Edge ib := MakeEdge( i, b )
Edge kb := MakeEdge( k, b )
Edge lc := MakeEdge( l, c )
Edge ic := MakeEdge( i, c )

IsBoundaryEdge( lj )
IsBoundaryEdge( ka )
IsBoundaryEdge( ja )
IsBoundaryEdge( ib )
IsBoundaryEdge( kb )
IsBoundaryEdge( lc )
IsBoundaryEdge( ic )

Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )
Triangle kja := MakeTriangle( k, j, a )
Triangle ikb := MakeTriangle( i, k, b )
Triangle lic := MakeTriangle( l, i, c )

Corner cjil := MakeCorner( j, i, l )
Corner cilj := MakeCorner( i, l, j )
Corner clji := MakeCorner( l, j, i )

-- Same mesh, under a similarity transformation

Vertex I := similarity(i)
Vertex J := similarity(j)
Vertex K := similarity(k)
Vertex L := similarity(l)
Vertex A := similarity(a)
Vertex B := similarity(b)
Vertex C := similarity(c)

Edge IJ := MakeEdge( I, J )
Edge JK := MakeEdge( J, K )
Edge KI := MakeEdge( K, I )
Edge IL := MakeEdge( I, L )
Edge LJ := MakeEdge( L, J )
Edge KA := MakeEdge( K, A )
Edge JA := MakeEdge( J, A )
Edge IB := MakeEdge( I, B )
Edge KB := MakeEdge( K, B )
Edge LC := MakeEdge( L, C )
Edge IC := MakeEdge( I, C )

IsBoundaryEdge( LJ )
IsBoundaryEdge( KA )
IsBoundaryEdge( JA )
IsBoundaryEdge( IB )
IsBoundaryEdge( KB )
IsBoundaryEdge( LC )
IsBoundaryEdge( IC )

Triangle IJK := MakeTriangle( I, J, K )
Triangle JIL := MakeTriangle( J, I, L )
Triangle KJA := MakeTriangle( K, J, A )
Triangle IKB := MakeTriangle( I, K, B )
Triangle LIC := MakeTriangle( L, I, C )

Corner cJIL := MakeCorner( J, I, L )
Corner cILJ := MakeCorner( I, L, J )
Corner cLJI := MakeCorner( L, J, I )


Label cjil "α"
Label cilj "β"
Label clji "γ"
Label cJIL "α"
Label cILJ "β"
Label cLJI "γ"
-- HasLabel( i )
-- HasLabel( j )
-- HasLabel( k )



`,Wr=`-- concyclic-pair.substance
-- 
-- This example re-diagrams Figure 9 from
-- Gillespie et al, "Discrete Conformal Equivalence
-- of Polyhedral Surfaces"

Vertex i, j, k, l

Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Edge il := MakeEdge(i,l)
Edge lj := MakeEdge(l,j)
Edge kl := MakeEdge(k,l)
IsFlipped(kl)
IsBoundaryEdge(jk)
IsBoundaryEdge(ki)
IsBoundaryEdge(il)
IsBoundaryEdge(lj)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)
Concyclic( ijk, jil )

Corner cK := MakeCorner(k,i,j)
Label cK "α"
Corner cL := MakeCorner(l,j,i)
Label cL "β"
Corner cI := MakeCorner(i,l,k)
Label cI "β’"
Corner cJ := MakeCorner(j,k,l)
Label cJ "α’"

`,Fr=`Vertex i, j, k, l

Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge jl := MakeEdge( j, l )

Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )

Corner ckij := MakeCorner( k, i, j )
Corner clji := MakeCorner( l, j, i )

DualEdge starij := MakeDualEdge( ijk, jil )

Point x := Circumcenter(ijk)
Point y := Circumcenter(jil)

Circle Cijk := Circumcircle(ijk)
Circle Cjil := Circumcircle(jil)

-- Length L := DualEdgeLength(starij)
-- Length L := EdgeLength(ki)

AutoLabel All
NoLabel ij
NoLabel jk
NoLabel ki
NoLabel il
NoLabel jl
Label ckij "α"
Label clji "β"
Label i "i"
Label j "j"
Label k "k"
Label l "l"
`,jr=`Vertex i, j, k, l, m
Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge il := MakeEdge( i, l )
Edge im := MakeEdge( i, m )

AutoLabel All
`,Hr=`Vertex i, j, k, l, m

Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge jl := MakeEdge( j, l )
Edge jm := MakeEdge( j, m )
Edge km := MakeEdge( k, m )

Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )
Triangle jmk := MakeTriangle( j, m, k )

Corner ckij := MakeCorner( k, i, j )
Corner clji := MakeCorner( l, j, i )

--DualEdge starij := MakeDualEdge( ijk, jil )

--Point x := Circumcenter(ijk)
--Point y := Circumcenter(jil)

Label ckij "α"
Label clji "β"
`,zr=`Vertex i, j, k, l, m

Edge eij := MakeEdge(i,j)
Edge ejk := MakeEdge(j,k)
Edge eki := MakeEdge(k,i)
Edge eil := MakeEdge(i,l)
Edge elj := MakeEdge(l,j)
Edge eim := MakeEdge(i,m)
Edge eml := MakeEdge(m,l)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)
Triangle iml := MakeTriangle(i,m,l)

Halfedge hij := MakeHalfedge(i,j)
Halfedge hji := MakeHalfedge(j,i)
Halfedge hjk := MakeHalfedge(j,k)
Halfedge hki := MakeHalfedge(k,i)
Halfedge hil := MakeHalfedge(i,l)
Halfedge hli := MakeHalfedge(l,i)

Label hij "h"
Label hji "h->twin"
Label hjk "next"
Label hki "next->next"
Label hil "twin->next"
Label hli "twin->next->twin"
`,Ur=`-- length-cross-ratio.substance
-- 
-- This example re-diagrams Figure 14 (left) from
-- Crane, "Conformal Geometry of Simplicial Surfaces"

Vertex i, j, k, l

Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Edge jl := MakeEdge(j,l)
Edge li := MakeEdge(l,i)

IsBoundaryEdge(jk)
IsBoundaryEdge(ki)
IsBoundaryEdge(jl)
IsBoundaryEdge(li)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)

AutoLabel All
NoLabel ij
Label jk $\\ell_{jk}$
Label ki $\\ell_{ki}$
Label jl $\\ell_{jl}$
Label li $\\ell_{li}$
HasLabel( i )
HasLabel( j )
HasLabel( k )
HasLabel( l )
`,Kr=`-- relative-orientation.substance
-- 
-- This example re-diagrams a figure from
-- Crane, "Discrete Differential Geometry: An
-- Applied Introduction", Section 2.2

Vertex i, j, k, l

Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Edge jl := MakeEdge(j,l)
Edge li := MakeEdge(l,i)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)

IsPositivelyOriented(ijk)
IsNegativelyOriented(jil)
Label i "i"
Label j "j"
Label k "k"
Label l "l"

`,Xr=`Vertex a, b, c, d
Edge ab := MakeEdge(a,b)
Edge cd := MakeEdge(c,d)
Point p := Intersection(ab,cd)
`,Yr=`Vertex i, j, k, l
Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge jl := MakeEdge( j, l )
Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )

AutoLabel All
`,Zr=`-- create a triangle with vertices i, j, k
Vertex i, j, k
Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Triangle ijk := MakeTriangle(i,j,k)

-- draw two different triangle centers
Point a, b
a := Circumcenter(ijk)
b := Incenter(ijk)

Circle Ca := Circumcircle(ijk)
Circle Cb := Incircle(ijk)

AutoLabel All
NoLabel ij
NoLabel jk
NoLabel ki
Label i "i"
Label j "j"
Label k "k"
`,Jr={"angle-equivalence.substance":$r,"concyclic-pair.substance":Wr,"cotan-formula.substance":Fr,"edge-label-test.substance":jr,"example.substance":Hr,"halfedge-mesh.substance":zr,"length-cross-ratio.substance":Ur,"relative-orientation.substance":Kr,"segment-intersection-test.substance":Xr,"triangle-label-test.substance":Yr,"triangleCenters.substance":Zr},na=`type Point
type Vertex
type Triangle

constructor MakeTriangle(Vertex i, Vertex j, Vertex k) -> Triangle

constructor Circumcenter(Triangle t) -> Point
constructor Incenter(Triangle t) -> Point

`,ea=`canvas {
   width = 360
   height = 270
}

Colors {
   color black = rgba(0.,0.,0.,1.)
   color red = rgba(1.,0.,0.,1.)
   color green = rgba(0.,.7,0.,1.)
   color blue = rgba(0.,0,1.,1.)
   color white = rgba(1.,1.,1.,1.)
   color lightGray = rgba(.8,.8,.8,1.)
   color clear = rgba(0.,0.,0.,0.)
   color semiBlue = rgba( 27./255., 31./255., 138./255., .2 )
}

Global {
   scalar vertexRadius = 4.
   scalar pointRadius = 2.

   shape box = Rectangle {
      center: (0.,0.)
      color: Colors.clear
      strokeColor: Colors.lightGray
      strokeWidth: 2.
      w: canvas.width
      h: canvas.height
   }
}

forall Vertex v {

   vec2 v.center = (?,?)

   -- black dot
   shape v.icon = Square {
      color: Colors.black
      side: Global.vertexRadius
      center: v.center
   }

   -- make sure the dot is on the canvas
   ensure contains( Global.box, v.icon )
}

forall Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k)
{
   vec2 a = i.icon.center
   vec2 b = j.icon.center
   vec2 c = k.icon.center

   -- Polygon is currently broken, so we draw the
   -- triangle as a Path instead.
   shape t.icon = Path {
      pathData: pathFromPoints("closed", [a,b,c])
      fill: rgba( 1., .7, 0., .2 )
      color: none()
   }

   -- Make sure triangles are positively
   -- oriented and not tiny, by making their
   -- signed area greater than some fixed constant.
   scalar signedArea = cross2D( b-a, c-a )
   encourage lessThan( 2000., signedArea )

   -- Make sure we don't get a skinny triangle by
   -- keeping two of the angles in a reasonable range
   -- (the third will of course make the sum equal to pi...)
   scalar aTheta = angleFrom( b-a, c-a )
   scalar bTheta = angleFrom( c-b, a-b )
   ensure inRange( aTheta, toRadians(30.), toRadians(80.) )
   ensure inRange( bTheta, toRadians(20.), toRadians(90.) )
}

-- Draw all points as stroked dots
forall Point p {

   vec2 p.center = (?,?)

   -- white dot
   shape p.icon = Circle {
      color: Colors.white
      strokeColor: Colors.black
      r: Global.pointRadius
      center: p.center
      strokeWidth: 1.
   }

   -- make sure the dot and label are both
   -- on the canvas
   ensure contains( Global.box, p.icon )
}

-- Draw all points above all triangles
forall Point p; Triangle t {
   layer p.icon above t.icon
}

-- Circumcenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Circumcenter(t) {

   -- put point at circumcenter 
   vec2 x = circumcenter( i.center, j.center, k.center )
   override p.center = x
   override p.icon.color = Colors.green
}

-- Incenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Incenter(t) {

   -- put point at incenter 
   vec2 p.inCenter = incenter( i.center, j.center, k.center )
   override p.center = p.inCenter
   override p.icon.color = Colors.blue
}

`,ta=`-- Mesh combinatorics
type Vertex
type Edge
type Halfedge
type DualEdge
type Triangle
type Corner

constructor MakeEdge(Vertex i, Vertex j) -> Edge
constructor MakeHalfedge(Vertex from, Vertex to) -> Halfedge
constructor MakeTriangle(Vertex i, Vertex j, Vertex k) -> Triangle
constructor MakeCorner( Vertex inner, Vertex outer1, Vertex outer2 ) -> Corner
constructor MakeDualEdge(Triangle a, Triangle b) -> DualEdge

predicate IsBoundaryVertex(Vertex v)
predicate IsBoundaryEdge(Edge e)
predicate IsBoundaryTriangle(Triangle t)

predicate HasLabel(Vertex v)

predicate IsPositivelyOriented(Triangle t)
predicate IsNegativelyOriented(Triangle t)

-- Geometry
type Point
type Circle
type Length

constructor Barycenter(Triangle t) -> Point
constructor Circumcenter(Triangle t) -> Point
constructor Incenter(Triangle t) -> Point

constructor Circumcircle(Triangle t) -> Circle
constructor Incircle(Triangle t) -> Circle

constructor EdgeLength(Edge e) -> Length
constructor DualEdgeLength(DualEdge d) -> Length

constructor Intersection(Edge e, Edge f) -> Point

-- Specific to angle-equivalence.substance
function similarity(Vertex i) -> Vertex

-- Specific to concyclic-pair.substance
predicate IsFlipped(Edge e)
predicate Concyclic(Triangle s, Triangle t)

`,oa=`layout = [shapes, labels]

canvas {
   width = 480
   height = 360
}

Colors {
   color black = rgba(0.,0.,0.,1.)
   color red = rgba(1.,0.,0.,1.)
   color green = rgba(0.,.7,0.,1.)
   color blue = rgba(0.,0,1.,1.)
   color white = rgba(1.,1.,1.,1.)
   color lightGray = rgba(.8,.8,.8,1.)
   color clear = rgba(0.,0.,0.,0.)
   color darkBlue = rgba( 27./255., 31./255., 138./255., 1. )
   color semiBlue = rgba( 27./255., 31./255., 138./255., .2 )
}

Global {
   scalar vertexRadius = 2.5
   scalar pointRadius = 2.0
   scalar lineThickness = 1.
   scalar labelBoundRadius = 8.
   scalar lineOffset = 8.0
   scalar wedgeSize = 30.0

   shape box = Rectangle {
      center: (0.,0.)
      fillColor: none()
      strokeColor: Colors.lightGray
      strokeWidth: 2.
      width: canvas.width
      height: canvas.height
   }

   -- string fontFamily = "Linux Libertine O"
   string fontFamily = "Palatino"
   string fontSize = "14px"
}

forall Vertex v {

   vec2 v.center = (?,?)

   -- black dot
   shape v.icon = Circle {
      fillColor: Colors.black
      r: Global.vertexRadius
      center: v.center
   }

   -- make sure the dot is on the canvas
   constraint v.onCanvas = ensure contains( Global.box, v.icon )
}

forall Vertex v
where v has label {

   v.labelAngle = ? in labels
   v.labelRadius = ? in labels

   -- label
   shape v.text = Text {
      center: v.icon.center + v.labelRadius*( cos(v.labelAngle), sin(v.labelAngle ) )
      string: v.label
      fontFamily: Global.fontFamily
      fontSize: Global.fontSize
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- make sure label doesn't get too far from the dot
   ensure inRange( v.labelRadius, 1.02*Global.labelBoundRadius, 1.1*Global.labelBoundRadius ) in labels

   -- invisible circle around label
   -- (used to prevent overlap)
   -- TODO requires #741 to use actual text bounding box instead
   shape v.bounds = Circle {
      center: v.text.center
      r: Global.labelBoundRadius
      fillColor: none()
   }

   -- make sure the label is on the canvas
   ensure contains( Global.box, v.bounds ) in labels

   -- keep the label near the dot
   encourage equal( norm(v.text.center - v.icon.center), Global.labelBoundRadius ) in labels
}

-- make sure vertex labels don't overlap
forall Vertex u; Vertex v
where u has label; v has label { -- requires #774 to replace with "where u has label; v has label"; currently hacked-in using predicate in Domain program
   ensure disjoint( u.bounds, v.bounds ) in labels
}

forall Edge e; Vertex i; Vertex j
where e := MakeEdge(i,j) {

   -- grab edge endpoints
   vec2 pi = i.icon.center
   vec2 pj = j.icon.center
   
   -- black line between endpoints
   shape e.segment = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      strokeLinecap: "round"
   }

   -- (DEBUG) draw full line through segment
   -- vec2 m = (pi+pj)/2.
   -- shape e.line = Line {
   --    start: m + 100.*(pi-m)
   --    end: m + 100.*(pj-m)
   --    strokeColor: rgba( 0., 0., 0., .35 )
   --    strokeWidth: Global.lineThickness
   --    style: "dashed"
   --    strokeDasharray: "4,4"
   -- }
}

forall Edge e; Vertex i; Vertex j
where e := MakeEdge(i,j); i has label; j has label {
   -- make sure edge doesn't cover the
   -- labels of its two endpoints
   ensure disjoint( i.bounds, e.segment, 1. ) in labels
   ensure disjoint( j.bounds, e.segment, 1. ) in labels
}

forall Edge e
where IsBoundaryEdge( e ) {
   override e.segment.strokeWidth = 1.5*Global.lineThickness
}

-- make sure no edge ij covers the label of
-- any other vertex k
forall Edge e; Vertex i; Vertex j; Vertex k
where e := MakeEdge(i,j); k has label {
   
   ensure disjoint( k.bounds, e.segment, 1. ) in labels
}

forall Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k)
{
   vec2 a = i.icon.center
   vec2 b = j.icon.center
   vec2 c = k.icon.center

   -- Define circles associated with each triangle, even
   -- if they're never used, so that they can be easily
   -- referenced by other rules.  (The overhead here is
   -- minimal, especially because we don't need to
   -- differentiate through these quantities unless they
   -- are explicitly incorporated into some later ensure/
   -- encourage statement.
   vec2 t.circumcenter = circumcenter(a,b,c)
   scalar t.circumradius = circumradius(a,b,c)
   vec2 t.incenter = incenter(a,b,c)
   scalar t.inradius = inradius(a,b,c)

   shape t.icon = Path {
      d: pathFromPoints("closed", [a,b,c])
      fillColor: Colors.semiBlue
      strokeColor: none()
   }

   -- Make sure triangles are positively
   -- oriented and not tiny, by making their
   -- signed area greater than some fixed constant.
   scalar A = cross2D( b-a, c-a )
   objective t.positiveArea = encourage lessThan( 4000., A )

   -- Also ensure triangles are not slivers by making
   -- sure two of their angles are in a reasonable range
   -- (the third will of course make the sum equal to 180)
   scalar aTheta = angleFrom( b-a, c-a )
   scalar bTheta = angleFrom( c-b, a-b )
   constraint t.goodAngleA = ensure inRange( aTheta, toRadians(40.), toRadians(80.) ) in labels
   constraint t.goodAngleB = ensure inRange( bTheta, toRadians(40.), toRadians(80.) ) in labels
}

forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k) {

   vec2 p = i.center
   vec2 q = j.center
   vec2 r = k.center
   
   scalar s = .25 -- arc radius as fraction of edge length
   scalar c.R = Global.wedgeSize -- s*norm(q-p)
   vec2 c.u = (q-p)/norm(q-p)
   vec2 c.v = (r-p)/norm(r-p)
   vec2 x = p + c.R*c.u
   vec2 y = p + c.R*c.v

   -- for use in later constraints
   c.wedgeAngle = angleBetween( q-p, r-p )

   shape c.arc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", x, y, (c.R,c.R), 0., 0, 0 )
      strokeWidth: .75
   }

   shape c.arcFill = Path {
      d: wedge( i.center, x, y, (c.R,c.R), 0., 0, 0 )
      fillColor: Colors.semiBlue
      strokeColor: none()
   }

   shape c.arcBall = Circle {
      center: i.center
      r: c.R
      fillColor: none()
      strokeColor: none()
   }
}

forall Corner c; Vertex i; Vertex j; Vertex k
where c := MakeCorner(i,j,k); c has text label {

   vec2 c.center = (?,?)
   vec2 p = i.center
   scalar R0 = Global.labelBoundRadius

   shape c.text = Text {
      string: c.label
      center: c.center
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- shape c.text = Text {
   --    string: c.label
   --    center: p + (c.R+R0)*unit(c.u+c.v)
   --    fontSize: Global.fontSize
   --    fontFamily: Global.fontFamily
   --    fontStyle: "italic"
   --    fillColor: Colors.black
   -- }

   shape c.textBounds = Circle {
      center: c.text.center
      r: 1.1 * .5 * max(c.text.width, c.text.height)
      fillColor: none()
   }

   vec2 w = (c.u+c.v)/2.
   vec2 z = c.center - i.center
   ensure lessThan( angleBetween(w,z), c.wedgeAngle/4. )
   encourage near( c.textBounds, i.icon ) in labels
   ensure disjoint( c.textBounds, c.arcBall ) in labels
}

forall Corner c1; Corner c2
where c1 has text label; c2 has text label {
   ensure disjoint( c1.textBounds, c2.textBounds ) in labels
}

forall Point p {

   vec2 p.center = (?,?)

   -- white dot
   shape p.icon = Circle {
      fillColor: Colors.white
      strokeColor: Colors.black
      r: Global.pointRadius
      center: p.center
      strokeWidth: 1.
   }

   -- label
   shape p.text = Text {
      center: (?,?)
      string: ""
      fontFamily: Global.fontFamily
      fontSize: Global.fontSize
      fontStyle: "italic"
      fillColor: Colors.black
   }

   -- make sure the dot and label are both
   -- on the canvas
   ensure contains( Global.box, p.icon )

   -- keep the label near the dot
   -- encourage equal( norm(p.text.center - p.icon.center), 0. )
}

-- draw all points above all triangles
forall Point p; Triangle t {
   layer p.icon above t.icon
}

-- draw halfedges as "hook" arrows
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(i,j); t := MakeTriangle(i,j,k)
{
   h.start = i.center
   h.end = j.center
   h.opposite = k.center
}
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(j,k); t := MakeTriangle(i,j,k)
{
   h.start = j.center
   h.end = k.center
   h.opposite = i.center
}
forall Halfedge h; Triangle t; Vertex i; Vertex j; Vertex k
where h := MakeHalfedge(k,i); t := MakeTriangle(i,j,k)
{
   h.start = k.center
   h.end = i.center
   h.opposite = j.center
}
forall Halfedge e {

   vec2 p0 = e.start
   vec2 p1 = e.end
   vec2 p2 = e.opposite

   -- unit vectors along the three edges
   vec2 t01 = unit(p1-p0)
   vec2 t12 = unit(p2-p1)
   vec2 t20 = unit(p0-p2)

   -- angle bisectors at the two endpoints
   vec2 z0 = unit(t01-t20)
   vec2 z1 = unit(t12-t01)

   -- angles at the two endpoints
   scalar alpha0 = angleBetween( p1-p0, p2-p0 )
   scalar alpha1 = angleBetween( p2-p1, p0-p1 )

   -- compute corners of constant-width inset
   scalar h = Global.lineOffset
   scalar r0 = h / sin(alpha0/2.)
   scalar r1 = h / sin(alpha1/2.)
   vec2 q0 = p0 + r0*z0
   vec2 q1 = p1 + r1*z1

   -- compute truncated segment endpoints
   scalar L = norm(p1-p0)
   vec2 m = (q0+q1)/2.
   vec2 Q0 = m + (L/4.)*t01
   vec2 Q1 = m - (L/4.)*t01

   vec2 n = rot90(t01)
   vec2 Q2 = Q0 + h*(n-t01)

   e.icon = Path {
      strokeWidth: 2.*Global.lineThickness
      strokeColor: Colors.black
      fillColor: none()
      d: pathFromPoints("open", [Q2,Q0,Q1])
   }

   e.labelText = Text {
      center: m + h*n
      string: e.label
      fontFamily: "Courier"
      fontSize: "6.75px"
      fillColor: Colors.black
      rotation: -toDegrees(angleOf(t01))
   }
}

-- default shape for a circle
forall Circle C {

   shape C.icon = Circle {
      center: (?,?)
      r: ?
      fillColor: Colors.semiBlue
   }

   constraint C.validRadius = ensure inRange( C.icon.r, 100, 200 )
   constraint C.onCanvas = ensure contains(Global.box, C.icon)
}

-- Barycenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Barycenter(t) {
   override p.center = barycenter( i.center, j.center, k.center )
   override p.icon.fillColor = Colors.red
}

-- Circumcenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Circumcenter(t) {
   -- put point at circumcenter 
   override p.center = t.circumcenter
}

-- Circumcircle
forall Circle C; Triangle t
where C := Circumcircle(t) {

   -- since the circumcircle can be huge, it's
   -- too restrictive to force it on the canvas
   delete C.onCanvas

   -- the circumradius will be valid by construction
   delete C.validRadius

   override C.icon.center = t.circumcenter
   override C.icon.r = t.circumradius
   override C.icon.fillColor = none()
   override C.icon.strokeColor = rgba( 0., 0., 0., .2 )
   override C.icon.strokeWidth = 1.5
}

-- Incenter
forall Point p; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); p := Incenter(t) {

   -- put point at incenter 
   override p.center = t.incenter
}

-- Incircle
forall Circle C; Triangle t; Vertex i; Vertex j; Vertex k
where t := MakeTriangle(i,j,k); C := Incircle(t) {
   
   delete C.onCanvas
   delete C.validRadius

   override C.icon.center = t.incenter
   override C.icon.r = t.inradius
   override C.icon.fillColor = none()
   override C.icon.strokeColor = rgba( 0., 0., 0., .2 )
   override C.icon.strokeWidth = 1.5
}

forall DualEdge d; Triangle t1; Triangle t2
where d := MakeDualEdge(t1,t2) {

   shape d.segment = Line {
      start: t1.circumcenter
      end: t2.circumcenter
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      style: "dashed"
      strokeDasharray: "4,4"
   }
}

-- if a length is constructed from an edge, set its
-- endpoints to the edge endpoints (setting things
-- up this way allows us to keep Length fairly generic,
-- e.g., it can also be used to draw the length of a
-- dual edge without duplicating code)
forall Length l; Edge e; Vertex i; Vertex j
where l := EdgeLength(e); e := MakeEdge(i,j) {
   vec2 l.x0 = e.segment.start
   vec2 l.x1 = e.segment.end
}
   
-- if a length is constructed from a dual edge, set its
-- endpoints to the dual edge endpoints
forall Length l; DualEdge d; Triangle s; Triangle t
where l := DualEdgeLength(d); d := MakeDualEdge(s,t) {
   vec2 l.x0 = d.segment.start
   vec2 l.x1 = d.segment.end
}
   
-- draw lengths as segments with straight ends
forall Length l {

   vec2 u = unit(l.x1-l.x0)
   vec2 n = -rot90(u)
   scalar w = Global.lineOffset

   scalar l.strokeWidth = .8*Global.lineThickness

   shape l.marker = Line {
      start: l.x0 + w*n
      end: l.x1 + w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }
   shape l.end0 = Line {
      start: l.marker.start + .35*w*n
      end: l.marker.start - .35*w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }
   shape l.end1 = Line {
      start: l.marker.end + .35*w*n
      end: l.marker.end - .35*w*n
      strokeColor: Colors.black
      strokeWidth: l.strokeWidth
   }

   vec2 m = (l.x0+l.x1)/2.
   shape l.text = Equation {
      center: m + 2.*w*n
      string: l.label
      fillColor: Colors.black
      fontSize: Global.fontSize
   }
}

-- mark a positively-oriented triangle with a
-- circular arrow pointing in the counter-clockwise
-- direction
forall Triangle t
where IsPositivelyOriented(t) {
   
   scalar R = .5*t.inradius
   scalar t0 = toRadians( -60. )
   scalar t1 = toRadians( 240. )
   vec2 p0 = t.incenter + R*( cos(t0), sin(t0) )
   vec2 p1 = t.incenter + R*( cos(t1), sin(t1) )

   shape t.orientationArc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", p0, p1, (R,R), 0., 1, 0 )
      strokeWidth: 2.*Global.lineThickness
      endArrowhead: "straight"
      endArrowheadSize: .5
   }
}
forall Triangle t
where IsNegativelyOriented(t) {
   
   scalar R = .5*t.inradius
   scalar t0 = toRadians( -60. )
   scalar t1 = toRadians( 240. )
   vec2 p0 = t.incenter + R*( cos(t0), sin(t0) )
   vec2 p1 = t.incenter + R*( cos(t1), sin(t1) )

   shape t.orientationArc = Path {
      fillColor: none()
      strokeColor: Colors.black
      d: arc( "open", p0, p1, (R,R), 0., 1, 0 )
      strokeWidth: 2.*Global.lineThickness
      startArrowhead: "straight"
      startArrowheadSize: .5
   }
}

-- draw flipped edge as dashed line
forall Edge e
where IsFlipped( e ) {
   e.segment.strokeDasharray = "5,5"
   override e.segment.strokeLinecap = "butt"
}

-- put edge labels on one side of the edge or another
forall Edge ij; Vertex i; Vertex j
where ij := MakeEdge(i,j); ij has math label {

   vec2 pi = i.center
   vec2 pj = j.center
   vec2 m = (pi+pj)/2.
   vec2 u = unit(pj-pi)
   vec2 n = rot90(u)

   vec2 ij.c = (?,?)
   scalar h = ?

   -- ensure that offset h is either plus or minus 1
   scalar s = h*h
   ensure equal( 1., s )

   -- put label at edge midpoint, offset in the plus
   -- or minus normal direction by some fixed amount
   shape ij.labelText = Equation {
      center: m + 12.*h*n
      string: ij.label
      fontSize: Global.fontSize
      fillColor: Colors.black
   }
}

-- if triangles are concyclic, put vertices on a common circle
forall Triangle s; Triangle t; Vertex i; Vertex j; Vertex k; Vertex l
where s := MakeTriangle(i,j,k); t := MakeTriangle(j,i,l); Concyclic(s,t) {

   scalar r0 = 100.
   vec2 c0 = (?,?)

   shape disk = Circle {
      center: c0
      r: r0
      fillColor: Colors.semiBlue
   }

   ensure contains( Global.box, disk )
   ensure equal( norm( i.center-c0 ), r0 )
   ensure equal( norm( j.center-c0 ), r0 )
   ensure equal( norm( k.center-c0 ), r0 )
   ensure equal( norm( l.center-c0 ), r0 )
}

forall Point p; Edge ab; Edge cd; Vertex a; Vertex b; Vertex c; Vertex d
where p := Intersection(ab,cd); ab := MakeEdge(a,b); cd := MakeEdge(c,d) {

   vec2 x = a.center
   vec2 y = b.center
   vec2 z = c.center
   vec2 w = d.center

   override p.center = lineLineIntersection( x, y, z, w )

   layer p.icon above ab.segment
   layer p.icon above cd.segment
}

-- Specific to diagrams/angle-equivalence.substance
-- TODO requires #776 to import these rules from an example-specific Style module

-- Don't draw vertices as dots in this
-- example (but still keep the icons, for
-- other purposes).
--- angle-equivalence --- forall Vertex v {
--- angle-equivalence ---    override v.icon.r = 0.
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- similarityMap {
--- angle-equivalence ---    scalar scale = .5
--- angle-equivalence ---    scalar angle = ?
--- angle-equivalence ---    vec2 shift = (canvas.width/3.,0.)
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- forall Vertex v; Vertex v0
--- angle-equivalence --- where v := similarity(v0) {
--- angle-equivalence --- 
--- angle-equivalence ---    scalar s = similarityMap.scale
--- angle-equivalence ---    scalar theta = similarityMap.angle
--- angle-equivalence ---    vec2 u = similarityMap.shift
--- angle-equivalence --- 
--- angle-equivalence ---    override v.center = s*rotateBy(v0.center,theta) + u
--- angle-equivalence ---    delete v.onCanvas
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- -- don't constrain triangle geometry so much
--- angle-equivalence --- forall Triangle t; Vertex i; Vertex j; Vertex k; Vertex i0
--- angle-equivalence --- where t := MakeTriangle(i,j,k); i := similarity(i0) {
--- angle-equivalence ---    delete t.positiveArea
--- angle-equivalence ---    delete t.goodAngleA
--- angle-equivalence ---    delete t.goodAngleB
--- angle-equivalence --- }
--- angle-equivalence --- 
--- angle-equivalence --- -- make sure triangles are disjoint if they are made similar
--- angle-equivalence --- forall Triangle t1, t2
--- angle-equivalence --- where t1 := MakeTriangle(i, j, k); t2 := MakeTriangle(I, J, K); i := similarity(i); j := similarity(j); k := similarity(k);
--- angle-equivalence --- with Vertex i, j, k, I, J, K {
--- angle-equivalence ---    ensure disjoint(t1.icon, t2.icon)
--- angle-equivalence --- }

-- Specific to diagrams/concyclic-pair.substance
-- TODO requires #776 to import these rules from an example-specific Style module

--- concyclic-pair --- -- make vertex dots bigger
--- concyclic-pair --- forall Vertex v {
--- concyclic-pair ---    override v.icon.r = 1.75*Global.vertexRadius
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make corner labels larger and dark blue
--- concyclic-pair --- forall Corner c; Vertex i; Vertex j; Vertex k
--- concyclic-pair --- where c := MakeCorner(i,j,k); c has text label {
--- concyclic-pair ---    override c.text.fontSize = "25px"
--- concyclic-pair ---    override c.text.fillColor = Colors.darkBlue
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- don't draw black outline on arcs
--- concyclic-pair --- forall Corner c; Vertex i; Vertex j; Vertex k
--- concyclic-pair --- where c := MakeCorner(i,j,k) {
--- concyclic-pair ---    delete c.arc
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make edges thicker
--- concyclic-pair --- forall Edge e {
--- concyclic-pair ---    override e.segment.strokeWidth = 2*Global.lineThickness
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- make boundary edges thicker
--- concyclic-pair --- forall Edge e
--- concyclic-pair --- where IsBoundaryEdge( e ) {
--- concyclic-pair ---    override e.segment.strokeWidth = 3*Global.lineThickness
--- concyclic-pair --- }
--- concyclic-pair --- 
--- concyclic-pair --- -- corner labels shouldn't overlap diagonals
--- concyclic-pair --- forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
--- concyclic-pair --- where c := MakeCorner(i,j,k); e := MakeEdge(l,i) {
--- concyclic-pair ---    ensure disjoint( c.textBounds, e.segment )
--- concyclic-pair --- }
--- concyclic-pair --- forall Edge e; Corner c; Vertex i; Vertex j; Vertex k; Vertex l
--- concyclic-pair --- where c := MakeCorner(i,j,k); e := MakeEdge(i,l) {
--- concyclic-pair ---    ensure disjoint( c.textBounds, e.segment )
--- concyclic-pair --- }`,ra={diagrams:Jr,"triangle-mesh-2d-minimal.domain":na,"triangle-mesh-2d-minimal.style":ea,"triangle-mesh-2d.domain":ta,"triangle-mesh-2d.style":oa},aa=`type Triangle
`,la=`canvas {
   width = 200
   height = 200
}

global {

   -- Ground plane coordinates in 3D
   scalar planeSize = 50 -- plane size
   scalar planeHeight = -40 -- plane height

   -- Use a simple pinhole camera model, where the
   -- only camera parameter is the distance along Z
   scalar cZ = -160 -- camera z coordinate

   -- Corner coordinates of the global ground plane
   vec3 q00 = ( -planeSize, planeHeight, -planeSize )
   vec3 q10 = (  planeSize, planeHeight, -planeSize )
   vec3 q01 = ( -planeSize, planeHeight,  planeSize )
   vec3 q11 = (  planeSize, planeHeight,  planeSize )

   -- Apply a random rotation to the ground plane
   -- (Note that we could also apply this rotation to the triangle
   -- vertices, but since they're sampled randomly, it wouldn't
   -- really change the appearance of the kinds of diagrams we sample).
   scalar θ = ?
   vec3 Q00 = ( q00[0]*cos(θ) + q00[2]*sin(θ), q00[1], q00[2]*cos(θ) - q00[0]*sin(θ) )
   vec3 Q10 = ( q10[0]*cos(θ) + q10[2]*sin(θ), q10[1], q10[2]*cos(θ) - q10[0]*sin(θ) )
   vec3 Q01 = ( q01[0]*cos(θ) + q01[2]*sin(θ), q01[1], q01[2]*cos(θ) - q01[0]*sin(θ) )
   vec3 Q11 = ( q11[0]*cos(θ) + q11[2]*sin(θ), q11[1], q11[2]*cos(θ) - q11[0]*sin(θ) )

   -- Perform perspective projection on 3D coordinates to get 2D coordinates p
   vec2 p00 = canvas.width * (Q00[0],Q00[1])/(Q00[2] - global.cZ)
   vec2 p10 = canvas.width * (Q10[0],Q10[1])/(Q10[2] - global.cZ)
   vec2 p01 = canvas.width * (Q01[0],Q01[1])/(Q01[2] - global.cZ)
   vec2 p11 = canvas.width * (Q11[0],Q11[1])/(Q11[2] - global.cZ)

   -- Draw polygon using projected 2D coordinates p
   shape groundPlane = Polygon {
      points: (p00,p10,p11,p01)
      width: canvas.width
      height: canvas.height
      fillColor: rgba(0,0,0,0.1)
      strokeColor: rgba(.7,.7,.7,1)
      strokeWidth: .5
      ensureOnCanvas: false
   }
}

forall Triangle t
{
   -- We'll sample the triangle vertices from a bounding box of size c
   scalar c = .9*min( global.planeSize, abs(global.planeHeight) )

   -- triangle vertex coordinates in 3D
   vec3 qi = (?,?,?)
   vec3 qj = (?,?,?)
   vec3 qk = (?,?,?)

   ensure -c < qi[0]
   ensure qi[0] < c
   ensure -c < qi[1]
   ensure qi[1] < c
   ensure -c < qi[2]
   ensure qi[2] < c

   ensure -c < qj[0]
   ensure qj[0] < c
   ensure -c < qj[1]
   ensure qj[1] < c
   ensure -c < qj[2]
   ensure qj[2] < c

   ensure -c < qk[0]
   ensure qk[0] < c
   ensure -c < qk[1]
   ensure qk[1] < c
   ensure -c < qk[2]
   ensure qk[2] < c

   -- Perform perspective projection on 3D coordinates to get 2D coordinates p
   vec2 t.pi = canvas.width * (qi[0],qi[1])/(qi[2]-global.cZ)
   vec2 t.pj = canvas.width * (qj[0],qj[1])/(qj[2]-global.cZ)
   vec2 t.pk = canvas.width * (qk[0],qk[1])/(qk[2]-global.cZ)

   -- Draw polygon using projected 2D coordinates p
   shape t.icon = Polygon {
      points: (t.pi,t.pj,t.pk)
      width: canvas.width
      height: canvas.height
      fillColor: #34379aaa
      strokeColor: #1b1f8a
      strokeWidth: .5
      ensureOnCanvas: false
   }

   -- Make sure the triangle is positively oriented in the
   -- image plane, and has some- "fat" angles so that it
   -- doesn't degenerate
   vec2 eij = t.pj - t.pi
   vec2 ejk = t.pk - t.pj
   vec2 eki = t.pi - t.pk
   ensure cross2D( eij, -ejk ) < 0
   ensure angleFrom( -ejk, eij ) > toRadians( 45 )
   ensure angleFrom( -eki, ejk ) > toRadians( 45 )
   ensure angleFrom( -eij, eki ) > toRadians( 45 )

   -- Draw triangle vertices and labels as dots and equations,
   -- using again the projected 2D coordinates p
   scalar dotSize = 1.0
   color dotColor = rgba(0,0,0,1)
   string dotFontSize = "4.5px"
   scalar offset = 6 -- offset of labels from vertices
   shape t.vertexI = Circle {
      center: t.pi
      r: dotSize
      fillColor: dotColor
   }
   shape t.vertexJ = Circle {
      center: t.pj
      r: dotSize
      fillColor: dotColor
   }
   shape t.vertexK = Circle {
      center: t.pk
      r: dotSize
      fillColor: dotColor
   }
   shape t.labelI = Equation {
       string: t.label + "_i"
       center: t.pi - offset*unit(eij-eki)
       fontSize: dotFontSize
   }
   shape t.labelJ = Equation {
       string: t.label + "_j"
       center: t.pj - offset*unit(ejk-eij)
       fontSize: dotFontSize
   }
   shape t.labelK = Equation {
       string: t.label + "_k"
       center: t.pk - offset*unit(eki-ejk)
       fontSize: dotFontSize
   }

   -- Finally, draw a shadow of the triangle on the global ground plane
   -- by just replacing the y-coordinate with the height of the ground plane
   scalar h = global.planeHeight
   vec2 ri = (qi[0],h)
   vec2 rj = (qj[0],h)
   vec2 rk = (qk[0],h)

   -- Perform perspective projection on 3D coordinates r to get 2D coordinates s
   vec2 si = canvas.width * ri/(qi[2]-global.cZ)
   vec2 sj = canvas.width * rj/(qj[2]-global.cZ)
   vec2 sk = canvas.width * rk/(qk[2]-global.cZ)

   -- Draw shadow polygon using projected 2D coordinates s
   shape t.shadow = Polygon {
      points: (si,sj,sk)
      width: canvas.width
      height: canvas.height
      fillColor: rgba(0,0,0,0.1)
      strokeColor: none()
      ensureOnCanvas: false
   }

   -- Make sure the triangle shadow lands on the ground plane
   --ensure contains( global.groundPlane, t.shadow )
}

-- For any pair of triangles, make sure that triangles
-- don't overlap, and moreover the vertices of one triangle are
-- far from being contained in the other triangle (which helps
-- to avoid overlapping labels).
forall Triangle s; Triangle t
{
   scalar padding = 10.0

   -- make sure triangles don't overlap
   ensure disjoint( t.icon, s.icon )

   -- make sure vertices of t are far from s
   ensure disjoint( t.vertexI, s.icon, padding )
   ensure disjoint( t.vertexJ, s.icon, padding )
   ensure disjoint( t.vertexK, s.icon, padding )

   -- make sure vertices of s are far from t
   ensure disjoint( s.vertexI, t.icon, padding )
   ensure disjoint( s.vertexJ, t.icon, padding )
   ensure disjoint( s.vertexK, t.icon, padding )
}

`,ia=`Triangle s, t

Label s $\\mathbf{f}$
Label t $\\widetilde{\\mathbf{f}}$
`,sa={"triangle-mesh-3d.domain":aa,"triangle-mesh-3d.style":la,"two-triangles.substance":ia},ca=`/* This is the starter code for the domain file of tutorial 1,
 * which teaches the basics of Penrose. Follow along with the write-up. 
 * Good luck! :)
 */

-- Start your code below

-- End your code above
`,da=`/* This is the starter code for the style file of tutorial 1,
 * which teaches the basics of Penrose. Follow along with the write-up. 
 * Good luck! :)
 */

-- Start your code below

-- End your code above`,pa=`/* This is the starter code for the substance file of tutorial 1,
 * which teaches the basics of Penrose. Follow along with the write-up. 
 * Good luck! :)
 */

-- Start your code below


-- End your code above
`,ga={"setTheory.domain":ca,"twoSets.style":da,"twoSets.substance":pa},fa=`/* This is the starter code for the domain program for tutorial 2,
 * which covers constraints in Penrose. Follow along with the write-up.
 * Good luck! :)
 */

type Set

-- Start your code below

-- End your code above
`,ua=`/* This is the starter code for the style program for tutorial 2,
 * which covers constraints in Penrose. Follow along with the write-up.
 * Good luck! :)
 */

canvas {
  width = 800
  height = 700
}

-- Start your code below

-- End your code above

`,ha=`/* This is the starter code for the substance program for tutorial 2,
 * which covers constraints in Penrose. Follow along with the write-up.
 * Good luck! :)
 */

-- Start your code below

-- End your code above
`,ba={"setTheory.domain":fa,"subset.style":ua,"subset.substance":ha},ka=`/* This is the starter code for the domain program for tutorial 3,
 * which covers functions in Penrose. Follow along with the write-up.
 * Good luck! :)
 */

type VectorSpace
type Vector
predicate In(Vector, VectorSpace V)

-- Start your code below

-- End your code above
`,xa=`/* This is the starter code for the style program for tutorial 3,
 * which covers functions in Penrose. Scroll down to the
 * bottom and follow along with the write-up. Good luck! :)
 */

/* here are some useful constants that we use to draw
 * the vector space
 */
canvas {
  width = 800
  height = 700
}

const {
  scalar vectorSpaceSize = 350.0
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  scalar arrowThickness = 1.5
  color gray = rgba(0.6, 0.6, 0.6, 1.)
  color lightBlue = rgba(0.2, 0.4, 0.8, 1.0)
  color lightGray = rgba(252, 252, 252, 0.015)
  color green = rgba(0., 0.8, 0., 1.)
  color none = rgba(0., 0., 0., 0.)
}

/* here we draw a vector space by defining an origin
 * of the vector space, and x-axis, y-axis that are
 * centered at the origin
 */
forall VectorSpace U {
    scalar axisSize = const.vectorSpaceSize / 2.0
    vec2 U.origin = (0., 0.)
    vec2 o = U.origin /* just so we don't need to type U.origin everytime */
    U.axisColor = const.gray

    U.background = Rectangle {
        center : U.origin
        width : const.vectorSpaceSize
        height : const.vectorSpaceSize
        fillColor : const.lightGray
        strokeColor : const.none
    }

    U.xAxis = Line {
        start : (o[0] - axisSize, o[1])
        end : (o[0] + axisSize, o[1])
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.yAxis = Line {
        start : (o[0], o[1] - axisSize)
        end : (o[0], o[1] + axisSize)
        strokeWidth : const.lineThickness
        style : "solid"
        strokeColor : U.axisColor
        startArrowhead: "straight"
        endArrowhead: "straight"
        startArrowheadSize : const.arrowheadSize * 2.
        endArrowheadSize : const.arrowheadSize * 2.
    }

    U.text = Equation {
        string : U.label
        center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
        fillColor : U.axisColor
    }
}

-- Start your code below

-- End your code above
`,va=`/* This is the starter code for the substance program for tutorial 3,
 * which covers functions in Penrose. Follow along with the write-up. 
 * Good luck! :)
 */
 
VectorSpace U
Vector v 
Vector w
In(v, U)
In(w, U)

-- Start your code below


-- End your code above
AutoLabel All /* leave this as the last line */`,ma={"linearAlgebra.domain":ka,"vector.style":xa,"vector.substance":va},ya={tutorial1:ga,tutorial2:ba,tutorial3:ma},wa={},Ca={},La={tutorial3:Ca},Aa={code:ya,solutions:wa,supplementary:La},Sa=`-- Describe a short walk used to estimate the solution to
-- a basic Laplace equation Δu = 0, which involves both
-- boundary samples x and source samples y.

Domain U
Step x0, x1, x2, x3, x4

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )
x4 := sampleBoundary( x3 )

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "x₂"
Label x3 "…"
Label x4 "xₖ"


`,Pa=`-- Describe a nested walk used to estimate the solution to,
-- e.g., a biharmonic equation Δ²u = 0, which starts new
-- walks at interior points y sampled in the ball around
-- each point x from the primary walk.

Domain U

Step x0, x1, x2, x3
Step y00, y01, y02
Step y10, y11, y12

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )

y00 := sampleInterior( x0 )
y01 := sampleBoundary( y00 )
y02 := sampleBoundary( y01 )
nested( y00 )
nested( y01 )
nested( y02 )

y10 := sampleInterior( x1 )
y11 := sampleBoundary( y10 )
y12 := sampleBoundary( y11 )
nested( y10 )
nested( y11 )
nested( y12 )

Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "…"
Label x3 "xₖ"
Label y00 "y₀"
Label y10 "y₁"

`,Ea=`-- Describe a short walk used to estimate the solution to
-- a basic Laplace equation Δu = 0, using off-centered
-- rather than on-centered steps.

Domain U

Step x0, x1, x2, x3, x4

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )
x4 := sampleBoundary( x3 )

offCenter( x0 )
offCenter( x1 )
offCenter( x2 )
offCenter( x3 )
offCenter( x4 )

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "x₂"
Label x3 "…"
Label x4 "xₖ"


`,Ta=`-- Describe a short walk used to estimate the solution to
-- a Poisson equation Δu = f, which involves both boundary
-- samples x and source samples y.

Domain U
Step x0, x1, x2, x3
Sample y0, y1, y2, y3

x1 := sampleBoundary(x0)
x2 := sampleBoundary(x1)
x3 := sampleBoundary(x2)
y0 := sampleSource(x0)
y1 := sampleSource(x1)
y2 := sampleSource(x2)
y3 := sampleSource(x3)

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "…"
Label x3 "xₖ"
Label y0 "y₀"
Label y1 "y₁"
NoLabel y2
NoLabel y3


`,Oa=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 240">
	<rect x="0" y="0" width="320" height="240" fill="none" stroke="none" rx="0" transform="rotate(0, 0, 0)" ensureOnCanvas="true">
		<title>
			Global.box
		</title>
	</rect>
	<polygon fill="#e6e6e6" fill-opacity="1" stroke="#808080" stroke-opacity="1" stroke-width="2" stroke-linecap="butt" transform="scale(1)" points="287.25016469418006,233.08096451792022,179.05322522858393,206.8293227581829,7.2865910873548785,237.19865131128645,1.3907381820613978,2.801539594930901,151.5272941701612,2.7932325343222573,281.4844272754509,6.758664350411522" ensureOnCanvas="false">
		<title>
			Global.domain
		</title>
	</polygon>
	<g transform="rotate(0, 21.514851933723605, 4.544701507378761)translate(21.514851933723605, 4.544701507378761)" name="x0.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="206.93212742986117" height="206.93212742986117">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst0.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst0.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 124.98091564865419 108.01076522230935 L 225.96262743001705 130.54781245432014" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst0.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="124.98091564865419" cy="108.01076522230935" stroke="none" r="103.46606371493058" ensureOnCanvas="false">
		<title>
			x0.ball
		</title>
	</circle>
	<circle fill="none" cx="124.98091564865419" cy="108.01076522230935" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="103.46606371493058" ensureOnCanvas="false">
		<title>
			x0.sphere
		</title>
	</circle>
	<g transform="rotate(0, 169.05623368712327, 73.64141871142637)translate(169.05623368712327, 73.64141871142637)" name="x1.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="113.81278748578754" height="113.81278748578754">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst1.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst1.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 225.96262743001705 130.54781245432014 L 235.81715002137383 186.59448501153122" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst1.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="225.96262743001705" cy="130.54781245432014" stroke="none" r="56.90639374289377" ensureOnCanvas="false">
		<title>
			x1.ball
		</title>
	</circle>
	<circle fill="none" cx="225.96262743001705" cy="130.54781245432014" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="56.90639374289377" ensureOnCanvas="false">
		<title>
			x1.sphere
		</title>
	</circle>
	<g transform="rotate(0, 204.51862536613012, 155.2959603562875)translate(204.51862536613012, 155.2959603562875)" name="x2.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="62.59704931048742" height="62.59704931048742">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst2.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst2.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 235.81715002137383 186.59448501153122 L 267.04014770361664 184.42079107699158" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst2.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="235.81715002137383" cy="186.59448501153122" stroke="none" r="31.29852465524371" ensureOnCanvas="false">
		<title>
			x2.ball
		</title>
	</circle>
	<circle fill="none" cx="235.81715002137383" cy="186.59448501153122" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="31.29852465524371" ensureOnCanvas="false">
		<title>
			x2.sphere
		</title>
	</circle>
	<g transform="rotate(0, 249.82593968250876, 167.2065830558837)translate(249.82593968250876, 167.2065830558837)" name="x3.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="34.42841604221577" height="34.42841604221577">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst3.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst3.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 267.04014770361664 184.42079107699158 L 274.3925480097661 168.85575267643696" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst3.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="267.04014770361664" cy="184.42079107699158" stroke="none" r="17.214208021107886" ensureOnCanvas="false">
		<title>
			x3.ball
		</title>
	</circle>
	<circle fill="none" cx="267.04014770361664" cy="184.42079107699158" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="17.214208021107886" ensureOnCanvas="false">
		<title>
			x3.sphere
		</title>
	</circle>
	<g transform="rotate(0, 264.92475844395193, 159.38796311062282)translate(264.92475844395193, 159.38796311062282)" name="x4.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="18.935579131628273" height="18.935579131628273">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="274.3925480097661" cy="168.85575267643696" stroke="none" r="9.467789565814137" ensureOnCanvas="false">
		<title>
			x4.ball
		</title>
	</circle>
	<circle fill="none" cx="274.3925480097661" cy="168.85575267643696" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="9.467789565814137" ensureOnCanvas="false">
		<title>
			x4.sphere
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 268.7552908563324, 155.51945457638567)" style="font: italic 12px Palatino" x="274.4623221063324" y="164.20890770138567" width="11.4140625" height="8.8359375" ascent="8.689453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x4.labelText
		</title>
		xk
	</text>
	<circle fill="#000000" fill-opacity="1" cx="274.3925480097661" cy="168.85575267643696" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x4.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 261.3363717642534, 178.5028478595414)" style="font: italic 12px Palatino" x="267.0756295767534" y="179.8622228595414" width="11.478515625" height="1.41796875" ascent="1.359375" descent="0.05859375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x3.labelText
		</title>
		…
	</text>
	<circle fill="#000000" fill-opacity="1" cx="267.04014770361664" cy="184.42079107699158" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x3.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 230.34131953771703, 191.09448568439382)" style="font: italic 12px Palatino" x="235.90479610021703" y="199.32104818439382" width="11.126953125" height="8.373046875" ascent="8.2265625" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x2.labelText
		</title>
		x2
	</text>
	<circle fill="#000000" fill-opacity="1" cx="235.81715002137383" cy="186.59448501153122" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x2.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 220.67193655743583, 135.0477783419941)" style="font: italic 12px Palatino" x="226.00982718243583" y="143.3622314669941" width="10.67578125" height="8.4609375" ascent="8.314453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x1.labelText
		</title>
		x1
	</text>
	<circle fill="#000000" fill-opacity="1" cx="225.96262743001705" cy="130.54781245432014" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x1.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 129.48123535030996, 103.72382229967113)" style="font: italic 12px Palatino" x="135.29666503780996" y="111.99725979967113" width="11.630859375" height="8.419921875" ascent="8.2734375" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x0.labelText
		</title>
		x0
	</text>
	<circle fill="#000000" fill-opacity="1" cx="124.98091564865419" cy="108.01076522230935" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x0.dot
		</title>
	</circle>
	<text fill="#808080" fill-opacity="1" stroke="none" transform="rotate(0, 271.8219307245297, 215.83431074453287)" style="font: 12px Palatino" x="276.7731025995297" y="224.28352949453287" width="9.90234375" height="8.53125" ascent="8.44921875" descent="0.08203125" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true">
		<title>
			U.labelText
		</title>
		Ω
	</text>
</svg>
`,Ba=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 240">
	<rect x="0" y="0" width="320" height="240" fill="none" stroke="none" rx="0" transform="rotate(0, 0, 0)" ensureOnCanvas="true">
		<title>
			Global.box
		</title>
	</rect>
	<polygon fill="#e6e6e6" fill-opacity="1" stroke="#808080" stroke-opacity="1" stroke-width="2" stroke-linecap="butt" transform="scale(1)" points="296.28377590462225,223.30334661732962,167.45384394066394,191.46679081535206,-1.0749913509293378,240.25814703637968,-1.0749913132735571,-0.2830729157056311,164.9868013036671,47.87845313555988,319.18031505073105,39.05300740691263" ensureOnCanvas="false">
		<title>
			Global.domain
		</title>
	</polygon>
	<g transform="rotate(0, 0.67500867569062, 31.399296489982717)translate(0.67500867569062, 31.399296489982717)" name="x0.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="177.23520099545576" height="177.23520099545576">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst0.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst0.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 89.2926091734185 120.0168969877106 L 175.96044894099032 121.551437481426" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst0.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="89.2926091734185" cy="120.0168969877106" stroke="none" r="88.61760049772788" ensureOnCanvas="false">
		<title>
			x0.ball
		</title>
	</circle>
	<circle fill="none" cx="89.2926091734185" cy="120.0168969877106" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="88.61760049772788" ensureOnCanvas="false">
		<title>
			x0.sphere
		</title>
	</circle>
	<g transform="rotate(0, 107.27949721918476, 52.87048575962045)translate(107.27949721918476, 52.87048575962045)" name="x1.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="137.3619034436111" height="137.3619034436111">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="175.96044894099032" cy="121.551437481426" stroke="none" r="68.68095172180556" ensureOnCanvas="false">
		<title>
			x1.ball
		</title>
	</circle>
	<circle fill="none" cx="175.96044894099032" cy="121.551437481426" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="68.68095172180556" ensureOnCanvas="false">
		<title>
			x1.sphere
		</title>
	</circle>
	<g transform="rotate(0, 155.347860671204, 49.211199934342645)translate(155.347860671204, 49.211199934342645)" name="y10.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="34.957618612593784" height="34.957618612593784">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst5.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst5.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 172.82666997750087 66.69000924063954 L 189.19501892950237 60.53075290230686" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst5.walkLine
		</title>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="172.82666997750087" cy="66.69000924063954" stroke="none" r="17.478809306296892" ensureOnCanvas="false">
		<title>
			y10.ball
		</title>
	</circle>
	<circle fill="none" cx="172.82666997750087" cy="66.69000924063954" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="17.478809306296892" ensureOnCanvas="false">
		<title>
			y10.sphere
		</title>
	</circle>
	<g transform="rotate(0, 176.93007113770926, 48.26580511051375)translate(176.93007113770926, 48.26580511051375)" name="y11.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="24.529895583586207" height="24.529895583586207">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst6.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst6.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 189.19501892950237 60.53075290230686 L 200.66759866700113 56.23179552366312" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst6.walkLine
		</title>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="189.19501892950237" cy="60.53075290230686" stroke="none" r="12.264947791793103" ensureOnCanvas="false">
		<title>
			y11.ball
		</title>
	</circle>
	<circle fill="none" cx="189.19501892950237" cy="60.53075290230686" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="12.264947791793103" ensureOnCanvas="false">
		<title>
			y11.sphere
		</title>
	</circle>
	<g transform="rotate(0, 192.03901031412363, 47.603207170785616)translate(192.03901031412363, 47.603207170785616)" name="y12.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="17.25717670575501" height="17.25717670575501">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="200.66759866700113" cy="56.23179552366312" stroke="none" r="8.628588352877506" ensureOnCanvas="false">
		<title>
			y12.ball
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="200.66759866700113" cy="56.23179552366312" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y12.dot
		</title>
	</circle>
	<circle fill="none" cx="200.66759866700113" cy="56.23179552366312" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="8.628588352877506" ensureOnCanvas="false">
		<title>
			y12.sphere
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="189.19501892950237" cy="60.53075290230686" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y11.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 157.6362368119688, 61.39091640110101)" style="font: italic 12px Palatino" x="163.0004946244688" y="69.70536952610101" width="10.728515625" height="11.625" ascent="8.314453125" descent="3.310546875" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			y10.labelText
		</title>
		y1
	</text>
	<circle fill="#ffffff" fill-opacity="1" cx="172.82666997750087" cy="66.69000924063954" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y10.dot
		</title>
	</circle>
	<g transform="rotate(0, 91.305562553112, 170.39184996431962)translate(91.305562553112, 170.39184996431962)" name="y00.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="35.440995529327395" height="35.440995529327395">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst3.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst3.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 109.0260603177757 188.1123477289833 L 94.30192289061763 197.85582942722155" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst3.walkLine
		</title>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="109.0260603177757" cy="188.1123477289833" stroke="none" r="17.720497764663698" ensureOnCanvas="false">
		<title>
			y00.ball
		</title>
	</circle>
	<circle fill="none" cx="109.0260603177757" cy="188.1123477289833" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="17.720497764663698" ensureOnCanvas="false">
		<title>
			y00.sphere
		</title>
	</circle>
	<g transform="rotate(0, 81.84588254636945, 185.39978908297337)translate(81.84588254636945, 185.39978908297337)" name="y01.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="24.91208068849636" height="24.91208068849636">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst4.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst4.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 94.30192289061763 197.85582942722155 L 83.90337681253462 204.76582632712876" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst4.walkLine
		</title>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="94.30192289061763" cy="197.85582942722155" stroke="none" r="12.45604034424818" ensureOnCanvas="false">
		<title>
			y01.ball
		</title>
	</circle>
	<circle fill="none" cx="94.30192289061763" cy="197.85582942722155" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="12.45604034424818" ensureOnCanvas="false">
		<title>
			y01.sphere
		</title>
	</circle>
	<g transform="rotate(0, 75.19299639283018, 196.0554459074243)translate(75.19299639283018, 196.0554459074243)" name="y02.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="17.42076083940887" height="17.42076083940887">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#ffffff" fill-opacity="0.4" cx="83.90337681253462" cy="204.76582632712876" stroke="none" r="8.710380419704435" ensureOnCanvas="false">
		<title>
			y02.ball
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="83.90337681253462" cy="204.76582632712876" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y02.dot
		</title>
	</circle>
	<circle fill="none" cx="83.90337681253462" cy="204.76582632712876" stroke="#ffffff" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="8.710380419704435" ensureOnCanvas="false">
		<title>
			y02.sphere
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="94.30192289061763" cy="197.85582942722155" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y01.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 122.69872494642095, 178.03914539134425)" style="font: italic 12px Palatino" x="128.54052182142095" y="186.31258289134425" width="11.68359375" height="11.583984375" ascent="8.2734375" descent="3.310546875" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			y00.labelText
		</title>
		y0
	</text>
	<circle fill="#ffffff" fill-opacity="1" cx="109.0260603177757" cy="188.1123477289833" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			y00.dot
		</title>
	</circle>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst1.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst1.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 175.96044894099032 121.551437481426 L 232.9349962718888 83.40291609351337" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst1.walkLine
		</title>
	</g>
	<g transform="rotate(0, 195.33583986907038, 45.80375969069497)translate(195.33583986907038, 45.80375969069497)" name="x2.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="75.1983128056368" height="75.1983128056368">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst2.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst2.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 232.9349962718888 83.40291609351337 L 265.33793693893153 64.52099509789348" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst2.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="232.9349962718888" cy="83.40291609351337" stroke="none" r="37.5991564028184" ensureOnCanvas="false">
		<title>
			x2.ball
		</title>
	</circle>
	<circle fill="none" cx="232.9349962718888" cy="83.40291609351337" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="37.5991564028184" ensureOnCanvas="false">
		<title>
			x2.sphere
		</title>
	</circle>
	<g transform="rotate(0, 244.73825920453115, 43.92131736349308)translate(244.73825920453115, 43.92131736349308)" name="x3.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="41.19935546880079" height="41.19935546880079">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="265.33793693893153" cy="64.52099509789348" stroke="none" r="20.599677734400395" ensureOnCanvas="false">
		<title>
			x3.ball
		</title>
	</circle>
	<circle fill="none" cx="265.33793693893153" cy="64.52099509789348" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="20.599677734400395" ensureOnCanvas="false">
		<title>
			x3.sphere
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 252.0741283347453, 73.50827813506436)" style="font: italic 12px Palatino" x="257.7811595847453" y="82.19773126006436" width="11.4140625" height="8.8359375" ascent="8.689453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x3.labelText
		</title>
		xk
	</text>
	<circle fill="#000000" fill-opacity="1" cx="265.33793693893153" cy="64.52099509789348" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x3.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 217.64148810331608, 74.63108062967251)" style="font: italic 12px Palatino" x="223.38074591581608" y="75.99045562967251" width="11.478515625" height="1.41796875" ascent="1.359375" descent="0.05859375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x2.labelText
		</title>
		…
	</text>
	<circle fill="#000000" fill-opacity="1" cx="232.9349962718888" cy="83.40291609351337" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x2.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 170.30306705769584, 125.96773413269685)" style="font: italic 12px Palatino" x="175.64095768269584" y="134.28218725769685" width="10.67578125" height="8.4609375" ascent="8.314453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x1.labelText
		</title>
		x1
	</text>
	<circle fill="#000000" fill-opacity="1" cx="175.96044894099032" cy="121.551437481426" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x1.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 98.13762803831992, 105.77722463995396)" style="font: italic 12px Palatino" x="103.95305772581992" y="114.05066213995396" width="11.630859375" height="8.419921875" ascent="8.2734375" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x0.labelText
		</title>
		x0
	</text>
	<circle fill="#000000" fill-opacity="1" cx="89.2926091734185" cy="120.0168969877106" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x0.dot
		</title>
	</circle>
	<text fill="#808080" fill-opacity="1" stroke="none" transform="rotate(0, 291.20278609544437, 139.35985130372416)" style="font: 12px Palatino" x="296.15395797044437" y="147.80907005372416" width="9.90234375" height="8.53125" ascent="8.44921875" descent="0.08203125" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true">
		<title>
			U.labelText
		</title>
		Ω
	</text>
</svg>
`,Ma=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 240">
	<rect x="0" y="0" width="320" height="240" fill="none" stroke="none" rx="0" transform="rotate(0, 0, 0)" ensureOnCanvas="true">
		<title>
			Global.box
		</title>
	</rect>
	<polygon fill="#e6e6e6" fill-opacity="1" stroke="#808080" stroke-opacity="1" stroke-width="2" stroke-linecap="butt" transform="scale(1)" points="291.58999105819703,229.9590401800724,175.13031467499823,210.4052445664708,3.7974051734806835,236.72636272578796,5.963906430987862,7.993639415372797,176.15902129564566,4.584657307330431,284.4107019716374,12.401025120568804" ensureOnCanvas="false">
		<title>
			Global.domain
		</title>
	</polygon>
	<g transform="rotate(0, 14.077276616172583, 7.528687402593903)translate(14.077276616172583, 7.528687402593903)" name="x0.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="207.0468039719565" height="207.0468039719565">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst0.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst0.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 108.65190339516555 61.21619873527873 L 214.53051891647297 74.69694078003937" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst0.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="117.60067860215084" cy="111.05208938857216" stroke="none" r="103.52340198597825" ensureOnCanvas="false">
		<title>
			x0.ball
		</title>
	</circle>
	<circle fill="none" cx="117.60067860215084" cy="111.05208938857216" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="103.52340198597825" ensureOnCanvas="false">
		<title>
			x0.sphere
		</title>
	</circle>
	<g transform="rotate(0, 172.00520103888394, 54.019253545240595)translate(172.00520103888394, 54.019253545240595)" name="x1.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="113.87585144269364" height="113.87585144269364">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst1.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst1.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 214.53051891647297 74.69694078003937 L 239.77220767787634 166.85583407120455" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst1.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="228.94312676023077" cy="110.95717926658742" stroke="none" r="56.93792572134682" ensureOnCanvas="false">
		<title>
			x1.ball
		</title>
	</circle>
	<circle fill="none" cx="228.94312676023077" cy="110.95717926658742" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="56.93792572134682" ensureOnCanvas="false">
		<title>
			x1.sphere
		</title>
	</circle>
	<g transform="rotate(0, 224.9904875558242, 131.98477505240658)translate(224.9904875558242, 131.98477505240658)" name="x2.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="62.63181730402775" height="62.63181730402775">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst2.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst2.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 239.77220767787634 166.85583407120455 L 238.0817885738261 188.76737525164285" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst2.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="256.3063962078381" cy="163.30068370442044" stroke="none" r="31.315908652013874" ensureOnCanvas="false">
		<title>
			x2.ball
		</title>
	</circle>
	<circle fill="none" cx="256.3063962078381" cy="163.30068370442044" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="31.315908652013874" ensureOnCanvas="false">
		<title>
			x2.sphere
		</title>
	</circle>
	<g transform="rotate(0, 218.27940867158424, 184.07878351580814)translate(218.27940867158424, 184.07878351580814)" name="x3.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="34.44759477266975" height="34.44759477266975">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst3.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst3.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 238.0817885738261 188.76737525164285 L 218.48907189326042 203.98188034444973" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst3.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="235.5032060579191" cy="201.302580902143" stroke="none" r="17.223797386334876" ensureOnCanvas="false">
		<title>
			x3.ball
		</title>
	</circle>
	<circle fill="none" cx="235.5032060579191" cy="201.302580902143" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="17.223797386334876" ensureOnCanvas="false">
		<title>
			x3.sphere
		</title>
	</circle>
	<g transform="rotate(0, 208.44391364923706, 196.73594139428354)translate(208.44391364923706, 196.73594139428354)" name="x4.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="18.946171137014407" height="18.946171137014407">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="217.91699921774426" cy="206.20902696279074" stroke="none" r="9.473085568507203" ensureOnCanvas="false">
		<title>
			x4.ball
		</title>
	</circle>
	<circle fill="none" cx="217.91699921774426" cy="206.20902696279074" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="9.473085568507203" ensureOnCanvas="false">
		<title>
			x4.sphere
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 202.57498680287938, 199.6219212774044)" style="font: italic 12px Palatino" x="208.28201805287938" y="208.3113744024044" width="11.4140625" height="8.8359375" ascent="8.689453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x4.labelText
		</title>
		xk
	</text>
	<circle fill="#000000" fill-opacity="1" cx="218.48907189326042" cy="203.98188034444973" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x4.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 222.10309131929648, 188.09630330559457)" style="font: italic 12px Palatino" x="227.84234913179648" y="189.45567830559457" width="11.478515625" height="1.41796875" ascent="1.359375" descent="0.05859375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x3.labelText
		</title>
		…
	</text>
	<circle fill="#000000" fill-opacity="1" cx="238.0817885738261" cy="188.76737525164285" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x3.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 234.29361074855154, 153.98279902498916)" style="font: italic 12px Palatino" x="239.85708731105154" y="162.20936152498916" width="11.126953125" height="8.373046875" ascent="8.2265625" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x2.labelText
		</title>
		x2
	</text>
	<circle fill="#000000" fill-opacity="1" cx="239.77220767787634" cy="166.85583407120455" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x2.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 209.0633215422859, 61.73588641556813)" style="font: italic 12px Palatino" x="214.4012121672859" y="70.05033954056813" width="10.67578125" height="8.4609375" ascent="8.314453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x1.labelText
		</title>
		x1
	</text>
	<circle fill="#000000" fill-opacity="1" cx="214.53051891647297" cy="74.69694078003937" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x1.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 102.83855074124533, 48.296149365203604)" style="font: italic 12px Palatino" x="108.65398042874533" y="56.569586865203604" width="11.630859375" height="8.419921875" ascent="8.2734375" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x0.labelText
		</title>
		x0
	</text>
	<circle fill="#000000" fill-opacity="1" cx="108.65190339516555" cy="61.21619873527873" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x0.dot
		</title>
	</circle>
	<text fill="#808080" fill-opacity="1" stroke="none" transform="rotate(0, 265.1665782965748, 210.09408916089063)" style="font: 12px Palatino" x="270.1177501715748" y="218.54330791089063" width="9.90234375" height="8.53125" ascent="8.44921875" descent="0.08203125" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true">
		<title>
			U.labelText
		</title>
		Ω
	</text>
</svg>
`,Ga=`<svg version="1.2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 240">
	<rect x="0" y="0" width="320" height="240" fill="none" stroke="none" rx="0" transform="rotate(0, 0, 0)" ensureOnCanvas="true">
		<title>
			Global.box
		</title>
	</rect>
	<polygon fill="#e6e6e6" fill-opacity="1" stroke="#808080" stroke-opacity="1" stroke-width="2" stroke-linecap="butt" transform="scale(1)" points="286.35673199336554,220.27989594910713,137.96972563112973,219.6422044978826,22.74184668616269,239.7767799156157,35.41181831450483,18.5926748585008,144.15007133967293,3.3896935366571057,312.53977808334827,31.692843475056407" ensureOnCanvas="false">
		<title>
			Global.domain
		</title>
	</polygon>
	<g transform="rotate(0, 74.66393503387387, 48.106957578018225)translate(74.66393503387387, 48.106957578018225)" name="x0.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="169.8774136307412" height="169.8774136307412">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst0.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst0.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 159.60264184924446 133.04566439338882 L 78.922696262936 106.4876209837027" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst0.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="159.60264184924446" cy="133.04566439338882" stroke="none" r="84.9387068153706" ensureOnCanvas="false">
		<title>
			x0.ball
		</title>
	</circle>
	<circle fill="none" cx="159.60264184924446" cy="133.04566439338882" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="84.9387068153706" ensureOnCanvas="false">
		<title>
			x0.sphere
		</title>
	</circle>
	<g transform="rotate(0, 32.20642939769765, 59.771354118464345)translate(32.20642939769765, 59.771354118464345)" name="x1.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="93.4325337304767" height="93.4325337304767">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst1.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst1.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 78.922696262936 106.4876209837027 L 60.320615518041365 63.63469116329577" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst1.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="78.922696262936" cy="106.4876209837027" stroke="none" r="46.71626686523835" ensureOnCanvas="false">
		<title>
			x1.ball
		</title>
	</circle>
	<circle fill="none" cx="78.922696262936" cy="106.4876209837027" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="46.71626686523835" ensureOnCanvas="false">
		<title>
			x1.sphere
		</title>
	</circle>
	<g transform="rotate(0, 34.62668915996051, 37.94076480521492)translate(34.62668915996051, 37.94076480521492)" name="x2.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="51.3878527161617" height="51.3878527161617">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<g ensureOnCanvas="true">
		<marker id="$LOCAL_block9_subst2.walkLine-startArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<marker id="$LOCAL_block9_subst2.walkLine-endArrowId" markerUnits="strokeWidth" markerWidth="9.95" markerHeight="8.12" viewBox="0 0 9.95 8.12" refX="2.36" refY="4.06" orient="auto-start-reverse">
			<path d="M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z" fill="#0000b3" fill-opacity="1">
			</path>
		</marker>
		<path d="M 60.320615518041365 63.63469116329577 L 47.465033186964874 85.88124744651302" stroke-opacity="1" stroke-width="2" stroke="#0000b3" stroke-linecap="butt">
		</path>
		<title>
			$LOCAL_block9_subst2.walkLine
		</title>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="60.320615518041365" cy="63.63469116329577" stroke="none" r="25.69392635808085" ensureOnCanvas="false">
		<title>
			x2.ball
		</title>
	</circle>
	<circle fill="none" cx="60.320615518041365" cy="63.63469116329577" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="25.69392635808085" ensureOnCanvas="false">
		<title>
			x2.sphere
		</title>
	</circle>
	<g transform="rotate(0, 33.333397462526364, 71.7496117220745)translate(33.333397462526364, 71.7496117220745)" name="x3.shading" ensureOnCanvas="false">
		<!-- ?xml version="1.0" encoding="utf-8"? -->
		<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
		<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background: new 0 0 221.7 221.7" xml:space="preserve" width="28.263271448877013" height="28.263271448877013">
			<style type="text/css">
				.st0 {
				opacity: 0.6;
				fill: url(#SVGID_1_);
				enable-background: new;
				}
			</style>
			<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
				<stop offset="0" style="stop-color: #ffffff">
				</stop>
				<stop offset="0.7844" style="stop-color: #5f5f5f">
				</stop>
				<stop offset="1" style="stop-color: #7f7f7f">
				</stop>
			</radialGradient>
			<circle class="st0" cx="110.9" cy="110.9" r="110.9">
			</circle>
		</svg>
	</g>
	<circle fill="#33cc33" fill-opacity="0.15" cx="47.465033186964874" cy="85.88124744651302" stroke="none" r="14.131635724438507" ensureOnCanvas="false">
		<title>
			x3.ball
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="37.846101362933524" cy="85.71287567967033" stroke="#000000" stroke-opacity="1" stroke-width="1" stroke-linecap="butt" r="2" ensureOnCanvas="true">
		<title>
			y3.dot
		</title>
	</circle>
	<circle fill="#ffffff" fill-opacity="1" cx="56.3833356726016" cy="43.872473226385736" stroke="#000000" stroke-opacity="1" stroke-width="1" stroke-linecap="butt" r="2" ensureOnCanvas="true">
		<title>
			y2.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 64.94232755818638, 143.1105005490698)" style="font: italic 12px Palatino" x="70.30658537068638" y="151.4249536740698" width="10.728515625" height="11.625" ascent="8.314453125" descent="3.310546875" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			y1.labelText
		</title>
		y1
	</text>
	<circle fill="#ffffff" fill-opacity="1" cx="70.32919830495527" cy="138.61028601827812" stroke="#000000" stroke-opacity="1" stroke-width="1" stroke-linecap="butt" r="2" ensureOnCanvas="true">
		<title>
			y1.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 161.8482711157261, 49.55367146775714)" style="font: italic 12px Palatino" x="167.6900679907261" y="57.82710896775714" width="11.68359375" height="11.583984375" ascent="8.2734375" descent="3.310546875" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			y0.labelText
		</title>
		y0
	</text>
	<circle fill="#ffffff" fill-opacity="1" cx="168.17012325910048" cy="65.63783571024616" stroke="#000000" stroke-opacity="1" stroke-width="1" stroke-linecap="butt" r="2" ensureOnCanvas="true">
		<title>
			y0.dot
		</title>
	</circle>
	<circle fill="none" cx="47.465033186964874" cy="85.88124744651302" stroke="#668066" stroke-opacity="1" stroke-width="1.5" stroke-linecap="butt" r="14.131635724438507" ensureOnCanvas="false">
		<title>
			x3.sphere
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 40.45153014197595, 72.54512522448154)" style="font: italic 12px Palatino" x="46.15856139197595" y="81.23457834948154" width="11.4140625" height="8.8359375" ascent="8.689453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x3.labelText
		</title>
		xk
	</text>
	<circle fill="#000000" fill-opacity="1" cx="47.465033186964874" cy="85.88124744651302" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x3.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 55.07451342423293, 57.71672051203235)" style="font: italic 12px Palatino" x="60.81377123673293" y="59.07609551203235" width="11.478515625" height="1.41796875" ascent="1.359375" descent="0.05859375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x2.labelText
		</title>
		…
	</text>
	<circle fill="#000000" fill-opacity="1" cx="60.320615518041365" cy="63.63469116329577" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x2.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 83.42282098937713, 102.54327901613394)" style="font: italic 12px Palatino" x="88.76071161437713" y="110.85773214113394" width="10.67578125" height="8.4609375" ascent="8.314453125" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x1.labelText
		</title>
		x1
	</text>
	<circle fill="#000000" fill-opacity="1" cx="78.922696262936" cy="106.4876209837027" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x1.dot
		</title>
	</circle>
	<text fill="#000000" fill-opacity="1" stroke="#ffffff" stroke-opacity="1" stroke-width="2.5" stroke-linecap="butt" transform="rotate(0, 164.10282051405525, 127.64720971325659)" style="font: italic 12px Palatino" x="169.91825020155525" y="135.9206472132566" width="11.630859375" height="8.419921875" ascent="8.2734375" descent="0.146484375" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true" stroke-linejoin="round" paint-order="stroke">
		<title>
			x0.labelText
		</title>
		x0
	</text>
	<circle fill="#000000" fill-opacity="1" cx="159.60264184924446" cy="133.04566439338882" stroke="none" r="2.5" ensureOnCanvas="false">
		<title>
			x0.dot
		</title>
	</circle>
	<text fill="#808080" fill-opacity="1" stroke="none" transform="rotate(0, 102.06425988528365, 26.55052144976308)" style="font: 12px Palatino" x="107.01543176028365" y="34.99974019976308" width="9.90234375" height="8.53125" ascent="8.44921875" descent="0.08203125" text-anchor="middle" alignment-baseline="alphabetic" dominant-baseline="alphabetic" ensureOnCanvas="true">
		<title>
			U.labelText
		</title>
		Ω
	</text>
</svg>
`,Ia={"wos-laplace-estimator-walk-on-spheres.svg":Oa,"wos-nested-estimator-walk-on-spheres.svg":Ba,"wos-offcenter-estimator-walk-on-spheres.svg":Ma,"wos-poisson-estimator-walk-on-spheres.svg":Ga},Va=`<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 26.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 221.7 221.7" style="enable-background:new 0 0 221.7 221.7;" xml:space="preserve">
	<style type="text/css">
		.st0{opacity:0.6;fill:url(#SVGID_1_);enable-background:new    ;}
	</style>
	<radialGradient id="SVGID_1_" cx="75.4328" cy="151.0149" r="157.9341" gradientTransform="matrix(1 0 0 -1 0 222)" gradientUnits="userSpaceOnUse">
		<stop offset="0" style="stop-color:#FFFFFF" />
		<stop offset="0.7844" style="stop-color:#5F5F5F" />
		<stop offset="1" style="stop-color:#7F7F7F" />
	</radialGradient>
	<circle class="st0" cx="110.9" cy="110.9" r="110.9" />
</svg>
`,Na=`type Domain -- a region in ℝⁿ
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

`,_a=`-- diagram dimensions (in px)
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

`,Qa={"Laplace.substance":Sa,"Nested.substance":Pa,"OffCenter.substance":Ea,"Poisson.substance":Ta,images:Ia,"walk-on-spheres-ball.svg":Va,"walk-on-spheres.domain":Na,"walk-on-spheres.style":_a},qa=`AWord Shells
AWord Minkowski
AWord Exact
AWord Riemannian
AWord BCs
AWord CurveSurface

BWord Tools
BWord Survey
BWord Courses
BWord Software
BWord Videos

AutoLabel All
Label Riemannian "shape space formulation"
Label BCs "boundary conditions"
Label Shells "repulsive shells"
Label Exact "exact discretization"
Label CurveSurface "repulsive curves + surfaces"
Label Minkowski "Minkowski penalties"
Label Tools "end-user tools"
Label Survey "survey article"
Label Courses "local & outreach courses"
Label Software "open-source software"
Label Videos "K-12 videos"

`,Ra=`type Word

type AWord <: Word -- word related to topic A
type BWord <: Word -- word related to topic B
`,Da=`-- Set the target diagram size
canvas {
   width = 240
   height = 180
}

-- A few colors used throughout
Colors {
   color black = rgba(0.,0.,0.,1.)
   color lightBlue = rgba( 27./255., 31./255., 138./255., .2 )
   color lightGreen = rgba( 27./255., 138./255., 31./255., .2 )
}

-- Define a global box around the canvas
Global {
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      strokeColor: rgba(0.,0.,0.,.05)
      fillColor: none()
      strokeWidth: 1
   }
}

-- Draw each word as text inside a
-- rounded rectangle.  Note that this
-- rule will get applied to all Words,
-- of any subtype.
forall Word w {

   -- Draw the text
   w.text = Text {
      center: (?,?) -- the center location will be optimized by Penrose
      string: w.label -- the label comes from the Substance program
      fillColor: Colors.black
      fontFamily: "Palatino"
      fontSize: "9px" -- make sure to use px, not pt, since pt appears inconsistently across programs/browsers
   }

   -- Draw the rectangle
   scalar padding = 8
   w.box = Rectangle {
      -- Use the same center, width, and
      -- height as the text, but add some padding
      center: w.text.center
      width: w.text.width + padding
      height: w.text.height + padding

      fillColor: Colors.lightGray
      strokeColor: Colors.black
      strokeWidth: .5
      cornerRadius: 5
   }

   -- Make sure the rectangle is always on the
   -- canvas (since the rectangle surrounds the text,
   -- the text will also then be on the canvas).
   ensure contains( Global.box, w.box )

   -- Draw the rectangle and text above the canvas (just in case
   -- the canvas is drawn using, e.g., an opaque fill color).
   layer w.box above Global.box
   layer w.text above Global.box
}

-- Set specific colors for specific types of text
forall AWord w {
   override w.box.fillColor = Colors.lightBlue
}
forall BWord w {
   override w.box.fillColor = Colors.lightGreen
}

-- Make sure no text boxes overlap
forall Word w1; Word w2 {
   ensure disjoint( w1.box, w2.box, 3. )
}

-- Encourage words of the same kind to be close to each-other
forall AWord w1; AWord w2 {
   encourage near( w1.box, w2.box )
}
forall BWord w1; BWord w2 {
   encourage near( w1.box, w2.box )
}


`,$a={"example.substance":qa,"word-cloud.domain":Ra,"word-cloud.style":Da},n={animation:a,"atoms-and-bonds":d,"closest-point":u,"closest-point-ellipse":x,"curve-examples":L,"existential-graph-domain":E,"exterior-algebra":M,"fake-3d-linear-algebra":N,"fancy-text":R,"full-moon":F,"geometry-domain":Ln,"graph-domain":$t,"group-theory":Xt,"hyperbolic-domain":to,hypergraph:so,"lagrange-bases":uo,"linear-algebra-domain":Lo,"logic-circuit-domain":To,"matrix-ops":Ko,"mesh-set-domain":n0,"minkowski-tests":p0,mobius:h0,molecules:e,"monoidal-category-domain":v0,"penrose-sound":L0,"persistent-homology":B0,"set-theory-domain":K0,"shape-distance":cr,"shape-spec":hr,"spec-shape-callout":mr,"structural-formula":Dr,"triangle-mesh-2d":ra,"triangle-mesh-3d":sa,tutorials:Aa,"walk-on-spheres":Qa,"word-cloud":$a},Fa={domain:"typeppp Set",substancce:"Set A + B",style:`
  Set a {

  }
  `},ja={domain:`
type Set
`,substance:`
Set A
AutoLabel All
`,style:`
canvas {
  width = 500
  height = 500
}
forall Set X {
  X.shape = Circle { strokeWidth : 0 }
  X.text  = Equation { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`,variation:""},Ha={substance:n["set-theory-domain"]["continuousmap.substance"],style:n["set-theory-domain"]["continuousmap.style"],domain:n["set-theory-domain"]["functions.domain"],variation:""},za={variation:"ArtemisCrane740",domain:n["exterior-algebra"]["exterior-algebra.domain"],substance:n["exterior-algebra"]["vector-wedge.substance"],style:n["exterior-algebra"]["exterior-algebra.style"]},Ua={variation:"MyrtleApe55311",domain:n["linear-algebra-domain"]["linear-algebra.domain"],substance:n["linear-algebra-domain"]["twoVectorsPerp-unsugared.substance"],style:n["linear-algebra-domain"]["linear-algebra-paper-simple.style"]};export{za as a,Ha as c,Fa as e,ja as o,Ua as v};
//# sourceMappingURL=PenrosePrograms-02594003.js.map
