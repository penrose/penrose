import{d as n,s as e,a as t}from"./vector-wedge.substance-c51fe7ca.js";const o=`canvas {
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



`,r=`-- Types
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
`,a=`VectorSpace V
Vector u 
In(u, V)
Vector v 
In(v, V)
Unit(u)
Orthogonal(v, u)
AutoLabel All
`,i=`canvas {
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
}`,s=`AutoLabel All

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
From(f, A, B)`,c=`type Set
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
`,l=`type Set

predicate Not(Prop p1)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`,d=`canvas {
  width = 800
  height = 700
}

forall Set x {
  x.icon = Circle {
    strokeWidth : 0
  }

  x.text = Equation {
    string : x.label
    fontSize : "32px"
  }

  ensure contains(x.icon, x.text)
  encourage sameCenter(x.text, x.icon)
  x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
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
`,u={domain:"typeppp Set",substancce:"Set A + B",style:`
  Set a {

  }
  `},S={domain:l,substance:`
Set A
AutoLabel All
`,style:d,variation:""},h={substance:s,style:i,domain:c,variation:""},v={variation:"ArtemisCrane740",domain:n,substance:e,style:t},f={variation:"MyrtleApe55311",domain:r,substance:a,style:o};export{v as a,h as c,u as e,S as o,f as v};
//# sourceMappingURL=PenrosePrograms-cd468070.js.map
