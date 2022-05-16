export const error = {
  domain: `typeppp Set`,
  substancce: `Set A + B`,
  style: `
  Set a {

  }
  `,
};
export const oneSet = {
  domain: `
type Set
`,
  substance: `
Set A
AutoLabel All
`,
  style: `
canvas {
  width = 500
  height = 500
}
Set X {
  X.shape = Circle { strokeWidth : 0 }
  X.text  = Equation { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`,
  variation: "",
};

export const continuousMap = {
  substance: `
AutoLabel All

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
From(f, A, B)
`,
  style: `
  canvas {
    width = 800
    height = 800
  }
  
  Const {
    Const.strokeWidth = 1.5
    Const.padding = 20.0
  }
  
  Colors {
    Colors.black = rgba(0.0, 0.0, 0.0, 1.0)
    Colors.lightBlue = rgba(0.1, 0.1, 0.9, 0.2)
    Colors.lightYellow = rgba(0.95, 0.96, 0.92, 0.5)
  }
  
  Set x {
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
  Set x; Set y
  where IsSubset(x, y) {
    ensure contains(y.icon, x.icon, 10.0)
    -- y.sizeFn    = ensure smallerThan(x.icon, y.icon)
    y.outsideFn = ensure disjoint(y.text, x.icon, 1.0)
    x.icon above y.icon
  }
  
  Map f
  where From(f, X, Y); IsSubset(X, R1); IsSubset(Y, R2)
  with Set X; Set Y; Set R1; Set R2 {
    f.padding = 20.0
  
      f.icon = Line {
        start : (R1.icon.center[0] + R1.icon.width / 2.0 + f.padding, R1.icon.center[1])
        end : (R2.icon.center[0] - R2.icon.width / 2.0 - f.padding, R2.icon.center[1])
        strokeWidth : 2.0
        strokeColor : Colors.black
        endArrowhead: true
          -- style : "curved"
      }
  
      f.text     = Equation {
        -- Doesn't seem to work after the first resample. Is the server updating f.text.height on resample?
        -- x : (f.icon.startX + f.icon.endX) / 2.0
        -- y : (f.icon.startY + f.icon.endY) / 2.0 + 1.1 * f.text.height
        string : f.label
        -- rotation : 0.0
      }
  
      encourage centerLabelAbove(f.icon, f.text, 5.0)
  
      -- Unused?
      -- f.centerFn = encourage centerArrow(f.icon, R1.icon, R2.icon)
  }
  
  Set \`U\` {
      override \`U\`.icon.strokeStyle = "dashed"
      override \`U\`.icon.strokeWidth = Const.strokeWidth
  }
  
  Set \`V\` {
      override \`V\`.icon.strokeStyle = "dashed"
      override \`V\`.icon.strokeWidth = Const.strokeWidth
  }
  
  -- TODO: use subtyping for reals?
  Set \`Rn\` {
      \`Rn\`.iconSize = ?
  
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
  
      ensure minSize(\`Rn\`.icon)
      ensure maxSize(\`Rn\`.icon, canvas.height / 3.)
  }
  
  Set \`Rm\`
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
  
      ensure minSize(\`Rm\`.icon)
      ensure maxSize(\`Rm\`.icon, canvas.height / 3.)
  }
`,
  domain: `
  type Set
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
  
`,
  variation: "",
};

// https://github.com/penrose/penrose/tree/c7bf34b2da79ce22fd1490a617c00040bc5a33ab/examples/exterior-algebra
export const vectorWedge = {
  variation: "NuthatchDunlin52049",

  domain: `type Scalar

-- define vectors and bivectors as subtypes of a base k-vector type
type kVector
type Vector <: kVector
type Bivector <: kVector

-- some basic operations on k-vectors
function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector

-- (more can be added here ) --
`,

  substance: `Vector a, b, c
Bivector u := Wedge(a,b)
Bivector v := Wedge(b,c)

AutoLabel All
Label u $a \\wedge b$
Label v $b \\wedge c$
`,

  style: `-- define the size of the drawing
canvas {
   width = 200
   height = 200
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

-- for each Vector declared in the .sub program, this match will
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
      endArrowhead: true
      arrowheadSize: .5
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
      startArrowhead: true
      arrowheadSize: .5
   }
}

-- draw a label for the bivector if it has one
forall Bivector w
where v has label {
   w.labelText = Equation {
      string: w.label
      center: w.c -- marker center
      fillColor: w.solidColor
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
   }
}
`,
};

export const vectorsPerp = {
  variation: "MyrtleApe55311",

  domain: `-- Types
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
`,

  substance: `VectorSpace V
Vector u 
In(u, V)
Vector v 
In(v, V)
Unit(u)
Orthogonal(v, u)
AutoLabel All
`,

  style: `canvas {
  width = 400
  height = 400
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
        x = { 1, {2, 3} }
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
    endArrowhead : true
    arrowheadSize : const.arrowheadSize
    strokeDasharray : "4 1 2"
  }

   u.vector = u.arrow.end - u.arrow.start -- Vector sugar for subtraction

   ensure contains(U.background, u.arrow)
   ensure contains(U.background, u.text)
   ensure atDist(u.arrow, u.text, 15.0)
   ensure lessThan(20, len(u.arrow))

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
}`,
};
