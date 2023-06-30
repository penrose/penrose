import{a as n}from"./index-d0dd7cba.js";const e=`VectorSpace V
Vector u 
In(u, V)
Vector v 
In(v, V)
Unit(u)
Orthogonal(v, u)
AutoLabel All
`,r=n("linear-algebra-domain"),t=`canvas {
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
   ensure signedDistanceRect(bboxPts(u.text), u.arrow.end) == 15.0
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
}`,o=`-- Types
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
`,i={substance:e,style:[{contents:t,resolver:r}],domain:o,variation:"",excludeWarnings:["BBoxApproximationWarning"]};export{i as default};
