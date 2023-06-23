import{d as n,s as e,a as r}from"./vector-wedge.substance-0729ca0f.js";import{d as o,s as t}from"./twoVectorsPerp-unsugared.substance-e42b5c97.js";import{s as a,a as i,d as l}from"./functions.domain-2dba78fc.js";const s=`canvas {
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



`,h={domain:"typeppp Set",substancce:"Set A + B",style:`
  Set a {

  }
  `},v={domain:`
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
`,variation:""},p={substance:a,style:i,domain:l,variation:""},m={variation:"ArtemisCrane740",domain:n,substance:e,style:r},b={variation:"MyrtleApe55311",domain:o,substance:t,style:s};export{m as a,p as c,h as e,v as o,b as v};
//# sourceMappingURL=PenrosePrograms-1250beaf.js.map
