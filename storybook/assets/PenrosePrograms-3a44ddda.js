import{d as e,s as t,a as n}from"./vector-wedge.substance-f7ebd76f.js";const o=`canvas {
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
where Subset(x, y) {
  ensure contains(y.icon, x.icon, 10.0)
  -- y.sizeFn    = ensure smallerThan(x.icon, y.icon)
  y.outsideFn = ensure disjoint(y.text, x.icon, 1.0)
  x.icon above y.icon
}

forall Map f
where From(f, X, Y); Subset(X, R1); Subset(Y, R2)
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
}`,i=`AutoLabel All

Set A
Set U
Label U $f^{-1}(V)$
Set Rn
Label Rn $\\mathbb{R}^n$
Subset(U, A)
Subset(A, Rn)

Set B
Set V
Set Rm
Label Rm $\\mathbb{R}^m$
Subset(V, B)
Subset(B, Rm)

Map f
From(f, A, B)`,r=`canvas {
  width = 800
  height = 700
}

forall Set x {
  shape x.icon = Circle { }
  shape x.text = Equation {
    string : x.label
    fontSize : "32px"
  }
  ensure contains(x.icon, x.text)
  encourage norm(x.text.center - x.icon.center) == 0
  layer x.text above x.icon
}

forall Set x; Set y
where Subset(x, y) {
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  layer x.icon above y.icon
}

forall Set x; Set y
where Disjoint(x, y) {
  ensure disjoint(x.icon, y.icon)
}

forall Set x; Set y
where Intersecting(x, y) {
  ensure overlapping(x.icon, y.icon)
  ensure disjoint(y.text, x.icon)
  ensure disjoint(x.text, y.icon)
}
`,s=`type Set
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

predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)
predicate Equal(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
`,a=`type Set

predicate Disjoint(Set s1, Set s2)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)
`,c={domain:"typeppp Set",substancce:"Set A + B",style:`
  Set a {

  }
  `},l={domain:a,substance:`
Set A
AutoLabel All
`,style:r,variation:""},d={substance:i,style:o,domain:s,variation:""},S={variation:"ArtemisCrane740",domain:e,substance:t,style:n};export{d as c,c as e,l as o,S as v};
