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
