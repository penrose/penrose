export const oneSet = {
  domainString: `
type Set
`,
  substanceString: `
Set A
AutoLabel All
`,
  styleString: `
canvas {
  width = 500
  height = 500
}
Set X {
  X.shape = Circle { strokeWidth : 0 }
  X.text  = Text { string: X.label }
  ensure contains(X.shape, X.text)
  ensure maxSize(X.shape, canvas.width / 2)
}
`,
};

export const continuousMap = {
  substanceString: `
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
  styleString: `
canvas {
  width = 800
  height = 700
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
        color : Colors.lightBlue
        strokeColor : Colors.black
        strokeStyle : "solid"
        strokeWidth : 1.0
        -- rotation : 0.0
    }

    x.text    = Text {
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

    f.icon = Arrow {
      start : (R1.icon.center[0] + R1.icon.side / 2.0 + f.padding, R1.icon.center[1])
      end : (R2.icon.center[0] - R2.icon.side / 2.0 - f.padding, R2.icon.center[1])
      thickness : 2.0
      color : Colors.black
        -- style : "curved"
    }

    f.text     = Text {
      -- Doesn't seem to work after the first resample. Is the server updating f.text.h on resample?
      -- x : (f.icon.startX + f.icon.endX) / 2.0
      -- y : (f.icon.startY + f.icon.endY) / 2.0 + 1.1 * f.text.h
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
    override \`Rn\`.icon = Square {
      -- Works but is slow
      -- x : -100.0
      -- y = 0.0
      color : Colors.lightYellow
      -- rotation : 0.0
      strokeWidth : Const.strokeWidth
      strokeColor : Colors.black
    }

    override \`Rn\`.text.center = (\`Rn\`.icon.center[0] + \`Rn\`.icon.side / 2.0 - Const.padding, \`Rn\`.icon.center[1] + \`Rn\`.icon.side / 2.0 - Const.padding)

    delete \`Rn\`.labelFn
    delete \`Rn\`.outsideFn

    ensure minSize(\`Rn\`.icon)
    ensure maxSize(\`Rn\`.icon, canvas.height / 3.)
}

Set \`Rm\`
with Set \`Rn\` {
    -- TODO: factor this block out
    override \`Rm\`.icon = Square {
        color : Colors.lightYellow
        center : (\`Rn\`.icon.center[0] + 400.0, \`Rn\`.icon.center[1])
        side  : \`Rn\`.icon.side
        -- rotation : 0.0
        strokeWidth : 1.0
        strokeColor : Colors.black
    }

     override \`Rm\`.text.center = (\`Rm\`.icon.center[0] + \`Rm\`.icon.side / 2.0 - Const.padding, \`Rm\`.icon.center[1] + \`Rm\`.icon.side / 2.0 - Const.padding)

    delete \`Rm\`.labelFn
    delete \`Rm\`.outsideFn

    ensure minSize(\`Rm\`.icon)
    ensure maxSize(\`Rm\`.icon, canvas.height / 3.)
}
`,
  domainString: `
type Set
type Point
type Map

constructor Singleton : Point p -> Set

function Intersection : Set a * Set b -> Set
function Union : Set a * Set b -> Set
function Subtraction : Set a * Set b -> Set
function CartesianProduct : Set a * Set b -> Set
function Difference : Set a * Set b -> Set
function Subset : Set a * Set b -> Set
function AddPoint : Point p * Set s1 -> Set

predicate Not : Prop p1
predicate From : Map f * Set domain * Set codomain
predicate Empty : Set s
predicate Intersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate Equal : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map

notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"
notation "A ∩ B = ∅" ~ "Not(Intersecting(A, B))"
notation "f: A -> B" ~ "Map f; From(f, A, B)"
`,
};
