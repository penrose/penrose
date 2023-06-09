import{a as n}from"./index-514348b8.js";const o=n("atoms-and-bonds"),r=`canvas {
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
}`,a=`type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate SBond( Atom a1, Atom a2 )
symmetric predicate WBond( Atom a1, Atom a2 )`;export{a as d,o as r,r as s};
