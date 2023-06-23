import{m as n}from"./resolver-52f68c7c.js";import{d as e}from"./all-shapes.domain-8745a2d9.js";import"./iframe-b9a20e36.js";const t=`-- fill shapes
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
`,c=n("shape-spec"),o=`canvas {
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
`,i={substance:t,style:[{contents:o,resolver:c}],domain:e,variation:""};export{i as default};
//# sourceMappingURL=all-shapes.trio-b8292c45.js.map
