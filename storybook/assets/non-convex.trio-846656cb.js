import{m as n}from"./resolver-33472b42.js";import"./iframe-39c020d4.js";const a=`
Maze M
Triangle T1, T2, T3
`,x=n("minkowski-tests/maze"),e=`
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
`,c=`
type Maze
type Triangle
`,t={substance:a,style:[{contents:e,resolver:x}],domain:c,variation:"",excludeWarnings:[]};export{t as default};
//# sourceMappingURL=non-convex.trio-846656cb.js.map
