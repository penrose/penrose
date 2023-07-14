import{s as n}from"./cubic-bezier.substance-6a79508c.js";import{a as e}from"./index-941eab72.js";import{d as p}from"./curves.domain-5da78019.js";const r=e("curve-examples"),o=`canvas {
    width = 600
    height = 400
}

global {
    scalar t = 0.4
    color semiBlue = rgba( 27./255., 31./255., 138./255., .2 )
}

forall Curve c {
    vec2 c.p1 = (?, ?)
    vec2 c.p2 = (?, ?)
    vec2 c.p3 = (?, ?)
    vec2 c.p4 = (?, ?)

    points = [c.p1, c.p2, c.p3, c.p4]
    shape curve = Path {
        d: cubicCurveFromPoints("open", points)
        strokeWidth: 3.5
        strokeColor: global.semiBlue
        ensureOnCanvas: true
    }

    ensure equal(perimeter(points, false), 900)
    ensure equal(signedArea(points, false), 1e5)
}

forall Point p {
    vec2 p.p = (?, ?) 

    shape p.point = Circle {
        center: p.p
        r: 4
        fillColor: rgba(0,0,0,1)
    }

    shape p.text = Equation {
        string: p.label
        center: (?, ?)
    }

    encourage near(p.text, p.point)
}

forall Curve c; Point p1; Point p2; Point p3; Point p4
where c := CurveFromPoints(p1, p2, p3, p4) {
    override p1.p = c.p1
    override p2.p = c.p2
    override p3.p = c.p3
    override p4.p = c.p4
}

forall Point p; Point p1; Point p2 
where p := Lerp(p1, p2) {
    vec2 p3 = global.t * p1.p + (1 - global.t) * p2.p
    override p.p = p3

    shape line = Line {
        start: p1.p
        end: p2.p
        strokeWidth: 2
        strokeColor: rgba(0,0,0,1)
    }

    ensure disjoint(line, p.text, -3)
    ensure disjoint(line, p1.text, -3)
    ensure disjoint(line, p2.text, -3)
}
`,a={substance:n,style:[{contents:o,resolver:r}],domain:p,variation:"CauliflowerDeer97138",excludeWarnings:[]};export{a as default};
