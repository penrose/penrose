import{a as n}from"./index-7ab1eacb.js";const e=`
Frames F
Point P1, P2, P3, P4, P5, P6, P7, P8, P9, P10

ConnectedOnFrame2(P1, P2)
ConnectedOnFrame2(P2, P3)
ConnectedOnFrame2(P3, P1)
ConnectedOnFrame2(P4, P5)
Connection C2P1P2 := MakeConnection2(P1, P2)
Connection C2P2P3 := MakeConnection2(P2, P3)
Connection C2P3P1 := MakeConnection2(P3, P1)
Connection C2P4P5 := MakeConnection2(P4, P5)

ConnectedOnFrame3(P1, P2)
ConnectedOnFrame3(P2, P3)
ConnectedOnFrame3(P3, P1)
ConnectedOnFrame3(P3, P4)
ConnectedOnFrame3(P4, P5)
ConnectedOnFrame3(P5, P6)
ConnectedOnFrame3(P6, P7)
ConnectedOnFrame3(P6, P8)
ConnectedOnFrame3(P6, P9)
ConnectedOnFrame3(P7, P8)
ConnectedOnFrame3(P7, P9)
ConnectedOnFrame3(P8, P9)
ConnectedOnFrame3(P9, P10)
ConnectedOnFrame3(P10, P1)
Connection C3P1P2 := MakeConnection3(P1, P2)
Connection C3P2P3 := MakeConnection3(P2, P3)
Connection C3P3P1 := MakeConnection3(P3, P1)
Connection C3P3P4 := MakeConnection3(P3, P4)
Connection C3P4P5 := MakeConnection3(P4, P5)
Connection C3P5P6 := MakeConnection3(P5, P6)
Connection C3P6P7 := MakeConnection3(P6, P7)
Connection C3P6P8 := MakeConnection3(P6, P8)
Connection C3P6P9 := MakeConnection3(P6, P9)
Connection C3P7P8 := MakeConnection3(P7, P8)
Connection C3P7P9 := MakeConnection3(P7, P9)
Connection C3P8P9 := MakeConnection3(P8, P9)
Connection C3P9P10 := MakeConnection3(P9, P10)
Connection C3P10P1 := MakeConnection3(P10, P1)

-- This can be deleted in the future (after issue #775 is resolved)

NotConnectedOnFrame1(P1, P2)
NotConnectedOnFrame1(P1, P3)
NotConnectedOnFrame1(P1, P4)
NotConnectedOnFrame1(P1, P5)
NotConnectedOnFrame1(P1, P6)
NotConnectedOnFrame1(P1, P7)
NotConnectedOnFrame1(P1, P8)
NotConnectedOnFrame1(P1, P9)
NotConnectedOnFrame1(P1, P10)
NotConnectedOnFrame1(P2, P3)
NotConnectedOnFrame1(P2, P4)
NotConnectedOnFrame1(P2, P5)
NotConnectedOnFrame1(P2, P6)
NotConnectedOnFrame1(P2, P7)
NotConnectedOnFrame1(P2, P8)
NotConnectedOnFrame1(P2, P9)
NotConnectedOnFrame1(P2, P10)
NotConnectedOnFrame1(P3, P4)
NotConnectedOnFrame1(P3, P5)
NotConnectedOnFrame1(P3, P6)
NotConnectedOnFrame1(P3, P7)
NotConnectedOnFrame1(P3, P8)
NotConnectedOnFrame1(P3, P9)
NotConnectedOnFrame1(P3, P10)
NotConnectedOnFrame1(P4, P5)
NotConnectedOnFrame1(P4, P6)
NotConnectedOnFrame1(P4, P7)
NotConnectedOnFrame1(P4, P8)
NotConnectedOnFrame1(P4, P9)
NotConnectedOnFrame1(P4, P10)
NotConnectedOnFrame1(P5, P6)
NotConnectedOnFrame1(P5, P7)
NotConnectedOnFrame1(P5, P8)
NotConnectedOnFrame1(P5, P9)
NotConnectedOnFrame1(P5, P10)
NotConnectedOnFrame1(P6, P7)
NotConnectedOnFrame1(P6, P8)
NotConnectedOnFrame1(P6, P9)
NotConnectedOnFrame1(P6, P10)
NotConnectedOnFrame1(P7, P8)
NotConnectedOnFrame1(P7, P9)
NotConnectedOnFrame1(P7, P10)
NotConnectedOnFrame1(P8, P9)
NotConnectedOnFrame1(P8, P10)
NotConnectedOnFrame1(P9, P10)

NotConnectedOnFrame2(P1, P4)
NotConnectedOnFrame2(P1, P5)
NotConnectedOnFrame2(P1, P6)
NotConnectedOnFrame2(P1, P7)
NotConnectedOnFrame2(P1, P8)
NotConnectedOnFrame2(P1, P9)
NotConnectedOnFrame2(P1, P10)
NotConnectedOnFrame2(P2, P4)
NotConnectedOnFrame2(P2, P5)
NotConnectedOnFrame2(P2, P6)
NotConnectedOnFrame2(P2, P7)
NotConnectedOnFrame2(P2, P8)
NotConnectedOnFrame2(P2, P9)
NotConnectedOnFrame2(P2, P10)
NotConnectedOnFrame2(P3, P4)
NotConnectedOnFrame2(P3, P5)
NotConnectedOnFrame2(P3, P6)
NotConnectedOnFrame2(P3, P7)
NotConnectedOnFrame2(P3, P8)
NotConnectedOnFrame2(P3, P9)
NotConnectedOnFrame2(P3, P10)
NotConnectedOnFrame2(P4, P6)
NotConnectedOnFrame2(P4, P7)
NotConnectedOnFrame2(P4, P8)
NotConnectedOnFrame2(P4, P9)
NotConnectedOnFrame2(P4, P10)
NotConnectedOnFrame2(P5, P6)
NotConnectedOnFrame2(P5, P7)
NotConnectedOnFrame2(P5, P8)
NotConnectedOnFrame2(P5, P9)
NotConnectedOnFrame2(P5, P10)
NotConnectedOnFrame2(P6, P7)
NotConnectedOnFrame2(P6, P8)
NotConnectedOnFrame2(P6, P9)
NotConnectedOnFrame2(P6, P10)
NotConnectedOnFrame2(P7, P8)
NotConnectedOnFrame2(P7, P9)
NotConnectedOnFrame2(P7, P10)
NotConnectedOnFrame2(P8, P9)
NotConnectedOnFrame2(P8, P10)
NotConnectedOnFrame2(P9, P10)

NotConnectedOnFrame3(P1, P4)
NotConnectedOnFrame3(P1, P5)
NotConnectedOnFrame3(P1, P6)
NotConnectedOnFrame3(P1, P7)
NotConnectedOnFrame3(P1, P8)
NotConnectedOnFrame3(P1, P9)
NotConnectedOnFrame3(P2, P4)
NotConnectedOnFrame3(P2, P5)
NotConnectedOnFrame3(P2, P6)
NotConnectedOnFrame3(P2, P7)
NotConnectedOnFrame3(P2, P8)
NotConnectedOnFrame3(P2, P9)
NotConnectedOnFrame3(P2, P10)
NotConnectedOnFrame3(P3, P5)
NotConnectedOnFrame3(P3, P6)
NotConnectedOnFrame3(P3, P7)
NotConnectedOnFrame3(P3, P8)
NotConnectedOnFrame3(P3, P9)
NotConnectedOnFrame3(P3, P10)
NotConnectedOnFrame3(P4, P6)
NotConnectedOnFrame3(P4, P7)
NotConnectedOnFrame3(P4, P8)
NotConnectedOnFrame3(P4, P9)
NotConnectedOnFrame3(P4, P10)
NotConnectedOnFrame3(P5, P7)
NotConnectedOnFrame3(P5, P8)
NotConnectedOnFrame3(P5, P9)
NotConnectedOnFrame3(P5, P10)
NotConnectedOnFrame3(P6, P10)
NotConnectedOnFrame3(P7, P10)
NotConnectedOnFrame3(P8, P10)
`,o=n("persistent-homology"),t=`canvas {
  width = 800
  height = 700
}

forall Frames f {
  f.d = ?
  f.r1 = ?
  f.r2 = ?
  f.r3 = ?
  f.y = ?
  f.ylab = ?
  ensure 200 < f.d
  ensure f.d < 500
  ensure 8 < f.r1
  ensure f.r1 < f.r2
  ensure 15 < f.r2
  ensure f.r2 < f.r3
  ensure 25 < f.r3
  encourage f.ylab - f.y < 400

  f.axis = Line {
    strokeColor: rgba(0.0, 0.0, 0.0, 0.5)
    strokeWidth: 2
    endArrowhead: "straight"
    start: (-1.5 * f.d, f.y)
    end: (1.5 * f.d, f.y)
    endArrowheadSize: 0.7
  }

  f.dot0 = Circle {
    center: (-1.5 * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot1 = Circle {
    center: ((-1.5 + 2.7 * f.r1 / f.r3) * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot2 = Circle {
    center: ((-1.5 + 2.7 * f.r2 / f.r3) * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  f.dot3 = Circle {
    center: (1.2 * f.d, f.y)
    r: 4
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }

  f.eps0 = Equation {
    string: "0"
    center: (f.dot0.center[0] - 20, f.dot0.center[1])
    fontSize: "30px"
  }
  f.eps = Equation {
    string: "\\varepsilon"
    center: (1.5 * f.d + 20, f.dot0.center[1])
    fontSize: "30px"
  }
  f.eps1 = Equation {
    string: "\\varepsilon_1"
    center: (f.dot1.center[0] + 20, f.dot1.center[1] - 20)
    fillColor: rgba(0.1, 0.3, 0.1, 0.8)
    fontSize: "30px"
  }
  f.eps2 = Equation {
    string: "\\varepsilon_2"
    center: (f.dot2.center[0] + 20, f.dot2.center[1] - 20)
    fillColor: rgba(0.5, 0.1, 0.1, 0.8)
    fontSize: "30px"
  }
  f.eps3 = Equation {
    string: "\\varepsilon_3"
    center: (f.dot3.center[0] + 20, f.dot3.center[1] - 20)
    fillColor: rgba(0.1, 0.1, 0.5, 0.8)
    fontSize: "30px"
  }

  f.lab1 = Equation {
    string: "X \\oplus B_{\\varepsilon_{1}}"
    fillColor: rgba(0.1, 0.3, 0.1, 0.8)
    center: (f.dot1.center[0], f.ylab)
    fontSize: "30px"
  }
  f.lab2 = Equation {
    string: "X \\oplus B_{\\varepsilon_{2}}"
    fillColor: rgba(0.5, 0.1, 0.1, 0.8)
    center: (f.dot2.center[0], f.ylab)
    fontSize: "30px"
  }
  f.lab3 = Equation {
    string: "X \\oplus B_{\\varepsilon_{3}}"
    fillColor: rgba(0.1, 0.1, 0.5, 0.8)
    center: (f.dot3.center[0], f.ylab)
    fontSize: "30px"
  }
}

forall Point p {
  p.x = ?
  p.d12 = ?
  p.d23 = ?
  p.y = ?
  p.r1 = ?
  p.r2 = ?
  p.r3 = ?

  p.dot1 = Circle {
    center: (p.x - p.d12, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot2 = Circle {
    center: (p.x, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.dot3 = Circle {
    center: (p.x + p.d23, p.y)
    r: 3
    fillColor: rgba(0.0, 0.0, 0.0, 1.0)
  }
  p.circle1 = Circle {
    center: (p.x - p.d12, p.y)
    r: p.r1
    fillColor: rgba(0.2, 0.6, 0.2, 0.3)
  }
  p.circle2 = Circle {
    center: (p.x, p.y)
    r: p.r2
    fillColor: rgba(1.0, 0.2, 0.2, 0.3)
  }
  p.circle3 = Circle {
    center: (p.x + p.d23, p.y)
    r: p.r3
    fillColor: rgba(0.2, 0.2, 1.0, 0.3)
  }

  p.shade1 = Image {
    center: p.circle1.center 
    width: p.circle1.r * 2.0
    height: p.circle1.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
   }
  p.shade2 = Image {
    center: p.circle2.center 
    width: p.circle2.r * 2.0
    height: p.circle2.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
  }
  p.shade3 = Image {
    center: p.circle3.center 
    width: p.circle3.r * 2.0
    height: p.circle3.r * 2.0
    href: "persistent-homology-shading.svg"
    ensureOnCanvas: false
  }

  p.circle1 below p.shade1
  p.circle2 below p.shade2
  p.circle3 below p.shade3
  
  p.dot1 above p.circle1
  p.dot2 above p.circle2
  p.dot3 above p.circle3

}

forall Point p; Frames f {
  ensure p.d12 == f.dot2.center[0] - f.dot1.center[0]
  ensure p.d23 == f.dot3.center[0] - f.dot2.center[0]
  ensure p.r1 == f.r1
  ensure p.r2 == f.r2
  ensure p.r3 == f.r3
  ensure f.y < p.y - p.r3 - 30
  ensure p.y + p.r3 + 30 < f.ylab
}

forall Point p1; Point p2 {
  p1.circle1 below p2.shade1
  p1.circle2 below p2.shade2
  p1.circle3 below p2.shade3
  p2.circle1 below p1.shade1
  p2.circle2 below p1.shade2
  p2.circle3 below p1.shade3
  ensure abs(p1.x - p2.x) + 100.0 < p1.d12
  ensure abs(p1.x - p2.x) + 100.0 < p1.d23
}

forall Point p1; Point p2 
where NotConnectedOnFrame1(p1, p2) {
  ensure disjoint(p1.circle1, p2.circle1, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame2(p1, p2) {
  ensure disjoint(p1.circle2, p2.circle2, 10)
}

forall Point p1; Point p2 
where NotConnectedOnFrame3(p1, p2) {
  ensure disjoint(p1.circle3, p2.circle3, 10)
}

forall Point p1; Point p2 
where ConnectedOnFrame1(p1, p2) {
  ensure overlapping(p1.circle1, p2.circle1, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame2(p1, p2) {
  ensure overlapping(p1.circle2, p2.circle2, 7)
}

forall Point p1; Point p2 
where ConnectedOnFrame3(p1, p2) {
  ensure overlapping(p1.circle3, p2.circle3, 7)
}

forall Connection c; Point p1; Point p2
where c := MakeConnection1(p1, p2) {
  c.line = Line {
    start: p1.dot1.center
    end: p2.dot1.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}

forall Connection c; Point p1; Point p2
where c := MakeConnection2(p1, p2) {
  c.line = Line {
    start: p1.dot2.center
    end: p2.dot2.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}

forall Connection c; Point p1; Point p2
where c := MakeConnection3(p1, p2) {
  c.line = Line {
    start: p1.dot3.center
    end: p2.dot3.center
    strokeColor: rgba(0.0, 0.0, 0.0, 1.0)
    strokeWidth: 3
  }
  p1.shade1 below c.line
  p1.shade2 below c.line
  p1.shade3 below c.line
  p2.shade1 below c.line
  p2.shade2 below c.line
  p2.shade3 below c.line
}
`,r=`
type Point
type Frames
type Connection

constructor MakeConnection1(Point p1, Point p2) -> Connection
constructor MakeConnection2(Point p1, Point p2) -> Connection
constructor MakeConnection3(Point p1, Point p2) -> Connection

predicate ConnectedOnFrame1(Point p1, Point p2)
predicate ConnectedOnFrame2(Point p1, Point p2)
predicate ConnectedOnFrame3(Point p1, Point p2)

predicate NotConnectedOnFrame1(Point p1, Point p2)
predicate NotConnectedOnFrame2(Point p1, Point p2)
predicate NotConnectedOnFrame3(Point p1, Point p2)
`,c={substance:e,style:[{contents:t,resolver:o}],domain:r,variation:"RainmakerMarten88256"};export{c as default};
