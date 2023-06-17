import{a as n,e}from"./index-59e475b0.js";const t=`
Vertex UCLA, SRI, UCSB, Utah, BBN, MIT, RAND, SDC, HARV, LL, CWRU, CMU, NASA

Edge(UCLA, SRI)
Edge(UCLA, UCSB)
Edge(SRI, UCSB)
Edge(SRI, Utah)
Edge(UCSB, Utah)
Edge(SRI, BBN)
Edge(BBN, MIT)
Edge(BBN, RAND)
Edge(RAND, SDC)
Edge(SDC, UCSB)
Edge(BBN, HARV)
Edge(HARV, MIT)
Edge(MIT, LL)
Edge(LL, NASA)
Edge(NASA, UCSB)
Edge(BBN, CWRU)
Edge(CWRU, CMU)

AutoLabel All
`,r=n("graph-domain"),o=`canvas {
  width = 600
  height = 600
}

layout = [dots, text, adjust]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
}

forall Vertex v {
  v.center = (?, ?)

  v.text = Text {
    center: v.center
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }

  padding = 4
  v.dot = Circle {
    center: v.center
    r: padding + max(v.text.width, v.text.height) / 2
    fillColor : #0000
    strokeColor : color.black
    strokeWidth: 2
  }
  v.dot2 = Circle {
    center: v.center
    r: padding + max(v.text.width, v.text.height) / 2
    fillColor : #ffffffff
    strokeColor : #0000
  }
  v.text above v.dot2 
  v.dot above v.text

  encourage shapeDistance(v.dot, v.text) == num.labelDist except dots
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v where Edge(u, v) as e {
  a = u.dot.center
  b = v.dot.center

  vec2 p1 = (?, ?)
  vec2 p2 = (?, ?)
  vec2 p3 = (?, ?)
  vec2 p4 = (?, ?)
  vec2 p5 = (?, ?)
  vec2 p6 = (?, ?)
  vec2 p7 = (?, ?)
  vec2 p8 = (?, ?)
  vec2 p9 = (?, ?)

  radius = 6
  shape e.c1 = Circle {
      center: p1
      r: radius
      fillColor: #0000
  }
  shape e.c2 = Circle {
      center: p2
      r: radius
      fillColor: #0000
  }
  shape e.c3 = Circle {
      center: p3
      r: radius
      fillColor: #0000
  }
  shape e.c4 = Circle {
      center: p4
      r: radius
      fillColor: #0000
  }
  shape e.c5 = Circle {
      center: p5
      r: radius
      fillColor: #0000
  }
  shape e.c6 = Circle {
      center: p6
      r: radius
      fillColor: #0000
  }
  shape e.c7 = Circle {
      center: p7
      r: radius
      fillColor: #0000
  }
  shape e.c8 = Circle {
      center: p8
      r: radius
      fillColor: #0000
  }
  shape e.c9 = Circle {
      center: p9
      r: radius
      fillColor: #0000
  }

  shape e.poly = Polyline {
    points: [a, p1, p2, p3, p4, p5, p6, p7, p8, p9, b]
    strokeWidth: 2
    strokeColor: #000
  }

  encourage minimal(elasticEnergy(e.poly.points, false))
  encourage isEquilateral(e.poly.points, false)

  e.start = a
  e.end = b
  e.arrow = Line {
    start: a
    end: b
    strokeColor: #0000
  }

  e.arrow below u.dot
  e.arrow below v.dot

  -- Gravity - very optional and useless
  -- encourage minimal(a[1])
  -- encourage minimal(b[1])

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
}

forall Vertex u; Vertex v where Edge(u, v) as e; u has label {
  ensure disjoint(u.dot, e.c3) in adjust
  ensure disjoint(u.dot, e.c4) in adjust
  ensure disjoint(u.dot, e.c5) in adjust
  ensure disjoint(u.dot, e.c6) in adjust
  ensure disjoint(u.dot, e.c7) in adjust
  u.dot2 above e.poly
}

forall Vertex u; Vertex v where Edge(u, v) as e; v has label {
  ensure disjoint(v.dot, e.c3) in adjust
  ensure disjoint(v.dot, e.c4) in adjust
  ensure disjoint(v.dot, e.c5) in adjust
  ensure disjoint(v.dot, e.c6) in adjust
  ensure disjoint(v.dot, e.c7) in adjust
  v.dot2 above e.poly
}

forall Vertex u; Vertex v; Vertex w where Edge(u, v) as e; w has label {
  ensure disjoint(w.dot, e.c3) in adjust
  ensure disjoint(w.dot, e.c4) in adjust
  ensure disjoint(w.dot, e.c5) in adjust
  ensure disjoint(w.dot, e.c6) in adjust
  ensure disjoint(w.dot, e.c7) in adjust
  w.dot2 above e.poly
}
`,s={substance:t,style:[{contents:o,resolver:r}],domain:e,variation:"CascaraKudu41689"};export{s as default};
