import{m as n}from"./resolver-8467d754.js";import{d as e}from"./simple-graph.domain-b2957daf.js";import"./iframe-e4576df4.js";const t=`Vertex a, b, c, d, e, f

Edge(a, b)
Edge(a, c)
Edge(a, d)
Edge(b, c)
Edge(b, e)
Edge(c, e)
Edge(d, e)
Edge(e, f)

Label a "𝑎"
Label b "𝑏"
Label c "𝑐"
Label d "𝑑"
Label e "𝑒"
Label f "𝑓"
`,i=n("graph-domain"),s=`canvas {
  width = 400
  height = 400
}

layout = [dots, text, adjust, dots, text, adjust, text, adjust]

color {
  black = #000000
  white = #ffffff
}

num {
  radius = 5
  labelDist = 5
  edgeDist = 100
  repelDist = 1.5 * edgeDist
  offset = 10
  loopRadius = 15
  pointerX = 6
  pointerY = 4
}

forall Vertex v {
  v.dot = Circle {
    center: (?, ?)
    r: num.radius
    fillColor : color.black
  }
  v.dot2 = Circle {
    center: v.dot.center
    r: 2 * num.radius
    fillColor : #0000
  }

  v.text = Text {
    string: v.label
    fillColor: color.black
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  }
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist except dots
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist)-- in text
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

  ensure perimeter(e.poly.points, false) < 1.5 * vdist(a, b)
  encourage perimeter(e.poly.points, false) == vdist(a, b)
  encourage elasticEnergy(e.poly.points, false) == 0
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

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
}

forall Vertex u; Vertex v where Edge(u, v) as e; u has label {
  ensure disjoint(u.text, e.c1) in adjust
  ensure disjoint(u.text, e.c2) in adjust
  ensure disjoint(u.text, e.c3) in adjust
  ensure disjoint(u.text, e.c4) in adjust
  ensure disjoint(u.text, e.c5) in adjust
  ensure disjoint(u.text, e.c6) in adjust
  ensure disjoint(u.text, e.c7) in adjust
  ensure disjoint(u.text, e.c8) in adjust
  ensure disjoint(u.text, e.c9) in adjust

  ensure disjoint(u.dot2, e.c2) in adjust
  ensure disjoint(u.dot2, e.c3) in adjust
  ensure disjoint(u.dot2, e.c4) in adjust
  ensure disjoint(u.dot2, e.c5) in adjust
  ensure disjoint(u.dot2, e.c6) in adjust
  ensure disjoint(u.dot2, e.c7) in adjust
  ensure disjoint(u.dot2, e.c8) in adjust
}

forall Vertex u; Vertex v where Edge(u, v) as e; v has label {
  ensure disjoint(v.text, e.c1) in adjust
  ensure disjoint(v.text, e.c2) in adjust
  ensure disjoint(v.text, e.c3) in adjust
  ensure disjoint(v.text, e.c4) in adjust
  ensure disjoint(v.text, e.c5) in adjust
  ensure disjoint(v.text, e.c6) in adjust
  ensure disjoint(v.text, e.c7) in adjust
  ensure disjoint(v.text, e.c8) in adjust
  ensure disjoint(v.text, e.c9) in adjust

  ensure disjoint(v.dot2, e.c2) in adjust
  ensure disjoint(v.dot2, e.c3) in adjust
  ensure disjoint(v.dot2, e.c4) in adjust
  ensure disjoint(v.dot2, e.c5) in adjust
  ensure disjoint(v.dot2, e.c6) in adjust
  ensure disjoint(v.dot2, e.c7) in adjust
  ensure disjoint(v.dot2, e.c8) in adjust
}

forall Vertex u; Vertex v; Vertex w where Edge(u, v) as e; w has label {
  ensure disjoint(w.text, e.c1) in adjust
  ensure disjoint(w.text, e.c2) in adjust
  ensure disjoint(w.text, e.c3) in adjust
  ensure disjoint(w.text, e.c4) in adjust
  ensure disjoint(w.text, e.c5) in adjust
  ensure disjoint(w.text, e.c6) in adjust
  ensure disjoint(w.text, e.c7) in adjust
  ensure disjoint(w.text, e.c8) in adjust
  ensure disjoint(w.text, e.c9) in adjust

  ensure disjoint(w.dot2, e.c2) in adjust
  ensure disjoint(w.dot2, e.c3) in adjust
  ensure disjoint(w.dot2, e.c4) in adjust
  ensure disjoint(w.dot2, e.c5) in adjust
  ensure disjoint(w.dot2, e.c6) in adjust
  ensure disjoint(w.dot2, e.c7) in adjust
  ensure disjoint(w.dot2, e.c8) in adjust
}
`,u={substance:t,style:[{contents:s,resolver:i}],domain:e,variation:"CascaraKudu41689"};export{u as default};
//# sourceMappingURL=ex32.trio-592b92b8.js.map
