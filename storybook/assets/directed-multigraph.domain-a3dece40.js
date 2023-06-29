import{m as e}from"./resolver-16fd6907.js";const t=e("graph-domain"),o=`canvas {
  width = 400
  height = 400
}

layout = [dots, loops, arrows, text]

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
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black
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

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}

forall Vertex u; Vertex v {
  d = vdist(u.dot.center, v.dot.center)
  dHat = num.repelDist
  -- equation 6 from https://ipc-sim.github.io/
  encourage minimal(max(0, -sqr(d - dHat) * log(d / dHat))) in dots

  ensure disjoint(u.text, v.text, num.labelDist) in text
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v) {
  a = u.dot.center
  b = v.dot.center
  t = normalize(b - a) -- tangent
  n = rot90(t) -- normal
  m = (a + b) / 2 -- midpoint

  e.start = a
  e.end = b
  e.theta = angleOf(t)
  e.offset = ? in dots
  e.arrow = Path {
    d: quadraticCurveFromPoints("open", [a, m + e.offset * n, b])
    strokeColor: color.black
  }

  e.step = ? in arrows
  e.pointerCenter = m + (e.offset / 2) * n + e.step * t
  p = e.pointerCenter
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path {
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot
  e.pointer below e.arrow

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
  encourage minimal(sqr(e.offset)) in dots
  encourage minimal(sqr(e.step)) 
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(u, v) {
  ensure abs(e2.offset - e1.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(v, u) {
  ensure abs(e1.offset + e2.offset) > 2 * num.offset in dots
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v); u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Link e where e := Arc(u, v); v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w; Link e where e := Arc(u, v); w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}

forall Vertex v; Link e where e := Arc(v, v) {
  e.theta = ? in loops
  n = (cos(e.theta), sin(e.theta)) -- normal
  delta = num.loopRadius * n

  e.arrow = Circle {
    center: v.dot.center + delta
    r: num.loopRadius
    strokeWidth: 1
    strokeColor: color.black
    fillColor: none()
  }

  t = rot90(n)
  p = v.dot.center + 2 * delta
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path {
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black
  }

  e.arrow below v.dot
  e.pointer below e.arrow
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(u, v); e2 := Arc(v, v) {
  encourage maximal(cos(e2.theta - e1.theta)) in loops
}

forall Vertex u; Vertex v; Link e1; Link e2 where e1 := Arc(v, u); e2 := Arc(v, v) {
  encourage minimal(cos(e2.theta - e1.theta)) in loops
}

forall Link e1, e2
where e1 := Arc(a, b); e2 := Arc(c, d)
with Vertex a, b, c, d {
  ensure norm(e2.pointerCenter - e1.pointerCenter) > max(num.pointerX, num.pointerY)*3 in arrows
  encourage e1.step == e2.step
}
`,r=`type Vertex
type Link
constructor Arc(Vertex u, Vertex v) -> Link
`;export{r as d,t as r,o as s};
//# sourceMappingURL=directed-multigraph.domain-a3dece40.js.map
