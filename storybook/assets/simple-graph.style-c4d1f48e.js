import{m as e}from"./resolver-b9429209.js";const n=e("graph-domain"),o=`canvas {
  width = 400
  height = 400
}

layout = [dots, text]

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

forall Vertex u; Vertex v where Edge(u, v) as e {
  a = u.dot.center
  b = v.dot.center

  e.start = a
  e.end = b
  e.arrow = Line {
    start: a
    end: b
    strokeColor: color.black
  }

  e.arrow below u.dot
  e.arrow below v.dot

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
}

forall Vertex u; Vertex v where Edge(u, v) as e; u has label {
  encourage maximal(min(num.labelDist, rectLineDist(u.bottomLeft, u.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v where Edge(u, v) as e; v has label {
  encourage maximal(min(num.labelDist, rectLineDist(v.bottomLeft, v.topRight, e.start, e.end))) in text
}

forall Vertex u; Vertex v; Vertex w where Edge(u, v) as e; w has label {
  encourage maximal(min(num.labelDist, rectLineDist(w.bottomLeft, w.topRight, e.start, e.end))) in text
}
`;export{n as r,o as s};
//# sourceMappingURL=simple-graph.style-c4d1f48e.js.map
