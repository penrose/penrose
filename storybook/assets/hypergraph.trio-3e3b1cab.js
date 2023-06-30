import{m as n}from"./resolver-9b16ebb5.js";import"./iframe-7b8a5a1d.js";const e=`
-- Graph
Graph G
Node N1, N2, N3, N4, N5, N6, N7, N8, N9

GraphHasNode(G, N1)
GraphHasNode(G, N2)
GraphHasNode(G, N3)
GraphHasNode(G, N4)
GraphHasNode(G, N5)
GraphHasNode(G, N6)
GraphHasNode(G, N7)
GraphHasNode(G, N8)
GraphHasNode(G, N9)

-- First layer
Layer1 L1
GraphHasLayer1(G, L1)
Edge E11, E12, E13, E14, E15

Layer1HasEdge(L1, E11)
Layer1HasEdge(L1, E12)
Layer1HasEdge(L1, E13)
Layer1HasEdge(L1, E14)
Layer1HasEdge(L1, E15)

EdgeHasNodeInLayer1(E11, N1)
EdgeHasNodeInLayer1(E11, N2)
EdgeHasNodeInLayer1(E11, N3)
EdgeHasNodeInLayer1(E12, N4)
EdgeHasNodeInLayer1(E13, N5)
EdgeHasNodeInLayer1(E14, N6)
EdgeHasNodeInLayer1(E14, N7)
EdgeHasNodeInLayer1(E15, N8)
EdgeHasNodeInLayer1(E15, N9)

-- Second layer
Layer2 L2
GraphHasLayer2(G, L2)
Edge E21, E22, E23, E24

Layer2HasEdge(L2, E21)
Layer2HasEdge(L2, E22)
Layer2HasEdge(L2, E23)
Layer2HasEdge(L2, E24)

EdgeHasNodeInLayer2(E21, N1)
EdgeHasNodeInLayer2(E22, N2)
EdgeHasNodeInLayer2(E22, N3)
EdgeHasNodeInLayer2(E23, N4)
EdgeHasNodeInLayer2(E23, N5)
EdgeHasNodeInLayer2(E24, N6)
EdgeHasNodeInLayer2(E24, N7)
EdgeHasNodeInLayer2(E24, N8)
EdgeHasNodeInLayer2(E24, N9)

-- Third layer
Layer3 L3
GraphHasLayer3(G, L3)
Edge E31, E32, E33

Layer3HasEdge(L3, E31)
Layer3HasEdge(L3, E32)
Layer3HasEdge(L3, E33)

EdgeHasNodeInLayer3(E31, N1)
EdgeHasNodeInLayer3(E32, N2)
EdgeHasNodeInLayer3(E32, N3)
EdgeHasNodeInLayer3(E32, N4)
EdgeHasNodeInLayer3(E31, N5)
EdgeHasNodeInLayer3(E33, N6)
EdgeHasNodeInLayer3(E33, N7)
EdgeHasNodeInLayer3(E33, N8)
EdgeHasNodeInLayer3(E33, N9)
`,r=n("hypergraph"),a=`
canvas {
  width = 800
  height = 700
}

forall Graph g {
  g.xl = ?
  g.x1 = ?
  g.x2 = ?
  g.x3 = ?
  g.xr = ?
  g.yb = ?
  g.yt = ?
  encourage g.xl == g.xr
  encourage g.yb == g.yt
  ensure 100 < g.x1 - g.xl
  ensure 100 < g.x2 - g.x1
  ensure 100 < g.x3 - g.x2
  ensure 100 < g.xr - g.x3
  encourage g.xl + g.xr == 0
  encourage g.yb + g.yt == 0
}

forall Node n {
  n.r = 5
  n.w = 3
  n.xl = ?
  n.x1 = ?
  n.x2 = ?
  n.x3 = ?
  n.xr = ?
  n.yl = ?
  n.y1 = ?
  n.y2 = ?
  n.y3 = ?
  n.yr = ?
  n.dotl = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
  }
  n.dotlP = Circle{ 
    r: n.r
    center: [n.xl, n.yl]
    fillColor: n.dotl.fillColor
  }
  n.dot1 = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot1P = Circle{ 
    r: n.r
    center: [n.x1, n.y1]
    fillColor: n.dotl.fillColor
  }
  n.dot2 = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot2P = Circle{ 
    r: n.r
    center: [n.x2, n.y2]
    fillColor: n.dotl.fillColor
  }
  n.dot3 = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dot3P = Circle{ 
    r: n.r
    center: [n.x3, n.y3]
    fillColor: n.dotl.fillColor
  }
  n.dotr = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.dotrP = Circle{ 
    r: n.r
    center: [n.xr, n.yr]
    fillColor: n.dotl.fillColor
  }
  n.linel1 = Path{
    d: makePath((n.xl, n.yl), (n.x1, n.y1) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line12 = Path{ 
    d: makePath((n.x1, n.y1), (n.x2, n.y2) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line23 = Path{ 
    d: makePath((n.x2, n.y2), (n.x3, n.y3) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  n.line3r = Path{
    d: makePath((n.x3, n.y3), (n.xr, n.yr) , 20, 0)
    strokeWidth: n.w
    strokeColor: n.dotl.fillColor
  }
  encourage abs(n.y1 - n.yl) == 50
  encourage abs(n.y1 - n.y2) == 50
  encourage abs(n.y3 - n.y2) == 50
  encourage abs(n.y3 - n.yr) == 50
}

forall Layer1 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Layer2 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Layer3 l {
  l.x = ?
  l.yb = ?
  l.yt = ?
}

forall Graph g; Layer1 l
where GraphHasLayer1(g, l) {
  override l.x = g.x1
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Layer2 l
where GraphHasLayer2(g, l) {
  override l.x = g.x2
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Layer3 l
where GraphHasLayer3(g, l) {
  override l.x = g.x3
  override l.yb = g.yb
  override l.yt = g.yt
}

forall Graph g; Node n
where GraphHasNode(g, n) {
  override n.xl = g.xl
  override n.x1 = g.x1
  override n.x2 = g.x2
  override n.x3 = g.x3
  override n.xr = g.xr
  ensure g.yb < n.yr
  ensure n.yr < g.yt
  ensure g.yb < n.yl
  ensure n.yl < g.yt
}

forall Node n1; Node n2 {
  encourage 50.0 < abs(n2.yl - n1.yl)
  encourage 50.0 < abs(n2.y1 - n1.y1)
  encourage 50.0 < abs(n2.y2 - n1.y2)
  encourage 50.0 < abs(n2.y3 - n1.y3)
  encourage 50.0 < abs(n2.yr - n1.yr)
}

forall Edge e {
  e.x = ?
  e.yt = ?
  e.yb = ?
  e.rectx = ?
  e.rect = Rectangle {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    width: 20.0
    height: e.yt - e.yb
    fillColor: rgba(1.0, 1.0, 1.0, 0.0)
  }
  e.ellipse = Ellipse {
    center: [e.rectx, 0.5 * (e.yt + e.yb)]
    rx: 10.0
    ry: 0.5 * (e.yt - e.yb)
    fillColor: rgba(0.0, 0.0, 0.0, 0.2)
  }
  encourage e.yt == e.yb
  ensure e.rectx == e.x
}

forall Layer1 l; Edge e
where Layer1HasEdge(l, e) {
  override e.x = l.x
  ensure l.yb < e.yb
  ensure e.yt < l.yt
}

forall Layer2 l; Edge e
where Layer2HasEdge(l, e) {
  override e.x = l.x
  ensure l.yb < e.yb
  ensure e.yt < l.yt
}

forall Layer3 l; Edge e
where Layer3HasEdge(l, e) {
  override e.x = l.x
  ensure l.yb < e.yb
  ensure e.yt < l.yt
}

forall Edge e; Node n
where EdgeHasNodeInLayer1(e, n) {
  ensure contains(e.rect, n.dot1, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer2(e, n) {
  ensure contains(e.rect, n.dot2, 4)
}

forall Edge e; Node n
where EdgeHasNodeInLayer3(e, n) {
  ensure contains(e.rect, n.dot3, 4)
}

forall Edge e1; Edge e2 {
  ensure disjoint(e1.rect, e2.rect, 20)
  encourage abs(e1.yt - e1.yb) + abs(e2.yt - e2.yb) < abs(e1.yt + e1.yb - e2.yt - e2.yb)
}
`,l=`
type Graph

type Layer

type Layer1
type Layer2
type Layer3

Layer1 <: Layer
Layer2 <: Layer
Layer3 <: Layer

type Node
type Edge

predicate GraphHasLayer1(Graph g, Layer1 l)
predicate GraphHasLayer2(Graph g, Layer2 l)
predicate GraphHasLayer3(Graph g, Layer3 l)

predicate GraphHasNode(Graph g, Node n)

predicate Layer1HasEdge(Layer1 l, Edge e)
predicate Layer2HasEdge(Layer2 l, Edge e)
predicate Layer3HasEdge(Layer3 l, Edge e)

predicate EdgeHasNodeInLayer1(Edge e, Node n)
predicate EdgeHasNodeInLayer2(Edge e, Node n)
predicate EdgeHasNodeInLayer3(Edge e, Node n)
`,d={substance:e,style:[{contents:a,resolver:r}],domain:l,variation:"ConceptualMeerkat694",excludeWarnings:[]};export{d as default};
//# sourceMappingURL=hypergraph.trio-3e3b1cab.js.map
