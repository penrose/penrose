import{m as n}from"./resolver-9b16ebb5.js";const o=n("spectral-graphs"),s=`canvas {
  width = 400
  height = 400
}

colors {
  contour = #00000093
}

global {
    scalar colorOffset = random(0, 100)
    shape background = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: hsva(colorOffset, 10, 100, 1)
        ensureOnCanvas: false
    } 
}

forall Node n {
    vec2 n.center = (?, ?)
    scalar n.normsq = normsq(n.center)
    scalar fillHue = global.colorOffset + 1.7 * norm(n.center)
    shape n.icon = Circle {
        center: n.center
        r: 6 - 0.03 * norm(n.center)
        fillColor: hsva(fillHue, 100, 100, 1)
        strokeColor: colors.contour
        strokeWidth: 2.6 - 0.008 * norm(n.center)
        ensureOnCanvas: false
    }
    shape n.bloom = Circle {
        center: n.icon.center
        r: n.icon.r
        fillColor: n.icon.fillColor
        strokeColor: n.icon.strokeColor
        strokeWidth: n.icon.strokeWidth
        ensureOnCanvas: false
    }
}

collect Node n into nodes {

   blooms = listof bloom from nodes
   
   shape nodeShadows = Group {
      shapes: blooms
      style: "filter:blur(3px);"
   }
}

forall Edge e; Node a; Node b
where e := MakeEdge(a, b) {
    scalar e.distsq = vdistsq(a.center, b.center)
    shape e.icon = Line {
        start: a.center
        end: b.center
        strokeWidth: 75 / vdist(a.center, b.center)
        strokeColor: colors.contour
        ensureOnCanvas: false
    }
    shape e.bloom = Line {
        start: e.icon.start
        end: e.icon.end
        strokeWidth: e.icon.strokeWidth
        strokeColor: e.icon.strokeColor
        ensureOnCanvas: false
    }
    layer e.bloom below a.icon
    layer e.bloom below b.icon
}

collect Edge e into edges {

   blooms = listof bloom from edges
   
   shape nodeShadows = Group {
      shapes: blooms
      style: "filter:blur(2px);"
   }
}


forall Node n; Edge e {
    e.icon below n.icon
}

collect Node n into nodes {
    normsqs = listof normsq from nodes
    centers = listof center from nodes
    scalar scale = (canvas.width / 5) * (canvas.height / 5)
    ensure sum(normsqs) / count(normsqs) == scale
    ensure norm(sumVectors(centers)) == 0
}

collect Edge e into edges
where e := MakeEdge(a, b)
foreach Node a; Node b {
    distsqs = listof distsq from edges
    encourage sum(distsqs) == 0
}
`,r=`type Node
type Edge

constructor MakeEdge(Node a, Node b) -> Edge`;export{r as d,o as r,s};
//# sourceMappingURL=graph.domain-3fcbb072.js.map
