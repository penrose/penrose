import{a as n}from"./index-2b9b0491.js";const o=n("shape-distance"),e=`canvas {
  width = 800
  height = 700
}

forall Point p {
    p.icon = Circle {
        strokeWidth : 0
        fillColor: rgba(0,0,0,1)
        r: 3
    }
}

forall ReverseL g {
    g.icon = Polygon {
        strokeWidth : 1
        fillColor: rgba(68/255,114/255,148/255,1)
        points: [(-200,-200), (-200,0), (0,0), (0,200), (200,200), (200,-200)]
    }
}

forall Star s {
    s.icon = Polygon {
        strokeWidth : 1
        fillColor: rgba(68/255,114/255,148/255,1)
        points: [(-200,-200), (0,200), (200,-300), (-200,150), (250,150)]
        fillRule : "evenodd"
    }
}

forall Line l {
    l.icon = Line {
        strokeWidth : 8
        start : (-200,-100)
        end : (250, 100)
    }
}

forall Polyline l {
    l.icon = Polyline {
        strokeWidth : 8
        points : [(-200,-200), (0,200), (200,-300), (-200,150), (250,150)]
    }
}

forall Text t {
    shape t.icon = Text {
      string: "Hello Nimo"
      center: (0,0)
      fillColor: rgba(0.,0.,0.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "128px"
   }
}

forall Ellipse e {
    shape e.icon = Ellipse {
      center: (10,10)
      rx : 100
      ry : 50
      
   }
}

forall Point p; Shape s
where Around(p, s) {
    ensure equal(signedDistance(s.icon, p.icon.center), 20)
}
`;export{o as r,e as s};
