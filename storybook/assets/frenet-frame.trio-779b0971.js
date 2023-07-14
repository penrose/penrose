import{m as n}from"./resolver-11ab920e.js";import{d as e}from"./curves.domain-5da78019.js";import"./iframe-f5449c82.js";const t=`Point p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16
Curve c
In(c, p1)
In(c, p2)
In(c, p3)
In(c, p4)
In(c, p5)
In(c, p6)
In(c, p7)
In(c, p8)
In(c, p9)
In(c, p10)
In(c, p11)
In(c, p12)
In(c, p13)
In(c, p14)
In(c, p15)
In(c, p16)`,o=n("curve-examples"),r=`canvas {
  width = 800
  height = 800
}

layout = [convex, blob]

settings {
  scale = 40
  tension = 0.19
  dotRadius = 5
  dotColor = #000 
  tangentColor = #112b9eff
  normalColor = #a01111ff
  arrowThickness = 3
  arrowSize = .5
}

global {
   color black = #000
}

forall Point p {
   p.x = (?,?)

  shape p.dot = Circle {
    center: p.x
    r: settings.dotRadius
    fillColor: settings.dotColor
  }
}

collect Point p into ps
where In(c,p)
foreach Curve c {
   xs = listof x from ps
   c.points = xs
}

forall Curve c {

  shape curve = Path {
    d: interpolatingSpline( "closed", c.points, settings.tension )
    strokeColor: #0a600a7b
    strokeWidth: 2
    strokeLinejoin: "round"
  }

  c.ts = tangentVectors(c.points, true)
  c.ns = normalVectors(c.points, true)

  ensure isEquilateral(c.points, true)
  encourage elasticEnergy(c.points, true) == 8000 in blob
  ensure signedArea(c.points, true) == 40000 in convex
  ensure perimeter(c.points, true) == 1000
}

forall Point p; Curve c
where In(c,p) {

   vec2 x = p.x
   vec2 t = oneBasedElement( c.ts, match_id )
   vec2 n = oneBasedElement( c.ns, match_id )

   p.lineT = Line {
         start: x
         end: x + settings.scale * t
         strokeColor: settings.tangentColor
         strokeWidth: settings.arrowThickness
     endArrowhead: "straight"
     endArrowheadSize: settings.arrowSize
   }
   p.lineN = Line {
     start: x
     end: x + settings.scale * n
     strokeColor: settings.normalColor
     strokeWidth: settings.arrowThickness
     endArrowhead: "straight"
     endArrowheadSize: settings.arrowSize
   }

   layer p.dot above p.lineT
   layer p.dot above p.lineN
}

`,p={substance:t,style:[{contents:r,resolver:o}],domain:e,variation:"WoodbineCod7138",excludeWarnings:[]};export{p as default};
//# sourceMappingURL=frenet-frame.trio-779b0971.js.map
