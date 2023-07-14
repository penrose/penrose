import{s as n}from"./cubic-bezier.substance-6a79508c.js";import{m as t}from"./resolver-f1d01b85.js";import{d as e}from"./curves.domain-5da78019.js";import"./iframe-02305a86.js";const s=t("curve-examples"),o=`canvas {
  width = 800
  height = 800
}

layout = [convex, blob]

settings {
  scale = 60
  tension = 0.19
  dotRadius = 5
  dotColor = #000 
  tangentColor = #112b9eff
  normalColor = #a01111ff
  arrowThickness = 3
  arrowSize = .5
}

forall Curve c {

  points = [(?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?)]

  shape curve = Path {
    d: interpolatingSpline( "closed", points, settings.tension )
    strokeColor: #0a600a7b
    strokeWidth: 2
    strokeLinejoin: "round"
  }

  ts = tangentVectors(points, true)
  ns = normalVectors(points, true)
  cs = curvatures(points, true)

  shape dot = Circle {
    center: (points[0][0], points[0][1])
    r: settings.dotRadius
    fillColor: settings.dotColor
  }

  scalar centerX = points[0][0] + 1 / cs[0] * ns[0][0]
  scalar centerY = points[0][1] + 1 / cs[0] * ns[0][1]
  shape osculatingCenter = Circle {
    center: (centerX, centerY)
    r: settings.dotRadius
    fillColor: settings.dotColor
  }
  shape osculatingCircle = Circle {
    center: (centerX, centerY)
    r: 1 / cs[0]
    fillColor: #0000
    strokeColor: #000
    strokeWidth: 1
  }

  lineT = Line {
	start: (points[0][0], points[0][1])
	end: (points[0][0] + settings.scale * ts[0][0], points[0][1] + settings.scale * ts[0][1])
	strokeColor: settings.tangentColor
	strokeWidth: settings.arrowThickness
    endArrowhead: "straight"
    endArrowheadSize: settings.arrowSize
  }

  dot above lineT

  lineN = Line {
    start: (points[0][0], points[0][1])
    end: (points[0][0] + settings.scale * ns[0][0], points[0][1] + settings.scale * ns[0][1])
    strokeColor: settings.normalColor
    strokeWidth: settings.arrowThickness
    endArrowhead: "straight"
    endArrowheadSize: settings.arrowSize
  }

  dot above lineN

  ensure isEquilateral(points, true)
  ensure signedArea(points, true) == 60000 in convex
  ensure perimeter(points, true) == 1000

}`,c={substance:n,style:[{contents:o,resolver:s}],domain:e,variation:"RetroKouprey113",excludeWarnings:[]};export{c as default};
//# sourceMappingURL=osculating-circle.trio-886665c8.js.map
