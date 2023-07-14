import{s as n}from"./cubic-bezier.substance-6a79508c.js";import{m as e}from"./resolver-11ab920e.js";import{d as o}from"./curves.domain-5da78019.js";import"./iframe-f5449c82.js";const t=e("curve-examples"),s=`canvas {
  width = 800
  height = 800
}

layout = [convex, blob]

settings {
  scale = 100 
  closed = true
  tension = 0.19
  strokeLinejoin = "round"
  offsetColor = #222fa8ff
}

forall Curve c {
  points = [(?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?), (?, ?)]

  shape curve = Path {
    d: interpolatingSpline( "closed", points, settings.tension )
    strokeColor: #0a600aff
    strokeWidth: 3
    strokeLinejoin: "round"
    fillColor: #ffffffff
  }
  shape bloom = Path {
    d: interpolatingSpline( "closed", points, settings.tension )
    strokeColor: #8e8e8e96
    strokeWidth: 40
    strokeLinejoin: "round"
    style: "filter:blur(15px);"
  }
  bloom below curve

  shape offset1 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -5), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 2.4
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset2 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -10), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 2.1
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset3 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -15), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 1.8
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset4 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -20), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 1.5
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset5 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -25), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 1.2
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset6 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -30), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 0.9
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset7 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -35), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 0.6
    strokeLinejoin: settings.strokeLinejoin
  }
  shape offset8 = Path {
    d: interpolatingSpline( "closed", offsetCurve(points, settings.closed, -40), settings.tension )
    strokeColor: settings.offsetColor
    strokeWidth: 0.3
    strokeLinejoin: settings.strokeLinejoin
  }

  offset1 above bloom
  offset2 above bloom
  offset3 above bloom
  offset4 above bloom
  offset5 above bloom
  offset6 above bloom
  offset7 above bloom
  offset8 above bloom

  ensure isEquilateral(points, settings.closed)
  ensure elasticEnergy(points, settings.closed) == 40000 in blob
  ensure signedArea(points, settings.closed) == 80000 in convex
  ensure perimeter(points, settings.closed) == 1000

}`,a={substance:n,style:[{contents:s,resolver:t}],domain:o,variation:"ClaretPorpoise01614",excludeWarnings:[]};export{a as default};
//# sourceMappingURL=offset.trio-e4dd1333.js.map
