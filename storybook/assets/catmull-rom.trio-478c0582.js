import{m as n}from"./resolver-028225a2.js";import"./iframe-2bac3a0c.js";const e=`Point p1, p2, p3, p4, p5, p6, p7, p8
InterpolatingCurve c
`,o=n("curve-examples/catmull-rom"),t=`canvas {
   width = 510
   height = 382
}

global {
   shape background = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: #05c2
   }

   shape line1 = Text {
      string: "shape spline = Path {"
      fontFamily: "Menlo"
      fontSize: "9px"
      fillColor: #ffdd00
   }
   shape line2 = Text {
      center: line1.center + (122,-12)
      string: "d: interpolatingSpline( “closed”, [ p1, ..., pN ], tension )"
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape line3 = Text {
      center: line1.center + (-54,-24)
      string: "}"
      fontFamily: line1.fontFamily
      fontSize: line1.fontSize
      fillColor: line1.fillColor
   }
   shape code = Group {
      shapes: [ line1, line2, line3 ]
   }

   shape textBox = Rectangle {
      fillColor: #127c
   }
   encourage textBox.width == 0
   encourage textBox.height == 0
   ensure contains( textBox, code, 10 )

   layer code above textBox
   layer textBox above background
}

forall Point p {

   vec2 p.x = (?,?)

   shape p.icon = Circle {
      center: p.x
      r: 3
      fillColor: #fff
      strokeColor: #000
      strokeWidth: 1.5
   }

   layer global.textBox above p.icon
}

collect Point p into points {

   centers = listof x from points

   scalar tension = .25
   scalar baseWidth = 1
   scalar width = 1

   string closing = "closed"

   shape spline0 = Path {
      d: interpolatingSpline( closing, centers, 0*tension/3 )
      strokeColor: #1b8a1f33
      strokeWidth: baseWidth + 0*width/3
      strokeLinejoin: "round"
   }
   shape spline1 = Path {
      d: interpolatingSpline( closing, centers, 1*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 1*width/3
      strokeLinejoin: "round"
   }
   shape spline2 = Path {
      d: interpolatingSpline( closing, centers, 2*tension/3 )
      strokeColor: #1b8a1f99
      strokeWidth: baseWidth + 2*width/3
      strokeLinejoin: "round"
   }
   shape global.spline3 = Path {
      d: interpolatingSpline( closing, centers, 3*tension/3 )
      strokeColor: #1b8a1fff
      strokeWidth: baseWidth + 3*width/3
      strokeLinejoin: "round"
   }

   layer global.textBox above global.spline3
   layer global.spline3 above spline2
   layer spline2 above spline1
   layer spline1 above spline0
   layer spline0 above global.background
}

forall Point p {
   layer p.icon above global.spline3
}

`,l=`type Point
type InterpolatingCurve

`,s={substance:e,style:[{contents:t,resolver:o}],domain:l,variation:"UranusYak398",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=catmull-rom.trio-478c0582.js.map
