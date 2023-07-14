import{m as n}from"./resolver-84c3bc8f.js";import"./iframe-38af56f5.js";const t=`Text t
Label t "Hello, Penrose!"
Point p1, p2, p3, p4, p5
Point p6, p7, p8, p9, p10
Point p11, p12, p13, p14
Point p15, p16, p17, p18
Point p19, p20, p21, p22
Point p23, p24
Label p1 $\\alpha$
Label p2 $\\beta$
Label p3 $\\gamma$
Label p4 $\\delta$
Label p5 $\\epsilon$
Label p6 $\\zeta$
Label p7 $\\eta$
Label p8 $\\theta$
Label p9 $\\iota$
Label p10 $\\kappa$
Label p11 $\\lambda$
Label p12 $\\mu$
Label p13 $\\nu$
Label p14 $\\xi$
Label p15 $\\omicron$
Label p16 $\\pi$
Label p17 $\\rho$
Label p18 $\\sigma$
Label p19 $\\tau$
Label p20 $\\upsilon$
Label p21 $\\phi$
Label p22 $\\chi$
Label p23 $\\psi$
Label p24 $\\omega$

`,e=n("fancy-text"),a=`canvas {
   scalar width  = 533.
   scalar height = 300.
}

Global {

   shape bbox = Rectangle {
      center: (0.,0.)
      width: canvas.width
      height: canvas.height
      fillColor: rgba( .8,.95,.9, 1. )
      strokeColor: rgba( .6,.85,.6, 1. )
      strokeWidth: 4.
   }

}

-- Make some fun-looking text by
-- layering it on top of a couple
-- stroked copies
forall Text t {

   string t.titleString = t.label
   vec2 t.titleCenter = (0.,0.)

   shape t.text = Text {
      string: t.titleString
      center: t.titleCenter
      fillColor: rgba(1.,1.,1.,1.)
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "38px"
      strokeWidth: 4
      strokeColor: #ff0303
      paintOrder: "stroke"
   }
   shape t.outerStroke = Text {
      string: t.titleString
      center: t.titleCenter
      fillColor: #ff6603
      fontFamily: "Palatino"
      fontStyle: "italic"
      fontSize: "38px"
      strokeWidth: 8
      strokeColor: #ff6603
      paintOrder: "stroke"
   }
   -- shape t.innerStroke = Text {
   --    string: t.titleString
   --    center: t.titleCenter
   --    fillColor: rgba(0.,0.,0.,1.)
   --    fontFamily: "Palatino"
   --    fontStyle: "italic"
   --    fontSize: "38px"
   --    style: "stroke:#FF0303;stroke-width:4;stroke-miterlimit:2;"
   -- }
   -- shape t.outerStroke = Text {
   --    string: t.titleString
   --    center: t.titleCenter
   --    fillColor: rgba(0.,0.,0.,1.)
   --    fontFamily: "Palatino"
   --    fontStyle: "italic"
   --    fontSize: "38px"
   --    style: "stroke:#FF6603;stroke-width:8;stroke-miterlimit:2;"
   -- }

   layer t.text above t.outerStroke
   layer t.outerStroke above Global.bbox
}


-- draw each point as a circular disk,
-- whose size is allowed to vary
forall Point p {

   vec3 c = unit(sphereRandom())

   shape p.icon = Circle {
      center: (?,?)
      r: random(18,38)
      fillColor: rgba(abs(c[0]/2),abs(c[1]/2),abs(c[2]),.25)
   }
   layer p.icon above Global.bbox

   shape p.text = Equation {
      center: p.icon.center
      string: p.label
      fillColor: rgba(abs(c[0]/4),abs(c[1]/4),abs(c[2]/2),.25)
      fontSize: "28px"
   }
   layer p.text above Global.bbox

   -- keep all disks on the canvas
   ensure contains( Global.bbox, p.icon )
}

-- repel all pairs of points
forall Point p; Point q {
   vec2 x = p.icon.center
   vec2 y = q.icon.center
   Δ = x-y
   encourage equal( 200000./normsq(Δ), 0. )
}

`,o=`type Text
type Point
`,r={substance:t,style:[{contents:a,resolver:e}],domain:o,variation:"ValentinoDonkey58541",excludeWarnings:[]};export{r as default};
//# sourceMappingURL=fancy-text.trio-b2d96b05.js.map
