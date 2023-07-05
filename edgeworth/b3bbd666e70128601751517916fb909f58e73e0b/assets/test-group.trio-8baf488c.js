import{a as n}from"./index-7196a63e.js";import{d as r}from"./ray-intersect.domain-6b8fb535.js";const e=`-- For each ray, draws the first intersection with any shape in the group G.

Ray r1
Ray r2
Ray r3
Ray r4
Ray r5
Ray r6
Ray r7
Ray r8
Ray r9
Ray r10
Ray r11
Ray r12
Ray r13
Ray r14
Ray r15
Ray r16
Ray r17
Ray r18
Ray r19
Ray r20
Ray r21
Ray r22
Ray r23
Ray r24
Ray r25
Ray r26
Ray r27
Ray r28
Ray r29
Ray r30
Ray r31
Ray r32
Ray r33
Ray r34
Ray r35
Ray r36
Ray r37
Ray r38
Ray r39
Ray r40
Ray r41
Ray r42
Ray r43
Ray r44
Ray r45
Ray r46
Ray r47
Ray r48
Ray r49
Ray r50
Ray r51
Ray r52
Ray r53
Ray r54
Ray r55
Ray r56
Ray r57
Ray r58
Ray r59
Ray r60
Ray r61
Ray r62
Ray r63
Ray r64
Ray r65
Ray r66
Ray r67
Ray r68
Ray r69
Ray r70
Ray r71
Ray r72
Ray r73
Ray r74
Ray r75
Ray r76
Ray r77
Ray r78
Ray r79
Ray r80
Ray r81
Ray r82
Ray r83
Ray r84
Ray r85
Ray r86
Ray r87
Ray r88
Ray r89
Ray r90
Ray r91
Ray r92
Ray r93
Ray r94
Ray r95
Ray r96
Ray r97
Ray r98
Ray r99
Ray r100

Group G
`,a=n("geometric-queries/ray-intersect"),y=`canvas {
   width = 800
   height = 700
}

-- Rules for drawing each type of shape

forall Ray r {
   scalar theta = random( 4, 5.42 )
   vec2 r.pos = (0,canvas.height/2 - 10.)
   vec2 r.vec = [ cos(theta), sin(theta) ]

   shape r.icon = Circle {
      center: r.pos
      r: 5
      fillColor: #000000ff
   } 
}

forall Group G {
   shape G.line = Line {
      start: (-canvas.width/2,-canvas.height/2 + 10)
      end: (canvas.width/2,-canvas.height/2 + 10)
      strokeWidth: 5
   }
   shape G.rectangle = Rectangle {
      center: (?, ?)
      width: random(40,100)
      height: random(40,100)
      strokeWidth: 1
   }
   shape G.circle = Circle {
      center: (?,?)
      r: random(40,80)
   }
   shape G.ellipse = Ellipse {
      center: (?,?)
      rx: random(60,100)
      ry: random(60,100)
   }

   vec2 p0 = (?,?)
   vec2 p1 = (?,?)
   vec2 p2 = (?,?)
   shape G.polygon = Polygon {
      points: [ p0, p1, p2 ]
   }
   vec2 e01 = p1-p0
   vec2 e12 = p2-p1
   vec2 e20 = p0-p2
   ensure angleBetween( e01, -e12 ) > toRadians(30)
   ensure angleBetween( e12, -e20 ) > toRadians(30)
   ensure angleBetween( e20, -e01 ) > toRadians(30)
   ensure abs(cross2D( e01, -e12 )) > 50

   shape G.icon = Group {
      shapes: [ G.line, G.rectangle, G.circle, G.ellipse, G.polygon ]
   }

   ensure disjoint( G.line, G.rectangle )
   ensure disjoint( G.line, G.circle )
   ensure disjoint( G.line, G.ellipse )
   ensure disjoint( G.line, G.polygon )
   ensure disjoint( G.rectangle, G.circle )
   ensure disjoint( G.rectangle, G.ellipse )
   ensure disjoint( G.rectangle, G.polygon )
   ensure disjoint( G.circle, G.ellipse )
   ensure disjoint( G.circle, G.polygon )
   ensure disjoint( G.ellipse, G.polygon )
}

forall Ray r; Shape S
{
   vec2 p = rayIntersect( S.icon, r.pos, r.vec )

   shape intersectDot = Circle {
      r: r.icon.r / 2
      center: p
      fillColor: #ffffffff
      strokeColor: #000000ff
      strokeWidth: 1.5
      ensureOnCanvas: false
   }

   shape line = Line {
      start: r.pos
      end: p
      strokeColor: rgba(1,.5,0,.1)
      strokeWidth: 3.5
      ensureOnCanvas: false
   }

   layer intersectDot below r.icon
   layer intersectDot above S.icon
   layer intersectDot above line
}


`,t={substance:e,style:[{contents:y,resolver:a}],domain:r,variation:"AntiguaSalmon4454",excludeWarnings:["BBoxApproximationWarning"]};export{t as default};
