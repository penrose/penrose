import{m as n}from"./resolver-2719b1fe.js";import"./iframe-4581b948.js";const s=`Manifold a, b

Map m := MakeMap(a, b)

Label a "M_1"
Label b "M_2"

Label m "\\phi"
`,i=n("curve-examples/blobs"),e=`canvas {
  width = 800
  height = 800
}

layout = [manifolds, maps]

settings {
  lineThickness = 2
  arrowThickness = 1.5
  blobbiness = 0.93
  pi = 3.14159
  fontSize = "15px"
  blobDistance = 300
}

forall Manifold m {

  shape m.eq = Equation {
    center: centerOfMass(m.poly.points)
    string: m.label
    fontSize: settings.fontSize
  }

  ensure isEquilateral(m.poly.points, true) in manifolds

  scalar radius = 80
  scalar length = 2 * settings.pi * radius
  scalar area = settings.blobbiness * settings.pi * radius * radius

  ensure perimeter(m.poly.points, true) == length in manifolds
  ensure signedArea(m.poly.points, true) == area in manifolds
  encourage elasticEnergy(m.poly.points, true) == 0 in manifolds

  vec2 p0 = (? in manifolds, ? in manifolds)
  vec2 p1 = (? in manifolds, ? in manifolds)
  vec2 p2 = (? in manifolds, ? in manifolds)
  vec2 p3 = (? in manifolds, ? in manifolds)
  vec2 p4 = (? in manifolds, ? in manifolds)
  vec2 p5 = (? in manifolds, ? in manifolds)
  vec2 p6 = (? in manifolds, ? in manifolds)
  vec2 p7 = (? in manifolds, ? in manifolds)
  vec2 p8 = (? in manifolds, ? in manifolds)
  vec2 p9 = (? in manifolds, ? in manifolds)
  vec2 p10 = (? in manifolds, ? in manifolds)
  vec2 p11 = (? in manifolds, ? in manifolds)
  vec2 p12 = (? in manifolds, ? in manifolds)
  vec2 p13 = (? in manifolds, ? in manifolds)
  vec2 p14 = (? in manifolds, ? in manifolds)
  vec2 p15 = (? in manifolds, ? in manifolds)
  vec2 p16 = (? in manifolds, ? in manifolds)
  vec2 p17 = (? in manifolds, ? in manifolds)
  vec2 p18 = (? in manifolds, ? in manifolds)
  vec2 p19 = (? in manifolds, ? in manifolds)
  vec2 p20 = (? in manifolds, ? in manifolds)
  vec2 p21 = (? in manifolds, ? in manifolds)
  vec2 p22 = (? in manifolds, ? in manifolds)
  vec2 p23 = (? in manifolds, ? in manifolds)
  vec2 p24 = (? in manifolds, ? in manifolds)
  vec2 p25 = (? in manifolds, ? in manifolds)
  vec2 p26 = (? in manifolds, ? in manifolds)
  vec2 p27 = (? in manifolds, ? in manifolds)
  vec2 p28 = (? in manifolds, ? in manifolds)
  vec2 p29 = (? in manifolds, ? in manifolds)
  vec2 p30 = (? in manifolds, ? in manifolds)
  vec2 p31 = (? in manifolds, ? in manifolds)
  vec2 p32 = (? in manifolds, ? in manifolds)
  vec2 p33 = (? in manifolds, ? in manifolds)
  vec2 p34 = (? in manifolds, ? in manifolds)
  vec2 p35 = (? in manifolds, ? in manifolds)
  vec2 p36 = (? in manifolds, ? in manifolds)
  vec2 p37 = (? in manifolds, ? in manifolds)
  vec2 p38 = (? in manifolds, ? in manifolds)
  vec2 p39 = (? in manifolds, ? in manifolds)
  vec2 p40 = (? in manifolds, ? in manifolds)
  vec2 p41 = (? in manifolds, ? in manifolds)
  vec2 p42 = (? in manifolds, ? in manifolds)
  vec2 p43 = (? in manifolds, ? in manifolds)
  vec2 p44 = (? in manifolds, ? in manifolds)
  vec2 p45 = (? in manifolds, ? in manifolds)
  vec2 p46 = (? in manifolds, ? in manifolds)
  vec2 p47 = (? in manifolds, ? in manifolds)
  vec2 p48 = (? in manifolds, ? in manifolds)
  vec2 p49 = (? in manifolds, ? in manifolds)
  vec2 p50 = (? in manifolds, ? in manifolds)
  vec2 p51 = (? in manifolds, ? in manifolds)
  vec2 p52 = (? in manifolds, ? in manifolds)
  vec2 p53 = (? in manifolds, ? in manifolds)
  vec2 p54 = (? in manifolds, ? in manifolds)
  vec2 p55 = (? in manifolds, ? in manifolds)
  vec2 p56 = (? in manifolds, ? in manifolds)
  vec2 p57 = (? in manifolds, ? in manifolds)
  vec2 p58 = (? in manifolds, ? in manifolds)
  vec2 p59 = (? in manifolds, ? in manifolds)
  vec2 p60 = (? in manifolds, ? in manifolds)
  vec2 p61 = (? in manifolds, ? in manifolds)
  vec2 p62 = (? in manifolds, ? in manifolds)
  vec2 p63 = (? in manifolds, ? in manifolds)
  vec2 p64 = (? in manifolds, ? in manifolds)
  vec2 p65 = (? in manifolds, ? in manifolds)
  vec2 p66 = (? in manifolds, ? in manifolds)
  vec2 p67 = (? in manifolds, ? in manifolds)
  vec2 p68 = (? in manifolds, ? in manifolds)
  vec2 p69 = (? in manifolds, ? in manifolds)
  vec2 p70 = (? in manifolds, ? in manifolds)
  vec2 p71 = (? in manifolds, ? in manifolds)
  vec2 p72 = (? in manifolds, ? in manifolds)
  vec2 p73 = (? in manifolds, ? in manifolds)
  vec2 p74 = (? in manifolds, ? in manifolds)
  vec2 p75 = (? in manifolds, ? in manifolds)
  vec2 p76 = (? in manifolds, ? in manifolds)
  vec2 p77 = (? in manifolds, ? in manifolds)
  vec2 p78 = (? in manifolds, ? in manifolds)
  vec2 p79 = (? in manifolds, ? in manifolds)
  vec2 p80 = (? in manifolds, ? in manifolds)
  vec2 p81 = (? in manifolds, ? in manifolds)
  vec2 p82 = (? in manifolds, ? in manifolds)
  vec2 p83 = (? in manifolds, ? in manifolds)
  vec2 p84 = (? in manifolds, ? in manifolds)
  vec2 p85 = (? in manifolds, ? in manifolds)
  vec2 p86 = (? in manifolds, ? in manifolds)
  vec2 p87 = (? in manifolds, ? in manifolds)
  vec2 p88 = (? in manifolds, ? in manifolds)
  vec2 p89 = (? in manifolds, ? in manifolds)
  vec2 p90 = (? in manifolds, ? in manifolds)
  vec2 p91 = (? in manifolds, ? in manifolds)
  vec2 p92 = (? in manifolds, ? in manifolds)
  vec2 p93 = (? in manifolds, ? in manifolds)
  vec2 p94 = (? in manifolds, ? in manifolds)
  vec2 p95 = (? in manifolds, ? in manifolds)
  vec2 p96 = (? in manifolds, ? in manifolds)
  vec2 p97 = (? in manifolds, ? in manifolds)
  vec2 p98 = (? in manifolds, ? in manifolds)
  vec2 p99 = (? in manifolds, ? in manifolds)

  shape m.poly = Polygon {
    points: [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49, p50, p51, p52, p53, p54, p55, p56, p57, p58, p59, p60, p61, p62, p63, p64, p65, p66, p67, p68, p69, p70, p71, p72, p73, p74, p75,  p76, p77, p78, p79, p80, p81, p82, p83, p84, p85, p86, p87, p88, p89, p90, p91, p92, p93, p94, p95, p96, p97, p98, p99]
    strokeWidth: settings.lineThickness
    strokeColor: #000
  }
  
}

forall Manifold a; Manifold b {

  ca = centerOfMass(a.poly.points)
  cb = centerOfMass(b.poly.points)
  ensure lessThan(settings.blobDistance, vdist(ca, cb)) in manifolds

}

forall Manifold a; Manifold b; Map m where m := MakeMap(a, b) {

  vec2 p0 = (?, ?)
  vec2 p1 = (?, ?)
  vec2 p2 = (?, ?)
  vec2 p3 = (?, ?)
  vec2 p4 = (?, ?)
  vec2 p5 = (?, ?)
  vec2 p6 = (?, ?)
  vec2 p7 = (?, ?)
  vec2 p8 = (?, ?)
  vec2 p9 = (?, ?)
  vec2 p10 = (?, ?)
  vec2 p11 = (?, ?)
  vec2 p12 = (?, ?)
  vec2 p13 = (?, ?)
  vec2 p14 = (?, ?)
  vec2 p15 = (?, ?)
  vec2 p16 = (?, ?)
  vec2 p17 = (?, ?)
  vec2 p18 = (?, ?)
  vec2 p19 = (?, ?)
  vec2 p20 = (?, ?)
  vec2 p21 = (?, ?)
  vec2 p22 = (?, ?)
  vec2 p23 = (?, ?)
  vec2 p24 = (?, ?)
  vec2 p25 = (?, ?)
  vec2 p26 = (?, ?)
  vec2 p27 = (?, ?)
  vec2 p28 = (?, ?)
  vec2 p29 = (?, ?)
  vec2 p30 = (?, ?)
  vec2 p31 = (?, ?)
  vec2 p32 = (?, ?)
  vec2 p33 = (?, ?)
  vec2 p34 = (?, ?)
  vec2 p35 = (?, ?)
  vec2 p36 = (?, ?)
  vec2 p37 = (?, ?)
  vec2 p38 = (?, ?)
  vec2 p39 = (?, ?)
  vec2 p40 = (?, ?)
  vec2 p41 = (?, ?)
  vec2 p42 = (?, ?)
  vec2 p43 = (?, ?)
  vec2 p44 = (?, ?)
  vec2 p45 = (?, ?)
  vec2 p46 = (?, ?)
  vec2 p47 = (?, ?)
  vec2 p48 = (?, ?)
  vec2 p49 = (?, ?)

  shape m.poly = Polyline {
    points: [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49]
    strokeWidth: settings.lineThickness
    strokeColor: #0000
    endArrowhead: "straight"
  }

  shape m.s0 = Line {
    start: p0
    strokeWidth: settings.lineThickness
    end: p1
    strokeColor: rgba(0,0,0,1.00)
  }
  shape m.s1 = Line {
    start: p1
    strokeWidth: settings.lineThickness
    end: p2
    strokeColor: rgba(0,0,0,0.98)
  }
  shape m.s2 = Line {
    start: p2
    strokeWidth: settings.lineThickness
    end: p3
    strokeColor: rgba(0,0,0,0.96)
  }
  shape m.s3 = Line {
    start: p3
    strokeWidth: settings.lineThickness
    end: p4
    strokeColor: rgba(0,0,0,0.94)
  }
  shape m.s4 = Line {
    start: p4
    strokeWidth: settings.lineThickness
    end: p5
    strokeColor: rgba(0,0,0,0.92)
  }
  shape m.s5 = Line {
    start: p5
    strokeWidth: settings.lineThickness
    end: p6
    strokeColor: rgba(0,0,0,0.90)
  }
  shape m.s6 = Line {
    start: p6
    strokeWidth: settings.lineThickness
    end: p7
    strokeColor: rgba(0,0,0,0.88)
  }
  shape m.s7 = Line {
    start: p7
    strokeWidth: settings.lineThickness
    end: p8
    strokeColor: rgba(0,0,0,0.86)
  }
  shape m.s8 = Line {
    start: p8
    strokeWidth: settings.lineThickness
    end: p9
    strokeColor: rgba(0,0,0,0.84)
  }
  shape m.s9 = Line {
    start: p9
    strokeWidth: settings.lineThickness
    end: p10
    strokeColor: rgba(0,0,0,0.82)
  }
  shape m.s10 = Line {
    start: p10
    strokeWidth: settings.lineThickness
    end: p11
    strokeColor: rgba(0,0,0,0.80)
  }
  shape m.s11 = Line {
    start: p11
    strokeWidth: settings.lineThickness
    end: p12
    strokeColor: rgba(0,0,0,0.78)
  }
  shape m.s12 = Line {
    start: p12
    strokeWidth: settings.lineThickness
    end: p13
    strokeColor: rgba(0,0,0,0.76)
  }
  shape m.s13 = Line {
    start: p13
    strokeWidth: settings.lineThickness
    end: p14
    strokeColor: rgba(0,0,0,0.74)
  }
  shape m.s14 = Line {
    start: p14
    strokeWidth: settings.lineThickness
    end: p15
    strokeColor: rgba(0,0,0,0.72)
  }
  shape m.s15 = Line {
    start: p15
    strokeWidth: settings.lineThickness
    end: p16
    strokeColor: rgba(0,0,0,0.70)
  }
  shape m.s16 = Line {
    start: p16
    strokeWidth: settings.lineThickness
    end: p17
    strokeColor: rgba(0,0,0,0.68)
  }
  shape m.s17 = Line {
    start: p17
    strokeWidth: settings.lineThickness
    end: p18
    strokeColor: rgba(0,0,0,0.66)
  }
  shape m.s18 = Line {
    start: p18
    strokeWidth: settings.lineThickness
    end: p19
    strokeColor: rgba(0,0,0,0.64)
  }
  shape m.s19 = Line {
    start: p19
    strokeWidth: settings.lineThickness
    end: p20
    strokeColor: rgba(0,0,0,0.62)
  }
  shape m.s20 = Line {
    start: p20
    strokeWidth: settings.lineThickness
    end: p21
    strokeColor: rgba(0,0,0,0.60)
  }
  shape m.s21 = Line {
    start: p21
    strokeWidth: settings.lineThickness
    end: p22
    strokeColor: rgba(0,0,0,0.58)
  }
  shape m.s22 = Line {
    start: p22
    strokeWidth: settings.lineThickness
    end: p23
    strokeColor: rgba(0,0,0,0.56)
  }
  shape m.s23 = Line {
    start: p23
    strokeWidth: settings.lineThickness
    end: p24
    strokeColor: rgba(0,0,0,0.54)
  }
  shape m.s24 = Line {
    start: p24
    strokeWidth: settings.lineThickness
    end: p25
    strokeColor: rgba(0,0,0,0.52)
  }
  shape m.s25 = Line {
    start: p25
    strokeWidth: settings.lineThickness
    end: p26
    strokeColor: rgba(0,0,0,0.5)
  }
  shape m.s26 = Line {
    start: p26
    strokeWidth: settings.lineThickness
    end: p27
    strokeColor: rgba(0,0,0,0.48)
  }
  shape m.s27 = Line {
    start: p27
    strokeWidth: settings.lineThickness
    end: p28
    strokeColor: rgba(0,0,0,0.46)
  }
  shape m.s28 = Line {
    start: p28
    strokeWidth: settings.lineThickness
    end: p29
    strokeColor: rgba(0,0,0,0.44)
  }
  shape m.s29 = Line {
    start: p29
    strokeWidth: settings.lineThickness
    end: p30
    strokeColor: rgba(0,0,0,0.42)
  }
  shape m.s30 = Line {
    start: p30
    strokeWidth: settings.lineThickness
    end: p31
    strokeColor: rgba(0,0,0,0.4)
  }
  shape m.s31 = Line {
    start: p31
    strokeWidth: settings.lineThickness
    end: p32
    strokeColor: rgba(0,0,0,0.38)
  }
  shape m.s32 = Line {
    start: p32
    strokeWidth: settings.lineThickness
    end: p33
    strokeColor: rgba(0,0,0,0.36)
  }
  shape m.s33 = Line {
    start: p33
    strokeWidth: settings.lineThickness
    end: p34
    strokeColor: rgba(0,0,0,0.34)
  }
  shape m.s34 = Line {
    start: p34
    strokeWidth: settings.lineThickness
    end: p35
    strokeColor: rgba(0,0,0,0.32)
  }
  shape m.s35 = Line {
    start: p35
    strokeWidth: settings.lineThickness
    end: p36
    strokeColor: rgba(0,0,0,0.30)
  }
  shape m.s36 = Line {
    start: p36
    strokeWidth: settings.lineThickness
    end: p37
    strokeColor: rgba(0,0,0,0.28)
  }
  shape m.s37 = Line {
    start: p37
    strokeWidth: settings.lineThickness
    end: p38
    strokeColor: rgba(0,0,0,0.26)
  }
  shape m.s38 = Line {
    start: p38
    strokeWidth: settings.lineThickness
    end: p39
    strokeColor: rgba(0,0,0,0.24)
  }
  shape m.s39 = Line {
    start: p39
    strokeWidth: settings.lineThickness
    end: p40
    strokeColor: rgba(0,0,0,0.22)
  }
  shape m.s40 = Line {
    start: p40
    strokeWidth: settings.lineThickness
    end: p41
    strokeColor: rgba(0,0,0,0.20)
  }
  shape m.s41 = Line {
    start: p41
    strokeWidth: settings.lineThickness
    end: p42
    strokeColor: rgba(0,0,0,0.18)
  }
  shape m.s42 = Line {
    start: p42
    strokeWidth: settings.lineThickness
    end: p43
    strokeColor: rgba(0,0,0,0.16)
  }
  shape m.s43 = Line {
    start: p43
    strokeWidth: settings.lineThickness
    end: p44
    strokeColor: rgba(0,0,0,0.14)
  }
  shape m.s44 = Line {
    start: p44
    strokeWidth: settings.lineThickness
    end: p45
    strokeColor: rgba(0,0,0,0.12)
  }
  shape m.s45 = Line {
    start: p45
    strokeWidth: settings.lineThickness
    end: p46
    strokeColor: rgba(0,0,0,0.10)
  }
  shape m.s46 = Line {
    start: p46
    strokeWidth: settings.lineThickness
    end: p47
    strokeColor: rgba(0,0,0,0.08)
  }
  shape m.s47 = Line {
    start: p47
    strokeWidth: settings.lineThickness
    end: p48
    strokeColor: rgba(0,0,0,0.06)
  }
  shape m.s48 = Line {
    start: p48
    strokeWidth: settings.lineThickness
    end: p49
    strokeColor: rgba(0,0,0,0.04)
  }

  shape m.arrow = Line {
    start: p0 + 10 * normalize(p1 - p0)
    end: p1 + 15 * normalize(p1 - p0)
    strokeWidth: settings.arrowThickness
    strokeColor: #000
    endArrowhead: "straight"
  }

  pa = closestPoint(a.poly, p0)
  ensure vdist(pa, p0) == 15 in maps

  pb = closestPoint(b.poly, p49)
  ensure vdist(pb, p49) == 15 in maps
  ensure isEquilateral(m.poly.points, false)

  ensure elasticEnergy(m.poly.points, false) == 0 in maps
  ensure perimeter(m.poly.points, false) == 5 in manifolds

  ensure vdist(pa, p0) + vdist(p0, p1) == vdist(pa, p1)
  ensure vdist(pb, p49) + vdist(p48, p49) == vdist(pb, p48)

  shape m.eq = Equation {
    string: m.label
    fontSize: settings.fontSize
  }

  ensure vdist(m.eq.center, p24) == 20 in maps

  ca = centerOfMass(a.poly.points)
  cb = centerOfMass(b.poly.points)
  ensure vdist(m.eq.center, ca) == vdist(m.eq.center, cb) in maps

}
`,p=`type Manifold
type Map

constructor MakeMap(Manifold a, Manifold b) -> Map
`,t={substance:s,style:[{contents:e,resolver:i}],domain:p,variation:"LavendaireWombat86913",excludeWarnings:[]};export{t as default};
//# sourceMappingURL=blobs.trio-5f8b8907.js.map
