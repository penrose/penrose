import{m as n}from"./resolver-7c033537.js";const o=n("molecules"),t=`canvas {
    width = 300
    height = 300
}

const {
  scalar dotSize = 3
  scalar atomSize = 30
  scalar k = 1 -- spring stiffness
  scalar L = 10 -- rest length
  scalar symmetryWeight = 1
  scalar symmetryDegree = 3
  scalar bondAvoidWeight = 1
}

-- generated element list


forall Hydrogen x {
  x.symbol = "H"
  x.id = 1
}

forall Helium x {
  x.symbol = "He"
  x.id = 2
}

forall Lithium x {
  x.symbol = "Li"
  x.id = 3
}

forall Beryllium x {
  x.symbol = "Be"
  x.id = 4
}

forall Boron x {
  x.symbol = "B"
  x.id = 5
}

forall Carbon x {
  x.symbol = "C"
  x.id = 6
}

forall Nitrogen x {
  x.symbol = "N"
  x.id = 7
}

forall Oxygen x {
  x.symbol = "O"
  x.id = 8
}

forall Fluorine x {
  x.symbol = "F"
  x.id = 9
}

forall Neon x {
  x.symbol = "Ne"
  x.id = 10
}

forall Sodium x {
  x.symbol = "Na"
  x.id = 11
}

forall Magnesium x {
  x.symbol = "Mg"
  x.id = 12
}

forall Aluminium x {
  x.symbol = "Al"
  x.id = 13
}

forall Silicon x {
  x.symbol = "Si"
  x.id = 14
}

forall Phosphorus x {
  x.symbol = "P"
  x.id = 15
}

forall Sulfur x {
  x.symbol = "S"
  x.id = 16
}

forall Chlorine x {
  x.symbol = "Cl"
  x.id = 17
}

forall Argon x {
  x.symbol = "Ar"
  x.id = 18
}

forall Potassium x {
  x.symbol = "K"
  x.id = 19
}

forall Calcium x {
  x.symbol = "Ca"
  x.id = 20
}

forall Scandium x {
  x.symbol = "Sc"
  x.id = 21
}

forall Titanium x {
  x.symbol = "Ti"
  x.id = 22
}

forall Vanadium x {
  x.symbol = "V"
  x.id = 23
}

forall Chromium x {
  x.symbol = "Cr"
  x.id = 24
}

forall Manganese x {
  x.symbol = "Mn"
  x.id = 25
}

forall Iron x {
  x.symbol = "Fe"
  x.id = 26
}

forall Cobalt x {
  x.symbol = "Co"
  x.id = 27
}

forall Nickel x {
  x.symbol = "Ni"
  x.id = 28
}

forall Copper x {
  x.symbol = "Cu"
  x.id = 29
}

forall Zinc x {
  x.symbol = "Zn"
  x.id = 30
}

forall Gallium x {
  x.symbol = "Ga"
  x.id = 31
}

forall Germanium x {
  x.symbol = "Ge"
  x.id = 32
}

forall Arsenic x {
  x.symbol = "As"
  x.id = 33
}

forall Selenium x {
  x.symbol = "Se"
  x.id = 34
}

forall Bromine x {
  x.symbol = "Br"
  x.id = 35
}

forall Krypton x {
  x.symbol = "Kr"
  x.id = 36
}

forall Rubidium x {
  x.symbol = "Rb"
  x.id = 37
}

forall Strontium x {
  x.symbol = "Sr"
  x.id = 38
}

forall Yttrium x {
  x.symbol = "Y"
  x.id = 39
}

forall Zirconium x {
  x.symbol = "Zr"
  x.id = 40
}

forall Niobium x {
  x.symbol = "Nb"
  x.id = 41
}

forall Molybdenum x {
  x.symbol = "Mo"
  x.id = 42
}

forall Technetium x {
  x.symbol = "Tc"
  x.id = 43
}

forall Ruthenium x {
  x.symbol = "Ru"
  x.id = 44
}

forall Rhodium x {
  x.symbol = "Rh"
  x.id = 45
}

forall Palladium x {
  x.symbol = "Pd"
  x.id = 46
}

forall Silver x {
  x.symbol = "Ag"
  x.id = 47
}

forall Cadmium x {
  x.symbol = "Cd"
  x.id = 48
}

forall Indium x {
  x.symbol = "In"
  x.id = 49
}

forall Tin x {
  x.symbol = "Sn"
  x.id = 50
}

forall Antimony x {
  x.symbol = "Sb"
  x.id = 51
}

forall Tellurium x {
  x.symbol = "Te"
  x.id = 52
}

forall Iodine x {
  x.symbol = "I"
  x.id = 53
}

forall Xenon x {
  x.symbol = "Xe"
  x.id = 54
}

forall Cesium x {
  x.symbol = "Cs"
  x.id = 55
}

forall Barium x {
  x.symbol = "Ba"
  x.id = 56
}

forall Lanthanum x {
  x.symbol = "La"
  x.id = 57
}

forall Cerium x {
  x.symbol = "Ce"
  x.id = 58
}

forall Praseodymium x {
  x.symbol = "Pr"
  x.id = 59
}

forall Neodymium x {
  x.symbol = "Nd"
  x.id = 60
}

forall Promethium x {
  x.symbol = "Pm"
  x.id = 61
}

forall Samarium x {
  x.symbol = "Sm"
  x.id = 62
}

forall Europium x {
  x.symbol = "Eu"
  x.id = 63
}

forall Gadolinium x {
  x.symbol = "Gd"
  x.id = 64
}

forall Terbium x {
  x.symbol = "Tb"
  x.id = 65
}

forall Dysprosium x {
  x.symbol = "Dy"
  x.id = 66
}

forall Holmium x {
  x.symbol = "Ho"
  x.id = 67
}

forall Erbium x {
  x.symbol = "Er"
  x.id = 68
}

forall Thulium x {
  x.symbol = "Tm"
  x.id = 69
}

forall Ytterbium x {
  x.symbol = "Yb"
  x.id = 70
}

forall Lutetium x {
  x.symbol = "Lu"
  x.id = 71
}

forall Hafnium x {
  x.symbol = "Hf"
  x.id = 72
}

forall Tantalum x {
  x.symbol = "Ta"
  x.id = 73
}

forall Tungsten x {
  x.symbol = "W"
  x.id = 74
}

forall Rhenium x {
  x.symbol = "Re"
  x.id = 75
}

forall Osmium x {
  x.symbol = "Os"
  x.id = 76
}

forall Iridium x {
  x.symbol = "Ir"
  x.id = 77
}

forall Platinum x {
  x.symbol = "Pt"
  x.id = 78
}

forall Gold x {
  x.symbol = "Au"
  x.id = 79
}

forall Mercury x {
  x.symbol = "Hg"
  x.id = 80
}

forall Thallium x {
  x.symbol = "Tl"
  x.id = 81
}

forall Lead x {
  x.symbol = "Pb"
  x.id = 82
}

forall Bismuth x {
  x.symbol = "Bi"
  x.id = 83
}

forall Polonium x {
  x.symbol = "Po"
  x.id = 84
}

forall Astatine x {
  x.symbol = "At"
  x.id = 85
}

forall Radon x {
  x.symbol = "Rn"
  x.id = 86
}

forall Francium x {
  x.symbol = "Fr"
  x.id = 87
}

forall Radium x {
  x.symbol = "Ra"
  x.id = 88
}

forall Actinium x {
  x.symbol = "Ac"
  x.id = 89
}

forall Thorium x {
  x.symbol = "Th"
  x.id = 90
}

forall Protactinium x {
  x.symbol = "Pa"
  x.id = 91
}

forall Uranium x {
  x.symbol = "U"
  x.id = 92
}

forall Neptunium x {
  x.symbol = "Np"
  x.id = 93
}

forall Plutonium x {
  x.symbol = "Pu"
  x.id = 94
}

forall Americium x {
  x.symbol = "Am"
  x.id = 95
}

forall Curium x {
  x.symbol = "Cm"
  x.id = 96
}

forall Berkelium x {
  x.symbol = "Bk"
  x.id = 97
}

forall Californium x {
  x.symbol = "Cf"
  x.id = 98
}

forall Einsteinium x {
  x.symbol = "Es"
  x.id = 99
}

forall Fermium x {
  x.symbol = "Fm"
  x.id = 100
}

forall Mendelevium x {
  x.symbol = "Md"
  x.id = 101
}

forall Nobelium x {
  x.symbol = "No"
  x.id = 102
}

forall Lawrencium x {
  x.symbol = "Lr"
  x.id = 103
}

forall Rutherfordium x {
  x.symbol = "Rf"
  x.id = 104
}

forall Dubnium x {
  x.symbol = "Db"
  x.id = 105
}

forall Seaborgium x {
  x.symbol = "Sg"
  x.id = 106
}

forall Bohrium x {
  x.symbol = "Bh"
  x.id = 107
}

forall Hassium x {
  x.symbol = "Hs"
  x.id = 108
}

forall Meitnerium x {
  x.symbol = "Mt"
  x.id = 109
}

forall Darmstadtium x {
  x.symbol = "Ds"
  x.id = 110
}

forall Roentgenium x {
  x.symbol = "Rg"
  x.id = 111
}

forall Copernicium x {
  x.symbol = "Cn"
  x.id = 112
}



-- Atoms

forall Atom x {
    shape x.icon = Circle {
        strokeWidth : 0.0
        r : const.atomSize / 2
        fillColor : none()
        -- fillColor : #ff000060 -- DEBUG
        center: (? except electron, ? except electron)
    }
    -- shape x.icon = Rectangle {
    --     strokeWidth : 0.0
    --     width : const.atomSize 
    --     height : const.atomSize 
    --     fillColor : #fff
    --     -- fillColor : #ff000060
    --     center: (? except electron, ? except electron)
    -- }
    shape x.text = Text {
        string : x.symbol
        rotation : 0.0
        center : x.icon.center
        fillColor: #000
        fontSize: "20px"
    }
    x.layering = x.text above x.icon
}



-- Bonds

forall Bond b {
  shape b.icon = Circle { fillColor: #0000 }
}

forall Bond b
where b := MakeSingleBond(x, y)
with Atom x; Atom y {
    vec2 b.vec = y.icon.center - x.icon.center
    vec2 b.dir = normalize(b.vec)
    vec2 paddingVec = b.dir * const.atomSize/2
    override b.icon = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 2.0
    }
    encourage equal(vdist(x.icon.center, y.icon.center), 60.0)
    b.icon below x.icon, y.icon
}

forall Bond b
where b := MakeDoubleBond(x, y)
with Atom x; Atom y {
    vec2 b.vec = y.icon.center - x.icon.center
    vec2 b.dir = normalize(b.vec)
    vec2 paddingVec = b.dir * const.atomSize/2
    override b.icon = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 6.0
    }
    shape b.line2 = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth: 2.0
    }
    b.line2 above b.icon, x.icon, y.icon
    ensure equal(vdist(b.icon.start, b.icon.end), 60.0)
}

forall Bond b
where b := MakeTripleBond(x, y)
with Atom x; Atom y {
    vec2 b.vec = y.icon.center - x.icon.center
    vec2 b.dir = normalize(b.vec)
    vec2 paddingVec = b.dir * const.atomSize/2
    override b.icon = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : #000
        strokeWidth: 10.0
    }
    shape b.line2 = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : #fff
        strokeWidth: 6.0
    }
    shape b.line3 = Line {
        start : x.icon.center + paddingVec
        end : y.icon.center - paddingVec
        strokeColor : #000
        strokeWidth: 2.0
    }
    b.line3 above b.line2
    b.line2 above b.icon
    b.icon below x.icon, y.icon
    ensure equal(vdist(b.icon.start, b.icon.end), 60.0)
}

-- Repulsion and disjoint-ness

forall Atom x; Atom y {
    encourage notTooClose(x.icon, y.icon)
    ensure disjoint(x.icon, y.icon) 
}

forall Bond x; Bond y {
    ensure disjoint(x.icon, y.icon) 
}

-- valance

forall Atom a 
where TwoValenceElectrons(a) as e {
  e.icon1 = Group {
    shapes: [e.circle11, e.circle12]
  }
  e.center1 = (?, ?)
  centerVec1 = normalize(a.icon.center - e.center1)
  e.circle11 = Circle {
    center: e.center1 + rot90(centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle12 = Circle {
    center: e.center1 + rot90(-centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug1 = Circle {
  --   center: e.center1
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle11) in electron
  ensure contains(a.icon, e.circle12) in electron
  ensure norm(e.center1 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon1
}

forall Atom a 
where FourValenceElectrons(a) as e {
  e.icon1 = Group {
    shapes: [e.circle11, e.circle12]
  }
  e.center1 = (?, ?)
  centerVec1 = normalize(a.icon.center - e.center1)
  e.circle11 = Circle {
    center: e.center1 + rot90(centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle12 = Circle {
    center: e.center1 + rot90(-centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug1 = Circle {
  --   center: e.center1
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle11) in electron
  ensure contains(a.icon, e.circle12) in electron
  ensure norm(e.center1 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon1

  e.icon2 = Group {
    shapes: [e.circle21, e.circle22]
  }
  e.center2 = (?, ?)
  centerVec2 = normalize(a.icon.center - e.center2)
  e.circle21 = Circle {
    center: e.center2 + rot90(centerVec2) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle22 = Circle {
    center: e.center2 + rot90(-centerVec2) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug2 = Circle {
  --   center: e.center2
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle21) in electron
  ensure contains(a.icon, e.circle22) in electron
  ensure norm(e.center2 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon2

  encourage notTooClose(e.icon1, e.icon2) in electron
}

forall Atom a 
where SixValenceElectrons(a) as e {
  e.icon1 = Group {
    shapes: [e.circle11, e.circle12]
  }
  e.center1 = (?, ?)
  centerVec1 = normalize(a.icon.center - e.center1)
  e.circle11 = Circle {
    center: e.center1 + rot90(centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle12 = Circle {
    center: e.center1 + rot90(-centerVec1) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug1 = Circle {
  --   center: e.center1
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle11) in electron
  ensure contains(a.icon, e.circle12) in electron
  ensure norm(e.center1 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon1

  e.icon2 = Group {
    shapes: [e.circle21, e.circle22]
  }
  e.center2 = (?, ?)
  centerVec2 = normalize(a.icon.center - e.center2)
  e.circle21 = Circle {
    center: e.center2 + rot90(centerVec2) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle22 = Circle {
    center: e.center2 + rot90(-centerVec2) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug2 = Circle {
  --   center: e.center2
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle21) in electron
  ensure contains(a.icon, e.circle22) in electron
  ensure norm(e.center2 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon2

  e.icon3 = Group {
    shapes: [e.circle31, e.circle32]
  }
  e.center3 = (?, ?)
  centerVec3 = normalize(a.icon.center - e.center3)
  e.circle31 = Circle {
    center: e.center3 + rot90(centerVec3) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  e.circle32 = Circle {
    center: e.center3 + rot90(-centerVec3) * 4
    r: const.dotSize / 2
    strokeWidth: 0
    fillColor: #000
  }
  -- debug3 = Circle {
  --   center: e.center2
  --   fillColor: #f00
  --   r: const.dotSize / 2
  -- }
  ensure contains(a.icon, e.circle31) in electron
  ensure contains(a.icon, e.circle32) in electron
  ensure norm(e.center3 - a.text.center) == const.atomSize/2 in electron
  a.icon below e.icon3

  encourage notTooClose(e.icon1, e.icon2) in electron
  encourage notTooClose(e.icon2, e.icon3) in electron
  encourage notTooClose(e.icon3, e.icon1) in electron
}


-- single bond
forall Bond b
where TwoValenceElectrons(x) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }
}
forall Bond b
where TwoValenceElectrons(y) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourValenceElectrons(x) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug2 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x21
  -- }
}
forall Bond b
where FourValenceElectrons(y) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where SixValenceElectrons(x) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug2 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x21
  -- }

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
  -- debug3 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x31
  -- }
}
forall Bond b
where SixValenceElectrons(y) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}


-- double bond
forall Bond b
where TwoValenceElectrons(x) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where TwoValenceElectrons(y) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourValenceElectrons(x) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where FourValenceElectrons(y) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where SixValenceElectrons(x) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where SixValenceElectrons(y) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}


-- triple bond
forall Bond b
where TwoValenceElectrons(x) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where TwoValenceElectrons(y) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourValenceElectrons(x) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where FourValenceElectrons(y) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where SixValenceElectrons(x) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where SixValenceElectrons(y) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}




layout = [general, symmetry, electron]

-- layout directives
forall Bond b
where b := MakeSingleBond(x, y) 
with Atom x, y {
  scalar weight = const.symmetryWeight
  scalar k = const.symmetryDegree
  vec2 u = b.dir
  scalar theta = atan2( u[0], u[1] )
  encourage sin( MathPI() + k*theta ) * weight == -1 * weight in symmetry
}

forall Bond b
where b := MakeDoubleBond(x, y) 
with Atom x, y {
  scalar weight = const.symmetryWeight
  scalar k = const.symmetryDegree
  vec2 u = b.icon.end - b.icon.start
  scalar theta = atan2( u[0], u[1] )
  encourage sin( MathPI() + k*theta ) * weight == -1 * weight in symmetry
}

forall Bond b
where b := MakeTripleBond(x, y) 
with Atom x, y {
  scalar weight = const.symmetryWeight
  scalar k = const.symmetryDegree
  vec2 u = b.dir
  scalar theta = atan2( u[0], u[1] )
  encourage sin( MathPI() + k*theta ) * weight == -1 * weight in symmetry
}

-- forall Atom a, b, c
-- where Collinear(a, b, c) {
--   ensure collinear(a.icon.center, b.icon.center, c.icon.center)
-- }
-- forall Atom a, b
-- where VerticalAlign(a, b) {
--   ensure a.icon.center[0] == b.icon.center[0]
-- }
-- forall Atom a, b
-- where HorizontalAlign(a, b) {
--   ensure a.icon.center[1] == b.icon.center[1]
-- }
`;export{o as r,t as s};
//# sourceMappingURL=lewis.style-f8728b58.js.map
