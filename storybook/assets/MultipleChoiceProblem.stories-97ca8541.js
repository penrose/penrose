var Si=Object.defineProperty;var u=(L,H)=>Si(L,"name",{value:H,configurable:!0});import{s as ke,F as zi}from"./styled-components.browser.esm-f529d418.js";import{r as we,j as X0,a as An}from"./jsx-runtime-4654fda8.js";import{S as Mi}from"./Simple-33d74ae5.js";import"./index-5b359ca1.js";import"./hoist-non-react-statics.cjs-7d741b1d.js";import"./iframe-57848583.js";import"./svg-11aedce1.js";const Ye=`Hydrogen h
Carbon c
Nitrogen n
Bond b1 := MakeSingleBond(c, h)
Bond b2 := MakeTripleBond(c, n)
ZeroDots(h)
ZeroDots(c)
TwoDots(n)

AutoLabel All`,Xe=`canvas {
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
where TwoDots(a) as e {
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
where FourDots(a) as e {
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
where SixDots(a) as e {
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
where TwoDots(x) as e; b := MakeSingleBond(x, y)
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
where TwoDots(y) as e; b := MakeSingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourDots(x) as e; b := MakeSingleBond(x, y)
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
where FourDots(y) as e; b := MakeSingleBond(x, y)
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
where SixDots(x) as e; b := MakeSingleBond(x, y)
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
where SixDots(y) as e; b := MakeSingleBond(x, y)
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
where TwoDots(x) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where TwoDots(y) as e; b := MakeDoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourDots(x) as e; b := MakeDoubleBond(x, y)
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
where FourDots(y) as e; b := MakeDoubleBond(x, y)
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
where SixDots(x) as e; b := MakeDoubleBond(x, y)
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
where SixDots(y) as e; b := MakeDoubleBond(x, y)
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
where TwoDots(x) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}
forall Bond b
where TwoDots(y) as e; b := MakeTripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize ½ k(d-L)²
}

forall Bond b
where FourDots(x) as e; b := MakeTripleBond(x, y)
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
where FourDots(y) as e; b := MakeTripleBond(x, y)
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
where SixDots(x) as e; b := MakeTripleBond(x, y)
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
where SixDots(y) as e; b := MakeTripleBond(x, y)
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
`,$e=`-- Atoms

type Atom

type Hydrogen <: Atom
type Helium <: Atom
type Lithium <: Atom
type Beryllium <: Atom
type Boron <: Atom
type Carbon <: Atom
type Nitrogen <: Atom
type Oxygen <: Atom
type Fluorine <: Atom
type Neon <: Atom
type Sodium <: Atom
type Magnesium <: Atom
type Aluminium <: Atom
type Silicon <: Atom
type Phosphorus <: Atom
type Sulfur <: Atom
type Chlorine <: Atom
type Argon <: Atom
type Potassium <: Atom
type Calcium <: Atom
type Scandium <: Atom
type Titanium <: Atom
type Vanadium <: Atom
type Chromium <: Atom
type Manganese <: Atom
type Iron <: Atom
type Cobalt <: Atom
type Nickel <: Atom
type Copper <: Atom
type Zinc <: Atom
type Gallium <: Atom
type Germanium <: Atom
type Arsenic <: Atom
type Selenium <: Atom
type Bromine <: Atom
type Krypton <: Atom
type Rubidium <: Atom
type Strontium <: Atom
type Yttrium <: Atom
type Zirconium <: Atom
type Niobium <: Atom
type Molybdenum <: Atom
type Technetium <: Atom
type Ruthenium <: Atom
type Rhodium <: Atom
type Palladium <: Atom
type Silver <: Atom
type Cadmium <: Atom
type Indium <: Atom
type Tin <: Atom
type Antimony <: Atom
type Tellurium <: Atom
type Iodine <: Atom
type Xenon <: Atom
type Cesium <: Atom
type Barium <: Atom
type Lanthanum <: Atom
type Cerium <: Atom
type Praseodymium <: Atom
type Neodymium <: Atom
type Promethium <: Atom
type Samarium <: Atom
type Europium <: Atom
type Gadolinium <: Atom
type Terbium <: Atom
type Dysprosium <: Atom
type Holmium <: Atom
type Erbium <: Atom
type Thulium <: Atom
type Ytterbium <: Atom
type Lutetium <: Atom
type Hafnium <: Atom
type Tantalum <: Atom
type Tungsten <: Atom
type Rhenium <: Atom
type Osmium <: Atom
type Iridium <: Atom
type Platinum <: Atom
type Gold <: Atom
type Mercury <: Atom
type Thallium <: Atom
type Lead <: Atom
type Bismuth <: Atom
type Polonium <: Atom
type Astatine <: Atom
type Radon <: Atom
type Francium <: Atom
type Radium <: Atom
type Actinium <: Atom
type Thorium <: Atom
type Protactinium <: Atom
type Uranium <: Atom
type Neptunium <: Atom
type Plutonium <: Atom
type Americium <: Atom
type Curium <: Atom
type Berkelium <: Atom
type Californium <: Atom
type Einsteinium <: Atom
type Fermium <: Atom
type Mendelevium <: Atom
type Nobelium <: Atom
type Lawrencium <: Atom
type Rutherfordium <: Atom
type Dubnium <: Atom
type Seaborgium <: Atom
type Bohrium <: Atom
type Hassium <: Atom
type Meitnerium <: Atom
type Darmstadtium <: Atom
type Roentgenium <: Atom
type Copernicium <: Atom

-- Bonds

type Bond

constructor MakeSingleBond(Atom a, Atom b) -> Bond
constructor MakeDoubleBond(Atom a, Atom b) -> Bond
constructor MakeTripleBond(Atom a, Atom b) -> Bond

-- Electrons 

-- these correspond to dots in a Lewis structure
predicate ZeroDots(Atom)
predicate TwoDots(Atom)
predicate FourDots(Atom)
predicate SixDots(Atom)

-- layout
-- predicate Collinear(Atom, Atom, Atom)
-- predicate VerticalAlign(Atom, Atom)
-- predicate HorizontalAlign(Atom, Atom)
`;function Bi(L,H){H===void 0&&(H={});var V=H.insertAt;if(!(!L||typeof document>"u")){var W=document.head||document.getElementsByTagName("head")[0],P=document.createElement("style");P.type="text/css",V==="top"&&W.firstChild?W.insertBefore(P,W.firstChild):W.appendChild(P),P.styleSheet?P.styleSheet.cssText=L:P.appendChild(document.createTextNode(L))}}u(Bi,"__$styleInject");/*! *****************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.
Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at http://www.apache.org/licenses/LICENSE-2.0

THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
MERCHANTABLITY OR NON-INFRINGEMENT.

See the Apache Version 2.0 License for specific language governing permissions
and limitations under the License.
***************************************************************************** */var Gt=u(function(L,H){return Gt=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(V,W){V.__proto__=W}||function(V,W){for(var P in W)W.hasOwnProperty(P)&&(V[P]=W[P])},Gt(L,H)},"extendStatics");function Ci(L,H){Gt(L,H);function V(){this.constructor=L}u(V,"__"),L.prototype=H===null?Object.create(H):(V.prototype=H.prototype,new V)}u(Ci,"__extends");var Ti=typeof window<"u"?window:typeof global<"u"?global:typeof self<"u"?self:{};function Di(L,H){return H={exports:{}},L(H,H.exports),H.exports}u(Di,"createCommonjsModule");var Sn=Di(function(L,H){u(function(W,P){L.exports=P()},"webpackUniversalModuleDefinition")(typeof self<"u"?self:Ti,function(){return function(){var V={};(function(){V.d=function(l,e){for(var t in e)V.o(e,t)&&!V.o(l,t)&&Object.defineProperty(l,t,{enumerable:!0,get:e[t]})}})(),function(){V.o=function(l,e){return Object.prototype.hasOwnProperty.call(l,e)}}();var W={};V.d(W,{default:function(){return xi}});var P=u(function l(e,t){this.position=void 0;var r="KaTeX parse error: "+e,n,a=t&&t.loc;if(a&&a.start<=a.end){var o=a.lexer.input;n=a.start;var m=a.end;n===o.length?r+=" at end of input: ":r+=" at position "+(n+1)+": ";var h=o.slice(n,m).replace(/[^]/g,"$&̲"),f;n>15?f="…"+o.slice(n-15,n):f=o.slice(0,n);var g;m+15<o.length?g=o.slice(m,m+15)+"…":g=o.slice(m),r+=f+h+g}var y=new Error(r);return y.name="ParseError",y.__proto__=l.prototype,y.position=n,y},"ParseError");P.prototype.__proto__=Error.prototype;var k=P,J=u(function(e,t){return e.indexOf(t)!==-1},"contains"),w0=u(function(e,t){return e===void 0?t:e},"deflt"),k0=/([A-Z])/g,$0=u(function(e){return e.replace(k0,"-$1").toLowerCase()},"hyphenate"),j0={"&":"&amp;",">":"&gt;","<":"&lt;",'"':"&quot;","'":"&#x27;"},te=/[&><"']/g;function ce(l){return String(l).replace(te,function(e){return j0[e]})}u(ce,"utils_escape");var Z0=u(function l(e){return e.type==="ordgroup"||e.type==="color"?e.body.length===1?l(e.body[0]):e:e.type==="font"?l(e.body):e},"getBaseElem"),Mn=u(function(e){var t=Z0(e);return t.type==="mathord"||t.type==="textord"||t.type==="atom"},"isCharacterBox"),Bn=u(function(e){if(!e)throw new Error("Expected non-null, but got "+String(e));return e},"assert"),Cn=u(function(e){var t=/^\s*([^\\/#]*?)(?::|&#0*58|&#x0*3a)/i.exec(e);return t!=null?t[1]:"_relative"},"protocolFromUrl"),E={contains:J,deflt:w0,escape:ce,hyphenate:$0,getBaseElem:Z0,isCharacterBox:Mn,protocolFromUrl:Cn},je=function(){function l(t){this.displayMode=void 0,this.output=void 0,this.leqno=void 0,this.fleqn=void 0,this.throwOnError=void 0,this.errorColor=void 0,this.macros=void 0,this.minRuleThickness=void 0,this.colorIsTextColor=void 0,this.strict=void 0,this.trust=void 0,this.maxSize=void 0,this.maxExpand=void 0,this.globalGroup=void 0,t=t||{},this.displayMode=E.deflt(t.displayMode,!1),this.output=E.deflt(t.output,"htmlAndMathml"),this.leqno=E.deflt(t.leqno,!1),this.fleqn=E.deflt(t.fleqn,!1),this.throwOnError=E.deflt(t.throwOnError,!0),this.errorColor=E.deflt(t.errorColor,"#cc0000"),this.macros=t.macros||{},this.minRuleThickness=Math.max(0,E.deflt(t.minRuleThickness,0)),this.colorIsTextColor=E.deflt(t.colorIsTextColor,!1),this.strict=E.deflt(t.strict,"warn"),this.trust=E.deflt(t.trust,!1),this.maxSize=Math.max(0,E.deflt(t.maxSize,1/0)),this.maxExpand=Math.max(0,E.deflt(t.maxExpand,1e3)),this.globalGroup=E.deflt(t.globalGroup,!1)}u(l,"Settings");var e=l.prototype;return e.reportNonstrict=u(function(r,n,a){var o=this.strict;if(typeof o=="function"&&(o=o(r,n,a)),!(!o||o==="ignore")){if(o===!0||o==="error")throw new k("LaTeX-incompatible input and strict mode is set to 'error': "+(n+" ["+r+"]"),a);o==="warn"?typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to 'warn': "+(n+" ["+r+"]")):typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to "+("unrecognized '"+o+"': "+n+" ["+r+"]"))}},"reportNonstrict"),e.useStrictBehavior=u(function(r,n,a){var o=this.strict;if(typeof o=="function")try{o=o(r,n,a)}catch{o="error"}return!o||o==="ignore"?!1:o===!0||o==="error"?!0:o==="warn"?(typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to 'warn': "+(n+" ["+r+"]")),!1):(typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to "+("unrecognized '"+o+"': "+n+" ["+r+"]")),!1)},"useStrictBehavior"),e.isTrusted=u(function(r){r.url&&!r.protocol&&(r.protocol=E.protocolFromUrl(r.url));var n=typeof this.trust=="function"?this.trust(r):this.trust;return!!n},"isTrusted"),l}(),I0=function(){function l(t,r,n){this.id=void 0,this.size=void 0,this.cramped=void 0,this.id=t,this.size=r,this.cramped=n}u(l,"Style");var e=l.prototype;return e.sup=u(function(){return M0[Tn[this.id]]},"sup"),e.sub=u(function(){return M0[Dn[this.id]]},"sub"),e.fracNum=u(function(){return M0[Nn[this.id]]},"fracNum"),e.fracDen=u(function(){return M0[En[this.id]]},"fracDen"),e.cramp=u(function(){return M0[Ln[this.id]]},"cramp"),e.text=u(function(){return M0[qn[this.id]]},"text"),e.isTight=u(function(){return this.size>=2},"isTight"),l}(),Ze=0,Ae=1,re=2,L0=3,he=4,g0=5,ne=6,s0=7,M0=[new I0(Ze,0,!1),new I0(Ae,0,!0),new I0(re,1,!1),new I0(L0,1,!0),new I0(he,2,!1),new I0(g0,2,!0),new I0(ne,3,!1),new I0(s0,3,!0)],Tn=[he,g0,he,g0,ne,s0,ne,s0],Dn=[g0,g0,g0,g0,s0,s0,s0,s0],Nn=[re,L0,he,g0,ne,s0,ne,s0],En=[L0,L0,g0,g0,s0,s0,s0,s0],Ln=[Ae,Ae,L0,L0,g0,g0,s0,s0],qn=[Ze,Ae,re,L0,re,L0,re,L0],R={DISPLAY:M0[Ze],TEXT:M0[re],SCRIPT:M0[he],SCRIPTSCRIPT:M0[ne]},Ke=[{name:"latin",blocks:[[256,591],[768,879]]},{name:"cyrillic",blocks:[[1024,1279]]},{name:"armenian",blocks:[[1328,1423]]},{name:"brahmic",blocks:[[2304,4255]]},{name:"georgian",blocks:[[4256,4351]]},{name:"cjk",blocks:[[12288,12543],[19968,40879],[65280,65376]]},{name:"hangul",blocks:[[44032,55215]]}];function Fn(l){for(var e=0;e<Ke.length;e++)for(var t=Ke[e],r=0;r<t.blocks.length;r++){var n=t.blocks[r];if(l>=n[0]&&l<=n[1])return t.name}return null}u(Fn,"scriptFromCodepoint");var de=[];Ke.forEach(function(l){return l.blocks.forEach(function(e){return de.push.apply(de,e)})});function Vt(l){for(var e=0;e<de.length;e+=2)if(l>=de[e]&&l<=de[e+1])return!0;return!1}u(Vt,"supportedCodepoint");var ae=80,Rn=u(function(e,t){return"M95,"+(622+e+t)+`
c-2.7,0,-7.17,-2.7,-13.5,-8c-5.8,-5.3,-9.5,-10,-9.5,-14
c0,-2,0.3,-3.3,1,-4c1.3,-2.7,23.83,-20.7,67.5,-54
c44.2,-33.3,65.8,-50.3,66.5,-51c1.3,-1.3,3,-2,5,-2c4.7,0,8.7,3.3,12,10
s173,378,173,378c0.7,0,35.3,-71,104,-213c68.7,-142,137.5,-285,206.5,-429
c69,-144,104.5,-217.7,106.5,-221
l`+e/2.075+" -"+e+`
c5.3,-9.3,12,-14,20,-14
H400000v`+(40+e)+`H845.2724
s-225.272,467,-225.272,467s-235,486,-235,486c-2.7,4.7,-9,7,-19,7
c-6,0,-10,-1,-12,-3s-194,-422,-194,-422s-65,47,-65,47z
M`+(834+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},"sqrtMain"),On=u(function(e,t){return"M263,"+(601+e+t)+`c0.7,0,18,39.7,52,119
c34,79.3,68.167,158.7,102.5,238c34.3,79.3,51.8,119.3,52.5,120
c340,-704.7,510.7,-1060.3,512,-1067
l`+e/2.084+" -"+e+`
c4.7,-7.3,11,-11,19,-11
H40000v`+(40+e)+`H1012.3
s-271.3,567,-271.3,567c-38.7,80.7,-84,175,-136,283c-52,108,-89.167,185.3,-111.5,232
c-22.3,46.7,-33.8,70.3,-34.5,71c-4.7,4.7,-12.3,7,-23,7s-12,-1,-12,-1
s-109,-253,-109,-253c-72.7,-168,-109.3,-252,-110,-252c-10.7,8,-22,16.7,-34,26
c-22,17.3,-33.3,26,-34,26s-26,-26,-26,-26s76,-59,76,-59s76,-60,76,-60z
M`+(1001+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},"sqrtSize1"),In=u(function(e,t){return"M983 "+(10+e+t)+`
l`+e/3.13+" -"+e+`
c4,-6.7,10,-10,18,-10 H400000v`+(40+e)+`
H1013.1s-83.4,268,-264.1,840c-180.7,572,-277,876.3,-289,913c-4.7,4.7,-12.7,7,-24,7
s-12,0,-12,0c-1.3,-3.3,-3.7,-11.7,-7,-25c-35.3,-125.3,-106.7,-373.3,-214,-744
c-10,12,-21,25,-33,39s-32,39,-32,39c-6,-5.3,-15,-14,-27,-26s25,-30,25,-30
c26.7,-32.7,52,-63,76,-91s52,-60,52,-60s208,722,208,722
c56,-175.3,126.3,-397.3,211,-666c84.7,-268.7,153.8,-488.2,207.5,-658.5
c53.7,-170.3,84.5,-266.8,92.5,-289.5z
M`+(1001+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},"sqrtSize2"),Hn=u(function(e,t){return"M424,"+(2398+e+t)+`
c-1.3,-0.7,-38.5,-172,-111.5,-514c-73,-342,-109.8,-513.3,-110.5,-514
c0,-2,-10.7,14.3,-32,49c-4.7,7.3,-9.8,15.7,-15.5,25c-5.7,9.3,-9.8,16,-12.5,20
s-5,7,-5,7c-4,-3.3,-8.3,-7.7,-13,-13s-13,-13,-13,-13s76,-122,76,-122s77,-121,77,-121
s209,968,209,968c0,-2,84.7,-361.7,254,-1079c169.3,-717.3,254.7,-1077.7,256,-1081
l`+e/4.223+" -"+e+`c4,-6.7,10,-10,18,-10 H400000
v`+(40+e)+`H1014.6
s-87.3,378.7,-272.6,1166c-185.3,787.3,-279.3,1182.3,-282,1185
c-2,6,-10,9,-24,9
c-8,0,-12,-0.7,-12,-2z M`+(1001+e)+" "+t+`
h400000v`+(40+e)+"h-400000z"},"sqrtSize3"),Pn=u(function(e,t){return"M473,"+(2713+e+t)+`
c339.3,-1799.3,509.3,-2700,510,-2702 l`+e/5.298+" -"+e+`
c3.3,-7.3,9.3,-11,18,-11 H400000v`+(40+e)+`H1017.7
s-90.5,478,-276.2,1466c-185.7,988,-279.5,1483,-281.5,1485c-2,6,-10,9,-24,9
c-8,0,-12,-0.7,-12,-2c0,-1.3,-5.3,-32,-16,-92c-50.7,-293.3,-119.7,-693.3,-207,-1200
c0,-1.3,-5.3,8.7,-16,30c-10.7,21.3,-21.3,42.7,-32,64s-16,33,-16,33s-26,-26,-26,-26
s76,-153,76,-153s77,-151,77,-151c0.7,0.7,35.7,202,105,604c67.3,400.7,102,602.7,104,
606zM`+(1001+e)+" "+t+"h400000v"+(40+e)+"H1017.7z"},"sqrtSize4"),Gn=u(function(e){var t=e/2;return"M400000 "+e+" H0 L"+t+" 0 l65 45 L145 "+(e-80)+" H400000z"},"phasePath"),Vn=u(function(e,t,r){var n=r-54-t-e;return"M702 "+(e+t)+"H400000"+(40+e)+`
H742v`+n+`l-4 4-4 4c-.667.7 -2 1.5-4 2.5s-4.167 1.833-6.5 2.5-5.5 1-9.5 1
h-12l-28-84c-16.667-52-96.667 -294.333-240-727l-212 -643 -85 170
c-4-3.333-8.333-7.667-13 -13l-13-13l77-155 77-156c66 199.333 139 419.667
219 661 l218 661zM702 `+t+"H400000v"+(40+e)+"H742z"},"sqrtTall"),Wn=u(function(e,t,r){t=1e3*t;var n="";switch(e){case"sqrtMain":n=Rn(t,ae);break;case"sqrtSize1":n=On(t,ae);break;case"sqrtSize2":n=In(t,ae);break;case"sqrtSize3":n=Hn(t,ae);break;case"sqrtSize4":n=Pn(t,ae);break;case"sqrtTall":n=Vn(t,ae,r)}return n},"sqrtPath"),Un=u(function(e,t){switch(e){case"⎜":return"M291 0 H417 V"+t+" H291z M291 0 H417 V"+t+" H291z";case"∣":return"M145 0 H188 V"+t+" H145z M145 0 H188 V"+t+" H145z";case"∥":return"M145 0 H188 V"+t+" H145z M145 0 H188 V"+t+" H145z"+("M367 0 H410 V"+t+" H367z M367 0 H410 V"+t+" H367z");case"⎟":return"M457 0 H583 V"+t+" H457z M457 0 H583 V"+t+" H457z";case"⎢":return"M319 0 H403 V"+t+" H319z M319 0 H403 V"+t+" H319z";case"⎥":return"M263 0 H347 V"+t+" H263z M263 0 H347 V"+t+" H263z";case"⎪":return"M384 0 H504 V"+t+" H384z M384 0 H504 V"+t+" H384z";case"⏐":return"M312 0 H355 V"+t+" H312z M312 0 H355 V"+t+" H312z";case"‖":return"M257 0 H300 V"+t+" H257z M257 0 H300 V"+t+" H257z"+("M478 0 H521 V"+t+" H478z M478 0 H521 V"+t+" H478z");default:return""}},"innerPath"),Wt={doubleleftarrow:`M262 157
l10-10c34-36 62.7-77 86-123 3.3-8 5-13.3 5-16 0-5.3-6.7-8-20-8-7.3
 0-12.2.5-14.5 1.5-2.3 1-4.8 4.5-7.5 10.5-49.3 97.3-121.7 169.3-217 216-28
 14-57.3 25-88 33-6.7 2-11 3.8-13 5.5-2 1.7-3 4.2-3 7.5s1 5.8 3 7.5
c2 1.7 6.3 3.5 13 5.5 68 17.3 128.2 47.8 180.5 91.5 52.3 43.7 93.8 96.2 124.5
 157.5 9.3 8 15.3 12.3 18 13h6c12-.7 18-4 18-10 0-2-1.7-7-5-15-23.3-46-52-87
-86-123l-10-10h399738v-40H218c328 0 0 0 0 0l-10-8c-26.7-20-65.7-43-117-69 2.7
-2 6-3.7 10-5 36.7-16 72.3-37.3 107-64l10-8h399782v-40z
m8 0v40h399730v-40zm0 194v40h399730v-40z`,doublerightarrow:`M399738 392l
-10 10c-34 36-62.7 77-86 123-3.3 8-5 13.3-5 16 0 5.3 6.7 8 20 8 7.3 0 12.2-.5
 14.5-1.5 2.3-1 4.8-4.5 7.5-10.5 49.3-97.3 121.7-169.3 217-216 28-14 57.3-25 88
-33 6.7-2 11-3.8 13-5.5 2-1.7 3-4.2 3-7.5s-1-5.8-3-7.5c-2-1.7-6.3-3.5-13-5.5-68
-17.3-128.2-47.8-180.5-91.5-52.3-43.7-93.8-96.2-124.5-157.5-9.3-8-15.3-12.3-18
-13h-6c-12 .7-18 4-18 10 0 2 1.7 7 5 15 23.3 46 52 87 86 123l10 10H0v40h399782
c-328 0 0 0 0 0l10 8c26.7 20 65.7 43 117 69-2.7 2-6 3.7-10 5-36.7 16-72.3 37.3
-107 64l-10 8H0v40zM0 157v40h399730v-40zm0 194v40h399730v-40z`,leftarrow:`M400000 241H110l3-3c68.7-52.7 113.7-120
 135-202 4-14.7 6-23 6-25 0-7.3-7-11-21-11-8 0-13.2.8-15.5 2.5-2.3 1.7-4.2 5.8
-5.5 12.5-1.3 4.7-2.7 10.3-4 17-12 48.7-34.8 92-68.5 130S65.3 228.3 18 247
c-10 4-16 7.7-18 11 0 8.7 6 14.3 18 17 47.3 18.7 87.8 47 121.5 85S196 441.3 208
 490c.7 2 1.3 5 2 9s1.2 6.7 1.5 8c.3 1.3 1 3.3 2 6s2.2 4.5 3.5 5.5c1.3 1 3.3
 1.8 6 2.5s6 1 10 1c14 0 21-3.7 21-11 0-2-2-10.3-6-25-20-79.3-65-146.7-135-202
 l-3-3h399890zM100 241v40h399900v-40z`,leftbrace:`M6 548l-6-6v-35l6-11c56-104 135.3-181.3 238-232 57.3-28.7 117
-45 179-50h399577v120H403c-43.3 7-81 15-113 26-100.7 33-179.7 91-237 174-2.7
 5-6 9-10 13-.7 1-7.3 1-20 1H6z`,leftbraceunder:`M0 6l6-6h17c12.688 0 19.313.3 20 1 4 4 7.313 8.3 10 13
 35.313 51.3 80.813 93.8 136.5 127.5 55.688 33.7 117.188 55.8 184.5 66.5.688
 0 2 .3 4 1 18.688 2.7 76 4.3 172 5h399450v120H429l-6-1c-124.688-8-235-61.7
-331-161C60.687 138.7 32.312 99.3 7 54L0 41V6z`,leftgroup:`M400000 80
H435C64 80 168.3 229.4 21 260c-5.9 1.2-18 0-18 0-2 0-3-1-3-3v-38C76 61 257 0
 435 0h399565z`,leftgroupunder:`M400000 262
H435C64 262 168.3 112.6 21 82c-5.9-1.2-18 0-18 0-2 0-3 1-3 3v38c76 158 257 219
 435 219h399565z`,leftharpoon:`M0 267c.7 5.3 3 10 7 14h399993v-40H93c3.3
-3.3 10.2-9.5 20.5-18.5s17.8-15.8 22.5-20.5c50.7-52 88-110.3 112-175 4-11.3 5
-18.3 3-21-1.3-4-7.3-6-18-6-8 0-13 .7-15 2s-4.7 6.7-8 16c-42 98.7-107.3 174.7
-196 228-6.7 4.7-10.7 8-12 10-1.3 2-2 5.7-2 11zm100-26v40h399900v-40z`,leftharpoonplus:`M0 267c.7 5.3 3 10 7 14h399993v-40H93c3.3-3.3 10.2-9.5
 20.5-18.5s17.8-15.8 22.5-20.5c50.7-52 88-110.3 112-175 4-11.3 5-18.3 3-21-1.3
-4-7.3-6-18-6-8 0-13 .7-15 2s-4.7 6.7-8 16c-42 98.7-107.3 174.7-196 228-6.7 4.7
-10.7 8-12 10-1.3 2-2 5.7-2 11zm100-26v40h399900v-40zM0 435v40h400000v-40z
m0 0v40h400000v-40z`,leftharpoondown:`M7 241c-4 4-6.333 8.667-7 14 0 5.333.667 9 2 11s5.333
 5.333 12 10c90.667 54 156 130 196 228 3.333 10.667 6.333 16.333 9 17 2 .667 5
 1 9 1h5c10.667 0 16.667-2 18-6 2-2.667 1-9.667-3-21-32-87.333-82.667-157.667
-152-211l-3-3h399907v-40zM93 281 H400000 v-40L7 241z`,leftharpoondownplus:`M7 435c-4 4-6.3 8.7-7 14 0 5.3.7 9 2 11s5.3 5.3 12
 10c90.7 54 156 130 196 228 3.3 10.7 6.3 16.3 9 17 2 .7 5 1 9 1h5c10.7 0 16.7
-2 18-6 2-2.7 1-9.7-3-21-32-87.3-82.7-157.7-152-211l-3-3h399907v-40H7zm93 0
v40h399900v-40zM0 241v40h399900v-40zm0 0v40h399900v-40z`,lefthook:`M400000 281 H103s-33-11.2-61-33.5S0 197.3 0 164s14.2-61.2 42.5
-83.5C70.8 58.2 104 47 142 47 c16.7 0 25 6.7 25 20 0 12-8.7 18.7-26 20-40 3.3
-68.7 15.7-86 37-10 12-15 25.3-15 40 0 22.7 9.8 40.7 29.5 54 19.7 13.3 43.5 21
 71.5 23h399859zM103 281v-40h399897v40z`,leftlinesegment:`M40 281 V428 H0 V94 H40 V241 H400000 v40z
M40 281 V428 H0 V94 H40 V241 H400000 v40z`,leftmapsto:`M40 281 V448H0V74H40V241H400000v40z
M40 281 V448H0V74H40V241H400000v40z`,leftToFrom:`M0 147h400000v40H0zm0 214c68 40 115.7 95.7 143 167h22c15.3 0 23
-.3 23-1 0-1.3-5.3-13.7-16-37-18-35.3-41.3-69-70-101l-7-8h399905v-40H95l7-8
c28.7-32 52-65.7 70-101 10.7-23.3 16-35.7 16-37 0-.7-7.7-1-23-1h-22C115.7 265.3
 68 321 0 361zm0-174v-40h399900v40zm100 154v40h399900v-40z`,longequal:`M0 50 h400000 v40H0z m0 194h40000v40H0z
M0 50 h400000 v40H0z m0 194h40000v40H0z`,midbrace:`M200428 334
c-100.7-8.3-195.3-44-280-108-55.3-42-101.7-93-139-153l-9-14c-2.7 4-5.7 8.7-9 14
-53.3 86.7-123.7 153-211 199-66.7 36-137.3 56.3-212 62H0V214h199568c178.3-11.7
 311.7-78.3 403-201 6-8 9.7-12 11-12 .7-.7 6.7-1 18-1s17.3.3 18 1c1.3 0 5 4 11
 12 44.7 59.3 101.3 106.3 170 141s145.3 54.3 229 60h199572v120z`,midbraceunder:`M199572 214
c100.7 8.3 195.3 44 280 108 55.3 42 101.7 93 139 153l9 14c2.7-4 5.7-8.7 9-14
 53.3-86.7 123.7-153 211-199 66.7-36 137.3-56.3 212-62h199568v120H200432c-178.3
 11.7-311.7 78.3-403 201-6 8-9.7 12-11 12-.7.7-6.7 1-18 1s-17.3-.3-18-1c-1.3 0
-5-4-11-12-44.7-59.3-101.3-106.3-170-141s-145.3-54.3-229-60H0V214z`,oiintSize1:`M512.6 71.6c272.6 0 320.3 106.8 320.3 178.2 0 70.8-47.7 177.6
-320.3 177.6S193.1 320.6 193.1 249.8c0-71.4 46.9-178.2 319.5-178.2z
m368.1 178.2c0-86.4-60.9-215.4-368.1-215.4-306.4 0-367.3 129-367.3 215.4 0 85.8
60.9 214.8 367.3 214.8 307.2 0 368.1-129 368.1-214.8z`,oiintSize2:`M757.8 100.1c384.7 0 451.1 137.6 451.1 230 0 91.3-66.4 228.8
-451.1 228.8-386.3 0-452.7-137.5-452.7-228.8 0-92.4 66.4-230 452.7-230z
m502.4 230c0-111.2-82.4-277.2-502.4-277.2s-504 166-504 277.2
c0 110 84 276 504 276s502.4-166 502.4-276z`,oiiintSize1:`M681.4 71.6c408.9 0 480.5 106.8 480.5 178.2 0 70.8-71.6 177.6
-480.5 177.6S202.1 320.6 202.1 249.8c0-71.4 70.5-178.2 479.3-178.2z
m525.8 178.2c0-86.4-86.8-215.4-525.7-215.4-437.9 0-524.7 129-524.7 215.4 0
85.8 86.8 214.8 524.7 214.8 438.9 0 525.7-129 525.7-214.8z`,oiiintSize2:`M1021.2 53c603.6 0 707.8 165.8 707.8 277.2 0 110-104.2 275.8
-707.8 275.8-606 0-710.2-165.8-710.2-275.8C311 218.8 415.2 53 1021.2 53z
m770.4 277.1c0-131.2-126.4-327.6-770.5-327.6S248.4 198.9 248.4 330.1
c0 130 128.8 326.4 772.7 326.4s770.5-196.4 770.5-326.4z`,rightarrow:`M0 241v40h399891c-47.3 35.3-84 78-110 128
-16.7 32-27.7 63.7-33 95 0 1.3-.2 2.7-.5 4-.3 1.3-.5 2.3-.5 3 0 7.3 6.7 11 20
 11 8 0 13.2-.8 15.5-2.5 2.3-1.7 4.2-5.5 5.5-11.5 2-13.3 5.7-27 11-41 14.7-44.7
 39-84.5 73-119.5s73.7-60.2 119-75.5c6-2 9-5.7 9-11s-3-9-9-11c-45.3-15.3-85
-40.5-119-75.5s-58.3-74.8-73-119.5c-4.7-14-8.3-27.3-11-40-1.3-6.7-3.2-10.8-5.5
-12.5-2.3-1.7-7.5-2.5-15.5-2.5-14 0-21 3.7-21 11 0 2 2 10.3 6 25 20.7 83.3 67
 151.7 139 205zm0 0v40h399900v-40z`,rightbrace:`M400000 542l
-6 6h-17c-12.7 0-19.3-.3-20-1-4-4-7.3-8.3-10-13-35.3-51.3-80.8-93.8-136.5-127.5
s-117.2-55.8-184.5-66.5c-.7 0-2-.3-4-1-18.7-2.7-76-4.3-172-5H0V214h399571l6 1
c124.7 8 235 61.7 331 161 31.3 33.3 59.7 72.7 85 118l7 13v35z`,rightbraceunder:`M399994 0l6 6v35l-6 11c-56 104-135.3 181.3-238 232-57.3
 28.7-117 45-179 50H-300V214h399897c43.3-7 81-15 113-26 100.7-33 179.7-91 237
-174 2.7-5 6-9 10-13 .7-1 7.3-1 20-1h17z`,rightgroup:`M0 80h399565c371 0 266.7 149.4 414 180 5.9 1.2 18 0 18 0 2 0
 3-1 3-3v-38c-76-158-257-219-435-219H0z`,rightgroupunder:`M0 262h399565c371 0 266.7-149.4 414-180 5.9-1.2 18 0 18
 0 2 0 3 1 3 3v38c-76 158-257 219-435 219H0z`,rightharpoon:`M0 241v40h399993c4.7-4.7 7-9.3 7-14 0-9.3
-3.7-15.3-11-18-92.7-56.7-159-133.7-199-231-3.3-9.3-6-14.7-8-16-2-1.3-7-2-15-2
-10.7 0-16.7 2-18 6-2 2.7-1 9.7 3 21 15.3 42 36.7 81.8 64 119.5 27.3 37.7 58
 69.2 92 94.5zm0 0v40h399900v-40z`,rightharpoonplus:`M0 241v40h399993c4.7-4.7 7-9.3 7-14 0-9.3-3.7-15.3-11
-18-92.7-56.7-159-133.7-199-231-3.3-9.3-6-14.7-8-16-2-1.3-7-2-15-2-10.7 0-16.7
 2-18 6-2 2.7-1 9.7 3 21 15.3 42 36.7 81.8 64 119.5 27.3 37.7 58 69.2 92 94.5z
m0 0v40h399900v-40z m100 194v40h399900v-40zm0 0v40h399900v-40z`,rightharpoondown:`M399747 511c0 7.3 6.7 11 20 11 8 0 13-.8 15-2.5s4.7-6.8
 8-15.5c40-94 99.3-166.3 178-217 13.3-8 20.3-12.3 21-13 5.3-3.3 8.5-5.8 9.5
-7.5 1-1.7 1.5-5.2 1.5-10.5s-2.3-10.3-7-15H0v40h399908c-34 25.3-64.7 57-92 95
-27.3 38-48.7 77.7-64 119-3.3 8.7-5 14-5 16zM0 241v40h399900v-40z`,rightharpoondownplus:`M399747 705c0 7.3 6.7 11 20 11 8 0 13-.8
 15-2.5s4.7-6.8 8-15.5c40-94 99.3-166.3 178-217 13.3-8 20.3-12.3 21-13 5.3-3.3
 8.5-5.8 9.5-7.5 1-1.7 1.5-5.2 1.5-10.5s-2.3-10.3-7-15H0v40h399908c-34 25.3
-64.7 57-92 95-27.3 38-48.7 77.7-64 119-3.3 8.7-5 14-5 16zM0 435v40h399900v-40z
m0-194v40h400000v-40zm0 0v40h400000v-40z`,righthook:`M399859 241c-764 0 0 0 0 0 40-3.3 68.7-15.7 86-37 10-12 15-25.3
 15-40 0-22.7-9.8-40.7-29.5-54-19.7-13.3-43.5-21-71.5-23-17.3-1.3-26-8-26-20 0
-13.3 8.7-20 26-20 38 0 71 11.2 99 33.5 0 0 7 5.6 21 16.7 14 11.2 21 33.5 21
 66.8s-14 61.2-42 83.5c-28 22.3-61 33.5-99 33.5L0 241z M0 281v-40h399859v40z`,rightlinesegment:`M399960 241 V94 h40 V428 h-40 V281 H0 v-40z
M399960 241 V94 h40 V428 h-40 V281 H0 v-40z`,rightToFrom:`M400000 167c-70.7-42-118-97.7-142-167h-23c-15.3 0-23 .3-23
 1 0 1.3 5.3 13.7 16 37 18 35.3 41.3 69 70 101l7 8H0v40h399905l-7 8c-28.7 32
-52 65.7-70 101-10.7 23.3-16 35.7-16 37 0 .7 7.7 1 23 1h23c24-69.3 71.3-125 142
-167z M100 147v40h399900v-40zM0 341v40h399900v-40z`,twoheadleftarrow:`M0 167c68 40
 115.7 95.7 143 167h22c15.3 0 23-.3 23-1 0-1.3-5.3-13.7-16-37-18-35.3-41.3-69
-70-101l-7-8h125l9 7c50.7 39.3 85 86 103 140h46c0-4.7-6.3-18.7-19-42-18-35.3
-40-67.3-66-96l-9-9h399716v-40H284l9-9c26-28.7 48-60.7 66-96 12.7-23.333 19
-37.333 19-42h-46c-18 54-52.3 100.7-103 140l-9 7H95l7-8c28.7-32 52-65.7 70-101
 10.7-23.333 16-35.7 16-37 0-.7-7.7-1-23-1h-22C115.7 71.3 68 127 0 167z`,twoheadrightarrow:`M400000 167
c-68-40-115.7-95.7-143-167h-22c-15.3 0-23 .3-23 1 0 1.3 5.3 13.7 16 37 18 35.3
 41.3 69 70 101l7 8h-125l-9-7c-50.7-39.3-85-86-103-140h-46c0 4.7 6.3 18.7 19 42
 18 35.3 40 67.3 66 96l9 9H0v40h399716l-9 9c-26 28.7-48 60.7-66 96-12.7 23.333
-19 37.333-19 42h46c18-54 52.3-100.7 103-140l9-7h125l-7 8c-28.7 32-52 65.7-70
 101-10.7 23.333-16 35.7-16 37 0 .7 7.7 1 23 1h22c27.3-71.3 75-127 143-167z`,tilde1:`M200 55.538c-77 0-168 73.953-177 73.953-3 0-7
-2.175-9-5.437L2 97c-1-2-2-4-2-6 0-4 2-7 5-9l20-12C116 12 171 0 207 0c86 0
 114 68 191 68 78 0 168-68 177-68 4 0 7 2 9 5l12 19c1 2.175 2 4.35 2 6.525 0
 4.35-2 7.613-5 9.788l-19 13.05c-92 63.077-116.937 75.308-183 76.128
-68.267.847-113-73.952-191-73.952z`,tilde2:`M344 55.266c-142 0-300.638 81.316-311.5 86.418
-8.01 3.762-22.5 10.91-23.5 5.562L1 120c-1-2-1-3-1-4 0-5 3-9 8-10l18.4-9C160.9
 31.9 283 0 358 0c148 0 188 122 331 122s314-97 326-97c4 0 8 2 10 7l7 21.114
c1 2.14 1 3.21 1 4.28 0 5.347-3 9.626-7 10.696l-22.3 12.622C852.6 158.372 751
 181.476 676 181.476c-149 0-189-126.21-332-126.21z`,tilde3:`M786 59C457 59 32 175.242 13 175.242c-6 0-10-3.457
-11-10.37L.15 138c-1-7 3-12 10-13l19.2-6.4C378.4 40.7 634.3 0 804.3 0c337 0
 411.8 157 746.8 157 328 0 754-112 773-112 5 0 10 3 11 9l1 14.075c1 8.066-.697
 16.595-6.697 17.492l-21.052 7.31c-367.9 98.146-609.15 122.696-778.15 122.696
 -338 0-409-156.573-744-156.573z`,tilde4:`M786 58C457 58 32 177.487 13 177.487c-6 0-10-3.345
-11-10.035L.15 143c-1-7 3-12 10-13l22-6.7C381.2 35 637.15 0 807.15 0c337 0 409
 177 744 177 328 0 754-127 773-127 5 0 10 3 11 9l1 14.794c1 7.805-3 13.38-9
 14.495l-20.7 5.574c-366.85 99.79-607.3 139.372-776.3 139.372-338 0-409
 -175.236-744-175.236z`,vec:`M377 20c0-5.333 1.833-10 5.5-14S391 0 397 0c4.667 0 8.667 1.667 12 5
3.333 2.667 6.667 9 10 19 6.667 24.667 20.333 43.667 41 57 7.333 4.667 11
10.667 11 18 0 6-1 10-3 12s-6.667 5-14 9c-28.667 14.667-53.667 35.667-75 63
-1.333 1.333-3.167 3.5-5.5 6.5s-4 4.833-5 5.5c-1 .667-2.5 1.333-4.5 2s-4.333 1
-7 1c-4.667 0-9.167-1.833-13.5-5.5S337 184 337 178c0-12.667 15.667-32.333 47-59
H213l-171-1c-8.667-6-13-12.333-13-19 0-4.667 4.333-11.333 13-20h359
c-16-25.333-24-45-24-59z`,widehat1:`M529 0h5l519 115c5 1 9 5 9 10 0 1-1 2-1 3l-4 22
c-1 5-5 9-11 9h-2L532 67 19 159h-2c-5 0-9-4-11-9l-5-22c-1-6 2-12 8-13z`,widehat2:`M1181 0h2l1171 176c6 0 10 5 10 11l-2 23c-1 6-5 10
-11 10h-1L1182 67 15 220h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z`,widehat3:`M1181 0h2l1171 236c6 0 10 5 10 11l-2 23c-1 6-5 10
-11 10h-1L1182 67 15 280h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z`,widehat4:`M1181 0h2l1171 296c6 0 10 5 10 11l-2 23c-1 6-5 10
-11 10h-1L1182 67 15 340h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z`,widecheck1:`M529,159h5l519,-115c5,-1,9,-5,9,-10c0,-1,-1,-2,-1,-3l-4,-22c-1,
-5,-5,-9,-11,-9h-2l-512,92l-513,-92h-2c-5,0,-9,4,-11,9l-5,22c-1,6,2,12,8,13z`,widecheck2:`M1181,220h2l1171,-176c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,
-11,-10h-1l-1168,153l-1167,-153h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z`,widecheck3:`M1181,280h2l1171,-236c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,
-11,-10h-1l-1168,213l-1167,-213h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z`,widecheck4:`M1181,340h2l1171,-296c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,
-11,-10h-1l-1168,273l-1167,-273h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z`,baraboveleftarrow:`M400000 620h-399890l3 -3c68.7 -52.7 113.7 -120 135 -202
c4 -14.7 6 -23 6 -25c0 -7.3 -7 -11 -21 -11c-8 0 -13.2 0.8 -15.5 2.5
c-2.3 1.7 -4.2 5.8 -5.5 12.5c-1.3 4.7 -2.7 10.3 -4 17c-12 48.7 -34.8 92 -68.5 130
s-74.2 66.3 -121.5 85c-10 4 -16 7.7 -18 11c0 8.7 6 14.3 18 17c47.3 18.7 87.8 47
121.5 85s56.5 81.3 68.5 130c0.7 2 1.3 5 2 9s1.2 6.7 1.5 8c0.3 1.3 1 3.3 2 6
s2.2 4.5 3.5 5.5c1.3 1 3.3 1.8 6 2.5s6 1 10 1c14 0 21 -3.7 21 -11
c0 -2 -2 -10.3 -6 -25c-20 -79.3 -65 -146.7 -135 -202l-3 -3h399890z
M100 620v40h399900v-40z M0 241v40h399900v-40zM0 241v40h399900v-40z`,rightarrowabovebar:`M0 241v40h399891c-47.3 35.3-84 78-110 128-16.7 32
-27.7 63.7-33 95 0 1.3-.2 2.7-.5 4-.3 1.3-.5 2.3-.5 3 0 7.3 6.7 11 20 11 8 0
13.2-.8 15.5-2.5 2.3-1.7 4.2-5.5 5.5-11.5 2-13.3 5.7-27 11-41 14.7-44.7 39
-84.5 73-119.5s73.7-60.2 119-75.5c6-2 9-5.7 9-11s-3-9-9-11c-45.3-15.3-85-40.5
-119-75.5s-58.3-74.8-73-119.5c-4.7-14-8.3-27.3-11-40-1.3-6.7-3.2-10.8-5.5
-12.5-2.3-1.7-7.5-2.5-15.5-2.5-14 0-21 3.7-21 11 0 2 2 10.3 6 25 20.7 83.3 67
151.7 139 205zm96 379h399894v40H0zm0 0h399904v40H0z`,baraboveshortleftharpoon:`M507,435c-4,4,-6.3,8.7,-7,14c0,5.3,0.7,9,2,11
c1.3,2,5.3,5.3,12,10c90.7,54,156,130,196,228c3.3,10.7,6.3,16.3,9,17
c2,0.7,5,1,9,1c0,0,5,0,5,0c10.7,0,16.7,-2,18,-6c2,-2.7,1,-9.7,-3,-21
c-32,-87.3,-82.7,-157.7,-152,-211c0,0,-3,-3,-3,-3l399351,0l0,-40
c-398570,0,-399437,0,-399437,0z M593 435 v40 H399500 v-40z
M0 281 v-40 H399908 v40z M0 281 v-40 H399908 v40z`,rightharpoonaboveshortbar:`M0,241 l0,40c399126,0,399993,0,399993,0
c4.7,-4.7,7,-9.3,7,-14c0,-9.3,-3.7,-15.3,-11,-18c-92.7,-56.7,-159,-133.7,-199,
-231c-3.3,-9.3,-6,-14.7,-8,-16c-2,-1.3,-7,-2,-15,-2c-10.7,0,-16.7,2,-18,6
c-2,2.7,-1,9.7,3,21c15.3,42,36.7,81.8,64,119.5c27.3,37.7,58,69.2,92,94.5z
M0 241 v40 H399908 v-40z M0 475 v-40 H399500 v40z M0 475 v-40 H399500 v40z`,shortbaraboveleftharpoon:`M7,435c-4,4,-6.3,8.7,-7,14c0,5.3,0.7,9,2,11
c1.3,2,5.3,5.3,12,10c90.7,54,156,130,196,228c3.3,10.7,6.3,16.3,9,17c2,0.7,5,1,9,
1c0,0,5,0,5,0c10.7,0,16.7,-2,18,-6c2,-2.7,1,-9.7,-3,-21c-32,-87.3,-82.7,-157.7,
-152,-211c0,0,-3,-3,-3,-3l399907,0l0,-40c-399126,0,-399993,0,-399993,0z
M93 435 v40 H400000 v-40z M500 241 v40 H400000 v-40z M500 241 v40 H400000 v-40z`,shortrightharpoonabovebar:`M53,241l0,40c398570,0,399437,0,399437,0
c4.7,-4.7,7,-9.3,7,-14c0,-9.3,-3.7,-15.3,-11,-18c-92.7,-56.7,-159,-133.7,-199,
-231c-3.3,-9.3,-6,-14.7,-8,-16c-2,-1.3,-7,-2,-15,-2c-10.7,0,-16.7,2,-18,6
c-2,2.7,-1,9.7,3,21c15.3,42,36.7,81.8,64,119.5c27.3,37.7,58,69.2,92,94.5z
M500 241 v40 H399408 v-40z M500 435 v40 H400000 v-40z`},fe=function(){function l(t){this.children=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,this.children=t,this.classes=[],this.height=0,this.depth=0,this.maxFontSize=0,this.style={}}u(l,"DocumentFragment");var e=l.prototype;return e.hasClass=u(function(r){return E.contains(this.classes,r)},"hasClass"),e.toNode=u(function(){for(var r=document.createDocumentFragment(),n=0;n<this.children.length;n++)r.appendChild(this.children[n].toNode());return r},"toNode"),e.toMarkup=u(function(){for(var r="",n=0;n<this.children.length;n++)r+=this.children[n].toMarkup();return r},"toMarkup"),e.toText=u(function(){var r=u(function(a){return a.toText()},"toText");return this.children.map(r).join("")},"toText"),l}(),H0=u(function(e){return e.filter(function(t){return t}).join(" ")},"createClass"),Ut=u(function(e,t,r){if(this.classes=e||[],this.attributes={},this.height=0,this.depth=0,this.maxFontSize=0,this.style=r||{},t){t.style.isTight()&&this.classes.push("mtight");var n=t.getColor();n&&(this.style.color=n)}},"initNode"),Yt=u(function(e){var t=document.createElement(e);t.className=H0(this.classes);for(var r in this.style)this.style.hasOwnProperty(r)&&(t.style[r]=this.style[r]);for(var n in this.attributes)this.attributes.hasOwnProperty(n)&&t.setAttribute(n,this.attributes[n]);for(var a=0;a<this.children.length;a++)t.appendChild(this.children[a].toNode());return t},"toNode"),Xt=u(function(e){var t="<"+e;this.classes.length&&(t+=' class="'+E.escape(H0(this.classes))+'"');var r="";for(var n in this.style)this.style.hasOwnProperty(n)&&(r+=E.hyphenate(n)+":"+this.style[n]+";");r&&(t+=' style="'+E.escape(r)+'"');for(var a in this.attributes)this.attributes.hasOwnProperty(a)&&(t+=" "+a+'="'+E.escape(this.attributes[a])+'"');t+=">";for(var o=0;o<this.children.length;o++)t+=this.children[o].toMarkup();return t+="</"+e+">",t},"toMarkup"),pe=function(){function l(t,r,n,a){this.children=void 0,this.attributes=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.width=void 0,this.maxFontSize=void 0,this.style=void 0,Ut.call(this,t,n,a),this.children=r||[]}u(l,"Span");var e=l.prototype;return e.setAttribute=u(function(r,n){this.attributes[r]=n},"setAttribute"),e.hasClass=u(function(r){return E.contains(this.classes,r)},"hasClass"),e.toNode=u(function(){return Yt.call(this,"span")},"toNode"),e.toMarkup=u(function(){return Xt.call(this,"span")},"toMarkup"),l}(),Je=function(){function l(t,r,n,a){this.children=void 0,this.attributes=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,Ut.call(this,r,a),this.children=n||[],this.setAttribute("href",t)}u(l,"Anchor");var e=l.prototype;return e.setAttribute=u(function(r,n){this.attributes[r]=n},"setAttribute"),e.hasClass=u(function(r){return E.contains(this.classes,r)},"hasClass"),e.toNode=u(function(){return Yt.call(this,"a")},"toNode"),e.toMarkup=u(function(){return Xt.call(this,"a")},"toMarkup"),l}(),Yn=function(){function l(t,r,n){this.src=void 0,this.alt=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,this.alt=r,this.src=t,this.classes=["mord"],this.style=n}u(l,"Img");var e=l.prototype;return e.hasClass=u(function(r){return E.contains(this.classes,r)},"hasClass"),e.toNode=u(function(){var r=document.createElement("img");r.src=this.src,r.alt=this.alt,r.className="mord";for(var n in this.style)this.style.hasOwnProperty(n)&&(r.style[n]=this.style[n]);return r},"toNode"),e.toMarkup=u(function(){var r="<img  src='"+this.src+" 'alt='"+this.alt+"' ",n="";for(var a in this.style)this.style.hasOwnProperty(a)&&(n+=E.hyphenate(a)+":"+this.style[a]+";");return n&&(r+=' style="'+E.escape(n)+'"'),r+="'/>",r},"toMarkup"),l}(),Xn={î:"ı̂",ï:"ı̈",í:"ı́",ì:"ı̀"},x0=function(){function l(t,r,n,a,o,m,h,f){this.text=void 0,this.height=void 0,this.depth=void 0,this.italic=void 0,this.skew=void 0,this.width=void 0,this.maxFontSize=void 0,this.classes=void 0,this.style=void 0,this.text=t,this.height=r||0,this.depth=n||0,this.italic=a||0,this.skew=o||0,this.width=m||0,this.classes=h||[],this.style=f||{},this.maxFontSize=0;var g=Fn(this.text.charCodeAt(0));g&&this.classes.push(g+"_fallback"),/[îïíì]/.test(this.text)&&(this.text=Xn[this.text])}u(l,"SymbolNode");var e=l.prototype;return e.hasClass=u(function(r){return E.contains(this.classes,r)},"hasClass"),e.toNode=u(function(){var r=document.createTextNode(this.text),n=null;this.italic>0&&(n=document.createElement("span"),n.style.marginRight=this.italic+"em"),this.classes.length>0&&(n=n||document.createElement("span"),n.className=H0(this.classes));for(var a in this.style)this.style.hasOwnProperty(a)&&(n=n||document.createElement("span"),n.style[a]=this.style[a]);return n?(n.appendChild(r),n):r},"toNode"),e.toMarkup=u(function(){var r=!1,n="<span";this.classes.length&&(r=!0,n+=' class="',n+=E.escape(H0(this.classes)),n+='"');var a="";this.italic>0&&(a+="margin-right:"+this.italic+"em;");for(var o in this.style)this.style.hasOwnProperty(o)&&(a+=E.hyphenate(o)+":"+this.style[o]+";");a&&(r=!0,n+=' style="'+E.escape(a)+'"');var m=E.escape(this.text);return r?(n+=">",n+=m,n+="</span>",n):m},"toMarkup"),l}(),P0=function(){function l(t,r){this.children=void 0,this.attributes=void 0,this.children=t||[],this.attributes=r||{}}u(l,"SvgNode");var e=l.prototype;return e.toNode=u(function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"svg");for(var a in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,a)&&n.setAttribute(a,this.attributes[a]);for(var o=0;o<this.children.length;o++)n.appendChild(this.children[o].toNode());return n},"toNode"),e.toMarkup=u(function(){var r="<svg";for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+"='"+this.attributes[n]+"'");r+=">";for(var a=0;a<this.children.length;a++)r+=this.children[a].toMarkup();return r+="</svg>",r},"toMarkup"),l}(),K0=function(){function l(t,r){this.pathName=void 0,this.alternate=void 0,this.pathName=t,this.alternate=r}u(l,"PathNode");var e=l.prototype;return e.toNode=u(function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"path");return this.alternate?n.setAttribute("d",this.alternate):n.setAttribute("d",Wt[this.pathName]),n},"toNode"),e.toMarkup=u(function(){return this.alternate?"<path d='"+this.alternate+"'/>":"<path d='"+Wt[this.pathName]+"'/>"},"toMarkup"),l}(),Qe=function(){function l(t){this.attributes=void 0,this.attributes=t||{}}u(l,"LineNode");var e=l.prototype;return e.toNode=u(function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"line");for(var a in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,a)&&n.setAttribute(a,this.attributes[a]);return n},"toNode"),e.toMarkup=u(function(){var r="<line";for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+"='"+this.attributes[n]+"'");return r+="/>",r},"toMarkup"),l}();function $t(l){if(l instanceof x0)return l;throw new Error("Expected symbolNode but got "+String(l)+".")}u($t,"assertSymbolDomNode");function $n(l){if(l instanceof pe)return l;throw new Error("Expected span<HtmlDomNode> but got "+String(l)+".")}u($n,"assertSpan");var B0={"AMS-Regular":{32:[0,0,0,0,.25],65:[0,.68889,0,0,.72222],66:[0,.68889,0,0,.66667],67:[0,.68889,0,0,.72222],68:[0,.68889,0,0,.72222],69:[0,.68889,0,0,.66667],70:[0,.68889,0,0,.61111],71:[0,.68889,0,0,.77778],72:[0,.68889,0,0,.77778],73:[0,.68889,0,0,.38889],74:[.16667,.68889,0,0,.5],75:[0,.68889,0,0,.77778],76:[0,.68889,0,0,.66667],77:[0,.68889,0,0,.94445],78:[0,.68889,0,0,.72222],79:[.16667,.68889,0,0,.77778],80:[0,.68889,0,0,.61111],81:[.16667,.68889,0,0,.77778],82:[0,.68889,0,0,.72222],83:[0,.68889,0,0,.55556],84:[0,.68889,0,0,.66667],85:[0,.68889,0,0,.72222],86:[0,.68889,0,0,.72222],87:[0,.68889,0,0,1],88:[0,.68889,0,0,.72222],89:[0,.68889,0,0,.72222],90:[0,.68889,0,0,.66667],107:[0,.68889,0,0,.55556],160:[0,0,0,0,.25],165:[0,.675,.025,0,.75],174:[.15559,.69224,0,0,.94666],240:[0,.68889,0,0,.55556],295:[0,.68889,0,0,.54028],710:[0,.825,0,0,2.33334],732:[0,.9,0,0,2.33334],770:[0,.825,0,0,2.33334],771:[0,.9,0,0,2.33334],989:[.08167,.58167,0,0,.77778],1008:[0,.43056,.04028,0,.66667],8245:[0,.54986,0,0,.275],8463:[0,.68889,0,0,.54028],8487:[0,.68889,0,0,.72222],8498:[0,.68889,0,0,.55556],8502:[0,.68889,0,0,.66667],8503:[0,.68889,0,0,.44445],8504:[0,.68889,0,0,.66667],8513:[0,.68889,0,0,.63889],8592:[-.03598,.46402,0,0,.5],8594:[-.03598,.46402,0,0,.5],8602:[-.13313,.36687,0,0,1],8603:[-.13313,.36687,0,0,1],8606:[.01354,.52239,0,0,1],8608:[.01354,.52239,0,0,1],8610:[.01354,.52239,0,0,1.11111],8611:[.01354,.52239,0,0,1.11111],8619:[0,.54986,0,0,1],8620:[0,.54986,0,0,1],8621:[-.13313,.37788,0,0,1.38889],8622:[-.13313,.36687,0,0,1],8624:[0,.69224,0,0,.5],8625:[0,.69224,0,0,.5],8630:[0,.43056,0,0,1],8631:[0,.43056,0,0,1],8634:[.08198,.58198,0,0,.77778],8635:[.08198,.58198,0,0,.77778],8638:[.19444,.69224,0,0,.41667],8639:[.19444,.69224,0,0,.41667],8642:[.19444,.69224,0,0,.41667],8643:[.19444,.69224,0,0,.41667],8644:[.1808,.675,0,0,1],8646:[.1808,.675,0,0,1],8647:[.1808,.675,0,0,1],8648:[.19444,.69224,0,0,.83334],8649:[.1808,.675,0,0,1],8650:[.19444,.69224,0,0,.83334],8651:[.01354,.52239,0,0,1],8652:[.01354,.52239,0,0,1],8653:[-.13313,.36687,0,0,1],8654:[-.13313,.36687,0,0,1],8655:[-.13313,.36687,0,0,1],8666:[.13667,.63667,0,0,1],8667:[.13667,.63667,0,0,1],8669:[-.13313,.37788,0,0,1],8672:[-.064,.437,0,0,1.334],8674:[-.064,.437,0,0,1.334],8705:[0,.825,0,0,.5],8708:[0,.68889,0,0,.55556],8709:[.08167,.58167,0,0,.77778],8717:[0,.43056,0,0,.42917],8722:[-.03598,.46402,0,0,.5],8724:[.08198,.69224,0,0,.77778],8726:[.08167,.58167,0,0,.77778],8733:[0,.69224,0,0,.77778],8736:[0,.69224,0,0,.72222],8737:[0,.69224,0,0,.72222],8738:[.03517,.52239,0,0,.72222],8739:[.08167,.58167,0,0,.22222],8740:[.25142,.74111,0,0,.27778],8741:[.08167,.58167,0,0,.38889],8742:[.25142,.74111,0,0,.5],8756:[0,.69224,0,0,.66667],8757:[0,.69224,0,0,.66667],8764:[-.13313,.36687,0,0,.77778],8765:[-.13313,.37788,0,0,.77778],8769:[-.13313,.36687,0,0,.77778],8770:[-.03625,.46375,0,0,.77778],8774:[.30274,.79383,0,0,.77778],8776:[-.01688,.48312,0,0,.77778],8778:[.08167,.58167,0,0,.77778],8782:[.06062,.54986,0,0,.77778],8783:[.06062,.54986,0,0,.77778],8785:[.08198,.58198,0,0,.77778],8786:[.08198,.58198,0,0,.77778],8787:[.08198,.58198,0,0,.77778],8790:[0,.69224,0,0,.77778],8791:[.22958,.72958,0,0,.77778],8796:[.08198,.91667,0,0,.77778],8806:[.25583,.75583,0,0,.77778],8807:[.25583,.75583,0,0,.77778],8808:[.25142,.75726,0,0,.77778],8809:[.25142,.75726,0,0,.77778],8812:[.25583,.75583,0,0,.5],8814:[.20576,.70576,0,0,.77778],8815:[.20576,.70576,0,0,.77778],8816:[.30274,.79383,0,0,.77778],8817:[.30274,.79383,0,0,.77778],8818:[.22958,.72958,0,0,.77778],8819:[.22958,.72958,0,0,.77778],8822:[.1808,.675,0,0,.77778],8823:[.1808,.675,0,0,.77778],8828:[.13667,.63667,0,0,.77778],8829:[.13667,.63667,0,0,.77778],8830:[.22958,.72958,0,0,.77778],8831:[.22958,.72958,0,0,.77778],8832:[.20576,.70576,0,0,.77778],8833:[.20576,.70576,0,0,.77778],8840:[.30274,.79383,0,0,.77778],8841:[.30274,.79383,0,0,.77778],8842:[.13597,.63597,0,0,.77778],8843:[.13597,.63597,0,0,.77778],8847:[.03517,.54986,0,0,.77778],8848:[.03517,.54986,0,0,.77778],8858:[.08198,.58198,0,0,.77778],8859:[.08198,.58198,0,0,.77778],8861:[.08198,.58198,0,0,.77778],8862:[0,.675,0,0,.77778],8863:[0,.675,0,0,.77778],8864:[0,.675,0,0,.77778],8865:[0,.675,0,0,.77778],8872:[0,.69224,0,0,.61111],8873:[0,.69224,0,0,.72222],8874:[0,.69224,0,0,.88889],8876:[0,.68889,0,0,.61111],8877:[0,.68889,0,0,.61111],8878:[0,.68889,0,0,.72222],8879:[0,.68889,0,0,.72222],8882:[.03517,.54986,0,0,.77778],8883:[.03517,.54986,0,0,.77778],8884:[.13667,.63667,0,0,.77778],8885:[.13667,.63667,0,0,.77778],8888:[0,.54986,0,0,1.11111],8890:[.19444,.43056,0,0,.55556],8891:[.19444,.69224,0,0,.61111],8892:[.19444,.69224,0,0,.61111],8901:[0,.54986,0,0,.27778],8903:[.08167,.58167,0,0,.77778],8905:[.08167,.58167,0,0,.77778],8906:[.08167,.58167,0,0,.77778],8907:[0,.69224,0,0,.77778],8908:[0,.69224,0,0,.77778],8909:[-.03598,.46402,0,0,.77778],8910:[0,.54986,0,0,.76042],8911:[0,.54986,0,0,.76042],8912:[.03517,.54986,0,0,.77778],8913:[.03517,.54986,0,0,.77778],8914:[0,.54986,0,0,.66667],8915:[0,.54986,0,0,.66667],8916:[0,.69224,0,0,.66667],8918:[.0391,.5391,0,0,.77778],8919:[.0391,.5391,0,0,.77778],8920:[.03517,.54986,0,0,1.33334],8921:[.03517,.54986,0,0,1.33334],8922:[.38569,.88569,0,0,.77778],8923:[.38569,.88569,0,0,.77778],8926:[.13667,.63667,0,0,.77778],8927:[.13667,.63667,0,0,.77778],8928:[.30274,.79383,0,0,.77778],8929:[.30274,.79383,0,0,.77778],8934:[.23222,.74111,0,0,.77778],8935:[.23222,.74111,0,0,.77778],8936:[.23222,.74111,0,0,.77778],8937:[.23222,.74111,0,0,.77778],8938:[.20576,.70576,0,0,.77778],8939:[.20576,.70576,0,0,.77778],8940:[.30274,.79383,0,0,.77778],8941:[.30274,.79383,0,0,.77778],8994:[.19444,.69224,0,0,.77778],8995:[.19444,.69224,0,0,.77778],9416:[.15559,.69224,0,0,.90222],9484:[0,.69224,0,0,.5],9488:[0,.69224,0,0,.5],9492:[0,.37788,0,0,.5],9496:[0,.37788,0,0,.5],9585:[.19444,.68889,0,0,.88889],9586:[.19444,.74111,0,0,.88889],9632:[0,.675,0,0,.77778],9633:[0,.675,0,0,.77778],9650:[0,.54986,0,0,.72222],9651:[0,.54986,0,0,.72222],9654:[.03517,.54986,0,0,.77778],9660:[0,.54986,0,0,.72222],9661:[0,.54986,0,0,.72222],9664:[.03517,.54986,0,0,.77778],9674:[.11111,.69224,0,0,.66667],9733:[.19444,.69224,0,0,.94445],10003:[0,.69224,0,0,.83334],10016:[0,.69224,0,0,.83334],10731:[.11111,.69224,0,0,.66667],10846:[.19444,.75583,0,0,.61111],10877:[.13667,.63667,0,0,.77778],10878:[.13667,.63667,0,0,.77778],10885:[.25583,.75583,0,0,.77778],10886:[.25583,.75583,0,0,.77778],10887:[.13597,.63597,0,0,.77778],10888:[.13597,.63597,0,0,.77778],10889:[.26167,.75726,0,0,.77778],10890:[.26167,.75726,0,0,.77778],10891:[.48256,.98256,0,0,.77778],10892:[.48256,.98256,0,0,.77778],10901:[.13667,.63667,0,0,.77778],10902:[.13667,.63667,0,0,.77778],10933:[.25142,.75726,0,0,.77778],10934:[.25142,.75726,0,0,.77778],10935:[.26167,.75726,0,0,.77778],10936:[.26167,.75726,0,0,.77778],10937:[.26167,.75726,0,0,.77778],10938:[.26167,.75726,0,0,.77778],10949:[.25583,.75583,0,0,.77778],10950:[.25583,.75583,0,0,.77778],10955:[.28481,.79383,0,0,.77778],10956:[.28481,.79383,0,0,.77778],57350:[.08167,.58167,0,0,.22222],57351:[.08167,.58167,0,0,.38889],57352:[.08167,.58167,0,0,.77778],57353:[0,.43056,.04028,0,.66667],57356:[.25142,.75726,0,0,.77778],57357:[.25142,.75726,0,0,.77778],57358:[.41951,.91951,0,0,.77778],57359:[.30274,.79383,0,0,.77778],57360:[.30274,.79383,0,0,.77778],57361:[.41951,.91951,0,0,.77778],57366:[.25142,.75726,0,0,.77778],57367:[.25142,.75726,0,0,.77778],57368:[.25142,.75726,0,0,.77778],57369:[.25142,.75726,0,0,.77778],57370:[.13597,.63597,0,0,.77778],57371:[.13597,.63597,0,0,.77778]},"Caligraphic-Regular":{32:[0,0,0,0,.25],65:[0,.68333,0,.19445,.79847],66:[0,.68333,.03041,.13889,.65681],67:[0,.68333,.05834,.13889,.52653],68:[0,.68333,.02778,.08334,.77139],69:[0,.68333,.08944,.11111,.52778],70:[0,.68333,.09931,.11111,.71875],71:[.09722,.68333,.0593,.11111,.59487],72:[0,.68333,.00965,.11111,.84452],73:[0,.68333,.07382,0,.54452],74:[.09722,.68333,.18472,.16667,.67778],75:[0,.68333,.01445,.05556,.76195],76:[0,.68333,0,.13889,.68972],77:[0,.68333,0,.13889,1.2009],78:[0,.68333,.14736,.08334,.82049],79:[0,.68333,.02778,.11111,.79611],80:[0,.68333,.08222,.08334,.69556],81:[.09722,.68333,0,.11111,.81667],82:[0,.68333,0,.08334,.8475],83:[0,.68333,.075,.13889,.60556],84:[0,.68333,.25417,0,.54464],85:[0,.68333,.09931,.08334,.62583],86:[0,.68333,.08222,0,.61278],87:[0,.68333,.08222,.08334,.98778],88:[0,.68333,.14643,.13889,.7133],89:[.09722,.68333,.08222,.08334,.66834],90:[0,.68333,.07944,.13889,.72473],160:[0,0,0,0,.25]},"Fraktur-Regular":{32:[0,0,0,0,.25],33:[0,.69141,0,0,.29574],34:[0,.69141,0,0,.21471],38:[0,.69141,0,0,.73786],39:[0,.69141,0,0,.21201],40:[.24982,.74947,0,0,.38865],41:[.24982,.74947,0,0,.38865],42:[0,.62119,0,0,.27764],43:[.08319,.58283,0,0,.75623],44:[0,.10803,0,0,.27764],45:[.08319,.58283,0,0,.75623],46:[0,.10803,0,0,.27764],47:[.24982,.74947,0,0,.50181],48:[0,.47534,0,0,.50181],49:[0,.47534,0,0,.50181],50:[0,.47534,0,0,.50181],51:[.18906,.47534,0,0,.50181],52:[.18906,.47534,0,0,.50181],53:[.18906,.47534,0,0,.50181],54:[0,.69141,0,0,.50181],55:[.18906,.47534,0,0,.50181],56:[0,.69141,0,0,.50181],57:[.18906,.47534,0,0,.50181],58:[0,.47534,0,0,.21606],59:[.12604,.47534,0,0,.21606],61:[-.13099,.36866,0,0,.75623],63:[0,.69141,0,0,.36245],65:[0,.69141,0,0,.7176],66:[0,.69141,0,0,.88397],67:[0,.69141,0,0,.61254],68:[0,.69141,0,0,.83158],69:[0,.69141,0,0,.66278],70:[.12604,.69141,0,0,.61119],71:[0,.69141,0,0,.78539],72:[.06302,.69141,0,0,.7203],73:[0,.69141,0,0,.55448],74:[.12604,.69141,0,0,.55231],75:[0,.69141,0,0,.66845],76:[0,.69141,0,0,.66602],77:[0,.69141,0,0,1.04953],78:[0,.69141,0,0,.83212],79:[0,.69141,0,0,.82699],80:[.18906,.69141,0,0,.82753],81:[.03781,.69141,0,0,.82699],82:[0,.69141,0,0,.82807],83:[0,.69141,0,0,.82861],84:[0,.69141,0,0,.66899],85:[0,.69141,0,0,.64576],86:[0,.69141,0,0,.83131],87:[0,.69141,0,0,1.04602],88:[0,.69141,0,0,.71922],89:[.18906,.69141,0,0,.83293],90:[.12604,.69141,0,0,.60201],91:[.24982,.74947,0,0,.27764],93:[.24982,.74947,0,0,.27764],94:[0,.69141,0,0,.49965],97:[0,.47534,0,0,.50046],98:[0,.69141,0,0,.51315],99:[0,.47534,0,0,.38946],100:[0,.62119,0,0,.49857],101:[0,.47534,0,0,.40053],102:[.18906,.69141,0,0,.32626],103:[.18906,.47534,0,0,.5037],104:[.18906,.69141,0,0,.52126],105:[0,.69141,0,0,.27899],106:[0,.69141,0,0,.28088],107:[0,.69141,0,0,.38946],108:[0,.69141,0,0,.27953],109:[0,.47534,0,0,.76676],110:[0,.47534,0,0,.52666],111:[0,.47534,0,0,.48885],112:[.18906,.52396,0,0,.50046],113:[.18906,.47534,0,0,.48912],114:[0,.47534,0,0,.38919],115:[0,.47534,0,0,.44266],116:[0,.62119,0,0,.33301],117:[0,.47534,0,0,.5172],118:[0,.52396,0,0,.5118],119:[0,.52396,0,0,.77351],120:[.18906,.47534,0,0,.38865],121:[.18906,.47534,0,0,.49884],122:[.18906,.47534,0,0,.39054],160:[0,0,0,0,.25],8216:[0,.69141,0,0,.21471],8217:[0,.69141,0,0,.21471],58112:[0,.62119,0,0,.49749],58113:[0,.62119,0,0,.4983],58114:[.18906,.69141,0,0,.33328],58115:[.18906,.69141,0,0,.32923],58116:[.18906,.47534,0,0,.50343],58117:[0,.69141,0,0,.33301],58118:[0,.62119,0,0,.33409],58119:[0,.47534,0,0,.50073]},"Main-Bold":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.35],34:[0,.69444,0,0,.60278],35:[.19444,.69444,0,0,.95833],36:[.05556,.75,0,0,.575],37:[.05556,.75,0,0,.95833],38:[0,.69444,0,0,.89444],39:[0,.69444,0,0,.31944],40:[.25,.75,0,0,.44722],41:[.25,.75,0,0,.44722],42:[0,.75,0,0,.575],43:[.13333,.63333,0,0,.89444],44:[.19444,.15556,0,0,.31944],45:[0,.44444,0,0,.38333],46:[0,.15556,0,0,.31944],47:[.25,.75,0,0,.575],48:[0,.64444,0,0,.575],49:[0,.64444,0,0,.575],50:[0,.64444,0,0,.575],51:[0,.64444,0,0,.575],52:[0,.64444,0,0,.575],53:[0,.64444,0,0,.575],54:[0,.64444,0,0,.575],55:[0,.64444,0,0,.575],56:[0,.64444,0,0,.575],57:[0,.64444,0,0,.575],58:[0,.44444,0,0,.31944],59:[.19444,.44444,0,0,.31944],60:[.08556,.58556,0,0,.89444],61:[-.10889,.39111,0,0,.89444],62:[.08556,.58556,0,0,.89444],63:[0,.69444,0,0,.54305],64:[0,.69444,0,0,.89444],65:[0,.68611,0,0,.86944],66:[0,.68611,0,0,.81805],67:[0,.68611,0,0,.83055],68:[0,.68611,0,0,.88194],69:[0,.68611,0,0,.75555],70:[0,.68611,0,0,.72361],71:[0,.68611,0,0,.90416],72:[0,.68611,0,0,.9],73:[0,.68611,0,0,.43611],74:[0,.68611,0,0,.59444],75:[0,.68611,0,0,.90138],76:[0,.68611,0,0,.69166],77:[0,.68611,0,0,1.09166],78:[0,.68611,0,0,.9],79:[0,.68611,0,0,.86388],80:[0,.68611,0,0,.78611],81:[.19444,.68611,0,0,.86388],82:[0,.68611,0,0,.8625],83:[0,.68611,0,0,.63889],84:[0,.68611,0,0,.8],85:[0,.68611,0,0,.88472],86:[0,.68611,.01597,0,.86944],87:[0,.68611,.01597,0,1.18888],88:[0,.68611,0,0,.86944],89:[0,.68611,.02875,0,.86944],90:[0,.68611,0,0,.70277],91:[.25,.75,0,0,.31944],92:[.25,.75,0,0,.575],93:[.25,.75,0,0,.31944],94:[0,.69444,0,0,.575],95:[.31,.13444,.03194,0,.575],97:[0,.44444,0,0,.55902],98:[0,.69444,0,0,.63889],99:[0,.44444,0,0,.51111],100:[0,.69444,0,0,.63889],101:[0,.44444,0,0,.52708],102:[0,.69444,.10903,0,.35139],103:[.19444,.44444,.01597,0,.575],104:[0,.69444,0,0,.63889],105:[0,.69444,0,0,.31944],106:[.19444,.69444,0,0,.35139],107:[0,.69444,0,0,.60694],108:[0,.69444,0,0,.31944],109:[0,.44444,0,0,.95833],110:[0,.44444,0,0,.63889],111:[0,.44444,0,0,.575],112:[.19444,.44444,0,0,.63889],113:[.19444,.44444,0,0,.60694],114:[0,.44444,0,0,.47361],115:[0,.44444,0,0,.45361],116:[0,.63492,0,0,.44722],117:[0,.44444,0,0,.63889],118:[0,.44444,.01597,0,.60694],119:[0,.44444,.01597,0,.83055],120:[0,.44444,0,0,.60694],121:[.19444,.44444,.01597,0,.60694],122:[0,.44444,0,0,.51111],123:[.25,.75,0,0,.575],124:[.25,.75,0,0,.31944],125:[.25,.75,0,0,.575],126:[.35,.34444,0,0,.575],160:[0,0,0,0,.25],163:[0,.69444,0,0,.86853],168:[0,.69444,0,0,.575],172:[0,.44444,0,0,.76666],176:[0,.69444,0,0,.86944],177:[.13333,.63333,0,0,.89444],184:[.17014,0,0,0,.51111],198:[0,.68611,0,0,1.04166],215:[.13333,.63333,0,0,.89444],216:[.04861,.73472,0,0,.89444],223:[0,.69444,0,0,.59722],230:[0,.44444,0,0,.83055],247:[.13333,.63333,0,0,.89444],248:[.09722,.54167,0,0,.575],305:[0,.44444,0,0,.31944],338:[0,.68611,0,0,1.16944],339:[0,.44444,0,0,.89444],567:[.19444,.44444,0,0,.35139],710:[0,.69444,0,0,.575],711:[0,.63194,0,0,.575],713:[0,.59611,0,0,.575],714:[0,.69444,0,0,.575],715:[0,.69444,0,0,.575],728:[0,.69444,0,0,.575],729:[0,.69444,0,0,.31944],730:[0,.69444,0,0,.86944],732:[0,.69444,0,0,.575],733:[0,.69444,0,0,.575],915:[0,.68611,0,0,.69166],916:[0,.68611,0,0,.95833],920:[0,.68611,0,0,.89444],923:[0,.68611,0,0,.80555],926:[0,.68611,0,0,.76666],928:[0,.68611,0,0,.9],931:[0,.68611,0,0,.83055],933:[0,.68611,0,0,.89444],934:[0,.68611,0,0,.83055],936:[0,.68611,0,0,.89444],937:[0,.68611,0,0,.83055],8211:[0,.44444,.03194,0,.575],8212:[0,.44444,.03194,0,1.14999],8216:[0,.69444,0,0,.31944],8217:[0,.69444,0,0,.31944],8220:[0,.69444,0,0,.60278],8221:[0,.69444,0,0,.60278],8224:[.19444,.69444,0,0,.51111],8225:[.19444,.69444,0,0,.51111],8242:[0,.55556,0,0,.34444],8407:[0,.72444,.15486,0,.575],8463:[0,.69444,0,0,.66759],8465:[0,.69444,0,0,.83055],8467:[0,.69444,0,0,.47361],8472:[.19444,.44444,0,0,.74027],8476:[0,.69444,0,0,.83055],8501:[0,.69444,0,0,.70277],8592:[-.10889,.39111,0,0,1.14999],8593:[.19444,.69444,0,0,.575],8594:[-.10889,.39111,0,0,1.14999],8595:[.19444,.69444,0,0,.575],8596:[-.10889,.39111,0,0,1.14999],8597:[.25,.75,0,0,.575],8598:[.19444,.69444,0,0,1.14999],8599:[.19444,.69444,0,0,1.14999],8600:[.19444,.69444,0,0,1.14999],8601:[.19444,.69444,0,0,1.14999],8636:[-.10889,.39111,0,0,1.14999],8637:[-.10889,.39111,0,0,1.14999],8640:[-.10889,.39111,0,0,1.14999],8641:[-.10889,.39111,0,0,1.14999],8656:[-.10889,.39111,0,0,1.14999],8657:[.19444,.69444,0,0,.70277],8658:[-.10889,.39111,0,0,1.14999],8659:[.19444,.69444,0,0,.70277],8660:[-.10889,.39111,0,0,1.14999],8661:[.25,.75,0,0,.70277],8704:[0,.69444,0,0,.63889],8706:[0,.69444,.06389,0,.62847],8707:[0,.69444,0,0,.63889],8709:[.05556,.75,0,0,.575],8711:[0,.68611,0,0,.95833],8712:[.08556,.58556,0,0,.76666],8715:[.08556,.58556,0,0,.76666],8722:[.13333,.63333,0,0,.89444],8723:[.13333,.63333,0,0,.89444],8725:[.25,.75,0,0,.575],8726:[.25,.75,0,0,.575],8727:[-.02778,.47222,0,0,.575],8728:[-.02639,.47361,0,0,.575],8729:[-.02639,.47361,0,0,.575],8730:[.18,.82,0,0,.95833],8733:[0,.44444,0,0,.89444],8734:[0,.44444,0,0,1.14999],8736:[0,.69224,0,0,.72222],8739:[.25,.75,0,0,.31944],8741:[.25,.75,0,0,.575],8743:[0,.55556,0,0,.76666],8744:[0,.55556,0,0,.76666],8745:[0,.55556,0,0,.76666],8746:[0,.55556,0,0,.76666],8747:[.19444,.69444,.12778,0,.56875],8764:[-.10889,.39111,0,0,.89444],8768:[.19444,.69444,0,0,.31944],8771:[.00222,.50222,0,0,.89444],8776:[.02444,.52444,0,0,.89444],8781:[.00222,.50222,0,0,.89444],8801:[.00222,.50222,0,0,.89444],8804:[.19667,.69667,0,0,.89444],8805:[.19667,.69667,0,0,.89444],8810:[.08556,.58556,0,0,1.14999],8811:[.08556,.58556,0,0,1.14999],8826:[.08556,.58556,0,0,.89444],8827:[.08556,.58556,0,0,.89444],8834:[.08556,.58556,0,0,.89444],8835:[.08556,.58556,0,0,.89444],8838:[.19667,.69667,0,0,.89444],8839:[.19667,.69667,0,0,.89444],8846:[0,.55556,0,0,.76666],8849:[.19667,.69667,0,0,.89444],8850:[.19667,.69667,0,0,.89444],8851:[0,.55556,0,0,.76666],8852:[0,.55556,0,0,.76666],8853:[.13333,.63333,0,0,.89444],8854:[.13333,.63333,0,0,.89444],8855:[.13333,.63333,0,0,.89444],8856:[.13333,.63333,0,0,.89444],8857:[.13333,.63333,0,0,.89444],8866:[0,.69444,0,0,.70277],8867:[0,.69444,0,0,.70277],8868:[0,.69444,0,0,.89444],8869:[0,.69444,0,0,.89444],8900:[-.02639,.47361,0,0,.575],8901:[-.02639,.47361,0,0,.31944],8902:[-.02778,.47222,0,0,.575],8968:[.25,.75,0,0,.51111],8969:[.25,.75,0,0,.51111],8970:[.25,.75,0,0,.51111],8971:[.25,.75,0,0,.51111],8994:[-.13889,.36111,0,0,1.14999],8995:[-.13889,.36111,0,0,1.14999],9651:[.19444,.69444,0,0,1.02222],9657:[-.02778,.47222,0,0,.575],9661:[.19444,.69444,0,0,1.02222],9667:[-.02778,.47222,0,0,.575],9711:[.19444,.69444,0,0,1.14999],9824:[.12963,.69444,0,0,.89444],9825:[.12963,.69444,0,0,.89444],9826:[.12963,.69444,0,0,.89444],9827:[.12963,.69444,0,0,.89444],9837:[0,.75,0,0,.44722],9838:[.19444,.69444,0,0,.44722],9839:[.19444,.69444,0,0,.44722],10216:[.25,.75,0,0,.44722],10217:[.25,.75,0,0,.44722],10815:[0,.68611,0,0,.9],10927:[.19667,.69667,0,0,.89444],10928:[.19667,.69667,0,0,.89444],57376:[.19444,.69444,0,0,0]},"Main-BoldItalic":{32:[0,0,0,0,.25],33:[0,.69444,.11417,0,.38611],34:[0,.69444,.07939,0,.62055],35:[.19444,.69444,.06833,0,.94444],37:[.05556,.75,.12861,0,.94444],38:[0,.69444,.08528,0,.88555],39:[0,.69444,.12945,0,.35555],40:[.25,.75,.15806,0,.47333],41:[.25,.75,.03306,0,.47333],42:[0,.75,.14333,0,.59111],43:[.10333,.60333,.03306,0,.88555],44:[.19444,.14722,0,0,.35555],45:[0,.44444,.02611,0,.41444],46:[0,.14722,0,0,.35555],47:[.25,.75,.15806,0,.59111],48:[0,.64444,.13167,0,.59111],49:[0,.64444,.13167,0,.59111],50:[0,.64444,.13167,0,.59111],51:[0,.64444,.13167,0,.59111],52:[.19444,.64444,.13167,0,.59111],53:[0,.64444,.13167,0,.59111],54:[0,.64444,.13167,0,.59111],55:[.19444,.64444,.13167,0,.59111],56:[0,.64444,.13167,0,.59111],57:[0,.64444,.13167,0,.59111],58:[0,.44444,.06695,0,.35555],59:[.19444,.44444,.06695,0,.35555],61:[-.10889,.39111,.06833,0,.88555],63:[0,.69444,.11472,0,.59111],64:[0,.69444,.09208,0,.88555],65:[0,.68611,0,0,.86555],66:[0,.68611,.0992,0,.81666],67:[0,.68611,.14208,0,.82666],68:[0,.68611,.09062,0,.87555],69:[0,.68611,.11431,0,.75666],70:[0,.68611,.12903,0,.72722],71:[0,.68611,.07347,0,.89527],72:[0,.68611,.17208,0,.8961],73:[0,.68611,.15681,0,.47166],74:[0,.68611,.145,0,.61055],75:[0,.68611,.14208,0,.89499],76:[0,.68611,0,0,.69777],77:[0,.68611,.17208,0,1.07277],78:[0,.68611,.17208,0,.8961],79:[0,.68611,.09062,0,.85499],80:[0,.68611,.0992,0,.78721],81:[.19444,.68611,.09062,0,.85499],82:[0,.68611,.02559,0,.85944],83:[0,.68611,.11264,0,.64999],84:[0,.68611,.12903,0,.7961],85:[0,.68611,.17208,0,.88083],86:[0,.68611,.18625,0,.86555],87:[0,.68611,.18625,0,1.15999],88:[0,.68611,.15681,0,.86555],89:[0,.68611,.19803,0,.86555],90:[0,.68611,.14208,0,.70888],91:[.25,.75,.1875,0,.35611],93:[.25,.75,.09972,0,.35611],94:[0,.69444,.06709,0,.59111],95:[.31,.13444,.09811,0,.59111],97:[0,.44444,.09426,0,.59111],98:[0,.69444,.07861,0,.53222],99:[0,.44444,.05222,0,.53222],100:[0,.69444,.10861,0,.59111],101:[0,.44444,.085,0,.53222],102:[.19444,.69444,.21778,0,.4],103:[.19444,.44444,.105,0,.53222],104:[0,.69444,.09426,0,.59111],105:[0,.69326,.11387,0,.35555],106:[.19444,.69326,.1672,0,.35555],107:[0,.69444,.11111,0,.53222],108:[0,.69444,.10861,0,.29666],109:[0,.44444,.09426,0,.94444],110:[0,.44444,.09426,0,.64999],111:[0,.44444,.07861,0,.59111],112:[.19444,.44444,.07861,0,.59111],113:[.19444,.44444,.105,0,.53222],114:[0,.44444,.11111,0,.50167],115:[0,.44444,.08167,0,.48694],116:[0,.63492,.09639,0,.385],117:[0,.44444,.09426,0,.62055],118:[0,.44444,.11111,0,.53222],119:[0,.44444,.11111,0,.76777],120:[0,.44444,.12583,0,.56055],121:[.19444,.44444,.105,0,.56166],122:[0,.44444,.13889,0,.49055],126:[.35,.34444,.11472,0,.59111],160:[0,0,0,0,.25],168:[0,.69444,.11473,0,.59111],176:[0,.69444,0,0,.94888],184:[.17014,0,0,0,.53222],198:[0,.68611,.11431,0,1.02277],216:[.04861,.73472,.09062,0,.88555],223:[.19444,.69444,.09736,0,.665],230:[0,.44444,.085,0,.82666],248:[.09722,.54167,.09458,0,.59111],305:[0,.44444,.09426,0,.35555],338:[0,.68611,.11431,0,1.14054],339:[0,.44444,.085,0,.82666],567:[.19444,.44444,.04611,0,.385],710:[0,.69444,.06709,0,.59111],711:[0,.63194,.08271,0,.59111],713:[0,.59444,.10444,0,.59111],714:[0,.69444,.08528,0,.59111],715:[0,.69444,0,0,.59111],728:[0,.69444,.10333,0,.59111],729:[0,.69444,.12945,0,.35555],730:[0,.69444,0,0,.94888],732:[0,.69444,.11472,0,.59111],733:[0,.69444,.11472,0,.59111],915:[0,.68611,.12903,0,.69777],916:[0,.68611,0,0,.94444],920:[0,.68611,.09062,0,.88555],923:[0,.68611,0,0,.80666],926:[0,.68611,.15092,0,.76777],928:[0,.68611,.17208,0,.8961],931:[0,.68611,.11431,0,.82666],933:[0,.68611,.10778,0,.88555],934:[0,.68611,.05632,0,.82666],936:[0,.68611,.10778,0,.88555],937:[0,.68611,.0992,0,.82666],8211:[0,.44444,.09811,0,.59111],8212:[0,.44444,.09811,0,1.18221],8216:[0,.69444,.12945,0,.35555],8217:[0,.69444,.12945,0,.35555],8220:[0,.69444,.16772,0,.62055],8221:[0,.69444,.07939,0,.62055]},"Main-Italic":{32:[0,0,0,0,.25],33:[0,.69444,.12417,0,.30667],34:[0,.69444,.06961,0,.51444],35:[.19444,.69444,.06616,0,.81777],37:[.05556,.75,.13639,0,.81777],38:[0,.69444,.09694,0,.76666],39:[0,.69444,.12417,0,.30667],40:[.25,.75,.16194,0,.40889],41:[.25,.75,.03694,0,.40889],42:[0,.75,.14917,0,.51111],43:[.05667,.56167,.03694,0,.76666],44:[.19444,.10556,0,0,.30667],45:[0,.43056,.02826,0,.35778],46:[0,.10556,0,0,.30667],47:[.25,.75,.16194,0,.51111],48:[0,.64444,.13556,0,.51111],49:[0,.64444,.13556,0,.51111],50:[0,.64444,.13556,0,.51111],51:[0,.64444,.13556,0,.51111],52:[.19444,.64444,.13556,0,.51111],53:[0,.64444,.13556,0,.51111],54:[0,.64444,.13556,0,.51111],55:[.19444,.64444,.13556,0,.51111],56:[0,.64444,.13556,0,.51111],57:[0,.64444,.13556,0,.51111],58:[0,.43056,.0582,0,.30667],59:[.19444,.43056,.0582,0,.30667],61:[-.13313,.36687,.06616,0,.76666],63:[0,.69444,.1225,0,.51111],64:[0,.69444,.09597,0,.76666],65:[0,.68333,0,0,.74333],66:[0,.68333,.10257,0,.70389],67:[0,.68333,.14528,0,.71555],68:[0,.68333,.09403,0,.755],69:[0,.68333,.12028,0,.67833],70:[0,.68333,.13305,0,.65277],71:[0,.68333,.08722,0,.77361],72:[0,.68333,.16389,0,.74333],73:[0,.68333,.15806,0,.38555],74:[0,.68333,.14028,0,.525],75:[0,.68333,.14528,0,.76888],76:[0,.68333,0,0,.62722],77:[0,.68333,.16389,0,.89666],78:[0,.68333,.16389,0,.74333],79:[0,.68333,.09403,0,.76666],80:[0,.68333,.10257,0,.67833],81:[.19444,.68333,.09403,0,.76666],82:[0,.68333,.03868,0,.72944],83:[0,.68333,.11972,0,.56222],84:[0,.68333,.13305,0,.71555],85:[0,.68333,.16389,0,.74333],86:[0,.68333,.18361,0,.74333],87:[0,.68333,.18361,0,.99888],88:[0,.68333,.15806,0,.74333],89:[0,.68333,.19383,0,.74333],90:[0,.68333,.14528,0,.61333],91:[.25,.75,.1875,0,.30667],93:[.25,.75,.10528,0,.30667],94:[0,.69444,.06646,0,.51111],95:[.31,.12056,.09208,0,.51111],97:[0,.43056,.07671,0,.51111],98:[0,.69444,.06312,0,.46],99:[0,.43056,.05653,0,.46],100:[0,.69444,.10333,0,.51111],101:[0,.43056,.07514,0,.46],102:[.19444,.69444,.21194,0,.30667],103:[.19444,.43056,.08847,0,.46],104:[0,.69444,.07671,0,.51111],105:[0,.65536,.1019,0,.30667],106:[.19444,.65536,.14467,0,.30667],107:[0,.69444,.10764,0,.46],108:[0,.69444,.10333,0,.25555],109:[0,.43056,.07671,0,.81777],110:[0,.43056,.07671,0,.56222],111:[0,.43056,.06312,0,.51111],112:[.19444,.43056,.06312,0,.51111],113:[.19444,.43056,.08847,0,.46],114:[0,.43056,.10764,0,.42166],115:[0,.43056,.08208,0,.40889],116:[0,.61508,.09486,0,.33222],117:[0,.43056,.07671,0,.53666],118:[0,.43056,.10764,0,.46],119:[0,.43056,.10764,0,.66444],120:[0,.43056,.12042,0,.46389],121:[.19444,.43056,.08847,0,.48555],122:[0,.43056,.12292,0,.40889],126:[.35,.31786,.11585,0,.51111],160:[0,0,0,0,.25],168:[0,.66786,.10474,0,.51111],176:[0,.69444,0,0,.83129],184:[.17014,0,0,0,.46],198:[0,.68333,.12028,0,.88277],216:[.04861,.73194,.09403,0,.76666],223:[.19444,.69444,.10514,0,.53666],230:[0,.43056,.07514,0,.71555],248:[.09722,.52778,.09194,0,.51111],338:[0,.68333,.12028,0,.98499],339:[0,.43056,.07514,0,.71555],710:[0,.69444,.06646,0,.51111],711:[0,.62847,.08295,0,.51111],713:[0,.56167,.10333,0,.51111],714:[0,.69444,.09694,0,.51111],715:[0,.69444,0,0,.51111],728:[0,.69444,.10806,0,.51111],729:[0,.66786,.11752,0,.30667],730:[0,.69444,0,0,.83129],732:[0,.66786,.11585,0,.51111],733:[0,.69444,.1225,0,.51111],915:[0,.68333,.13305,0,.62722],916:[0,.68333,0,0,.81777],920:[0,.68333,.09403,0,.76666],923:[0,.68333,0,0,.69222],926:[0,.68333,.15294,0,.66444],928:[0,.68333,.16389,0,.74333],931:[0,.68333,.12028,0,.71555],933:[0,.68333,.11111,0,.76666],934:[0,.68333,.05986,0,.71555],936:[0,.68333,.11111,0,.76666],937:[0,.68333,.10257,0,.71555],8211:[0,.43056,.09208,0,.51111],8212:[0,.43056,.09208,0,1.02222],8216:[0,.69444,.12417,0,.30667],8217:[0,.69444,.12417,0,.30667],8220:[0,.69444,.1685,0,.51444],8221:[0,.69444,.06961,0,.51444],8463:[0,.68889,0,0,.54028]},"Main-Regular":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.27778],34:[0,.69444,0,0,.5],35:[.19444,.69444,0,0,.83334],36:[.05556,.75,0,0,.5],37:[.05556,.75,0,0,.83334],38:[0,.69444,0,0,.77778],39:[0,.69444,0,0,.27778],40:[.25,.75,0,0,.38889],41:[.25,.75,0,0,.38889],42:[0,.75,0,0,.5],43:[.08333,.58333,0,0,.77778],44:[.19444,.10556,0,0,.27778],45:[0,.43056,0,0,.33333],46:[0,.10556,0,0,.27778],47:[.25,.75,0,0,.5],48:[0,.64444,0,0,.5],49:[0,.64444,0,0,.5],50:[0,.64444,0,0,.5],51:[0,.64444,0,0,.5],52:[0,.64444,0,0,.5],53:[0,.64444,0,0,.5],54:[0,.64444,0,0,.5],55:[0,.64444,0,0,.5],56:[0,.64444,0,0,.5],57:[0,.64444,0,0,.5],58:[0,.43056,0,0,.27778],59:[.19444,.43056,0,0,.27778],60:[.0391,.5391,0,0,.77778],61:[-.13313,.36687,0,0,.77778],62:[.0391,.5391,0,0,.77778],63:[0,.69444,0,0,.47222],64:[0,.69444,0,0,.77778],65:[0,.68333,0,0,.75],66:[0,.68333,0,0,.70834],67:[0,.68333,0,0,.72222],68:[0,.68333,0,0,.76389],69:[0,.68333,0,0,.68056],70:[0,.68333,0,0,.65278],71:[0,.68333,0,0,.78472],72:[0,.68333,0,0,.75],73:[0,.68333,0,0,.36111],74:[0,.68333,0,0,.51389],75:[0,.68333,0,0,.77778],76:[0,.68333,0,0,.625],77:[0,.68333,0,0,.91667],78:[0,.68333,0,0,.75],79:[0,.68333,0,0,.77778],80:[0,.68333,0,0,.68056],81:[.19444,.68333,0,0,.77778],82:[0,.68333,0,0,.73611],83:[0,.68333,0,0,.55556],84:[0,.68333,0,0,.72222],85:[0,.68333,0,0,.75],86:[0,.68333,.01389,0,.75],87:[0,.68333,.01389,0,1.02778],88:[0,.68333,0,0,.75],89:[0,.68333,.025,0,.75],90:[0,.68333,0,0,.61111],91:[.25,.75,0,0,.27778],92:[.25,.75,0,0,.5],93:[.25,.75,0,0,.27778],94:[0,.69444,0,0,.5],95:[.31,.12056,.02778,0,.5],97:[0,.43056,0,0,.5],98:[0,.69444,0,0,.55556],99:[0,.43056,0,0,.44445],100:[0,.69444,0,0,.55556],101:[0,.43056,0,0,.44445],102:[0,.69444,.07778,0,.30556],103:[.19444,.43056,.01389,0,.5],104:[0,.69444,0,0,.55556],105:[0,.66786,0,0,.27778],106:[.19444,.66786,0,0,.30556],107:[0,.69444,0,0,.52778],108:[0,.69444,0,0,.27778],109:[0,.43056,0,0,.83334],110:[0,.43056,0,0,.55556],111:[0,.43056,0,0,.5],112:[.19444,.43056,0,0,.55556],113:[.19444,.43056,0,0,.52778],114:[0,.43056,0,0,.39167],115:[0,.43056,0,0,.39445],116:[0,.61508,0,0,.38889],117:[0,.43056,0,0,.55556],118:[0,.43056,.01389,0,.52778],119:[0,.43056,.01389,0,.72222],120:[0,.43056,0,0,.52778],121:[.19444,.43056,.01389,0,.52778],122:[0,.43056,0,0,.44445],123:[.25,.75,0,0,.5],124:[.25,.75,0,0,.27778],125:[.25,.75,0,0,.5],126:[.35,.31786,0,0,.5],160:[0,0,0,0,.25],163:[0,.69444,0,0,.76909],167:[.19444,.69444,0,0,.44445],168:[0,.66786,0,0,.5],172:[0,.43056,0,0,.66667],176:[0,.69444,0,0,.75],177:[.08333,.58333,0,0,.77778],182:[.19444,.69444,0,0,.61111],184:[.17014,0,0,0,.44445],198:[0,.68333,0,0,.90278],215:[.08333,.58333,0,0,.77778],216:[.04861,.73194,0,0,.77778],223:[0,.69444,0,0,.5],230:[0,.43056,0,0,.72222],247:[.08333,.58333,0,0,.77778],248:[.09722,.52778,0,0,.5],305:[0,.43056,0,0,.27778],338:[0,.68333,0,0,1.01389],339:[0,.43056,0,0,.77778],567:[.19444,.43056,0,0,.30556],710:[0,.69444,0,0,.5],711:[0,.62847,0,0,.5],713:[0,.56778,0,0,.5],714:[0,.69444,0,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,0,0,.5],729:[0,.66786,0,0,.27778],730:[0,.69444,0,0,.75],732:[0,.66786,0,0,.5],733:[0,.69444,0,0,.5],915:[0,.68333,0,0,.625],916:[0,.68333,0,0,.83334],920:[0,.68333,0,0,.77778],923:[0,.68333,0,0,.69445],926:[0,.68333,0,0,.66667],928:[0,.68333,0,0,.75],931:[0,.68333,0,0,.72222],933:[0,.68333,0,0,.77778],934:[0,.68333,0,0,.72222],936:[0,.68333,0,0,.77778],937:[0,.68333,0,0,.72222],8211:[0,.43056,.02778,0,.5],8212:[0,.43056,.02778,0,1],8216:[0,.69444,0,0,.27778],8217:[0,.69444,0,0,.27778],8220:[0,.69444,0,0,.5],8221:[0,.69444,0,0,.5],8224:[.19444,.69444,0,0,.44445],8225:[.19444,.69444,0,0,.44445],8230:[0,.12,0,0,1.172],8242:[0,.55556,0,0,.275],8407:[0,.71444,.15382,0,.5],8463:[0,.68889,0,0,.54028],8465:[0,.69444,0,0,.72222],8467:[0,.69444,0,.11111,.41667],8472:[.19444,.43056,0,.11111,.63646],8476:[0,.69444,0,0,.72222],8501:[0,.69444,0,0,.61111],8592:[-.13313,.36687,0,0,1],8593:[.19444,.69444,0,0,.5],8594:[-.13313,.36687,0,0,1],8595:[.19444,.69444,0,0,.5],8596:[-.13313,.36687,0,0,1],8597:[.25,.75,0,0,.5],8598:[.19444,.69444,0,0,1],8599:[.19444,.69444,0,0,1],8600:[.19444,.69444,0,0,1],8601:[.19444,.69444,0,0,1],8614:[.011,.511,0,0,1],8617:[.011,.511,0,0,1.126],8618:[.011,.511,0,0,1.126],8636:[-.13313,.36687,0,0,1],8637:[-.13313,.36687,0,0,1],8640:[-.13313,.36687,0,0,1],8641:[-.13313,.36687,0,0,1],8652:[.011,.671,0,0,1],8656:[-.13313,.36687,0,0,1],8657:[.19444,.69444,0,0,.61111],8658:[-.13313,.36687,0,0,1],8659:[.19444,.69444,0,0,.61111],8660:[-.13313,.36687,0,0,1],8661:[.25,.75,0,0,.61111],8704:[0,.69444,0,0,.55556],8706:[0,.69444,.05556,.08334,.5309],8707:[0,.69444,0,0,.55556],8709:[.05556,.75,0,0,.5],8711:[0,.68333,0,0,.83334],8712:[.0391,.5391,0,0,.66667],8715:[.0391,.5391,0,0,.66667],8722:[.08333,.58333,0,0,.77778],8723:[.08333,.58333,0,0,.77778],8725:[.25,.75,0,0,.5],8726:[.25,.75,0,0,.5],8727:[-.03472,.46528,0,0,.5],8728:[-.05555,.44445,0,0,.5],8729:[-.05555,.44445,0,0,.5],8730:[.2,.8,0,0,.83334],8733:[0,.43056,0,0,.77778],8734:[0,.43056,0,0,1],8736:[0,.69224,0,0,.72222],8739:[.25,.75,0,0,.27778],8741:[.25,.75,0,0,.5],8743:[0,.55556,0,0,.66667],8744:[0,.55556,0,0,.66667],8745:[0,.55556,0,0,.66667],8746:[0,.55556,0,0,.66667],8747:[.19444,.69444,.11111,0,.41667],8764:[-.13313,.36687,0,0,.77778],8768:[.19444,.69444,0,0,.27778],8771:[-.03625,.46375,0,0,.77778],8773:[-.022,.589,0,0,1],8776:[-.01688,.48312,0,0,.77778],8781:[-.03625,.46375,0,0,.77778],8784:[-.133,.67,0,0,.778],8801:[-.03625,.46375,0,0,.77778],8804:[.13597,.63597,0,0,.77778],8805:[.13597,.63597,0,0,.77778],8810:[.0391,.5391,0,0,1],8811:[.0391,.5391,0,0,1],8826:[.0391,.5391,0,0,.77778],8827:[.0391,.5391,0,0,.77778],8834:[.0391,.5391,0,0,.77778],8835:[.0391,.5391,0,0,.77778],8838:[.13597,.63597,0,0,.77778],8839:[.13597,.63597,0,0,.77778],8846:[0,.55556,0,0,.66667],8849:[.13597,.63597,0,0,.77778],8850:[.13597,.63597,0,0,.77778],8851:[0,.55556,0,0,.66667],8852:[0,.55556,0,0,.66667],8853:[.08333,.58333,0,0,.77778],8854:[.08333,.58333,0,0,.77778],8855:[.08333,.58333,0,0,.77778],8856:[.08333,.58333,0,0,.77778],8857:[.08333,.58333,0,0,.77778],8866:[0,.69444,0,0,.61111],8867:[0,.69444,0,0,.61111],8868:[0,.69444,0,0,.77778],8869:[0,.69444,0,0,.77778],8872:[.249,.75,0,0,.867],8900:[-.05555,.44445,0,0,.5],8901:[-.05555,.44445,0,0,.27778],8902:[-.03472,.46528,0,0,.5],8904:[.005,.505,0,0,.9],8942:[.03,.9,0,0,.278],8943:[-.19,.31,0,0,1.172],8945:[-.1,.82,0,0,1.282],8968:[.25,.75,0,0,.44445],8969:[.25,.75,0,0,.44445],8970:[.25,.75,0,0,.44445],8971:[.25,.75,0,0,.44445],8994:[-.14236,.35764,0,0,1],8995:[-.14236,.35764,0,0,1],9136:[.244,.744,0,0,.412],9137:[.244,.744,0,0,.412],9651:[.19444,.69444,0,0,.88889],9657:[-.03472,.46528,0,0,.5],9661:[.19444,.69444,0,0,.88889],9667:[-.03472,.46528,0,0,.5],9711:[.19444,.69444,0,0,1],9824:[.12963,.69444,0,0,.77778],9825:[.12963,.69444,0,0,.77778],9826:[.12963,.69444,0,0,.77778],9827:[.12963,.69444,0,0,.77778],9837:[0,.75,0,0,.38889],9838:[.19444,.69444,0,0,.38889],9839:[.19444,.69444,0,0,.38889],10216:[.25,.75,0,0,.38889],10217:[.25,.75,0,0,.38889],10222:[.244,.744,0,0,.412],10223:[.244,.744,0,0,.412],10229:[.011,.511,0,0,1.609],10230:[.011,.511,0,0,1.638],10231:[.011,.511,0,0,1.859],10232:[.024,.525,0,0,1.609],10233:[.024,.525,0,0,1.638],10234:[.024,.525,0,0,1.858],10236:[.011,.511,0,0,1.638],10815:[0,.68333,0,0,.75],10927:[.13597,.63597,0,0,.77778],10928:[.13597,.63597,0,0,.77778],57376:[.19444,.69444,0,0,0]},"Math-BoldItalic":{32:[0,0,0,0,.25],48:[0,.44444,0,0,.575],49:[0,.44444,0,0,.575],50:[0,.44444,0,0,.575],51:[.19444,.44444,0,0,.575],52:[.19444,.44444,0,0,.575],53:[.19444,.44444,0,0,.575],54:[0,.64444,0,0,.575],55:[.19444,.44444,0,0,.575],56:[0,.64444,0,0,.575],57:[.19444,.44444,0,0,.575],65:[0,.68611,0,0,.86944],66:[0,.68611,.04835,0,.8664],67:[0,.68611,.06979,0,.81694],68:[0,.68611,.03194,0,.93812],69:[0,.68611,.05451,0,.81007],70:[0,.68611,.15972,0,.68889],71:[0,.68611,0,0,.88673],72:[0,.68611,.08229,0,.98229],73:[0,.68611,.07778,0,.51111],74:[0,.68611,.10069,0,.63125],75:[0,.68611,.06979,0,.97118],76:[0,.68611,0,0,.75555],77:[0,.68611,.11424,0,1.14201],78:[0,.68611,.11424,0,.95034],79:[0,.68611,.03194,0,.83666],80:[0,.68611,.15972,0,.72309],81:[.19444,.68611,0,0,.86861],82:[0,.68611,.00421,0,.87235],83:[0,.68611,.05382,0,.69271],84:[0,.68611,.15972,0,.63663],85:[0,.68611,.11424,0,.80027],86:[0,.68611,.25555,0,.67778],87:[0,.68611,.15972,0,1.09305],88:[0,.68611,.07778,0,.94722],89:[0,.68611,.25555,0,.67458],90:[0,.68611,.06979,0,.77257],97:[0,.44444,0,0,.63287],98:[0,.69444,0,0,.52083],99:[0,.44444,0,0,.51342],100:[0,.69444,0,0,.60972],101:[0,.44444,0,0,.55361],102:[.19444,.69444,.11042,0,.56806],103:[.19444,.44444,.03704,0,.5449],104:[0,.69444,0,0,.66759],105:[0,.69326,0,0,.4048],106:[.19444,.69326,.0622,0,.47083],107:[0,.69444,.01852,0,.6037],108:[0,.69444,.0088,0,.34815],109:[0,.44444,0,0,1.0324],110:[0,.44444,0,0,.71296],111:[0,.44444,0,0,.58472],112:[.19444,.44444,0,0,.60092],113:[.19444,.44444,.03704,0,.54213],114:[0,.44444,.03194,0,.5287],115:[0,.44444,0,0,.53125],116:[0,.63492,0,0,.41528],117:[0,.44444,0,0,.68102],118:[0,.44444,.03704,0,.56666],119:[0,.44444,.02778,0,.83148],120:[0,.44444,0,0,.65903],121:[.19444,.44444,.03704,0,.59028],122:[0,.44444,.04213,0,.55509],160:[0,0,0,0,.25],915:[0,.68611,.15972,0,.65694],916:[0,.68611,0,0,.95833],920:[0,.68611,.03194,0,.86722],923:[0,.68611,0,0,.80555],926:[0,.68611,.07458,0,.84125],928:[0,.68611,.08229,0,.98229],931:[0,.68611,.05451,0,.88507],933:[0,.68611,.15972,0,.67083],934:[0,.68611,0,0,.76666],936:[0,.68611,.11653,0,.71402],937:[0,.68611,.04835,0,.8789],945:[0,.44444,0,0,.76064],946:[.19444,.69444,.03403,0,.65972],947:[.19444,.44444,.06389,0,.59003],948:[0,.69444,.03819,0,.52222],949:[0,.44444,0,0,.52882],950:[.19444,.69444,.06215,0,.50833],951:[.19444,.44444,.03704,0,.6],952:[0,.69444,.03194,0,.5618],953:[0,.44444,0,0,.41204],954:[0,.44444,0,0,.66759],955:[0,.69444,0,0,.67083],956:[.19444,.44444,0,0,.70787],957:[0,.44444,.06898,0,.57685],958:[.19444,.69444,.03021,0,.50833],959:[0,.44444,0,0,.58472],960:[0,.44444,.03704,0,.68241],961:[.19444,.44444,0,0,.6118],962:[.09722,.44444,.07917,0,.42361],963:[0,.44444,.03704,0,.68588],964:[0,.44444,.13472,0,.52083],965:[0,.44444,.03704,0,.63055],966:[.19444,.44444,0,0,.74722],967:[.19444,.44444,0,0,.71805],968:[.19444,.69444,.03704,0,.75833],969:[0,.44444,.03704,0,.71782],977:[0,.69444,0,0,.69155],981:[.19444,.69444,0,0,.7125],982:[0,.44444,.03194,0,.975],1009:[.19444,.44444,0,0,.6118],1013:[0,.44444,0,0,.48333],57649:[0,.44444,0,0,.39352],57911:[.19444,.44444,0,0,.43889]},"Math-Italic":{32:[0,0,0,0,.25],48:[0,.43056,0,0,.5],49:[0,.43056,0,0,.5],50:[0,.43056,0,0,.5],51:[.19444,.43056,0,0,.5],52:[.19444,.43056,0,0,.5],53:[.19444,.43056,0,0,.5],54:[0,.64444,0,0,.5],55:[.19444,.43056,0,0,.5],56:[0,.64444,0,0,.5],57:[.19444,.43056,0,0,.5],65:[0,.68333,0,.13889,.75],66:[0,.68333,.05017,.08334,.75851],67:[0,.68333,.07153,.08334,.71472],68:[0,.68333,.02778,.05556,.82792],69:[0,.68333,.05764,.08334,.7382],70:[0,.68333,.13889,.08334,.64306],71:[0,.68333,0,.08334,.78625],72:[0,.68333,.08125,.05556,.83125],73:[0,.68333,.07847,.11111,.43958],74:[0,.68333,.09618,.16667,.55451],75:[0,.68333,.07153,.05556,.84931],76:[0,.68333,0,.02778,.68056],77:[0,.68333,.10903,.08334,.97014],78:[0,.68333,.10903,.08334,.80347],79:[0,.68333,.02778,.08334,.76278],80:[0,.68333,.13889,.08334,.64201],81:[.19444,.68333,0,.08334,.79056],82:[0,.68333,.00773,.08334,.75929],83:[0,.68333,.05764,.08334,.6132],84:[0,.68333,.13889,.08334,.58438],85:[0,.68333,.10903,.02778,.68278],86:[0,.68333,.22222,0,.58333],87:[0,.68333,.13889,0,.94445],88:[0,.68333,.07847,.08334,.82847],89:[0,.68333,.22222,0,.58056],90:[0,.68333,.07153,.08334,.68264],97:[0,.43056,0,0,.52859],98:[0,.69444,0,0,.42917],99:[0,.43056,0,.05556,.43276],100:[0,.69444,0,.16667,.52049],101:[0,.43056,0,.05556,.46563],102:[.19444,.69444,.10764,.16667,.48959],103:[.19444,.43056,.03588,.02778,.47697],104:[0,.69444,0,0,.57616],105:[0,.65952,0,0,.34451],106:[.19444,.65952,.05724,0,.41181],107:[0,.69444,.03148,0,.5206],108:[0,.69444,.01968,.08334,.29838],109:[0,.43056,0,0,.87801],110:[0,.43056,0,0,.60023],111:[0,.43056,0,.05556,.48472],112:[.19444,.43056,0,.08334,.50313],113:[.19444,.43056,.03588,.08334,.44641],114:[0,.43056,.02778,.05556,.45116],115:[0,.43056,0,.05556,.46875],116:[0,.61508,0,.08334,.36111],117:[0,.43056,0,.02778,.57246],118:[0,.43056,.03588,.02778,.48472],119:[0,.43056,.02691,.08334,.71592],120:[0,.43056,0,.02778,.57153],121:[.19444,.43056,.03588,.05556,.49028],122:[0,.43056,.04398,.05556,.46505],160:[0,0,0,0,.25],915:[0,.68333,.13889,.08334,.61528],916:[0,.68333,0,.16667,.83334],920:[0,.68333,.02778,.08334,.76278],923:[0,.68333,0,.16667,.69445],926:[0,.68333,.07569,.08334,.74236],928:[0,.68333,.08125,.05556,.83125],931:[0,.68333,.05764,.08334,.77986],933:[0,.68333,.13889,.05556,.58333],934:[0,.68333,0,.08334,.66667],936:[0,.68333,.11,.05556,.61222],937:[0,.68333,.05017,.08334,.7724],945:[0,.43056,.0037,.02778,.6397],946:[.19444,.69444,.05278,.08334,.56563],947:[.19444,.43056,.05556,0,.51773],948:[0,.69444,.03785,.05556,.44444],949:[0,.43056,0,.08334,.46632],950:[.19444,.69444,.07378,.08334,.4375],951:[.19444,.43056,.03588,.05556,.49653],952:[0,.69444,.02778,.08334,.46944],953:[0,.43056,0,.05556,.35394],954:[0,.43056,0,0,.57616],955:[0,.69444,0,0,.58334],956:[.19444,.43056,0,.02778,.60255],957:[0,.43056,.06366,.02778,.49398],958:[.19444,.69444,.04601,.11111,.4375],959:[0,.43056,0,.05556,.48472],960:[0,.43056,.03588,0,.57003],961:[.19444,.43056,0,.08334,.51702],962:[.09722,.43056,.07986,.08334,.36285],963:[0,.43056,.03588,0,.57141],964:[0,.43056,.1132,.02778,.43715],965:[0,.43056,.03588,.02778,.54028],966:[.19444,.43056,0,.08334,.65417],967:[.19444,.43056,0,.05556,.62569],968:[.19444,.69444,.03588,.11111,.65139],969:[0,.43056,.03588,0,.62245],977:[0,.69444,0,.08334,.59144],981:[.19444,.69444,0,.08334,.59583],982:[0,.43056,.02778,0,.82813],1009:[.19444,.43056,0,.08334,.51702],1013:[0,.43056,0,.05556,.4059],57649:[0,.43056,0,.02778,.32246],57911:[.19444,.43056,0,.08334,.38403]},"SansSerif-Bold":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.36667],34:[0,.69444,0,0,.55834],35:[.19444,.69444,0,0,.91667],36:[.05556,.75,0,0,.55],37:[.05556,.75,0,0,1.02912],38:[0,.69444,0,0,.83056],39:[0,.69444,0,0,.30556],40:[.25,.75,0,0,.42778],41:[.25,.75,0,0,.42778],42:[0,.75,0,0,.55],43:[.11667,.61667,0,0,.85556],44:[.10556,.13056,0,0,.30556],45:[0,.45833,0,0,.36667],46:[0,.13056,0,0,.30556],47:[.25,.75,0,0,.55],48:[0,.69444,0,0,.55],49:[0,.69444,0,0,.55],50:[0,.69444,0,0,.55],51:[0,.69444,0,0,.55],52:[0,.69444,0,0,.55],53:[0,.69444,0,0,.55],54:[0,.69444,0,0,.55],55:[0,.69444,0,0,.55],56:[0,.69444,0,0,.55],57:[0,.69444,0,0,.55],58:[0,.45833,0,0,.30556],59:[.10556,.45833,0,0,.30556],61:[-.09375,.40625,0,0,.85556],63:[0,.69444,0,0,.51945],64:[0,.69444,0,0,.73334],65:[0,.69444,0,0,.73334],66:[0,.69444,0,0,.73334],67:[0,.69444,0,0,.70278],68:[0,.69444,0,0,.79445],69:[0,.69444,0,0,.64167],70:[0,.69444,0,0,.61111],71:[0,.69444,0,0,.73334],72:[0,.69444,0,0,.79445],73:[0,.69444,0,0,.33056],74:[0,.69444,0,0,.51945],75:[0,.69444,0,0,.76389],76:[0,.69444,0,0,.58056],77:[0,.69444,0,0,.97778],78:[0,.69444,0,0,.79445],79:[0,.69444,0,0,.79445],80:[0,.69444,0,0,.70278],81:[.10556,.69444,0,0,.79445],82:[0,.69444,0,0,.70278],83:[0,.69444,0,0,.61111],84:[0,.69444,0,0,.73334],85:[0,.69444,0,0,.76389],86:[0,.69444,.01528,0,.73334],87:[0,.69444,.01528,0,1.03889],88:[0,.69444,0,0,.73334],89:[0,.69444,.0275,0,.73334],90:[0,.69444,0,0,.67223],91:[.25,.75,0,0,.34306],93:[.25,.75,0,0,.34306],94:[0,.69444,0,0,.55],95:[.35,.10833,.03056,0,.55],97:[0,.45833,0,0,.525],98:[0,.69444,0,0,.56111],99:[0,.45833,0,0,.48889],100:[0,.69444,0,0,.56111],101:[0,.45833,0,0,.51111],102:[0,.69444,.07639,0,.33611],103:[.19444,.45833,.01528,0,.55],104:[0,.69444,0,0,.56111],105:[0,.69444,0,0,.25556],106:[.19444,.69444,0,0,.28611],107:[0,.69444,0,0,.53056],108:[0,.69444,0,0,.25556],109:[0,.45833,0,0,.86667],110:[0,.45833,0,0,.56111],111:[0,.45833,0,0,.55],112:[.19444,.45833,0,0,.56111],113:[.19444,.45833,0,0,.56111],114:[0,.45833,.01528,0,.37222],115:[0,.45833,0,0,.42167],116:[0,.58929,0,0,.40417],117:[0,.45833,0,0,.56111],118:[0,.45833,.01528,0,.5],119:[0,.45833,.01528,0,.74445],120:[0,.45833,0,0,.5],121:[.19444,.45833,.01528,0,.5],122:[0,.45833,0,0,.47639],126:[.35,.34444,0,0,.55],160:[0,0,0,0,.25],168:[0,.69444,0,0,.55],176:[0,.69444,0,0,.73334],180:[0,.69444,0,0,.55],184:[.17014,0,0,0,.48889],305:[0,.45833,0,0,.25556],567:[.19444,.45833,0,0,.28611],710:[0,.69444,0,0,.55],711:[0,.63542,0,0,.55],713:[0,.63778,0,0,.55],728:[0,.69444,0,0,.55],729:[0,.69444,0,0,.30556],730:[0,.69444,0,0,.73334],732:[0,.69444,0,0,.55],733:[0,.69444,0,0,.55],915:[0,.69444,0,0,.58056],916:[0,.69444,0,0,.91667],920:[0,.69444,0,0,.85556],923:[0,.69444,0,0,.67223],926:[0,.69444,0,0,.73334],928:[0,.69444,0,0,.79445],931:[0,.69444,0,0,.79445],933:[0,.69444,0,0,.85556],934:[0,.69444,0,0,.79445],936:[0,.69444,0,0,.85556],937:[0,.69444,0,0,.79445],8211:[0,.45833,.03056,0,.55],8212:[0,.45833,.03056,0,1.10001],8216:[0,.69444,0,0,.30556],8217:[0,.69444,0,0,.30556],8220:[0,.69444,0,0,.55834],8221:[0,.69444,0,0,.55834]},"SansSerif-Italic":{32:[0,0,0,0,.25],33:[0,.69444,.05733,0,.31945],34:[0,.69444,.00316,0,.5],35:[.19444,.69444,.05087,0,.83334],36:[.05556,.75,.11156,0,.5],37:[.05556,.75,.03126,0,.83334],38:[0,.69444,.03058,0,.75834],39:[0,.69444,.07816,0,.27778],40:[.25,.75,.13164,0,.38889],41:[.25,.75,.02536,0,.38889],42:[0,.75,.11775,0,.5],43:[.08333,.58333,.02536,0,.77778],44:[.125,.08333,0,0,.27778],45:[0,.44444,.01946,0,.33333],46:[0,.08333,0,0,.27778],47:[.25,.75,.13164,0,.5],48:[0,.65556,.11156,0,.5],49:[0,.65556,.11156,0,.5],50:[0,.65556,.11156,0,.5],51:[0,.65556,.11156,0,.5],52:[0,.65556,.11156,0,.5],53:[0,.65556,.11156,0,.5],54:[0,.65556,.11156,0,.5],55:[0,.65556,.11156,0,.5],56:[0,.65556,.11156,0,.5],57:[0,.65556,.11156,0,.5],58:[0,.44444,.02502,0,.27778],59:[.125,.44444,.02502,0,.27778],61:[-.13,.37,.05087,0,.77778],63:[0,.69444,.11809,0,.47222],64:[0,.69444,.07555,0,.66667],65:[0,.69444,0,0,.66667],66:[0,.69444,.08293,0,.66667],67:[0,.69444,.11983,0,.63889],68:[0,.69444,.07555,0,.72223],69:[0,.69444,.11983,0,.59722],70:[0,.69444,.13372,0,.56945],71:[0,.69444,.11983,0,.66667],72:[0,.69444,.08094,0,.70834],73:[0,.69444,.13372,0,.27778],74:[0,.69444,.08094,0,.47222],75:[0,.69444,.11983,0,.69445],76:[0,.69444,0,0,.54167],77:[0,.69444,.08094,0,.875],78:[0,.69444,.08094,0,.70834],79:[0,.69444,.07555,0,.73611],80:[0,.69444,.08293,0,.63889],81:[.125,.69444,.07555,0,.73611],82:[0,.69444,.08293,0,.64584],83:[0,.69444,.09205,0,.55556],84:[0,.69444,.13372,0,.68056],85:[0,.69444,.08094,0,.6875],86:[0,.69444,.1615,0,.66667],87:[0,.69444,.1615,0,.94445],88:[0,.69444,.13372,0,.66667],89:[0,.69444,.17261,0,.66667],90:[0,.69444,.11983,0,.61111],91:[.25,.75,.15942,0,.28889],93:[.25,.75,.08719,0,.28889],94:[0,.69444,.0799,0,.5],95:[.35,.09444,.08616,0,.5],97:[0,.44444,.00981,0,.48056],98:[0,.69444,.03057,0,.51667],99:[0,.44444,.08336,0,.44445],100:[0,.69444,.09483,0,.51667],101:[0,.44444,.06778,0,.44445],102:[0,.69444,.21705,0,.30556],103:[.19444,.44444,.10836,0,.5],104:[0,.69444,.01778,0,.51667],105:[0,.67937,.09718,0,.23889],106:[.19444,.67937,.09162,0,.26667],107:[0,.69444,.08336,0,.48889],108:[0,.69444,.09483,0,.23889],109:[0,.44444,.01778,0,.79445],110:[0,.44444,.01778,0,.51667],111:[0,.44444,.06613,0,.5],112:[.19444,.44444,.0389,0,.51667],113:[.19444,.44444,.04169,0,.51667],114:[0,.44444,.10836,0,.34167],115:[0,.44444,.0778,0,.38333],116:[0,.57143,.07225,0,.36111],117:[0,.44444,.04169,0,.51667],118:[0,.44444,.10836,0,.46111],119:[0,.44444,.10836,0,.68334],120:[0,.44444,.09169,0,.46111],121:[.19444,.44444,.10836,0,.46111],122:[0,.44444,.08752,0,.43472],126:[.35,.32659,.08826,0,.5],160:[0,0,0,0,.25],168:[0,.67937,.06385,0,.5],176:[0,.69444,0,0,.73752],184:[.17014,0,0,0,.44445],305:[0,.44444,.04169,0,.23889],567:[.19444,.44444,.04169,0,.26667],710:[0,.69444,.0799,0,.5],711:[0,.63194,.08432,0,.5],713:[0,.60889,.08776,0,.5],714:[0,.69444,.09205,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,.09483,0,.5],729:[0,.67937,.07774,0,.27778],730:[0,.69444,0,0,.73752],732:[0,.67659,.08826,0,.5],733:[0,.69444,.09205,0,.5],915:[0,.69444,.13372,0,.54167],916:[0,.69444,0,0,.83334],920:[0,.69444,.07555,0,.77778],923:[0,.69444,0,0,.61111],926:[0,.69444,.12816,0,.66667],928:[0,.69444,.08094,0,.70834],931:[0,.69444,.11983,0,.72222],933:[0,.69444,.09031,0,.77778],934:[0,.69444,.04603,0,.72222],936:[0,.69444,.09031,0,.77778],937:[0,.69444,.08293,0,.72222],8211:[0,.44444,.08616,0,.5],8212:[0,.44444,.08616,0,1],8216:[0,.69444,.07816,0,.27778],8217:[0,.69444,.07816,0,.27778],8220:[0,.69444,.14205,0,.5],8221:[0,.69444,.00316,0,.5]},"SansSerif-Regular":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.31945],34:[0,.69444,0,0,.5],35:[.19444,.69444,0,0,.83334],36:[.05556,.75,0,0,.5],37:[.05556,.75,0,0,.83334],38:[0,.69444,0,0,.75834],39:[0,.69444,0,0,.27778],40:[.25,.75,0,0,.38889],41:[.25,.75,0,0,.38889],42:[0,.75,0,0,.5],43:[.08333,.58333,0,0,.77778],44:[.125,.08333,0,0,.27778],45:[0,.44444,0,0,.33333],46:[0,.08333,0,0,.27778],47:[.25,.75,0,0,.5],48:[0,.65556,0,0,.5],49:[0,.65556,0,0,.5],50:[0,.65556,0,0,.5],51:[0,.65556,0,0,.5],52:[0,.65556,0,0,.5],53:[0,.65556,0,0,.5],54:[0,.65556,0,0,.5],55:[0,.65556,0,0,.5],56:[0,.65556,0,0,.5],57:[0,.65556,0,0,.5],58:[0,.44444,0,0,.27778],59:[.125,.44444,0,0,.27778],61:[-.13,.37,0,0,.77778],63:[0,.69444,0,0,.47222],64:[0,.69444,0,0,.66667],65:[0,.69444,0,0,.66667],66:[0,.69444,0,0,.66667],67:[0,.69444,0,0,.63889],68:[0,.69444,0,0,.72223],69:[0,.69444,0,0,.59722],70:[0,.69444,0,0,.56945],71:[0,.69444,0,0,.66667],72:[0,.69444,0,0,.70834],73:[0,.69444,0,0,.27778],74:[0,.69444,0,0,.47222],75:[0,.69444,0,0,.69445],76:[0,.69444,0,0,.54167],77:[0,.69444,0,0,.875],78:[0,.69444,0,0,.70834],79:[0,.69444,0,0,.73611],80:[0,.69444,0,0,.63889],81:[.125,.69444,0,0,.73611],82:[0,.69444,0,0,.64584],83:[0,.69444,0,0,.55556],84:[0,.69444,0,0,.68056],85:[0,.69444,0,0,.6875],86:[0,.69444,.01389,0,.66667],87:[0,.69444,.01389,0,.94445],88:[0,.69444,0,0,.66667],89:[0,.69444,.025,0,.66667],90:[0,.69444,0,0,.61111],91:[.25,.75,0,0,.28889],93:[.25,.75,0,0,.28889],94:[0,.69444,0,0,.5],95:[.35,.09444,.02778,0,.5],97:[0,.44444,0,0,.48056],98:[0,.69444,0,0,.51667],99:[0,.44444,0,0,.44445],100:[0,.69444,0,0,.51667],101:[0,.44444,0,0,.44445],102:[0,.69444,.06944,0,.30556],103:[.19444,.44444,.01389,0,.5],104:[0,.69444,0,0,.51667],105:[0,.67937,0,0,.23889],106:[.19444,.67937,0,0,.26667],107:[0,.69444,0,0,.48889],108:[0,.69444,0,0,.23889],109:[0,.44444,0,0,.79445],110:[0,.44444,0,0,.51667],111:[0,.44444,0,0,.5],112:[.19444,.44444,0,0,.51667],113:[.19444,.44444,0,0,.51667],114:[0,.44444,.01389,0,.34167],115:[0,.44444,0,0,.38333],116:[0,.57143,0,0,.36111],117:[0,.44444,0,0,.51667],118:[0,.44444,.01389,0,.46111],119:[0,.44444,.01389,0,.68334],120:[0,.44444,0,0,.46111],121:[.19444,.44444,.01389,0,.46111],122:[0,.44444,0,0,.43472],126:[.35,.32659,0,0,.5],160:[0,0,0,0,.25],168:[0,.67937,0,0,.5],176:[0,.69444,0,0,.66667],184:[.17014,0,0,0,.44445],305:[0,.44444,0,0,.23889],567:[.19444,.44444,0,0,.26667],710:[0,.69444,0,0,.5],711:[0,.63194,0,0,.5],713:[0,.60889,0,0,.5],714:[0,.69444,0,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,0,0,.5],729:[0,.67937,0,0,.27778],730:[0,.69444,0,0,.66667],732:[0,.67659,0,0,.5],733:[0,.69444,0,0,.5],915:[0,.69444,0,0,.54167],916:[0,.69444,0,0,.83334],920:[0,.69444,0,0,.77778],923:[0,.69444,0,0,.61111],926:[0,.69444,0,0,.66667],928:[0,.69444,0,0,.70834],931:[0,.69444,0,0,.72222],933:[0,.69444,0,0,.77778],934:[0,.69444,0,0,.72222],936:[0,.69444,0,0,.77778],937:[0,.69444,0,0,.72222],8211:[0,.44444,.02778,0,.5],8212:[0,.44444,.02778,0,1],8216:[0,.69444,0,0,.27778],8217:[0,.69444,0,0,.27778],8220:[0,.69444,0,0,.5],8221:[0,.69444,0,0,.5]},"Script-Regular":{32:[0,0,0,0,.25],65:[0,.7,.22925,0,.80253],66:[0,.7,.04087,0,.90757],67:[0,.7,.1689,0,.66619],68:[0,.7,.09371,0,.77443],69:[0,.7,.18583,0,.56162],70:[0,.7,.13634,0,.89544],71:[0,.7,.17322,0,.60961],72:[0,.7,.29694,0,.96919],73:[0,.7,.19189,0,.80907],74:[.27778,.7,.19189,0,1.05159],75:[0,.7,.31259,0,.91364],76:[0,.7,.19189,0,.87373],77:[0,.7,.15981,0,1.08031],78:[0,.7,.3525,0,.9015],79:[0,.7,.08078,0,.73787],80:[0,.7,.08078,0,1.01262],81:[0,.7,.03305,0,.88282],82:[0,.7,.06259,0,.85],83:[0,.7,.19189,0,.86767],84:[0,.7,.29087,0,.74697],85:[0,.7,.25815,0,.79996],86:[0,.7,.27523,0,.62204],87:[0,.7,.27523,0,.80532],88:[0,.7,.26006,0,.94445],89:[0,.7,.2939,0,.70961],90:[0,.7,.24037,0,.8212],160:[0,0,0,0,.25]},"Size1-Regular":{32:[0,0,0,0,.25],40:[.35001,.85,0,0,.45834],41:[.35001,.85,0,0,.45834],47:[.35001,.85,0,0,.57778],91:[.35001,.85,0,0,.41667],92:[.35001,.85,0,0,.57778],93:[.35001,.85,0,0,.41667],123:[.35001,.85,0,0,.58334],125:[.35001,.85,0,0,.58334],160:[0,0,0,0,.25],710:[0,.72222,0,0,.55556],732:[0,.72222,0,0,.55556],770:[0,.72222,0,0,.55556],771:[0,.72222,0,0,.55556],8214:[-99e-5,.601,0,0,.77778],8593:[1e-5,.6,0,0,.66667],8595:[1e-5,.6,0,0,.66667],8657:[1e-5,.6,0,0,.77778],8659:[1e-5,.6,0,0,.77778],8719:[.25001,.75,0,0,.94445],8720:[.25001,.75,0,0,.94445],8721:[.25001,.75,0,0,1.05556],8730:[.35001,.85,0,0,1],8739:[-.00599,.606,0,0,.33333],8741:[-.00599,.606,0,0,.55556],8747:[.30612,.805,.19445,0,.47222],8748:[.306,.805,.19445,0,.47222],8749:[.306,.805,.19445,0,.47222],8750:[.30612,.805,.19445,0,.47222],8896:[.25001,.75,0,0,.83334],8897:[.25001,.75,0,0,.83334],8898:[.25001,.75,0,0,.83334],8899:[.25001,.75,0,0,.83334],8968:[.35001,.85,0,0,.47222],8969:[.35001,.85,0,0,.47222],8970:[.35001,.85,0,0,.47222],8971:[.35001,.85,0,0,.47222],9168:[-99e-5,.601,0,0,.66667],10216:[.35001,.85,0,0,.47222],10217:[.35001,.85,0,0,.47222],10752:[.25001,.75,0,0,1.11111],10753:[.25001,.75,0,0,1.11111],10754:[.25001,.75,0,0,1.11111],10756:[.25001,.75,0,0,.83334],10758:[.25001,.75,0,0,.83334]},"Size2-Regular":{32:[0,0,0,0,.25],40:[.65002,1.15,0,0,.59722],41:[.65002,1.15,0,0,.59722],47:[.65002,1.15,0,0,.81111],91:[.65002,1.15,0,0,.47222],92:[.65002,1.15,0,0,.81111],93:[.65002,1.15,0,0,.47222],123:[.65002,1.15,0,0,.66667],125:[.65002,1.15,0,0,.66667],160:[0,0,0,0,.25],710:[0,.75,0,0,1],732:[0,.75,0,0,1],770:[0,.75,0,0,1],771:[0,.75,0,0,1],8719:[.55001,1.05,0,0,1.27778],8720:[.55001,1.05,0,0,1.27778],8721:[.55001,1.05,0,0,1.44445],8730:[.65002,1.15,0,0,1],8747:[.86225,1.36,.44445,0,.55556],8748:[.862,1.36,.44445,0,.55556],8749:[.862,1.36,.44445,0,.55556],8750:[.86225,1.36,.44445,0,.55556],8896:[.55001,1.05,0,0,1.11111],8897:[.55001,1.05,0,0,1.11111],8898:[.55001,1.05,0,0,1.11111],8899:[.55001,1.05,0,0,1.11111],8968:[.65002,1.15,0,0,.52778],8969:[.65002,1.15,0,0,.52778],8970:[.65002,1.15,0,0,.52778],8971:[.65002,1.15,0,0,.52778],10216:[.65002,1.15,0,0,.61111],10217:[.65002,1.15,0,0,.61111],10752:[.55001,1.05,0,0,1.51112],10753:[.55001,1.05,0,0,1.51112],10754:[.55001,1.05,0,0,1.51112],10756:[.55001,1.05,0,0,1.11111],10758:[.55001,1.05,0,0,1.11111]},"Size3-Regular":{32:[0,0,0,0,.25],40:[.95003,1.45,0,0,.73611],41:[.95003,1.45,0,0,.73611],47:[.95003,1.45,0,0,1.04445],91:[.95003,1.45,0,0,.52778],92:[.95003,1.45,0,0,1.04445],93:[.95003,1.45,0,0,.52778],123:[.95003,1.45,0,0,.75],125:[.95003,1.45,0,0,.75],160:[0,0,0,0,.25],710:[0,.75,0,0,1.44445],732:[0,.75,0,0,1.44445],770:[0,.75,0,0,1.44445],771:[0,.75,0,0,1.44445],8730:[.95003,1.45,0,0,1],8968:[.95003,1.45,0,0,.58334],8969:[.95003,1.45,0,0,.58334],8970:[.95003,1.45,0,0,.58334],8971:[.95003,1.45,0,0,.58334],10216:[.95003,1.45,0,0,.75],10217:[.95003,1.45,0,0,.75]},"Size4-Regular":{32:[0,0,0,0,.25],40:[1.25003,1.75,0,0,.79167],41:[1.25003,1.75,0,0,.79167],47:[1.25003,1.75,0,0,1.27778],91:[1.25003,1.75,0,0,.58334],92:[1.25003,1.75,0,0,1.27778],93:[1.25003,1.75,0,0,.58334],123:[1.25003,1.75,0,0,.80556],125:[1.25003,1.75,0,0,.80556],160:[0,0,0,0,.25],710:[0,.825,0,0,1.8889],732:[0,.825,0,0,1.8889],770:[0,.825,0,0,1.8889],771:[0,.825,0,0,1.8889],8730:[1.25003,1.75,0,0,1],8968:[1.25003,1.75,0,0,.63889],8969:[1.25003,1.75,0,0,.63889],8970:[1.25003,1.75,0,0,.63889],8971:[1.25003,1.75,0,0,.63889],9115:[.64502,1.155,0,0,.875],9116:[1e-5,.6,0,0,.875],9117:[.64502,1.155,0,0,.875],9118:[.64502,1.155,0,0,.875],9119:[1e-5,.6,0,0,.875],9120:[.64502,1.155,0,0,.875],9121:[.64502,1.155,0,0,.66667],9122:[-99e-5,.601,0,0,.66667],9123:[.64502,1.155,0,0,.66667],9124:[.64502,1.155,0,0,.66667],9125:[-99e-5,.601,0,0,.66667],9126:[.64502,1.155,0,0,.66667],9127:[1e-5,.9,0,0,.88889],9128:[.65002,1.15,0,0,.88889],9129:[.90001,0,0,0,.88889],9130:[0,.3,0,0,.88889],9131:[1e-5,.9,0,0,.88889],9132:[.65002,1.15,0,0,.88889],9133:[.90001,0,0,0,.88889],9143:[.88502,.915,0,0,1.05556],10216:[1.25003,1.75,0,0,.80556],10217:[1.25003,1.75,0,0,.80556],57344:[-.00499,.605,0,0,1.05556],57345:[-.00499,.605,0,0,1.05556],57680:[0,.12,0,0,.45],57681:[0,.12,0,0,.45],57682:[0,.12,0,0,.45],57683:[0,.12,0,0,.45]},"Typewriter-Regular":{32:[0,0,0,0,.525],33:[0,.61111,0,0,.525],34:[0,.61111,0,0,.525],35:[0,.61111,0,0,.525],36:[.08333,.69444,0,0,.525],37:[.08333,.69444,0,0,.525],38:[0,.61111,0,0,.525],39:[0,.61111,0,0,.525],40:[.08333,.69444,0,0,.525],41:[.08333,.69444,0,0,.525],42:[0,.52083,0,0,.525],43:[-.08056,.53055,0,0,.525],44:[.13889,.125,0,0,.525],45:[-.08056,.53055,0,0,.525],46:[0,.125,0,0,.525],47:[.08333,.69444,0,0,.525],48:[0,.61111,0,0,.525],49:[0,.61111,0,0,.525],50:[0,.61111,0,0,.525],51:[0,.61111,0,0,.525],52:[0,.61111,0,0,.525],53:[0,.61111,0,0,.525],54:[0,.61111,0,0,.525],55:[0,.61111,0,0,.525],56:[0,.61111,0,0,.525],57:[0,.61111,0,0,.525],58:[0,.43056,0,0,.525],59:[.13889,.43056,0,0,.525],60:[-.05556,.55556,0,0,.525],61:[-.19549,.41562,0,0,.525],62:[-.05556,.55556,0,0,.525],63:[0,.61111,0,0,.525],64:[0,.61111,0,0,.525],65:[0,.61111,0,0,.525],66:[0,.61111,0,0,.525],67:[0,.61111,0,0,.525],68:[0,.61111,0,0,.525],69:[0,.61111,0,0,.525],70:[0,.61111,0,0,.525],71:[0,.61111,0,0,.525],72:[0,.61111,0,0,.525],73:[0,.61111,0,0,.525],74:[0,.61111,0,0,.525],75:[0,.61111,0,0,.525],76:[0,.61111,0,0,.525],77:[0,.61111,0,0,.525],78:[0,.61111,0,0,.525],79:[0,.61111,0,0,.525],80:[0,.61111,0,0,.525],81:[.13889,.61111,0,0,.525],82:[0,.61111,0,0,.525],83:[0,.61111,0,0,.525],84:[0,.61111,0,0,.525],85:[0,.61111,0,0,.525],86:[0,.61111,0,0,.525],87:[0,.61111,0,0,.525],88:[0,.61111,0,0,.525],89:[0,.61111,0,0,.525],90:[0,.61111,0,0,.525],91:[.08333,.69444,0,0,.525],92:[.08333,.69444,0,0,.525],93:[.08333,.69444,0,0,.525],94:[0,.61111,0,0,.525],95:[.09514,0,0,0,.525],96:[0,.61111,0,0,.525],97:[0,.43056,0,0,.525],98:[0,.61111,0,0,.525],99:[0,.43056,0,0,.525],100:[0,.61111,0,0,.525],101:[0,.43056,0,0,.525],102:[0,.61111,0,0,.525],103:[.22222,.43056,0,0,.525],104:[0,.61111,0,0,.525],105:[0,.61111,0,0,.525],106:[.22222,.61111,0,0,.525],107:[0,.61111,0,0,.525],108:[0,.61111,0,0,.525],109:[0,.43056,0,0,.525],110:[0,.43056,0,0,.525],111:[0,.43056,0,0,.525],112:[.22222,.43056,0,0,.525],113:[.22222,.43056,0,0,.525],114:[0,.43056,0,0,.525],115:[0,.43056,0,0,.525],116:[0,.55358,0,0,.525],117:[0,.43056,0,0,.525],118:[0,.43056,0,0,.525],119:[0,.43056,0,0,.525],120:[0,.43056,0,0,.525],121:[.22222,.43056,0,0,.525],122:[0,.43056,0,0,.525],123:[.08333,.69444,0,0,.525],124:[.08333,.69444,0,0,.525],125:[.08333,.69444,0,0,.525],126:[0,.61111,0,0,.525],127:[0,.61111,0,0,.525],160:[0,0,0,0,.525],176:[0,.61111,0,0,.525],184:[.19445,0,0,0,.525],305:[0,.43056,0,0,.525],567:[.22222,.43056,0,0,.525],711:[0,.56597,0,0,.525],713:[0,.56555,0,0,.525],714:[0,.61111,0,0,.525],715:[0,.61111,0,0,.525],728:[0,.61111,0,0,.525],730:[0,.61111,0,0,.525],770:[0,.61111,0,0,.525],771:[0,.61111,0,0,.525],776:[0,.61111,0,0,.525],915:[0,.61111,0,0,.525],916:[0,.61111,0,0,.525],920:[0,.61111,0,0,.525],923:[0,.61111,0,0,.525],926:[0,.61111,0,0,.525],928:[0,.61111,0,0,.525],931:[0,.61111,0,0,.525],933:[0,.61111,0,0,.525],934:[0,.61111,0,0,.525],936:[0,.61111,0,0,.525],937:[0,.61111,0,0,.525],8216:[0,.61111,0,0,.525],8217:[0,.61111,0,0,.525],8242:[0,.61111,0,0,.525],9251:[.11111,.21944,0,0,.525]}},Se={slant:[.25,.25,.25],space:[0,0,0],stretch:[0,0,0],shrink:[0,0,0],xHeight:[.431,.431,.431],quad:[1,1.171,1.472],extraSpace:[0,0,0],num1:[.677,.732,.925],num2:[.394,.384,.387],num3:[.444,.471,.504],denom1:[.686,.752,1.025],denom2:[.345,.344,.532],sup1:[.413,.503,.504],sup2:[.363,.431,.404],sup3:[.289,.286,.294],sub1:[.15,.143,.2],sub2:[.247,.286,.4],supDrop:[.386,.353,.494],subDrop:[.05,.071,.1],delim1:[2.39,1.7,1.98],delim2:[1.01,1.157,1.42],axisHeight:[.25,.25,.25],defaultRuleThickness:[.04,.049,.049],bigOpSpacing1:[.111,.111,.111],bigOpSpacing2:[.166,.166,.166],bigOpSpacing3:[.2,.2,.2],bigOpSpacing4:[.6,.611,.611],bigOpSpacing5:[.1,.143,.143],sqrtRuleThickness:[.04,.04,.04],ptPerEm:[10,10,10],doubleRuleSep:[.2,.2,.2],arrayRuleWidth:[.04,.04,.04],fboxsep:[.3,.3,.3],fboxrule:[.04,.04,.04]},jt={Å:"A",Ç:"C",Ð:"D",Þ:"o",å:"a",ç:"c",ð:"d",þ:"o",А:"A",Б:"B",В:"B",Г:"F",Д:"A",Е:"E",Ж:"K",З:"3",И:"N",Й:"N",К:"K",Л:"N",М:"M",Н:"H",О:"O",П:"N",Р:"P",С:"C",Т:"T",У:"y",Ф:"O",Х:"X",Ц:"U",Ч:"h",Ш:"W",Щ:"W",Ъ:"B",Ы:"X",Ь:"B",Э:"3",Ю:"X",Я:"R",а:"a",б:"b",в:"a",г:"r",д:"y",е:"e",ж:"m",з:"e",и:"n",й:"n",к:"n",л:"n",м:"m",н:"n",о:"o",п:"n",р:"p",с:"c",т:"o",у:"y",ф:"b",х:"x",ц:"n",ч:"n",ш:"w",щ:"w",ъ:"a",ы:"m",ь:"a",э:"e",ю:"m",я:"r"};function jn(l,e){B0[l]=e}u(jn,"setFontMetrics");function _e(l,e,t){if(!B0[e])throw new Error("Font metrics not found for font: "+e+".");var r=l.charCodeAt(0),n=B0[e][r];if(!n&&l[0]in jt&&(r=jt[l[0]].charCodeAt(0),n=B0[e][r]),!n&&t==="text"&&Vt(r)&&(n=B0[e][77]),n)return{depth:n[0],height:n[1],italic:n[2],skew:n[3],width:n[4]}}u(_e,"getCharacterMetrics");var et={};function Zn(l){var e;if(l>=5?e=0:l>=3?e=1:e=2,!et[e]){var t=et[e]={cssEmPerMu:Se.quad[e]/18};for(var r in Se)Se.hasOwnProperty(r)&&(t[r]=Se[r][e])}return et[e]}u(Zn,"getGlobalMetrics");var Kn={bin:1,close:1,inner:1,open:1,punct:1,rel:1},Jn={"accent-token":1,mathord:1,"op-token":1,spacing:1,textord:1},ze={math:{},text:{}},_=ze;function i(l,e,t,r,n,a){ze[l][n]={font:e,group:t,replace:r},a&&r&&(ze[l][r]=ze[l][n])}u(i,"defineSymbol");var s="math",S="text",c="main",p="ams",e0="accent-token",D="bin",u0="close",ie="inner",F="mathord",a0="op-token",h0="open",Me="punct",v="rel",A0="spacing",x="textord";i(s,c,v,"≡","\\equiv",!0),i(s,c,v,"≺","\\prec",!0),i(s,c,v,"≻","\\succ",!0),i(s,c,v,"∼","\\sim",!0),i(s,c,v,"⊥","\\perp"),i(s,c,v,"⪯","\\preceq",!0),i(s,c,v,"⪰","\\succeq",!0),i(s,c,v,"≃","\\simeq",!0),i(s,c,v,"∣","\\mid",!0),i(s,c,v,"≪","\\ll",!0),i(s,c,v,"≫","\\gg",!0),i(s,c,v,"≍","\\asymp",!0),i(s,c,v,"∥","\\parallel"),i(s,c,v,"⋈","\\bowtie",!0),i(s,c,v,"⌣","\\smile",!0),i(s,c,v,"⊑","\\sqsubseteq",!0),i(s,c,v,"⊒","\\sqsupseteq",!0),i(s,c,v,"≐","\\doteq",!0),i(s,c,v,"⌢","\\frown",!0),i(s,c,v,"∋","\\ni",!0),i(s,c,v,"∝","\\propto",!0),i(s,c,v,"⊢","\\vdash",!0),i(s,c,v,"⊣","\\dashv",!0),i(s,c,v,"∋","\\owns"),i(s,c,Me,".","\\ldotp"),i(s,c,Me,"⋅","\\cdotp"),i(s,c,x,"#","\\#"),i(S,c,x,"#","\\#"),i(s,c,x,"&","\\&"),i(S,c,x,"&","\\&"),i(s,c,x,"ℵ","\\aleph",!0),i(s,c,x,"∀","\\forall",!0),i(s,c,x,"ℏ","\\hbar",!0),i(s,c,x,"∃","\\exists",!0),i(s,c,x,"∇","\\nabla",!0),i(s,c,x,"♭","\\flat",!0),i(s,c,x,"ℓ","\\ell",!0),i(s,c,x,"♮","\\natural",!0),i(s,c,x,"♣","\\clubsuit",!0),i(s,c,x,"℘","\\wp",!0),i(s,c,x,"♯","\\sharp",!0),i(s,c,x,"♢","\\diamondsuit",!0),i(s,c,x,"ℜ","\\Re",!0),i(s,c,x,"♡","\\heartsuit",!0),i(s,c,x,"ℑ","\\Im",!0),i(s,c,x,"♠","\\spadesuit",!0),i(S,c,x,"§","\\S",!0),i(S,c,x,"¶","\\P",!0),i(s,c,x,"†","\\dag"),i(S,c,x,"†","\\dag"),i(S,c,x,"†","\\textdagger"),i(s,c,x,"‡","\\ddag"),i(S,c,x,"‡","\\ddag"),i(S,c,x,"‡","\\textdaggerdbl"),i(s,c,u0,"⎱","\\rmoustache",!0),i(s,c,h0,"⎰","\\lmoustache",!0),i(s,c,u0,"⟯","\\rgroup",!0),i(s,c,h0,"⟮","\\lgroup",!0),i(s,c,D,"∓","\\mp",!0),i(s,c,D,"⊖","\\ominus",!0),i(s,c,D,"⊎","\\uplus",!0),i(s,c,D,"⊓","\\sqcap",!0),i(s,c,D,"∗","\\ast"),i(s,c,D,"⊔","\\sqcup",!0),i(s,c,D,"◯","\\bigcirc",!0),i(s,c,D,"∙","\\bullet"),i(s,c,D,"‡","\\ddagger"),i(s,c,D,"≀","\\wr",!0),i(s,c,D,"⨿","\\amalg"),i(s,c,D,"&","\\And"),i(s,c,v,"⟵","\\longleftarrow",!0),i(s,c,v,"⇐","\\Leftarrow",!0),i(s,c,v,"⟸","\\Longleftarrow",!0),i(s,c,v,"⟶","\\longrightarrow",!0),i(s,c,v,"⇒","\\Rightarrow",!0),i(s,c,v,"⟹","\\Longrightarrow",!0),i(s,c,v,"↔","\\leftrightarrow",!0),i(s,c,v,"⟷","\\longleftrightarrow",!0),i(s,c,v,"⇔","\\Leftrightarrow",!0),i(s,c,v,"⟺","\\Longleftrightarrow",!0),i(s,c,v,"↦","\\mapsto",!0),i(s,c,v,"⟼","\\longmapsto",!0),i(s,c,v,"↗","\\nearrow",!0),i(s,c,v,"↩","\\hookleftarrow",!0),i(s,c,v,"↪","\\hookrightarrow",!0),i(s,c,v,"↘","\\searrow",!0),i(s,c,v,"↼","\\leftharpoonup",!0),i(s,c,v,"⇀","\\rightharpoonup",!0),i(s,c,v,"↙","\\swarrow",!0),i(s,c,v,"↽","\\leftharpoondown",!0),i(s,c,v,"⇁","\\rightharpoondown",!0),i(s,c,v,"↖","\\nwarrow",!0),i(s,c,v,"⇌","\\rightleftharpoons",!0),i(s,p,v,"≮","\\nless",!0),i(s,p,v,"","\\@nleqslant"),i(s,p,v,"","\\@nleqq"),i(s,p,v,"⪇","\\lneq",!0),i(s,p,v,"≨","\\lneqq",!0),i(s,p,v,"","\\@lvertneqq"),i(s,p,v,"⋦","\\lnsim",!0),i(s,p,v,"⪉","\\lnapprox",!0),i(s,p,v,"⊀","\\nprec",!0),i(s,p,v,"⋠","\\npreceq",!0),i(s,p,v,"⋨","\\precnsim",!0),i(s,p,v,"⪹","\\precnapprox",!0),i(s,p,v,"≁","\\nsim",!0),i(s,p,v,"","\\@nshortmid"),i(s,p,v,"∤","\\nmid",!0),i(s,p,v,"⊬","\\nvdash",!0),i(s,p,v,"⊭","\\nvDash",!0),i(s,p,v,"⋪","\\ntriangleleft"),i(s,p,v,"⋬","\\ntrianglelefteq",!0),i(s,p,v,"⊊","\\subsetneq",!0),i(s,p,v,"","\\@varsubsetneq"),i(s,p,v,"⫋","\\subsetneqq",!0),i(s,p,v,"","\\@varsubsetneqq"),i(s,p,v,"≯","\\ngtr",!0),i(s,p,v,"","\\@ngeqslant"),i(s,p,v,"","\\@ngeqq"),i(s,p,v,"⪈","\\gneq",!0),i(s,p,v,"≩","\\gneqq",!0),i(s,p,v,"","\\@gvertneqq"),i(s,p,v,"⋧","\\gnsim",!0),i(s,p,v,"⪊","\\gnapprox",!0),i(s,p,v,"⊁","\\nsucc",!0),i(s,p,v,"⋡","\\nsucceq",!0),i(s,p,v,"⋩","\\succnsim",!0),i(s,p,v,"⪺","\\succnapprox",!0),i(s,p,v,"≆","\\ncong",!0),i(s,p,v,"","\\@nshortparallel"),i(s,p,v,"∦","\\nparallel",!0),i(s,p,v,"⊯","\\nVDash",!0),i(s,p,v,"⋫","\\ntriangleright"),i(s,p,v,"⋭","\\ntrianglerighteq",!0),i(s,p,v,"","\\@nsupseteqq"),i(s,p,v,"⊋","\\supsetneq",!0),i(s,p,v,"","\\@varsupsetneq"),i(s,p,v,"⫌","\\supsetneqq",!0),i(s,p,v,"","\\@varsupsetneqq"),i(s,p,v,"⊮","\\nVdash",!0),i(s,p,v,"⪵","\\precneqq",!0),i(s,p,v,"⪶","\\succneqq",!0),i(s,p,v,"","\\@nsubseteqq"),i(s,p,D,"⊴","\\unlhd"),i(s,p,D,"⊵","\\unrhd"),i(s,p,v,"↚","\\nleftarrow",!0),i(s,p,v,"↛","\\nrightarrow",!0),i(s,p,v,"⇍","\\nLeftarrow",!0),i(s,p,v,"⇏","\\nRightarrow",!0),i(s,p,v,"↮","\\nleftrightarrow",!0),i(s,p,v,"⇎","\\nLeftrightarrow",!0),i(s,p,v,"△","\\vartriangle"),i(s,p,x,"ℏ","\\hslash"),i(s,p,x,"▽","\\triangledown"),i(s,p,x,"◊","\\lozenge"),i(s,p,x,"Ⓢ","\\circledS"),i(s,p,x,"®","\\circledR"),i(S,p,x,"®","\\circledR"),i(s,p,x,"∡","\\measuredangle",!0),i(s,p,x,"∄","\\nexists"),i(s,p,x,"℧","\\mho"),i(s,p,x,"Ⅎ","\\Finv",!0),i(s,p,x,"⅁","\\Game",!0),i(s,p,x,"‵","\\backprime"),i(s,p,x,"▲","\\blacktriangle"),i(s,p,x,"▼","\\blacktriangledown"),i(s,p,x,"■","\\blacksquare"),i(s,p,x,"⧫","\\blacklozenge"),i(s,p,x,"★","\\bigstar"),i(s,p,x,"∢","\\sphericalangle",!0),i(s,p,x,"∁","\\complement",!0),i(s,p,x,"ð","\\eth",!0),i(S,c,x,"ð","ð"),i(s,p,x,"╱","\\diagup"),i(s,p,x,"╲","\\diagdown"),i(s,p,x,"□","\\square"),i(s,p,x,"□","\\Box"),i(s,p,x,"◊","\\Diamond"),i(s,p,x,"¥","\\yen",!0),i(S,p,x,"¥","\\yen",!0),i(s,p,x,"✓","\\checkmark",!0),i(S,p,x,"✓","\\checkmark"),i(s,p,x,"ℶ","\\beth",!0),i(s,p,x,"ℸ","\\daleth",!0),i(s,p,x,"ℷ","\\gimel",!0),i(s,p,x,"ϝ","\\digamma",!0),i(s,p,x,"ϰ","\\varkappa"),i(s,p,h0,"┌","\\@ulcorner",!0),i(s,p,u0,"┐","\\@urcorner",!0),i(s,p,h0,"└","\\@llcorner",!0),i(s,p,u0,"┘","\\@lrcorner",!0),i(s,p,v,"≦","\\leqq",!0),i(s,p,v,"⩽","\\leqslant",!0),i(s,p,v,"⪕","\\eqslantless",!0),i(s,p,v,"≲","\\lesssim",!0),i(s,p,v,"⪅","\\lessapprox",!0),i(s,p,v,"≊","\\approxeq",!0),i(s,p,D,"⋖","\\lessdot"),i(s,p,v,"⋘","\\lll",!0),i(s,p,v,"≶","\\lessgtr",!0),i(s,p,v,"⋚","\\lesseqgtr",!0),i(s,p,v,"⪋","\\lesseqqgtr",!0),i(s,p,v,"≑","\\doteqdot"),i(s,p,v,"≓","\\risingdotseq",!0),i(s,p,v,"≒","\\fallingdotseq",!0),i(s,p,v,"∽","\\backsim",!0),i(s,p,v,"⋍","\\backsimeq",!0),i(s,p,v,"⫅","\\subseteqq",!0),i(s,p,v,"⋐","\\Subset",!0),i(s,p,v,"⊏","\\sqsubset",!0),i(s,p,v,"≼","\\preccurlyeq",!0),i(s,p,v,"⋞","\\curlyeqprec",!0),i(s,p,v,"≾","\\precsim",!0),i(s,p,v,"⪷","\\precapprox",!0),i(s,p,v,"⊲","\\vartriangleleft"),i(s,p,v,"⊴","\\trianglelefteq"),i(s,p,v,"⊨","\\vDash",!0),i(s,p,v,"⊪","\\Vvdash",!0),i(s,p,v,"⌣","\\smallsmile"),i(s,p,v,"⌢","\\smallfrown"),i(s,p,v,"≏","\\bumpeq",!0),i(s,p,v,"≎","\\Bumpeq",!0),i(s,p,v,"≧","\\geqq",!0),i(s,p,v,"⩾","\\geqslant",!0),i(s,p,v,"⪖","\\eqslantgtr",!0),i(s,p,v,"≳","\\gtrsim",!0),i(s,p,v,"⪆","\\gtrapprox",!0),i(s,p,D,"⋗","\\gtrdot"),i(s,p,v,"⋙","\\ggg",!0),i(s,p,v,"≷","\\gtrless",!0),i(s,p,v,"⋛","\\gtreqless",!0),i(s,p,v,"⪌","\\gtreqqless",!0),i(s,p,v,"≖","\\eqcirc",!0),i(s,p,v,"≗","\\circeq",!0),i(s,p,v,"≜","\\triangleq",!0),i(s,p,v,"∼","\\thicksim"),i(s,p,v,"≈","\\thickapprox"),i(s,p,v,"⫆","\\supseteqq",!0),i(s,p,v,"⋑","\\Supset",!0),i(s,p,v,"⊐","\\sqsupset",!0),i(s,p,v,"≽","\\succcurlyeq",!0),i(s,p,v,"⋟","\\curlyeqsucc",!0),i(s,p,v,"≿","\\succsim",!0),i(s,p,v,"⪸","\\succapprox",!0),i(s,p,v,"⊳","\\vartriangleright"),i(s,p,v,"⊵","\\trianglerighteq"),i(s,p,v,"⊩","\\Vdash",!0),i(s,p,v,"∣","\\shortmid"),i(s,p,v,"∥","\\shortparallel"),i(s,p,v,"≬","\\between",!0),i(s,p,v,"⋔","\\pitchfork",!0),i(s,p,v,"∝","\\varpropto"),i(s,p,v,"◀","\\blacktriangleleft"),i(s,p,v,"∴","\\therefore",!0),i(s,p,v,"∍","\\backepsilon"),i(s,p,v,"▶","\\blacktriangleright"),i(s,p,v,"∵","\\because",!0),i(s,p,v,"⋘","\\llless"),i(s,p,v,"⋙","\\gggtr"),i(s,p,D,"⊲","\\lhd"),i(s,p,D,"⊳","\\rhd"),i(s,p,v,"≂","\\eqsim",!0),i(s,c,v,"⋈","\\Join"),i(s,p,v,"≑","\\Doteq",!0),i(s,p,D,"∔","\\dotplus",!0),i(s,p,D,"∖","\\smallsetminus"),i(s,p,D,"⋒","\\Cap",!0),i(s,p,D,"⋓","\\Cup",!0),i(s,p,D,"⩞","\\doublebarwedge",!0),i(s,p,D,"⊟","\\boxminus",!0),i(s,p,D,"⊞","\\boxplus",!0),i(s,p,D,"⋇","\\divideontimes",!0),i(s,p,D,"⋉","\\ltimes",!0),i(s,p,D,"⋊","\\rtimes",!0),i(s,p,D,"⋋","\\leftthreetimes",!0),i(s,p,D,"⋌","\\rightthreetimes",!0),i(s,p,D,"⋏","\\curlywedge",!0),i(s,p,D,"⋎","\\curlyvee",!0),i(s,p,D,"⊝","\\circleddash",!0),i(s,p,D,"⊛","\\circledast",!0),i(s,p,D,"⋅","\\centerdot"),i(s,p,D,"⊺","\\intercal",!0),i(s,p,D,"⋒","\\doublecap"),i(s,p,D,"⋓","\\doublecup"),i(s,p,D,"⊠","\\boxtimes",!0),i(s,p,v,"⇢","\\dashrightarrow",!0),i(s,p,v,"⇠","\\dashleftarrow",!0),i(s,p,v,"⇇","\\leftleftarrows",!0),i(s,p,v,"⇆","\\leftrightarrows",!0),i(s,p,v,"⇚","\\Lleftarrow",!0),i(s,p,v,"↞","\\twoheadleftarrow",!0),i(s,p,v,"↢","\\leftarrowtail",!0),i(s,p,v,"↫","\\looparrowleft",!0),i(s,p,v,"⇋","\\leftrightharpoons",!0),i(s,p,v,"↶","\\curvearrowleft",!0),i(s,p,v,"↺","\\circlearrowleft",!0),i(s,p,v,"↰","\\Lsh",!0),i(s,p,v,"⇈","\\upuparrows",!0),i(s,p,v,"↿","\\upharpoonleft",!0),i(s,p,v,"⇃","\\downharpoonleft",!0),i(s,c,v,"⊶","\\origof",!0),i(s,c,v,"⊷","\\imageof",!0),i(s,p,v,"⊸","\\multimap",!0),i(s,p,v,"↭","\\leftrightsquigarrow",!0),i(s,p,v,"⇉","\\rightrightarrows",!0),i(s,p,v,"⇄","\\rightleftarrows",!0),i(s,p,v,"↠","\\twoheadrightarrow",!0),i(s,p,v,"↣","\\rightarrowtail",!0),i(s,p,v,"↬","\\looparrowright",!0),i(s,p,v,"↷","\\curvearrowright",!0),i(s,p,v,"↻","\\circlearrowright",!0),i(s,p,v,"↱","\\Rsh",!0),i(s,p,v,"⇊","\\downdownarrows",!0),i(s,p,v,"↾","\\upharpoonright",!0),i(s,p,v,"⇂","\\downharpoonright",!0),i(s,p,v,"⇝","\\rightsquigarrow",!0),i(s,p,v,"⇝","\\leadsto"),i(s,p,v,"⇛","\\Rrightarrow",!0),i(s,p,v,"↾","\\restriction"),i(s,c,x,"‘","`"),i(s,c,x,"$","\\$"),i(S,c,x,"$","\\$"),i(S,c,x,"$","\\textdollar"),i(s,c,x,"%","\\%"),i(S,c,x,"%","\\%"),i(s,c,x,"_","\\_"),i(S,c,x,"_","\\_"),i(S,c,x,"_","\\textunderscore"),i(s,c,x,"∠","\\angle",!0),i(s,c,x,"∞","\\infty",!0),i(s,c,x,"′","\\prime"),i(s,c,x,"△","\\triangle"),i(s,c,x,"Γ","\\Gamma",!0),i(s,c,x,"Δ","\\Delta",!0),i(s,c,x,"Θ","\\Theta",!0),i(s,c,x,"Λ","\\Lambda",!0),i(s,c,x,"Ξ","\\Xi",!0),i(s,c,x,"Π","\\Pi",!0),i(s,c,x,"Σ","\\Sigma",!0),i(s,c,x,"Υ","\\Upsilon",!0),i(s,c,x,"Φ","\\Phi",!0),i(s,c,x,"Ψ","\\Psi",!0),i(s,c,x,"Ω","\\Omega",!0),i(s,c,x,"A","Α"),i(s,c,x,"B","Β"),i(s,c,x,"E","Ε"),i(s,c,x,"Z","Ζ"),i(s,c,x,"H","Η"),i(s,c,x,"I","Ι"),i(s,c,x,"K","Κ"),i(s,c,x,"M","Μ"),i(s,c,x,"N","Ν"),i(s,c,x,"O","Ο"),i(s,c,x,"P","Ρ"),i(s,c,x,"T","Τ"),i(s,c,x,"X","Χ"),i(s,c,x,"¬","\\neg",!0),i(s,c,x,"¬","\\lnot"),i(s,c,x,"⊤","\\top"),i(s,c,x,"⊥","\\bot"),i(s,c,x,"∅","\\emptyset"),i(s,p,x,"∅","\\varnothing"),i(s,c,F,"α","\\alpha",!0),i(s,c,F,"β","\\beta",!0),i(s,c,F,"γ","\\gamma",!0),i(s,c,F,"δ","\\delta",!0),i(s,c,F,"ϵ","\\epsilon",!0),i(s,c,F,"ζ","\\zeta",!0),i(s,c,F,"η","\\eta",!0),i(s,c,F,"θ","\\theta",!0),i(s,c,F,"ι","\\iota",!0),i(s,c,F,"κ","\\kappa",!0),i(s,c,F,"λ","\\lambda",!0),i(s,c,F,"μ","\\mu",!0),i(s,c,F,"ν","\\nu",!0),i(s,c,F,"ξ","\\xi",!0),i(s,c,F,"ο","\\omicron",!0),i(s,c,F,"π","\\pi",!0),i(s,c,F,"ρ","\\rho",!0),i(s,c,F,"σ","\\sigma",!0),i(s,c,F,"τ","\\tau",!0),i(s,c,F,"υ","\\upsilon",!0),i(s,c,F,"ϕ","\\phi",!0),i(s,c,F,"χ","\\chi",!0),i(s,c,F,"ψ","\\psi",!0),i(s,c,F,"ω","\\omega",!0),i(s,c,F,"ε","\\varepsilon",!0),i(s,c,F,"ϑ","\\vartheta",!0),i(s,c,F,"ϖ","\\varpi",!0),i(s,c,F,"ϱ","\\varrho",!0),i(s,c,F,"ς","\\varsigma",!0),i(s,c,F,"φ","\\varphi",!0),i(s,c,D,"∗","*"),i(s,c,D,"+","+"),i(s,c,D,"−","-"),i(s,c,D,"⋅","\\cdot",!0),i(s,c,D,"∘","\\circ"),i(s,c,D,"÷","\\div",!0),i(s,c,D,"±","\\pm",!0),i(s,c,D,"×","\\times",!0),i(s,c,D,"∩","\\cap",!0),i(s,c,D,"∪","\\cup",!0),i(s,c,D,"∖","\\setminus"),i(s,c,D,"∧","\\land"),i(s,c,D,"∨","\\lor"),i(s,c,D,"∧","\\wedge",!0),i(s,c,D,"∨","\\vee",!0),i(s,c,x,"√","\\surd"),i(s,c,h0,"⟨","\\langle",!0),i(s,c,h0,"∣","\\lvert"),i(s,c,h0,"∥","\\lVert"),i(s,c,u0,"?","?"),i(s,c,u0,"!","!"),i(s,c,u0,"⟩","\\rangle",!0),i(s,c,u0,"∣","\\rvert"),i(s,c,u0,"∥","\\rVert"),i(s,c,v,"=","="),i(s,c,v,":",":"),i(s,c,v,"≈","\\approx",!0),i(s,c,v,"≅","\\cong",!0),i(s,c,v,"≥","\\ge"),i(s,c,v,"≥","\\geq",!0),i(s,c,v,"←","\\gets"),i(s,c,v,">","\\gt",!0),i(s,c,v,"∈","\\in",!0),i(s,c,v,"","\\@not"),i(s,c,v,"⊂","\\subset",!0),i(s,c,v,"⊃","\\supset",!0),i(s,c,v,"⊆","\\subseteq",!0),i(s,c,v,"⊇","\\supseteq",!0),i(s,p,v,"⊈","\\nsubseteq",!0),i(s,p,v,"⊉","\\nsupseteq",!0),i(s,c,v,"⊨","\\models"),i(s,c,v,"←","\\leftarrow",!0),i(s,c,v,"≤","\\le"),i(s,c,v,"≤","\\leq",!0),i(s,c,v,"<","\\lt",!0),i(s,c,v,"→","\\rightarrow",!0),i(s,c,v,"→","\\to"),i(s,p,v,"≱","\\ngeq",!0),i(s,p,v,"≰","\\nleq",!0),i(s,c,A0," ","\\ "),i(s,c,A0," ","~"),i(s,c,A0," ","\\space"),i(s,c,A0," ","\\nobreakspace"),i(S,c,A0," ","\\ "),i(S,c,A0," "," "),i(S,c,A0," ","~"),i(S,c,A0," ","\\space"),i(S,c,A0," ","\\nobreakspace"),i(s,c,A0,null,"\\nobreak"),i(s,c,A0,null,"\\allowbreak"),i(s,c,Me,",",","),i(s,c,Me,";",";"),i(s,p,D,"⊼","\\barwedge",!0),i(s,p,D,"⊻","\\veebar",!0),i(s,c,D,"⊙","\\odot",!0),i(s,c,D,"⊕","\\oplus",!0),i(s,c,D,"⊗","\\otimes",!0),i(s,c,x,"∂","\\partial",!0),i(s,c,D,"⊘","\\oslash",!0),i(s,p,D,"⊚","\\circledcirc",!0),i(s,p,D,"⊡","\\boxdot",!0),i(s,c,D,"△","\\bigtriangleup"),i(s,c,D,"▽","\\bigtriangledown"),i(s,c,D,"†","\\dagger"),i(s,c,D,"⋄","\\diamond"),i(s,c,D,"⋆","\\star"),i(s,c,D,"◃","\\triangleleft"),i(s,c,D,"▹","\\triangleright"),i(s,c,h0,"{","\\{"),i(S,c,x,"{","\\{"),i(S,c,x,"{","\\textbraceleft"),i(s,c,u0,"}","\\}"),i(S,c,x,"}","\\}"),i(S,c,x,"}","\\textbraceright"),i(s,c,h0,"{","\\lbrace"),i(s,c,u0,"}","\\rbrace"),i(s,c,h0,"[","\\lbrack",!0),i(S,c,x,"[","\\lbrack",!0),i(s,c,u0,"]","\\rbrack",!0),i(S,c,x,"]","\\rbrack",!0),i(s,c,h0,"(","\\lparen",!0),i(s,c,u0,")","\\rparen",!0),i(S,c,x,"<","\\textless",!0),i(S,c,x,">","\\textgreater",!0),i(s,c,h0,"⌊","\\lfloor",!0),i(s,c,u0,"⌋","\\rfloor",!0),i(s,c,h0,"⌈","\\lceil",!0),i(s,c,u0,"⌉","\\rceil",!0),i(s,c,x,"\\","\\backslash"),i(s,c,x,"∣","|"),i(s,c,x,"∣","\\vert"),i(S,c,x,"|","\\textbar",!0),i(s,c,x,"∥","\\|"),i(s,c,x,"∥","\\Vert"),i(S,c,x,"∥","\\textbardbl"),i(S,c,x,"~","\\textasciitilde"),i(S,c,x,"\\","\\textbackslash"),i(S,c,x,"^","\\textasciicircum"),i(s,c,v,"↑","\\uparrow",!0),i(s,c,v,"⇑","\\Uparrow",!0),i(s,c,v,"↓","\\downarrow",!0),i(s,c,v,"⇓","\\Downarrow",!0),i(s,c,v,"↕","\\updownarrow",!0),i(s,c,v,"⇕","\\Updownarrow",!0),i(s,c,a0,"∐","\\coprod"),i(s,c,a0,"⋁","\\bigvee"),i(s,c,a0,"⋀","\\bigwedge"),i(s,c,a0,"⨄","\\biguplus"),i(s,c,a0,"⋂","\\bigcap"),i(s,c,a0,"⋃","\\bigcup"),i(s,c,a0,"∫","\\int"),i(s,c,a0,"∫","\\intop"),i(s,c,a0,"∬","\\iint"),i(s,c,a0,"∭","\\iiint"),i(s,c,a0,"∏","\\prod"),i(s,c,a0,"∑","\\sum"),i(s,c,a0,"⨂","\\bigotimes"),i(s,c,a0,"⨁","\\bigoplus"),i(s,c,a0,"⨀","\\bigodot"),i(s,c,a0,"∮","\\oint"),i(s,c,a0,"∯","\\oiint"),i(s,c,a0,"∰","\\oiiint"),i(s,c,a0,"⨆","\\bigsqcup"),i(s,c,a0,"∫","\\smallint"),i(S,c,ie,"…","\\textellipsis"),i(s,c,ie,"…","\\mathellipsis"),i(S,c,ie,"…","\\ldots",!0),i(s,c,ie,"…","\\ldots",!0),i(s,c,ie,"⋯","\\@cdots",!0),i(s,c,ie,"⋱","\\ddots",!0),i(s,c,x,"⋮","\\varvdots"),i(s,c,e0,"ˊ","\\acute"),i(s,c,e0,"ˋ","\\grave"),i(s,c,e0,"¨","\\ddot"),i(s,c,e0,"~","\\tilde"),i(s,c,e0,"ˉ","\\bar"),i(s,c,e0,"˘","\\breve"),i(s,c,e0,"ˇ","\\check"),i(s,c,e0,"^","\\hat"),i(s,c,e0,"⃗","\\vec"),i(s,c,e0,"˙","\\dot"),i(s,c,e0,"˚","\\mathring"),i(s,c,F,"","\\@imath"),i(s,c,F,"","\\@jmath"),i(s,c,x,"ı","ı"),i(s,c,x,"ȷ","ȷ"),i(S,c,x,"ı","\\i",!0),i(S,c,x,"ȷ","\\j",!0),i(S,c,x,"ß","\\ss",!0),i(S,c,x,"æ","\\ae",!0),i(S,c,x,"œ","\\oe",!0),i(S,c,x,"ø","\\o",!0),i(S,c,x,"Æ","\\AE",!0),i(S,c,x,"Œ","\\OE",!0),i(S,c,x,"Ø","\\O",!0),i(S,c,e0,"ˊ","\\'"),i(S,c,e0,"ˋ","\\`"),i(S,c,e0,"ˆ","\\^"),i(S,c,e0,"˜","\\~"),i(S,c,e0,"ˉ","\\="),i(S,c,e0,"˘","\\u"),i(S,c,e0,"˙","\\."),i(S,c,e0,"˚","\\r"),i(S,c,e0,"ˇ","\\v"),i(S,c,e0,"¨",'\\"'),i(S,c,e0,"˝","\\H"),i(S,c,e0,"◯","\\textcircled");var Zt={"--":!0,"---":!0,"``":!0,"''":!0};i(S,c,x,"–","--",!0),i(S,c,x,"–","\\textendash"),i(S,c,x,"—","---",!0),i(S,c,x,"—","\\textemdash"),i(S,c,x,"‘","`",!0),i(S,c,x,"‘","\\textquoteleft"),i(S,c,x,"’","'",!0),i(S,c,x,"’","\\textquoteright"),i(S,c,x,"“","``",!0),i(S,c,x,"“","\\textquotedblleft"),i(S,c,x,"”","''",!0),i(S,c,x,"”","\\textquotedblright"),i(s,c,x,"°","\\degree",!0),i(S,c,x,"°","\\degree"),i(S,c,x,"°","\\textdegree",!0),i(s,c,x,"£","\\pounds"),i(s,c,x,"£","\\mathsterling",!0),i(S,c,x,"£","\\pounds"),i(S,c,x,"£","\\textsterling",!0),i(s,p,x,"✠","\\maltese"),i(S,p,x,"✠","\\maltese");for(var Kt='0123456789/@."',tt=0;tt<Kt.length;tt++){var Jt=Kt.charAt(tt);i(s,c,x,Jt,Jt)}for(var Qt='0123456789!@*()-=+";:?/.,',rt=0;rt<Qt.length;rt++){var _t=Qt.charAt(rt);i(S,c,x,_t,_t)}for(var Be="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",nt=0;nt<Be.length;nt++){var Ce=Be.charAt(nt);i(s,c,F,Ce,Ce),i(S,c,x,Ce,Ce)}i(s,p,x,"C","ℂ"),i(S,p,x,"C","ℂ"),i(s,p,x,"H","ℍ"),i(S,p,x,"H","ℍ"),i(s,p,x,"N","ℕ"),i(S,p,x,"N","ℕ"),i(s,p,x,"P","ℙ"),i(S,p,x,"P","ℙ"),i(s,p,x,"Q","ℚ"),i(S,p,x,"Q","ℚ"),i(s,p,x,"R","ℝ"),i(S,p,x,"R","ℝ"),i(s,p,x,"Z","ℤ"),i(S,p,x,"Z","ℤ"),i(s,c,F,"h","ℎ"),i(S,c,F,"h","ℎ");for(var I="",m0=0;m0<Be.length;m0++){var i0=Be.charAt(m0);I=String.fromCharCode(55349,56320+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56372+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56424+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56580+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56736+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56788+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56840+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56944+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),m0<26&&(I=String.fromCharCode(55349,56632+m0),i(s,c,F,i0,I),i(S,c,x,i0,I),I=String.fromCharCode(55349,56476+m0),i(s,c,F,i0,I),i(S,c,x,i0,I))}I=String.fromCharCode(55349,56668),i(s,c,F,"k",I),i(S,c,x,"k",I);for(var J0=0;J0<10;J0++){var G0=J0.toString();I=String.fromCharCode(55349,57294+J0),i(s,c,F,G0,I),i(S,c,x,G0,I),I=String.fromCharCode(55349,57314+J0),i(s,c,F,G0,I),i(S,c,x,G0,I),I=String.fromCharCode(55349,57324+J0),i(s,c,F,G0,I),i(S,c,x,G0,I),I=String.fromCharCode(55349,57334+J0),i(s,c,F,G0,I),i(S,c,x,G0,I)}for(var at="ÇÐÞçþ",it=0;it<at.length;it++){var Te=at.charAt(it);i(s,c,F,Te,Te),i(S,c,x,Te,Te)}var De=[["mathbf","textbf","Main-Bold"],["mathbf","textbf","Main-Bold"],["mathnormal","textit","Math-Italic"],["mathnormal","textit","Math-Italic"],["boldsymbol","boldsymbol","Main-BoldItalic"],["boldsymbol","boldsymbol","Main-BoldItalic"],["mathscr","textscr","Script-Regular"],["","",""],["","",""],["","",""],["mathfrak","textfrak","Fraktur-Regular"],["mathfrak","textfrak","Fraktur-Regular"],["mathbb","textbb","AMS-Regular"],["mathbb","textbb","AMS-Regular"],["","",""],["","",""],["mathsf","textsf","SansSerif-Regular"],["mathsf","textsf","SansSerif-Regular"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathitsf","textitsf","SansSerif-Italic"],["mathitsf","textitsf","SansSerif-Italic"],["","",""],["","",""],["mathtt","texttt","Typewriter-Regular"],["mathtt","texttt","Typewriter-Regular"]],er=[["mathbf","textbf","Main-Bold"],["","",""],["mathsf","textsf","SansSerif-Regular"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathtt","texttt","Typewriter-Regular"]],Qn=u(function(e,t){var r=e.charCodeAt(0),n=e.charCodeAt(1),a=(r-55296)*1024+(n-56320)+65536,o=t==="math"?0:1;if(119808<=a&&a<120484){var m=Math.floor((a-119808)/26);return[De[m][2],De[m][o]]}else if(120782<=a&&a<=120831){var h=Math.floor((a-120782)/10);return[er[h][2],er[h][o]]}else{if(a===120485||a===120486)return[De[0][2],De[0][o]];if(120486<a&&a<120782)return["",""];throw new k("Unsupported character: "+e)}},"wideCharacterFont"),_n=[[1,1,1],[2,1,1],[3,1,1],[4,2,1],[5,2,1],[6,3,1],[7,4,2],[8,6,3],[9,7,6],[10,8,7],[11,10,9]],tr=[.5,.6,.7,.8,.9,1,1.2,1.44,1.728,2.074,2.488],rr=u(function(e,t){return t.size<2?e:_n[e-1][t.size-1]},"sizeAtStyle"),nr=function(){function l(t){this.style=void 0,this.color=void 0,this.size=void 0,this.textSize=void 0,this.phantom=void 0,this.font=void 0,this.fontFamily=void 0,this.fontWeight=void 0,this.fontShape=void 0,this.sizeMultiplier=void 0,this.maxSize=void 0,this.minRuleThickness=void 0,this._fontMetrics=void 0,this.style=t.style,this.color=t.color,this.size=t.size||l.BASESIZE,this.textSize=t.textSize||this.size,this.phantom=!!t.phantom,this.font=t.font||"",this.fontFamily=t.fontFamily||"",this.fontWeight=t.fontWeight||"",this.fontShape=t.fontShape||"",this.sizeMultiplier=tr[this.size-1],this.maxSize=t.maxSize,this.minRuleThickness=t.minRuleThickness,this._fontMetrics=void 0}u(l,"Options");var e=l.prototype;return e.extend=u(function(r){var n={style:this.style,size:this.size,textSize:this.textSize,color:this.color,phantom:this.phantom,font:this.font,fontFamily:this.fontFamily,fontWeight:this.fontWeight,fontShape:this.fontShape,maxSize:this.maxSize,minRuleThickness:this.minRuleThickness};for(var a in r)r.hasOwnProperty(a)&&(n[a]=r[a]);return new l(n)},"extend"),e.havingStyle=u(function(r){return this.style===r?this:this.extend({style:r,size:rr(this.textSize,r)})},"havingStyle"),e.havingCrampedStyle=u(function(){return this.havingStyle(this.style.cramp())},"havingCrampedStyle"),e.havingSize=u(function(r){return this.size===r&&this.textSize===r?this:this.extend({style:this.style.text(),size:r,textSize:r,sizeMultiplier:tr[r-1]})},"havingSize"),e.havingBaseStyle=u(function(r){r=r||this.style.text();var n=rr(l.BASESIZE,r);return this.size===n&&this.textSize===l.BASESIZE&&this.style===r?this:this.extend({style:r,size:n})},"havingBaseStyle"),e.havingBaseSizing=u(function(){var r;switch(this.style.id){case 4:case 5:r=3;break;case 6:case 7:r=1;break;default:r=6}return this.extend({style:this.style.text(),size:r})},"havingBaseSizing"),e.withColor=u(function(r){return this.extend({color:r})},"withColor"),e.withPhantom=u(function(){return this.extend({phantom:!0})},"withPhantom"),e.withFont=u(function(r){return this.extend({font:r})},"withFont"),e.withTextFontFamily=u(function(r){return this.extend({fontFamily:r,font:""})},"withTextFontFamily"),e.withTextFontWeight=u(function(r){return this.extend({fontWeight:r,font:""})},"withTextFontWeight"),e.withTextFontShape=u(function(r){return this.extend({fontShape:r,font:""})},"withTextFontShape"),e.sizingClasses=u(function(r){return r.size!==this.size?["sizing","reset-size"+r.size,"size"+this.size]:[]},"sizingClasses"),e.baseSizingClasses=u(function(){return this.size!==l.BASESIZE?["sizing","reset-size"+this.size,"size"+l.BASESIZE]:[]},"baseSizingClasses"),e.fontMetrics=u(function(){return this._fontMetrics||(this._fontMetrics=Zn(this.size)),this._fontMetrics},"fontMetrics"),e.getColor=u(function(){return this.phantom?"transparent":this.color},"getColor"),l}();nr.BASESIZE=6;var ea=nr,ot={pt:1,mm:7227/2540,cm:7227/254,in:72.27,bp:803/800,pc:12,dd:1238/1157,cc:14856/1157,nd:685/642,nc:1370/107,sp:1/65536,px:803/800},ta={ex:!0,em:!0,mu:!0},ar=u(function(e){return typeof e!="string"&&(e=e.unit),e in ot||e in ta||e==="ex"},"validUnit"),t0=u(function(e,t){var r;if(e.unit in ot)r=ot[e.unit]/t.fontMetrics().ptPerEm/t.sizeMultiplier;else if(e.unit==="mu")r=t.fontMetrics().cssEmPerMu;else{var n;if(t.style.isTight()?n=t.havingStyle(t.style.text()):n=t,e.unit==="ex")r=n.fontMetrics().xHeight;else if(e.unit==="em")r=n.fontMetrics().quad;else throw new k("Invalid unit: '"+e.unit+"'");n!==t&&(r*=n.sizeMultiplier/t.sizeMultiplier)}return Math.min(e.number*r,t.maxSize)},"calculateSize"),Ne=u(function(e,t,r){return _[r][e]&&_[r][e].replace&&(e=_[r][e].replace),{value:e,metrics:_e(e,t,r)}},"lookupSymbol"),S0=u(function(e,t,r,n,a){var o=Ne(e,t,r),m=o.metrics;e=o.value;var h;if(m){var f=m.italic;(r==="text"||n&&n.font==="mathit")&&(f=0),h=new x0(e,m.height,m.depth,f,m.skew,m.width,a)}else typeof console<"u"&&console.warn("No character metrics "+("for '"+e+"' in style '"+t+"' and mode '"+r+"'")),h=new x0(e,0,0,0,0,0,a);if(n){h.maxFontSize=n.sizeMultiplier,n.style.isTight()&&h.classes.push("mtight");var g=n.getColor();g&&(h.style.color=g)}return h},"makeSymbol"),ra=u(function(e,t,r,n){return n===void 0&&(n=[]),r.font==="boldsymbol"&&Ne(e,"Main-Bold",t).metrics?S0(e,"Main-Bold",t,r,n.concat(["mathbf"])):e==="\\"||_[t][e].font==="main"?S0(e,"Main-Regular",t,r,n):S0(e,"AMS-Regular",t,r,n.concat(["amsrm"]))},"mathsym"),na=u(function(e,t,r,n,a){return a!=="textord"&&Ne(e,"Math-BoldItalic",t).metrics?{fontName:"Math-BoldItalic",fontClass:"boldsymbol"}:{fontName:"Main-Bold",fontClass:"mathbf"}},"boldsymbol"),aa=u(function(e,t,r){var n=e.mode,a=e.text,o=["mord"],m=n==="math"||n==="text"&&t.font,h=m?t.font:t.fontFamily;if(a.charCodeAt(0)===55349){var f=Qn(a,n),g=f[0],y=f[1];return S0(a,g,n,t,o.concat(y))}else if(h){var w,A;if(h==="boldsymbol"){var z=na(a,n,t,o,r);w=z.fontName,A=[z.fontClass]}else m?(w=lr[h].fontName,A=[h]):(w=Ee(h,t.fontWeight,t.fontShape),A=[h,t.fontWeight,t.fontShape]);if(Ne(a,w,n).metrics)return S0(a,w,n,t,o.concat(A));if(Zt.hasOwnProperty(a)&&w.substr(0,10)==="Typewriter"){for(var C=[],B=0;B<a.length;B++)C.push(S0(a[B],w,n,t,o.concat(A)));return or(C)}}if(r==="mathord")return S0(a,"Math-Italic",n,t,o.concat(["mathnormal"]));if(r==="textord"){var N=_[n][a]&&_[n][a].font;if(N==="ams"){var O=Ee("amsrm",t.fontWeight,t.fontShape);return S0(a,O,n,t,o.concat("amsrm",t.fontWeight,t.fontShape))}else if(N==="main"||!N){var q=Ee("textrm",t.fontWeight,t.fontShape);return S0(a,q,n,t,o.concat(t.fontWeight,t.fontShape))}else{var X=Ee(N,t.fontWeight,t.fontShape);return S0(a,X,n,t,o.concat(X,t.fontWeight,t.fontShape))}}else throw new Error("unexpected type: "+r+" in makeOrd")},"makeOrd"),ia=u(function(e,t){if(H0(e.classes)!==H0(t.classes)||e.skew!==t.skew||e.maxFontSize!==t.maxFontSize)return!1;if(e.classes.length===1){var r=e.classes[0];if(r==="mbin"||r==="mord")return!1}for(var n in e.style)if(e.style.hasOwnProperty(n)&&e.style[n]!==t.style[n])return!1;for(var a in t.style)if(t.style.hasOwnProperty(a)&&e.style[a]!==t.style[a])return!1;return!0},"canCombine"),oa=u(function(e){for(var t=0;t<e.length-1;t++){var r=e[t],n=e[t+1];r instanceof x0&&n instanceof x0&&ia(r,n)&&(r.text+=n.text,r.height=Math.max(r.height,n.height),r.depth=Math.max(r.depth,n.depth),r.italic=n.italic,e.splice(t+1,1),t--)}return e},"tryCombineChars"),lt=u(function(e){for(var t=0,r=0,n=0,a=0;a<e.children.length;a++){var o=e.children[a];o.height>t&&(t=o.height),o.depth>r&&(r=o.depth),o.maxFontSize>n&&(n=o.maxFontSize)}e.height=t,e.depth=r,e.maxFontSize=n},"sizeElementFromChildren"),c0=u(function(e,t,r,n){var a=new pe(e,t,r,n);return lt(a),a},"makeSpan"),ir=u(function(e,t,r,n){return new pe(e,t,r,n)},"makeSvgSpan"),la=u(function(e,t,r){var n=c0([e],[],t);return n.height=Math.max(r||t.fontMetrics().defaultRuleThickness,t.minRuleThickness),n.style.borderBottomWidth=n.height+"em",n.maxFontSize=1,n},"makeLineSpan"),sa=u(function(e,t,r,n){var a=new Je(e,t,r,n);return lt(a),a},"makeAnchor"),or=u(function(e){var t=new fe(e);return lt(t),t},"makeFragment"),ua=u(function(e,t){return e instanceof fe?c0([],[e],t):e},"wrapFragment"),ma=u(function(e){if(e.positionType==="individualShift"){for(var t=e.children,r=[t[0]],n=-t[0].shift-t[0].elem.depth,a=n,o=1;o<t.length;o++){var m=-t[o].shift-a-t[o].elem.depth,h=m-(t[o-1].elem.height+t[o-1].elem.depth);a=a+m,r.push({type:"kern",size:h}),r.push(t[o])}return{children:r,depth:n}}var f;if(e.positionType==="top"){for(var g=e.positionData,y=0;y<e.children.length;y++){var w=e.children[y];g-=w.type==="kern"?w.size:w.elem.height+w.elem.depth}f=g}else if(e.positionType==="bottom")f=-e.positionData;else{var A=e.children[0];if(A.type!=="elem")throw new Error('First child must have type "elem".');if(e.positionType==="shift")f=-A.elem.depth-e.positionData;else if(e.positionType==="firstBaseline")f=-A.elem.depth;else throw new Error("Invalid positionType "+e.positionType+".")}return{children:e.children,depth:f}},"getVListChildrenAndDepth"),ca=u(function(e,t){for(var r=ma(e),n=r.children,a=r.depth,o=0,m=0;m<n.length;m++){var h=n[m];if(h.type==="elem"){var f=h.elem;o=Math.max(o,f.maxFontSize,f.height)}}o+=2;var g=c0(["pstrut"],[]);g.style.height=o+"em";for(var y=[],w=a,A=a,z=a,C=0;C<n.length;C++){var B=n[C];if(B.type==="kern")z+=B.size;else{var N=B.elem,O=B.wrapperClasses||[],q=B.wrapperStyle||{},X=c0(O,[g,N],void 0,q);X.style.top=-o-z-N.depth+"em",B.marginLeft&&(X.style.marginLeft=B.marginLeft),B.marginRight&&(X.style.marginRight=B.marginRight),y.push(X),z+=N.height+N.depth}w=Math.min(w,z),A=Math.max(A,z)}var j=c0(["vlist"],y);j.style.height=A+"em";var Q;if(w<0){var Y=c0([],[]),$=c0(["vlist"],[Y]);$.style.height=-w+"em";var n0=c0(["vlist-s"],[new x0("​")]);Q=[c0(["vlist-r"],[j,n0]),c0(["vlist-r"],[$])]}else Q=[c0(["vlist-r"],[j])];var K=c0(["vlist-t"],Q);return Q.length===2&&K.classes.push("vlist-t2"),K.height=A,K.depth=-w,K},"makeVList"),ha=u(function(e,t){var r=c0(["mspace"],[],t),n=t0(e,t);return r.style.marginRight=n+"em",r},"makeGlue"),Ee=u(function(e,t,r){var n="";switch(e){case"amsrm":n="AMS";break;case"textrm":n="Main";break;case"textsf":n="SansSerif";break;case"texttt":n="Typewriter";break;default:n=e}var a;return t==="textbf"&&r==="textit"?a="BoldItalic":t==="textbf"?a="Bold":t==="textit"?a="Italic":a="Regular",n+"-"+a},"retrieveTextFontName"),lr={mathbf:{variant:"bold",fontName:"Main-Bold"},mathrm:{variant:"normal",fontName:"Main-Regular"},textit:{variant:"italic",fontName:"Main-Italic"},mathit:{variant:"italic",fontName:"Main-Italic"},mathnormal:{variant:"italic",fontName:"Math-Italic"},mathbb:{variant:"double-struck",fontName:"AMS-Regular"},mathcal:{variant:"script",fontName:"Caligraphic-Regular"},mathfrak:{variant:"fraktur",fontName:"Fraktur-Regular"},mathscr:{variant:"script",fontName:"Script-Regular"},mathsf:{variant:"sans-serif",fontName:"SansSerif-Regular"},mathtt:{variant:"monospace",fontName:"Typewriter-Regular"}},sr={vec:["vec",.471,.714],oiintSize1:["oiintSize1",.957,.499],oiintSize2:["oiintSize2",1.472,.659],oiiintSize1:["oiiintSize1",1.304,.499],oiiintSize2:["oiiintSize2",1.98,.659]},da=u(function(e,t){var r=sr[e],n=r[0],a=r[1],o=r[2],m=new K0(n),h=new P0([m],{width:a+"em",height:o+"em",style:"width:"+a+"em",viewBox:"0 0 "+1e3*a+" "+1e3*o,preserveAspectRatio:"xMinYMin"}),f=ir(["overlay"],[h],t);return f.height=o,f.style.height=o+"em",f.style.width=a+"em",f},"staticSvg"),b={fontMap:lr,makeSymbol:S0,mathsym:ra,makeSpan:c0,makeSvgSpan:ir,makeLineSpan:la,makeAnchor:sa,makeFragment:or,wrapFragment:ua,makeVList:ca,makeOrd:aa,makeGlue:ha,staticSvg:da,svgData:sr,tryCombineChars:oa},r0={number:3,unit:"mu"},Q0={number:4,unit:"mu"},q0={number:5,unit:"mu"},fa={mord:{mop:r0,mbin:Q0,mrel:q0,minner:r0},mop:{mord:r0,mop:r0,mrel:q0,minner:r0},mbin:{mord:Q0,mop:Q0,mopen:Q0,minner:Q0},mrel:{mord:q0,mop:q0,mopen:q0,minner:q0},mopen:{},mclose:{mop:r0,mbin:Q0,mrel:q0,minner:r0},mpunct:{mord:r0,mop:r0,mrel:q0,mopen:r0,mclose:r0,mpunct:r0,minner:r0},minner:{mord:r0,mop:r0,mbin:Q0,mrel:q0,mopen:r0,mpunct:r0,minner:r0}},pa={mord:{mop:r0},mop:{mord:r0,mop:r0},mbin:{},mrel:{},mopen:{},mclose:{mop:r0},mpunct:{},minner:{mop:r0}},ur={},Le={},qe={};function T(l){for(var e=l.type,t=l.names,r=l.props,n=l.handler,a=l.htmlBuilder,o=l.mathmlBuilder,m={type:e,numArgs:r.numArgs,argTypes:r.argTypes,allowedInArgument:!!r.allowedInArgument,allowedInText:!!r.allowedInText,allowedInMath:r.allowedInMath===void 0?!0:r.allowedInMath,numOptionalArgs:r.numOptionalArgs||0,infix:!!r.infix,primitive:!!r.primitive,handler:n},h=0;h<t.length;++h)ur[t[h]]=m;e&&(a&&(Le[e]=a),o&&(qe[e]=o))}u(T,"defineFunction");function _0(l){var e=l.type,t=l.htmlBuilder,r=l.mathmlBuilder;T({type:e,names:[],props:{numArgs:0},handler:u(function(){throw new Error("Should never be called.")},"handler"),htmlBuilder:t,mathmlBuilder:r})}u(_0,"defineFunctionBuilders");var Fe=u(function(e){return e.type==="ordgroup"&&e.body.length===1?e.body[0]:e},"normalizeArgument"),o0=u(function(e){return e.type==="ordgroup"?e.body:[e]},"ordargument"),F0=b.makeSpan,va=["leftmost","mbin","mopen","mrel","mop","mpunct"],ga=["rightmost","mrel","mclose","mpunct"],xa={display:R.DISPLAY,text:R.TEXT,script:R.SCRIPT,scriptscript:R.SCRIPTSCRIPT},ya={mord:"mord",mop:"mop",mbin:"mbin",mrel:"mrel",mopen:"mopen",mclose:"mclose",mpunct:"mpunct",minner:"minner"},l0=u(function(e,t,r,n){n===void 0&&(n=[null,null]);for(var a=[],o=0;o<e.length;o++){var m=U(e[o],t);if(m instanceof fe){var h=m.children;a.push.apply(a,h)}else a.push(m)}if(b.tryCombineChars(a),!r)return a;var f=t;if(e.length===1){var g=e[0];g.type==="sizing"?f=t.havingSize(g.size):g.type==="styling"&&(f=t.havingStyle(xa[g.style]))}var y=F0([n[0]||"leftmost"],[],t),w=F0([n[1]||"rightmost"],[],t),A=r==="root";return mr(a,function(z,C){var B=C.classes[0],N=z.classes[0];B==="mbin"&&E.contains(ga,N)?C.classes[0]="mord":N==="mbin"&&E.contains(va,B)&&(z.classes[0]="mord")},{node:y},w,A),mr(a,function(z,C){var B=st(C),N=st(z),O=B&&N?z.hasClass("mtight")?pa[B][N]:fa[B][N]:null;if(O)return b.makeGlue(O,f)},{node:y},w,A),a},"buildExpression"),mr=u(function l(e,t,r,n,a){n&&e.push(n);for(var o=0;o<e.length;o++){var m=e[o],h=cr(m);if(h){l(h.children,t,r,null,a);continue}var f=!m.hasClass("mspace");if(f){var g=t(m,r.node);g&&(r.insertAfter?r.insertAfter(g):(e.unshift(g),o++))}f?r.node=m:a&&m.hasClass("newline")&&(r.node=F0(["leftmost"])),r.insertAfter=function(y){return function(w){e.splice(y+1,0,w),o++}}(o)}n&&e.pop()},"traverseNonSpaceNodes"),cr=u(function(e){return e instanceof fe||e instanceof Je||e instanceof pe&&e.hasClass("enclosing")?e:null},"checkPartialGroup"),ba=u(function l(e,t){var r=cr(e);if(r){var n=r.children;if(n.length){if(t==="right")return l(n[n.length-1],"right");if(t==="left")return l(n[0],"left")}}return e},"getOutermostNode"),st=u(function(e,t){return e?(t&&(e=ba(e,t)),ya[e.classes[0]]||null):null},"getTypeOfDomTree"),ve=u(function(e,t){var r=["nulldelimiter"].concat(e.baseSizingClasses());return F0(t.concat(r))},"makeNullDelimiter"),U=u(function(e,t,r){if(!e)return F0();if(Le[e.type]){var n=Le[e.type](e,t);if(r&&t.size!==r.size){n=F0(t.sizingClasses(r),[n],t);var a=t.sizeMultiplier/r.sizeMultiplier;n.height*=a,n.depth*=a}return n}else throw new k("Got group of unknown type: '"+e.type+"'")},"buildGroup");function Re(l,e){var t=F0(["base"],l,e),r=F0(["strut"]);return r.style.height=t.height+t.depth+"em",r.style.verticalAlign=-t.depth+"em",t.children.unshift(r),t}u(Re,"buildHTMLUnbreakable");function ut(l,e){var t=null;l.length===1&&l[0].type==="tag"&&(t=l[0].tag,l=l[0].body);var r=l0(l,e,"root"),n;r.length===2&&r[1].hasClass("tag")&&(n=r.pop());for(var a=[],o=[],m=0;m<r.length;m++)if(o.push(r[m]),r[m].hasClass("mbin")||r[m].hasClass("mrel")||r[m].hasClass("allowbreak")){for(var h=!1;m<r.length-1&&r[m+1].hasClass("mspace")&&!r[m+1].hasClass("newline");)m++,o.push(r[m]),r[m].hasClass("nobreak")&&(h=!0);h||(a.push(Re(o,e)),o=[])}else r[m].hasClass("newline")&&(o.pop(),o.length>0&&(a.push(Re(o,e)),o=[]),a.push(r[m]));o.length>0&&a.push(Re(o,e));var f;t?(f=Re(l0(t,e,!0)),f.classes=["tag"],a.push(f)):n&&a.push(n);var g=F0(["katex-html"],a);if(g.setAttribute("aria-hidden","true"),f){var y=f.children[0];y.style.height=g.height+g.depth+"em",y.style.verticalAlign=-g.depth+"em"}return g}u(ut,"buildHTML");function hr(l){return new fe(l)}u(hr,"newDocumentFragment");var y0=function(){function l(t,r,n){this.type=void 0,this.attributes=void 0,this.children=void 0,this.classes=void 0,this.type=t,this.attributes={},this.children=r||[],this.classes=n||[]}u(l,"MathNode");var e=l.prototype;return e.setAttribute=u(function(r,n){this.attributes[r]=n},"setAttribute"),e.getAttribute=u(function(r){return this.attributes[r]},"getAttribute"),e.toNode=u(function(){var r=document.createElementNS("http://www.w3.org/1998/Math/MathML",this.type);for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&r.setAttribute(n,this.attributes[n]);this.classes.length>0&&(r.className=H0(this.classes));for(var a=0;a<this.children.length;a++)r.appendChild(this.children[a].toNode());return r},"toNode"),e.toMarkup=u(function(){var r="<"+this.type;for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+'="',r+=E.escape(this.attributes[n]),r+='"');this.classes.length>0&&(r+=' class ="'+E.escape(H0(this.classes))+'"'),r+=">";for(var a=0;a<this.children.length;a++)r+=this.children[a].toMarkup();return r+="</"+this.type+">",r},"toMarkup"),e.toText=u(function(){return this.children.map(function(r){return r.toText()}).join("")},"toText"),l}(),ge=function(){function l(t){this.text=void 0,this.text=t}u(l,"TextNode");var e=l.prototype;return e.toNode=u(function(){return document.createTextNode(this.text)},"toNode"),e.toMarkup=u(function(){return E.escape(this.toText())},"toMarkup"),e.toText=u(function(){return this.text},"toText"),l}(),wa=function(){function l(t){this.width=void 0,this.character=void 0,this.width=t,t>=.05555&&t<=.05556?this.character=" ":t>=.1666&&t<=.1667?this.character=" ":t>=.2222&&t<=.2223?this.character=" ":t>=.2777&&t<=.2778?this.character="  ":t>=-.05556&&t<=-.05555?this.character=" ⁣":t>=-.1667&&t<=-.1666?this.character=" ⁣":t>=-.2223&&t<=-.2222?this.character=" ⁣":t>=-.2778&&t<=-.2777?this.character=" ⁣":this.character=null}u(l,"SpaceNode");var e=l.prototype;return e.toNode=u(function(){if(this.character)return document.createTextNode(this.character);var r=document.createElementNS("http://www.w3.org/1998/Math/MathML","mspace");return r.setAttribute("width",this.width+"em"),r},"toNode"),e.toMarkup=u(function(){return this.character?"<mtext>"+this.character+"</mtext>":'<mspace width="'+this.width+'em"/>'},"toMarkup"),e.toText=u(function(){return this.character?this.character:" "},"toText"),l}(),M={MathNode:y0,TextNode:ge,SpaceNode:wa,newDocumentFragment:hr},b0=u(function(e,t,r){return _[t][e]&&_[t][e].replace&&e.charCodeAt(0)!==55349&&!(Zt.hasOwnProperty(e)&&r&&(r.fontFamily&&r.fontFamily.substr(4,2)==="tt"||r.font&&r.font.substr(4,2)==="tt"))&&(e=_[t][e].replace),new M.TextNode(e)},"makeText"),mt=u(function(e){return e.length===1?e[0]:new M.MathNode("mrow",e)},"makeRow"),ct=u(function(e,t){if(t.fontFamily==="texttt")return"monospace";if(t.fontFamily==="textsf")return t.fontShape==="textit"&&t.fontWeight==="textbf"?"sans-serif-bold-italic":t.fontShape==="textit"?"sans-serif-italic":t.fontWeight==="textbf"?"bold-sans-serif":"sans-serif";if(t.fontShape==="textit"&&t.fontWeight==="textbf")return"bold-italic";if(t.fontShape==="textit")return"italic";if(t.fontWeight==="textbf")return"bold";var r=t.font;if(!r||r==="mathnormal")return null;var n=e.mode;if(r==="mathit")return"italic";if(r==="boldsymbol")return e.type==="textord"?"bold":"bold-italic";if(r==="mathbf")return"bold";if(r==="mathbb")return"double-struck";if(r==="mathfrak")return"fraktur";if(r==="mathscr"||r==="mathcal")return"script";if(r==="mathsf")return"sans-serif";if(r==="mathtt")return"monospace";var a=e.text;if(E.contains(["\\imath","\\jmath"],a))return null;_[n][a]&&_[n][a].replace&&(a=_[n][a].replace);var o=b.fontMap[r].fontName;return _e(a,o,n)?b.fontMap[r].variant:null},"getVariant"),d0=u(function(e,t,r){if(e.length===1){var n=Z(e[0],t);return r&&n instanceof y0&&n.type==="mo"&&(n.setAttribute("lspace","0em"),n.setAttribute("rspace","0em")),[n]}for(var a=[],o,m=0;m<e.length;m++){var h=Z(e[m],t);if(h instanceof y0&&o instanceof y0){if(h.type==="mtext"&&o.type==="mtext"&&h.getAttribute("mathvariant")===o.getAttribute("mathvariant")){var f;(f=o.children).push.apply(f,h.children);continue}else if(h.type==="mn"&&o.type==="mn"){var g;(g=o.children).push.apply(g,h.children);continue}else if(h.type==="mi"&&h.children.length===1&&o.type==="mn"){var y=h.children[0];if(y instanceof ge&&y.text==="."){var w;(w=o.children).push.apply(w,h.children);continue}}else if(o.type==="mi"&&o.children.length===1){var A=o.children[0];if(A instanceof ge&&A.text==="̸"&&(h.type==="mo"||h.type==="mi"||h.type==="mn")){var z=h.children[0];z instanceof ge&&z.text.length>0&&(z.text=z.text.slice(0,1)+"̸"+z.text.slice(1),a.pop())}}}a.push(h),o=h}return a},"buildExpression"),V0=u(function(e,t,r){return mt(d0(e,t,r))},"buildExpressionRow"),Z=u(function(e,t){if(!e)return new M.MathNode("mrow");if(qe[e.type]){var r=qe[e.type](e,t);return r}else throw new k("Got group of unknown type: '"+e.type+"'")},"buildGroup");function dr(l,e,t,r,n){var a=d0(l,t),o;a.length===1&&a[0]instanceof y0&&E.contains(["mrow","mtable"],a[0].type)?o=a[0]:o=new M.MathNode("mrow",a);var m=new M.MathNode("annotation",[new M.TextNode(e)]);m.setAttribute("encoding","application/x-tex");var h=new M.MathNode("semantics",[o,m]),f=new M.MathNode("math",[h]);f.setAttribute("xmlns","http://www.w3.org/1998/Math/MathML"),r&&f.setAttribute("display","block");var g=n?"katex":"katex-mathml";return b.makeSpan([g],[f])}u(dr,"buildMathML");var fr=u(function(e){return new ea({style:e.displayMode?R.DISPLAY:R.TEXT,maxSize:e.maxSize,minRuleThickness:e.minRuleThickness})},"optionsFromSettings"),pr=u(function(e,t){if(t.displayMode){var r=["katex-display"];t.leqno&&r.push("leqno"),t.fleqn&&r.push("fleqn"),e=b.makeSpan(r,[e])}return e},"displayWrap"),ka=u(function(e,t,r){var n=fr(r),a;if(r.output==="mathml")return dr(e,t,n,r.displayMode,!0);if(r.output==="html"){var o=ut(e,n);a=b.makeSpan(["katex"],[o])}else{var m=dr(e,t,n,r.displayMode,!1),h=ut(e,n);a=b.makeSpan(["katex"],[m,h])}return pr(a,r)},"buildTree"),Aa=u(function(e,t,r){var n=fr(r),a=ut(e,n),o=b.makeSpan(["katex"],[a]);return pr(o,r)},"buildHTMLTree"),Sa={widehat:"^",widecheck:"ˇ",widetilde:"~",utilde:"~",overleftarrow:"←",underleftarrow:"←",xleftarrow:"←",overrightarrow:"→",underrightarrow:"→",xrightarrow:"→",underbrace:"⏟",overbrace:"⏞",overgroup:"⏠",undergroup:"⏡",overleftrightarrow:"↔",underleftrightarrow:"↔",xleftrightarrow:"↔",Overrightarrow:"⇒",xRightarrow:"⇒",overleftharpoon:"↼",xleftharpoonup:"↼",overrightharpoon:"⇀",xrightharpoonup:"⇀",xLeftarrow:"⇐",xLeftrightarrow:"⇔",xhookleftarrow:"↩",xhookrightarrow:"↪",xmapsto:"↦",xrightharpoondown:"⇁",xleftharpoondown:"↽",xrightleftharpoons:"⇌",xleftrightharpoons:"⇋",xtwoheadleftarrow:"↞",xtwoheadrightarrow:"↠",xlongequal:"=",xtofrom:"⇄",xrightleftarrows:"⇄",xrightequilibrium:"⇌",xleftequilibrium:"⇋","\\\\cdrightarrow":"→","\\\\cdleftarrow":"←","\\\\cdlongequal":"="},za=u(function(e){var t=new M.MathNode("mo",[new M.TextNode(Sa[e])]);return t.setAttribute("stretchy","true"),t},"mathMLnode"),Ma={overrightarrow:[["rightarrow"],.888,522,"xMaxYMin"],overleftarrow:[["leftarrow"],.888,522,"xMinYMin"],underrightarrow:[["rightarrow"],.888,522,"xMaxYMin"],underleftarrow:[["leftarrow"],.888,522,"xMinYMin"],xrightarrow:[["rightarrow"],1.469,522,"xMaxYMin"],"\\cdrightarrow":[["rightarrow"],3,522,"xMaxYMin"],xleftarrow:[["leftarrow"],1.469,522,"xMinYMin"],"\\cdleftarrow":[["leftarrow"],3,522,"xMinYMin"],Overrightarrow:[["doublerightarrow"],.888,560,"xMaxYMin"],xRightarrow:[["doublerightarrow"],1.526,560,"xMaxYMin"],xLeftarrow:[["doubleleftarrow"],1.526,560,"xMinYMin"],overleftharpoon:[["leftharpoon"],.888,522,"xMinYMin"],xleftharpoonup:[["leftharpoon"],.888,522,"xMinYMin"],xleftharpoondown:[["leftharpoondown"],.888,522,"xMinYMin"],overrightharpoon:[["rightharpoon"],.888,522,"xMaxYMin"],xrightharpoonup:[["rightharpoon"],.888,522,"xMaxYMin"],xrightharpoondown:[["rightharpoondown"],.888,522,"xMaxYMin"],xlongequal:[["longequal"],.888,334,"xMinYMin"],"\\cdlongequal":[["longequal"],3,334,"xMinYMin"],xtwoheadleftarrow:[["twoheadleftarrow"],.888,334,"xMinYMin"],xtwoheadrightarrow:[["twoheadrightarrow"],.888,334,"xMaxYMin"],overleftrightarrow:[["leftarrow","rightarrow"],.888,522],overbrace:[["leftbrace","midbrace","rightbrace"],1.6,548],underbrace:[["leftbraceunder","midbraceunder","rightbraceunder"],1.6,548],underleftrightarrow:[["leftarrow","rightarrow"],.888,522],xleftrightarrow:[["leftarrow","rightarrow"],1.75,522],xLeftrightarrow:[["doubleleftarrow","doublerightarrow"],1.75,560],xrightleftharpoons:[["leftharpoondownplus","rightharpoonplus"],1.75,716],xleftrightharpoons:[["leftharpoonplus","rightharpoondownplus"],1.75,716],xhookleftarrow:[["leftarrow","righthook"],1.08,522],xhookrightarrow:[["lefthook","rightarrow"],1.08,522],overlinesegment:[["leftlinesegment","rightlinesegment"],.888,522],underlinesegment:[["leftlinesegment","rightlinesegment"],.888,522],overgroup:[["leftgroup","rightgroup"],.888,342],undergroup:[["leftgroupunder","rightgroupunder"],.888,342],xmapsto:[["leftmapsto","rightarrow"],1.5,522],xtofrom:[["leftToFrom","rightToFrom"],1.75,528],xrightleftarrows:[["baraboveleftarrow","rightarrowabovebar"],1.75,901],xrightequilibrium:[["baraboveshortleftharpoon","rightharpoonaboveshortbar"],1.75,716],xleftequilibrium:[["shortbaraboveleftharpoon","shortrightharpoonabovebar"],1.75,716]},Ba=u(function(e){return e.type==="ordgroup"?e.body.length:1},"groupLength"),Ca=u(function(e,t){function r(){var h=4e5,f=e.label.substr(1);if(E.contains(["widehat","widecheck","widetilde","utilde"],f)){var g=e,y=Ba(g.base),w,A,z;if(y>5)f==="widehat"||f==="widecheck"?(w=420,h=2364,z=.42,A=f+"4"):(w=312,h=2340,z=.34,A="tilde4");else{var C=[1,1,2,2,3,3][y];f==="widehat"||f==="widecheck"?(h=[0,1062,2364,2364,2364][C],w=[0,239,300,360,420][C],z=[0,.24,.3,.3,.36,.42][C],A=f+C):(h=[0,600,1033,2339,2340][C],w=[0,260,286,306,312][C],z=[0,.26,.286,.3,.306,.34][C],A="tilde"+C)}var B=new K0(A),N=new P0([B],{width:"100%",height:z+"em",viewBox:"0 0 "+h+" "+w,preserveAspectRatio:"none"});return{span:b.makeSvgSpan([],[N],t),minWidth:0,height:z}}else{var O=[],q=Ma[f],X=q[0],j=q[1],Q=q[2],Y=Q/1e3,$=X.length,n0,K;if($===1){var f0=q[3];n0=["hide-tail"],K=[f0]}else if($===2)n0=["halfarrow-left","halfarrow-right"],K=["xMinYMin","xMaxYMin"];else if($===3)n0=["brace-left","brace-center","brace-right"],K=["xMinYMin","xMidYMin","xMaxYMin"];else throw new Error(`Correct katexImagesData or update code here to support
                    `+$+" children.");for(var p0=0;p0<$;p0++){var Y0=new K0(X[p0]),E0=new P0([Y0],{width:"400em",height:Y+"em",viewBox:"0 0 "+h+" "+Q,preserveAspectRatio:K[p0]+" slice"}),v0=b.makeSvgSpan([n0[p0]],[E0],t);if($===1)return{span:v0,minWidth:j,height:Y};v0.style.height=Y+"em",O.push(v0)}return{span:b.makeSpan(["stretchy"],O,t),minWidth:j,height:Y}}}u(r,"buildSvgSpan_");var n=r(),a=n.span,o=n.minWidth,m=n.height;return a.height=m,a.style.height=m+"em",o>0&&(a.style.minWidth=o+"em"),a},"svgSpan"),Ta=u(function(e,t,r,n,a){var o,m=e.height+e.depth+r+n;if(/fbox|color|angl/.test(t)){if(o=b.makeSpan(["stretchy",t],[],a),t==="fbox"){var h=a.color&&a.getColor();h&&(o.style.borderColor=h)}}else{var f=[];/^[bx]cancel$/.test(t)&&f.push(new Qe({x1:"0",y1:"0",x2:"100%",y2:"100%","stroke-width":"0.046em"})),/^x?cancel$/.test(t)&&f.push(new Qe({x1:"0",y1:"100%",x2:"100%",y2:"0","stroke-width":"0.046em"}));var g=new P0(f,{width:"100%",height:m+"em"});o=b.makeSvgSpan([],[g],a)}return o.height=m,o.style.height=m+"em",o},"encloseSpan"),R0={encloseSpan:Ta,mathMLnode:za,svgSpan:Ca};function G(l,e){if(!l||l.type!==e)throw new Error("Expected node of type "+e+", but got "+(l?"node of type "+l.type:String(l)));return l}u(G,"assertNodeType");function ht(l){var e=Oe(l);if(!e)throw new Error("Expected node of symbol group type, but got "+(l?"node of type "+l.type:String(l)));return e}u(ht,"assertSymbolNodeType");function Oe(l){return l&&(l.type==="atom"||Jn.hasOwnProperty(l.type))?l:null}u(Oe,"checkSymbolNodeType");var dt=u(function(e,t){var r,n,a;e&&e.type==="supsub"?(n=G(e.base,"accent"),r=n.base,e.base=r,a=$n(U(e,t)),e.base=n):(n=G(e,"accent"),r=n.base);var o=U(r,t.havingCrampedStyle()),m=n.isShifty&&E.isCharacterBox(r),h=0;if(m){var f=E.getBaseElem(r),g=U(f,t.havingCrampedStyle());h=$t(g).skew}var y=Math.min(o.height,t.fontMetrics().xHeight),w;if(n.isStretchy)w=R0.svgSpan(n,t),w=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:o},{type:"elem",elem:w,wrapperClasses:["svg-align"],wrapperStyle:h>0?{width:"calc(100% - "+2*h+"em)",marginLeft:2*h+"em"}:void 0}]},t);else{var A,z;n.label==="\\vec"?(A=b.staticSvg("vec",t),z=b.svgData.vec[1]):(A=b.makeOrd({mode:n.mode,text:n.label},t,"textord"),A=$t(A),A.italic=0,z=A.width),w=b.makeSpan(["accent-body"],[A]);var C=n.label==="\\textcircled";C&&(w.classes.push("accent-full"),y=o.height);var B=h;C||(B-=z/2),w.style.left=B+"em",n.label==="\\textcircled"&&(w.style.top=".2em"),w=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:o},{type:"kern",size:-y},{type:"elem",elem:w}]},t)}var N=b.makeSpan(["mord","accent"],[w],t);return a?(a.children[0]=N,a.height=Math.max(N.height,a.height),a.classes[0]="mord",a):N},"htmlBuilder"),vr=u(function(e,t){var r=e.isStretchy?R0.mathMLnode(e.label):new M.MathNode("mo",[b0(e.label,e.mode)]),n=new M.MathNode("mover",[Z(e.base,t),r]);return n.setAttribute("accent","true"),n},"mathmlBuilder"),Da=new RegExp(["\\acute","\\grave","\\ddot","\\tilde","\\bar","\\breve","\\check","\\hat","\\vec","\\dot","\\mathring"].map(function(l){return"\\"+l}).join("|"));T({type:"accent",names:["\\acute","\\grave","\\ddot","\\tilde","\\bar","\\breve","\\check","\\hat","\\vec","\\dot","\\mathring","\\widecheck","\\widehat","\\widetilde","\\overrightarrow","\\overleftarrow","\\Overrightarrow","\\overleftrightarrow","\\overgroup","\\overlinesegment","\\overleftharpoon","\\overrightharpoon"],props:{numArgs:1},handler:u(function(e,t){var r=Fe(t[0]),n=!Da.test(e.funcName),a=!n||e.funcName==="\\widehat"||e.funcName==="\\widetilde"||e.funcName==="\\widecheck";return{type:"accent",mode:e.parser.mode,label:e.funcName,isStretchy:n,isShifty:a,base:r}},"handler"),htmlBuilder:dt,mathmlBuilder:vr}),T({type:"accent",names:["\\'","\\`","\\^","\\~","\\=","\\u","\\.",'\\"',"\\r","\\H","\\v","\\textcircled"],props:{numArgs:1,allowedInText:!0,allowedInMath:!1,argTypes:["primitive"]},handler:u(function(e,t){var r=t[0];return{type:"accent",mode:e.parser.mode,label:e.funcName,isStretchy:!1,isShifty:!0,base:r}},"handler"),htmlBuilder:dt,mathmlBuilder:vr}),T({type:"accentUnder",names:["\\underleftarrow","\\underrightarrow","\\underleftrightarrow","\\undergroup","\\underlinesegment","\\utilde"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"accentUnder",mode:r.mode,label:n,base:a}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.base,t),n=R0.svgSpan(e,t),a=e.label==="\\utilde"?.12:0,o=b.makeVList({positionType:"top",positionData:r.height,children:[{type:"elem",elem:n,wrapperClasses:["svg-align"]},{type:"kern",size:a},{type:"elem",elem:r}]},t);return b.makeSpan(["mord","accentunder"],[o],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=R0.mathMLnode(e.label),n=new M.MathNode("munder",[Z(e.base,t),r]);return n.setAttribute("accentunder","true"),n},"mathmlBuilder")});var Ie=u(function(e){var t=new M.MathNode("mpadded",e?[e]:[]);return t.setAttribute("width","+0.6em"),t.setAttribute("lspace","0.3em"),t},"paddedNode");T({type:"xArrow",names:["\\xleftarrow","\\xrightarrow","\\xLeftarrow","\\xRightarrow","\\xleftrightarrow","\\xLeftrightarrow","\\xhookleftarrow","\\xhookrightarrow","\\xmapsto","\\xrightharpoondown","\\xrightharpoonup","\\xleftharpoondown","\\xleftharpoonup","\\xrightleftharpoons","\\xleftrightharpoons","\\xlongequal","\\xtwoheadrightarrow","\\xtwoheadleftarrow","\\xtofrom","\\xrightleftarrows","\\xrightequilibrium","\\xleftequilibrium","\\\\cdrightarrow","\\\\cdleftarrow","\\\\cdlongequal"],props:{numArgs:1,numOptionalArgs:1},handler:u(function(e,t,r){var n=e.parser,a=e.funcName;return{type:"xArrow",mode:n.mode,label:a,body:t[0],below:r[0]}},"handler"),htmlBuilder:u(function(e,t){var r=t.style,n=t.havingStyle(r.sup()),a=b.wrapFragment(U(e.body,n,t),t),o=e.label.slice(0,2)==="\\x"?"x":"cd";a.classes.push(o+"-arrow-pad");var m;e.below&&(n=t.havingStyle(r.sub()),m=b.wrapFragment(U(e.below,n,t),t),m.classes.push(o+"-arrow-pad"));var h=R0.svgSpan(e,t),f=-t.fontMetrics().axisHeight+.5*h.height,g=-t.fontMetrics().axisHeight-.5*h.height-.111;(a.depth>.25||e.label==="\\xleftequilibrium")&&(g-=a.depth);var y;if(m){var w=-t.fontMetrics().axisHeight+m.height+.5*h.height+.111;y=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:a,shift:g},{type:"elem",elem:h,shift:f},{type:"elem",elem:m,shift:w}]},t)}else y=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:a,shift:g},{type:"elem",elem:h,shift:f}]},t);return y.children[0].children[0].children[1].classes.push("svg-align"),b.makeSpan(["mrel","x-arrow"],[y],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=R0.mathMLnode(e.label);r.setAttribute("minsize",e.label.charAt(0)==="x"?"1.75em":"3.0em");var n;if(e.body){var a=Ie(Z(e.body,t));if(e.below){var o=Ie(Z(e.below,t));n=new M.MathNode("munderover",[r,o,a])}else n=new M.MathNode("mover",[r,a])}else if(e.below){var m=Ie(Z(e.below,t));n=new M.MathNode("munder",[r,m])}else n=Ie(),n=new M.MathNode("mover",[r,n]);return n},"mathmlBuilder")});var Na={">":"\\\\cdrightarrow","<":"\\\\cdleftarrow","=":"\\\\cdlongequal",A:"\\uparrow",V:"\\downarrow","|":"\\Vert",".":"no arrow"},gr=u(function(){return{type:"styling",body:[],mode:"math",style:"display"}},"newCell"),xr=u(function(e){return e.type==="textord"&&e.text==="@"},"isStartOfArrow"),Ea=u(function(e,t){return(e.type==="mathord"||e.type==="atom")&&e.text===t},"isLabelEnd");function La(l,e,t){var r=Na[l];switch(r){case"\\\\cdrightarrow":case"\\\\cdleftarrow":return t.callFunction(r,[e[0]],[e[1]]);case"\\uparrow":case"\\downarrow":{var n=t.callFunction("\\\\cdleft",[e[0]],[]),a={type:"atom",text:r,mode:"math",family:"rel"},o=t.callFunction("\\Big",[a],[]),m=t.callFunction("\\\\cdright",[e[1]],[]),h={type:"ordgroup",mode:"math",body:[n,o,m]};return t.callFunction("\\\\cdparent",[h],[])}case"\\\\cdlongequal":return t.callFunction("\\\\cdlongequal",[],[]);case"\\Vert":{var f={type:"textord",text:"\\Vert",mode:"math"};return t.callFunction("\\Big",[f],[])}default:return{type:"textord",text:" ",mode:"math"}}}u(La,"cdArrow");function qa(l){var e=[];for(l.gullet.beginGroup(),l.gullet.macros.set("\\cr","\\\\\\relax"),l.gullet.beginGroup();;){e.push(l.parseExpression(!1,"\\\\")),l.gullet.endGroup(),l.gullet.beginGroup();var t=l.fetch().text;if(t==="&"||t==="\\\\")l.consume();else if(t==="\\end"){e[e.length-1].length===0&&e.pop();break}else throw new k("Expected \\\\ or \\cr or \\end",l.nextToken)}for(var r=[],n=[r],a=0;a<e.length;a++){for(var o=e[a],m=gr(),h=0;h<o.length;h++)if(!xr(o[h]))m.body.push(o[h]);else{r.push(m),h+=1;var f=ht(o[h]).text,g=new Array(2);if(g[0]={type:"ordgroup",mode:"math",body:[]},g[1]={type:"ordgroup",mode:"math",body:[]},!("=|.".indexOf(f)>-1))if("<>AV".indexOf(f)>-1)for(var y=0;y<2;y++){for(var w=!0,A=h+1;A<o.length;A++){if(Ea(o[A],f)){w=!1,h=A;break}if(xr(o[A]))throw new k("Missing a "+f+" character to complete a CD arrow.",o[A]);g[y].body.push(o[A])}if(w)throw new k("Missing a "+f+" character to complete a CD arrow.",o[h])}else throw new k('Expected one of "<>AV=|." after @',o[h]);var z=La(f,g,l),C={type:"styling",body:[z],mode:"math",style:"display"};r.push(C),m=gr()}a%2===0?r.push(m):r.shift(),r=[],n.push(r)}l.gullet.endGroup(),l.gullet.endGroup();var B=new Array(n[0].length).fill({type:"align",align:"c",pregap:.25,postgap:.25});return{type:"array",mode:"math",body:n,arraystretch:1,addJot:!0,rowGaps:[null],cols:B,colSeparationType:"CD",hLinesBeforeRow:new Array(n.length+1).fill([])}}u(qa,"parseCD"),T({type:"cdlabel",names:["\\\\cdleft","\\\\cdright"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=e.funcName;return{type:"cdlabel",mode:r.mode,side:n.slice(4),label:t[0]}},"handler"),htmlBuilder:u(function(e,t){var r=t.havingStyle(t.style.sup()),n=b.wrapFragment(U(e.label,r,t),t);return n.classes.push("cd-label-"+e.side),n.style.bottom=.8-n.depth+"em",n.height=0,n.depth=0,n},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mrow",[Z(e.label,t)]);return r=new M.MathNode("mpadded",[r]),r.setAttribute("width","0"),e.side==="left"&&r.setAttribute("lspace","-1width"),r.setAttribute("voffset","0.7em"),r=new M.MathNode("mstyle",[r]),r.setAttribute("displaystyle","false"),r.setAttribute("scriptlevel","1"),r},"mathmlBuilder")}),T({type:"cdlabelparent",names:["\\\\cdparent"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser;return{type:"cdlabelparent",mode:r.mode,fragment:t[0]}},"handler"),htmlBuilder:u(function(e,t){var r=b.wrapFragment(U(e.fragment,t),t);return r.classes.push("cd-vert-arrow"),r},"htmlBuilder"),mathmlBuilder:u(function(e,t){return new M.MathNode("mrow",[Z(e.fragment,t)])},"mathmlBuilder")}),T({type:"textord",names:["\\@char"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){for(var r=e.parser,n=G(t[0],"ordgroup"),a=n.body,o="",m=0;m<a.length;m++){var h=G(a[m],"textord");o+=h.text}var f=parseInt(o);if(isNaN(f))throw new k("\\@char has non-numeric argument "+o);return{type:"textord",mode:r.mode,text:String.fromCharCode(f)}},"handler")});var yr=u(function(e,t){var r=l0(e.body,t.withColor(e.color),!1);return b.makeFragment(r)},"htmlBuilder"),br=u(function(e,t){var r=d0(e.body,t.withColor(e.color)),n=new M.MathNode("mstyle",r);return n.setAttribute("mathcolor",e.color),n},"mathmlBuilder");T({type:"color",names:["\\textcolor"],props:{numArgs:2,allowedInText:!0,argTypes:["color","original"]},handler:u(function(e,t){var r=e.parser,n=G(t[0],"color-token").color,a=t[1];return{type:"color",mode:r.mode,color:n,body:o0(a)}},"handler"),htmlBuilder:yr,mathmlBuilder:br}),T({type:"color",names:["\\color"],props:{numArgs:1,allowedInText:!0,argTypes:["color"]},handler:u(function(e,t){var r=e.parser,n=e.breakOnTokenText,a=G(t[0],"color-token").color;r.gullet.macros.set("\\current@color",a);var o=r.parseExpression(!0,n);return{type:"color",mode:r.mode,color:a,body:o}},"handler"),htmlBuilder:yr,mathmlBuilder:br}),T({type:"cr",names:["\\\\"],props:{numArgs:0,numOptionalArgs:1,argTypes:["size"],allowedInText:!0},handler:u(function(e,t,r){var n=e.parser,a=r[0],o=!n.settings.displayMode||!n.settings.useStrictBehavior("newLineInDisplayMode","In LaTeX, \\\\ or \\newline does nothing in display mode");return{type:"cr",mode:n.mode,newLine:o,size:a&&G(a,"size").value}},"handler"),htmlBuilder:u(function(e,t){var r=b.makeSpan(["mspace"],[],t);return e.newLine&&(r.classes.push("newline"),e.size&&(r.style.marginTop=t0(e.size,t)+"em")),r},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mspace");return e.newLine&&(r.setAttribute("linebreak","newline"),e.size&&r.setAttribute("height",t0(e.size,t)+"em")),r},"mathmlBuilder")});var ft={"\\global":"\\global","\\long":"\\\\globallong","\\\\globallong":"\\\\globallong","\\def":"\\gdef","\\gdef":"\\gdef","\\edef":"\\xdef","\\xdef":"\\xdef","\\let":"\\\\globallet","\\futurelet":"\\\\globalfuture"},wr=u(function(e){var t=e.text;if(/^(?:[\\{}$&#^_]|EOF)$/.test(t))throw new k("Expected a control sequence",e);return t},"checkControlSequence"),Fa=u(function(e){var t=e.gullet.popToken();return t.text==="="&&(t=e.gullet.popToken(),t.text===" "&&(t=e.gullet.popToken())),t},"getRHS"),kr=u(function(e,t,r,n){var a=e.gullet.macros.get(r.text);a==null&&(r.noexpand=!0,a={tokens:[r],numArgs:0,unexpandable:!e.gullet.isExpandable(r.text)}),e.gullet.macros.set(t,a,n)},"letCommand");T({type:"internal",names:["\\global","\\long","\\\\globallong"],props:{numArgs:0,allowedInText:!0},handler:u(function(e){var t=e.parser,r=e.funcName;t.consumeSpaces();var n=t.fetch();if(ft[n.text])return(r==="\\global"||r==="\\\\globallong")&&(n.text=ft[n.text]),G(t.parseFunction(),"internal");throw new k("Invalid token after macro prefix",n)},"handler")}),T({type:"internal",names:["\\def","\\gdef","\\edef","\\xdef"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:u(function(e){var t=e.parser,r=e.funcName,n=t.gullet.popToken(),a=n.text;if(/^(?:[\\{}$&#^_]|EOF)$/.test(a))throw new k("Expected a control sequence",n);for(var o=0,m,h=[[]];t.gullet.future().text!=="{";)if(n=t.gullet.popToken(),n.text==="#"){if(t.gullet.future().text==="{"){m=t.gullet.future(),h[o].push("{");break}if(n=t.gullet.popToken(),!/^[1-9]$/.test(n.text))throw new k('Invalid argument number "'+n.text+'"');if(parseInt(n.text)!==o+1)throw new k('Argument number "'+n.text+'" out of order');o++,h.push([])}else{if(n.text==="EOF")throw new k("Expected a macro definition");h[o].push(n.text)}var f=t.gullet.consumeArg(),g=f.tokens;return m&&g.unshift(m),(r==="\\edef"||r==="\\xdef")&&(g=t.gullet.expandTokens(g),g.reverse()),t.gullet.macros.set(a,{tokens:g,numArgs:o,delimiters:h},r===ft[r]),{type:"internal",mode:t.mode}},"handler")}),T({type:"internal",names:["\\let","\\\\globallet"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:u(function(e){var t=e.parser,r=e.funcName,n=wr(t.gullet.popToken());t.gullet.consumeSpaces();var a=Fa(t);return kr(t,n,a,r==="\\\\globallet"),{type:"internal",mode:t.mode}},"handler")}),T({type:"internal",names:["\\futurelet","\\\\globalfuture"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:u(function(e){var t=e.parser,r=e.funcName,n=wr(t.gullet.popToken()),a=t.gullet.popToken(),o=t.gullet.popToken();return kr(t,n,o,r==="\\\\globalfuture"),t.gullet.pushToken(o),t.gullet.pushToken(a),{type:"internal",mode:t.mode}},"handler")});var xe=u(function(e,t,r){var n=_.math[e]&&_.math[e].replace,a=_e(n||e,t,r);if(!a)throw new Error("Unsupported symbol "+e+" and font size "+t+".");return a},"getMetrics"),pt=u(function(e,t,r,n){var a=r.havingBaseStyle(t),o=b.makeSpan(n.concat(a.sizingClasses(r)),[e],r),m=a.sizeMultiplier/r.sizeMultiplier;return o.height*=m,o.depth*=m,o.maxFontSize=a.sizeMultiplier,o},"styleWrap"),Ar=u(function(e,t,r){var n=t.havingBaseStyle(r),a=(1-t.sizeMultiplier/n.sizeMultiplier)*t.fontMetrics().axisHeight;e.classes.push("delimcenter"),e.style.top=a+"em",e.height-=a,e.depth+=a},"centerSpan"),Ra=u(function(e,t,r,n,a,o){var m=b.makeSymbol(e,"Main-Regular",a,n),h=pt(m,t,n,o);return r&&Ar(h,n,t),h},"makeSmallDelim"),Oa=u(function(e,t,r,n){return b.makeSymbol(e,"Size"+t+"-Regular",r,n)},"mathrmSize"),Sr=u(function(e,t,r,n,a,o){var m=Oa(e,t,a,n),h=pt(b.makeSpan(["delimsizing","size"+t],[m],n),R.TEXT,n,o);return r&&Ar(h,n,R.TEXT),h},"makeLargeDelim"),vt=u(function(e,t,r){var n;t==="Size1-Regular"?n="delim-size1":n="delim-size4";var a=b.makeSpan(["delimsizinginner",n],[b.makeSpan([],[b.makeSymbol(e,t,r)])]);return{type:"elem",elem:a}},"makeGlyphSpan"),gt=u(function(e,t,r){var n=B0["Size4-Regular"][e.charCodeAt(0)]?B0["Size4-Regular"][e.charCodeAt(0)][4].toFixed(3):B0["Size1-Regular"][e.charCodeAt(0)][4].toFixed(3),a=new K0("inner",Un(e,Math.round(1e3*t))),o=new P0([a],{width:n+"em",height:t+"em",style:"width:"+n+"em",viewBox:"0 0 "+1e3*n+" "+Math.round(1e3*t),preserveAspectRatio:"xMinYMin"}),m=b.makeSvgSpan([],[o],r);return m.height=t,m.style.height=t+"em",m.style.width=n+"em",{type:"elem",elem:m}},"makeInner"),xt=.008,He={type:"kern",size:-1*xt},Ia=["|","\\lvert","\\rvert","\\vert"],Ha=["\\|","\\lVert","\\rVert","\\Vert"],zr=u(function(e,t,r,n,a,o){var m,h,f,g;m=f=g=e,h=null;var y="Size1-Regular";e==="\\uparrow"?f=g="⏐":e==="\\Uparrow"?f=g="‖":e==="\\downarrow"?m=f="⏐":e==="\\Downarrow"?m=f="‖":e==="\\updownarrow"?(m="\\uparrow",f="⏐",g="\\downarrow"):e==="\\Updownarrow"?(m="\\Uparrow",f="‖",g="\\Downarrow"):E.contains(Ia,e)?f="∣":E.contains(Ha,e)?f="∥":e==="["||e==="\\lbrack"?(m="⎡",f="⎢",g="⎣",y="Size4-Regular"):e==="]"||e==="\\rbrack"?(m="⎤",f="⎥",g="⎦",y="Size4-Regular"):e==="\\lfloor"||e==="⌊"?(f=m="⎢",g="⎣",y="Size4-Regular"):e==="\\lceil"||e==="⌈"?(m="⎡",f=g="⎢",y="Size4-Regular"):e==="\\rfloor"||e==="⌋"?(f=m="⎥",g="⎦",y="Size4-Regular"):e==="\\rceil"||e==="⌉"?(m="⎤",f=g="⎥",y="Size4-Regular"):e==="("||e==="\\lparen"?(m="⎛",f="⎜",g="⎝",y="Size4-Regular"):e===")"||e==="\\rparen"?(m="⎞",f="⎟",g="⎠",y="Size4-Regular"):e==="\\{"||e==="\\lbrace"?(m="⎧",h="⎨",g="⎩",f="⎪",y="Size4-Regular"):e==="\\}"||e==="\\rbrace"?(m="⎫",h="⎬",g="⎭",f="⎪",y="Size4-Regular"):e==="\\lgroup"||e==="⟮"?(m="⎧",g="⎩",f="⎪",y="Size4-Regular"):e==="\\rgroup"||e==="⟯"?(m="⎫",g="⎭",f="⎪",y="Size4-Regular"):e==="\\lmoustache"||e==="⎰"?(m="⎧",g="⎭",f="⎪",y="Size4-Regular"):(e==="\\rmoustache"||e==="⎱")&&(m="⎫",g="⎩",f="⎪",y="Size4-Regular");var w=xe(m,y,a),A=w.height+w.depth,z=xe(f,y,a),C=z.height+z.depth,B=xe(g,y,a),N=B.height+B.depth,O=0,q=1;if(h!==null){var X=xe(h,y,a);O=X.height+X.depth,q=2}var j=A+N+O,Q=Math.max(0,Math.ceil((t-j)/(q*C))),Y=j+Q*q*C,$=n.fontMetrics().axisHeight;r&&($*=n.sizeMultiplier);var n0=Y/2-$,K=[];if(K.push(vt(g,y,a)),K.push(He),h===null){var f0=Y-A-N+2*xt;K.push(gt(f,f0,n))}else{var p0=(Y-A-N-O)/2+2*xt;K.push(gt(f,p0,n)),K.push(He),K.push(vt(h,y,a)),K.push(He),K.push(gt(f,p0,n))}K.push(He),K.push(vt(m,y,a));var Y0=n.havingBaseStyle(R.TEXT),E0=b.makeVList({positionType:"bottom",positionData:n0,children:K},Y0);return pt(b.makeSpan(["delimsizing","mult"],[E0],Y0),R.TEXT,n,o)},"makeStackedDelim"),yt=80,bt=.08,wt=u(function(e,t,r,n,a){var o=Wn(e,n,r),m=new K0(e,o),h=new P0([m],{width:"400em",height:t+"em",viewBox:"0 0 400000 "+r,preserveAspectRatio:"xMinYMin slice"});return b.makeSvgSpan(["hide-tail"],[h],a)},"sqrtSvg"),Pa=u(function(e,t){var r=t.havingBaseSizing(),n=Tr("\\surd",e*r.sizeMultiplier,Cr,r),a=r.sizeMultiplier,o=Math.max(0,t.minRuleThickness-t.fontMetrics().sqrtRuleThickness),m,h=0,f=0,g=0,y;return n.type==="small"?(g=1e3+1e3*o+yt,e<1?a=1:e<1.4&&(a=.7),h=(1+o+bt)/a,f=(1+o)/a,m=wt("sqrtMain",h,g,o,t),m.style.minWidth="0.853em",y=.833/a):n.type==="large"?(g=(1e3+yt)*ye[n.size],f=(ye[n.size]+o)/a,h=(ye[n.size]+o+bt)/a,m=wt("sqrtSize"+n.size,h,g,o,t),m.style.minWidth="1.02em",y=1/a):(h=e+o+bt,f=e+o,g=Math.floor(1e3*e+o)+yt,m=wt("sqrtTall",h,g,o,t),m.style.minWidth="0.742em",y=1.056),m.height=f,m.style.height=h+"em",{span:m,advanceWidth:y,ruleWidth:(t.fontMetrics().sqrtRuleThickness+o)*a}},"makeSqrtImage"),Mr=["(","\\lparen",")","\\rparen","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","⌊","⌋","\\lceil","\\rceil","⌈","⌉","\\surd"],Ga=["\\uparrow","\\downarrow","\\updownarrow","\\Uparrow","\\Downarrow","\\Updownarrow","|","\\|","\\vert","\\Vert","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","⟮","⟯","\\lmoustache","\\rmoustache","⎰","⎱"],Br=["<",">","\\langle","\\rangle","/","\\backslash","\\lt","\\gt"],ye=[0,1.2,1.8,2.4,3],Va=u(function(e,t,r,n,a){if(e==="<"||e==="\\lt"||e==="⟨"?e="\\langle":(e===">"||e==="\\gt"||e==="⟩")&&(e="\\rangle"),E.contains(Mr,e)||E.contains(Br,e))return Sr(e,t,!1,r,n,a);if(E.contains(Ga,e))return zr(e,ye[t],!1,r,n,a);throw new k("Illegal delimiter: '"+e+"'")},"makeSizedDelim"),Wa=[{type:"small",style:R.SCRIPTSCRIPT},{type:"small",style:R.SCRIPT},{type:"small",style:R.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4}],Ua=[{type:"small",style:R.SCRIPTSCRIPT},{type:"small",style:R.SCRIPT},{type:"small",style:R.TEXT},{type:"stack"}],Cr=[{type:"small",style:R.SCRIPTSCRIPT},{type:"small",style:R.SCRIPT},{type:"small",style:R.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4},{type:"stack"}],Ya=u(function(e){if(e.type==="small")return"Main-Regular";if(e.type==="large")return"Size"+e.size+"-Regular";if(e.type==="stack")return"Size4-Regular";throw new Error("Add support for delim type '"+e.type+"' here.")},"delimTypeToFont"),Tr=u(function(e,t,r,n){for(var a=Math.min(2,3-n.style.size),o=a;o<r.length&&r[o].type!=="stack";o++){var m=xe(e,Ya(r[o]),"math"),h=m.height+m.depth;if(r[o].type==="small"){var f=n.havingBaseStyle(r[o].style);h*=f.sizeMultiplier}if(h>t)return r[o]}return r[r.length-1]},"traverseSequence"),Dr=u(function(e,t,r,n,a,o){e==="<"||e==="\\lt"||e==="⟨"?e="\\langle":(e===">"||e==="\\gt"||e==="⟩")&&(e="\\rangle");var m;E.contains(Br,e)?m=Wa:E.contains(Mr,e)?m=Cr:m=Ua;var h=Tr(e,t,m,n);return h.type==="small"?Ra(e,h.style,r,n,a,o):h.type==="large"?Sr(e,h.size,r,n,a,o):zr(e,t,r,n,a,o)},"makeCustomSizedDelim"),Xa=u(function(e,t,r,n,a,o){var m=n.fontMetrics().axisHeight*n.sizeMultiplier,h=901,f=5/n.fontMetrics().ptPerEm,g=Math.max(t-m,r+m),y=Math.max(g/500*h,2*g-f);return Dr(e,y,!0,n,a,o)},"makeLeftRightDelim"),C0={sqrtImage:Pa,sizedDelim:Va,sizeToMaxHeight:ye,customSizedDelim:Dr,leftRightDelim:Xa},Nr={"\\bigl":{mclass:"mopen",size:1},"\\Bigl":{mclass:"mopen",size:2},"\\biggl":{mclass:"mopen",size:3},"\\Biggl":{mclass:"mopen",size:4},"\\bigr":{mclass:"mclose",size:1},"\\Bigr":{mclass:"mclose",size:2},"\\biggr":{mclass:"mclose",size:3},"\\Biggr":{mclass:"mclose",size:4},"\\bigm":{mclass:"mrel",size:1},"\\Bigm":{mclass:"mrel",size:2},"\\biggm":{mclass:"mrel",size:3},"\\Biggm":{mclass:"mrel",size:4},"\\big":{mclass:"mord",size:1},"\\Big":{mclass:"mord",size:2},"\\bigg":{mclass:"mord",size:3},"\\Bigg":{mclass:"mord",size:4}},$a=["(","\\lparen",")","\\rparen","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","⌊","⌋","\\lceil","\\rceil","⌈","⌉","<",">","\\langle","⟨","\\rangle","⟩","\\lt","\\gt","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","⟮","⟯","\\lmoustache","\\rmoustache","⎰","⎱","/","\\backslash","|","\\vert","\\|","\\Vert","\\uparrow","\\Uparrow","\\downarrow","\\Downarrow","\\updownarrow","\\Updownarrow","."];function Pe(l,e){var t=Oe(l);if(t&&E.contains($a,t.text))return t;throw t?new k("Invalid delimiter '"+t.text+"' after '"+e.funcName+"'",l):new k("Invalid delimiter type '"+l.type+"'",l)}u(Pe,"checkDelimiter"),T({type:"delimsizing",names:["\\bigl","\\Bigl","\\biggl","\\Biggl","\\bigr","\\Bigr","\\biggr","\\Biggr","\\bigm","\\Bigm","\\biggm","\\Biggm","\\big","\\Big","\\bigg","\\Bigg"],props:{numArgs:1,argTypes:["primitive"]},handler:u(function(e,t){var r=Pe(t[0],e);return{type:"delimsizing",mode:e.parser.mode,size:Nr[e.funcName].size,mclass:Nr[e.funcName].mclass,delim:r.text}},"handler"),htmlBuilder:u(function(e,t){return e.delim==="."?b.makeSpan([e.mclass]):C0.sizedDelim(e.delim,e.size,t,e.mode,[e.mclass])},"htmlBuilder"),mathmlBuilder:u(function(e){var t=[];e.delim!=="."&&t.push(b0(e.delim,e.mode));var r=new M.MathNode("mo",t);return e.mclass==="mopen"||e.mclass==="mclose"?r.setAttribute("fence","true"):r.setAttribute("fence","false"),r.setAttribute("stretchy","true"),r.setAttribute("minsize",C0.sizeToMaxHeight[e.size]+"em"),r.setAttribute("maxsize",C0.sizeToMaxHeight[e.size]+"em"),r},"mathmlBuilder")});function Er(l){if(!l.body)throw new Error("Bug: The leftright ParseNode wasn't fully parsed.")}u(Er,"assertParsed"),T({type:"leftright-right",names:["\\right"],props:{numArgs:1,primitive:!0},handler:u(function(e,t){var r=e.parser.gullet.macros.get("\\current@color");if(r&&typeof r!="string")throw new k("\\current@color set to non-string in \\right");return{type:"leftright-right",mode:e.parser.mode,delim:Pe(t[0],e).text,color:r}},"handler")}),T({type:"leftright",names:["\\left"],props:{numArgs:1,primitive:!0},handler:u(function(e,t){var r=Pe(t[0],e),n=e.parser;++n.leftrightDepth;var a=n.parseExpression(!1);--n.leftrightDepth,n.expect("\\right",!1);var o=G(n.parseFunction(),"leftright-right");return{type:"leftright",mode:n.mode,body:a,left:r.text,right:o.delim,rightColor:o.color}},"handler"),htmlBuilder:u(function(e,t){Er(e);for(var r=l0(e.body,t,!0,["mopen","mclose"]),n=0,a=0,o=!1,m=0;m<r.length;m++)r[m].isMiddle?o=!0:(n=Math.max(r[m].height,n),a=Math.max(r[m].depth,a));n*=t.sizeMultiplier,a*=t.sizeMultiplier;var h;if(e.left==="."?h=ve(t,["mopen"]):h=C0.leftRightDelim(e.left,n,a,t,e.mode,["mopen"]),r.unshift(h),o)for(var f=1;f<r.length;f++){var g=r[f],y=g.isMiddle;y&&(r[f]=C0.leftRightDelim(y.delim,n,a,y.options,e.mode,[]))}var w;if(e.right===".")w=ve(t,["mclose"]);else{var A=e.rightColor?t.withColor(e.rightColor):t;w=C0.leftRightDelim(e.right,n,a,A,e.mode,["mclose"])}return r.push(w),b.makeSpan(["minner"],r,t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){Er(e);var r=d0(e.body,t);if(e.left!=="."){var n=new M.MathNode("mo",[b0(e.left,e.mode)]);n.setAttribute("fence","true"),r.unshift(n)}if(e.right!=="."){var a=new M.MathNode("mo",[b0(e.right,e.mode)]);a.setAttribute("fence","true"),e.rightColor&&a.setAttribute("mathcolor",e.rightColor),r.push(a)}return mt(r)},"mathmlBuilder")}),T({type:"middle",names:["\\middle"],props:{numArgs:1,primitive:!0},handler:u(function(e,t){var r=Pe(t[0],e);if(!e.parser.leftrightDepth)throw new k("\\middle without preceding \\left",r);return{type:"middle",mode:e.parser.mode,delim:r.text}},"handler"),htmlBuilder:u(function(e,t){var r;if(e.delim===".")r=ve(t,[]);else{r=C0.sizedDelim(e.delim,1,t,e.mode,[]);var n={delim:e.delim,options:t};r.isMiddle=n}return r},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=e.delim==="\\vert"||e.delim==="|"?b0("|","text"):b0(e.delim,e.mode),n=new M.MathNode("mo",[r]);return n.setAttribute("fence","true"),n.setAttribute("lspace","0.05em"),n.setAttribute("rspace","0.05em"),n},"mathmlBuilder")});var kt=u(function(e,t){var r=b.wrapFragment(U(e.body,t),t),n=e.label.substr(1),a=t.sizeMultiplier,o,m=0,h=E.isCharacterBox(e.body);if(n==="sout")o=b.makeSpan(["stretchy","sout"]),o.height=t.fontMetrics().defaultRuleThickness/a,m=-.5*t.fontMetrics().xHeight;else if(n==="phase"){var f=t0({number:.6,unit:"pt"},t),g=t0({number:.35,unit:"ex"},t),y=t.havingBaseSizing();a=a/y.sizeMultiplier;var w=r.height+r.depth+f+g;r.style.paddingLeft=w/2+f+"em";var A=Math.floor(1e3*w*a),z=Gn(A),C=new P0([new K0("phase",z)],{width:"400em",height:A/1e3+"em",viewBox:"0 0 400000 "+A,preserveAspectRatio:"xMinYMin slice"});o=b.makeSvgSpan(["hide-tail"],[C],t),o.style.height=w+"em",m=r.depth+f+g}else{/cancel/.test(n)?h||r.classes.push("cancel-pad"):n==="angl"?r.classes.push("anglpad"):r.classes.push("boxpad");var B=0,N=0,O=0;/box/.test(n)?(O=Math.max(t.fontMetrics().fboxrule,t.minRuleThickness),B=t.fontMetrics().fboxsep+(n==="colorbox"?0:O),N=B):n==="angl"?(O=Math.max(t.fontMetrics().defaultRuleThickness,t.minRuleThickness),B=4*O,N=Math.max(0,.25-r.depth)):(B=h?.2:0,N=B),o=R0.encloseSpan(r,n,B,N,t),/fbox|boxed|fcolorbox/.test(n)?(o.style.borderStyle="solid",o.style.borderWidth=O+"em"):n==="angl"&&O!==.049&&(o.style.borderTopWidth=O+"em",o.style.borderRightWidth=O+"em"),m=r.depth+N,e.backgroundColor&&(o.style.backgroundColor=e.backgroundColor,e.borderColor&&(o.style.borderColor=e.borderColor))}var q;if(e.backgroundColor)q=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:o,shift:m},{type:"elem",elem:r,shift:0}]},t);else{var X=/cancel|phase/.test(n)?["svg-align"]:[];q=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:r,shift:0},{type:"elem",elem:o,shift:m,wrapperClasses:X}]},t)}return/cancel/.test(n)&&(q.height=r.height,q.depth=r.depth),/cancel/.test(n)&&!h?b.makeSpan(["mord","cancel-lap"],[q],t):b.makeSpan(["mord"],[q],t)},"htmlBuilder"),At=u(function(e,t){var r=0,n=new M.MathNode(e.label.indexOf("colorbox")>-1?"mpadded":"menclose",[Z(e.body,t)]);switch(e.label){case"\\cancel":n.setAttribute("notation","updiagonalstrike");break;case"\\bcancel":n.setAttribute("notation","downdiagonalstrike");break;case"\\phase":n.setAttribute("notation","phasorangle");break;case"\\sout":n.setAttribute("notation","horizontalstrike");break;case"\\fbox":n.setAttribute("notation","box");break;case"\\angl":n.setAttribute("notation","actuarial");break;case"\\fcolorbox":case"\\colorbox":if(r=t.fontMetrics().fboxsep*t.fontMetrics().ptPerEm,n.setAttribute("width","+"+2*r+"pt"),n.setAttribute("height","+"+2*r+"pt"),n.setAttribute("lspace",r+"pt"),n.setAttribute("voffset",r+"pt"),e.label==="\\fcolorbox"){var a=Math.max(t.fontMetrics().fboxrule,t.minRuleThickness);n.setAttribute("style","border: "+a+"em solid "+String(e.borderColor))}break;case"\\xcancel":n.setAttribute("notation","updiagonalstrike downdiagonalstrike");break}return e.backgroundColor&&n.setAttribute("mathbackground",e.backgroundColor),n},"mathmlBuilder");T({type:"enclose",names:["\\colorbox"],props:{numArgs:2,allowedInText:!0,argTypes:["color","text"]},handler:u(function(e,t,r){var n=e.parser,a=e.funcName,o=G(t[0],"color-token").color,m=t[1];return{type:"enclose",mode:n.mode,label:a,backgroundColor:o,body:m}},"handler"),htmlBuilder:kt,mathmlBuilder:At}),T({type:"enclose",names:["\\fcolorbox"],props:{numArgs:3,allowedInText:!0,argTypes:["color","color","text"]},handler:u(function(e,t,r){var n=e.parser,a=e.funcName,o=G(t[0],"color-token").color,m=G(t[1],"color-token").color,h=t[2];return{type:"enclose",mode:n.mode,label:a,backgroundColor:m,borderColor:o,body:h}},"handler"),htmlBuilder:kt,mathmlBuilder:At}),T({type:"enclose",names:["\\fbox"],props:{numArgs:1,argTypes:["hbox"],allowedInText:!0},handler:u(function(e,t){var r=e.parser;return{type:"enclose",mode:r.mode,label:"\\fbox",body:t[0]}},"handler")}),T({type:"enclose",names:["\\cancel","\\bcancel","\\xcancel","\\sout","\\phase"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"enclose",mode:r.mode,label:n,body:a}},"handler"),htmlBuilder:kt,mathmlBuilder:At}),T({type:"enclose",names:["\\angl"],props:{numArgs:1,argTypes:["hbox"],allowedInText:!1},handler:u(function(e,t){var r=e.parser;return{type:"enclose",mode:r.mode,label:"\\angl",body:t[0]}},"handler")});var Lr={};function T0(l){for(var e=l.type,t=l.names,r=l.props,n=l.handler,a=l.htmlBuilder,o=l.mathmlBuilder,m={type:e,numArgs:r.numArgs||0,allowedInText:!1,numOptionalArgs:0,handler:n},h=0;h<t.length;++h)Lr[t[h]]=m;a&&(Le[e]=a),o&&(qe[e]=o)}u(T0,"defineEnvironment");function qr(l){var e=[];l.consumeSpaces();for(var t=l.fetch().text;t==="\\hline"||t==="\\hdashline";)l.consume(),e.push(t==="\\hdashline"),l.consumeSpaces(),t=l.fetch().text;return e}u(qr,"getHLines");var Ge=u(function(e){var t=e.parser.settings;if(!t.displayMode)throw new k("{"+e.envName+"} can be used only in display mode.")},"validateAmsEnvironmentContext");function W0(l,e,t){var r=e.hskipBeforeAndAfter,n=e.addJot,a=e.cols,o=e.arraystretch,m=e.colSeparationType,h=e.addEqnNum,f=e.singleRow,g=e.maxNumCols,y=e.leqno;if(l.gullet.beginGroup(),f||l.gullet.macros.set("\\cr","\\\\\\relax"),!o){var w=l.gullet.expandMacroAsText("\\arraystretch");if(w==null)o=1;else if(o=parseFloat(w),!o||o<0)throw new k("Invalid \\arraystretch: "+w)}l.gullet.beginGroup();var A=[],z=[A],C=[],B=[];for(B.push(qr(l));;){var N=l.parseExpression(!1,f?"\\end":"\\\\");l.gullet.endGroup(),l.gullet.beginGroup(),N={type:"ordgroup",mode:l.mode,body:N},t&&(N={type:"styling",mode:l.mode,style:t,body:[N]}),A.push(N);var O=l.fetch().text;if(O==="&"){if(g&&A.length===g){if(f||m)throw new k("Too many tab characters: &",l.nextToken);l.settings.reportNonstrict("textEnv","Too few columns specified in the {array} column argument.")}l.consume()}else if(O==="\\end"){A.length===1&&N.type==="styling"&&N.body[0].body.length===0&&z.pop(),B.length<z.length+1&&B.push([]);break}else if(O==="\\\\"){l.consume();var q=void 0;l.gullet.future().text!==" "&&(q=l.parseSizeGroup(!0)),C.push(q?q.value:null),B.push(qr(l)),A=[],z.push(A)}else throw new k("Expected & or \\\\ or \\cr or \\end",l.nextToken)}return l.gullet.endGroup(),l.gullet.endGroup(),{type:"array",mode:l.mode,addJot:n,arraystretch:o,body:z,cols:a,rowGaps:C,hskipBeforeAndAfter:r,hLinesBeforeRow:B,colSeparationType:m,addEqnNum:h,leqno:y}}u(W0,"parseArray");function St(l){return l.substr(0,1)==="d"?"display":"text"}u(St,"dCellStyle");var D0=u(function(e,t){var r,n,a=e.body.length,o=e.hLinesBeforeRow,m=0,h=new Array(a),f=[],g=Math.max(t.fontMetrics().arrayRuleWidth,t.minRuleThickness),y=1/t.fontMetrics().ptPerEm,w=5*y;if(e.colSeparationType&&e.colSeparationType==="small"){var A=t.havingStyle(R.SCRIPT).sizeMultiplier;w=.2778*(A/t.sizeMultiplier)}var z=e.colSeparationType==="CD"?t0({number:3,unit:"ex"},t):12*y,C=3*y,B=e.arraystretch*z,N=.7*B,O=.3*B,q=0;function X(kn){for(var Ue=0;Ue<kn.length;++Ue)Ue>0&&(q+=.25),f.push({pos:q,isDashed:kn[Ue]})}for(u(X,"setHLinePos"),X(o[0]),r=0;r<e.body.length;++r){var j=e.body[r],Q=N,Y=O;m<j.length&&(m=j.length);var $=new Array(j.length);for(n=0;n<j.length;++n){var n0=U(j[n],t);Y<n0.depth&&(Y=n0.depth),Q<n0.height&&(Q=n0.height),$[n]=n0}var K=e.rowGaps[r],f0=0;K&&(f0=t0(K,t),f0>0&&(f0+=O,Y<f0&&(Y=f0),f0=0)),e.addJot&&(Y+=C),$.height=Q,$.depth=Y,q+=Q,$.pos=q,q+=Y+f0,h[r]=$,X(o[r+1])}var p0=q/2+t.fontMetrics().axisHeight,Y0=e.cols||[],E0=[],v0,le,xn=[];if(e.addEqnNum)for(r=0;r<a;++r){var Ot=h[r],yi=Ot.pos-p0,It=b.makeSpan(["eqn-num"],[],t);It.depth=Ot.depth,It.height=Ot.height,xn.push({type:"elem",elem:It,shift:yi})}for(n=0,le=0;n<m||le<Y0.length;++n,++le){for(var O0=Y0[le]||{},yn=!0;O0.type==="separator";){if(yn||(v0=b.makeSpan(["arraycolsep"],[]),v0.style.width=t.fontMetrics().doubleRuleSep+"em",E0.push(v0)),O0.separator==="|"||O0.separator===":"){var bi=O0.separator==="|"?"solid":"dashed",se=b.makeSpan(["vertical-separator"],[],t);se.style.height=q+"em",se.style.borderRightWidth=g+"em",se.style.borderRightStyle=bi,se.style.margin="0 -"+g/2+"em",se.style.verticalAlign=-(q-p0)+"em",E0.push(se)}else throw new k("Invalid separator type: "+O0.separator);le++,O0=Y0[le]||{},yn=!1}if(!(n>=m)){var ue=void 0;(n>0||e.hskipBeforeAndAfter)&&(ue=E.deflt(O0.pregap,w),ue!==0&&(v0=b.makeSpan(["arraycolsep"],[]),v0.style.width=ue+"em",E0.push(v0)));var me=[];for(r=0;r<a;++r){var Ve=h[r],We=Ve[n];if(We){var wi=Ve.pos-p0;We.depth=Ve.depth,We.height=Ve.height,me.push({type:"elem",elem:We,shift:wi})}}me=b.makeVList({positionType:"individualShift",children:me},t),me=b.makeSpan(["col-align-"+(O0.align||"c")],[me]),E0.push(me),(n<m-1||e.hskipBeforeAndAfter)&&(ue=E.deflt(O0.postgap,w),ue!==0&&(v0=b.makeSpan(["arraycolsep"],[]),v0.style.width=ue+"em",E0.push(v0)))}}if(h=b.makeSpan(["mtable"],E0),f.length>0){for(var ki=b.makeLineSpan("hline",t,g),Ai=b.makeLineSpan("hdashline",t,g),Ht=[{type:"elem",elem:h,shift:0}];f.length>0;){var bn=f.pop(),wn=bn.pos-p0;bn.isDashed?Ht.push({type:"elem",elem:Ai,shift:wn}):Ht.push({type:"elem",elem:ki,shift:wn})}h=b.makeVList({positionType:"individualShift",children:Ht},t)}if(e.addEqnNum){var Pt=b.makeVList({positionType:"individualShift",children:xn},t);return Pt=b.makeSpan(["tag"],[Pt],t),b.makeFragment([h,Pt])}else return b.makeSpan(["mord"],[h],t)},"htmlBuilder"),ja={c:"center ",l:"left ",r:"right "},N0=u(function(e,t){for(var r=[],n=new M.MathNode("mtd",[],["mtr-glue"]),a=new M.MathNode("mtd",[],["mml-eqn-num"]),o=0;o<e.body.length;o++){for(var m=e.body[o],h=[],f=0;f<m.length;f++)h.push(new M.MathNode("mtd",[Z(m[f],t)]));e.addEqnNum&&(h.unshift(n),h.push(n),e.leqno?h.unshift(a):h.push(a)),r.push(new M.MathNode("mtr",h))}var g=new M.MathNode("mtable",r),y=e.arraystretch===.5?.1:.16+e.arraystretch-1+(e.addJot?.09:0);g.setAttribute("rowspacing",y.toFixed(4)+"em");var w="",A="";if(e.cols&&e.cols.length>0){var z=e.cols,C="",B=!1,N=0,O=z.length;z[0].type==="separator"&&(w+="top ",N=1),z[z.length-1].type==="separator"&&(w+="bottom ",O-=1);for(var q=N;q<O;q++)z[q].type==="align"?(A+=ja[z[q].align],B&&(C+="none "),B=!0):z[q].type==="separator"&&B&&(C+=z[q].separator==="|"?"solid ":"dashed ",B=!1);g.setAttribute("columnalign",A.trim()),/[sd]/.test(C)&&g.setAttribute("columnlines",C.trim())}if(e.colSeparationType==="align"){for(var X=e.cols||[],j="",Q=1;Q<X.length;Q++)j+=Q%2?"0em ":"1em ";g.setAttribute("columnspacing",j.trim())}else e.colSeparationType==="alignat"||e.colSeparationType==="gather"?g.setAttribute("columnspacing","0em"):e.colSeparationType==="small"?g.setAttribute("columnspacing","0.2778em"):e.colSeparationType==="CD"?g.setAttribute("columnspacing","0.5em"):g.setAttribute("columnspacing","1em");var Y="",$=e.hLinesBeforeRow;w+=$[0].length>0?"left ":"",w+=$[$.length-1].length>0?"right ":"";for(var n0=1;n0<$.length-1;n0++)Y+=$[n0].length===0?"none ":$[n0][0]?"dashed ":"solid ";return/[sd]/.test(Y)&&g.setAttribute("rowlines",Y.trim()),w!==""&&(g=new M.MathNode("menclose",[g]),g.setAttribute("notation",w.trim())),e.arraystretch&&e.arraystretch<1&&(g=new M.MathNode("mstyle",[g]),g.setAttribute("scriptlevel","1")),g},"mathmlBuilder"),Fr=u(function(e,t){e.envName.indexOf("ed")===-1&&Ge(e);var r=[],n=e.envName.indexOf("at")>-1?"alignat":"align",a=W0(e.parser,{cols:r,addJot:!0,addEqnNum:e.envName==="align"||e.envName==="alignat",colSeparationType:n,maxNumCols:e.envName==="split"?2:void 0,leqno:e.parser.settings.leqno},"display"),o,m=0,h={type:"ordgroup",mode:e.mode,body:[]};if(t[0]&&t[0].type==="ordgroup"){for(var f="",g=0;g<t[0].body.length;g++){var y=G(t[0].body[g],"textord");f+=y.text}o=Number(f),m=o*2}var w=!m;a.body.forEach(function(B){for(var N=1;N<B.length;N+=2){var O=G(B[N],"styling"),q=G(O.body[0],"ordgroup");q.body.unshift(h)}if(w)m<B.length&&(m=B.length);else{var X=B.length/2;if(o<X)throw new k("Too many math in a row: "+("expected "+o+", but got "+X),B[0])}});for(var A=0;A<m;++A){var z="r",C=0;A%2===1?z="l":A>0&&w&&(C=1),r[A]={type:"align",align:z,pregap:C,postgap:0}}return a.colSeparationType=w?"align":"alignat",a},"alignedHandler");T0({type:"array",names:["array","darray"],props:{numArgs:1},handler:u(function(e,t){var r=Oe(t[0]),n=r?[t[0]]:G(t[0],"ordgroup").body,a=n.map(function(m){var h=ht(m),f=h.text;if("lcr".indexOf(f)!==-1)return{type:"align",align:f};if(f==="|")return{type:"separator",separator:"|"};if(f===":")return{type:"separator",separator:":"};throw new k("Unknown column alignment: "+f,m)}),o={cols:a,hskipBeforeAndAfter:!0,maxNumCols:a.length};return W0(e.parser,o,St(e.envName))},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["matrix","pmatrix","bmatrix","Bmatrix","vmatrix","Vmatrix","matrix*","pmatrix*","bmatrix*","Bmatrix*","vmatrix*","Vmatrix*"],props:{numArgs:0},handler:u(function(e){var t={matrix:null,pmatrix:["(",")"],bmatrix:["[","]"],Bmatrix:["\\{","\\}"],vmatrix:["|","|"],Vmatrix:["\\Vert","\\Vert"]}[e.envName.replace("*","")],r="c",n={hskipBeforeAndAfter:!1,cols:[{type:"align",align:r}]};if(e.envName.charAt(e.envName.length-1)==="*"){var a=e.parser;if(a.consumeSpaces(),a.fetch().text==="["){if(a.consume(),a.consumeSpaces(),r=a.fetch().text,"lcr".indexOf(r)===-1)throw new k("Expected l or c or r",a.nextToken);a.consume(),a.consumeSpaces(),a.expect("]"),a.consume(),n.cols=[{type:"align",align:r}]}}var o=W0(e.parser,n,St(e.envName));return o.cols=new Array(o.body[0].length).fill({type:"align",align:r}),t?{type:"leftright",mode:e.mode,body:[o],left:t[0],right:t[1],rightColor:void 0}:o},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["smallmatrix"],props:{numArgs:0},handler:u(function(e){var t={arraystretch:.5},r=W0(e.parser,t,"script");return r.colSeparationType="small",r},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["subarray"],props:{numArgs:1},handler:u(function(e,t){var r=Oe(t[0]),n=r?[t[0]]:G(t[0],"ordgroup").body,a=n.map(function(m){var h=ht(m),f=h.text;if("lc".indexOf(f)!==-1)return{type:"align",align:f};throw new k("Unknown column alignment: "+f,m)});if(a.length>1)throw new k("{subarray} can contain only one column");var o={cols:a,hskipBeforeAndAfter:!1,arraystretch:.5};if(o=W0(e.parser,o,"script"),o.body.length>0&&o.body[0].length>1)throw new k("{subarray} can contain only one column");return o},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["cases","dcases","rcases","drcases"],props:{numArgs:0},handler:u(function(e){var t={arraystretch:1.2,cols:[{type:"align",align:"l",pregap:0,postgap:1},{type:"align",align:"l",pregap:0,postgap:0}]},r=W0(e.parser,t,St(e.envName));return{type:"leftright",mode:e.mode,body:[r],left:e.envName.indexOf("r")>-1?".":"\\{",right:e.envName.indexOf("r")>-1?"\\}":".",rightColor:void 0}},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["align","align*","aligned","split"],props:{numArgs:0},handler:Fr,htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["gathered","gather","gather*"],props:{numArgs:0},handler:u(function(e){E.contains(["gather","gather*"],e.envName)&&Ge(e);var t={cols:[{type:"align",align:"c"}],addJot:!0,colSeparationType:"gather",addEqnNum:e.envName==="gather",leqno:e.parser.settings.leqno};return W0(e.parser,t,"display")},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["alignat","alignat*","alignedat"],props:{numArgs:1},handler:Fr,htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["equation","equation*"],props:{numArgs:0},handler:u(function(e){Ge(e);var t={addEqnNum:e.envName==="equation",singleRow:!0,maxNumCols:1,leqno:e.parser.settings.leqno};return W0(e.parser,t,"display")},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T0({type:"array",names:["CD"],props:{numArgs:0},handler:u(function(e){return Ge(e),qa(e.parser)},"handler"),htmlBuilder:D0,mathmlBuilder:N0}),T({type:"text",names:["\\hline","\\hdashline"],props:{numArgs:0,allowedInText:!0,allowedInMath:!0},handler:u(function(e,t){throw new k(e.funcName+" valid only within array environment")},"handler")});var Za=Lr,Rr=Za;T({type:"environment",names:["\\begin","\\end"],props:{numArgs:1,argTypes:["text"]},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];if(a.type!=="ordgroup")throw new k("Invalid environment name",a);for(var o="",m=0;m<a.body.length;++m)o+=G(a.body[m],"textord").text;if(n==="\\begin"){if(!Rr.hasOwnProperty(o))throw new k("No such environment: "+o,a);var h=Rr[o],f=r.parseArguments("\\begin{"+o+"}",h),g=f.args,y=f.optArgs,w={mode:r.mode,envName:o,parser:r},A=h.handler(w,g,y);r.expect("\\end",!1);var z=r.nextToken,C=G(r.parseFunction(),"environment");if(C.name!==o)throw new k("Mismatch: \\begin{"+o+"} matched by \\end{"+C.name+"}",z);return A}return{type:"environment",mode:r.mode,name:o,nameGroup:a}},"handler")});var Ka=b.makeSpan;function Or(l,e){var t=l0(l.body,e,!0);return Ka([l.mclass],t,e)}u(Or,"mclass_htmlBuilder");function Ir(l,e){var t,r=d0(l.body,e);return l.mclass==="minner"?M.newDocumentFragment(r):(l.mclass==="mord"?l.isCharacterBox?(t=r[0],t.type="mi"):t=new M.MathNode("mi",r):(l.isCharacterBox?(t=r[0],t.type="mo"):t=new M.MathNode("mo",r),l.mclass==="mbin"?(t.attributes.lspace="0.22em",t.attributes.rspace="0.22em"):l.mclass==="mpunct"?(t.attributes.lspace="0em",t.attributes.rspace="0.17em"):(l.mclass==="mopen"||l.mclass==="mclose")&&(t.attributes.lspace="0em",t.attributes.rspace="0em")),t)}u(Ir,"mclass_mathmlBuilder"),T({type:"mclass",names:["\\mathord","\\mathbin","\\mathrel","\\mathopen","\\mathclose","\\mathpunct","\\mathinner"],props:{numArgs:1,primitive:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"mclass",mode:r.mode,mclass:"m"+n.substr(5),body:o0(a),isCharacterBox:E.isCharacterBox(a)}},"handler"),htmlBuilder:Or,mathmlBuilder:Ir});var zt=u(function(e){var t=e.type==="ordgroup"&&e.body.length?e.body[0]:e;return t.type==="atom"&&(t.family==="bin"||t.family==="rel")?"m"+t.family:"mord"},"binrelClass");T({type:"mclass",names:["\\@binrel"],props:{numArgs:2},handler:u(function(e,t){var r=e.parser;return{type:"mclass",mode:r.mode,mclass:zt(t[0]),body:o0(t[1]),isCharacterBox:E.isCharacterBox(t[1])}},"handler")}),T({type:"mclass",names:["\\stackrel","\\overset","\\underset"],props:{numArgs:2},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[1],o=t[0],m;n!=="\\stackrel"?m=zt(a):m="mrel";var h={type:"op",mode:a.mode,limits:!0,alwaysHandleSupSub:!0,parentIsSupSub:!1,symbol:!1,suppressBaseShift:n!=="\\stackrel",body:o0(a)},f={type:"supsub",mode:o.mode,base:h,sup:n==="\\underset"?null:o,sub:n==="\\underset"?o:null};return{type:"mclass",mode:r.mode,mclass:m,body:[f],isCharacterBox:E.isCharacterBox(f)}},"handler"),htmlBuilder:Or,mathmlBuilder:Ir});var Hr=u(function(e,t){var r=e.font,n=t.withFont(r);return U(e.body,n)},"htmlBuilder"),Pr=u(function(e,t){var r=e.font,n=t.withFont(r);return Z(e.body,n)},"mathmlBuilder"),Gr={"\\Bbb":"\\mathbb","\\bold":"\\mathbf","\\frak":"\\mathfrak","\\bm":"\\boldsymbol"};T({type:"font",names:["\\mathrm","\\mathit","\\mathbf","\\mathnormal","\\mathbb","\\mathcal","\\mathfrak","\\mathscr","\\mathsf","\\mathtt","\\Bbb","\\bold","\\frak"],props:{numArgs:1,allowedInArgument:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=Fe(t[0]),o=n;return o in Gr&&(o=Gr[o]),{type:"font",mode:r.mode,font:o.slice(1),body:a}},"handler"),htmlBuilder:Hr,mathmlBuilder:Pr}),T({type:"mclass",names:["\\boldsymbol","\\bm"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=t[0],a=E.isCharacterBox(n);return{type:"mclass",mode:r.mode,mclass:zt(n),body:[{type:"font",mode:r.mode,font:"boldsymbol",body:n}],isCharacterBox:a}},"handler")}),T({type:"font",names:["\\rm","\\sf","\\tt","\\bf","\\it","\\cal"],props:{numArgs:0,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=e.breakOnTokenText,o=r.mode,m=r.parseExpression(!0,a),h="math"+n.slice(1);return{type:"font",mode:o,font:h,body:{type:"ordgroup",mode:r.mode,body:m}}},"handler"),htmlBuilder:Hr,mathmlBuilder:Pr});var Vr=u(function(e,t){var r=t;return e==="display"?r=r.id>=R.SCRIPT.id?r.text():R.DISPLAY:e==="text"&&r.size===R.DISPLAY.size?r=R.TEXT:e==="script"?r=R.SCRIPT:e==="scriptscript"&&(r=R.SCRIPTSCRIPT),r},"adjustStyle"),Mt=u(function(e,t){var r=Vr(e.size,t.style),n=r.fracNum(),a=r.fracDen(),o;o=t.havingStyle(n);var m=U(e.numer,o,t);if(e.continued){var h=8.5/t.fontMetrics().ptPerEm,f=3.5/t.fontMetrics().ptPerEm;m.height=m.height<h?h:m.height,m.depth=m.depth<f?f:m.depth}o=t.havingStyle(a);var g=U(e.denom,o,t),y,w,A;e.hasBarLine?(e.barSize?(w=t0(e.barSize,t),y=b.makeLineSpan("frac-line",t,w)):y=b.makeLineSpan("frac-line",t),w=y.height,A=y.height):(y=null,w=0,A=t.fontMetrics().defaultRuleThickness);var z,C,B;r.size===R.DISPLAY.size||e.size==="display"?(z=t.fontMetrics().num1,w>0?C=3*A:C=7*A,B=t.fontMetrics().denom1):(w>0?(z=t.fontMetrics().num2,C=A):(z=t.fontMetrics().num3,C=3*A),B=t.fontMetrics().denom2);var N;if(y){var q=t.fontMetrics().axisHeight;z-m.depth-(q+.5*w)<C&&(z+=C-(z-m.depth-(q+.5*w))),q-.5*w-(g.height-B)<C&&(B+=C-(q-.5*w-(g.height-B)));var X=-(q-.5*w);N=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:g,shift:B},{type:"elem",elem:y,shift:X},{type:"elem",elem:m,shift:-z}]},t)}else{var O=z-m.depth-(g.height-B);O<C&&(z+=.5*(C-O),B+=.5*(C-O)),N=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:g,shift:B},{type:"elem",elem:m,shift:-z}]},t)}o=t.havingStyle(r),N.height*=o.sizeMultiplier/t.sizeMultiplier,N.depth*=o.sizeMultiplier/t.sizeMultiplier;var j;r.size===R.DISPLAY.size?j=t.fontMetrics().delim1:j=t.fontMetrics().delim2;var Q,Y;return e.leftDelim==null?Q=ve(t,["mopen"]):Q=C0.customSizedDelim(e.leftDelim,j,!0,t.havingStyle(r),e.mode,["mopen"]),e.continued?Y=b.makeSpan([]):e.rightDelim==null?Y=ve(t,["mclose"]):Y=C0.customSizedDelim(e.rightDelim,j,!0,t.havingStyle(r),e.mode,["mclose"]),b.makeSpan(["mord"].concat(o.sizingClasses(t)),[Q,b.makeSpan(["mfrac"],[N]),Y],t)},"htmlBuilder"),Bt=u(function(e,t){var r=new M.MathNode("mfrac",[Z(e.numer,t),Z(e.denom,t)]);if(!e.hasBarLine)r.setAttribute("linethickness","0px");else if(e.barSize){var n=t0(e.barSize,t);r.setAttribute("linethickness",n+"em")}var a=Vr(e.size,t.style);if(a.size!==t.style.size){r=new M.MathNode("mstyle",[r]);var o=a.size===R.DISPLAY.size?"true":"false";r.setAttribute("displaystyle",o),r.setAttribute("scriptlevel","0")}if(e.leftDelim!=null||e.rightDelim!=null){var m=[];if(e.leftDelim!=null){var h=new M.MathNode("mo",[new M.TextNode(e.leftDelim.replace("\\",""))]);h.setAttribute("fence","true"),m.push(h)}if(m.push(r),e.rightDelim!=null){var f=new M.MathNode("mo",[new M.TextNode(e.rightDelim.replace("\\",""))]);f.setAttribute("fence","true"),m.push(f)}return mt(m)}return r},"mathmlBuilder");T({type:"genfrac",names:["\\dfrac","\\frac","\\tfrac","\\dbinom","\\binom","\\tbinom","\\\\atopfrac","\\\\bracefrac","\\\\brackfrac"],props:{numArgs:2,allowedInArgument:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0],o=t[1],m,h=null,f=null,g="auto";switch(n){case"\\dfrac":case"\\frac":case"\\tfrac":m=!0;break;case"\\\\atopfrac":m=!1;break;case"\\dbinom":case"\\binom":case"\\tbinom":m=!1,h="(",f=")";break;case"\\\\bracefrac":m=!1,h="\\{",f="\\}";break;case"\\\\brackfrac":m=!1,h="[",f="]";break;default:throw new Error("Unrecognized genfrac command")}switch(n){case"\\dfrac":case"\\dbinom":g="display";break;case"\\tfrac":case"\\tbinom":g="text";break}return{type:"genfrac",mode:r.mode,continued:!1,numer:a,denom:o,hasBarLine:m,leftDelim:h,rightDelim:f,size:g,barSize:null}},"handler"),htmlBuilder:Mt,mathmlBuilder:Bt}),T({type:"genfrac",names:["\\cfrac"],props:{numArgs:2},handler:u(function(e,t){var r=e.parser;e.funcName;var n=t[0],a=t[1];return{type:"genfrac",mode:r.mode,continued:!0,numer:n,denom:a,hasBarLine:!0,leftDelim:null,rightDelim:null,size:"display",barSize:null}},"handler")}),T({type:"infix",names:["\\over","\\choose","\\atop","\\brace","\\brack"],props:{numArgs:0,infix:!0},handler:u(function(e){var t=e.parser,r=e.funcName,n=e.token,a;switch(r){case"\\over":a="\\frac";break;case"\\choose":a="\\binom";break;case"\\atop":a="\\\\atopfrac";break;case"\\brace":a="\\\\bracefrac";break;case"\\brack":a="\\\\brackfrac";break;default:throw new Error("Unrecognized infix genfrac command")}return{type:"infix",mode:t.mode,replaceWith:a,token:n}},"handler")});var Wr=["display","text","script","scriptscript"],Ur=u(function(e){var t=null;return e.length>0&&(t=e,t=t==="."?null:t),t},"delimFromValue");T({type:"genfrac",names:["\\genfrac"],props:{numArgs:6,allowedInArgument:!0,argTypes:["math","math","size","text","math","math"]},handler:u(function(e,t){var r=e.parser,n=t[4],a=t[5],o=Fe(t[0]),m=o.type==="atom"&&o.family==="open"?Ur(o.text):null,h=Fe(t[1]),f=h.type==="atom"&&h.family==="close"?Ur(h.text):null,g=G(t[2],"size"),y,w=null;g.isBlank?y=!0:(w=g.value,y=w.number>0);var A="auto",z=t[3];if(z.type==="ordgroup"){if(z.body.length>0){var C=G(z.body[0],"textord");A=Wr[Number(C.text)]}}else z=G(z,"textord"),A=Wr[Number(z.text)];return{type:"genfrac",mode:r.mode,numer:n,denom:a,continued:!1,hasBarLine:y,barSize:w,leftDelim:m,rightDelim:f,size:A}},"handler"),htmlBuilder:Mt,mathmlBuilder:Bt}),T({type:"infix",names:["\\above"],props:{numArgs:1,argTypes:["size"],infix:!0},handler:u(function(e,t){var r=e.parser;e.funcName;var n=e.token;return{type:"infix",mode:r.mode,replaceWith:"\\\\abovefrac",size:G(t[0],"size").value,token:n}},"handler")}),T({type:"genfrac",names:["\\\\abovefrac"],props:{numArgs:3,argTypes:["math","size","math"]},handler:u(function(e,t){var r=e.parser;e.funcName;var n=t[0],a=Bn(G(t[1],"infix").size),o=t[2],m=a.number>0;return{type:"genfrac",mode:r.mode,numer:n,denom:o,continued:!1,hasBarLine:m,barSize:a,leftDelim:null,rightDelim:null,size:"auto"}},"handler"),htmlBuilder:Mt,mathmlBuilder:Bt});var Yr=u(function(e,t){var r=t.style,n,a;e.type==="supsub"?(n=e.sup?U(e.sup,t.havingStyle(r.sup()),t):U(e.sub,t.havingStyle(r.sub()),t),a=G(e.base,"horizBrace")):a=G(e,"horizBrace");var o=U(a.base,t.havingBaseStyle(R.DISPLAY)),m=R0.svgSpan(a,t),h;if(a.isOver?(h=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:o},{type:"kern",size:.1},{type:"elem",elem:m}]},t),h.children[0].children[0].children[1].classes.push("svg-align")):(h=b.makeVList({positionType:"bottom",positionData:o.depth+.1+m.height,children:[{type:"elem",elem:m},{type:"kern",size:.1},{type:"elem",elem:o}]},t),h.children[0].children[0].children[0].classes.push("svg-align")),n){var f=b.makeSpan(["mord",a.isOver?"mover":"munder"],[h],t);a.isOver?h=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:f},{type:"kern",size:.2},{type:"elem",elem:n}]},t):h=b.makeVList({positionType:"bottom",positionData:f.depth+.2+n.height+n.depth,children:[{type:"elem",elem:n},{type:"kern",size:.2},{type:"elem",elem:f}]},t)}return b.makeSpan(["mord",a.isOver?"mover":"munder"],[h],t)},"htmlBuilder"),Ja=u(function(e,t){var r=R0.mathMLnode(e.label);return new M.MathNode(e.isOver?"mover":"munder",[Z(e.base,t),r])},"mathmlBuilder");T({type:"horizBrace",names:["\\overbrace","\\underbrace"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=e.funcName;return{type:"horizBrace",mode:r.mode,label:n,isOver:/^\\over/.test(n),base:t[0]}},"handler"),htmlBuilder:Yr,mathmlBuilder:Ja}),T({type:"href",names:["\\href"],props:{numArgs:2,argTypes:["url","original"],allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=t[1],a=G(t[0],"url").url;return r.settings.isTrusted({command:"\\href",url:a})?{type:"href",mode:r.mode,href:a,body:o0(n)}:r.formatUnsupportedCmd("\\href")},"handler"),htmlBuilder:u(function(e,t){var r=l0(e.body,t,!1);return b.makeAnchor(e.href,[],r,t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=V0(e.body,t);return r instanceof y0||(r=new y0("mrow",[r])),r.setAttribute("href",e.href),r},"mathmlBuilder")}),T({type:"href",names:["\\url"],props:{numArgs:1,argTypes:["url"],allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=G(t[0],"url").url;if(!r.settings.isTrusted({command:"\\url",url:n}))return r.formatUnsupportedCmd("\\url");for(var a=[],o=0;o<n.length;o++){var m=n[o];m==="~"&&(m="\\textasciitilde"),a.push({type:"textord",mode:"text",text:m})}var h={type:"text",mode:r.mode,font:"\\texttt",body:a};return{type:"href",mode:r.mode,href:n,body:o0(h)}},"handler")}),T({type:"hbox",names:["\\hbox"],props:{numArgs:1,argTypes:["text"],allowedInText:!0,primitive:!0},handler:u(function(e,t){var r=e.parser;return{type:"hbox",mode:r.mode,body:o0(t[0])}},"handler"),htmlBuilder:u(function(e,t){var r=l0(e.body,t,!1);return b.makeFragment(r)},"htmlBuilder"),mathmlBuilder:u(function(e,t){return new M.MathNode("mrow",d0(e.body,t))},"mathmlBuilder")}),T({type:"html",names:["\\htmlClass","\\htmlId","\\htmlStyle","\\htmlData"],props:{numArgs:2,argTypes:["raw","original"],allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName;e.token;var a=G(t[0],"raw").string,o=t[1];r.settings.strict&&r.settings.reportNonstrict("htmlExtension","HTML extension is disabled on strict mode");var m,h={};switch(n){case"\\htmlClass":h.class=a,m={command:"\\htmlClass",class:a};break;case"\\htmlId":h.id=a,m={command:"\\htmlId",id:a};break;case"\\htmlStyle":h.style=a,m={command:"\\htmlStyle",style:a};break;case"\\htmlData":{for(var f=a.split(","),g=0;g<f.length;g++){var y=f[g].split("=");if(y.length!==2)throw new k("Error parsing key-value for \\htmlData");h["data-"+y[0].trim()]=y[1].trim()}m={command:"\\htmlData",attributes:h};break}default:throw new Error("Unrecognized html command")}return r.settings.isTrusted(m)?{type:"html",mode:r.mode,attributes:h,body:o0(o)}:r.formatUnsupportedCmd(n)},"handler"),htmlBuilder:u(function(e,t){var r=l0(e.body,t,!1),n=["enclosing"];e.attributes.class&&n.push.apply(n,e.attributes.class.trim().split(/\s+/));var a=b.makeSpan(n,r,t);for(var o in e.attributes)o!=="class"&&e.attributes.hasOwnProperty(o)&&a.setAttribute(o,e.attributes[o]);return a},"htmlBuilder"),mathmlBuilder:u(function(e,t){return V0(e.body,t)},"mathmlBuilder")}),T({type:"htmlmathml",names:["\\html@mathml"],props:{numArgs:2,allowedInText:!0},handler:u(function(e,t){var r=e.parser;return{type:"htmlmathml",mode:r.mode,html:o0(t[0]),mathml:o0(t[1])}},"handler"),htmlBuilder:u(function(e,t){var r=l0(e.html,t,!1);return b.makeFragment(r)},"htmlBuilder"),mathmlBuilder:u(function(e,t){return V0(e.mathml,t)},"mathmlBuilder")});var Ct=u(function(e){if(/^[-+]? *(\d+(\.\d*)?|\.\d+)$/.test(e))return{number:+e,unit:"bp"};var t=/([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(e);if(!t)throw new k("Invalid size: '"+e+"' in \\includegraphics");var r={number:+(t[1]+t[2]),unit:t[3]};if(!ar(r))throw new k("Invalid unit: '"+r.unit+"' in \\includegraphics.");return r},"sizeData");T({type:"includegraphics",names:["\\includegraphics"],props:{numArgs:1,numOptionalArgs:1,argTypes:["raw","url"],allowedInText:!1},handler:u(function(e,t,r){var n=e.parser,a={number:0,unit:"em"},o={number:.9,unit:"em"},m={number:0,unit:"em"},h="";if(r[0])for(var f=G(r[0],"raw").string,g=f.split(","),y=0;y<g.length;y++){var w=g[y].split("=");if(w.length===2){var A=w[1].trim();switch(w[0].trim()){case"alt":h=A;break;case"width":a=Ct(A);break;case"height":o=Ct(A);break;case"totalheight":m=Ct(A);break;default:throw new k("Invalid key: '"+w[0]+"' in \\includegraphics.")}}}var z=G(t[0],"url").url;return h===""&&(h=z,h=h.replace(/^.*[\\/]/,""),h=h.substring(0,h.lastIndexOf("."))),n.settings.isTrusted({command:"\\includegraphics",url:z})?{type:"includegraphics",mode:n.mode,alt:h,width:a,height:o,totalheight:m,src:z}:n.formatUnsupportedCmd("\\includegraphics")},"handler"),htmlBuilder:u(function(e,t){var r=t0(e.height,t),n=0;e.totalheight.number>0&&(n=t0(e.totalheight,t)-r,n=Number(n.toFixed(2)));var a=0;e.width.number>0&&(a=t0(e.width,t));var o={height:r+n+"em"};a>0&&(o.width=a+"em"),n>0&&(o.verticalAlign=-n+"em");var m=new Yn(e.src,e.alt,o);return m.height=r,m.depth=n,m},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mglyph",[]);r.setAttribute("alt",e.alt);var n=t0(e.height,t),a=0;if(e.totalheight.number>0&&(a=t0(e.totalheight,t)-n,a=a.toFixed(2),r.setAttribute("valign","-"+a+"em")),r.setAttribute("height",n+a+"em"),e.width.number>0){var o=t0(e.width,t);r.setAttribute("width",o+"em")}return r.setAttribute("src",e.src),r},"mathmlBuilder")}),T({type:"kern",names:["\\kern","\\mkern","\\hskip","\\mskip"],props:{numArgs:1,argTypes:["size"],primitive:!0,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=G(t[0],"size");if(r.settings.strict){var o=n[1]==="m",m=a.value.unit==="mu";o?(m||r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" supports only mu units, "+("not "+a.value.unit+" units")),r.mode!=="math"&&r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" works only in math mode")):m&&r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" doesn't support mu units")}return{type:"kern",mode:r.mode,dimension:a.value}},"handler"),htmlBuilder:u(function(e,t){return b.makeGlue(e.dimension,t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=t0(e.dimension,t);return new M.SpaceNode(r)},"mathmlBuilder")}),T({type:"lap",names:["\\mathllap","\\mathrlap","\\mathclap"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"lap",mode:r.mode,alignment:n.slice(5),body:a}},"handler"),htmlBuilder:u(function(e,t){var r;e.alignment==="clap"?(r=b.makeSpan([],[U(e.body,t)]),r=b.makeSpan(["inner"],[r],t)):r=b.makeSpan(["inner"],[U(e.body,t)]);var n=b.makeSpan(["fix"],[]),a=b.makeSpan([e.alignment],[r,n],t),o=b.makeSpan(["strut"]);return o.style.height=a.height+a.depth+"em",o.style.verticalAlign=-a.depth+"em",a.children.unshift(o),a=b.makeSpan(["thinbox"],[a],t),b.makeSpan(["mord","vbox"],[a],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mpadded",[Z(e.body,t)]);if(e.alignment!=="rlap"){var n=e.alignment==="llap"?"-1":"-0.5";r.setAttribute("lspace",n+"width")}return r.setAttribute("width","0px"),r},"mathmlBuilder")}),T({type:"styling",names:["\\(","$"],props:{numArgs:0,allowedInText:!0,allowedInMath:!1},handler:u(function(e,t){var r=e.funcName,n=e.parser,a=n.mode;n.switchMode("math");var o=r==="\\("?"\\)":"$",m=n.parseExpression(!1,o);return n.expect(o),n.switchMode(a),{type:"styling",mode:n.mode,style:"text",body:m}},"handler")}),T({type:"text",names:["\\)","\\]"],props:{numArgs:0,allowedInText:!0,allowedInMath:!1},handler:u(function(e,t){throw new k("Mismatched "+e.funcName)},"handler")});var Xr=u(function(e,t){switch(t.style.size){case R.DISPLAY.size:return e.display;case R.TEXT.size:return e.text;case R.SCRIPT.size:return e.script;case R.SCRIPTSCRIPT.size:return e.scriptscript;default:return e.text}},"chooseMathStyle");T({type:"mathchoice",names:["\\mathchoice"],props:{numArgs:4,primitive:!0},handler:u(function(e,t){var r=e.parser;return{type:"mathchoice",mode:r.mode,display:o0(t[0]),text:o0(t[1]),script:o0(t[2]),scriptscript:o0(t[3])}},"handler"),htmlBuilder:u(function(e,t){var r=Xr(e,t),n=l0(r,t,!1);return b.makeFragment(n)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=Xr(e,t);return V0(r,t)},"mathmlBuilder")});var $r=u(function(e,t,r,n,a,o,m){e=b.makeSpan([],[e]);var h,f;if(t){var g=U(t,n.havingStyle(a.sup()),n);f={elem:g,kern:Math.max(n.fontMetrics().bigOpSpacing1,n.fontMetrics().bigOpSpacing3-g.depth)}}if(r){var y=U(r,n.havingStyle(a.sub()),n);h={elem:y,kern:Math.max(n.fontMetrics().bigOpSpacing2,n.fontMetrics().bigOpSpacing4-y.height)}}var w;if(f&&h){var A=n.fontMetrics().bigOpSpacing5+h.elem.height+h.elem.depth+h.kern+e.depth+m;w=b.makeVList({positionType:"bottom",positionData:A,children:[{type:"kern",size:n.fontMetrics().bigOpSpacing5},{type:"elem",elem:h.elem,marginLeft:-o+"em"},{type:"kern",size:h.kern},{type:"elem",elem:e},{type:"kern",size:f.kern},{type:"elem",elem:f.elem,marginLeft:o+"em"},{type:"kern",size:n.fontMetrics().bigOpSpacing5}]},n)}else if(h){var z=e.height-m;w=b.makeVList({positionType:"top",positionData:z,children:[{type:"kern",size:n.fontMetrics().bigOpSpacing5},{type:"elem",elem:h.elem,marginLeft:-o+"em"},{type:"kern",size:h.kern},{type:"elem",elem:e}]},n)}else if(f){var C=e.depth+m;w=b.makeVList({positionType:"bottom",positionData:C,children:[{type:"elem",elem:e},{type:"kern",size:f.kern},{type:"elem",elem:f.elem,marginLeft:o+"em"},{type:"kern",size:n.fontMetrics().bigOpSpacing5}]},n)}else return e;return b.makeSpan(["mop","op-limits"],[w],n)},"assembleSupSub"),jr=["\\smallint"],oe=u(function(e,t){var r,n,a=!1,o;e.type==="supsub"?(r=e.sup,n=e.sub,o=G(e.base,"op"),a=!0):o=G(e,"op");var m=t.style,h=!1;m.size===R.DISPLAY.size&&o.symbol&&!E.contains(jr,o.name)&&(h=!0);var f;if(o.symbol){var g=h?"Size2-Regular":"Size1-Regular",y="";if((o.name==="\\oiint"||o.name==="\\oiiint")&&(y=o.name.substr(1),o.name=y==="oiint"?"\\iint":"\\iiint"),f=b.makeSymbol(o.name,g,"math",t,["mop","op-symbol",h?"large-op":"small-op"]),y.length>0){var w=f.italic,A=b.staticSvg(y+"Size"+(h?"2":"1"),t);f=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:f,shift:0},{type:"elem",elem:A,shift:h?.08:0}]},t),o.name="\\"+y,f.classes.unshift("mop"),f.italic=w}}else if(o.body){var z=l0(o.body,t,!0);z.length===1&&z[0]instanceof x0?(f=z[0],f.classes[0]="mop"):f=b.makeSpan(["mop"],z,t)}else{for(var C=[],B=1;B<o.name.length;B++)C.push(b.mathsym(o.name[B],o.mode,t));f=b.makeSpan(["mop"],C,t)}var N=0,O=0;return(f instanceof x0||o.name==="\\oiint"||o.name==="\\oiiint")&&!o.suppressBaseShift&&(N=(f.height-f.depth)/2-t.fontMetrics().axisHeight,O=f.italic),a?$r(f,r,n,t,m,O,N):(N&&(f.style.position="relative",f.style.top=N+"em"),f)},"htmlBuilder"),be=u(function(e,t){var r;if(e.symbol)r=new y0("mo",[b0(e.name,e.mode)]),E.contains(jr,e.name)&&r.setAttribute("largeop","false");else if(e.body)r=new y0("mo",d0(e.body,t));else{r=new y0("mi",[new ge(e.name.slice(1))]);var n=new y0("mo",[b0("⁡","text")]);e.parentIsSupSub?r=new y0("mrow",[r,n]):r=hr([r,n])}return r},"mathmlBuilder"),Qa={"∏":"\\prod","∐":"\\coprod","∑":"\\sum","⋀":"\\bigwedge","⋁":"\\bigvee","⋂":"\\bigcap","⋃":"\\bigcup","⨀":"\\bigodot","⨁":"\\bigoplus","⨂":"\\bigotimes","⨄":"\\biguplus","⨆":"\\bigsqcup"};T({type:"op",names:["\\coprod","\\bigvee","\\bigwedge","\\biguplus","\\bigcap","\\bigcup","\\intop","\\prod","\\sum","\\bigotimes","\\bigoplus","\\bigodot","\\bigsqcup","\\smallint","∏","∐","∑","⋀","⋁","⋂","⋃","⨀","⨁","⨂","⨄","⨆"],props:{numArgs:0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=n;return a.length===1&&(a=Qa[a]),{type:"op",mode:r.mode,limits:!0,parentIsSupSub:!1,symbol:!0,name:a}},"handler"),htmlBuilder:oe,mathmlBuilder:be}),T({type:"op",names:["\\mathop"],props:{numArgs:1,primitive:!0},handler:u(function(e,t){var r=e.parser,n=t[0];return{type:"op",mode:r.mode,limits:!1,parentIsSupSub:!1,symbol:!1,body:o0(n)}},"handler"),htmlBuilder:oe,mathmlBuilder:be});var _a={"∫":"\\int","∬":"\\iint","∭":"\\iiint","∮":"\\oint","∯":"\\oiint","∰":"\\oiiint"};T({type:"op",names:["\\arcsin","\\arccos","\\arctan","\\arctg","\\arcctg","\\arg","\\ch","\\cos","\\cosec","\\cosh","\\cot","\\cotg","\\coth","\\csc","\\ctg","\\cth","\\deg","\\dim","\\exp","\\hom","\\ker","\\lg","\\ln","\\log","\\sec","\\sin","\\sinh","\\sh","\\tan","\\tanh","\\tg","\\th"],props:{numArgs:0},handler:u(function(e){var t=e.parser,r=e.funcName;return{type:"op",mode:t.mode,limits:!1,parentIsSupSub:!1,symbol:!1,name:r}},"handler"),htmlBuilder:oe,mathmlBuilder:be}),T({type:"op",names:["\\det","\\gcd","\\inf","\\lim","\\max","\\min","\\Pr","\\sup"],props:{numArgs:0},handler:u(function(e){var t=e.parser,r=e.funcName;return{type:"op",mode:t.mode,limits:!0,parentIsSupSub:!1,symbol:!1,name:r}},"handler"),htmlBuilder:oe,mathmlBuilder:be}),T({type:"op",names:["\\int","\\iint","\\iiint","\\oint","\\oiint","\\oiiint","∫","∬","∭","∮","∯","∰"],props:{numArgs:0},handler:u(function(e){var t=e.parser,r=e.funcName,n=r;return n.length===1&&(n=_a[n]),{type:"op",mode:t.mode,limits:!1,parentIsSupSub:!1,symbol:!0,name:n}},"handler"),htmlBuilder:oe,mathmlBuilder:be});var Zr=u(function(e,t){var r,n,a=!1,o;e.type==="supsub"?(r=e.sup,n=e.sub,o=G(e.base,"operatorname"),a=!0):o=G(e,"operatorname");var m;if(o.body.length>0){for(var h=o.body.map(function(w){var A=w.text;return typeof A=="string"?{type:"textord",mode:w.mode,text:A}:w}),f=l0(h,t.withFont("mathrm"),!0),g=0;g<f.length;g++){var y=f[g];y instanceof x0&&(y.text=y.text.replace(/\u2212/,"-").replace(/\u2217/,"*"))}m=b.makeSpan(["mop"],f,t)}else m=b.makeSpan(["mop"],[],t);return a?$r(m,r,n,t,t.style,0,0):m},"htmlBuilder"),ei=u(function(e,t){for(var r=d0(e.body,t.withFont("mathrm")),n=!0,a=0;a<r.length;a++){var o=r[a];if(!(o instanceof M.SpaceNode))if(o instanceof M.MathNode)switch(o.type){case"mi":case"mn":case"ms":case"mspace":case"mtext":break;case"mo":{var m=o.children[0];o.children.length===1&&m instanceof M.TextNode?m.text=m.text.replace(/\u2212/,"-").replace(/\u2217/,"*"):n=!1;break}default:n=!1}else n=!1}if(n){var h=r.map(function(y){return y.toText()}).join("");r=[new M.TextNode(h)]}var f=new M.MathNode("mi",r);f.setAttribute("mathvariant","normal");var g=new M.MathNode("mo",[b0("⁡","text")]);return e.parentIsSupSub?new M.MathNode("mrow",[f,g]):M.newDocumentFragment([f,g])},"mathmlBuilder");T({type:"operatorname",names:["\\operatorname","\\operatorname*"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"operatorname",mode:r.mode,body:o0(a),alwaysHandleSupSub:n==="\\operatorname*",limits:!1,parentIsSupSub:!1}},"handler"),htmlBuilder:Zr,mathmlBuilder:ei}),_0({type:"ordgroup",htmlBuilder:u(function(e,t){return e.semisimple?b.makeFragment(l0(e.body,t,!1)):b.makeSpan(["mord"],l0(e.body,t,!0),t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){return V0(e.body,t,!0)},"mathmlBuilder")}),T({type:"overline",names:["\\overline"],props:{numArgs:1},handler:u(function(e,t){var r=e.parser,n=t[0];return{type:"overline",mode:r.mode,body:n}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.body,t.havingCrampedStyle()),n=b.makeLineSpan("overline-line",t),a=t.fontMetrics().defaultRuleThickness,o=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r},{type:"kern",size:3*a},{type:"elem",elem:n},{type:"kern",size:a}]},t);return b.makeSpan(["mord","overline"],[o],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mo",[new M.TextNode("‾")]);r.setAttribute("stretchy","true");var n=new M.MathNode("mover",[Z(e.body,t),r]);return n.setAttribute("accent","true"),n},"mathmlBuilder")}),T({type:"phantom",names:["\\phantom"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=t[0];return{type:"phantom",mode:r.mode,body:o0(n)}},"handler"),htmlBuilder:u(function(e,t){var r=l0(e.body,t.withPhantom(),!1);return b.makeFragment(r)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=d0(e.body,t);return new M.MathNode("mphantom",r)},"mathmlBuilder")}),T({type:"hphantom",names:["\\hphantom"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=t[0];return{type:"hphantom",mode:r.mode,body:n}},"handler"),htmlBuilder:u(function(e,t){var r=b.makeSpan([],[U(e.body,t.withPhantom())]);if(r.height=0,r.depth=0,r.children)for(var n=0;n<r.children.length;n++)r.children[n].height=0,r.children[n].depth=0;return r=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r}]},t),b.makeSpan(["mord"],[r],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=d0(o0(e.body),t),n=new M.MathNode("mphantom",r),a=new M.MathNode("mpadded",[n]);return a.setAttribute("height","0px"),a.setAttribute("depth","0px"),a},"mathmlBuilder")}),T({type:"vphantom",names:["\\vphantom"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=t[0];return{type:"vphantom",mode:r.mode,body:n}},"handler"),htmlBuilder:u(function(e,t){var r=b.makeSpan(["inner"],[U(e.body,t.withPhantom())]),n=b.makeSpan(["fix"],[]);return b.makeSpan(["mord","rlap"],[r,n],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=d0(o0(e.body),t),n=new M.MathNode("mphantom",r),a=new M.MathNode("mpadded",[n]);return a.setAttribute("width","0px"),a},"mathmlBuilder")}),T({type:"raisebox",names:["\\raisebox"],props:{numArgs:2,argTypes:["size","hbox"],allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=G(t[0],"size").value,a=t[1];return{type:"raisebox",mode:r.mode,dy:n,body:a}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.body,t),n=t0(e.dy,t);return b.makeVList({positionType:"shift",positionData:-n,children:[{type:"elem",elem:r}]},t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mpadded",[Z(e.body,t)]),n=e.dy.number+e.dy.unit;return r.setAttribute("voffset",n),r},"mathmlBuilder")}),T({type:"rule",names:["\\rule"],props:{numArgs:2,numOptionalArgs:1,argTypes:["size","size","size"]},handler:u(function(e,t,r){var n=e.parser,a=r[0],o=G(t[0],"size"),m=G(t[1],"size");return{type:"rule",mode:n.mode,shift:a&&G(a,"size").value,width:o.value,height:m.value}},"handler"),htmlBuilder:u(function(e,t){var r=b.makeSpan(["mord","rule"],[],t),n=t0(e.width,t),a=t0(e.height,t),o=e.shift?t0(e.shift,t):0;return r.style.borderRightWidth=n+"em",r.style.borderTopWidth=a+"em",r.style.bottom=o+"em",r.width=n,r.height=a+o,r.depth=-o,r.maxFontSize=a*1.125*t.sizeMultiplier,r},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=t0(e.width,t),n=t0(e.height,t),a=e.shift?t0(e.shift,t):0,o=t.color&&t.getColor()||"black",m=new M.MathNode("mspace");m.setAttribute("mathbackground",o),m.setAttribute("width",r+"em"),m.setAttribute("height",n+"em");var h=new M.MathNode("mpadded",[m]);return a>=0?h.setAttribute("height","+"+a+"em"):(h.setAttribute("height",a+"em"),h.setAttribute("depth","+"+-a+"em")),h.setAttribute("voffset",a+"em"),h},"mathmlBuilder")});function Kr(l,e,t){for(var r=l0(l,e,!1),n=e.sizeMultiplier/t.sizeMultiplier,a=0;a<r.length;a++){var o=r[a].classes.indexOf("sizing");o<0?Array.prototype.push.apply(r[a].classes,e.sizingClasses(t)):r[a].classes[o+1]==="reset-size"+e.size&&(r[a].classes[o+1]="reset-size"+t.size),r[a].height*=n,r[a].depth*=n}return b.makeFragment(r)}u(Kr,"sizingGroup");var Jr=["\\tiny","\\sixptsize","\\scriptsize","\\footnotesize","\\small","\\normalsize","\\large","\\Large","\\LARGE","\\huge","\\Huge"],ti=u(function(e,t){var r=t.havingSize(e.size);return Kr(e.body,r,t)},"htmlBuilder");T({type:"sizing",names:Jr,props:{numArgs:0,allowedInText:!0},handler:u(function(e,t){var r=e.breakOnTokenText,n=e.funcName,a=e.parser,o=a.parseExpression(!1,r);return{type:"sizing",mode:a.mode,size:Jr.indexOf(n)+1,body:o}},"handler"),htmlBuilder:ti,mathmlBuilder:u(function(e,t){var r=t.havingSize(e.size),n=d0(e.body,r),a=new M.MathNode("mstyle",n);return a.setAttribute("mathsize",r.sizeMultiplier+"em"),a},"mathmlBuilder")}),T({type:"smash",names:["\\smash"],props:{numArgs:1,numOptionalArgs:1,allowedInText:!0},handler:u(function(e,t,r){var n=e.parser,a=!1,o=!1,m=r[0]&&G(r[0],"ordgroup");if(m)for(var h="",f=0;f<m.body.length;++f){var g=m.body[f];if(h=g.text,h==="t")a=!0;else if(h==="b")o=!0;else{a=!1,o=!1;break}}else a=!0,o=!0;var y=t[0];return{type:"smash",mode:n.mode,body:y,smashHeight:a,smashDepth:o}},"handler"),htmlBuilder:u(function(e,t){var r=b.makeSpan([],[U(e.body,t)]);if(!e.smashHeight&&!e.smashDepth)return r;if(e.smashHeight&&(r.height=0,r.children))for(var n=0;n<r.children.length;n++)r.children[n].height=0;if(e.smashDepth&&(r.depth=0,r.children))for(var a=0;a<r.children.length;a++)r.children[a].depth=0;var o=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r}]},t);return b.makeSpan(["mord"],[o],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mpadded",[Z(e.body,t)]);return e.smashHeight&&r.setAttribute("height","0px"),e.smashDepth&&r.setAttribute("depth","0px"),r},"mathmlBuilder")}),T({type:"sqrt",names:["\\sqrt"],props:{numArgs:1,numOptionalArgs:1},handler:u(function(e,t,r){var n=e.parser,a=r[0],o=t[0];return{type:"sqrt",mode:n.mode,body:o,index:a}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.body,t.havingCrampedStyle());r.height===0&&(r.height=t.fontMetrics().xHeight),r=b.wrapFragment(r,t);var n=t.fontMetrics(),a=n.defaultRuleThickness,o=a;t.style.id<R.TEXT.id&&(o=t.fontMetrics().xHeight);var m=a+o/4,h=r.height+r.depth+m+a,f=C0.sqrtImage(h,t),g=f.span,y=f.ruleWidth,w=f.advanceWidth,A=g.height-y;A>r.height+r.depth+m&&(m=(m+A-r.height-r.depth)/2);var z=g.height-r.height-m-y;r.style.paddingLeft=w+"em";var C=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r,wrapperClasses:["svg-align"]},{type:"kern",size:-(r.height+z)},{type:"elem",elem:g},{type:"kern",size:y}]},t);if(e.index){var B=t.havingStyle(R.SCRIPTSCRIPT),N=U(e.index,B,t),O=.6*(C.height-C.depth),q=b.makeVList({positionType:"shift",positionData:-O,children:[{type:"elem",elem:N}]},t),X=b.makeSpan(["root"],[q]);return b.makeSpan(["mord","sqrt"],[X,C],t)}else return b.makeSpan(["mord","sqrt"],[C],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=e.body,n=e.index;return n?new M.MathNode("mroot",[Z(r,t),Z(n,t)]):new M.MathNode("msqrt",[Z(r,t)])},"mathmlBuilder")});var Qr={display:R.DISPLAY,text:R.TEXT,script:R.SCRIPT,scriptscript:R.SCRIPTSCRIPT};T({type:"styling",names:["\\displaystyle","\\textstyle","\\scriptstyle","\\scriptscriptstyle"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:u(function(e,t){var r=e.breakOnTokenText,n=e.funcName,a=e.parser,o=a.parseExpression(!0,r),m=n.slice(1,n.length-5);return{type:"styling",mode:a.mode,style:m,body:o}},"handler"),htmlBuilder:u(function(e,t){var r=Qr[e.style],n=t.havingStyle(r).withFont("");return Kr(e.body,n,t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=Qr[e.style],n=t.havingStyle(r),a=d0(e.body,n),o=new M.MathNode("mstyle",a),m={display:["0","true"],text:["0","false"],script:["1","false"],scriptscript:["2","false"]},h=m[e.style];return o.setAttribute("scriptlevel",h[0]),o.setAttribute("displaystyle",h[1]),o},"mathmlBuilder")});var ri=u(function(e,t){var r=e.base;if(r)if(r.type==="op"){var n=r.limits&&(t.style.size===R.DISPLAY.size||r.alwaysHandleSupSub);return n?oe:null}else if(r.type==="operatorname"){var a=r.alwaysHandleSupSub&&(t.style.size===R.DISPLAY.size||r.limits);return a?Zr:null}else{if(r.type==="accent")return E.isCharacterBox(r.base)?dt:null;if(r.type==="horizBrace"){var o=!e.sub;return o===r.isOver?Yr:null}else return null}else return null},"htmlBuilderDelegate");_0({type:"supsub",htmlBuilder:u(function(e,t){var r=ri(e,t);if(r)return r(e,t);var n=e.base,a=e.sup,o=e.sub,m=U(n,t),h,f,g=t.fontMetrics(),y=0,w=0,A=n&&E.isCharacterBox(n);if(a){var z=t.havingStyle(t.style.sup());h=U(a,z,t),A||(y=m.height-z.fontMetrics().supDrop*z.sizeMultiplier/t.sizeMultiplier)}if(o){var C=t.havingStyle(t.style.sub());f=U(o,C,t),A||(w=m.depth+C.fontMetrics().subDrop*C.sizeMultiplier/t.sizeMultiplier)}var B;t.style===R.DISPLAY?B=g.sup1:t.style.cramped?B=g.sup3:B=g.sup2;var N=t.sizeMultiplier,O=.5/g.ptPerEm/N+"em",q=null;if(f){var X=e.base&&e.base.type==="op"&&e.base.name&&(e.base.name==="\\oiint"||e.base.name==="\\oiiint");(m instanceof x0||X)&&(q=-m.italic+"em")}var j;if(h&&f){y=Math.max(y,B,h.depth+.25*g.xHeight),w=Math.max(w,g.sub2);var Q=g.defaultRuleThickness,Y=4*Q;if(y-h.depth-(f.height-w)<Y){w=Y-(y-h.depth)+f.height;var $=.8*g.xHeight-(y-h.depth);$>0&&(y+=$,w-=$)}var n0=[{type:"elem",elem:f,shift:w,marginRight:O,marginLeft:q},{type:"elem",elem:h,shift:-y,marginRight:O}];j=b.makeVList({positionType:"individualShift",children:n0},t)}else if(f){w=Math.max(w,g.sub1,f.height-.8*g.xHeight);var K=[{type:"elem",elem:f,marginLeft:q,marginRight:O}];j=b.makeVList({positionType:"shift",positionData:w,children:K},t)}else if(h)y=Math.max(y,B,h.depth+.25*g.xHeight),j=b.makeVList({positionType:"shift",positionData:-y,children:[{type:"elem",elem:h,marginRight:O}]},t);else throw new Error("supsub must have either sup or sub.");var f0=st(m,"right")||"mord";return b.makeSpan([f0],[m,b.makeSpan(["msupsub"],[j])],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=!1,n,a;e.base&&e.base.type==="horizBrace"&&(a=!!e.sup,a===e.base.isOver&&(r=!0,n=e.base.isOver)),e.base&&(e.base.type==="op"||e.base.type==="operatorname")&&(e.base.parentIsSupSub=!0);var o=[Z(e.base,t)];e.sub&&o.push(Z(e.sub,t)),e.sup&&o.push(Z(e.sup,t));var m;if(r)m=n?"mover":"munder";else if(e.sub)if(e.sup){var g=e.base;g&&g.type==="op"&&g.limits&&t.style===R.DISPLAY||g&&g.type==="operatorname"&&g.alwaysHandleSupSub&&(t.style===R.DISPLAY||g.limits)?m="munderover":m="msubsup"}else{var f=e.base;f&&f.type==="op"&&f.limits&&(t.style===R.DISPLAY||f.alwaysHandleSupSub)||f&&f.type==="operatorname"&&f.alwaysHandleSupSub&&(f.limits||t.style===R.DISPLAY)?m="munder":m="msub"}else{var h=e.base;h&&h.type==="op"&&h.limits&&(t.style===R.DISPLAY||h.alwaysHandleSupSub)||h&&h.type==="operatorname"&&h.alwaysHandleSupSub&&(h.limits||t.style===R.DISPLAY)?m="mover":m="msup"}return new M.MathNode(m,o)},"mathmlBuilder")}),_0({type:"atom",htmlBuilder:u(function(e,t){return b.mathsym(e.text,e.mode,t,["m"+e.family])},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mo",[b0(e.text,e.mode)]);if(e.family==="bin"){var n=ct(e,t);n==="bold-italic"&&r.setAttribute("mathvariant",n)}else e.family==="punct"?r.setAttribute("separator","true"):(e.family==="open"||e.family==="close")&&r.setAttribute("stretchy","false");return r},"mathmlBuilder")});var _r={mi:"italic",mn:"normal",mtext:"normal"};_0({type:"mathord",htmlBuilder:u(function(e,t){return b.makeOrd(e,t,"mathord")},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mi",[b0(e.text,e.mode,t)]),n=ct(e,t)||"italic";return n!==_r[r.type]&&r.setAttribute("mathvariant",n),r},"mathmlBuilder")}),_0({type:"textord",htmlBuilder:u(function(e,t){return b.makeOrd(e,t,"textord")},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=b0(e.text,e.mode,t),n=ct(e,t)||"normal",a;return e.mode==="text"?a=new M.MathNode("mtext",[r]):/[0-9]/.test(e.text)?a=new M.MathNode("mn",[r]):e.text==="\\prime"?a=new M.MathNode("mo",[r]):a=new M.MathNode("mi",[r]),n!==_r[a.type]&&a.setAttribute("mathvariant",n),a},"mathmlBuilder")});var Tt={"\\nobreak":"nobreak","\\allowbreak":"allowbreak"},Dt={" ":{},"\\ ":{},"~":{className:"nobreak"},"\\space":{},"\\nobreakspace":{className:"nobreak"}};_0({type:"spacing",htmlBuilder:u(function(e,t){if(Dt.hasOwnProperty(e.text)){var r=Dt[e.text].className||"";if(e.mode==="text"){var n=b.makeOrd(e,t,"textord");return n.classes.push(r),n}else return b.makeSpan(["mspace",r],[b.mathsym(e.text,e.mode,t)],t)}else{if(Tt.hasOwnProperty(e.text))return b.makeSpan(["mspace",Tt[e.text]],[],t);throw new k('Unknown type of space "'+e.text+'"')}},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r;if(Dt.hasOwnProperty(e.text))r=new M.MathNode("mtext",[new M.TextNode(" ")]);else{if(Tt.hasOwnProperty(e.text))return new M.MathNode("mspace");throw new k('Unknown type of space "'+e.text+'"')}return r},"mathmlBuilder")});var en=u(function(){var e=new M.MathNode("mtd",[]);return e.setAttribute("width","50%"),e},"pad");_0({type:"tag",mathmlBuilder:u(function(e,t){var r=new M.MathNode("mtable",[new M.MathNode("mtr",[en(),new M.MathNode("mtd",[V0(e.body,t)]),en(),new M.MathNode("mtd",[V0(e.tag,t)])])]);return r.setAttribute("width","100%"),r},"mathmlBuilder")});var tn={"\\text":void 0,"\\textrm":"textrm","\\textsf":"textsf","\\texttt":"texttt","\\textnormal":"textrm"},rn={"\\textbf":"textbf","\\textmd":"textmd"},ni={"\\textit":"textit","\\textup":"textup"},nn=u(function(e,t){var r=e.font;return r?tn[r]?t.withTextFontFamily(tn[r]):rn[r]?t.withTextFontWeight(rn[r]):t.withTextFontShape(ni[r]):t},"optionsWithFont");T({type:"text",names:["\\text","\\textrm","\\textsf","\\texttt","\\textnormal","\\textbf","\\textmd","\\textit","\\textup"],props:{numArgs:1,argTypes:["text"],allowedInArgument:!0,allowedInText:!0},handler:u(function(e,t){var r=e.parser,n=e.funcName,a=t[0];return{type:"text",mode:r.mode,body:o0(a),font:n}},"handler"),htmlBuilder:u(function(e,t){var r=nn(e,t),n=l0(e.body,r,!0);return b.makeSpan(["mord","text"],n,r)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=nn(e,t);return V0(e.body,r)},"mathmlBuilder")}),T({type:"underline",names:["\\underline"],props:{numArgs:1,allowedInText:!0},handler:u(function(e,t){var r=e.parser;return{type:"underline",mode:r.mode,body:t[0]}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.body,t),n=b.makeLineSpan("underline-line",t),a=t.fontMetrics().defaultRuleThickness,o=b.makeVList({positionType:"top",positionData:r.height,children:[{type:"kern",size:a},{type:"elem",elem:n},{type:"kern",size:3*a},{type:"elem",elem:r}]},t);return b.makeSpan(["mord","underline"],[o],t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.MathNode("mo",[new M.TextNode("‾")]);r.setAttribute("stretchy","true");var n=new M.MathNode("munder",[Z(e.body,t),r]);return n.setAttribute("accentunder","true"),n},"mathmlBuilder")}),T({type:"vcenter",names:["\\vcenter"],props:{numArgs:1,argTypes:["original"],allowedInText:!1},handler:u(function(e,t){var r=e.parser;return{type:"vcenter",mode:r.mode,body:t[0]}},"handler"),htmlBuilder:u(function(e,t){var r=U(e.body,t),n=t.fontMetrics().axisHeight,a=.5*(r.height-n-(r.depth+n));return b.makeVList({positionType:"shift",positionData:a,children:[{type:"elem",elem:r}]},t)},"htmlBuilder"),mathmlBuilder:u(function(e,t){return new M.MathNode("mpadded",[Z(e.body,t)],["vcenter"])},"mathmlBuilder")}),T({type:"verb",names:["\\verb"],props:{numArgs:0,allowedInText:!0},handler:u(function(e,t,r){throw new k("\\verb ended by end of line instead of matching delimiter")},"handler"),htmlBuilder:u(function(e,t){for(var r=an(e),n=[],a=t.havingStyle(t.style.text()),o=0;o<r.length;o++){var m=r[o];m==="~"&&(m="\\textasciitilde"),n.push(b.makeSymbol(m,"Typewriter-Regular",e.mode,a,["mord","texttt"]))}return b.makeSpan(["mord","text"].concat(a.sizingClasses(t)),b.tryCombineChars(n),a)},"htmlBuilder"),mathmlBuilder:u(function(e,t){var r=new M.TextNode(an(e)),n=new M.MathNode("mtext",[r]);return n.setAttribute("mathvariant","monospace"),n},"mathmlBuilder")});var an=u(function(e){return e.body.replace(/ /g,e.star?"␣":" ")},"makeVerb"),ai=ur,U0=ai,z0=function(){function l(e,t,r){this.lexer=void 0,this.start=void 0,this.end=void 0,this.lexer=e,this.start=t,this.end=r}return u(l,"SourceLocation"),l.range=u(function(t,r){return r?!t||!t.loc||!r.loc||t.loc.lexer!==r.loc.lexer?null:new l(t.loc.lexer,t.loc.start,r.loc.end):t&&t.loc},"range"),l}(),ee=function(){function l(t,r){this.text=void 0,this.loc=void 0,this.noexpand=void 0,this.treatAsRelax=void 0,this.text=t,this.loc=r}u(l,"Token");var e=l.prototype;return e.range=u(function(r,n){return new l(n,z0.range(this,r))},"range"),l}(),Nt=`[ \r
	]`,on="\\\\[a-zA-Z@]+",ii="\\\\[^\uD800-\uDFFF]",oi=""+on+Nt+"*",li=new RegExp("^("+on+")"+Nt+"*$"),Et="[̀-ͯ]",si=new RegExp(Et+"+$"),ui="("+Nt+"+)|([!-\\[\\]-‧‪-퟿豈-￿]"+(Et+"*")+"|[\uD800-\uDBFF][\uDC00-\uDFFF]"+(Et+"*")+"|\\\\verb\\*([^]).*?\\3|\\\\verb([^*a-zA-Z]).*?\\4|\\\\operatorname\\*"+("|"+oi)+("|"+ii+")"),ln=function(){function l(t,r){this.input=void 0,this.settings=void 0,this.tokenRegex=void 0,this.catcodes=void 0,this.input=t,this.settings=r,this.tokenRegex=new RegExp(ui,"g"),this.catcodes={"%":14}}u(l,"Lexer");var e=l.prototype;return e.setCatcode=u(function(r,n){this.catcodes[r]=n},"setCatcode"),e.lex=u(function(){var r=this.input,n=this.tokenRegex.lastIndex;if(n===r.length)return new ee("EOF",new z0(this,n,n));var a=this.tokenRegex.exec(r);if(a===null||a.index!==n)throw new k("Unexpected character: '"+r[n]+"'",new ee(r[n],new z0(this,n,n+1)));var o=a[2]||" ";if(this.catcodes[o]===14){var m=r.indexOf(`
`,this.tokenRegex.lastIndex);return m===-1?(this.tokenRegex.lastIndex=r.length,this.settings.reportNonstrict("commentAtEnd","% comment has no terminating newline; LaTeX would fail because of commenting the end of math mode (e.g. $)")):this.tokenRegex.lastIndex=m+1,this.lex()}var h=o.match(li);return h&&(o=h[1]),new ee(o,new z0(this,n,this.tokenRegex.lastIndex))},"lex"),l}(),mi=function(){function l(t,r){t===void 0&&(t={}),r===void 0&&(r={}),this.current=void 0,this.builtins=void 0,this.undefStack=void 0,this.current=r,this.builtins=t,this.undefStack=[]}u(l,"Namespace");var e=l.prototype;return e.beginGroup=u(function(){this.undefStack.push({})},"beginGroup"),e.endGroup=u(function(){if(this.undefStack.length===0)throw new k("Unbalanced namespace destruction: attempt to pop global namespace; please report this as a bug");var r=this.undefStack.pop();for(var n in r)r.hasOwnProperty(n)&&(r[n]===void 0?delete this.current[n]:this.current[n]=r[n])},"endGroup"),e.has=u(function(r){return this.current.hasOwnProperty(r)||this.builtins.hasOwnProperty(r)},"has"),e.get=u(function(r){return this.current.hasOwnProperty(r)?this.current[r]:this.builtins[r]},"get"),e.set=u(function(r,n,a){if(a===void 0&&(a=!1),a){for(var o=0;o<this.undefStack.length;o++)delete this.undefStack[o][r];this.undefStack.length>0&&(this.undefStack[this.undefStack.length-1][r]=n)}else{var m=this.undefStack[this.undefStack.length-1];m&&!m.hasOwnProperty(r)&&(m[r]=this.current[r])}this.current[r]=n},"set"),l}(),sn={},ci=sn;function d(l,e){sn[l]=e}u(d,"defineMacro"),d("\\noexpand",function(l){var e=l.popToken();return l.isExpandable(e.text)&&(e.noexpand=!0,e.treatAsRelax=!0),{tokens:[e],numArgs:0}}),d("\\expandafter",function(l){var e=l.popToken();return l.expandOnce(!0),{tokens:[e],numArgs:0}}),d("\\@firstoftwo",function(l){var e=l.consumeArgs(2);return{tokens:e[0],numArgs:0}}),d("\\@secondoftwo",function(l){var e=l.consumeArgs(2);return{tokens:e[1],numArgs:0}}),d("\\@ifnextchar",function(l){var e=l.consumeArgs(3);l.consumeSpaces();var t=l.future();return e[0].length===1&&e[0][0].text===t.text?{tokens:e[1],numArgs:0}:{tokens:e[2],numArgs:0}}),d("\\@ifstar","\\@ifnextchar *{\\@firstoftwo{#1}}"),d("\\TextOrMath",function(l){var e=l.consumeArgs(2);return l.mode==="text"?{tokens:e[0],numArgs:0}:{tokens:e[1],numArgs:0}});var un={0:0,1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9,a:10,A:10,b:11,B:11,c:12,C:12,d:13,D:13,e:14,E:14,f:15,F:15};d("\\char",function(l){var e=l.popToken(),t,r="";if(e.text==="'")t=8,e=l.popToken();else if(e.text==='"')t=16,e=l.popToken();else if(e.text==="`")if(e=l.popToken(),e.text[0]==="\\")r=e.text.charCodeAt(1);else{if(e.text==="EOF")throw new k("\\char` missing argument");r=e.text.charCodeAt(0)}else t=10;if(t){if(r=un[e.text],r==null||r>=t)throw new k("Invalid base-"+t+" digit "+e.text);for(var n;(n=un[l.future().text])!=null&&n<t;)r*=t,r+=n,l.popToken()}return"\\@char{"+r+"}"});var Lt=u(function(e,t,r){var n=e.consumeArg().tokens;if(n.length!==1)throw new k("\\newcommand's first argument must be a macro name");var a=n[0].text,o=e.isDefined(a);if(o&&!t)throw new k("\\newcommand{"+a+"} attempting to redefine "+(a+"; use \\renewcommand"));if(!o&&!r)throw new k("\\renewcommand{"+a+"} when command "+a+" does not yet exist; use \\newcommand");var m=0;if(n=e.consumeArg().tokens,n.length===1&&n[0].text==="["){for(var h="",f=e.expandNextToken();f.text!=="]"&&f.text!=="EOF";)h+=f.text,f=e.expandNextToken();if(!h.match(/^\s*[0-9]+\s*$/))throw new k("Invalid number of arguments: "+h);m=parseInt(h),n=e.consumeArg().tokens}return e.macros.set(a,{tokens:n,numArgs:m}),""},"newcommand");d("\\newcommand",function(l){return Lt(l,!1,!0)}),d("\\renewcommand",function(l){return Lt(l,!0,!1)}),d("\\providecommand",function(l){return Lt(l,!0,!0)}),d("\\message",function(l){var e=l.consumeArgs(1)[0];return console.log(e.reverse().map(function(t){return t.text}).join("")),""}),d("\\errmessage",function(l){var e=l.consumeArgs(1)[0];return console.error(e.reverse().map(function(t){return t.text}).join("")),""}),d("\\show",function(l){var e=l.popToken(),t=e.text;return console.log(e,l.macros.get(t),U0[t],_.math[t],_.text[t]),""}),d("\\bgroup","{"),d("\\egroup","}"),d("\\lq","`"),d("\\rq","'"),d("\\aa","\\r a"),d("\\AA","\\r A"),d("\\textcopyright","\\html@mathml{\\textcircled{c}}{\\char`©}"),d("\\copyright","\\TextOrMath{\\textcopyright}{\\text{\\textcopyright}}"),d("\\textregistered","\\html@mathml{\\textcircled{\\scriptsize R}}{\\char`®}"),d("ℬ","\\mathscr{B}"),d("ℰ","\\mathscr{E}"),d("ℱ","\\mathscr{F}"),d("ℋ","\\mathscr{H}"),d("ℐ","\\mathscr{I}"),d("ℒ","\\mathscr{L}"),d("ℳ","\\mathscr{M}"),d("ℛ","\\mathscr{R}"),d("ℭ","\\mathfrak{C}"),d("ℌ","\\mathfrak{H}"),d("ℨ","\\mathfrak{Z}"),d("\\Bbbk","\\Bbb{k}"),d("·","\\cdotp"),d("\\llap","\\mathllap{\\textrm{#1}}"),d("\\rlap","\\mathrlap{\\textrm{#1}}"),d("\\clap","\\mathclap{\\textrm{#1}}"),d("\\mathstrut","\\vphantom{(}"),d("\\underbar","\\underline{\\text{#1}}"),d("\\not",'\\html@mathml{\\mathrel{\\mathrlap\\@not}}{\\char"338}'),d("\\neq","\\html@mathml{\\mathrel{\\not=}}{\\mathrel{\\char`≠}}"),d("\\ne","\\neq"),d("≠","\\neq"),d("\\notin","\\html@mathml{\\mathrel{{\\in}\\mathllap{/\\mskip1mu}}}{\\mathrel{\\char`∉}}"),d("∉","\\notin"),d("≘","\\html@mathml{\\mathrel{=\\kern{-1em}\\raisebox{0.4em}{$\\scriptsize\\frown$}}}{\\mathrel{\\char`≘}}"),d("≙","\\html@mathml{\\stackrel{\\tiny\\wedge}{=}}{\\mathrel{\\char`≘}}"),d("≚","\\html@mathml{\\stackrel{\\tiny\\vee}{=}}{\\mathrel{\\char`≚}}"),d("≛","\\html@mathml{\\stackrel{\\scriptsize\\star}{=}}{\\mathrel{\\char`≛}}"),d("≝","\\html@mathml{\\stackrel{\\tiny\\mathrm{def}}{=}}{\\mathrel{\\char`≝}}"),d("≞","\\html@mathml{\\stackrel{\\tiny\\mathrm{m}}{=}}{\\mathrel{\\char`≞}}"),d("≟","\\html@mathml{\\stackrel{\\tiny?}{=}}{\\mathrel{\\char`≟}}"),d("⟂","\\perp"),d("‼","\\mathclose{!\\mkern-0.8mu!}"),d("∌","\\notni"),d("⌜","\\ulcorner"),d("⌝","\\urcorner"),d("⌞","\\llcorner"),d("⌟","\\lrcorner"),d("©","\\copyright"),d("®","\\textregistered"),d("️","\\textregistered"),d("\\ulcorner",'\\html@mathml{\\@ulcorner}{\\mathop{\\char"231c}}'),d("\\urcorner",'\\html@mathml{\\@urcorner}{\\mathop{\\char"231d}}'),d("\\llcorner",'\\html@mathml{\\@llcorner}{\\mathop{\\char"231e}}'),d("\\lrcorner",'\\html@mathml{\\@lrcorner}{\\mathop{\\char"231f}}'),d("\\vdots","\\mathord{\\varvdots\\rule{0pt}{15pt}}"),d("⋮","\\vdots"),d("\\varGamma","\\mathit{\\Gamma}"),d("\\varDelta","\\mathit{\\Delta}"),d("\\varTheta","\\mathit{\\Theta}"),d("\\varLambda","\\mathit{\\Lambda}"),d("\\varXi","\\mathit{\\Xi}"),d("\\varPi","\\mathit{\\Pi}"),d("\\varSigma","\\mathit{\\Sigma}"),d("\\varUpsilon","\\mathit{\\Upsilon}"),d("\\varPhi","\\mathit{\\Phi}"),d("\\varPsi","\\mathit{\\Psi}"),d("\\varOmega","\\mathit{\\Omega}"),d("\\substack","\\begin{subarray}{c}#1\\end{subarray}"),d("\\colon","\\nobreak\\mskip2mu\\mathpunct{}\\mathchoice{\\mkern-3mu}{\\mkern-3mu}{}{}{:}\\mskip6mu"),d("\\boxed","\\fbox{$\\displaystyle{#1}$}"),d("\\iff","\\DOTSB\\;\\Longleftrightarrow\\;"),d("\\implies","\\DOTSB\\;\\Longrightarrow\\;"),d("\\impliedby","\\DOTSB\\;\\Longleftarrow\\;");var mn={",":"\\dotsc","\\not":"\\dotsb","+":"\\dotsb","=":"\\dotsb","<":"\\dotsb",">":"\\dotsb","-":"\\dotsb","*":"\\dotsb",":":"\\dotsb","\\DOTSB":"\\dotsb","\\coprod":"\\dotsb","\\bigvee":"\\dotsb","\\bigwedge":"\\dotsb","\\biguplus":"\\dotsb","\\bigcap":"\\dotsb","\\bigcup":"\\dotsb","\\prod":"\\dotsb","\\sum":"\\dotsb","\\bigotimes":"\\dotsb","\\bigoplus":"\\dotsb","\\bigodot":"\\dotsb","\\bigsqcup":"\\dotsb","\\And":"\\dotsb","\\longrightarrow":"\\dotsb","\\Longrightarrow":"\\dotsb","\\longleftarrow":"\\dotsb","\\Longleftarrow":"\\dotsb","\\longleftrightarrow":"\\dotsb","\\Longleftrightarrow":"\\dotsb","\\mapsto":"\\dotsb","\\longmapsto":"\\dotsb","\\hookrightarrow":"\\dotsb","\\doteq":"\\dotsb","\\mathbin":"\\dotsb","\\mathrel":"\\dotsb","\\relbar":"\\dotsb","\\Relbar":"\\dotsb","\\xrightarrow":"\\dotsb","\\xleftarrow":"\\dotsb","\\DOTSI":"\\dotsi","\\int":"\\dotsi","\\oint":"\\dotsi","\\iint":"\\dotsi","\\iiint":"\\dotsi","\\iiiint":"\\dotsi","\\idotsint":"\\dotsi","\\DOTSX":"\\dotsx"};d("\\dots",function(l){var e="\\dotso",t=l.expandAfterFuture().text;return t in mn?e=mn[t]:(t.substr(0,4)==="\\not"||t in _.math&&E.contains(["bin","rel"],_.math[t].group))&&(e="\\dotsb"),e});var qt={")":!0,"]":!0,"\\rbrack":!0,"\\}":!0,"\\rbrace":!0,"\\rangle":!0,"\\rceil":!0,"\\rfloor":!0,"\\rgroup":!0,"\\rmoustache":!0,"\\right":!0,"\\bigr":!0,"\\biggr":!0,"\\Bigr":!0,"\\Biggr":!0,$:!0,";":!0,".":!0,",":!0};d("\\dotso",function(l){var e=l.future().text;return e in qt?"\\ldots\\,":"\\ldots"}),d("\\dotsc",function(l){var e=l.future().text;return e in qt&&e!==","?"\\ldots\\,":"\\ldots"}),d("\\cdots",function(l){var e=l.future().text;return e in qt?"\\@cdots\\,":"\\@cdots"}),d("\\dotsb","\\cdots"),d("\\dotsm","\\cdots"),d("\\dotsi","\\!\\cdots"),d("\\dotsx","\\ldots\\,"),d("\\DOTSI","\\relax"),d("\\DOTSB","\\relax"),d("\\DOTSX","\\relax"),d("\\tmspace","\\TextOrMath{\\kern#1#3}{\\mskip#1#2}\\relax"),d("\\,","\\tmspace+{3mu}{.1667em}"),d("\\thinspace","\\,"),d("\\>","\\mskip{4mu}"),d("\\:","\\tmspace+{4mu}{.2222em}"),d("\\medspace","\\:"),d("\\;","\\tmspace+{5mu}{.2777em}"),d("\\thickspace","\\;"),d("\\!","\\tmspace-{3mu}{.1667em}"),d("\\negthinspace","\\!"),d("\\negmedspace","\\tmspace-{4mu}{.2222em}"),d("\\negthickspace","\\tmspace-{5mu}{.277em}"),d("\\enspace","\\kern.5em "),d("\\enskip","\\hskip.5em\\relax"),d("\\quad","\\hskip1em\\relax"),d("\\qquad","\\hskip2em\\relax"),d("\\tag","\\@ifstar\\tag@literal\\tag@paren"),d("\\tag@paren","\\tag@literal{({#1})}"),d("\\tag@literal",function(l){if(l.macros.get("\\df@tag"))throw new k("Multiple \\tag");return"\\gdef\\df@tag{\\text{#1}}"}),d("\\bmod","\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}\\mathbin{\\rm mod}\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}"),d("\\pod","\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern8mu}{\\mkern8mu}{\\mkern8mu}(#1)"),d("\\pmod","\\pod{{\\rm mod}\\mkern6mu#1}"),d("\\mod","\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern12mu}{\\mkern12mu}{\\mkern12mu}{\\rm mod}\\,\\,#1"),d("\\pmb","\\html@mathml{\\@binrel{#1}{\\mathrlap{#1}\\kern0.5px#1}}{\\mathbf{#1}}"),d("\\newline","\\\\\\relax"),d("\\TeX","\\textrm{\\html@mathml{T\\kern-.1667em\\raisebox{-.5ex}{E}\\kern-.125emX}{TeX}}");var cn=B0["Main-Regular"]["T".charCodeAt(0)][1]-.7*B0["Main-Regular"]["A".charCodeAt(0)][1]+"em";d("\\LaTeX","\\textrm{\\html@mathml{"+("L\\kern-.36em\\raisebox{"+cn+"}{\\scriptstyle A}")+"\\kern-.15em\\TeX}{LaTeX}}"),d("\\KaTeX","\\textrm{\\html@mathml{"+("K\\kern-.17em\\raisebox{"+cn+"}{\\scriptstyle A}")+"\\kern-.15em\\TeX}{KaTeX}}"),d("\\hspace","\\@ifstar\\@hspacer\\@hspace"),d("\\@hspace","\\hskip #1\\relax"),d("\\@hspacer","\\rule{0pt}{0pt}\\hskip #1\\relax"),d("\\ordinarycolon",":"),d("\\vcentcolon","\\mathrel{\\mathop\\ordinarycolon}"),d("\\dblcolon",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-.9mu}\\vcentcolon}}{\\mathop{\\char"2237}}'),d("\\coloneqq",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2254}}'),d("\\Coloneqq",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2237\\char"3d}}'),d("\\coloneq",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"3a\\char"2212}}'),d("\\Coloneq",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"2237\\char"2212}}'),d("\\eqqcolon",'\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2255}}'),d("\\Eqqcolon",'\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"3d\\char"2237}}'),d("\\eqcolon",'\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2239}}'),d("\\Eqcolon",'\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"2212\\char"2237}}'),d("\\colonapprox",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"3a\\char"2248}}'),d("\\Colonapprox",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"2237\\char"2248}}'),d("\\colonsim",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"3a\\char"223c}}'),d("\\Colonsim",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"2237\\char"223c}}'),d("∷","\\dblcolon"),d("∹","\\eqcolon"),d("≔","\\coloneqq"),d("≕","\\eqqcolon"),d("⩴","\\Coloneqq"),d("\\ratio","\\vcentcolon"),d("\\coloncolon","\\dblcolon"),d("\\colonequals","\\coloneqq"),d("\\coloncolonequals","\\Coloneqq"),d("\\equalscolon","\\eqqcolon"),d("\\equalscoloncolon","\\Eqqcolon"),d("\\colonminus","\\coloneq"),d("\\coloncolonminus","\\Coloneq"),d("\\minuscolon","\\eqcolon"),d("\\minuscoloncolon","\\Eqcolon"),d("\\coloncolonapprox","\\Colonapprox"),d("\\coloncolonsim","\\Colonsim"),d("\\simcolon","\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\vcentcolon}"),d("\\simcoloncolon","\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\dblcolon}"),d("\\approxcolon","\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\vcentcolon}"),d("\\approxcoloncolon","\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\dblcolon}"),d("\\notni","\\html@mathml{\\not\\ni}{\\mathrel{\\char`∌}}"),d("\\limsup","\\DOTSB\\operatorname*{lim\\,sup}"),d("\\liminf","\\DOTSB\\operatorname*{lim\\,inf}"),d("\\injlim","\\DOTSB\\operatorname*{inj\\,lim}"),d("\\projlim","\\DOTSB\\operatorname*{proj\\,lim}"),d("\\varlimsup","\\DOTSB\\operatorname*{\\overline{lim}}"),d("\\varliminf","\\DOTSB\\operatorname*{\\underline{lim}}"),d("\\varinjlim","\\DOTSB\\operatorname*{\\underrightarrow{lim}}"),d("\\varprojlim","\\DOTSB\\operatorname*{\\underleftarrow{lim}}"),d("\\gvertneqq","\\html@mathml{\\@gvertneqq}{≩}"),d("\\lvertneqq","\\html@mathml{\\@lvertneqq}{≨}"),d("\\ngeqq","\\html@mathml{\\@ngeqq}{≱}"),d("\\ngeqslant","\\html@mathml{\\@ngeqslant}{≱}"),d("\\nleqq","\\html@mathml{\\@nleqq}{≰}"),d("\\nleqslant","\\html@mathml{\\@nleqslant}{≰}"),d("\\nshortmid","\\html@mathml{\\@nshortmid}{∤}"),d("\\nshortparallel","\\html@mathml{\\@nshortparallel}{∦}"),d("\\nsubseteqq","\\html@mathml{\\@nsubseteqq}{⊈}"),d("\\nsupseteqq","\\html@mathml{\\@nsupseteqq}{⊉}"),d("\\varsubsetneq","\\html@mathml{\\@varsubsetneq}{⊊}"),d("\\varsubsetneqq","\\html@mathml{\\@varsubsetneqq}{⫋}"),d("\\varsupsetneq","\\html@mathml{\\@varsupsetneq}{⊋}"),d("\\varsupsetneqq","\\html@mathml{\\@varsupsetneqq}{⫌}"),d("\\imath","\\html@mathml{\\@imath}{ı}"),d("\\jmath","\\html@mathml{\\@jmath}{ȷ}"),d("\\llbracket","\\html@mathml{\\mathopen{[\\mkern-3.2mu[}}{\\mathopen{\\char`⟦}}"),d("\\rrbracket","\\html@mathml{\\mathclose{]\\mkern-3.2mu]}}{\\mathclose{\\char`⟧}}"),d("⟦","\\llbracket"),d("⟧","\\rrbracket"),d("\\lBrace","\\html@mathml{\\mathopen{\\{\\mkern-3.2mu[}}{\\mathopen{\\char`⦃}}"),d("\\rBrace","\\html@mathml{\\mathclose{]\\mkern-3.2mu\\}}}{\\mathclose{\\char`⦄}}"),d("⦃","\\lBrace"),d("⦄","\\rBrace"),d("\\minuso","\\mathbin{\\html@mathml{{\\mathrlap{\\mathchoice{\\kern{0.145em}}{\\kern{0.145em}}{\\kern{0.1015em}}{\\kern{0.0725em}}\\circ}{-}}}{\\char`⦵}}"),d("⦵","\\minuso"),d("\\darr","\\downarrow"),d("\\dArr","\\Downarrow"),d("\\Darr","\\Downarrow"),d("\\lang","\\langle"),d("\\rang","\\rangle"),d("\\uarr","\\uparrow"),d("\\uArr","\\Uparrow"),d("\\Uarr","\\Uparrow"),d("\\N","\\mathbb{N}"),d("\\R","\\mathbb{R}"),d("\\Z","\\mathbb{Z}"),d("\\alef","\\aleph"),d("\\alefsym","\\aleph"),d("\\Alpha","\\mathrm{A}"),d("\\Beta","\\mathrm{B}"),d("\\bull","\\bullet"),d("\\Chi","\\mathrm{X}"),d("\\clubs","\\clubsuit"),d("\\cnums","\\mathbb{C}"),d("\\Complex","\\mathbb{C}"),d("\\Dagger","\\ddagger"),d("\\diamonds","\\diamondsuit"),d("\\empty","\\emptyset"),d("\\Epsilon","\\mathrm{E}"),d("\\Eta","\\mathrm{H}"),d("\\exist","\\exists"),d("\\harr","\\leftrightarrow"),d("\\hArr","\\Leftrightarrow"),d("\\Harr","\\Leftrightarrow"),d("\\hearts","\\heartsuit"),d("\\image","\\Im"),d("\\infin","\\infty"),d("\\Iota","\\mathrm{I}"),d("\\isin","\\in"),d("\\Kappa","\\mathrm{K}"),d("\\larr","\\leftarrow"),d("\\lArr","\\Leftarrow"),d("\\Larr","\\Leftarrow"),d("\\lrarr","\\leftrightarrow"),d("\\lrArr","\\Leftrightarrow"),d("\\Lrarr","\\Leftrightarrow"),d("\\Mu","\\mathrm{M}"),d("\\natnums","\\mathbb{N}"),d("\\Nu","\\mathrm{N}"),d("\\Omicron","\\mathrm{O}"),d("\\plusmn","\\pm"),d("\\rarr","\\rightarrow"),d("\\rArr","\\Rightarrow"),d("\\Rarr","\\Rightarrow"),d("\\real","\\Re"),d("\\reals","\\mathbb{R}"),d("\\Reals","\\mathbb{R}"),d("\\Rho","\\mathrm{P}"),d("\\sdot","\\cdot"),d("\\sect","\\S"),d("\\spades","\\spadesuit"),d("\\sub","\\subset"),d("\\sube","\\subseteq"),d("\\supe","\\supseteq"),d("\\Tau","\\mathrm{T}"),d("\\thetasym","\\vartheta"),d("\\weierp","\\wp"),d("\\Zeta","\\mathrm{Z}"),d("\\argmin","\\DOTSB\\operatorname*{arg\\,min}"),d("\\argmax","\\DOTSB\\operatorname*{arg\\,max}"),d("\\plim","\\DOTSB\\mathop{\\operatorname{plim}}\\limits"),d("\\bra","\\mathinner{\\langle{#1}|}"),d("\\ket","\\mathinner{|{#1}\\rangle}"),d("\\braket","\\mathinner{\\langle{#1}\\rangle}"),d("\\Bra","\\left\\langle#1\\right|"),d("\\Ket","\\left|#1\\right\\rangle"),d("\\angln","{\\angl n}"),d("\\blue","\\textcolor{##6495ed}{#1}"),d("\\orange","\\textcolor{##ffa500}{#1}"),d("\\pink","\\textcolor{##ff00af}{#1}"),d("\\red","\\textcolor{##df0030}{#1}"),d("\\green","\\textcolor{##28ae7b}{#1}"),d("\\gray","\\textcolor{gray}{#1}"),d("\\purple","\\textcolor{##9d38bd}{#1}"),d("\\blueA","\\textcolor{##ccfaff}{#1}"),d("\\blueB","\\textcolor{##80f6ff}{#1}"),d("\\blueC","\\textcolor{##63d9ea}{#1}"),d("\\blueD","\\textcolor{##11accd}{#1}"),d("\\blueE","\\textcolor{##0c7f99}{#1}"),d("\\tealA","\\textcolor{##94fff5}{#1}"),d("\\tealB","\\textcolor{##26edd5}{#1}"),d("\\tealC","\\textcolor{##01d1c1}{#1}"),d("\\tealD","\\textcolor{##01a995}{#1}"),d("\\tealE","\\textcolor{##208170}{#1}"),d("\\greenA","\\textcolor{##b6ffb0}{#1}"),d("\\greenB","\\textcolor{##8af281}{#1}"),d("\\greenC","\\textcolor{##74cf70}{#1}"),d("\\greenD","\\textcolor{##1fab54}{#1}"),d("\\greenE","\\textcolor{##0d923f}{#1}"),d("\\goldA","\\textcolor{##ffd0a9}{#1}"),d("\\goldB","\\textcolor{##ffbb71}{#1}"),d("\\goldC","\\textcolor{##ff9c39}{#1}"),d("\\goldD","\\textcolor{##e07d10}{#1}"),d("\\goldE","\\textcolor{##a75a05}{#1}"),d("\\redA","\\textcolor{##fca9a9}{#1}"),d("\\redB","\\textcolor{##ff8482}{#1}"),d("\\redC","\\textcolor{##f9685d}{#1}"),d("\\redD","\\textcolor{##e84d39}{#1}"),d("\\redE","\\textcolor{##bc2612}{#1}"),d("\\maroonA","\\textcolor{##ffbde0}{#1}"),d("\\maroonB","\\textcolor{##ff92c6}{#1}"),d("\\maroonC","\\textcolor{##ed5fa6}{#1}"),d("\\maroonD","\\textcolor{##ca337c}{#1}"),d("\\maroonE","\\textcolor{##9e034e}{#1}"),d("\\purpleA","\\textcolor{##ddd7ff}{#1}"),d("\\purpleB","\\textcolor{##c6b9fc}{#1}"),d("\\purpleC","\\textcolor{##aa87ff}{#1}"),d("\\purpleD","\\textcolor{##7854ab}{#1}"),d("\\purpleE","\\textcolor{##543b78}{#1}"),d("\\mintA","\\textcolor{##f5f9e8}{#1}"),d("\\mintB","\\textcolor{##edf2df}{#1}"),d("\\mintC","\\textcolor{##e0e5cc}{#1}"),d("\\grayA","\\textcolor{##f6f7f7}{#1}"),d("\\grayB","\\textcolor{##f0f1f2}{#1}"),d("\\grayC","\\textcolor{##e3e5e6}{#1}"),d("\\grayD","\\textcolor{##d6d8da}{#1}"),d("\\grayE","\\textcolor{##babec2}{#1}"),d("\\grayF","\\textcolor{##888d93}{#1}"),d("\\grayG","\\textcolor{##626569}{#1}"),d("\\grayH","\\textcolor{##3b3e40}{#1}"),d("\\grayI","\\textcolor{##21242c}{#1}"),d("\\kaBlue","\\textcolor{##314453}{#1}"),d("\\kaGreen","\\textcolor{##71B307}{#1}");var hn={"\\relax":!0,"^":!0,_:!0,"\\limits":!0,"\\nolimits":!0},hi=function(){function l(t,r,n){this.settings=void 0,this.expansionCount=void 0,this.lexer=void 0,this.macros=void 0,this.stack=void 0,this.mode=void 0,this.settings=r,this.expansionCount=0,this.feed(t),this.macros=new mi(ci,r.macros),this.mode=n,this.stack=[]}u(l,"MacroExpander");var e=l.prototype;return e.feed=u(function(r){this.lexer=new ln(r,this.settings)},"feed"),e.switchMode=u(function(r){this.mode=r},"switchMode"),e.beginGroup=u(function(){this.macros.beginGroup()},"beginGroup"),e.endGroup=u(function(){this.macros.endGroup()},"endGroup"),e.future=u(function(){return this.stack.length===0&&this.pushToken(this.lexer.lex()),this.stack[this.stack.length-1]},"future"),e.popToken=u(function(){return this.future(),this.stack.pop()},"popToken"),e.pushToken=u(function(r){this.stack.push(r)},"pushToken"),e.pushTokens=u(function(r){var n;(n=this.stack).push.apply(n,r)},"pushTokens"),e.scanArgument=u(function(r){var n,a,o;if(r){if(this.consumeSpaces(),this.future().text!=="[")return null;n=this.popToken();var m=this.consumeArg(["]"]);o=m.tokens,a=m.end}else{var h=this.consumeArg();o=h.tokens,n=h.start,a=h.end}return this.pushToken(new ee("EOF",a.loc)),this.pushTokens(o),n.range(a,"")},"scanArgument"),e.consumeSpaces=u(function(){for(;;){var r=this.future();if(r.text===" ")this.stack.pop();else break}},"consumeSpaces"),e.consumeArg=u(function(r){var n=[],a=r&&r.length>0;a||this.consumeSpaces();var o=this.future(),m,h=0,f=0;do{if(m=this.popToken(),n.push(m),m.text==="{")++h;else if(m.text==="}"){if(--h,h===-1)throw new k("Extra }",m)}else if(m.text==="EOF")throw new k("Unexpected end of input in a macro argument, expected '"+(r&&a?r[f]:"}")+"'",m);if(r&&a)if((h===0||h===1&&r[f]==="{")&&m.text===r[f]){if(++f,f===r.length){n.splice(-f,f);break}}else f=0}while(h!==0||a);return o.text==="{"&&n[n.length-1].text==="}"&&(n.pop(),n.shift()),n.reverse(),{tokens:n,start:o,end:m}},"consumeArg"),e.consumeArgs=u(function(r,n){if(n){if(n.length!==r+1)throw new k("The length of delimiters doesn't match the number of args!");for(var a=n[0],o=0;o<a.length;o++){var m=this.popToken();if(a[o]!==m.text)throw new k("Use of the macro doesn't match its definition",m)}}for(var h=[],f=0;f<r;f++)h.push(this.consumeArg(n&&n[f+1]).tokens);return h},"consumeArgs"),e.expandOnce=u(function(r){var n=this.popToken(),a=n.text,o=n.noexpand?null:this._getExpansion(a);if(o==null||r&&o.unexpandable){if(r&&o==null&&a[0]==="\\"&&!this.isDefined(a))throw new k("Undefined control sequence: "+a);return this.pushToken(n),n}if(this.expansionCount++,this.expansionCount>this.settings.maxExpand)throw new k("Too many expansions: infinite loop or need to increase maxExpand setting");var m=o.tokens,h=this.consumeArgs(o.numArgs,o.delimiters);if(o.numArgs){m=m.slice();for(var f=m.length-1;f>=0;--f){var g=m[f];if(g.text==="#"){if(f===0)throw new k("Incomplete placeholder at end of macro body",g);if(g=m[--f],g.text==="#")m.splice(f+1,1);else if(/^[1-9]$/.test(g.text)){var y;(y=m).splice.apply(y,[f,2].concat(h[+g.text-1]))}else throw new k("Not a valid argument number",g)}}}return this.pushTokens(m),m},"expandOnce"),e.expandAfterFuture=u(function(){return this.expandOnce(),this.future()},"expandAfterFuture"),e.expandNextToken=u(function(){for(;;){var r=this.expandOnce();if(r instanceof ee)if(r.text==="\\relax"||r.treatAsRelax)this.stack.pop();else return this.stack.pop()}throw new Error},"expandNextToken"),e.expandMacro=u(function(r){return this.macros.has(r)?this.expandTokens([new ee(r)]):void 0},"expandMacro"),e.expandTokens=u(function(r){var n=[],a=this.stack.length;for(this.pushTokens(r);this.stack.length>a;){var o=this.expandOnce(!0);o instanceof ee&&(o.treatAsRelax&&(o.noexpand=!1,o.treatAsRelax=!1),n.push(this.stack.pop()))}return n},"expandTokens"),e.expandMacroAsText=u(function(r){var n=this.expandMacro(r);return n&&n.map(function(a){return a.text}).join("")},"expandMacroAsText"),e._getExpansion=u(function(r){var n=this.macros.get(r);if(n==null)return n;var a=typeof n=="function"?n(this):n;if(typeof a=="string"){var o=0;if(a.indexOf("#")!==-1)for(var m=a.replace(/##/g,"");m.indexOf("#"+(o+1))!==-1;)++o;for(var h=new ln(a,this.settings),f=[],g=h.lex();g.text!=="EOF";)f.push(g),g=h.lex();f.reverse();var y={tokens:f,numArgs:o};return y}return a},"_getExpansion"),e.isDefined=u(function(r){return this.macros.has(r)||U0.hasOwnProperty(r)||_.math.hasOwnProperty(r)||_.text.hasOwnProperty(r)||hn.hasOwnProperty(r)},"isDefined"),e.isExpandable=u(function(r){var n=this.macros.get(r);return n!=null?typeof n=="string"||typeof n=="function"||!n.unexpandable:U0.hasOwnProperty(r)&&!U0[r].primitive},"isExpandable"),l}(),dn={"́":{text:"\\'",math:"\\acute"},"̀":{text:"\\`",math:"\\grave"},"̈":{text:'\\"',math:"\\ddot"},"̃":{text:"\\~",math:"\\tilde"},"̄":{text:"\\=",math:"\\bar"},"̆":{text:"\\u",math:"\\breve"},"̌":{text:"\\v",math:"\\check"},"̂":{text:"\\^",math:"\\hat"},"̇":{text:"\\.",math:"\\dot"},"̊":{text:"\\r",math:"\\mathring"},"̋":{text:"\\H"}},fn={á:"á",à:"à",ä:"ä",ǟ:"ǟ",ã:"ã",ā:"ā",ă:"ă",ắ:"ắ",ằ:"ằ",ẵ:"ẵ",ǎ:"ǎ",â:"â",ấ:"ấ",ầ:"ầ",ẫ:"ẫ",ȧ:"ȧ",ǡ:"ǡ",å:"å",ǻ:"ǻ",ḃ:"ḃ",ć:"ć",č:"č",ĉ:"ĉ",ċ:"ċ",ď:"ď",ḋ:"ḋ",é:"é",è:"è",ë:"ë",ẽ:"ẽ",ē:"ē",ḗ:"ḗ",ḕ:"ḕ",ĕ:"ĕ",ě:"ě",ê:"ê",ế:"ế",ề:"ề",ễ:"ễ",ė:"ė",ḟ:"ḟ",ǵ:"ǵ",ḡ:"ḡ",ğ:"ğ",ǧ:"ǧ",ĝ:"ĝ",ġ:"ġ",ḧ:"ḧ",ȟ:"ȟ",ĥ:"ĥ",ḣ:"ḣ",í:"í",ì:"ì",ï:"ï",ḯ:"ḯ",ĩ:"ĩ",ī:"ī",ĭ:"ĭ",ǐ:"ǐ",î:"î",ǰ:"ǰ",ĵ:"ĵ",ḱ:"ḱ",ǩ:"ǩ",ĺ:"ĺ",ľ:"ľ",ḿ:"ḿ",ṁ:"ṁ",ń:"ń",ǹ:"ǹ",ñ:"ñ",ň:"ň",ṅ:"ṅ",ó:"ó",ò:"ò",ö:"ö",ȫ:"ȫ",õ:"õ",ṍ:"ṍ",ṏ:"ṏ",ȭ:"ȭ",ō:"ō",ṓ:"ṓ",ṑ:"ṑ",ŏ:"ŏ",ǒ:"ǒ",ô:"ô",ố:"ố",ồ:"ồ",ỗ:"ỗ",ȯ:"ȯ",ȱ:"ȱ",ő:"ő",ṕ:"ṕ",ṗ:"ṗ",ŕ:"ŕ",ř:"ř",ṙ:"ṙ",ś:"ś",ṥ:"ṥ",š:"š",ṧ:"ṧ",ŝ:"ŝ",ṡ:"ṡ",ẗ:"ẗ",ť:"ť",ṫ:"ṫ",ú:"ú",ù:"ù",ü:"ü",ǘ:"ǘ",ǜ:"ǜ",ǖ:"ǖ",ǚ:"ǚ",ũ:"ũ",ṹ:"ṹ",ū:"ū",ṻ:"ṻ",ŭ:"ŭ",ǔ:"ǔ",û:"û",ů:"ů",ű:"ű",ṽ:"ṽ",ẃ:"ẃ",ẁ:"ẁ",ẅ:"ẅ",ŵ:"ŵ",ẇ:"ẇ",ẘ:"ẘ",ẍ:"ẍ",ẋ:"ẋ",ý:"ý",ỳ:"ỳ",ÿ:"ÿ",ỹ:"ỹ",ȳ:"ȳ",ŷ:"ŷ",ẏ:"ẏ",ẙ:"ẙ",ź:"ź",ž:"ž",ẑ:"ẑ",ż:"ż",Á:"Á",À:"À",Ä:"Ä",Ǟ:"Ǟ",Ã:"Ã",Ā:"Ā",Ă:"Ă",Ắ:"Ắ",Ằ:"Ằ",Ẵ:"Ẵ",Ǎ:"Ǎ",Â:"Â",Ấ:"Ấ",Ầ:"Ầ",Ẫ:"Ẫ",Ȧ:"Ȧ",Ǡ:"Ǡ",Å:"Å",Ǻ:"Ǻ",Ḃ:"Ḃ",Ć:"Ć",Č:"Č",Ĉ:"Ĉ",Ċ:"Ċ",Ď:"Ď",Ḋ:"Ḋ",É:"É",È:"È",Ë:"Ë",Ẽ:"Ẽ",Ē:"Ē",Ḗ:"Ḗ",Ḕ:"Ḕ",Ĕ:"Ĕ",Ě:"Ě",Ê:"Ê",Ế:"Ế",Ề:"Ề",Ễ:"Ễ",Ė:"Ė",Ḟ:"Ḟ",Ǵ:"Ǵ",Ḡ:"Ḡ",Ğ:"Ğ",Ǧ:"Ǧ",Ĝ:"Ĝ",Ġ:"Ġ",Ḧ:"Ḧ",Ȟ:"Ȟ",Ĥ:"Ĥ",Ḣ:"Ḣ",Í:"Í",Ì:"Ì",Ï:"Ï",Ḯ:"Ḯ",Ĩ:"Ĩ",Ī:"Ī",Ĭ:"Ĭ",Ǐ:"Ǐ",Î:"Î",İ:"İ",Ĵ:"Ĵ",Ḱ:"Ḱ",Ǩ:"Ǩ",Ĺ:"Ĺ",Ľ:"Ľ",Ḿ:"Ḿ",Ṁ:"Ṁ",Ń:"Ń",Ǹ:"Ǹ",Ñ:"Ñ",Ň:"Ň",Ṅ:"Ṅ",Ó:"Ó",Ò:"Ò",Ö:"Ö",Ȫ:"Ȫ",Õ:"Õ",Ṍ:"Ṍ",Ṏ:"Ṏ",Ȭ:"Ȭ",Ō:"Ō",Ṓ:"Ṓ",Ṑ:"Ṑ",Ŏ:"Ŏ",Ǒ:"Ǒ",Ô:"Ô",Ố:"Ố",Ồ:"Ồ",Ỗ:"Ỗ",Ȯ:"Ȯ",Ȱ:"Ȱ",Ő:"Ő",Ṕ:"Ṕ",Ṗ:"Ṗ",Ŕ:"Ŕ",Ř:"Ř",Ṙ:"Ṙ",Ś:"Ś",Ṥ:"Ṥ",Š:"Š",Ṧ:"Ṧ",Ŝ:"Ŝ",Ṡ:"Ṡ",Ť:"Ť",Ṫ:"Ṫ",Ú:"Ú",Ù:"Ù",Ü:"Ü",Ǘ:"Ǘ",Ǜ:"Ǜ",Ǖ:"Ǖ",Ǚ:"Ǚ",Ũ:"Ũ",Ṹ:"Ṹ",Ū:"Ū",Ṻ:"Ṻ",Ŭ:"Ŭ",Ǔ:"Ǔ",Û:"Û",Ů:"Ů",Ű:"Ű",Ṽ:"Ṽ",Ẃ:"Ẃ",Ẁ:"Ẁ",Ẅ:"Ẅ",Ŵ:"Ŵ",Ẇ:"Ẇ",Ẍ:"Ẍ",Ẋ:"Ẋ",Ý:"Ý",Ỳ:"Ỳ",Ÿ:"Ÿ",Ỹ:"Ỹ",Ȳ:"Ȳ",Ŷ:"Ŷ",Ẏ:"Ẏ",Ź:"Ź",Ž:"Ž",Ẑ:"Ẑ",Ż:"Ż",ά:"ά",ὰ:"ὰ",ᾱ:"ᾱ",ᾰ:"ᾰ",έ:"έ",ὲ:"ὲ",ή:"ή",ὴ:"ὴ",ί:"ί",ὶ:"ὶ",ϊ:"ϊ",ΐ:"ΐ",ῒ:"ῒ",ῑ:"ῑ",ῐ:"ῐ",ό:"ό",ὸ:"ὸ",ύ:"ύ",ὺ:"ὺ",ϋ:"ϋ",ΰ:"ΰ",ῢ:"ῢ",ῡ:"ῡ",ῠ:"ῠ",ώ:"ώ",ὼ:"ὼ",Ύ:"Ύ",Ὺ:"Ὺ",Ϋ:"Ϋ",Ῡ:"Ῡ",Ῠ:"Ῠ",Ώ:"Ώ",Ὼ:"Ὼ"},pn=function(){function l(t,r){this.mode=void 0,this.gullet=void 0,this.settings=void 0,this.leftrightDepth=void 0,this.nextToken=void 0,this.mode="math",this.gullet=new hi(t,r,this.mode),this.settings=r,this.leftrightDepth=0}u(l,"Parser");var e=l.prototype;return e.expect=u(function(r,n){if(n===void 0&&(n=!0),this.fetch().text!==r)throw new k("Expected '"+r+"', got '"+this.fetch().text+"'",this.fetch());n&&this.consume()},"expect"),e.consume=u(function(){this.nextToken=null},"consume"),e.fetch=u(function(){return this.nextToken==null&&(this.nextToken=this.gullet.expandNextToken()),this.nextToken},"fetch"),e.switchMode=u(function(r){this.mode=r,this.gullet.switchMode(r)},"switchMode"),e.parse=u(function(){this.settings.globalGroup||this.gullet.beginGroup(),this.settings.colorIsTextColor&&this.gullet.macros.set("\\color","\\textcolor");var r=this.parseExpression(!1);return this.expect("EOF"),this.settings.globalGroup||this.gullet.endGroup(),r},"parse"),e.parseExpression=u(function(r,n){for(var a=[];;){this.mode==="math"&&this.consumeSpaces();var o=this.fetch();if(l.endOfExpression.indexOf(o.text)!==-1||n&&o.text===n||r&&U0[o.text]&&U0[o.text].infix)break;var m=this.parseAtom(n);if(m){if(m.type==="internal")continue}else break;a.push(m)}return this.mode==="text"&&this.formLigatures(a),this.handleInfixNodes(a)},"parseExpression"),e.handleInfixNodes=u(function(r){for(var n=-1,a,o=0;o<r.length;o++)if(r[o].type==="infix"){if(n!==-1)throw new k("only one infix operator per group",r[o].token);n=o,a=r[o].replaceWith}if(n!==-1&&a){var m,h,f=r.slice(0,n),g=r.slice(n+1);f.length===1&&f[0].type==="ordgroup"?m=f[0]:m={type:"ordgroup",mode:this.mode,body:f},g.length===1&&g[0].type==="ordgroup"?h=g[0]:h={type:"ordgroup",mode:this.mode,body:g};var y;return a==="\\\\abovefrac"?y=this.callFunction(a,[m,r[n],h],[]):y=this.callFunction(a,[m,h],[]),[y]}else return r},"handleInfixNodes"),e.handleSupSubscript=u(function(r){var n=this.fetch(),a=n.text;this.consume(),this.consumeSpaces();var o=this.parseGroup(r);if(!o)throw new k("Expected group after '"+a+"'",n);return o},"handleSupSubscript"),e.formatUnsupportedCmd=u(function(r){for(var n=[],a=0;a<r.length;a++)n.push({type:"textord",mode:"text",text:r[a]});var o={type:"text",mode:this.mode,body:n},m={type:"color",mode:this.mode,color:this.settings.errorColor,body:[o]};return m},"formatUnsupportedCmd"),e.parseAtom=u(function(r){var n=this.parseGroup("atom",r);if(this.mode==="text")return n;for(var a,o;;){this.consumeSpaces();var m=this.fetch();if(m.text==="\\limits"||m.text==="\\nolimits"){if(n&&n.type==="op"){var h=m.text==="\\limits";n.limits=h,n.alwaysHandleSupSub=!0}else if(n&&n.type==="operatorname"&&n.alwaysHandleSupSub){var f=m.text==="\\limits";n.limits=f}else throw new k("Limit controls must follow a math operator",m);this.consume()}else if(m.text==="^"){if(a)throw new k("Double superscript",m);a=this.handleSupSubscript("superscript")}else if(m.text==="_"){if(o)throw new k("Double subscript",m);o=this.handleSupSubscript("subscript")}else if(m.text==="'"){if(a)throw new k("Double superscript",m);var g={type:"textord",mode:this.mode,text:"\\prime"},y=[g];for(this.consume();this.fetch().text==="'";)y.push(g),this.consume();this.fetch().text==="^"&&y.push(this.handleSupSubscript("superscript")),a={type:"ordgroup",mode:this.mode,body:y}}else break}return a||o?{type:"supsub",mode:this.mode,base:n,sup:a,sub:o}:n},"parseAtom"),e.parseFunction=u(function(r,n){var a=this.fetch(),o=a.text,m=U0[o];if(!m)return null;if(this.consume(),n&&n!=="atom"&&!m.allowedInArgument)throw new k("Got function '"+o+"' with no arguments"+(n?" as "+n:""),a);if(this.mode==="text"&&!m.allowedInText)throw new k("Can't use function '"+o+"' in text mode",a);if(this.mode==="math"&&m.allowedInMath===!1)throw new k("Can't use function '"+o+"' in math mode",a);var h=this.parseArguments(o,m),f=h.args,g=h.optArgs;return this.callFunction(o,f,g,a,r)},"parseFunction"),e.callFunction=u(function(r,n,a,o,m){var h={funcName:r,parser:this,token:o,breakOnTokenText:m},f=U0[r];if(f&&f.handler)return f.handler(h,n,a);throw new k("No function handler for "+r)},"callFunction"),e.parseArguments=u(function(r,n){var a=n.numArgs+n.numOptionalArgs;if(a===0)return{args:[],optArgs:[]};for(var o=[],m=[],h=0;h<a;h++){var f=n.argTypes&&n.argTypes[h],g=h<n.numOptionalArgs;(n.primitive&&f==null||n.type==="sqrt"&&h===1&&m[0]==null)&&(f="primitive");var y=this.parseGroupOfType("argument to '"+r+"'",f,g);if(g)m.push(y);else if(y!=null)o.push(y);else throw new k("Null argument, please report this as a bug")}return{args:o,optArgs:m}},"parseArguments"),e.parseGroupOfType=u(function(r,n,a){switch(n){case"color":return this.parseColorGroup(a);case"size":return this.parseSizeGroup(a);case"url":return this.parseUrlGroup(a);case"math":case"text":return this.parseArgumentGroup(a,n);case"hbox":{var o=this.parseArgumentGroup(a,"text");return o!=null?{type:"styling",mode:o.mode,body:[o],style:"text"}:null}case"raw":{var m=this.parseStringGroup("raw",a);return m!=null?{type:"raw",mode:"text",string:m.text}:null}case"primitive":{if(a)throw new k("A primitive argument cannot be optional");var h=this.parseGroup(r);if(h==null)throw new k("Expected group as "+r,this.fetch());return h}case"original":case null:case void 0:return this.parseArgumentGroup(a);default:throw new k("Unknown group type as "+r,this.fetch())}},"parseGroupOfType"),e.consumeSpaces=u(function(){for(;this.fetch().text===" ";)this.consume()},"consumeSpaces"),e.parseStringGroup=u(function(r,n){var a=this.gullet.scanArgument(n);if(a==null)return null;for(var o="",m;(m=this.fetch()).text!=="EOF";)o+=m.text,this.consume();return this.consume(),a.text=o,a},"parseStringGroup"),e.parseRegexGroup=u(function(r,n){for(var a=this.fetch(),o=a,m="",h;(h=this.fetch()).text!=="EOF"&&r.test(m+h.text);)o=h,m+=o.text,this.consume();if(m==="")throw new k("Invalid "+n+": '"+a.text+"'",a);return a.range(o,m)},"parseRegexGroup"),e.parseColorGroup=u(function(r){var n=this.parseStringGroup("color",r);if(n==null)return null;var a=/^(#[a-f0-9]{3}|#?[a-f0-9]{6}|[a-z]+)$/i.exec(n.text);if(!a)throw new k("Invalid color: '"+n.text+"'",n);var o=a[0];return/^[0-9a-f]{6}$/i.test(o)&&(o="#"+o),{type:"color-token",mode:this.mode,color:o}},"parseColorGroup"),e.parseSizeGroup=u(function(r){var n,a=!1;if(this.gullet.consumeSpaces(),!r&&this.gullet.future().text!=="{"?n=this.parseRegexGroup(/^[-+]? *(?:$|\d+|\d+\.\d*|\.\d*) *[a-z]{0,2} *$/,"size"):n=this.parseStringGroup("size",r),!n)return null;!r&&n.text.length===0&&(n.text="0pt",a=!0);var o=/([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(n.text);if(!o)throw new k("Invalid size: '"+n.text+"'",n);var m={number:+(o[1]+o[2]),unit:o[3]};if(!ar(m))throw new k("Invalid unit: '"+m.unit+"'",n);return{type:"size",mode:this.mode,value:m,isBlank:a}},"parseSizeGroup"),e.parseUrlGroup=u(function(r){this.gullet.lexer.setCatcode("%",13);var n=this.parseStringGroup("url",r);if(this.gullet.lexer.setCatcode("%",14),n==null)return null;var a=n.text.replace(/\\([#$%&~_^{}])/g,"$1");return{type:"url",mode:this.mode,url:a}},"parseUrlGroup"),e.parseArgumentGroup=u(function(r,n){var a=this.gullet.scanArgument(r);if(a==null)return null;var o=this.mode;n&&this.switchMode(n),this.gullet.beginGroup();var m=this.parseExpression(!1,"EOF");this.expect("EOF"),this.gullet.endGroup();var h={type:"ordgroup",mode:this.mode,loc:a.loc,body:m};return n&&this.switchMode(o),h},"parseArgumentGroup"),e.parseGroup=u(function(r,n){var a=this.fetch(),o=a.text,m;if(o==="{"||o==="\\begingroup"){this.consume();var h=o==="{"?"}":"\\endgroup";this.gullet.beginGroup();var f=this.parseExpression(!1,h),g=this.fetch();this.expect(h),this.gullet.endGroup(),m={type:"ordgroup",mode:this.mode,loc:z0.range(a,g),body:f,semisimple:o==="\\begingroup"||void 0}}else if(m=this.parseFunction(n,r)||this.parseSymbol(),m==null&&o[0]==="\\"&&!hn.hasOwnProperty(o)){if(this.settings.throwOnError)throw new k("Undefined control sequence: "+o,a);m=this.formatUnsupportedCmd(o),this.consume()}return m},"parseGroup"),e.formLigatures=u(function(r){for(var n=r.length-1,a=0;a<n;++a){var o=r[a],m=o.text;m==="-"&&r[a+1].text==="-"&&(a+1<n&&r[a+2].text==="-"?(r.splice(a,3,{type:"textord",mode:"text",loc:z0.range(o,r[a+2]),text:"---"}),n-=2):(r.splice(a,2,{type:"textord",mode:"text",loc:z0.range(o,r[a+1]),text:"--"}),n-=1)),(m==="'"||m==="`")&&r[a+1].text===m&&(r.splice(a,2,{type:"textord",mode:"text",loc:z0.range(o,r[a+1]),text:m+m}),n-=1)}},"formLigatures"),e.parseSymbol=u(function(){var r=this.fetch(),n=r.text;if(/^\\verb[^a-zA-Z]/.test(n)){this.consume();var a=n.slice(5),o=a.charAt(0)==="*";if(o&&(a=a.slice(1)),a.length<2||a.charAt(0)!==a.slice(-1))throw new k(`\\verb assertion failed --
                    please report what input caused this bug`);return a=a.slice(1,-1),{type:"verb",mode:"text",body:a,star:o}}fn.hasOwnProperty(n[0])&&!_[this.mode][n[0]]&&(this.settings.strict&&this.mode==="math"&&this.settings.reportNonstrict("unicodeTextInMathMode",'Accented Unicode text character "'+n[0]+'" used in math mode',r),n=fn[n[0]]+n.substr(1));var m=si.exec(n);m&&(n=n.substring(0,m.index),n==="i"?n="ı":n==="j"&&(n="ȷ"));var h;if(_[this.mode][n]){this.settings.strict&&this.mode==="math"&&at.indexOf(n)>=0&&this.settings.reportNonstrict("unicodeTextInMathMode",'Latin-1/Unicode text character "'+n[0]+'" used in math mode',r);var f=_[this.mode][n].group,g=z0.range(r),y;if(Kn.hasOwnProperty(f)){var w=f;y={type:"atom",mode:this.mode,family:w,loc:g,text:n}}else y={type:f,mode:this.mode,loc:g,text:n};h=y}else if(n.charCodeAt(0)>=128)this.settings.strict&&(Vt(n.charCodeAt(0))?this.mode==="math"&&this.settings.reportNonstrict("unicodeTextInMathMode",'Unicode text character "'+n[0]+'" used in math mode',r):this.settings.reportNonstrict("unknownSymbol",'Unrecognized Unicode character "'+n[0]+'"'+(" ("+n.charCodeAt(0)+")"),r)),h={type:"textord",mode:"text",loc:z0.range(r),text:n};else return null;if(this.consume(),m)for(var A=0;A<m[0].length;A++){var z=m[0][A];if(!dn[z])throw new k("Unknown accent ' "+z+"'",r);var C=dn[z][this.mode];if(!C)throw new k("Accent "+z+" unsupported in "+this.mode+" mode",r);h={type:"accent",mode:this.mode,loc:z0.range(r),label:C,isStretchy:!1,isShifty:!0,base:h}}return h},"parseSymbol"),l}();pn.endOfExpression=["}","\\endgroup","\\end","\\right","&"];var di=u(function(e,t){if(!(typeof e=="string"||e instanceof String))throw new TypeError("KaTeX can only parse string typed expression");var r=new pn(e,t);delete r.gullet.macros.current["\\df@tag"];var n=r.parse();if(delete r.gullet.macros.current["\\current@color"],delete r.gullet.macros.current["\\color"],r.gullet.macros.get("\\df@tag")){if(!t.displayMode)throw new k("\\tag works only in display equations");r.gullet.feed("\\df@tag"),n=[{type:"tag",mode:"text",body:n,tag:r.parse()}]}return n},"parseTree"),Ft=di,vn=u(function(e,t,r){t.textContent="";var n=Rt(e,r).toNode();t.appendChild(n)},"render");typeof document<"u"&&document.compatMode!=="CSS1Compat"&&(typeof console<"u"&&console.warn("Warning: KaTeX doesn't work in quirks mode. Make sure your website has a suitable doctype."),vn=u(function(){throw new k("KaTeX doesn't work in quirks mode.")},"render"));var fi=u(function(e,t){var r=Rt(e,t).toMarkup();return r},"renderToString"),pi=u(function(e,t){var r=new je(t);return Ft(e,r)},"generateParseTree"),gn=u(function(e,t,r){if(r.throwOnError||!(e instanceof k))throw e;var n=b.makeSpan(["katex-error"],[new x0(t)]);return n.setAttribute("title",e.toString()),n.setAttribute("style","color:"+r.errorColor),n},"renderError"),Rt=u(function(e,t){var r=new je(t);try{var n=Ft(e,r);return ka(n,e,r)}catch(a){return gn(a,e,r)}},"renderToDomTree"),vi=u(function(e,t){var r=new je(t);try{var n=Ft(e,r);return Aa(n,e,r)}catch(a){return gn(a,e,r)}},"renderToHTMLTree"),gi={version:"0.13.0",render:vn,renderToString:fi,ParseError:k,__parse:pi,__renderToDomTree:Rt,__renderToHTMLTree:vi,__setFontMetrics:jn,__defineSymbol:i,__defineMacro:d,__domTree:{Span:pe,Anchor:Je,SymbolNode:x0,SvgNode:P0,PathNode:K0,LineNode:Qe}},xi=gi;return W=W.default,W}()})});Sn.ParseError;var Ni=Sn.renderToString;function Ei(L,H,V){for(var W=V,P=0,k=L.length;W<H.length;){var J=H[W];if(P<=0&&H.slice(W,W+k)===L)return W;J==="\\"?W++:J==="{"?P++:J==="}"&&P--,W++}return-1}u(Ei,"findEndOfMath");function Li(L){return L.replace(/[-/\\^$*+?.()|[\]{}]/g,"\\$&")}u(Li,"escapeRegex");var qi=/^\\begin{/;function Fi(L,H){for(var V,W=[],P=new RegExp("("+H.map(function(k0){return Li(k0.left)}).join("|")+")");V=L.search(P),V!==-1;){V>0&&(W.push({type:"text",data:L.slice(0,V)}),L=L.slice(V));var k=H.findIndex(function(k0){return L.startsWith(k0.left)});if(V=Ei(H[k].right,L,H[k].left.length),V===-1)break;var J=L.slice(0,V+H[k].right.length),w0=qi.test(J)?J:L.slice(H[k].left.length,V);W.push({type:"math",data:w0,rawData:J,display:H[k].display}),L=L.slice(V+H[k].right.length)}return L!==""&&W.push({type:"text",data:L}),W}u(Fi,"splitAtDelimiters");function Ri(L,H,V,W){for(var P=Fi(L,H),k=[],J=0;J<P.length;J++)if(P[J].type==="text")k.push(P[J].data);else{var w0=P[J].data,k0=P[J].display;try{var $0=Ni(w0,{displayMode:k0,macros:W});k.push($0)}catch(j0){if(V)throw j0;k.push(P[J].data)}}return k.join("")}u(Ri,"renderLatexInTextAsHTMLString");Bi(`.___Latex___1nfc2_1 ._latex_1nfc2_1 {
  font: inherit
}
`,{});var Oi=function(L){Ci(H,L);function H(){return L!==null&&L.apply(this,arguments)||this}return u(H,"Latex"),H.prototype.render=function(){var V=this.props,W=V.children,P=V.delimiters,k=V.strict,J=V.macros,w0=Ri(W,P,k,J);return we.createElement("span",{className:"__Latex__",dangerouslySetInnerHTML:{__html:w0}})},H.defaultProps={delimiters:[{left:"$$",right:"$$",display:!0},{left:"\\(",right:"\\)",display:!1},{left:"$",right:"$",display:!1},{left:"\\[",right:"\\]",display:!0}],strict:!1},H}(we.Component);const Ii=Oi,Hi=ke.div`
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  justify-content: center;
  border-width: 1px;
  background-color: #f5f5f5;
  border-radius: 5px;
  border-style: solid;
  width: 44rem;
  margin: 0.5rem;
  @media print {
    page-break-after: always;
  }
`,Pi=ke.div`
  margin: 0.5rem;
  width: 20rem;
  height: 20rem;
  border-color: ${L=>L.checked?"#40b4f7":L.theme.primary};
  border-width: ${L=>L.checked?"2px":"0.5px"};
  border-style: solid;
  border-radius: 5px;
  background-color: ${L=>L.showAnswer?L.answer?"#C0F09D40":"#F69E9E40":"#fff"};
  display: flex;
  flex-direction: row;
`,Gi=ke.div`
  font-family: "Roboto Mono";
  padding: 0.5rem;
`,Vi=ke.div`
  font-family: "Open Sans";
  width: 100%;
  text-align: center;
  padding: 1rem;
`,Wi=ke.button`
  font-family: "Open Sans";
  margin: 1rem;
  border-width: 0.5px;
  border-style: solid;
  border-radius: 5px;
  margin: 1em;
  padding: 0.25em 1em;
  @media print {
    visibility: hidden;
    height: 0;
    margin: 0;
  }
`,Ui=u(({domain:L,style:H,substance:V,variation:W,index:P,answer:k,showAnswer:J,onSelect:w0,onDeselect:k0})=>{const[$0,j0]=we.useState(!1);return An(Pi,{checked:$0,answer:k,showAnswer:J,onClick:()=>{j0(te=>(te?k0(P):w0(P),!te))},children:[X0(Gi,{children:P+1}),X0(Mi,{name:`choice-${P}`,domain:L,style:H,substance:V,variation:W,interactive:!1,excludeWarnings:[]},`choice-${P}`)]})},"ProblemChoice");function zn(L){const[H,V]=we.useState(new Set),[W,P]=we.useState(!1),k=L.diagrams.map(({domain:J,style:w0,substance:k0,variation:$0,answer:j0},te)=>X0(Ui,{domain:J,style:w0,substance:k0,variation:$0,index:te,answer:j0,showAnswer:W,onSelect:ce=>{V(Z0=>Z0.add(ce)),P(!1)},onDeselect:ce=>V(Z0=>(Z0.delete(ce),P(!1),Z0))}));return An(Hi,{onClick:J=>J.stopPropagation(),children:[X0(Vi,{children:X0(Ii,{delimiters:[{right:"$",left:"$",display:!1}],children:L.prompt})}),k,X0(Wi,{onClick:()=>P(!0),children:"Check Answer"})]})}u(zn,"MultipleChoiceProblem$1");try{MultipleChoiceProblem.displayName="MultipleChoiceProblem",MultipleChoiceProblem.__docgenInfo={description:"",displayName:"MultipleChoiceProblem",props:{diagrams:{defaultValue:null,description:"",name:"diagrams",required:!0,type:{name:"DiagramOption[]"}},correctIndices:{defaultValue:null,description:"",name:"correctIndices",required:!0,type:{name:"number[]"}},prompt:{defaultValue:null,description:"",name:"prompt",required:!0,type:{name:"string"}}}}}catch{}const t1={parameters:{storySource:{source:`import substance from "@penrose/examples/dist/molecules/hydrogencyanide.substance";
import style from "@penrose/examples/dist/molecules/lewis.style";
import domain from "@penrose/examples/dist/molecules/molecules.domain";
import { ComponentMeta, ComponentStory } from "@storybook/react";
import { ThemeProvider } from "styled-components";
import MultipleChoiceProblem from "../MultipleChoiceProblem.js";

// const diagram = await getDiagram();

// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: "Example/Multiple Choice Problem Component",
  component: MultipleChoiceProblem,
} as ComponentMeta<typeof MultipleChoiceProblem>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof MultipleChoiceProblem> = (args) => (
  <ThemeProvider
    theme={{
      default: "#fff",
    }}
  >
    <MultipleChoiceProblem {...args} />
  </ThemeProvider>
);

export const Props = Template.bind({});
Props.args = {
  prompt: "Choose the correct Lewis structure for $\\\\mathrm{HCN}$.",
  diagrams: [
    {
      substance,
      domain,
      style,
      variation: "1",
      answer: true,
    },
    {
      substance,
      domain,
      style,
      variation: "2",
      answer: true,
    },
    {
      substance,
      domain,
      style,
      variation: "3",
      answer: false,
    },
    {
      substance,
      domain,
      style,
      variation: "4",
      answer: false,
    },
  ],
};
`,locationsMap:{props:{startLoc:{col:63,line:17},endLoc:{col:1,line:25},startBody:{col:63,line:17},endBody:{col:1,line:25}}}}},title:"Example/Multiple Choice Problem Component",component:zn},Yi=u(L=>X0(zi,{theme:{default:"#fff"},children:X0(zn,{...L})}),"Template"),Xi=Yi.bind({});Xi.args={prompt:"Choose the correct Lewis structure for $\\mathrm{HCN}$.",diagrams:[{substance:Ye,domain:$e,style:Xe,variation:"1",answer:!0},{substance:Ye,domain:$e,style:Xe,variation:"2",answer:!0},{substance:Ye,domain:$e,style:Xe,variation:"3",answer:!1},{substance:Ye,domain:$e,style:Xe,variation:"4",answer:!1}]};const r1=["Props"];export{Xi as Props,r1 as __namedExportsOrder,t1 as default};
//# sourceMappingURL=MultipleChoiceProblem.stories-97ca8541.js.map
