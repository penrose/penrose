import{j as qe}from"./jsx-runtime-1438501e.js";import{s as vt,F as $i}from"./styled-components.browser.esm-60fac4ad.js";import{r as wt}from"./index-f46741a2.js";import{S as ji,__tla as Xi}from"./Simple-6c44ae80.js";import"./hoist-non-react-statics.cjs-84dc48a6.js";import{__tla as Zi}from"./svg-5972ad76.js";let et,Cn,Bn,Ki=Promise.all([(()=>{try{return Xi}catch{}})(),(()=>{try{return Zi}catch{}})()]).then(async()=>{var G0,U0,Y0;const kt=`Hydrogen h
Carbon c
Nitrogen n
Bond b1 := SingleBond(c, h)
Bond b2 := TripleBond(c, n)
ZeroDots(h)
ZeroDots(c)
TwoDots(n)

AutoLabel All`,St=`canvas {
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
where b := SingleBond(x, y)
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
where b := DoubleBond(x, y)
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
where b := TripleBond(x, y)
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
where TwoDots(x) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }
}
forall Bond b
where TwoDots(y) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where FourDots(x) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug2 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x21
  -- }
}
forall Bond b
where FourDots(y) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where SixDots(x) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug1 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x11
  -- }

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug2 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x21
  -- }

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
  -- debug3 = Circle {
  --   r: const.dotSize / 2
  --   fillColor: #f00
  --   center: x31
  -- }
}
forall Bond b
where SixDots(y) as e; b := SingleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}


-- double bond
forall Bond b
where TwoDots(x) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where TwoDots(y) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where FourDots(x) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where FourDots(y) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where SixDots(x) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where SixDots(y) as e; b := DoubleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}


-- triple bond
forall Bond b
where TwoDots(x) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where TwoDots(y) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight
  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where FourDots(x) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where FourDots(y) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}

forall Bond b
where SixDots(x) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x31 = x.icon.center + normalize(-b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}
forall Bond b
where SixDots(y) as e; b := TripleBond(x, y)
with Atom x, y {
  -- attract all electrons to the opposite of the bond
  weight = const.bondAvoidWeight

  vec2 x11 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x12 = e.center1
  scalar d1 = norm( x11 - x12 )
  encourage weight * const.k*(d1 - const.L)*(d1 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x21 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x22 = e.center2
  scalar d2 = norm( x21 - x22 )
  encourage weight * const.k*(d2 - const.L)*(d2 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2

  vec2 x31 = y.icon.center + normalize(b.vec) * const.atomSize / 2
  vec2 x32 = e.center3
  scalar d3 = norm( x31 - x32 )
  encourage weight * const.k*(d3 - const.L)*(d3 - const.L)/2 == weight in electron -- minimize \xBD k(d-L)\xB2
}




layout = [general, symmetry, electron]

-- layout directives
forall Bond b
where b := SingleBond(x, y) 
with Atom x, y {
  scalar weight = const.symmetryWeight
  scalar k = const.symmetryDegree
  vec2 u = b.dir
  scalar theta = atan2( u[0], u[1] )
  encourage sin( MathPI() + k*theta ) * weight == -1 * weight in symmetry
}

forall Bond b
where b := DoubleBond(x, y) 
with Atom x, y {
  scalar weight = const.symmetryWeight
  scalar k = const.symmetryDegree
  vec2 u = b.icon.end - b.icon.start
  scalar theta = atan2( u[0], u[1] )
  encourage sin( MathPI() + k*theta ) * weight == -1 * weight in symmetry
}

forall Bond b
where b := TripleBond(x, y) 
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
`,zt=`-- Atoms

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

constructor SingleBond(Atom a, Atom b) -> Bond
constructor DoubleBond(Atom a, Atom b) -> Bond
constructor TripleBond(Atom a, Atom b) -> Bond

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
`;function Nn(L,O){O===void 0&&(O={});var P=O.insertAt;if(!(!L||typeof document>"u")){var F=document.head||document.getElementsByTagName("head")[0],V=document.createElement("style");V.type="text/css",P==="top"&&F.firstChild?F.insertBefore(V,F.firstChild):F.appendChild(V),V.styleSheet?V.styleSheet.cssText=L:V.appendChild(document.createTextNode(L))}}var $t=function(L,O){return $t=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(P,F){P.__proto__=F}||function(P,F){for(var V in F)F.hasOwnProperty(V)&&(P[V]=F[V])},$t(L,O)};function qn(L,O){$t(L,O);function P(){this.constructor=L}L.prototype=O===null?Object.create(O):(P.prototype=O.prototype,new P)}var Ln=typeof window<"u"?window:typeof global<"u"?global:typeof self<"u"?self:{};function Rn(L,O){return O={exports:{}},L(O,O.exports),O.exports}var V0=Rn(function(L,O){(function(P,F){L.exports=F()})(typeof self<"u"?self:Ln,function(){return function(){var P={};(function(){P.d=function(e,t){for(var r in t)P.o(t,r)&&!P.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:t[r]})}})(),function(){P.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)}}();var F={};P.d(F,{default:function(){return Vi}});var V=function e(t,r){this.position=void 0;var n="KaTeX parse error: "+t,o,a=r&&r.loc;if(a&&a.start<=a.end){var l=a.lexer.input;o=a.start;var m=a.end;o===l.length?n+=" at end of input: ":n+=" at position "+(o+1)+": ";var u=l.slice(o,m).replace(/[^]/g,"$&\u0332"),f;o>15?f="\u2026"+l.slice(o-15,o):f=l.slice(0,o);var x;m+15<l.length?x=l.slice(m,m+15)+"\u2026":x=l.slice(m),n+=f+u+x}var y=new Error(n);return y.name="ParseError",y.__proto__=e.prototype,y.position=o,y};V.prototype.__proto__=Error.prototype;var v=V,X=function(e,t){return e.indexOf(t)!==-1},ye=function(e,t){return e===void 0?t:e},ve=/([A-Z])/g,Ue=function(e){return e.replace(ve,"-$1").toLowerCase()},Ye={"&":"&amp;",">":"&gt;","<":"&lt;",'"':"&quot;","'":"&#x27;"},$e=/[&><"']/g;function mt(e){return String(e).replace($e,function(t){return Ye[t]})}var je=function e(t){return t.type==="ordgroup"||t.type==="color"?t.body.length===1?e(t.body[0]):t:t.type==="font"?e(t.body):t},jn=function(e){var t=je(e);return t.type==="mathord"||t.type==="textord"||t.type==="atom"},Xn=function(e){if(!e)throw new Error("Expected non-null, but got "+String(e));return e},Zn=function(e){var t=/^\s*([^\\/#]*?)(?::|&#0*58|&#x0*3a)/i.exec(e);return t!=null?t[1]:"_relative"},N={contains:X,deflt:ye,escape:mt,hyphenate:Ue,getBaseElem:je,isCharacterBox:jn,protocolFromUrl:Zn},jt=function(){function e(r){this.displayMode=void 0,this.output=void 0,this.leqno=void 0,this.fleqn=void 0,this.throwOnError=void 0,this.errorColor=void 0,this.macros=void 0,this.minRuleThickness=void 0,this.colorIsTextColor=void 0,this.strict=void 0,this.trust=void 0,this.maxSize=void 0,this.maxExpand=void 0,this.globalGroup=void 0,r=r||{},this.displayMode=N.deflt(r.displayMode,!1),this.output=N.deflt(r.output,"htmlAndMathml"),this.leqno=N.deflt(r.leqno,!1),this.fleqn=N.deflt(r.fleqn,!1),this.throwOnError=N.deflt(r.throwOnError,!0),this.errorColor=N.deflt(r.errorColor,"#cc0000"),this.macros=r.macros||{},this.minRuleThickness=Math.max(0,N.deflt(r.minRuleThickness,0)),this.colorIsTextColor=N.deflt(r.colorIsTextColor,!1),this.strict=N.deflt(r.strict,"warn"),this.trust=N.deflt(r.trust,!1),this.maxSize=Math.max(0,N.deflt(r.maxSize,1/0)),this.maxExpand=Math.max(0,N.deflt(r.maxExpand,1e3)),this.globalGroup=N.deflt(r.globalGroup,!1)}var t=e.prototype;return t.reportNonstrict=function(r,n,o){var a=this.strict;if(typeof a=="function"&&(a=a(r,n,o)),!(!a||a==="ignore")){if(a===!0||a==="error")throw new v("LaTeX-incompatible input and strict mode is set to 'error': "+(n+" ["+r+"]"),o);a==="warn"?typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to 'warn': "+(n+" ["+r+"]")):typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to "+("unrecognized '"+a+"': "+n+" ["+r+"]"))}},t.useStrictBehavior=function(r,n,o){var a=this.strict;if(typeof a=="function")try{a=a(r,n,o)}catch{a="error"}return!a||a==="ignore"?!1:a===!0||a==="error"?!0:a==="warn"?(typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to 'warn': "+(n+" ["+r+"]")),!1):(typeof console<"u"&&console.warn("LaTeX-incompatible input and strict mode is set to "+("unrecognized '"+a+"': "+n+" ["+r+"]")),!1)},t.isTrusted=function(r){r.url&&!r.protocol&&(r.protocol=N.protocolFromUrl(r.url));var n=typeof this.trust=="function"?this.trust(r):this.trust;return!!n},e}(),Ee=function(){function e(r,n,o){this.id=void 0,this.size=void 0,this.cramped=void 0,this.id=r,this.size=n,this.cramped=o}var t=e.prototype;return t.sup=function(){return ze[Kn[this.id]]},t.sub=function(){return ze[Qn[this.id]]},t.fracNum=function(){return ze[Jn[this.id]]},t.fracDen=function(){return ze[eo[this.id]]},t.cramp=function(){return ze[to[this.id]]},t.text=function(){return ze[ro[this.id]]},t.isTight=function(){return this.size>=2},e}(),Xt=0,At=1,tt=2,Le=3,ht=4,fe=5,rt=6,ae=7,ze=[new Ee(Xt,0,!1),new Ee(At,0,!0),new Ee(tt,1,!1),new Ee(Le,1,!0),new Ee(ht,2,!1),new Ee(fe,2,!0),new Ee(rt,3,!1),new Ee(ae,3,!0)],Kn=[ht,fe,ht,fe,rt,ae,rt,ae],Qn=[fe,fe,fe,fe,ae,ae,ae,ae],Jn=[tt,Le,ht,fe,rt,ae,rt,ae],eo=[Le,Le,fe,fe,ae,ae,ae,ae],to=[At,At,Le,Le,fe,fe,ae,ae],ro=[Xt,At,tt,Le,tt,Le,tt,Le],D={DISPLAY:ze[Xt],TEXT:ze[tt],SCRIPT:ze[ht],SCRIPTSCRIPT:ze[rt]},Zt=[{name:"latin",blocks:[[256,591],[768,879]]},{name:"cyrillic",blocks:[[1024,1279]]},{name:"armenian",blocks:[[1328,1423]]},{name:"brahmic",blocks:[[2304,4255]]},{name:"georgian",blocks:[[4256,4351]]},{name:"cjk",blocks:[[12288,12543],[19968,40879],[65280,65376]]},{name:"hangul",blocks:[[44032,55215]]}];function no(e){for(var t=0;t<Zt.length;t++)for(var r=Zt[t],n=0;n<r.blocks.length;n++){var o=r.blocks[n];if(e>=o[0]&&e<=o[1])return r.name}return null}var ut=[];Zt.forEach(function(e){return e.blocks.forEach(function(t){return ut.push.apply(ut,t)})});function $0(e){for(var t=0;t<ut.length;t+=2)if(e>=ut[t]&&e<=ut[t+1])return!0;return!1}var nt=80,oo=function(e,t){return"M95,"+(622+e+t)+`
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
M`+(834+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},io=function(e,t){return"M263,"+(601+e+t)+`c0.7,0,18,39.7,52,119
c34,79.3,68.167,158.7,102.5,238c34.3,79.3,51.8,119.3,52.5,120
c340,-704.7,510.7,-1060.3,512,-1067
l`+e/2.084+" -"+e+`
c4.7,-7.3,11,-11,19,-11
H40000v`+(40+e)+`H1012.3
s-271.3,567,-271.3,567c-38.7,80.7,-84,175,-136,283c-52,108,-89.167,185.3,-111.5,232
c-22.3,46.7,-33.8,70.3,-34.5,71c-4.7,4.7,-12.3,7,-23,7s-12,-1,-12,-1
s-109,-253,-109,-253c-72.7,-168,-109.3,-252,-110,-252c-10.7,8,-22,16.7,-34,26
c-22,17.3,-33.3,26,-34,26s-26,-26,-26,-26s76,-59,76,-59s76,-60,76,-60z
M`+(1001+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},ao=function(e,t){return"M983 "+(10+e+t)+`
l`+e/3.13+" -"+e+`
c4,-6.7,10,-10,18,-10 H400000v`+(40+e)+`
H1013.1s-83.4,268,-264.1,840c-180.7,572,-277,876.3,-289,913c-4.7,4.7,-12.7,7,-24,7
s-12,0,-12,0c-1.3,-3.3,-3.7,-11.7,-7,-25c-35.3,-125.3,-106.7,-373.3,-214,-744
c-10,12,-21,25,-33,39s-32,39,-32,39c-6,-5.3,-15,-14,-27,-26s25,-30,25,-30
c26.7,-32.7,52,-63,76,-91s52,-60,52,-60s208,722,208,722
c56,-175.3,126.3,-397.3,211,-666c84.7,-268.7,153.8,-488.2,207.5,-658.5
c53.7,-170.3,84.5,-266.8,92.5,-289.5z
M`+(1001+e)+" "+t+"h400000v"+(40+e)+"h-400000z"},so=function(e,t){return"M424,"+(2398+e+t)+`
c-1.3,-0.7,-38.5,-172,-111.5,-514c-73,-342,-109.8,-513.3,-110.5,-514
c0,-2,-10.7,14.3,-32,49c-4.7,7.3,-9.8,15.7,-15.5,25c-5.7,9.3,-9.8,16,-12.5,20
s-5,7,-5,7c-4,-3.3,-8.3,-7.7,-13,-13s-13,-13,-13,-13s76,-122,76,-122s77,-121,77,-121
s209,968,209,968c0,-2,84.7,-361.7,254,-1079c169.3,-717.3,254.7,-1077.7,256,-1081
l`+e/4.223+" -"+e+`c4,-6.7,10,-10,18,-10 H400000
v`+(40+e)+`H1014.6
s-87.3,378.7,-272.6,1166c-185.3,787.3,-279.3,1182.3,-282,1185
c-2,6,-10,9,-24,9
c-8,0,-12,-0.7,-12,-2z M`+(1001+e)+" "+t+`
h400000v`+(40+e)+"h-400000z"},lo=function(e,t){return"M473,"+(2713+e+t)+`
c339.3,-1799.3,509.3,-2700,510,-2702 l`+e/5.298+" -"+e+`
c3.3,-7.3,9.3,-11,18,-11 H400000v`+(40+e)+`H1017.7
s-90.5,478,-276.2,1466c-185.7,988,-279.5,1483,-281.5,1485c-2,6,-10,9,-24,9
c-8,0,-12,-0.7,-12,-2c0,-1.3,-5.3,-32,-16,-92c-50.7,-293.3,-119.7,-693.3,-207,-1200
c0,-1.3,-5.3,8.7,-16,30c-10.7,21.3,-21.3,42.7,-32,64s-16,33,-16,33s-26,-26,-26,-26
s76,-153,76,-153s77,-151,77,-151c0.7,0.7,35.7,202,105,604c67.3,400.7,102,602.7,104,
606zM`+(1001+e)+" "+t+"h400000v"+(40+e)+"H1017.7z"},co=function(e){var t=e/2;return"M400000 "+e+" H0 L"+t+" 0 l65 45 L145 "+(e-80)+" H400000z"},mo=function(e,t,r){var n=r-54-t-e;return"M702 "+(e+t)+"H400000"+(40+e)+`
H742v`+n+`l-4 4-4 4c-.667.7 -2 1.5-4 2.5s-4.167 1.833-6.5 2.5-5.5 1-9.5 1
h-12l-28-84c-16.667-52-96.667 -294.333-240-727l-212 -643 -85 170
c-4-3.333-8.333-7.667-13 -13l-13-13l77-155 77-156c66 199.333 139 419.667
219 661 l218 661zM702 `+t+"H400000v"+(40+e)+"H742z"},ho=function(e,t,r){t=1e3*t;var n="";switch(e){case"sqrtMain":n=oo(t,nt);break;case"sqrtSize1":n=io(t,nt);break;case"sqrtSize2":n=ao(t,nt);break;case"sqrtSize3":n=so(t,nt);break;case"sqrtSize4":n=lo(t,nt);break;case"sqrtTall":n=mo(t,nt,r)}return n},uo=function(e,t){switch(e){case"\u239C":return"M291 0 H417 V"+t+" H291z M291 0 H417 V"+t+" H291z";case"\u2223":return"M145 0 H188 V"+t+" H145z M145 0 H188 V"+t+" H145z";case"\u2225":return"M145 0 H188 V"+t+" H145z M145 0 H188 V"+t+" H145z"+("M367 0 H410 V"+t+" H367z M367 0 H410 V"+t+" H367z");case"\u239F":return"M457 0 H583 V"+t+" H457z M457 0 H583 V"+t+" H457z";case"\u23A2":return"M319 0 H403 V"+t+" H319z M319 0 H403 V"+t+" H319z";case"\u23A5":return"M263 0 H347 V"+t+" H263z M263 0 H347 V"+t+" H263z";case"\u23AA":return"M384 0 H504 V"+t+" H384z M384 0 H504 V"+t+" H384z";case"\u23D0":return"M312 0 H355 V"+t+" H312z M312 0 H355 V"+t+" H312z";case"\u2016":return"M257 0 H300 V"+t+" H257z M257 0 H300 V"+t+" H257z"+("M478 0 H521 V"+t+" H478z M478 0 H521 V"+t+" H478z");default:return""}},j0={doubleleftarrow:`M262 157
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
M500 241 v40 H399408 v-40z M500 435 v40 H400000 v-40z`},dt=function(){function e(r){this.children=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,this.children=r,this.classes=[],this.height=0,this.depth=0,this.maxFontSize=0,this.style={}}var t=e.prototype;return t.hasClass=function(r){return N.contains(this.classes,r)},t.toNode=function(){for(var r=document.createDocumentFragment(),n=0;n<this.children.length;n++)r.appendChild(this.children[n].toNode());return r},t.toMarkup=function(){for(var r="",n=0;n<this.children.length;n++)r+=this.children[n].toMarkup();return r},t.toText=function(){var r=function(n){return n.toText()};return this.children.map(r).join("")},e}(),Pe=function(e){return e.filter(function(t){return t}).join(" ")},X0=function(e,t,r){if(this.classes=e||[],this.attributes={},this.height=0,this.depth=0,this.maxFontSize=0,this.style=r||{},t){t.style.isTight()&&this.classes.push("mtight");var n=t.getColor();n&&(this.style.color=n)}},Z0=function(e){var t=document.createElement(e);t.className=Pe(this.classes);for(var r in this.style)this.style.hasOwnProperty(r)&&(t.style[r]=this.style[r]);for(var n in this.attributes)this.attributes.hasOwnProperty(n)&&t.setAttribute(n,this.attributes[n]);for(var o=0;o<this.children.length;o++)t.appendChild(this.children[o].toNode());return t},K0=function(e){var t="<"+e;this.classes.length&&(t+=' class="'+N.escape(Pe(this.classes))+'"');var r="";for(var n in this.style)this.style.hasOwnProperty(n)&&(r+=N.hyphenate(n)+":"+this.style[n]+";");r&&(t+=' style="'+N.escape(r)+'"');for(var o in this.attributes)this.attributes.hasOwnProperty(o)&&(t+=" "+o+'="'+N.escape(this.attributes[o])+'"');t+=">";for(var a=0;a<this.children.length;a++)t+=this.children[a].toMarkup();return t+="</"+e+">",t},pt=function(){function e(r,n,o,a){this.children=void 0,this.attributes=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.width=void 0,this.maxFontSize=void 0,this.style=void 0,X0.call(this,r,o,a),this.children=n||[]}var t=e.prototype;return t.setAttribute=function(r,n){this.attributes[r]=n},t.hasClass=function(r){return N.contains(this.classes,r)},t.toNode=function(){return Z0.call(this,"span")},t.toMarkup=function(){return K0.call(this,"span")},e}(),Kt=function(){function e(r,n,o,a){this.children=void 0,this.attributes=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,X0.call(this,n,a),this.children=o||[],this.setAttribute("href",r)}var t=e.prototype;return t.setAttribute=function(r,n){this.attributes[r]=n},t.hasClass=function(r){return N.contains(this.classes,r)},t.toNode=function(){return Z0.call(this,"a")},t.toMarkup=function(){return K0.call(this,"a")},e}(),po=function(){function e(r,n,o){this.src=void 0,this.alt=void 0,this.classes=void 0,this.height=void 0,this.depth=void 0,this.maxFontSize=void 0,this.style=void 0,this.alt=n,this.src=r,this.classes=["mord"],this.style=o}var t=e.prototype;return t.hasClass=function(r){return N.contains(this.classes,r)},t.toNode=function(){var r=document.createElement("img");r.src=this.src,r.alt=this.alt,r.className="mord";for(var n in this.style)this.style.hasOwnProperty(n)&&(r.style[n]=this.style[n]);return r},t.toMarkup=function(){var r="<img  src='"+this.src+" 'alt='"+this.alt+"' ",n="";for(var o in this.style)this.style.hasOwnProperty(o)&&(n+=N.hyphenate(o)+":"+this.style[o]+";");return n&&(r+=' style="'+N.escape(n)+'"'),r+="'/>",r},e}(),fo={\u00EE:"\u0131\u0302",\u00EF:"\u0131\u0308",\u00ED:"\u0131\u0301",\u00EC:"\u0131\u0300"},ge=function(){function e(r,n,o,a,l,m,u,f){this.text=void 0,this.height=void 0,this.depth=void 0,this.italic=void 0,this.skew=void 0,this.width=void 0,this.maxFontSize=void 0,this.classes=void 0,this.style=void 0,this.text=r,this.height=n||0,this.depth=o||0,this.italic=a||0,this.skew=l||0,this.width=m||0,this.classes=u||[],this.style=f||{},this.maxFontSize=0;var x=no(this.text.charCodeAt(0));x&&this.classes.push(x+"_fallback"),/[îïíì]/.test(this.text)&&(this.text=fo[this.text])}var t=e.prototype;return t.hasClass=function(r){return N.contains(this.classes,r)},t.toNode=function(){var r=document.createTextNode(this.text),n=null;this.italic>0&&(n=document.createElement("span"),n.style.marginRight=this.italic+"em"),this.classes.length>0&&(n=n||document.createElement("span"),n.className=Pe(this.classes));for(var o in this.style)this.style.hasOwnProperty(o)&&(n=n||document.createElement("span"),n.style[o]=this.style[o]);return n?(n.appendChild(r),n):r},t.toMarkup=function(){var r=!1,n="<span";this.classes.length&&(r=!0,n+=' class="',n+=N.escape(Pe(this.classes)),n+='"');var o="";this.italic>0&&(o+="margin-right:"+this.italic+"em;");for(var a in this.style)this.style.hasOwnProperty(a)&&(o+=N.hyphenate(a)+":"+this.style[a]+";");o&&(r=!0,n+=' style="'+N.escape(o)+'"');var l=N.escape(this.text);return r?(n+=">",n+=l,n+="</span>",n):l},e}(),Oe=function(){function e(r,n){this.children=void 0,this.attributes=void 0,this.children=r||[],this.attributes=n||{}}var t=e.prototype;return t.toNode=function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"svg");for(var o in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,o)&&n.setAttribute(o,this.attributes[o]);for(var a=0;a<this.children.length;a++)n.appendChild(this.children[a].toNode());return n},t.toMarkup=function(){var r="<svg";for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+"='"+this.attributes[n]+"'");r+=">";for(var o=0;o<this.children.length;o++)r+=this.children[o].toMarkup();return r+="</svg>",r},e}(),Xe=function(){function e(r,n){this.pathName=void 0,this.alternate=void 0,this.pathName=r,this.alternate=n}var t=e.prototype;return t.toNode=function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"path");return this.alternate?n.setAttribute("d",this.alternate):n.setAttribute("d",j0[this.pathName]),n},t.toMarkup=function(){return this.alternate?"<path d='"+this.alternate+"'/>":"<path d='"+j0[this.pathName]+"'/>"},e}(),Qt=function(){function e(r){this.attributes=void 0,this.attributes=r||{}}var t=e.prototype;return t.toNode=function(){var r="http://www.w3.org/2000/svg",n=document.createElementNS(r,"line");for(var o in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,o)&&n.setAttribute(o,this.attributes[o]);return n},t.toMarkup=function(){var r="<line";for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+"='"+this.attributes[n]+"'");return r+="/>",r},e}();function Q0(e){if(e instanceof ge)return e;throw new Error("Expected symbolNode but got "+String(e)+".")}function go(e){if(e instanceof pt)return e;throw new Error("Expected span<HtmlDomNode> but got "+String(e)+".")}var Ae={"AMS-Regular":{32:[0,0,0,0,.25],65:[0,.68889,0,0,.72222],66:[0,.68889,0,0,.66667],67:[0,.68889,0,0,.72222],68:[0,.68889,0,0,.72222],69:[0,.68889,0,0,.66667],70:[0,.68889,0,0,.61111],71:[0,.68889,0,0,.77778],72:[0,.68889,0,0,.77778],73:[0,.68889,0,0,.38889],74:[.16667,.68889,0,0,.5],75:[0,.68889,0,0,.77778],76:[0,.68889,0,0,.66667],77:[0,.68889,0,0,.94445],78:[0,.68889,0,0,.72222],79:[.16667,.68889,0,0,.77778],80:[0,.68889,0,0,.61111],81:[.16667,.68889,0,0,.77778],82:[0,.68889,0,0,.72222],83:[0,.68889,0,0,.55556],84:[0,.68889,0,0,.66667],85:[0,.68889,0,0,.72222],86:[0,.68889,0,0,.72222],87:[0,.68889,0,0,1],88:[0,.68889,0,0,.72222],89:[0,.68889,0,0,.72222],90:[0,.68889,0,0,.66667],107:[0,.68889,0,0,.55556],160:[0,0,0,0,.25],165:[0,.675,.025,0,.75],174:[.15559,.69224,0,0,.94666],240:[0,.68889,0,0,.55556],295:[0,.68889,0,0,.54028],710:[0,.825,0,0,2.33334],732:[0,.9,0,0,2.33334],770:[0,.825,0,0,2.33334],771:[0,.9,0,0,2.33334],989:[.08167,.58167,0,0,.77778],1008:[0,.43056,.04028,0,.66667],8245:[0,.54986,0,0,.275],8463:[0,.68889,0,0,.54028],8487:[0,.68889,0,0,.72222],8498:[0,.68889,0,0,.55556],8502:[0,.68889,0,0,.66667],8503:[0,.68889,0,0,.44445],8504:[0,.68889,0,0,.66667],8513:[0,.68889,0,0,.63889],8592:[-.03598,.46402,0,0,.5],8594:[-.03598,.46402,0,0,.5],8602:[-.13313,.36687,0,0,1],8603:[-.13313,.36687,0,0,1],8606:[.01354,.52239,0,0,1],8608:[.01354,.52239,0,0,1],8610:[.01354,.52239,0,0,1.11111],8611:[.01354,.52239,0,0,1.11111],8619:[0,.54986,0,0,1],8620:[0,.54986,0,0,1],8621:[-.13313,.37788,0,0,1.38889],8622:[-.13313,.36687,0,0,1],8624:[0,.69224,0,0,.5],8625:[0,.69224,0,0,.5],8630:[0,.43056,0,0,1],8631:[0,.43056,0,0,1],8634:[.08198,.58198,0,0,.77778],8635:[.08198,.58198,0,0,.77778],8638:[.19444,.69224,0,0,.41667],8639:[.19444,.69224,0,0,.41667],8642:[.19444,.69224,0,0,.41667],8643:[.19444,.69224,0,0,.41667],8644:[.1808,.675,0,0,1],8646:[.1808,.675,0,0,1],8647:[.1808,.675,0,0,1],8648:[.19444,.69224,0,0,.83334],8649:[.1808,.675,0,0,1],8650:[.19444,.69224,0,0,.83334],8651:[.01354,.52239,0,0,1],8652:[.01354,.52239,0,0,1],8653:[-.13313,.36687,0,0,1],8654:[-.13313,.36687,0,0,1],8655:[-.13313,.36687,0,0,1],8666:[.13667,.63667,0,0,1],8667:[.13667,.63667,0,0,1],8669:[-.13313,.37788,0,0,1],8672:[-.064,.437,0,0,1.334],8674:[-.064,.437,0,0,1.334],8705:[0,.825,0,0,.5],8708:[0,.68889,0,0,.55556],8709:[.08167,.58167,0,0,.77778],8717:[0,.43056,0,0,.42917],8722:[-.03598,.46402,0,0,.5],8724:[.08198,.69224,0,0,.77778],8726:[.08167,.58167,0,0,.77778],8733:[0,.69224,0,0,.77778],8736:[0,.69224,0,0,.72222],8737:[0,.69224,0,0,.72222],8738:[.03517,.52239,0,0,.72222],8739:[.08167,.58167,0,0,.22222],8740:[.25142,.74111,0,0,.27778],8741:[.08167,.58167,0,0,.38889],8742:[.25142,.74111,0,0,.5],8756:[0,.69224,0,0,.66667],8757:[0,.69224,0,0,.66667],8764:[-.13313,.36687,0,0,.77778],8765:[-.13313,.37788,0,0,.77778],8769:[-.13313,.36687,0,0,.77778],8770:[-.03625,.46375,0,0,.77778],8774:[.30274,.79383,0,0,.77778],8776:[-.01688,.48312,0,0,.77778],8778:[.08167,.58167,0,0,.77778],8782:[.06062,.54986,0,0,.77778],8783:[.06062,.54986,0,0,.77778],8785:[.08198,.58198,0,0,.77778],8786:[.08198,.58198,0,0,.77778],8787:[.08198,.58198,0,0,.77778],8790:[0,.69224,0,0,.77778],8791:[.22958,.72958,0,0,.77778],8796:[.08198,.91667,0,0,.77778],8806:[.25583,.75583,0,0,.77778],8807:[.25583,.75583,0,0,.77778],8808:[.25142,.75726,0,0,.77778],8809:[.25142,.75726,0,0,.77778],8812:[.25583,.75583,0,0,.5],8814:[.20576,.70576,0,0,.77778],8815:[.20576,.70576,0,0,.77778],8816:[.30274,.79383,0,0,.77778],8817:[.30274,.79383,0,0,.77778],8818:[.22958,.72958,0,0,.77778],8819:[.22958,.72958,0,0,.77778],8822:[.1808,.675,0,0,.77778],8823:[.1808,.675,0,0,.77778],8828:[.13667,.63667,0,0,.77778],8829:[.13667,.63667,0,0,.77778],8830:[.22958,.72958,0,0,.77778],8831:[.22958,.72958,0,0,.77778],8832:[.20576,.70576,0,0,.77778],8833:[.20576,.70576,0,0,.77778],8840:[.30274,.79383,0,0,.77778],8841:[.30274,.79383,0,0,.77778],8842:[.13597,.63597,0,0,.77778],8843:[.13597,.63597,0,0,.77778],8847:[.03517,.54986,0,0,.77778],8848:[.03517,.54986,0,0,.77778],8858:[.08198,.58198,0,0,.77778],8859:[.08198,.58198,0,0,.77778],8861:[.08198,.58198,0,0,.77778],8862:[0,.675,0,0,.77778],8863:[0,.675,0,0,.77778],8864:[0,.675,0,0,.77778],8865:[0,.675,0,0,.77778],8872:[0,.69224,0,0,.61111],8873:[0,.69224,0,0,.72222],8874:[0,.69224,0,0,.88889],8876:[0,.68889,0,0,.61111],8877:[0,.68889,0,0,.61111],8878:[0,.68889,0,0,.72222],8879:[0,.68889,0,0,.72222],8882:[.03517,.54986,0,0,.77778],8883:[.03517,.54986,0,0,.77778],8884:[.13667,.63667,0,0,.77778],8885:[.13667,.63667,0,0,.77778],8888:[0,.54986,0,0,1.11111],8890:[.19444,.43056,0,0,.55556],8891:[.19444,.69224,0,0,.61111],8892:[.19444,.69224,0,0,.61111],8901:[0,.54986,0,0,.27778],8903:[.08167,.58167,0,0,.77778],8905:[.08167,.58167,0,0,.77778],8906:[.08167,.58167,0,0,.77778],8907:[0,.69224,0,0,.77778],8908:[0,.69224,0,0,.77778],8909:[-.03598,.46402,0,0,.77778],8910:[0,.54986,0,0,.76042],8911:[0,.54986,0,0,.76042],8912:[.03517,.54986,0,0,.77778],8913:[.03517,.54986,0,0,.77778],8914:[0,.54986,0,0,.66667],8915:[0,.54986,0,0,.66667],8916:[0,.69224,0,0,.66667],8918:[.0391,.5391,0,0,.77778],8919:[.0391,.5391,0,0,.77778],8920:[.03517,.54986,0,0,1.33334],8921:[.03517,.54986,0,0,1.33334],8922:[.38569,.88569,0,0,.77778],8923:[.38569,.88569,0,0,.77778],8926:[.13667,.63667,0,0,.77778],8927:[.13667,.63667,0,0,.77778],8928:[.30274,.79383,0,0,.77778],8929:[.30274,.79383,0,0,.77778],8934:[.23222,.74111,0,0,.77778],8935:[.23222,.74111,0,0,.77778],8936:[.23222,.74111,0,0,.77778],8937:[.23222,.74111,0,0,.77778],8938:[.20576,.70576,0,0,.77778],8939:[.20576,.70576,0,0,.77778],8940:[.30274,.79383,0,0,.77778],8941:[.30274,.79383,0,0,.77778],8994:[.19444,.69224,0,0,.77778],8995:[.19444,.69224,0,0,.77778],9416:[.15559,.69224,0,0,.90222],9484:[0,.69224,0,0,.5],9488:[0,.69224,0,0,.5],9492:[0,.37788,0,0,.5],9496:[0,.37788,0,0,.5],9585:[.19444,.68889,0,0,.88889],9586:[.19444,.74111,0,0,.88889],9632:[0,.675,0,0,.77778],9633:[0,.675,0,0,.77778],9650:[0,.54986,0,0,.72222],9651:[0,.54986,0,0,.72222],9654:[.03517,.54986,0,0,.77778],9660:[0,.54986,0,0,.72222],9661:[0,.54986,0,0,.72222],9664:[.03517,.54986,0,0,.77778],9674:[.11111,.69224,0,0,.66667],9733:[.19444,.69224,0,0,.94445],10003:[0,.69224,0,0,.83334],10016:[0,.69224,0,0,.83334],10731:[.11111,.69224,0,0,.66667],10846:[.19444,.75583,0,0,.61111],10877:[.13667,.63667,0,0,.77778],10878:[.13667,.63667,0,0,.77778],10885:[.25583,.75583,0,0,.77778],10886:[.25583,.75583,0,0,.77778],10887:[.13597,.63597,0,0,.77778],10888:[.13597,.63597,0,0,.77778],10889:[.26167,.75726,0,0,.77778],10890:[.26167,.75726,0,0,.77778],10891:[.48256,.98256,0,0,.77778],10892:[.48256,.98256,0,0,.77778],10901:[.13667,.63667,0,0,.77778],10902:[.13667,.63667,0,0,.77778],10933:[.25142,.75726,0,0,.77778],10934:[.25142,.75726,0,0,.77778],10935:[.26167,.75726,0,0,.77778],10936:[.26167,.75726,0,0,.77778],10937:[.26167,.75726,0,0,.77778],10938:[.26167,.75726,0,0,.77778],10949:[.25583,.75583,0,0,.77778],10950:[.25583,.75583,0,0,.77778],10955:[.28481,.79383,0,0,.77778],10956:[.28481,.79383,0,0,.77778],57350:[.08167,.58167,0,0,.22222],57351:[.08167,.58167,0,0,.38889],57352:[.08167,.58167,0,0,.77778],57353:[0,.43056,.04028,0,.66667],57356:[.25142,.75726,0,0,.77778],57357:[.25142,.75726,0,0,.77778],57358:[.41951,.91951,0,0,.77778],57359:[.30274,.79383,0,0,.77778],57360:[.30274,.79383,0,0,.77778],57361:[.41951,.91951,0,0,.77778],57366:[.25142,.75726,0,0,.77778],57367:[.25142,.75726,0,0,.77778],57368:[.25142,.75726,0,0,.77778],57369:[.25142,.75726,0,0,.77778],57370:[.13597,.63597,0,0,.77778],57371:[.13597,.63597,0,0,.77778]},"Caligraphic-Regular":{32:[0,0,0,0,.25],65:[0,.68333,0,.19445,.79847],66:[0,.68333,.03041,.13889,.65681],67:[0,.68333,.05834,.13889,.52653],68:[0,.68333,.02778,.08334,.77139],69:[0,.68333,.08944,.11111,.52778],70:[0,.68333,.09931,.11111,.71875],71:[.09722,.68333,.0593,.11111,.59487],72:[0,.68333,.00965,.11111,.84452],73:[0,.68333,.07382,0,.54452],74:[.09722,.68333,.18472,.16667,.67778],75:[0,.68333,.01445,.05556,.76195],76:[0,.68333,0,.13889,.68972],77:[0,.68333,0,.13889,1.2009],78:[0,.68333,.14736,.08334,.82049],79:[0,.68333,.02778,.11111,.79611],80:[0,.68333,.08222,.08334,.69556],81:[.09722,.68333,0,.11111,.81667],82:[0,.68333,0,.08334,.8475],83:[0,.68333,.075,.13889,.60556],84:[0,.68333,.25417,0,.54464],85:[0,.68333,.09931,.08334,.62583],86:[0,.68333,.08222,0,.61278],87:[0,.68333,.08222,.08334,.98778],88:[0,.68333,.14643,.13889,.7133],89:[.09722,.68333,.08222,.08334,.66834],90:[0,.68333,.07944,.13889,.72473],160:[0,0,0,0,.25]},"Fraktur-Regular":{32:[0,0,0,0,.25],33:[0,.69141,0,0,.29574],34:[0,.69141,0,0,.21471],38:[0,.69141,0,0,.73786],39:[0,.69141,0,0,.21201],40:[.24982,.74947,0,0,.38865],41:[.24982,.74947,0,0,.38865],42:[0,.62119,0,0,.27764],43:[.08319,.58283,0,0,.75623],44:[0,.10803,0,0,.27764],45:[.08319,.58283,0,0,.75623],46:[0,.10803,0,0,.27764],47:[.24982,.74947,0,0,.50181],48:[0,.47534,0,0,.50181],49:[0,.47534,0,0,.50181],50:[0,.47534,0,0,.50181],51:[.18906,.47534,0,0,.50181],52:[.18906,.47534,0,0,.50181],53:[.18906,.47534,0,0,.50181],54:[0,.69141,0,0,.50181],55:[.18906,.47534,0,0,.50181],56:[0,.69141,0,0,.50181],57:[.18906,.47534,0,0,.50181],58:[0,.47534,0,0,.21606],59:[.12604,.47534,0,0,.21606],61:[-.13099,.36866,0,0,.75623],63:[0,.69141,0,0,.36245],65:[0,.69141,0,0,.7176],66:[0,.69141,0,0,.88397],67:[0,.69141,0,0,.61254],68:[0,.69141,0,0,.83158],69:[0,.69141,0,0,.66278],70:[.12604,.69141,0,0,.61119],71:[0,.69141,0,0,.78539],72:[.06302,.69141,0,0,.7203],73:[0,.69141,0,0,.55448],74:[.12604,.69141,0,0,.55231],75:[0,.69141,0,0,.66845],76:[0,.69141,0,0,.66602],77:[0,.69141,0,0,1.04953],78:[0,.69141,0,0,.83212],79:[0,.69141,0,0,.82699],80:[.18906,.69141,0,0,.82753],81:[.03781,.69141,0,0,.82699],82:[0,.69141,0,0,.82807],83:[0,.69141,0,0,.82861],84:[0,.69141,0,0,.66899],85:[0,.69141,0,0,.64576],86:[0,.69141,0,0,.83131],87:[0,.69141,0,0,1.04602],88:[0,.69141,0,0,.71922],89:[.18906,.69141,0,0,.83293],90:[.12604,.69141,0,0,.60201],91:[.24982,.74947,0,0,.27764],93:[.24982,.74947,0,0,.27764],94:[0,.69141,0,0,.49965],97:[0,.47534,0,0,.50046],98:[0,.69141,0,0,.51315],99:[0,.47534,0,0,.38946],100:[0,.62119,0,0,.49857],101:[0,.47534,0,0,.40053],102:[.18906,.69141,0,0,.32626],103:[.18906,.47534,0,0,.5037],104:[.18906,.69141,0,0,.52126],105:[0,.69141,0,0,.27899],106:[0,.69141,0,0,.28088],107:[0,.69141,0,0,.38946],108:[0,.69141,0,0,.27953],109:[0,.47534,0,0,.76676],110:[0,.47534,0,0,.52666],111:[0,.47534,0,0,.48885],112:[.18906,.52396,0,0,.50046],113:[.18906,.47534,0,0,.48912],114:[0,.47534,0,0,.38919],115:[0,.47534,0,0,.44266],116:[0,.62119,0,0,.33301],117:[0,.47534,0,0,.5172],118:[0,.52396,0,0,.5118],119:[0,.52396,0,0,.77351],120:[.18906,.47534,0,0,.38865],121:[.18906,.47534,0,0,.49884],122:[.18906,.47534,0,0,.39054],160:[0,0,0,0,.25],8216:[0,.69141,0,0,.21471],8217:[0,.69141,0,0,.21471],58112:[0,.62119,0,0,.49749],58113:[0,.62119,0,0,.4983],58114:[.18906,.69141,0,0,.33328],58115:[.18906,.69141,0,0,.32923],58116:[.18906,.47534,0,0,.50343],58117:[0,.69141,0,0,.33301],58118:[0,.62119,0,0,.33409],58119:[0,.47534,0,0,.50073]},"Main-Bold":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.35],34:[0,.69444,0,0,.60278],35:[.19444,.69444,0,0,.95833],36:[.05556,.75,0,0,.575],37:[.05556,.75,0,0,.95833],38:[0,.69444,0,0,.89444],39:[0,.69444,0,0,.31944],40:[.25,.75,0,0,.44722],41:[.25,.75,0,0,.44722],42:[0,.75,0,0,.575],43:[.13333,.63333,0,0,.89444],44:[.19444,.15556,0,0,.31944],45:[0,.44444,0,0,.38333],46:[0,.15556,0,0,.31944],47:[.25,.75,0,0,.575],48:[0,.64444,0,0,.575],49:[0,.64444,0,0,.575],50:[0,.64444,0,0,.575],51:[0,.64444,0,0,.575],52:[0,.64444,0,0,.575],53:[0,.64444,0,0,.575],54:[0,.64444,0,0,.575],55:[0,.64444,0,0,.575],56:[0,.64444,0,0,.575],57:[0,.64444,0,0,.575],58:[0,.44444,0,0,.31944],59:[.19444,.44444,0,0,.31944],60:[.08556,.58556,0,0,.89444],61:[-.10889,.39111,0,0,.89444],62:[.08556,.58556,0,0,.89444],63:[0,.69444,0,0,.54305],64:[0,.69444,0,0,.89444],65:[0,.68611,0,0,.86944],66:[0,.68611,0,0,.81805],67:[0,.68611,0,0,.83055],68:[0,.68611,0,0,.88194],69:[0,.68611,0,0,.75555],70:[0,.68611,0,0,.72361],71:[0,.68611,0,0,.90416],72:[0,.68611,0,0,.9],73:[0,.68611,0,0,.43611],74:[0,.68611,0,0,.59444],75:[0,.68611,0,0,.90138],76:[0,.68611,0,0,.69166],77:[0,.68611,0,0,1.09166],78:[0,.68611,0,0,.9],79:[0,.68611,0,0,.86388],80:[0,.68611,0,0,.78611],81:[.19444,.68611,0,0,.86388],82:[0,.68611,0,0,.8625],83:[0,.68611,0,0,.63889],84:[0,.68611,0,0,.8],85:[0,.68611,0,0,.88472],86:[0,.68611,.01597,0,.86944],87:[0,.68611,.01597,0,1.18888],88:[0,.68611,0,0,.86944],89:[0,.68611,.02875,0,.86944],90:[0,.68611,0,0,.70277],91:[.25,.75,0,0,.31944],92:[.25,.75,0,0,.575],93:[.25,.75,0,0,.31944],94:[0,.69444,0,0,.575],95:[.31,.13444,.03194,0,.575],97:[0,.44444,0,0,.55902],98:[0,.69444,0,0,.63889],99:[0,.44444,0,0,.51111],100:[0,.69444,0,0,.63889],101:[0,.44444,0,0,.52708],102:[0,.69444,.10903,0,.35139],103:[.19444,.44444,.01597,0,.575],104:[0,.69444,0,0,.63889],105:[0,.69444,0,0,.31944],106:[.19444,.69444,0,0,.35139],107:[0,.69444,0,0,.60694],108:[0,.69444,0,0,.31944],109:[0,.44444,0,0,.95833],110:[0,.44444,0,0,.63889],111:[0,.44444,0,0,.575],112:[.19444,.44444,0,0,.63889],113:[.19444,.44444,0,0,.60694],114:[0,.44444,0,0,.47361],115:[0,.44444,0,0,.45361],116:[0,.63492,0,0,.44722],117:[0,.44444,0,0,.63889],118:[0,.44444,.01597,0,.60694],119:[0,.44444,.01597,0,.83055],120:[0,.44444,0,0,.60694],121:[.19444,.44444,.01597,0,.60694],122:[0,.44444,0,0,.51111],123:[.25,.75,0,0,.575],124:[.25,.75,0,0,.31944],125:[.25,.75,0,0,.575],126:[.35,.34444,0,0,.575],160:[0,0,0,0,.25],163:[0,.69444,0,0,.86853],168:[0,.69444,0,0,.575],172:[0,.44444,0,0,.76666],176:[0,.69444,0,0,.86944],177:[.13333,.63333,0,0,.89444],184:[.17014,0,0,0,.51111],198:[0,.68611,0,0,1.04166],215:[.13333,.63333,0,0,.89444],216:[.04861,.73472,0,0,.89444],223:[0,.69444,0,0,.59722],230:[0,.44444,0,0,.83055],247:[.13333,.63333,0,0,.89444],248:[.09722,.54167,0,0,.575],305:[0,.44444,0,0,.31944],338:[0,.68611,0,0,1.16944],339:[0,.44444,0,0,.89444],567:[.19444,.44444,0,0,.35139],710:[0,.69444,0,0,.575],711:[0,.63194,0,0,.575],713:[0,.59611,0,0,.575],714:[0,.69444,0,0,.575],715:[0,.69444,0,0,.575],728:[0,.69444,0,0,.575],729:[0,.69444,0,0,.31944],730:[0,.69444,0,0,.86944],732:[0,.69444,0,0,.575],733:[0,.69444,0,0,.575],915:[0,.68611,0,0,.69166],916:[0,.68611,0,0,.95833],920:[0,.68611,0,0,.89444],923:[0,.68611,0,0,.80555],926:[0,.68611,0,0,.76666],928:[0,.68611,0,0,.9],931:[0,.68611,0,0,.83055],933:[0,.68611,0,0,.89444],934:[0,.68611,0,0,.83055],936:[0,.68611,0,0,.89444],937:[0,.68611,0,0,.83055],8211:[0,.44444,.03194,0,.575],8212:[0,.44444,.03194,0,1.14999],8216:[0,.69444,0,0,.31944],8217:[0,.69444,0,0,.31944],8220:[0,.69444,0,0,.60278],8221:[0,.69444,0,0,.60278],8224:[.19444,.69444,0,0,.51111],8225:[.19444,.69444,0,0,.51111],8242:[0,.55556,0,0,.34444],8407:[0,.72444,.15486,0,.575],8463:[0,.69444,0,0,.66759],8465:[0,.69444,0,0,.83055],8467:[0,.69444,0,0,.47361],8472:[.19444,.44444,0,0,.74027],8476:[0,.69444,0,0,.83055],8501:[0,.69444,0,0,.70277],8592:[-.10889,.39111,0,0,1.14999],8593:[.19444,.69444,0,0,.575],8594:[-.10889,.39111,0,0,1.14999],8595:[.19444,.69444,0,0,.575],8596:[-.10889,.39111,0,0,1.14999],8597:[.25,.75,0,0,.575],8598:[.19444,.69444,0,0,1.14999],8599:[.19444,.69444,0,0,1.14999],8600:[.19444,.69444,0,0,1.14999],8601:[.19444,.69444,0,0,1.14999],8636:[-.10889,.39111,0,0,1.14999],8637:[-.10889,.39111,0,0,1.14999],8640:[-.10889,.39111,0,0,1.14999],8641:[-.10889,.39111,0,0,1.14999],8656:[-.10889,.39111,0,0,1.14999],8657:[.19444,.69444,0,0,.70277],8658:[-.10889,.39111,0,0,1.14999],8659:[.19444,.69444,0,0,.70277],8660:[-.10889,.39111,0,0,1.14999],8661:[.25,.75,0,0,.70277],8704:[0,.69444,0,0,.63889],8706:[0,.69444,.06389,0,.62847],8707:[0,.69444,0,0,.63889],8709:[.05556,.75,0,0,.575],8711:[0,.68611,0,0,.95833],8712:[.08556,.58556,0,0,.76666],8715:[.08556,.58556,0,0,.76666],8722:[.13333,.63333,0,0,.89444],8723:[.13333,.63333,0,0,.89444],8725:[.25,.75,0,0,.575],8726:[.25,.75,0,0,.575],8727:[-.02778,.47222,0,0,.575],8728:[-.02639,.47361,0,0,.575],8729:[-.02639,.47361,0,0,.575],8730:[.18,.82,0,0,.95833],8733:[0,.44444,0,0,.89444],8734:[0,.44444,0,0,1.14999],8736:[0,.69224,0,0,.72222],8739:[.25,.75,0,0,.31944],8741:[.25,.75,0,0,.575],8743:[0,.55556,0,0,.76666],8744:[0,.55556,0,0,.76666],8745:[0,.55556,0,0,.76666],8746:[0,.55556,0,0,.76666],8747:[.19444,.69444,.12778,0,.56875],8764:[-.10889,.39111,0,0,.89444],8768:[.19444,.69444,0,0,.31944],8771:[.00222,.50222,0,0,.89444],8776:[.02444,.52444,0,0,.89444],8781:[.00222,.50222,0,0,.89444],8801:[.00222,.50222,0,0,.89444],8804:[.19667,.69667,0,0,.89444],8805:[.19667,.69667,0,0,.89444],8810:[.08556,.58556,0,0,1.14999],8811:[.08556,.58556,0,0,1.14999],8826:[.08556,.58556,0,0,.89444],8827:[.08556,.58556,0,0,.89444],8834:[.08556,.58556,0,0,.89444],8835:[.08556,.58556,0,0,.89444],8838:[.19667,.69667,0,0,.89444],8839:[.19667,.69667,0,0,.89444],8846:[0,.55556,0,0,.76666],8849:[.19667,.69667,0,0,.89444],8850:[.19667,.69667,0,0,.89444],8851:[0,.55556,0,0,.76666],8852:[0,.55556,0,0,.76666],8853:[.13333,.63333,0,0,.89444],8854:[.13333,.63333,0,0,.89444],8855:[.13333,.63333,0,0,.89444],8856:[.13333,.63333,0,0,.89444],8857:[.13333,.63333,0,0,.89444],8866:[0,.69444,0,0,.70277],8867:[0,.69444,0,0,.70277],8868:[0,.69444,0,0,.89444],8869:[0,.69444,0,0,.89444],8900:[-.02639,.47361,0,0,.575],8901:[-.02639,.47361,0,0,.31944],8902:[-.02778,.47222,0,0,.575],8968:[.25,.75,0,0,.51111],8969:[.25,.75,0,0,.51111],8970:[.25,.75,0,0,.51111],8971:[.25,.75,0,0,.51111],8994:[-.13889,.36111,0,0,1.14999],8995:[-.13889,.36111,0,0,1.14999],9651:[.19444,.69444,0,0,1.02222],9657:[-.02778,.47222,0,0,.575],9661:[.19444,.69444,0,0,1.02222],9667:[-.02778,.47222,0,0,.575],9711:[.19444,.69444,0,0,1.14999],9824:[.12963,.69444,0,0,.89444],9825:[.12963,.69444,0,0,.89444],9826:[.12963,.69444,0,0,.89444],9827:[.12963,.69444,0,0,.89444],9837:[0,.75,0,0,.44722],9838:[.19444,.69444,0,0,.44722],9839:[.19444,.69444,0,0,.44722],10216:[.25,.75,0,0,.44722],10217:[.25,.75,0,0,.44722],10815:[0,.68611,0,0,.9],10927:[.19667,.69667,0,0,.89444],10928:[.19667,.69667,0,0,.89444],57376:[.19444,.69444,0,0,0]},"Main-BoldItalic":{32:[0,0,0,0,.25],33:[0,.69444,.11417,0,.38611],34:[0,.69444,.07939,0,.62055],35:[.19444,.69444,.06833,0,.94444],37:[.05556,.75,.12861,0,.94444],38:[0,.69444,.08528,0,.88555],39:[0,.69444,.12945,0,.35555],40:[.25,.75,.15806,0,.47333],41:[.25,.75,.03306,0,.47333],42:[0,.75,.14333,0,.59111],43:[.10333,.60333,.03306,0,.88555],44:[.19444,.14722,0,0,.35555],45:[0,.44444,.02611,0,.41444],46:[0,.14722,0,0,.35555],47:[.25,.75,.15806,0,.59111],48:[0,.64444,.13167,0,.59111],49:[0,.64444,.13167,0,.59111],50:[0,.64444,.13167,0,.59111],51:[0,.64444,.13167,0,.59111],52:[.19444,.64444,.13167,0,.59111],53:[0,.64444,.13167,0,.59111],54:[0,.64444,.13167,0,.59111],55:[.19444,.64444,.13167,0,.59111],56:[0,.64444,.13167,0,.59111],57:[0,.64444,.13167,0,.59111],58:[0,.44444,.06695,0,.35555],59:[.19444,.44444,.06695,0,.35555],61:[-.10889,.39111,.06833,0,.88555],63:[0,.69444,.11472,0,.59111],64:[0,.69444,.09208,0,.88555],65:[0,.68611,0,0,.86555],66:[0,.68611,.0992,0,.81666],67:[0,.68611,.14208,0,.82666],68:[0,.68611,.09062,0,.87555],69:[0,.68611,.11431,0,.75666],70:[0,.68611,.12903,0,.72722],71:[0,.68611,.07347,0,.89527],72:[0,.68611,.17208,0,.8961],73:[0,.68611,.15681,0,.47166],74:[0,.68611,.145,0,.61055],75:[0,.68611,.14208,0,.89499],76:[0,.68611,0,0,.69777],77:[0,.68611,.17208,0,1.07277],78:[0,.68611,.17208,0,.8961],79:[0,.68611,.09062,0,.85499],80:[0,.68611,.0992,0,.78721],81:[.19444,.68611,.09062,0,.85499],82:[0,.68611,.02559,0,.85944],83:[0,.68611,.11264,0,.64999],84:[0,.68611,.12903,0,.7961],85:[0,.68611,.17208,0,.88083],86:[0,.68611,.18625,0,.86555],87:[0,.68611,.18625,0,1.15999],88:[0,.68611,.15681,0,.86555],89:[0,.68611,.19803,0,.86555],90:[0,.68611,.14208,0,.70888],91:[.25,.75,.1875,0,.35611],93:[.25,.75,.09972,0,.35611],94:[0,.69444,.06709,0,.59111],95:[.31,.13444,.09811,0,.59111],97:[0,.44444,.09426,0,.59111],98:[0,.69444,.07861,0,.53222],99:[0,.44444,.05222,0,.53222],100:[0,.69444,.10861,0,.59111],101:[0,.44444,.085,0,.53222],102:[.19444,.69444,.21778,0,.4],103:[.19444,.44444,.105,0,.53222],104:[0,.69444,.09426,0,.59111],105:[0,.69326,.11387,0,.35555],106:[.19444,.69326,.1672,0,.35555],107:[0,.69444,.11111,0,.53222],108:[0,.69444,.10861,0,.29666],109:[0,.44444,.09426,0,.94444],110:[0,.44444,.09426,0,.64999],111:[0,.44444,.07861,0,.59111],112:[.19444,.44444,.07861,0,.59111],113:[.19444,.44444,.105,0,.53222],114:[0,.44444,.11111,0,.50167],115:[0,.44444,.08167,0,.48694],116:[0,.63492,.09639,0,.385],117:[0,.44444,.09426,0,.62055],118:[0,.44444,.11111,0,.53222],119:[0,.44444,.11111,0,.76777],120:[0,.44444,.12583,0,.56055],121:[.19444,.44444,.105,0,.56166],122:[0,.44444,.13889,0,.49055],126:[.35,.34444,.11472,0,.59111],160:[0,0,0,0,.25],168:[0,.69444,.11473,0,.59111],176:[0,.69444,0,0,.94888],184:[.17014,0,0,0,.53222],198:[0,.68611,.11431,0,1.02277],216:[.04861,.73472,.09062,0,.88555],223:[.19444,.69444,.09736,0,.665],230:[0,.44444,.085,0,.82666],248:[.09722,.54167,.09458,0,.59111],305:[0,.44444,.09426,0,.35555],338:[0,.68611,.11431,0,1.14054],339:[0,.44444,.085,0,.82666],567:[.19444,.44444,.04611,0,.385],710:[0,.69444,.06709,0,.59111],711:[0,.63194,.08271,0,.59111],713:[0,.59444,.10444,0,.59111],714:[0,.69444,.08528,0,.59111],715:[0,.69444,0,0,.59111],728:[0,.69444,.10333,0,.59111],729:[0,.69444,.12945,0,.35555],730:[0,.69444,0,0,.94888],732:[0,.69444,.11472,0,.59111],733:[0,.69444,.11472,0,.59111],915:[0,.68611,.12903,0,.69777],916:[0,.68611,0,0,.94444],920:[0,.68611,.09062,0,.88555],923:[0,.68611,0,0,.80666],926:[0,.68611,.15092,0,.76777],928:[0,.68611,.17208,0,.8961],931:[0,.68611,.11431,0,.82666],933:[0,.68611,.10778,0,.88555],934:[0,.68611,.05632,0,.82666],936:[0,.68611,.10778,0,.88555],937:[0,.68611,.0992,0,.82666],8211:[0,.44444,.09811,0,.59111],8212:[0,.44444,.09811,0,1.18221],8216:[0,.69444,.12945,0,.35555],8217:[0,.69444,.12945,0,.35555],8220:[0,.69444,.16772,0,.62055],8221:[0,.69444,.07939,0,.62055]},"Main-Italic":{32:[0,0,0,0,.25],33:[0,.69444,.12417,0,.30667],34:[0,.69444,.06961,0,.51444],35:[.19444,.69444,.06616,0,.81777],37:[.05556,.75,.13639,0,.81777],38:[0,.69444,.09694,0,.76666],39:[0,.69444,.12417,0,.30667],40:[.25,.75,.16194,0,.40889],41:[.25,.75,.03694,0,.40889],42:[0,.75,.14917,0,.51111],43:[.05667,.56167,.03694,0,.76666],44:[.19444,.10556,0,0,.30667],45:[0,.43056,.02826,0,.35778],46:[0,.10556,0,0,.30667],47:[.25,.75,.16194,0,.51111],48:[0,.64444,.13556,0,.51111],49:[0,.64444,.13556,0,.51111],50:[0,.64444,.13556,0,.51111],51:[0,.64444,.13556,0,.51111],52:[.19444,.64444,.13556,0,.51111],53:[0,.64444,.13556,0,.51111],54:[0,.64444,.13556,0,.51111],55:[.19444,.64444,.13556,0,.51111],56:[0,.64444,.13556,0,.51111],57:[0,.64444,.13556,0,.51111],58:[0,.43056,.0582,0,.30667],59:[.19444,.43056,.0582,0,.30667],61:[-.13313,.36687,.06616,0,.76666],63:[0,.69444,.1225,0,.51111],64:[0,.69444,.09597,0,.76666],65:[0,.68333,0,0,.74333],66:[0,.68333,.10257,0,.70389],67:[0,.68333,.14528,0,.71555],68:[0,.68333,.09403,0,.755],69:[0,.68333,.12028,0,.67833],70:[0,.68333,.13305,0,.65277],71:[0,.68333,.08722,0,.77361],72:[0,.68333,.16389,0,.74333],73:[0,.68333,.15806,0,.38555],74:[0,.68333,.14028,0,.525],75:[0,.68333,.14528,0,.76888],76:[0,.68333,0,0,.62722],77:[0,.68333,.16389,0,.89666],78:[0,.68333,.16389,0,.74333],79:[0,.68333,.09403,0,.76666],80:[0,.68333,.10257,0,.67833],81:[.19444,.68333,.09403,0,.76666],82:[0,.68333,.03868,0,.72944],83:[0,.68333,.11972,0,.56222],84:[0,.68333,.13305,0,.71555],85:[0,.68333,.16389,0,.74333],86:[0,.68333,.18361,0,.74333],87:[0,.68333,.18361,0,.99888],88:[0,.68333,.15806,0,.74333],89:[0,.68333,.19383,0,.74333],90:[0,.68333,.14528,0,.61333],91:[.25,.75,.1875,0,.30667],93:[.25,.75,.10528,0,.30667],94:[0,.69444,.06646,0,.51111],95:[.31,.12056,.09208,0,.51111],97:[0,.43056,.07671,0,.51111],98:[0,.69444,.06312,0,.46],99:[0,.43056,.05653,0,.46],100:[0,.69444,.10333,0,.51111],101:[0,.43056,.07514,0,.46],102:[.19444,.69444,.21194,0,.30667],103:[.19444,.43056,.08847,0,.46],104:[0,.69444,.07671,0,.51111],105:[0,.65536,.1019,0,.30667],106:[.19444,.65536,.14467,0,.30667],107:[0,.69444,.10764,0,.46],108:[0,.69444,.10333,0,.25555],109:[0,.43056,.07671,0,.81777],110:[0,.43056,.07671,0,.56222],111:[0,.43056,.06312,0,.51111],112:[.19444,.43056,.06312,0,.51111],113:[.19444,.43056,.08847,0,.46],114:[0,.43056,.10764,0,.42166],115:[0,.43056,.08208,0,.40889],116:[0,.61508,.09486,0,.33222],117:[0,.43056,.07671,0,.53666],118:[0,.43056,.10764,0,.46],119:[0,.43056,.10764,0,.66444],120:[0,.43056,.12042,0,.46389],121:[.19444,.43056,.08847,0,.48555],122:[0,.43056,.12292,0,.40889],126:[.35,.31786,.11585,0,.51111],160:[0,0,0,0,.25],168:[0,.66786,.10474,0,.51111],176:[0,.69444,0,0,.83129],184:[.17014,0,0,0,.46],198:[0,.68333,.12028,0,.88277],216:[.04861,.73194,.09403,0,.76666],223:[.19444,.69444,.10514,0,.53666],230:[0,.43056,.07514,0,.71555],248:[.09722,.52778,.09194,0,.51111],338:[0,.68333,.12028,0,.98499],339:[0,.43056,.07514,0,.71555],710:[0,.69444,.06646,0,.51111],711:[0,.62847,.08295,0,.51111],713:[0,.56167,.10333,0,.51111],714:[0,.69444,.09694,0,.51111],715:[0,.69444,0,0,.51111],728:[0,.69444,.10806,0,.51111],729:[0,.66786,.11752,0,.30667],730:[0,.69444,0,0,.83129],732:[0,.66786,.11585,0,.51111],733:[0,.69444,.1225,0,.51111],915:[0,.68333,.13305,0,.62722],916:[0,.68333,0,0,.81777],920:[0,.68333,.09403,0,.76666],923:[0,.68333,0,0,.69222],926:[0,.68333,.15294,0,.66444],928:[0,.68333,.16389,0,.74333],931:[0,.68333,.12028,0,.71555],933:[0,.68333,.11111,0,.76666],934:[0,.68333,.05986,0,.71555],936:[0,.68333,.11111,0,.76666],937:[0,.68333,.10257,0,.71555],8211:[0,.43056,.09208,0,.51111],8212:[0,.43056,.09208,0,1.02222],8216:[0,.69444,.12417,0,.30667],8217:[0,.69444,.12417,0,.30667],8220:[0,.69444,.1685,0,.51444],8221:[0,.69444,.06961,0,.51444],8463:[0,.68889,0,0,.54028]},"Main-Regular":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.27778],34:[0,.69444,0,0,.5],35:[.19444,.69444,0,0,.83334],36:[.05556,.75,0,0,.5],37:[.05556,.75,0,0,.83334],38:[0,.69444,0,0,.77778],39:[0,.69444,0,0,.27778],40:[.25,.75,0,0,.38889],41:[.25,.75,0,0,.38889],42:[0,.75,0,0,.5],43:[.08333,.58333,0,0,.77778],44:[.19444,.10556,0,0,.27778],45:[0,.43056,0,0,.33333],46:[0,.10556,0,0,.27778],47:[.25,.75,0,0,.5],48:[0,.64444,0,0,.5],49:[0,.64444,0,0,.5],50:[0,.64444,0,0,.5],51:[0,.64444,0,0,.5],52:[0,.64444,0,0,.5],53:[0,.64444,0,0,.5],54:[0,.64444,0,0,.5],55:[0,.64444,0,0,.5],56:[0,.64444,0,0,.5],57:[0,.64444,0,0,.5],58:[0,.43056,0,0,.27778],59:[.19444,.43056,0,0,.27778],60:[.0391,.5391,0,0,.77778],61:[-.13313,.36687,0,0,.77778],62:[.0391,.5391,0,0,.77778],63:[0,.69444,0,0,.47222],64:[0,.69444,0,0,.77778],65:[0,.68333,0,0,.75],66:[0,.68333,0,0,.70834],67:[0,.68333,0,0,.72222],68:[0,.68333,0,0,.76389],69:[0,.68333,0,0,.68056],70:[0,.68333,0,0,.65278],71:[0,.68333,0,0,.78472],72:[0,.68333,0,0,.75],73:[0,.68333,0,0,.36111],74:[0,.68333,0,0,.51389],75:[0,.68333,0,0,.77778],76:[0,.68333,0,0,.625],77:[0,.68333,0,0,.91667],78:[0,.68333,0,0,.75],79:[0,.68333,0,0,.77778],80:[0,.68333,0,0,.68056],81:[.19444,.68333,0,0,.77778],82:[0,.68333,0,0,.73611],83:[0,.68333,0,0,.55556],84:[0,.68333,0,0,.72222],85:[0,.68333,0,0,.75],86:[0,.68333,.01389,0,.75],87:[0,.68333,.01389,0,1.02778],88:[0,.68333,0,0,.75],89:[0,.68333,.025,0,.75],90:[0,.68333,0,0,.61111],91:[.25,.75,0,0,.27778],92:[.25,.75,0,0,.5],93:[.25,.75,0,0,.27778],94:[0,.69444,0,0,.5],95:[.31,.12056,.02778,0,.5],97:[0,.43056,0,0,.5],98:[0,.69444,0,0,.55556],99:[0,.43056,0,0,.44445],100:[0,.69444,0,0,.55556],101:[0,.43056,0,0,.44445],102:[0,.69444,.07778,0,.30556],103:[.19444,.43056,.01389,0,.5],104:[0,.69444,0,0,.55556],105:[0,.66786,0,0,.27778],106:[.19444,.66786,0,0,.30556],107:[0,.69444,0,0,.52778],108:[0,.69444,0,0,.27778],109:[0,.43056,0,0,.83334],110:[0,.43056,0,0,.55556],111:[0,.43056,0,0,.5],112:[.19444,.43056,0,0,.55556],113:[.19444,.43056,0,0,.52778],114:[0,.43056,0,0,.39167],115:[0,.43056,0,0,.39445],116:[0,.61508,0,0,.38889],117:[0,.43056,0,0,.55556],118:[0,.43056,.01389,0,.52778],119:[0,.43056,.01389,0,.72222],120:[0,.43056,0,0,.52778],121:[.19444,.43056,.01389,0,.52778],122:[0,.43056,0,0,.44445],123:[.25,.75,0,0,.5],124:[.25,.75,0,0,.27778],125:[.25,.75,0,0,.5],126:[.35,.31786,0,0,.5],160:[0,0,0,0,.25],163:[0,.69444,0,0,.76909],167:[.19444,.69444,0,0,.44445],168:[0,.66786,0,0,.5],172:[0,.43056,0,0,.66667],176:[0,.69444,0,0,.75],177:[.08333,.58333,0,0,.77778],182:[.19444,.69444,0,0,.61111],184:[.17014,0,0,0,.44445],198:[0,.68333,0,0,.90278],215:[.08333,.58333,0,0,.77778],216:[.04861,.73194,0,0,.77778],223:[0,.69444,0,0,.5],230:[0,.43056,0,0,.72222],247:[.08333,.58333,0,0,.77778],248:[.09722,.52778,0,0,.5],305:[0,.43056,0,0,.27778],338:[0,.68333,0,0,1.01389],339:[0,.43056,0,0,.77778],567:[.19444,.43056,0,0,.30556],710:[0,.69444,0,0,.5],711:[0,.62847,0,0,.5],713:[0,.56778,0,0,.5],714:[0,.69444,0,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,0,0,.5],729:[0,.66786,0,0,.27778],730:[0,.69444,0,0,.75],732:[0,.66786,0,0,.5],733:[0,.69444,0,0,.5],915:[0,.68333,0,0,.625],916:[0,.68333,0,0,.83334],920:[0,.68333,0,0,.77778],923:[0,.68333,0,0,.69445],926:[0,.68333,0,0,.66667],928:[0,.68333,0,0,.75],931:[0,.68333,0,0,.72222],933:[0,.68333,0,0,.77778],934:[0,.68333,0,0,.72222],936:[0,.68333,0,0,.77778],937:[0,.68333,0,0,.72222],8211:[0,.43056,.02778,0,.5],8212:[0,.43056,.02778,0,1],8216:[0,.69444,0,0,.27778],8217:[0,.69444,0,0,.27778],8220:[0,.69444,0,0,.5],8221:[0,.69444,0,0,.5],8224:[.19444,.69444,0,0,.44445],8225:[.19444,.69444,0,0,.44445],8230:[0,.12,0,0,1.172],8242:[0,.55556,0,0,.275],8407:[0,.71444,.15382,0,.5],8463:[0,.68889,0,0,.54028],8465:[0,.69444,0,0,.72222],8467:[0,.69444,0,.11111,.41667],8472:[.19444,.43056,0,.11111,.63646],8476:[0,.69444,0,0,.72222],8501:[0,.69444,0,0,.61111],8592:[-.13313,.36687,0,0,1],8593:[.19444,.69444,0,0,.5],8594:[-.13313,.36687,0,0,1],8595:[.19444,.69444,0,0,.5],8596:[-.13313,.36687,0,0,1],8597:[.25,.75,0,0,.5],8598:[.19444,.69444,0,0,1],8599:[.19444,.69444,0,0,1],8600:[.19444,.69444,0,0,1],8601:[.19444,.69444,0,0,1],8614:[.011,.511,0,0,1],8617:[.011,.511,0,0,1.126],8618:[.011,.511,0,0,1.126],8636:[-.13313,.36687,0,0,1],8637:[-.13313,.36687,0,0,1],8640:[-.13313,.36687,0,0,1],8641:[-.13313,.36687,0,0,1],8652:[.011,.671,0,0,1],8656:[-.13313,.36687,0,0,1],8657:[.19444,.69444,0,0,.61111],8658:[-.13313,.36687,0,0,1],8659:[.19444,.69444,0,0,.61111],8660:[-.13313,.36687,0,0,1],8661:[.25,.75,0,0,.61111],8704:[0,.69444,0,0,.55556],8706:[0,.69444,.05556,.08334,.5309],8707:[0,.69444,0,0,.55556],8709:[.05556,.75,0,0,.5],8711:[0,.68333,0,0,.83334],8712:[.0391,.5391,0,0,.66667],8715:[.0391,.5391,0,0,.66667],8722:[.08333,.58333,0,0,.77778],8723:[.08333,.58333,0,0,.77778],8725:[.25,.75,0,0,.5],8726:[.25,.75,0,0,.5],8727:[-.03472,.46528,0,0,.5],8728:[-.05555,.44445,0,0,.5],8729:[-.05555,.44445,0,0,.5],8730:[.2,.8,0,0,.83334],8733:[0,.43056,0,0,.77778],8734:[0,.43056,0,0,1],8736:[0,.69224,0,0,.72222],8739:[.25,.75,0,0,.27778],8741:[.25,.75,0,0,.5],8743:[0,.55556,0,0,.66667],8744:[0,.55556,0,0,.66667],8745:[0,.55556,0,0,.66667],8746:[0,.55556,0,0,.66667],8747:[.19444,.69444,.11111,0,.41667],8764:[-.13313,.36687,0,0,.77778],8768:[.19444,.69444,0,0,.27778],8771:[-.03625,.46375,0,0,.77778],8773:[-.022,.589,0,0,1],8776:[-.01688,.48312,0,0,.77778],8781:[-.03625,.46375,0,0,.77778],8784:[-.133,.67,0,0,.778],8801:[-.03625,.46375,0,0,.77778],8804:[.13597,.63597,0,0,.77778],8805:[.13597,.63597,0,0,.77778],8810:[.0391,.5391,0,0,1],8811:[.0391,.5391,0,0,1],8826:[.0391,.5391,0,0,.77778],8827:[.0391,.5391,0,0,.77778],8834:[.0391,.5391,0,0,.77778],8835:[.0391,.5391,0,0,.77778],8838:[.13597,.63597,0,0,.77778],8839:[.13597,.63597,0,0,.77778],8846:[0,.55556,0,0,.66667],8849:[.13597,.63597,0,0,.77778],8850:[.13597,.63597,0,0,.77778],8851:[0,.55556,0,0,.66667],8852:[0,.55556,0,0,.66667],8853:[.08333,.58333,0,0,.77778],8854:[.08333,.58333,0,0,.77778],8855:[.08333,.58333,0,0,.77778],8856:[.08333,.58333,0,0,.77778],8857:[.08333,.58333,0,0,.77778],8866:[0,.69444,0,0,.61111],8867:[0,.69444,0,0,.61111],8868:[0,.69444,0,0,.77778],8869:[0,.69444,0,0,.77778],8872:[.249,.75,0,0,.867],8900:[-.05555,.44445,0,0,.5],8901:[-.05555,.44445,0,0,.27778],8902:[-.03472,.46528,0,0,.5],8904:[.005,.505,0,0,.9],8942:[.03,.9,0,0,.278],8943:[-.19,.31,0,0,1.172],8945:[-.1,.82,0,0,1.282],8968:[.25,.75,0,0,.44445],8969:[.25,.75,0,0,.44445],8970:[.25,.75,0,0,.44445],8971:[.25,.75,0,0,.44445],8994:[-.14236,.35764,0,0,1],8995:[-.14236,.35764,0,0,1],9136:[.244,.744,0,0,.412],9137:[.244,.744,0,0,.412],9651:[.19444,.69444,0,0,.88889],9657:[-.03472,.46528,0,0,.5],9661:[.19444,.69444,0,0,.88889],9667:[-.03472,.46528,0,0,.5],9711:[.19444,.69444,0,0,1],9824:[.12963,.69444,0,0,.77778],9825:[.12963,.69444,0,0,.77778],9826:[.12963,.69444,0,0,.77778],9827:[.12963,.69444,0,0,.77778],9837:[0,.75,0,0,.38889],9838:[.19444,.69444,0,0,.38889],9839:[.19444,.69444,0,0,.38889],10216:[.25,.75,0,0,.38889],10217:[.25,.75,0,0,.38889],10222:[.244,.744,0,0,.412],10223:[.244,.744,0,0,.412],10229:[.011,.511,0,0,1.609],10230:[.011,.511,0,0,1.638],10231:[.011,.511,0,0,1.859],10232:[.024,.525,0,0,1.609],10233:[.024,.525,0,0,1.638],10234:[.024,.525,0,0,1.858],10236:[.011,.511,0,0,1.638],10815:[0,.68333,0,0,.75],10927:[.13597,.63597,0,0,.77778],10928:[.13597,.63597,0,0,.77778],57376:[.19444,.69444,0,0,0]},"Math-BoldItalic":{32:[0,0,0,0,.25],48:[0,.44444,0,0,.575],49:[0,.44444,0,0,.575],50:[0,.44444,0,0,.575],51:[.19444,.44444,0,0,.575],52:[.19444,.44444,0,0,.575],53:[.19444,.44444,0,0,.575],54:[0,.64444,0,0,.575],55:[.19444,.44444,0,0,.575],56:[0,.64444,0,0,.575],57:[.19444,.44444,0,0,.575],65:[0,.68611,0,0,.86944],66:[0,.68611,.04835,0,.8664],67:[0,.68611,.06979,0,.81694],68:[0,.68611,.03194,0,.93812],69:[0,.68611,.05451,0,.81007],70:[0,.68611,.15972,0,.68889],71:[0,.68611,0,0,.88673],72:[0,.68611,.08229,0,.98229],73:[0,.68611,.07778,0,.51111],74:[0,.68611,.10069,0,.63125],75:[0,.68611,.06979,0,.97118],76:[0,.68611,0,0,.75555],77:[0,.68611,.11424,0,1.14201],78:[0,.68611,.11424,0,.95034],79:[0,.68611,.03194,0,.83666],80:[0,.68611,.15972,0,.72309],81:[.19444,.68611,0,0,.86861],82:[0,.68611,.00421,0,.87235],83:[0,.68611,.05382,0,.69271],84:[0,.68611,.15972,0,.63663],85:[0,.68611,.11424,0,.80027],86:[0,.68611,.25555,0,.67778],87:[0,.68611,.15972,0,1.09305],88:[0,.68611,.07778,0,.94722],89:[0,.68611,.25555,0,.67458],90:[0,.68611,.06979,0,.77257],97:[0,.44444,0,0,.63287],98:[0,.69444,0,0,.52083],99:[0,.44444,0,0,.51342],100:[0,.69444,0,0,.60972],101:[0,.44444,0,0,.55361],102:[.19444,.69444,.11042,0,.56806],103:[.19444,.44444,.03704,0,.5449],104:[0,.69444,0,0,.66759],105:[0,.69326,0,0,.4048],106:[.19444,.69326,.0622,0,.47083],107:[0,.69444,.01852,0,.6037],108:[0,.69444,.0088,0,.34815],109:[0,.44444,0,0,1.0324],110:[0,.44444,0,0,.71296],111:[0,.44444,0,0,.58472],112:[.19444,.44444,0,0,.60092],113:[.19444,.44444,.03704,0,.54213],114:[0,.44444,.03194,0,.5287],115:[0,.44444,0,0,.53125],116:[0,.63492,0,0,.41528],117:[0,.44444,0,0,.68102],118:[0,.44444,.03704,0,.56666],119:[0,.44444,.02778,0,.83148],120:[0,.44444,0,0,.65903],121:[.19444,.44444,.03704,0,.59028],122:[0,.44444,.04213,0,.55509],160:[0,0,0,0,.25],915:[0,.68611,.15972,0,.65694],916:[0,.68611,0,0,.95833],920:[0,.68611,.03194,0,.86722],923:[0,.68611,0,0,.80555],926:[0,.68611,.07458,0,.84125],928:[0,.68611,.08229,0,.98229],931:[0,.68611,.05451,0,.88507],933:[0,.68611,.15972,0,.67083],934:[0,.68611,0,0,.76666],936:[0,.68611,.11653,0,.71402],937:[0,.68611,.04835,0,.8789],945:[0,.44444,0,0,.76064],946:[.19444,.69444,.03403,0,.65972],947:[.19444,.44444,.06389,0,.59003],948:[0,.69444,.03819,0,.52222],949:[0,.44444,0,0,.52882],950:[.19444,.69444,.06215,0,.50833],951:[.19444,.44444,.03704,0,.6],952:[0,.69444,.03194,0,.5618],953:[0,.44444,0,0,.41204],954:[0,.44444,0,0,.66759],955:[0,.69444,0,0,.67083],956:[.19444,.44444,0,0,.70787],957:[0,.44444,.06898,0,.57685],958:[.19444,.69444,.03021,0,.50833],959:[0,.44444,0,0,.58472],960:[0,.44444,.03704,0,.68241],961:[.19444,.44444,0,0,.6118],962:[.09722,.44444,.07917,0,.42361],963:[0,.44444,.03704,0,.68588],964:[0,.44444,.13472,0,.52083],965:[0,.44444,.03704,0,.63055],966:[.19444,.44444,0,0,.74722],967:[.19444,.44444,0,0,.71805],968:[.19444,.69444,.03704,0,.75833],969:[0,.44444,.03704,0,.71782],977:[0,.69444,0,0,.69155],981:[.19444,.69444,0,0,.7125],982:[0,.44444,.03194,0,.975],1009:[.19444,.44444,0,0,.6118],1013:[0,.44444,0,0,.48333],57649:[0,.44444,0,0,.39352],57911:[.19444,.44444,0,0,.43889]},"Math-Italic":{32:[0,0,0,0,.25],48:[0,.43056,0,0,.5],49:[0,.43056,0,0,.5],50:[0,.43056,0,0,.5],51:[.19444,.43056,0,0,.5],52:[.19444,.43056,0,0,.5],53:[.19444,.43056,0,0,.5],54:[0,.64444,0,0,.5],55:[.19444,.43056,0,0,.5],56:[0,.64444,0,0,.5],57:[.19444,.43056,0,0,.5],65:[0,.68333,0,.13889,.75],66:[0,.68333,.05017,.08334,.75851],67:[0,.68333,.07153,.08334,.71472],68:[0,.68333,.02778,.05556,.82792],69:[0,.68333,.05764,.08334,.7382],70:[0,.68333,.13889,.08334,.64306],71:[0,.68333,0,.08334,.78625],72:[0,.68333,.08125,.05556,.83125],73:[0,.68333,.07847,.11111,.43958],74:[0,.68333,.09618,.16667,.55451],75:[0,.68333,.07153,.05556,.84931],76:[0,.68333,0,.02778,.68056],77:[0,.68333,.10903,.08334,.97014],78:[0,.68333,.10903,.08334,.80347],79:[0,.68333,.02778,.08334,.76278],80:[0,.68333,.13889,.08334,.64201],81:[.19444,.68333,0,.08334,.79056],82:[0,.68333,.00773,.08334,.75929],83:[0,.68333,.05764,.08334,.6132],84:[0,.68333,.13889,.08334,.58438],85:[0,.68333,.10903,.02778,.68278],86:[0,.68333,.22222,0,.58333],87:[0,.68333,.13889,0,.94445],88:[0,.68333,.07847,.08334,.82847],89:[0,.68333,.22222,0,.58056],90:[0,.68333,.07153,.08334,.68264],97:[0,.43056,0,0,.52859],98:[0,.69444,0,0,.42917],99:[0,.43056,0,.05556,.43276],100:[0,.69444,0,.16667,.52049],101:[0,.43056,0,.05556,.46563],102:[.19444,.69444,.10764,.16667,.48959],103:[.19444,.43056,.03588,.02778,.47697],104:[0,.69444,0,0,.57616],105:[0,.65952,0,0,.34451],106:[.19444,.65952,.05724,0,.41181],107:[0,.69444,.03148,0,.5206],108:[0,.69444,.01968,.08334,.29838],109:[0,.43056,0,0,.87801],110:[0,.43056,0,0,.60023],111:[0,.43056,0,.05556,.48472],112:[.19444,.43056,0,.08334,.50313],113:[.19444,.43056,.03588,.08334,.44641],114:[0,.43056,.02778,.05556,.45116],115:[0,.43056,0,.05556,.46875],116:[0,.61508,0,.08334,.36111],117:[0,.43056,0,.02778,.57246],118:[0,.43056,.03588,.02778,.48472],119:[0,.43056,.02691,.08334,.71592],120:[0,.43056,0,.02778,.57153],121:[.19444,.43056,.03588,.05556,.49028],122:[0,.43056,.04398,.05556,.46505],160:[0,0,0,0,.25],915:[0,.68333,.13889,.08334,.61528],916:[0,.68333,0,.16667,.83334],920:[0,.68333,.02778,.08334,.76278],923:[0,.68333,0,.16667,.69445],926:[0,.68333,.07569,.08334,.74236],928:[0,.68333,.08125,.05556,.83125],931:[0,.68333,.05764,.08334,.77986],933:[0,.68333,.13889,.05556,.58333],934:[0,.68333,0,.08334,.66667],936:[0,.68333,.11,.05556,.61222],937:[0,.68333,.05017,.08334,.7724],945:[0,.43056,.0037,.02778,.6397],946:[.19444,.69444,.05278,.08334,.56563],947:[.19444,.43056,.05556,0,.51773],948:[0,.69444,.03785,.05556,.44444],949:[0,.43056,0,.08334,.46632],950:[.19444,.69444,.07378,.08334,.4375],951:[.19444,.43056,.03588,.05556,.49653],952:[0,.69444,.02778,.08334,.46944],953:[0,.43056,0,.05556,.35394],954:[0,.43056,0,0,.57616],955:[0,.69444,0,0,.58334],956:[.19444,.43056,0,.02778,.60255],957:[0,.43056,.06366,.02778,.49398],958:[.19444,.69444,.04601,.11111,.4375],959:[0,.43056,0,.05556,.48472],960:[0,.43056,.03588,0,.57003],961:[.19444,.43056,0,.08334,.51702],962:[.09722,.43056,.07986,.08334,.36285],963:[0,.43056,.03588,0,.57141],964:[0,.43056,.1132,.02778,.43715],965:[0,.43056,.03588,.02778,.54028],966:[.19444,.43056,0,.08334,.65417],967:[.19444,.43056,0,.05556,.62569],968:[.19444,.69444,.03588,.11111,.65139],969:[0,.43056,.03588,0,.62245],977:[0,.69444,0,.08334,.59144],981:[.19444,.69444,0,.08334,.59583],982:[0,.43056,.02778,0,.82813],1009:[.19444,.43056,0,.08334,.51702],1013:[0,.43056,0,.05556,.4059],57649:[0,.43056,0,.02778,.32246],57911:[.19444,.43056,0,.08334,.38403]},"SansSerif-Bold":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.36667],34:[0,.69444,0,0,.55834],35:[.19444,.69444,0,0,.91667],36:[.05556,.75,0,0,.55],37:[.05556,.75,0,0,1.02912],38:[0,.69444,0,0,.83056],39:[0,.69444,0,0,.30556],40:[.25,.75,0,0,.42778],41:[.25,.75,0,0,.42778],42:[0,.75,0,0,.55],43:[.11667,.61667,0,0,.85556],44:[.10556,.13056,0,0,.30556],45:[0,.45833,0,0,.36667],46:[0,.13056,0,0,.30556],47:[.25,.75,0,0,.55],48:[0,.69444,0,0,.55],49:[0,.69444,0,0,.55],50:[0,.69444,0,0,.55],51:[0,.69444,0,0,.55],52:[0,.69444,0,0,.55],53:[0,.69444,0,0,.55],54:[0,.69444,0,0,.55],55:[0,.69444,0,0,.55],56:[0,.69444,0,0,.55],57:[0,.69444,0,0,.55],58:[0,.45833,0,0,.30556],59:[.10556,.45833,0,0,.30556],61:[-.09375,.40625,0,0,.85556],63:[0,.69444,0,0,.51945],64:[0,.69444,0,0,.73334],65:[0,.69444,0,0,.73334],66:[0,.69444,0,0,.73334],67:[0,.69444,0,0,.70278],68:[0,.69444,0,0,.79445],69:[0,.69444,0,0,.64167],70:[0,.69444,0,0,.61111],71:[0,.69444,0,0,.73334],72:[0,.69444,0,0,.79445],73:[0,.69444,0,0,.33056],74:[0,.69444,0,0,.51945],75:[0,.69444,0,0,.76389],76:[0,.69444,0,0,.58056],77:[0,.69444,0,0,.97778],78:[0,.69444,0,0,.79445],79:[0,.69444,0,0,.79445],80:[0,.69444,0,0,.70278],81:[.10556,.69444,0,0,.79445],82:[0,.69444,0,0,.70278],83:[0,.69444,0,0,.61111],84:[0,.69444,0,0,.73334],85:[0,.69444,0,0,.76389],86:[0,.69444,.01528,0,.73334],87:[0,.69444,.01528,0,1.03889],88:[0,.69444,0,0,.73334],89:[0,.69444,.0275,0,.73334],90:[0,.69444,0,0,.67223],91:[.25,.75,0,0,.34306],93:[.25,.75,0,0,.34306],94:[0,.69444,0,0,.55],95:[.35,.10833,.03056,0,.55],97:[0,.45833,0,0,.525],98:[0,.69444,0,0,.56111],99:[0,.45833,0,0,.48889],100:[0,.69444,0,0,.56111],101:[0,.45833,0,0,.51111],102:[0,.69444,.07639,0,.33611],103:[.19444,.45833,.01528,0,.55],104:[0,.69444,0,0,.56111],105:[0,.69444,0,0,.25556],106:[.19444,.69444,0,0,.28611],107:[0,.69444,0,0,.53056],108:[0,.69444,0,0,.25556],109:[0,.45833,0,0,.86667],110:[0,.45833,0,0,.56111],111:[0,.45833,0,0,.55],112:[.19444,.45833,0,0,.56111],113:[.19444,.45833,0,0,.56111],114:[0,.45833,.01528,0,.37222],115:[0,.45833,0,0,.42167],116:[0,.58929,0,0,.40417],117:[0,.45833,0,0,.56111],118:[0,.45833,.01528,0,.5],119:[0,.45833,.01528,0,.74445],120:[0,.45833,0,0,.5],121:[.19444,.45833,.01528,0,.5],122:[0,.45833,0,0,.47639],126:[.35,.34444,0,0,.55],160:[0,0,0,0,.25],168:[0,.69444,0,0,.55],176:[0,.69444,0,0,.73334],180:[0,.69444,0,0,.55],184:[.17014,0,0,0,.48889],305:[0,.45833,0,0,.25556],567:[.19444,.45833,0,0,.28611],710:[0,.69444,0,0,.55],711:[0,.63542,0,0,.55],713:[0,.63778,0,0,.55],728:[0,.69444,0,0,.55],729:[0,.69444,0,0,.30556],730:[0,.69444,0,0,.73334],732:[0,.69444,0,0,.55],733:[0,.69444,0,0,.55],915:[0,.69444,0,0,.58056],916:[0,.69444,0,0,.91667],920:[0,.69444,0,0,.85556],923:[0,.69444,0,0,.67223],926:[0,.69444,0,0,.73334],928:[0,.69444,0,0,.79445],931:[0,.69444,0,0,.79445],933:[0,.69444,0,0,.85556],934:[0,.69444,0,0,.79445],936:[0,.69444,0,0,.85556],937:[0,.69444,0,0,.79445],8211:[0,.45833,.03056,0,.55],8212:[0,.45833,.03056,0,1.10001],8216:[0,.69444,0,0,.30556],8217:[0,.69444,0,0,.30556],8220:[0,.69444,0,0,.55834],8221:[0,.69444,0,0,.55834]},"SansSerif-Italic":{32:[0,0,0,0,.25],33:[0,.69444,.05733,0,.31945],34:[0,.69444,.00316,0,.5],35:[.19444,.69444,.05087,0,.83334],36:[.05556,.75,.11156,0,.5],37:[.05556,.75,.03126,0,.83334],38:[0,.69444,.03058,0,.75834],39:[0,.69444,.07816,0,.27778],40:[.25,.75,.13164,0,.38889],41:[.25,.75,.02536,0,.38889],42:[0,.75,.11775,0,.5],43:[.08333,.58333,.02536,0,.77778],44:[.125,.08333,0,0,.27778],45:[0,.44444,.01946,0,.33333],46:[0,.08333,0,0,.27778],47:[.25,.75,.13164,0,.5],48:[0,.65556,.11156,0,.5],49:[0,.65556,.11156,0,.5],50:[0,.65556,.11156,0,.5],51:[0,.65556,.11156,0,.5],52:[0,.65556,.11156,0,.5],53:[0,.65556,.11156,0,.5],54:[0,.65556,.11156,0,.5],55:[0,.65556,.11156,0,.5],56:[0,.65556,.11156,0,.5],57:[0,.65556,.11156,0,.5],58:[0,.44444,.02502,0,.27778],59:[.125,.44444,.02502,0,.27778],61:[-.13,.37,.05087,0,.77778],63:[0,.69444,.11809,0,.47222],64:[0,.69444,.07555,0,.66667],65:[0,.69444,0,0,.66667],66:[0,.69444,.08293,0,.66667],67:[0,.69444,.11983,0,.63889],68:[0,.69444,.07555,0,.72223],69:[0,.69444,.11983,0,.59722],70:[0,.69444,.13372,0,.56945],71:[0,.69444,.11983,0,.66667],72:[0,.69444,.08094,0,.70834],73:[0,.69444,.13372,0,.27778],74:[0,.69444,.08094,0,.47222],75:[0,.69444,.11983,0,.69445],76:[0,.69444,0,0,.54167],77:[0,.69444,.08094,0,.875],78:[0,.69444,.08094,0,.70834],79:[0,.69444,.07555,0,.73611],80:[0,.69444,.08293,0,.63889],81:[.125,.69444,.07555,0,.73611],82:[0,.69444,.08293,0,.64584],83:[0,.69444,.09205,0,.55556],84:[0,.69444,.13372,0,.68056],85:[0,.69444,.08094,0,.6875],86:[0,.69444,.1615,0,.66667],87:[0,.69444,.1615,0,.94445],88:[0,.69444,.13372,0,.66667],89:[0,.69444,.17261,0,.66667],90:[0,.69444,.11983,0,.61111],91:[.25,.75,.15942,0,.28889],93:[.25,.75,.08719,0,.28889],94:[0,.69444,.0799,0,.5],95:[.35,.09444,.08616,0,.5],97:[0,.44444,.00981,0,.48056],98:[0,.69444,.03057,0,.51667],99:[0,.44444,.08336,0,.44445],100:[0,.69444,.09483,0,.51667],101:[0,.44444,.06778,0,.44445],102:[0,.69444,.21705,0,.30556],103:[.19444,.44444,.10836,0,.5],104:[0,.69444,.01778,0,.51667],105:[0,.67937,.09718,0,.23889],106:[.19444,.67937,.09162,0,.26667],107:[0,.69444,.08336,0,.48889],108:[0,.69444,.09483,0,.23889],109:[0,.44444,.01778,0,.79445],110:[0,.44444,.01778,0,.51667],111:[0,.44444,.06613,0,.5],112:[.19444,.44444,.0389,0,.51667],113:[.19444,.44444,.04169,0,.51667],114:[0,.44444,.10836,0,.34167],115:[0,.44444,.0778,0,.38333],116:[0,.57143,.07225,0,.36111],117:[0,.44444,.04169,0,.51667],118:[0,.44444,.10836,0,.46111],119:[0,.44444,.10836,0,.68334],120:[0,.44444,.09169,0,.46111],121:[.19444,.44444,.10836,0,.46111],122:[0,.44444,.08752,0,.43472],126:[.35,.32659,.08826,0,.5],160:[0,0,0,0,.25],168:[0,.67937,.06385,0,.5],176:[0,.69444,0,0,.73752],184:[.17014,0,0,0,.44445],305:[0,.44444,.04169,0,.23889],567:[.19444,.44444,.04169,0,.26667],710:[0,.69444,.0799,0,.5],711:[0,.63194,.08432,0,.5],713:[0,.60889,.08776,0,.5],714:[0,.69444,.09205,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,.09483,0,.5],729:[0,.67937,.07774,0,.27778],730:[0,.69444,0,0,.73752],732:[0,.67659,.08826,0,.5],733:[0,.69444,.09205,0,.5],915:[0,.69444,.13372,0,.54167],916:[0,.69444,0,0,.83334],920:[0,.69444,.07555,0,.77778],923:[0,.69444,0,0,.61111],926:[0,.69444,.12816,0,.66667],928:[0,.69444,.08094,0,.70834],931:[0,.69444,.11983,0,.72222],933:[0,.69444,.09031,0,.77778],934:[0,.69444,.04603,0,.72222],936:[0,.69444,.09031,0,.77778],937:[0,.69444,.08293,0,.72222],8211:[0,.44444,.08616,0,.5],8212:[0,.44444,.08616,0,1],8216:[0,.69444,.07816,0,.27778],8217:[0,.69444,.07816,0,.27778],8220:[0,.69444,.14205,0,.5],8221:[0,.69444,.00316,0,.5]},"SansSerif-Regular":{32:[0,0,0,0,.25],33:[0,.69444,0,0,.31945],34:[0,.69444,0,0,.5],35:[.19444,.69444,0,0,.83334],36:[.05556,.75,0,0,.5],37:[.05556,.75,0,0,.83334],38:[0,.69444,0,0,.75834],39:[0,.69444,0,0,.27778],40:[.25,.75,0,0,.38889],41:[.25,.75,0,0,.38889],42:[0,.75,0,0,.5],43:[.08333,.58333,0,0,.77778],44:[.125,.08333,0,0,.27778],45:[0,.44444,0,0,.33333],46:[0,.08333,0,0,.27778],47:[.25,.75,0,0,.5],48:[0,.65556,0,0,.5],49:[0,.65556,0,0,.5],50:[0,.65556,0,0,.5],51:[0,.65556,0,0,.5],52:[0,.65556,0,0,.5],53:[0,.65556,0,0,.5],54:[0,.65556,0,0,.5],55:[0,.65556,0,0,.5],56:[0,.65556,0,0,.5],57:[0,.65556,0,0,.5],58:[0,.44444,0,0,.27778],59:[.125,.44444,0,0,.27778],61:[-.13,.37,0,0,.77778],63:[0,.69444,0,0,.47222],64:[0,.69444,0,0,.66667],65:[0,.69444,0,0,.66667],66:[0,.69444,0,0,.66667],67:[0,.69444,0,0,.63889],68:[0,.69444,0,0,.72223],69:[0,.69444,0,0,.59722],70:[0,.69444,0,0,.56945],71:[0,.69444,0,0,.66667],72:[0,.69444,0,0,.70834],73:[0,.69444,0,0,.27778],74:[0,.69444,0,0,.47222],75:[0,.69444,0,0,.69445],76:[0,.69444,0,0,.54167],77:[0,.69444,0,0,.875],78:[0,.69444,0,0,.70834],79:[0,.69444,0,0,.73611],80:[0,.69444,0,0,.63889],81:[.125,.69444,0,0,.73611],82:[0,.69444,0,0,.64584],83:[0,.69444,0,0,.55556],84:[0,.69444,0,0,.68056],85:[0,.69444,0,0,.6875],86:[0,.69444,.01389,0,.66667],87:[0,.69444,.01389,0,.94445],88:[0,.69444,0,0,.66667],89:[0,.69444,.025,0,.66667],90:[0,.69444,0,0,.61111],91:[.25,.75,0,0,.28889],93:[.25,.75,0,0,.28889],94:[0,.69444,0,0,.5],95:[.35,.09444,.02778,0,.5],97:[0,.44444,0,0,.48056],98:[0,.69444,0,0,.51667],99:[0,.44444,0,0,.44445],100:[0,.69444,0,0,.51667],101:[0,.44444,0,0,.44445],102:[0,.69444,.06944,0,.30556],103:[.19444,.44444,.01389,0,.5],104:[0,.69444,0,0,.51667],105:[0,.67937,0,0,.23889],106:[.19444,.67937,0,0,.26667],107:[0,.69444,0,0,.48889],108:[0,.69444,0,0,.23889],109:[0,.44444,0,0,.79445],110:[0,.44444,0,0,.51667],111:[0,.44444,0,0,.5],112:[.19444,.44444,0,0,.51667],113:[.19444,.44444,0,0,.51667],114:[0,.44444,.01389,0,.34167],115:[0,.44444,0,0,.38333],116:[0,.57143,0,0,.36111],117:[0,.44444,0,0,.51667],118:[0,.44444,.01389,0,.46111],119:[0,.44444,.01389,0,.68334],120:[0,.44444,0,0,.46111],121:[.19444,.44444,.01389,0,.46111],122:[0,.44444,0,0,.43472],126:[.35,.32659,0,0,.5],160:[0,0,0,0,.25],168:[0,.67937,0,0,.5],176:[0,.69444,0,0,.66667],184:[.17014,0,0,0,.44445],305:[0,.44444,0,0,.23889],567:[.19444,.44444,0,0,.26667],710:[0,.69444,0,0,.5],711:[0,.63194,0,0,.5],713:[0,.60889,0,0,.5],714:[0,.69444,0,0,.5],715:[0,.69444,0,0,.5],728:[0,.69444,0,0,.5],729:[0,.67937,0,0,.27778],730:[0,.69444,0,0,.66667],732:[0,.67659,0,0,.5],733:[0,.69444,0,0,.5],915:[0,.69444,0,0,.54167],916:[0,.69444,0,0,.83334],920:[0,.69444,0,0,.77778],923:[0,.69444,0,0,.61111],926:[0,.69444,0,0,.66667],928:[0,.69444,0,0,.70834],931:[0,.69444,0,0,.72222],933:[0,.69444,0,0,.77778],934:[0,.69444,0,0,.72222],936:[0,.69444,0,0,.77778],937:[0,.69444,0,0,.72222],8211:[0,.44444,.02778,0,.5],8212:[0,.44444,.02778,0,1],8216:[0,.69444,0,0,.27778],8217:[0,.69444,0,0,.27778],8220:[0,.69444,0,0,.5],8221:[0,.69444,0,0,.5]},"Script-Regular":{32:[0,0,0,0,.25],65:[0,.7,.22925,0,.80253],66:[0,.7,.04087,0,.90757],67:[0,.7,.1689,0,.66619],68:[0,.7,.09371,0,.77443],69:[0,.7,.18583,0,.56162],70:[0,.7,.13634,0,.89544],71:[0,.7,.17322,0,.60961],72:[0,.7,.29694,0,.96919],73:[0,.7,.19189,0,.80907],74:[.27778,.7,.19189,0,1.05159],75:[0,.7,.31259,0,.91364],76:[0,.7,.19189,0,.87373],77:[0,.7,.15981,0,1.08031],78:[0,.7,.3525,0,.9015],79:[0,.7,.08078,0,.73787],80:[0,.7,.08078,0,1.01262],81:[0,.7,.03305,0,.88282],82:[0,.7,.06259,0,.85],83:[0,.7,.19189,0,.86767],84:[0,.7,.29087,0,.74697],85:[0,.7,.25815,0,.79996],86:[0,.7,.27523,0,.62204],87:[0,.7,.27523,0,.80532],88:[0,.7,.26006,0,.94445],89:[0,.7,.2939,0,.70961],90:[0,.7,.24037,0,.8212],160:[0,0,0,0,.25]},"Size1-Regular":{32:[0,0,0,0,.25],40:[.35001,.85,0,0,.45834],41:[.35001,.85,0,0,.45834],47:[.35001,.85,0,0,.57778],91:[.35001,.85,0,0,.41667],92:[.35001,.85,0,0,.57778],93:[.35001,.85,0,0,.41667],123:[.35001,.85,0,0,.58334],125:[.35001,.85,0,0,.58334],160:[0,0,0,0,.25],710:[0,.72222,0,0,.55556],732:[0,.72222,0,0,.55556],770:[0,.72222,0,0,.55556],771:[0,.72222,0,0,.55556],8214:[-99e-5,.601,0,0,.77778],8593:[1e-5,.6,0,0,.66667],8595:[1e-5,.6,0,0,.66667],8657:[1e-5,.6,0,0,.77778],8659:[1e-5,.6,0,0,.77778],8719:[.25001,.75,0,0,.94445],8720:[.25001,.75,0,0,.94445],8721:[.25001,.75,0,0,1.05556],8730:[.35001,.85,0,0,1],8739:[-.00599,.606,0,0,.33333],8741:[-.00599,.606,0,0,.55556],8747:[.30612,.805,.19445,0,.47222],8748:[.306,.805,.19445,0,.47222],8749:[.306,.805,.19445,0,.47222],8750:[.30612,.805,.19445,0,.47222],8896:[.25001,.75,0,0,.83334],8897:[.25001,.75,0,0,.83334],8898:[.25001,.75,0,0,.83334],8899:[.25001,.75,0,0,.83334],8968:[.35001,.85,0,0,.47222],8969:[.35001,.85,0,0,.47222],8970:[.35001,.85,0,0,.47222],8971:[.35001,.85,0,0,.47222],9168:[-99e-5,.601,0,0,.66667],10216:[.35001,.85,0,0,.47222],10217:[.35001,.85,0,0,.47222],10752:[.25001,.75,0,0,1.11111],10753:[.25001,.75,0,0,1.11111],10754:[.25001,.75,0,0,1.11111],10756:[.25001,.75,0,0,.83334],10758:[.25001,.75,0,0,.83334]},"Size2-Regular":{32:[0,0,0,0,.25],40:[.65002,1.15,0,0,.59722],41:[.65002,1.15,0,0,.59722],47:[.65002,1.15,0,0,.81111],91:[.65002,1.15,0,0,.47222],92:[.65002,1.15,0,0,.81111],93:[.65002,1.15,0,0,.47222],123:[.65002,1.15,0,0,.66667],125:[.65002,1.15,0,0,.66667],160:[0,0,0,0,.25],710:[0,.75,0,0,1],732:[0,.75,0,0,1],770:[0,.75,0,0,1],771:[0,.75,0,0,1],8719:[.55001,1.05,0,0,1.27778],8720:[.55001,1.05,0,0,1.27778],8721:[.55001,1.05,0,0,1.44445],8730:[.65002,1.15,0,0,1],8747:[.86225,1.36,.44445,0,.55556],8748:[.862,1.36,.44445,0,.55556],8749:[.862,1.36,.44445,0,.55556],8750:[.86225,1.36,.44445,0,.55556],8896:[.55001,1.05,0,0,1.11111],8897:[.55001,1.05,0,0,1.11111],8898:[.55001,1.05,0,0,1.11111],8899:[.55001,1.05,0,0,1.11111],8968:[.65002,1.15,0,0,.52778],8969:[.65002,1.15,0,0,.52778],8970:[.65002,1.15,0,0,.52778],8971:[.65002,1.15,0,0,.52778],10216:[.65002,1.15,0,0,.61111],10217:[.65002,1.15,0,0,.61111],10752:[.55001,1.05,0,0,1.51112],10753:[.55001,1.05,0,0,1.51112],10754:[.55001,1.05,0,0,1.51112],10756:[.55001,1.05,0,0,1.11111],10758:[.55001,1.05,0,0,1.11111]},"Size3-Regular":{32:[0,0,0,0,.25],40:[.95003,1.45,0,0,.73611],41:[.95003,1.45,0,0,.73611],47:[.95003,1.45,0,0,1.04445],91:[.95003,1.45,0,0,.52778],92:[.95003,1.45,0,0,1.04445],93:[.95003,1.45,0,0,.52778],123:[.95003,1.45,0,0,.75],125:[.95003,1.45,0,0,.75],160:[0,0,0,0,.25],710:[0,.75,0,0,1.44445],732:[0,.75,0,0,1.44445],770:[0,.75,0,0,1.44445],771:[0,.75,0,0,1.44445],8730:[.95003,1.45,0,0,1],8968:[.95003,1.45,0,0,.58334],8969:[.95003,1.45,0,0,.58334],8970:[.95003,1.45,0,0,.58334],8971:[.95003,1.45,0,0,.58334],10216:[.95003,1.45,0,0,.75],10217:[.95003,1.45,0,0,.75]},"Size4-Regular":{32:[0,0,0,0,.25],40:[1.25003,1.75,0,0,.79167],41:[1.25003,1.75,0,0,.79167],47:[1.25003,1.75,0,0,1.27778],91:[1.25003,1.75,0,0,.58334],92:[1.25003,1.75,0,0,1.27778],93:[1.25003,1.75,0,0,.58334],123:[1.25003,1.75,0,0,.80556],125:[1.25003,1.75,0,0,.80556],160:[0,0,0,0,.25],710:[0,.825,0,0,1.8889],732:[0,.825,0,0,1.8889],770:[0,.825,0,0,1.8889],771:[0,.825,0,0,1.8889],8730:[1.25003,1.75,0,0,1],8968:[1.25003,1.75,0,0,.63889],8969:[1.25003,1.75,0,0,.63889],8970:[1.25003,1.75,0,0,.63889],8971:[1.25003,1.75,0,0,.63889],9115:[.64502,1.155,0,0,.875],9116:[1e-5,.6,0,0,.875],9117:[.64502,1.155,0,0,.875],9118:[.64502,1.155,0,0,.875],9119:[1e-5,.6,0,0,.875],9120:[.64502,1.155,0,0,.875],9121:[.64502,1.155,0,0,.66667],9122:[-99e-5,.601,0,0,.66667],9123:[.64502,1.155,0,0,.66667],9124:[.64502,1.155,0,0,.66667],9125:[-99e-5,.601,0,0,.66667],9126:[.64502,1.155,0,0,.66667],9127:[1e-5,.9,0,0,.88889],9128:[.65002,1.15,0,0,.88889],9129:[.90001,0,0,0,.88889],9130:[0,.3,0,0,.88889],9131:[1e-5,.9,0,0,.88889],9132:[.65002,1.15,0,0,.88889],9133:[.90001,0,0,0,.88889],9143:[.88502,.915,0,0,1.05556],10216:[1.25003,1.75,0,0,.80556],10217:[1.25003,1.75,0,0,.80556],57344:[-.00499,.605,0,0,1.05556],57345:[-.00499,.605,0,0,1.05556],57680:[0,.12,0,0,.45],57681:[0,.12,0,0,.45],57682:[0,.12,0,0,.45],57683:[0,.12,0,0,.45]},"Typewriter-Regular":{32:[0,0,0,0,.525],33:[0,.61111,0,0,.525],34:[0,.61111,0,0,.525],35:[0,.61111,0,0,.525],36:[.08333,.69444,0,0,.525],37:[.08333,.69444,0,0,.525],38:[0,.61111,0,0,.525],39:[0,.61111,0,0,.525],40:[.08333,.69444,0,0,.525],41:[.08333,.69444,0,0,.525],42:[0,.52083,0,0,.525],43:[-.08056,.53055,0,0,.525],44:[.13889,.125,0,0,.525],45:[-.08056,.53055,0,0,.525],46:[0,.125,0,0,.525],47:[.08333,.69444,0,0,.525],48:[0,.61111,0,0,.525],49:[0,.61111,0,0,.525],50:[0,.61111,0,0,.525],51:[0,.61111,0,0,.525],52:[0,.61111,0,0,.525],53:[0,.61111,0,0,.525],54:[0,.61111,0,0,.525],55:[0,.61111,0,0,.525],56:[0,.61111,0,0,.525],57:[0,.61111,0,0,.525],58:[0,.43056,0,0,.525],59:[.13889,.43056,0,0,.525],60:[-.05556,.55556,0,0,.525],61:[-.19549,.41562,0,0,.525],62:[-.05556,.55556,0,0,.525],63:[0,.61111,0,0,.525],64:[0,.61111,0,0,.525],65:[0,.61111,0,0,.525],66:[0,.61111,0,0,.525],67:[0,.61111,0,0,.525],68:[0,.61111,0,0,.525],69:[0,.61111,0,0,.525],70:[0,.61111,0,0,.525],71:[0,.61111,0,0,.525],72:[0,.61111,0,0,.525],73:[0,.61111,0,0,.525],74:[0,.61111,0,0,.525],75:[0,.61111,0,0,.525],76:[0,.61111,0,0,.525],77:[0,.61111,0,0,.525],78:[0,.61111,0,0,.525],79:[0,.61111,0,0,.525],80:[0,.61111,0,0,.525],81:[.13889,.61111,0,0,.525],82:[0,.61111,0,0,.525],83:[0,.61111,0,0,.525],84:[0,.61111,0,0,.525],85:[0,.61111,0,0,.525],86:[0,.61111,0,0,.525],87:[0,.61111,0,0,.525],88:[0,.61111,0,0,.525],89:[0,.61111,0,0,.525],90:[0,.61111,0,0,.525],91:[.08333,.69444,0,0,.525],92:[.08333,.69444,0,0,.525],93:[.08333,.69444,0,0,.525],94:[0,.61111,0,0,.525],95:[.09514,0,0,0,.525],96:[0,.61111,0,0,.525],97:[0,.43056,0,0,.525],98:[0,.61111,0,0,.525],99:[0,.43056,0,0,.525],100:[0,.61111,0,0,.525],101:[0,.43056,0,0,.525],102:[0,.61111,0,0,.525],103:[.22222,.43056,0,0,.525],104:[0,.61111,0,0,.525],105:[0,.61111,0,0,.525],106:[.22222,.61111,0,0,.525],107:[0,.61111,0,0,.525],108:[0,.61111,0,0,.525],109:[0,.43056,0,0,.525],110:[0,.43056,0,0,.525],111:[0,.43056,0,0,.525],112:[.22222,.43056,0,0,.525],113:[.22222,.43056,0,0,.525],114:[0,.43056,0,0,.525],115:[0,.43056,0,0,.525],116:[0,.55358,0,0,.525],117:[0,.43056,0,0,.525],118:[0,.43056,0,0,.525],119:[0,.43056,0,0,.525],120:[0,.43056,0,0,.525],121:[.22222,.43056,0,0,.525],122:[0,.43056,0,0,.525],123:[.08333,.69444,0,0,.525],124:[.08333,.69444,0,0,.525],125:[.08333,.69444,0,0,.525],126:[0,.61111,0,0,.525],127:[0,.61111,0,0,.525],160:[0,0,0,0,.525],176:[0,.61111,0,0,.525],184:[.19445,0,0,0,.525],305:[0,.43056,0,0,.525],567:[.22222,.43056,0,0,.525],711:[0,.56597,0,0,.525],713:[0,.56555,0,0,.525],714:[0,.61111,0,0,.525],715:[0,.61111,0,0,.525],728:[0,.61111,0,0,.525],730:[0,.61111,0,0,.525],770:[0,.61111,0,0,.525],771:[0,.61111,0,0,.525],776:[0,.61111,0,0,.525],915:[0,.61111,0,0,.525],916:[0,.61111,0,0,.525],920:[0,.61111,0,0,.525],923:[0,.61111,0,0,.525],926:[0,.61111,0,0,.525],928:[0,.61111,0,0,.525],931:[0,.61111,0,0,.525],933:[0,.61111,0,0,.525],934:[0,.61111,0,0,.525],936:[0,.61111,0,0,.525],937:[0,.61111,0,0,.525],8216:[0,.61111,0,0,.525],8217:[0,.61111,0,0,.525],8242:[0,.61111,0,0,.525],9251:[.11111,.21944,0,0,.525]}},Mt={slant:[.25,.25,.25],space:[0,0,0],stretch:[0,0,0],shrink:[0,0,0],xHeight:[.431,.431,.431],quad:[1,1.171,1.472],extraSpace:[0,0,0],num1:[.677,.732,.925],num2:[.394,.384,.387],num3:[.444,.471,.504],denom1:[.686,.752,1.025],denom2:[.345,.344,.532],sup1:[.413,.503,.504],sup2:[.363,.431,.404],sup3:[.289,.286,.294],sub1:[.15,.143,.2],sub2:[.247,.286,.4],supDrop:[.386,.353,.494],subDrop:[.05,.071,.1],delim1:[2.39,1.7,1.98],delim2:[1.01,1.157,1.42],axisHeight:[.25,.25,.25],defaultRuleThickness:[.04,.049,.049],bigOpSpacing1:[.111,.111,.111],bigOpSpacing2:[.166,.166,.166],bigOpSpacing3:[.2,.2,.2],bigOpSpacing4:[.6,.611,.611],bigOpSpacing5:[.1,.143,.143],sqrtRuleThickness:[.04,.04,.04],ptPerEm:[10,10,10],doubleRuleSep:[.2,.2,.2],arrayRuleWidth:[.04,.04,.04],fboxsep:[.3,.3,.3],fboxrule:[.04,.04,.04]},J0={\u00C5:"A",\u00C7:"C",\u00D0:"D",\u00DE:"o",\u00E5:"a",\u00E7:"c",\u00F0:"d",\u00FE:"o",\u0410:"A",\u0411:"B",\u0412:"B",\u0413:"F",\u0414:"A",\u0415:"E",\u0416:"K",\u0417:"3",\u0418:"N",\u0419:"N",\u041A:"K",\u041B:"N",\u041C:"M",\u041D:"H",\u041E:"O",\u041F:"N",\u0420:"P",\u0421:"C",\u0422:"T",\u0423:"y",\u0424:"O",\u0425:"X",\u0426:"U",\u0427:"h",\u0428:"W",\u0429:"W",\u042A:"B",\u042B:"X",\u042C:"B",\u042D:"3",\u042E:"X",\u042F:"R",\u0430:"a",\u0431:"b",\u0432:"a",\u0433:"r",\u0434:"y",\u0435:"e",\u0436:"m",\u0437:"e",\u0438:"n",\u0439:"n",\u043A:"n",\u043B:"n",\u043C:"m",\u043D:"n",\u043E:"o",\u043F:"n",\u0440:"p",\u0441:"c",\u0442:"o",\u0443:"y",\u0444:"b",\u0445:"x",\u0446:"n",\u0447:"n",\u0448:"w",\u0449:"w",\u044A:"a",\u044B:"m",\u044C:"a",\u044D:"e",\u044E:"m",\u044F:"r"};function xo(e,t){Ae[e]=t}function Jt(e,t,r){if(!Ae[t])throw new Error("Font metrics not found for font: "+t+".");var n=e.charCodeAt(0),o=Ae[t][n];if(!o&&e[0]in J0&&(n=J0[e[0]].charCodeAt(0),o=Ae[t][n]),!o&&r==="text"&&$0(n)&&(o=Ae[t][77]),o)return{depth:o[0],height:o[1],italic:o[2],skew:o[3],width:o[4]}}var e0={};function bo(e){var t;if(e>=5?t=0:e>=3?t=1:t=2,!e0[t]){var r=e0[t]={cssEmPerMu:Mt.quad[t]/18};for(var n in Mt)Mt.hasOwnProperty(n)&&(r[n]=Mt[n][t])}return e0[t]}var yo={bin:1,close:1,inner:1,open:1,punct:1,rel:1},vo={"accent-token":1,mathord:1,"op-token":1,spacing:1,textord:1},Tt={math:{},text:{}},K=Tt;function i(e,t,r,n,o,a){Tt[e][o]={font:t,group:r,replace:n},a&&n&&(Tt[e][n]=Tt[e][o])}var s="math",k="text",c="main",d="ams",Q="accent-token",C="bin",se="close",ot="inner",R="mathord",re="op-token",me="open",Ct="punct",p="rel",we="spacing",g="textord";i(s,c,p,"\u2261","\\equiv",!0),i(s,c,p,"\u227A","\\prec",!0),i(s,c,p,"\u227B","\\succ",!0),i(s,c,p,"\u223C","\\sim",!0),i(s,c,p,"\u22A5","\\perp"),i(s,c,p,"\u2AAF","\\preceq",!0),i(s,c,p,"\u2AB0","\\succeq",!0),i(s,c,p,"\u2243","\\simeq",!0),i(s,c,p,"\u2223","\\mid",!0),i(s,c,p,"\u226A","\\ll",!0),i(s,c,p,"\u226B","\\gg",!0),i(s,c,p,"\u224D","\\asymp",!0),i(s,c,p,"\u2225","\\parallel"),i(s,c,p,"\u22C8","\\bowtie",!0),i(s,c,p,"\u2323","\\smile",!0),i(s,c,p,"\u2291","\\sqsubseteq",!0),i(s,c,p,"\u2292","\\sqsupseteq",!0),i(s,c,p,"\u2250","\\doteq",!0),i(s,c,p,"\u2322","\\frown",!0),i(s,c,p,"\u220B","\\ni",!0),i(s,c,p,"\u221D","\\propto",!0),i(s,c,p,"\u22A2","\\vdash",!0),i(s,c,p,"\u22A3","\\dashv",!0),i(s,c,p,"\u220B","\\owns"),i(s,c,Ct,".","\\ldotp"),i(s,c,Ct,"\u22C5","\\cdotp"),i(s,c,g,"#","\\#"),i(k,c,g,"#","\\#"),i(s,c,g,"&","\\&"),i(k,c,g,"&","\\&"),i(s,c,g,"\u2135","\\aleph",!0),i(s,c,g,"\u2200","\\forall",!0),i(s,c,g,"\u210F","\\hbar",!0),i(s,c,g,"\u2203","\\exists",!0),i(s,c,g,"\u2207","\\nabla",!0),i(s,c,g,"\u266D","\\flat",!0),i(s,c,g,"\u2113","\\ell",!0),i(s,c,g,"\u266E","\\natural",!0),i(s,c,g,"\u2663","\\clubsuit",!0),i(s,c,g,"\u2118","\\wp",!0),i(s,c,g,"\u266F","\\sharp",!0),i(s,c,g,"\u2662","\\diamondsuit",!0),i(s,c,g,"\u211C","\\Re",!0),i(s,c,g,"\u2661","\\heartsuit",!0),i(s,c,g,"\u2111","\\Im",!0),i(s,c,g,"\u2660","\\spadesuit",!0),i(k,c,g,"\xA7","\\S",!0),i(k,c,g,"\xB6","\\P",!0),i(s,c,g,"\u2020","\\dag"),i(k,c,g,"\u2020","\\dag"),i(k,c,g,"\u2020","\\textdagger"),i(s,c,g,"\u2021","\\ddag"),i(k,c,g,"\u2021","\\ddag"),i(k,c,g,"\u2021","\\textdaggerdbl"),i(s,c,se,"\u23B1","\\rmoustache",!0),i(s,c,me,"\u23B0","\\lmoustache",!0),i(s,c,se,"\u27EF","\\rgroup",!0),i(s,c,me,"\u27EE","\\lgroup",!0),i(s,c,C,"\u2213","\\mp",!0),i(s,c,C,"\u2296","\\ominus",!0),i(s,c,C,"\u228E","\\uplus",!0),i(s,c,C,"\u2293","\\sqcap",!0),i(s,c,C,"\u2217","\\ast"),i(s,c,C,"\u2294","\\sqcup",!0),i(s,c,C,"\u25EF","\\bigcirc",!0),i(s,c,C,"\u2219","\\bullet"),i(s,c,C,"\u2021","\\ddagger"),i(s,c,C,"\u2240","\\wr",!0),i(s,c,C,"\u2A3F","\\amalg"),i(s,c,C,"&","\\And"),i(s,c,p,"\u27F5","\\longleftarrow",!0),i(s,c,p,"\u21D0","\\Leftarrow",!0),i(s,c,p,"\u27F8","\\Longleftarrow",!0),i(s,c,p,"\u27F6","\\longrightarrow",!0),i(s,c,p,"\u21D2","\\Rightarrow",!0),i(s,c,p,"\u27F9","\\Longrightarrow",!0),i(s,c,p,"\u2194","\\leftrightarrow",!0),i(s,c,p,"\u27F7","\\longleftrightarrow",!0),i(s,c,p,"\u21D4","\\Leftrightarrow",!0),i(s,c,p,"\u27FA","\\Longleftrightarrow",!0),i(s,c,p,"\u21A6","\\mapsto",!0),i(s,c,p,"\u27FC","\\longmapsto",!0),i(s,c,p,"\u2197","\\nearrow",!0),i(s,c,p,"\u21A9","\\hookleftarrow",!0),i(s,c,p,"\u21AA","\\hookrightarrow",!0),i(s,c,p,"\u2198","\\searrow",!0),i(s,c,p,"\u21BC","\\leftharpoonup",!0),i(s,c,p,"\u21C0","\\rightharpoonup",!0),i(s,c,p,"\u2199","\\swarrow",!0),i(s,c,p,"\u21BD","\\leftharpoondown",!0),i(s,c,p,"\u21C1","\\rightharpoondown",!0),i(s,c,p,"\u2196","\\nwarrow",!0),i(s,c,p,"\u21CC","\\rightleftharpoons",!0),i(s,d,p,"\u226E","\\nless",!0),i(s,d,p,"\uE010","\\@nleqslant"),i(s,d,p,"\uE011","\\@nleqq"),i(s,d,p,"\u2A87","\\lneq",!0),i(s,d,p,"\u2268","\\lneqq",!0),i(s,d,p,"\uE00C","\\@lvertneqq"),i(s,d,p,"\u22E6","\\lnsim",!0),i(s,d,p,"\u2A89","\\lnapprox",!0),i(s,d,p,"\u2280","\\nprec",!0),i(s,d,p,"\u22E0","\\npreceq",!0),i(s,d,p,"\u22E8","\\precnsim",!0),i(s,d,p,"\u2AB9","\\precnapprox",!0),i(s,d,p,"\u2241","\\nsim",!0),i(s,d,p,"\uE006","\\@nshortmid"),i(s,d,p,"\u2224","\\nmid",!0),i(s,d,p,"\u22AC","\\nvdash",!0),i(s,d,p,"\u22AD","\\nvDash",!0),i(s,d,p,"\u22EA","\\ntriangleleft"),i(s,d,p,"\u22EC","\\ntrianglelefteq",!0),i(s,d,p,"\u228A","\\subsetneq",!0),i(s,d,p,"\uE01A","\\@varsubsetneq"),i(s,d,p,"\u2ACB","\\subsetneqq",!0),i(s,d,p,"\uE017","\\@varsubsetneqq"),i(s,d,p,"\u226F","\\ngtr",!0),i(s,d,p,"\uE00F","\\@ngeqslant"),i(s,d,p,"\uE00E","\\@ngeqq"),i(s,d,p,"\u2A88","\\gneq",!0),i(s,d,p,"\u2269","\\gneqq",!0),i(s,d,p,"\uE00D","\\@gvertneqq"),i(s,d,p,"\u22E7","\\gnsim",!0),i(s,d,p,"\u2A8A","\\gnapprox",!0),i(s,d,p,"\u2281","\\nsucc",!0),i(s,d,p,"\u22E1","\\nsucceq",!0),i(s,d,p,"\u22E9","\\succnsim",!0),i(s,d,p,"\u2ABA","\\succnapprox",!0),i(s,d,p,"\u2246","\\ncong",!0),i(s,d,p,"\uE007","\\@nshortparallel"),i(s,d,p,"\u2226","\\nparallel",!0),i(s,d,p,"\u22AF","\\nVDash",!0),i(s,d,p,"\u22EB","\\ntriangleright"),i(s,d,p,"\u22ED","\\ntrianglerighteq",!0),i(s,d,p,"\uE018","\\@nsupseteqq"),i(s,d,p,"\u228B","\\supsetneq",!0),i(s,d,p,"\uE01B","\\@varsupsetneq"),i(s,d,p,"\u2ACC","\\supsetneqq",!0),i(s,d,p,"\uE019","\\@varsupsetneqq"),i(s,d,p,"\u22AE","\\nVdash",!0),i(s,d,p,"\u2AB5","\\precneqq",!0),i(s,d,p,"\u2AB6","\\succneqq",!0),i(s,d,p,"\uE016","\\@nsubseteqq"),i(s,d,C,"\u22B4","\\unlhd"),i(s,d,C,"\u22B5","\\unrhd"),i(s,d,p,"\u219A","\\nleftarrow",!0),i(s,d,p,"\u219B","\\nrightarrow",!0),i(s,d,p,"\u21CD","\\nLeftarrow",!0),i(s,d,p,"\u21CF","\\nRightarrow",!0),i(s,d,p,"\u21AE","\\nleftrightarrow",!0),i(s,d,p,"\u21CE","\\nLeftrightarrow",!0),i(s,d,p,"\u25B3","\\vartriangle"),i(s,d,g,"\u210F","\\hslash"),i(s,d,g,"\u25BD","\\triangledown"),i(s,d,g,"\u25CA","\\lozenge"),i(s,d,g,"\u24C8","\\circledS"),i(s,d,g,"\xAE","\\circledR"),i(k,d,g,"\xAE","\\circledR"),i(s,d,g,"\u2221","\\measuredangle",!0),i(s,d,g,"\u2204","\\nexists"),i(s,d,g,"\u2127","\\mho"),i(s,d,g,"\u2132","\\Finv",!0),i(s,d,g,"\u2141","\\Game",!0),i(s,d,g,"\u2035","\\backprime"),i(s,d,g,"\u25B2","\\blacktriangle"),i(s,d,g,"\u25BC","\\blacktriangledown"),i(s,d,g,"\u25A0","\\blacksquare"),i(s,d,g,"\u29EB","\\blacklozenge"),i(s,d,g,"\u2605","\\bigstar"),i(s,d,g,"\u2222","\\sphericalangle",!0),i(s,d,g,"\u2201","\\complement",!0),i(s,d,g,"\xF0","\\eth",!0),i(k,c,g,"\xF0","\xF0"),i(s,d,g,"\u2571","\\diagup"),i(s,d,g,"\u2572","\\diagdown"),i(s,d,g,"\u25A1","\\square"),i(s,d,g,"\u25A1","\\Box"),i(s,d,g,"\u25CA","\\Diamond"),i(s,d,g,"\xA5","\\yen",!0),i(k,d,g,"\xA5","\\yen",!0),i(s,d,g,"\u2713","\\checkmark",!0),i(k,d,g,"\u2713","\\checkmark"),i(s,d,g,"\u2136","\\beth",!0),i(s,d,g,"\u2138","\\daleth",!0),i(s,d,g,"\u2137","\\gimel",!0),i(s,d,g,"\u03DD","\\digamma",!0),i(s,d,g,"\u03F0","\\varkappa"),i(s,d,me,"\u250C","\\@ulcorner",!0),i(s,d,se,"\u2510","\\@urcorner",!0),i(s,d,me,"\u2514","\\@llcorner",!0),i(s,d,se,"\u2518","\\@lrcorner",!0),i(s,d,p,"\u2266","\\leqq",!0),i(s,d,p,"\u2A7D","\\leqslant",!0),i(s,d,p,"\u2A95","\\eqslantless",!0),i(s,d,p,"\u2272","\\lesssim",!0),i(s,d,p,"\u2A85","\\lessapprox",!0),i(s,d,p,"\u224A","\\approxeq",!0),i(s,d,C,"\u22D6","\\lessdot"),i(s,d,p,"\u22D8","\\lll",!0),i(s,d,p,"\u2276","\\lessgtr",!0),i(s,d,p,"\u22DA","\\lesseqgtr",!0),i(s,d,p,"\u2A8B","\\lesseqqgtr",!0),i(s,d,p,"\u2251","\\doteqdot"),i(s,d,p,"\u2253","\\risingdotseq",!0),i(s,d,p,"\u2252","\\fallingdotseq",!0),i(s,d,p,"\u223D","\\backsim",!0),i(s,d,p,"\u22CD","\\backsimeq",!0),i(s,d,p,"\u2AC5","\\subseteqq",!0),i(s,d,p,"\u22D0","\\Subset",!0),i(s,d,p,"\u228F","\\sqsubset",!0),i(s,d,p,"\u227C","\\preccurlyeq",!0),i(s,d,p,"\u22DE","\\curlyeqprec",!0),i(s,d,p,"\u227E","\\precsim",!0),i(s,d,p,"\u2AB7","\\precapprox",!0),i(s,d,p,"\u22B2","\\vartriangleleft"),i(s,d,p,"\u22B4","\\trianglelefteq"),i(s,d,p,"\u22A8","\\vDash",!0),i(s,d,p,"\u22AA","\\Vvdash",!0),i(s,d,p,"\u2323","\\smallsmile"),i(s,d,p,"\u2322","\\smallfrown"),i(s,d,p,"\u224F","\\bumpeq",!0),i(s,d,p,"\u224E","\\Bumpeq",!0),i(s,d,p,"\u2267","\\geqq",!0),i(s,d,p,"\u2A7E","\\geqslant",!0),i(s,d,p,"\u2A96","\\eqslantgtr",!0),i(s,d,p,"\u2273","\\gtrsim",!0),i(s,d,p,"\u2A86","\\gtrapprox",!0),i(s,d,C,"\u22D7","\\gtrdot"),i(s,d,p,"\u22D9","\\ggg",!0),i(s,d,p,"\u2277","\\gtrless",!0),i(s,d,p,"\u22DB","\\gtreqless",!0),i(s,d,p,"\u2A8C","\\gtreqqless",!0),i(s,d,p,"\u2256","\\eqcirc",!0),i(s,d,p,"\u2257","\\circeq",!0),i(s,d,p,"\u225C","\\triangleq",!0),i(s,d,p,"\u223C","\\thicksim"),i(s,d,p,"\u2248","\\thickapprox"),i(s,d,p,"\u2AC6","\\supseteqq",!0),i(s,d,p,"\u22D1","\\Supset",!0),i(s,d,p,"\u2290","\\sqsupset",!0),i(s,d,p,"\u227D","\\succcurlyeq",!0),i(s,d,p,"\u22DF","\\curlyeqsucc",!0),i(s,d,p,"\u227F","\\succsim",!0),i(s,d,p,"\u2AB8","\\succapprox",!0),i(s,d,p,"\u22B3","\\vartriangleright"),i(s,d,p,"\u22B5","\\trianglerighteq"),i(s,d,p,"\u22A9","\\Vdash",!0),i(s,d,p,"\u2223","\\shortmid"),i(s,d,p,"\u2225","\\shortparallel"),i(s,d,p,"\u226C","\\between",!0),i(s,d,p,"\u22D4","\\pitchfork",!0),i(s,d,p,"\u221D","\\varpropto"),i(s,d,p,"\u25C0","\\blacktriangleleft"),i(s,d,p,"\u2234","\\therefore",!0),i(s,d,p,"\u220D","\\backepsilon"),i(s,d,p,"\u25B6","\\blacktriangleright"),i(s,d,p,"\u2235","\\because",!0),i(s,d,p,"\u22D8","\\llless"),i(s,d,p,"\u22D9","\\gggtr"),i(s,d,C,"\u22B2","\\lhd"),i(s,d,C,"\u22B3","\\rhd"),i(s,d,p,"\u2242","\\eqsim",!0),i(s,c,p,"\u22C8","\\Join"),i(s,d,p,"\u2251","\\Doteq",!0),i(s,d,C,"\u2214","\\dotplus",!0),i(s,d,C,"\u2216","\\smallsetminus"),i(s,d,C,"\u22D2","\\Cap",!0),i(s,d,C,"\u22D3","\\Cup",!0),i(s,d,C,"\u2A5E","\\doublebarwedge",!0),i(s,d,C,"\u229F","\\boxminus",!0),i(s,d,C,"\u229E","\\boxplus",!0),i(s,d,C,"\u22C7","\\divideontimes",!0),i(s,d,C,"\u22C9","\\ltimes",!0),i(s,d,C,"\u22CA","\\rtimes",!0),i(s,d,C,"\u22CB","\\leftthreetimes",!0),i(s,d,C,"\u22CC","\\rightthreetimes",!0),i(s,d,C,"\u22CF","\\curlywedge",!0),i(s,d,C,"\u22CE","\\curlyvee",!0),i(s,d,C,"\u229D","\\circleddash",!0),i(s,d,C,"\u229B","\\circledast",!0),i(s,d,C,"\u22C5","\\centerdot"),i(s,d,C,"\u22BA","\\intercal",!0),i(s,d,C,"\u22D2","\\doublecap"),i(s,d,C,"\u22D3","\\doublecup"),i(s,d,C,"\u22A0","\\boxtimes",!0),i(s,d,p,"\u21E2","\\dashrightarrow",!0),i(s,d,p,"\u21E0","\\dashleftarrow",!0),i(s,d,p,"\u21C7","\\leftleftarrows",!0),i(s,d,p,"\u21C6","\\leftrightarrows",!0),i(s,d,p,"\u21DA","\\Lleftarrow",!0),i(s,d,p,"\u219E","\\twoheadleftarrow",!0),i(s,d,p,"\u21A2","\\leftarrowtail",!0),i(s,d,p,"\u21AB","\\looparrowleft",!0),i(s,d,p,"\u21CB","\\leftrightharpoons",!0),i(s,d,p,"\u21B6","\\curvearrowleft",!0),i(s,d,p,"\u21BA","\\circlearrowleft",!0),i(s,d,p,"\u21B0","\\Lsh",!0),i(s,d,p,"\u21C8","\\upuparrows",!0),i(s,d,p,"\u21BF","\\upharpoonleft",!0),i(s,d,p,"\u21C3","\\downharpoonleft",!0),i(s,c,p,"\u22B6","\\origof",!0),i(s,c,p,"\u22B7","\\imageof",!0),i(s,d,p,"\u22B8","\\multimap",!0),i(s,d,p,"\u21AD","\\leftrightsquigarrow",!0),i(s,d,p,"\u21C9","\\rightrightarrows",!0),i(s,d,p,"\u21C4","\\rightleftarrows",!0),i(s,d,p,"\u21A0","\\twoheadrightarrow",!0),i(s,d,p,"\u21A3","\\rightarrowtail",!0),i(s,d,p,"\u21AC","\\looparrowright",!0),i(s,d,p,"\u21B7","\\curvearrowright",!0),i(s,d,p,"\u21BB","\\circlearrowright",!0),i(s,d,p,"\u21B1","\\Rsh",!0),i(s,d,p,"\u21CA","\\downdownarrows",!0),i(s,d,p,"\u21BE","\\upharpoonright",!0),i(s,d,p,"\u21C2","\\downharpoonright",!0),i(s,d,p,"\u21DD","\\rightsquigarrow",!0),i(s,d,p,"\u21DD","\\leadsto"),i(s,d,p,"\u21DB","\\Rrightarrow",!0),i(s,d,p,"\u21BE","\\restriction"),i(s,c,g,"\u2018","`"),i(s,c,g,"$","\\$"),i(k,c,g,"$","\\$"),i(k,c,g,"$","\\textdollar"),i(s,c,g,"%","\\%"),i(k,c,g,"%","\\%"),i(s,c,g,"_","\\_"),i(k,c,g,"_","\\_"),i(k,c,g,"_","\\textunderscore"),i(s,c,g,"\u2220","\\angle",!0),i(s,c,g,"\u221E","\\infty",!0),i(s,c,g,"\u2032","\\prime"),i(s,c,g,"\u25B3","\\triangle"),i(s,c,g,"\u0393","\\Gamma",!0),i(s,c,g,"\u0394","\\Delta",!0),i(s,c,g,"\u0398","\\Theta",!0),i(s,c,g,"\u039B","\\Lambda",!0),i(s,c,g,"\u039E","\\Xi",!0),i(s,c,g,"\u03A0","\\Pi",!0),i(s,c,g,"\u03A3","\\Sigma",!0),i(s,c,g,"\u03A5","\\Upsilon",!0),i(s,c,g,"\u03A6","\\Phi",!0),i(s,c,g,"\u03A8","\\Psi",!0),i(s,c,g,"\u03A9","\\Omega",!0),i(s,c,g,"A","\u0391"),i(s,c,g,"B","\u0392"),i(s,c,g,"E","\u0395"),i(s,c,g,"Z","\u0396"),i(s,c,g,"H","\u0397"),i(s,c,g,"I","\u0399"),i(s,c,g,"K","\u039A"),i(s,c,g,"M","\u039C"),i(s,c,g,"N","\u039D"),i(s,c,g,"O","\u039F"),i(s,c,g,"P","\u03A1"),i(s,c,g,"T","\u03A4"),i(s,c,g,"X","\u03A7"),i(s,c,g,"\xAC","\\neg",!0),i(s,c,g,"\xAC","\\lnot"),i(s,c,g,"\u22A4","\\top"),i(s,c,g,"\u22A5","\\bot"),i(s,c,g,"\u2205","\\emptyset"),i(s,d,g,"\u2205","\\varnothing"),i(s,c,R,"\u03B1","\\alpha",!0),i(s,c,R,"\u03B2","\\beta",!0),i(s,c,R,"\u03B3","\\gamma",!0),i(s,c,R,"\u03B4","\\delta",!0),i(s,c,R,"\u03F5","\\epsilon",!0),i(s,c,R,"\u03B6","\\zeta",!0),i(s,c,R,"\u03B7","\\eta",!0),i(s,c,R,"\u03B8","\\theta",!0),i(s,c,R,"\u03B9","\\iota",!0),i(s,c,R,"\u03BA","\\kappa",!0),i(s,c,R,"\u03BB","\\lambda",!0),i(s,c,R,"\u03BC","\\mu",!0),i(s,c,R,"\u03BD","\\nu",!0),i(s,c,R,"\u03BE","\\xi",!0),i(s,c,R,"\u03BF","\\omicron",!0),i(s,c,R,"\u03C0","\\pi",!0),i(s,c,R,"\u03C1","\\rho",!0),i(s,c,R,"\u03C3","\\sigma",!0),i(s,c,R,"\u03C4","\\tau",!0),i(s,c,R,"\u03C5","\\upsilon",!0),i(s,c,R,"\u03D5","\\phi",!0),i(s,c,R,"\u03C7","\\chi",!0),i(s,c,R,"\u03C8","\\psi",!0),i(s,c,R,"\u03C9","\\omega",!0),i(s,c,R,"\u03B5","\\varepsilon",!0),i(s,c,R,"\u03D1","\\vartheta",!0),i(s,c,R,"\u03D6","\\varpi",!0),i(s,c,R,"\u03F1","\\varrho",!0),i(s,c,R,"\u03C2","\\varsigma",!0),i(s,c,R,"\u03C6","\\varphi",!0),i(s,c,C,"\u2217","*"),i(s,c,C,"+","+"),i(s,c,C,"\u2212","-"),i(s,c,C,"\u22C5","\\cdot",!0),i(s,c,C,"\u2218","\\circ"),i(s,c,C,"\xF7","\\div",!0),i(s,c,C,"\xB1","\\pm",!0),i(s,c,C,"\xD7","\\times",!0),i(s,c,C,"\u2229","\\cap",!0),i(s,c,C,"\u222A","\\cup",!0),i(s,c,C,"\u2216","\\setminus"),i(s,c,C,"\u2227","\\land"),i(s,c,C,"\u2228","\\lor"),i(s,c,C,"\u2227","\\wedge",!0),i(s,c,C,"\u2228","\\vee",!0),i(s,c,g,"\u221A","\\surd"),i(s,c,me,"\u27E8","\\langle",!0),i(s,c,me,"\u2223","\\lvert"),i(s,c,me,"\u2225","\\lVert"),i(s,c,se,"?","?"),i(s,c,se,"!","!"),i(s,c,se,"\u27E9","\\rangle",!0),i(s,c,se,"\u2223","\\rvert"),i(s,c,se,"\u2225","\\rVert"),i(s,c,p,"=","="),i(s,c,p,":",":"),i(s,c,p,"\u2248","\\approx",!0),i(s,c,p,"\u2245","\\cong",!0),i(s,c,p,"\u2265","\\ge"),i(s,c,p,"\u2265","\\geq",!0),i(s,c,p,"\u2190","\\gets"),i(s,c,p,">","\\gt",!0),i(s,c,p,"\u2208","\\in",!0),i(s,c,p,"\uE020","\\@not"),i(s,c,p,"\u2282","\\subset",!0),i(s,c,p,"\u2283","\\supset",!0),i(s,c,p,"\u2286","\\subseteq",!0),i(s,c,p,"\u2287","\\supseteq",!0),i(s,d,p,"\u2288","\\nsubseteq",!0),i(s,d,p,"\u2289","\\nsupseteq",!0),i(s,c,p,"\u22A8","\\models"),i(s,c,p,"\u2190","\\leftarrow",!0),i(s,c,p,"\u2264","\\le"),i(s,c,p,"\u2264","\\leq",!0),i(s,c,p,"<","\\lt",!0),i(s,c,p,"\u2192","\\rightarrow",!0),i(s,c,p,"\u2192","\\to"),i(s,d,p,"\u2271","\\ngeq",!0),i(s,d,p,"\u2270","\\nleq",!0),i(s,c,we,"\xA0","\\ "),i(s,c,we,"\xA0","~"),i(s,c,we,"\xA0","\\space"),i(s,c,we,"\xA0","\\nobreakspace"),i(k,c,we,"\xA0","\\ "),i(k,c,we,"\xA0"," "),i(k,c,we,"\xA0","~"),i(k,c,we,"\xA0","\\space"),i(k,c,we,"\xA0","\\nobreakspace"),i(s,c,we,null,"\\nobreak"),i(s,c,we,null,"\\allowbreak"),i(s,c,Ct,",",","),i(s,c,Ct,";",";"),i(s,d,C,"\u22BC","\\barwedge",!0),i(s,d,C,"\u22BB","\\veebar",!0),i(s,c,C,"\u2299","\\odot",!0),i(s,c,C,"\u2295","\\oplus",!0),i(s,c,C,"\u2297","\\otimes",!0),i(s,c,g,"\u2202","\\partial",!0),i(s,c,C,"\u2298","\\oslash",!0),i(s,d,C,"\u229A","\\circledcirc",!0),i(s,d,C,"\u22A1","\\boxdot",!0),i(s,c,C,"\u25B3","\\bigtriangleup"),i(s,c,C,"\u25BD","\\bigtriangledown"),i(s,c,C,"\u2020","\\dagger"),i(s,c,C,"\u22C4","\\diamond"),i(s,c,C,"\u22C6","\\star"),i(s,c,C,"\u25C3","\\triangleleft"),i(s,c,C,"\u25B9","\\triangleright"),i(s,c,me,"{","\\{"),i(k,c,g,"{","\\{"),i(k,c,g,"{","\\textbraceleft"),i(s,c,se,"}","\\}"),i(k,c,g,"}","\\}"),i(k,c,g,"}","\\textbraceright"),i(s,c,me,"{","\\lbrace"),i(s,c,se,"}","\\rbrace"),i(s,c,me,"[","\\lbrack",!0),i(k,c,g,"[","\\lbrack",!0),i(s,c,se,"]","\\rbrack",!0),i(k,c,g,"]","\\rbrack",!0),i(s,c,me,"(","\\lparen",!0),i(s,c,se,")","\\rparen",!0),i(k,c,g,"<","\\textless",!0),i(k,c,g,">","\\textgreater",!0),i(s,c,me,"\u230A","\\lfloor",!0),i(s,c,se,"\u230B","\\rfloor",!0),i(s,c,me,"\u2308","\\lceil",!0),i(s,c,se,"\u2309","\\rceil",!0),i(s,c,g,"\\","\\backslash"),i(s,c,g,"\u2223","|"),i(s,c,g,"\u2223","\\vert"),i(k,c,g,"|","\\textbar",!0),i(s,c,g,"\u2225","\\|"),i(s,c,g,"\u2225","\\Vert"),i(k,c,g,"\u2225","\\textbardbl"),i(k,c,g,"~","\\textasciitilde"),i(k,c,g,"\\","\\textbackslash"),i(k,c,g,"^","\\textasciicircum"),i(s,c,p,"\u2191","\\uparrow",!0),i(s,c,p,"\u21D1","\\Uparrow",!0),i(s,c,p,"\u2193","\\downarrow",!0),i(s,c,p,"\u21D3","\\Downarrow",!0),i(s,c,p,"\u2195","\\updownarrow",!0),i(s,c,p,"\u21D5","\\Updownarrow",!0),i(s,c,re,"\u2210","\\coprod"),i(s,c,re,"\u22C1","\\bigvee"),i(s,c,re,"\u22C0","\\bigwedge"),i(s,c,re,"\u2A04","\\biguplus"),i(s,c,re,"\u22C2","\\bigcap"),i(s,c,re,"\u22C3","\\bigcup"),i(s,c,re,"\u222B","\\int"),i(s,c,re,"\u222B","\\intop"),i(s,c,re,"\u222C","\\iint"),i(s,c,re,"\u222D","\\iiint"),i(s,c,re,"\u220F","\\prod"),i(s,c,re,"\u2211","\\sum"),i(s,c,re,"\u2A02","\\bigotimes"),i(s,c,re,"\u2A01","\\bigoplus"),i(s,c,re,"\u2A00","\\bigodot"),i(s,c,re,"\u222E","\\oint"),i(s,c,re,"\u222F","\\oiint"),i(s,c,re,"\u2230","\\oiiint"),i(s,c,re,"\u2A06","\\bigsqcup"),i(s,c,re,"\u222B","\\smallint"),i(k,c,ot,"\u2026","\\textellipsis"),i(s,c,ot,"\u2026","\\mathellipsis"),i(k,c,ot,"\u2026","\\ldots",!0),i(s,c,ot,"\u2026","\\ldots",!0),i(s,c,ot,"\u22EF","\\@cdots",!0),i(s,c,ot,"\u22F1","\\ddots",!0),i(s,c,g,"\u22EE","\\varvdots"),i(s,c,Q,"\u02CA","\\acute"),i(s,c,Q,"\u02CB","\\grave"),i(s,c,Q,"\xA8","\\ddot"),i(s,c,Q,"~","\\tilde"),i(s,c,Q,"\u02C9","\\bar"),i(s,c,Q,"\u02D8","\\breve"),i(s,c,Q,"\u02C7","\\check"),i(s,c,Q,"^","\\hat"),i(s,c,Q,"\u20D7","\\vec"),i(s,c,Q,"\u02D9","\\dot"),i(s,c,Q,"\u02DA","\\mathring"),i(s,c,R,"\uE131","\\@imath"),i(s,c,R,"\uE237","\\@jmath"),i(s,c,g,"\u0131","\u0131"),i(s,c,g,"\u0237","\u0237"),i(k,c,g,"\u0131","\\i",!0),i(k,c,g,"\u0237","\\j",!0),i(k,c,g,"\xDF","\\ss",!0),i(k,c,g,"\xE6","\\ae",!0),i(k,c,g,"\u0153","\\oe",!0),i(k,c,g,"\xF8","\\o",!0),i(k,c,g,"\xC6","\\AE",!0),i(k,c,g,"\u0152","\\OE",!0),i(k,c,g,"\xD8","\\O",!0),i(k,c,Q,"\u02CA","\\'"),i(k,c,Q,"\u02CB","\\`"),i(k,c,Q,"\u02C6","\\^"),i(k,c,Q,"\u02DC","\\~"),i(k,c,Q,"\u02C9","\\="),i(k,c,Q,"\u02D8","\\u"),i(k,c,Q,"\u02D9","\\."),i(k,c,Q,"\u02DA","\\r"),i(k,c,Q,"\u02C7","\\v"),i(k,c,Q,"\xA8",'\\"'),i(k,c,Q,"\u02DD","\\H"),i(k,c,Q,"\u25EF","\\textcircled");var er={"--":!0,"---":!0,"``":!0,"''":!0};i(k,c,g,"\u2013","--",!0),i(k,c,g,"\u2013","\\textendash"),i(k,c,g,"\u2014","---",!0),i(k,c,g,"\u2014","\\textemdash"),i(k,c,g,"\u2018","`",!0),i(k,c,g,"\u2018","\\textquoteleft"),i(k,c,g,"\u2019","'",!0),i(k,c,g,"\u2019","\\textquoteright"),i(k,c,g,"\u201C","``",!0),i(k,c,g,"\u201C","\\textquotedblleft"),i(k,c,g,"\u201D","''",!0),i(k,c,g,"\u201D","\\textquotedblright"),i(s,c,g,"\xB0","\\degree",!0),i(k,c,g,"\xB0","\\degree"),i(k,c,g,"\xB0","\\textdegree",!0),i(s,c,g,"\xA3","\\pounds"),i(s,c,g,"\xA3","\\mathsterling",!0),i(k,c,g,"\xA3","\\pounds"),i(k,c,g,"\xA3","\\textsterling",!0),i(s,d,g,"\u2720","\\maltese"),i(k,d,g,"\u2720","\\maltese");for(var tr='0123456789/@."',t0=0;t0<tr.length;t0++){var rr=tr.charAt(t0);i(s,c,g,rr,rr)}for(var nr='0123456789!@*()-=+";:?/.,',r0=0;r0<nr.length;r0++){var or=nr.charAt(r0);i(k,c,g,or,or)}for(var Bt="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",n0=0;n0<Bt.length;n0++){var Nt=Bt.charAt(n0);i(s,c,R,Nt,Nt),i(k,c,g,Nt,Nt)}i(s,d,g,"C","\u2102"),i(k,d,g,"C","\u2102"),i(s,d,g,"H","\u210D"),i(k,d,g,"H","\u210D"),i(s,d,g,"N","\u2115"),i(k,d,g,"N","\u2115"),i(s,d,g,"P","\u2119"),i(k,d,g,"P","\u2119"),i(s,d,g,"Q","\u211A"),i(k,d,g,"Q","\u211A"),i(s,d,g,"R","\u211D"),i(k,d,g,"R","\u211D"),i(s,d,g,"Z","\u2124"),i(k,d,g,"Z","\u2124"),i(s,c,R,"h","\u210E"),i(k,c,R,"h","\u210E");for(var H="",le=0;le<Bt.length;le++){var ne=Bt.charAt(le);H=String.fromCharCode(55349,56320+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56372+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56424+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56580+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56736+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56788+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56840+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56944+le),i(s,c,R,ne,H),i(k,c,g,ne,H),le<26&&(H=String.fromCharCode(55349,56632+le),i(s,c,R,ne,H),i(k,c,g,ne,H),H=String.fromCharCode(55349,56476+le),i(s,c,R,ne,H),i(k,c,g,ne,H))}H=String.fromCharCode(55349,56668),i(s,c,R,"k",H),i(k,c,g,"k",H);for(var Ze=0;Ze<10;Ze++){var Fe=Ze.toString();H=String.fromCharCode(55349,57294+Ze),i(s,c,R,Fe,H),i(k,c,g,Fe,H),H=String.fromCharCode(55349,57314+Ze),i(s,c,R,Fe,H),i(k,c,g,Fe,H),H=String.fromCharCode(55349,57324+Ze),i(s,c,R,Fe,H),i(k,c,g,Fe,H),H=String.fromCharCode(55349,57334+Ze),i(s,c,R,Fe,H),i(k,c,g,Fe,H)}for(var o0="\xC7\xD0\xDE\xE7\xFE",i0=0;i0<o0.length;i0++){var qt=o0.charAt(i0);i(s,c,R,qt,qt),i(k,c,g,qt,qt)}var Lt=[["mathbf","textbf","Main-Bold"],["mathbf","textbf","Main-Bold"],["mathnormal","textit","Math-Italic"],["mathnormal","textit","Math-Italic"],["boldsymbol","boldsymbol","Main-BoldItalic"],["boldsymbol","boldsymbol","Main-BoldItalic"],["mathscr","textscr","Script-Regular"],["","",""],["","",""],["","",""],["mathfrak","textfrak","Fraktur-Regular"],["mathfrak","textfrak","Fraktur-Regular"],["mathbb","textbb","AMS-Regular"],["mathbb","textbb","AMS-Regular"],["","",""],["","",""],["mathsf","textsf","SansSerif-Regular"],["mathsf","textsf","SansSerif-Regular"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathitsf","textitsf","SansSerif-Italic"],["mathitsf","textitsf","SansSerif-Italic"],["","",""],["","",""],["mathtt","texttt","Typewriter-Regular"],["mathtt","texttt","Typewriter-Regular"]],ir=[["mathbf","textbf","Main-Bold"],["","",""],["mathsf","textsf","SansSerif-Regular"],["mathboldsf","textboldsf","SansSerif-Bold"],["mathtt","texttt","Typewriter-Regular"]],wo=function(e,t){var r=e.charCodeAt(0),n=e.charCodeAt(1),o=(r-55296)*1024+(n-56320)+65536,a=t==="math"?0:1;if(119808<=o&&o<120484){var l=Math.floor((o-119808)/26);return[Lt[l][2],Lt[l][a]]}else if(120782<=o&&o<=120831){var m=Math.floor((o-120782)/10);return[ir[m][2],ir[m][a]]}else{if(o===120485||o===120486)return[Lt[0][2],Lt[0][a]];if(120486<o&&o<120782)return["",""];throw new v("Unsupported character: "+e)}},ko=[[1,1,1],[2,1,1],[3,1,1],[4,2,1],[5,2,1],[6,3,1],[7,4,2],[8,6,3],[9,7,6],[10,8,7],[11,10,9]],ar=[.5,.6,.7,.8,.9,1,1.2,1.44,1.728,2.074,2.488],sr=function(e,t){return t.size<2?e:ko[e-1][t.size-1]},lr=function(){function e(r){this.style=void 0,this.color=void 0,this.size=void 0,this.textSize=void 0,this.phantom=void 0,this.font=void 0,this.fontFamily=void 0,this.fontWeight=void 0,this.fontShape=void 0,this.sizeMultiplier=void 0,this.maxSize=void 0,this.minRuleThickness=void 0,this._fontMetrics=void 0,this.style=r.style,this.color=r.color,this.size=r.size||e.BASESIZE,this.textSize=r.textSize||this.size,this.phantom=!!r.phantom,this.font=r.font||"",this.fontFamily=r.fontFamily||"",this.fontWeight=r.fontWeight||"",this.fontShape=r.fontShape||"",this.sizeMultiplier=ar[this.size-1],this.maxSize=r.maxSize,this.minRuleThickness=r.minRuleThickness,this._fontMetrics=void 0}var t=e.prototype;return t.extend=function(r){var n={style:this.style,size:this.size,textSize:this.textSize,color:this.color,phantom:this.phantom,font:this.font,fontFamily:this.fontFamily,fontWeight:this.fontWeight,fontShape:this.fontShape,maxSize:this.maxSize,minRuleThickness:this.minRuleThickness};for(var o in r)r.hasOwnProperty(o)&&(n[o]=r[o]);return new e(n)},t.havingStyle=function(r){return this.style===r?this:this.extend({style:r,size:sr(this.textSize,r)})},t.havingCrampedStyle=function(){return this.havingStyle(this.style.cramp())},t.havingSize=function(r){return this.size===r&&this.textSize===r?this:this.extend({style:this.style.text(),size:r,textSize:r,sizeMultiplier:ar[r-1]})},t.havingBaseStyle=function(r){r=r||this.style.text();var n=sr(e.BASESIZE,r);return this.size===n&&this.textSize===e.BASESIZE&&this.style===r?this:this.extend({style:r,size:n})},t.havingBaseSizing=function(){var r;switch(this.style.id){case 4:case 5:r=3;break;case 6:case 7:r=1;break;default:r=6}return this.extend({style:this.style.text(),size:r})},t.withColor=function(r){return this.extend({color:r})},t.withPhantom=function(){return this.extend({phantom:!0})},t.withFont=function(r){return this.extend({font:r})},t.withTextFontFamily=function(r){return this.extend({fontFamily:r,font:""})},t.withTextFontWeight=function(r){return this.extend({fontWeight:r,font:""})},t.withTextFontShape=function(r){return this.extend({fontShape:r,font:""})},t.sizingClasses=function(r){return r.size!==this.size?["sizing","reset-size"+r.size,"size"+this.size]:[]},t.baseSizingClasses=function(){return this.size!==e.BASESIZE?["sizing","reset-size"+this.size,"size"+e.BASESIZE]:[]},t.fontMetrics=function(){return this._fontMetrics||(this._fontMetrics=bo(this.size)),this._fontMetrics},t.getColor=function(){return this.phantom?"transparent":this.color},e}();lr.BASESIZE=6;var So=lr,a0={pt:1,mm:7227/2540,cm:7227/254,in:72.27,bp:803/800,pc:12,dd:1238/1157,cc:14856/1157,nd:685/642,nc:1370/107,sp:1/65536,px:803/800},zo={ex:!0,em:!0,mu:!0},cr=function(e){return typeof e!="string"&&(e=e.unit),e in a0||e in zo||e==="ex"},J=function(e,t){var r;if(e.unit in a0)r=a0[e.unit]/t.fontMetrics().ptPerEm/t.sizeMultiplier;else if(e.unit==="mu")r=t.fontMetrics().cssEmPerMu;else{var n;if(t.style.isTight()?n=t.havingStyle(t.style.text()):n=t,e.unit==="ex")r=n.fontMetrics().xHeight;else if(e.unit==="em")r=n.fontMetrics().quad;else throw new v("Invalid unit: '"+e.unit+"'");n!==t&&(r*=n.sizeMultiplier/t.sizeMultiplier)}return Math.min(e.number*r,t.maxSize)},Rt=function(e,t,r){return K[r][e]&&K[r][e].replace&&(e=K[r][e].replace),{value:e,metrics:Jt(e,t,r)}},ke=function(e,t,r,n,o){var a=Rt(e,t,r),l=a.metrics;e=a.value;var m;if(l){var u=l.italic;(r==="text"||n&&n.font==="mathit")&&(u=0),m=new ge(e,l.height,l.depth,u,l.skew,l.width,o)}else typeof console<"u"&&console.warn("No character metrics "+("for '"+e+"' in style '"+t+"' and mode '"+r+"'")),m=new ge(e,0,0,0,0,0,o);if(n){m.maxFontSize=n.sizeMultiplier,n.style.isTight()&&m.classes.push("mtight");var f=n.getColor();f&&(m.style.color=f)}return m},Ao=function(e,t,r,n){return n===void 0&&(n=[]),r.font==="boldsymbol"&&Rt(e,"Main-Bold",t).metrics?ke(e,"Main-Bold",t,r,n.concat(["mathbf"])):e==="\\"||K[t][e].font==="main"?ke(e,"Main-Regular",t,r,n):ke(e,"AMS-Regular",t,r,n.concat(["amsrm"]))},Mo=function(e,t,r,n,o){return o!=="textord"&&Rt(e,"Math-BoldItalic",t).metrics?{fontName:"Math-BoldItalic",fontClass:"boldsymbol"}:{fontName:"Main-Bold",fontClass:"mathbf"}},To=function(e,t,r){var n=e.mode,o=e.text,a=["mord"],l=n==="math"||n==="text"&&t.font,m=l?t.font:t.fontFamily;if(o.charCodeAt(0)===55349){var u=wo(o,n),f=u[0],x=u[1];return ke(o,f,n,t,a.concat(x))}else if(m){var y,S;if(m==="boldsymbol"){var w=Mo(o,n,t,a,r);y=w.fontName,S=[w.fontClass]}else l?(y=ur[m].fontName,S=[m]):(y=Dt(m,t.fontWeight,t.fontShape),S=[m,t.fontWeight,t.fontShape]);if(Rt(o,y,n).metrics)return ke(o,y,n,t,a.concat(S));if(er.hasOwnProperty(o)&&y.substr(0,10)==="Typewriter"){for(var A=[],M=0;M<o.length;M++)A.push(ke(o[M],y,n,t,a.concat(S)));return hr(A)}}if(r==="mathord")return ke(o,"Math-Italic",n,t,a.concat(["mathnormal"]));if(r==="textord"){var B=K[n][o]&&K[n][o].font;if(B==="ams"){var q=Dt("amsrm",t.fontWeight,t.fontShape);return ke(o,q,n,t,a.concat("amsrm",t.fontWeight,t.fontShape))}else if(B==="main"||!B){var I=Dt("textrm",t.fontWeight,t.fontShape);return ke(o,I,n,t,a.concat(t.fontWeight,t.fontShape))}else{var W=Dt(B,t.fontWeight,t.fontShape);return ke(o,W,n,t,a.concat(W,t.fontWeight,t.fontShape))}}else throw new Error("unexpected type: "+r+" in makeOrd")},Co=function(e,t){if(Pe(e.classes)!==Pe(t.classes)||e.skew!==t.skew||e.maxFontSize!==t.maxFontSize)return!1;if(e.classes.length===1){var r=e.classes[0];if(r==="mbin"||r==="mord")return!1}for(var n in e.style)if(e.style.hasOwnProperty(n)&&e.style[n]!==t.style[n])return!1;for(var o in t.style)if(t.style.hasOwnProperty(o)&&e.style[o]!==t.style[o])return!1;return!0},Bo=function(e){for(var t=0;t<e.length-1;t++){var r=e[t],n=e[t+1];r instanceof ge&&n instanceof ge&&Co(r,n)&&(r.text+=n.text,r.height=Math.max(r.height,n.height),r.depth=Math.max(r.depth,n.depth),r.italic=n.italic,e.splice(t+1,1),t--)}return e},s0=function(e){for(var t=0,r=0,n=0,o=0;o<e.children.length;o++){var a=e.children[o];a.height>t&&(t=a.height),a.depth>r&&(r=a.depth),a.maxFontSize>n&&(n=a.maxFontSize)}e.height=t,e.depth=r,e.maxFontSize=n},ce=function(e,t,r,n){var o=new pt(e,t,r,n);return s0(o),o},mr=function(e,t,r,n){return new pt(e,t,r,n)},No=function(e,t,r){var n=ce([e],[],t);return n.height=Math.max(r||t.fontMetrics().defaultRuleThickness,t.minRuleThickness),n.style.borderBottomWidth=n.height+"em",n.maxFontSize=1,n},qo=function(e,t,r,n){var o=new Kt(e,t,r,n);return s0(o),o},hr=function(e){var t=new dt(e);return s0(t),t},Lo=function(e,t){return e instanceof dt?ce([],[e],t):e},Ro=function(e){if(e.positionType==="individualShift"){for(var t=e.children,r=[t[0]],n=-t[0].shift-t[0].elem.depth,o=n,a=1;a<t.length;a++){var l=-t[a].shift-o-t[a].elem.depth,m=l-(t[a-1].elem.height+t[a-1].elem.depth);o=o+l,r.push({type:"kern",size:m}),r.push(t[a])}return{children:r,depth:n}}var u;if(e.positionType==="top"){for(var f=e.positionData,x=0;x<e.children.length;x++){var y=e.children[x];f-=y.type==="kern"?y.size:y.elem.height+y.elem.depth}u=f}else if(e.positionType==="bottom")u=-e.positionData;else{var S=e.children[0];if(S.type!=="elem")throw new Error('First child must have type "elem".');if(e.positionType==="shift")u=-S.elem.depth-e.positionData;else if(e.positionType==="firstBaseline")u=-S.elem.depth;else throw new Error("Invalid positionType "+e.positionType+".")}return{children:e.children,depth:u}},Do=function(e,t){for(var r=Ro(e),n=r.children,o=r.depth,a=0,l=0;l<n.length;l++){var m=n[l];if(m.type==="elem"){var u=m.elem;a=Math.max(a,u.maxFontSize,u.height)}}a+=2;var f=ce(["pstrut"],[]);f.style.height=a+"em";for(var x=[],y=o,S=o,w=o,A=0;A<n.length;A++){var M=n[A];if(M.type==="kern")w+=M.size;else{var B=M.elem,q=M.wrapperClasses||[],I=M.wrapperStyle||{},W=ce(q,[f,B],void 0,I);W.style.top=-a-w-B.depth+"em",M.marginLeft&&(W.style.marginLeft=M.marginLeft),M.marginRight&&(W.style.marginRight=M.marginRight),x.push(W),w+=B.height+B.depth}y=Math.min(y,w),S=Math.max(S,w)}var Y=ce(["vlist"],x);Y.style.height=S+"em";var Z;if(y<0){var G=ce([],[]),U=ce(["vlist"],[G]);U.style.height=-y+"em";var te=ce(["vlist-s"],[new ge("\u200B")]);Z=[ce(["vlist-r"],[Y,te]),ce(["vlist-r"],[U])]}else Z=[ce(["vlist-r"],[Y])];var j=ce(["vlist-t"],Z);return Z.length===2&&j.classes.push("vlist-t2"),j.height=S,j.depth=-y,j},Io=function(e,t){var r=ce(["mspace"],[],t),n=J(e,t);return r.style.marginRight=n+"em",r},Dt=function(e,t,r){var n="";switch(e){case"amsrm":n="AMS";break;case"textrm":n="Main";break;case"textsf":n="SansSerif";break;case"texttt":n="Typewriter";break;default:n=e}var o;return t==="textbf"&&r==="textit"?o="BoldItalic":t==="textbf"?o="Bold":t==="textit"?o="Italic":o="Regular",n+"-"+o},ur={mathbf:{variant:"bold",fontName:"Main-Bold"},mathrm:{variant:"normal",fontName:"Main-Regular"},textit:{variant:"italic",fontName:"Main-Italic"},mathit:{variant:"italic",fontName:"Main-Italic"},mathnormal:{variant:"italic",fontName:"Math-Italic"},mathbb:{variant:"double-struck",fontName:"AMS-Regular"},mathcal:{variant:"script",fontName:"Caligraphic-Regular"},mathfrak:{variant:"fraktur",fontName:"Fraktur-Regular"},mathscr:{variant:"script",fontName:"Script-Regular"},mathsf:{variant:"sans-serif",fontName:"SansSerif-Regular"},mathtt:{variant:"monospace",fontName:"Typewriter-Regular"}},dr={vec:["vec",.471,.714],oiintSize1:["oiintSize1",.957,.499],oiintSize2:["oiintSize2",1.472,.659],oiiintSize1:["oiiintSize1",1.304,.499],oiiintSize2:["oiiintSize2",1.98,.659]},Ho=function(e,t){var r=dr[e],n=r[0],o=r[1],a=r[2],l=new Xe(n),m=new Oe([l],{width:o+"em",height:a+"em",style:"width:"+o+"em",viewBox:"0 0 "+1e3*o+" "+1e3*a,preserveAspectRatio:"xMinYMin"}),u=mr(["overlay"],[m],t);return u.height=a,u.style.height=a+"em",u.style.width=o+"em",u},b={fontMap:ur,makeSymbol:ke,mathsym:Ao,makeSpan:ce,makeSvgSpan:mr,makeLineSpan:No,makeAnchor:qo,makeFragment:hr,wrapFragment:Lo,makeVList:Do,makeOrd:To,makeGlue:Io,staticSvg:Ho,svgData:dr,tryCombineChars:Bo},ee={number:3,unit:"mu"},Ke={number:4,unit:"mu"},Re={number:5,unit:"mu"},Eo={mord:{mop:ee,mbin:Ke,mrel:Re,minner:ee},mop:{mord:ee,mop:ee,mrel:Re,minner:ee},mbin:{mord:Ke,mop:Ke,mopen:Ke,minner:Ke},mrel:{mord:Re,mop:Re,mopen:Re,minner:Re},mopen:{},mclose:{mop:ee,mbin:Ke,mrel:Re,minner:ee},mpunct:{mord:ee,mop:ee,mrel:Re,mopen:ee,mclose:ee,mpunct:ee,minner:ee},minner:{mord:ee,mop:ee,mbin:Ke,mrel:Re,mopen:ee,mpunct:ee,minner:ee}},Po={mord:{mop:ee},mop:{mord:ee,mop:ee},mbin:{},mrel:{},mopen:{},mclose:{mop:ee},mpunct:{},minner:{mop:ee}},pr={},It={},Ht={};function T(e){for(var t=e.type,r=e.names,n=e.props,o=e.handler,a=e.htmlBuilder,l=e.mathmlBuilder,m={type:t,numArgs:n.numArgs,argTypes:n.argTypes,allowedInArgument:!!n.allowedInArgument,allowedInText:!!n.allowedInText,allowedInMath:n.allowedInMath===void 0?!0:n.allowedInMath,numOptionalArgs:n.numOptionalArgs||0,infix:!!n.infix,primitive:!!n.primitive,handler:o},u=0;u<r.length;++u)pr[r[u]]=m;t&&(a&&(It[t]=a),l&&(Ht[t]=l))}function Qe(e){var t=e.type,r=e.htmlBuilder,n=e.mathmlBuilder;T({type:t,names:[],props:{numArgs:0},handler:function(){throw new Error("Should never be called.")},htmlBuilder:r,mathmlBuilder:n})}var Et=function(e){return e.type==="ordgroup"&&e.body.length===1?e.body[0]:e},oe=function(e){return e.type==="ordgroup"?e.body:[e]},De=b.makeSpan,Oo=["leftmost","mbin","mopen","mrel","mop","mpunct"],Fo=["rightmost","mrel","mclose","mpunct"],Vo={display:D.DISPLAY,text:D.TEXT,script:D.SCRIPT,scriptscript:D.SCRIPTSCRIPT},Wo={mord:"mord",mop:"mop",mbin:"mbin",mrel:"mrel",mopen:"mopen",mclose:"mclose",mpunct:"mpunct",minner:"minner"},ie=function(e,t,r,n){n===void 0&&(n=[null,null]);for(var o=[],a=0;a<e.length;a++){var l=_(e[a],t);if(l instanceof dt){var m=l.children;o.push.apply(o,m)}else o.push(l)}if(b.tryCombineChars(o),!r)return o;var u=t;if(e.length===1){var f=e[0];f.type==="sizing"?u=t.havingSize(f.size):f.type==="styling"&&(u=t.havingStyle(Vo[f.style]))}var x=De([n[0]||"leftmost"],[],t),y=De([n[1]||"rightmost"],[],t),S=r==="root";return fr(o,function(w,A){var M=A.classes[0],B=w.classes[0];M==="mbin"&&N.contains(Fo,B)?A.classes[0]="mord":B==="mbin"&&N.contains(Oo,M)&&(w.classes[0]="mord")},{node:x},y,S),fr(o,function(w,A){var M=l0(A),B=l0(w),q=M&&B?w.hasClass("mtight")?Po[M][B]:Eo[M][B]:null;if(q)return b.makeGlue(q,u)},{node:x},y,S),o},fr=function e(t,r,n,o,a){o&&t.push(o);for(var l=0;l<t.length;l++){var m=t[l],u=gr(m);if(u){e(u.children,r,n,null,a);continue}var f=!m.hasClass("mspace");if(f){var x=r(m,n.node);x&&(n.insertAfter?n.insertAfter(x):(t.unshift(x),l++))}f?n.node=m:a&&m.hasClass("newline")&&(n.node=De(["leftmost"])),n.insertAfter=function(y){return function(S){t.splice(y+1,0,S),l++}}(l)}o&&t.pop()},gr=function(e){return e instanceof dt||e instanceof Kt||e instanceof pt&&e.hasClass("enclosing")?e:null},_o=function e(t,r){var n=gr(t);if(n){var o=n.children;if(o.length){if(r==="right")return e(o[o.length-1],"right");if(r==="left")return e(o[0],"left")}}return t},l0=function(e,t){return e?(t&&(e=_o(e,t)),Wo[e.classes[0]]||null):null},ft=function(e,t){var r=["nulldelimiter"].concat(e.baseSizingClasses());return De(t.concat(r))},_=function(e,t,r){if(!e)return De();if(It[e.type]){var n=It[e.type](e,t);if(r&&t.size!==r.size){n=De(t.sizingClasses(r),[n],t);var o=t.sizeMultiplier/r.sizeMultiplier;n.height*=o,n.depth*=o}return n}else throw new v("Got group of unknown type: '"+e.type+"'")};function Pt(e,t){var r=De(["base"],e,t),n=De(["strut"]);return n.style.height=r.height+r.depth+"em",n.style.verticalAlign=-r.depth+"em",r.children.unshift(n),r}function c0(e,t){var r=null;e.length===1&&e[0].type==="tag"&&(r=e[0].tag,e=e[0].body);var n=ie(e,t,"root"),o;n.length===2&&n[1].hasClass("tag")&&(o=n.pop());for(var a=[],l=[],m=0;m<n.length;m++)if(l.push(n[m]),n[m].hasClass("mbin")||n[m].hasClass("mrel")||n[m].hasClass("allowbreak")){for(var u=!1;m<n.length-1&&n[m+1].hasClass("mspace")&&!n[m+1].hasClass("newline");)m++,l.push(n[m]),n[m].hasClass("nobreak")&&(u=!0);u||(a.push(Pt(l,t)),l=[])}else n[m].hasClass("newline")&&(l.pop(),l.length>0&&(a.push(Pt(l,t)),l=[]),a.push(n[m]));l.length>0&&a.push(Pt(l,t));var f;r?(f=Pt(ie(r,t,!0)),f.classes=["tag"],a.push(f)):o&&a.push(o);var x=De(["katex-html"],a);if(x.setAttribute("aria-hidden","true"),f){var y=f.children[0];y.style.height=x.height+x.depth+"em",y.style.verticalAlign=-x.depth+"em"}return x}function xr(e){return new dt(e)}var xe=function(){function e(r,n,o){this.type=void 0,this.attributes=void 0,this.children=void 0,this.classes=void 0,this.type=r,this.attributes={},this.children=n||[],this.classes=o||[]}var t=e.prototype;return t.setAttribute=function(r,n){this.attributes[r]=n},t.getAttribute=function(r){return this.attributes[r]},t.toNode=function(){var r=document.createElementNS("http://www.w3.org/1998/Math/MathML",this.type);for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&r.setAttribute(n,this.attributes[n]);this.classes.length>0&&(r.className=Pe(this.classes));for(var o=0;o<this.children.length;o++)r.appendChild(this.children[o].toNode());return r},t.toMarkup=function(){var r="<"+this.type;for(var n in this.attributes)Object.prototype.hasOwnProperty.call(this.attributes,n)&&(r+=" "+n+'="',r+=N.escape(this.attributes[n]),r+='"');this.classes.length>0&&(r+=' class ="'+N.escape(Pe(this.classes))+'"'),r+=">";for(var o=0;o<this.children.length;o++)r+=this.children[o].toMarkup();return r+="</"+this.type+">",r},t.toText=function(){return this.children.map(function(r){return r.toText()}).join("")},e}(),gt=function(){function e(r){this.text=void 0,this.text=r}var t=e.prototype;return t.toNode=function(){return document.createTextNode(this.text)},t.toMarkup=function(){return N.escape(this.toText())},t.toText=function(){return this.text},e}(),Go=function(){function e(r){this.width=void 0,this.character=void 0,this.width=r,r>=.05555&&r<=.05556?this.character="\u200A":r>=.1666&&r<=.1667?this.character="\u2009":r>=.2222&&r<=.2223?this.character="\u2005":r>=.2777&&r<=.2778?this.character="\u2005\u200A":r>=-.05556&&r<=-.05555?this.character="\u200A\u2063":r>=-.1667&&r<=-.1666?this.character="\u2009\u2063":r>=-.2223&&r<=-.2222?this.character="\u205F\u2063":r>=-.2778&&r<=-.2777?this.character="\u2005\u2063":this.character=null}var t=e.prototype;return t.toNode=function(){if(this.character)return document.createTextNode(this.character);var r=document.createElementNS("http://www.w3.org/1998/Math/MathML","mspace");return r.setAttribute("width",this.width+"em"),r},t.toMarkup=function(){return this.character?"<mtext>"+this.character+"</mtext>":'<mspace width="'+this.width+'em"/>'},t.toText=function(){return this.character?this.character:" "},e}(),z={MathNode:xe,TextNode:gt,SpaceNode:Go,newDocumentFragment:xr},be=function(e,t,r){return K[t][e]&&K[t][e].replace&&e.charCodeAt(0)!==55349&&!(er.hasOwnProperty(e)&&r&&(r.fontFamily&&r.fontFamily.substr(4,2)==="tt"||r.font&&r.font.substr(4,2)==="tt"))&&(e=K[t][e].replace),new z.TextNode(e)},m0=function(e){return e.length===1?e[0]:new z.MathNode("mrow",e)},h0=function(e,t){if(t.fontFamily==="texttt")return"monospace";if(t.fontFamily==="textsf")return t.fontShape==="textit"&&t.fontWeight==="textbf"?"sans-serif-bold-italic":t.fontShape==="textit"?"sans-serif-italic":t.fontWeight==="textbf"?"bold-sans-serif":"sans-serif";if(t.fontShape==="textit"&&t.fontWeight==="textbf")return"bold-italic";if(t.fontShape==="textit")return"italic";if(t.fontWeight==="textbf")return"bold";var r=t.font;if(!r||r==="mathnormal")return null;var n=e.mode;if(r==="mathit")return"italic";if(r==="boldsymbol")return e.type==="textord"?"bold":"bold-italic";if(r==="mathbf")return"bold";if(r==="mathbb")return"double-struck";if(r==="mathfrak")return"fraktur";if(r==="mathscr"||r==="mathcal")return"script";if(r==="mathsf")return"sans-serif";if(r==="mathtt")return"monospace";var o=e.text;if(N.contains(["\\imath","\\jmath"],o))return null;K[n][o]&&K[n][o].replace&&(o=K[n][o].replace);var a=b.fontMap[r].fontName;return Jt(o,a,n)?b.fontMap[r].variant:null},he=function(e,t,r){if(e.length===1){var n=$(e[0],t);return r&&n instanceof xe&&n.type==="mo"&&(n.setAttribute("lspace","0em"),n.setAttribute("rspace","0em")),[n]}for(var o=[],a,l=0;l<e.length;l++){var m=$(e[l],t);if(m instanceof xe&&a instanceof xe){if(m.type==="mtext"&&a.type==="mtext"&&m.getAttribute("mathvariant")===a.getAttribute("mathvariant")){var u;(u=a.children).push.apply(u,m.children);continue}else if(m.type==="mn"&&a.type==="mn"){var f;(f=a.children).push.apply(f,m.children);continue}else if(m.type==="mi"&&m.children.length===1&&a.type==="mn"){var x=m.children[0];if(x instanceof gt&&x.text==="."){var y;(y=a.children).push.apply(y,m.children);continue}}else if(a.type==="mi"&&a.children.length===1){var S=a.children[0];if(S instanceof gt&&S.text==="\u0338"&&(m.type==="mo"||m.type==="mi"||m.type==="mn")){var w=m.children[0];w instanceof gt&&w.text.length>0&&(w.text=w.text.slice(0,1)+"\u0338"+w.text.slice(1),o.pop())}}}o.push(m),a=m}return o},Ve=function(e,t,r){return m0(he(e,t,r))},$=function(e,t){if(!e)return new z.MathNode("mrow");if(Ht[e.type]){var r=Ht[e.type](e,t);return r}else throw new v("Got group of unknown type: '"+e.type+"'")};function br(e,t,r,n,o){var a=he(e,r),l;a.length===1&&a[0]instanceof xe&&N.contains(["mrow","mtable"],a[0].type)?l=a[0]:l=new z.MathNode("mrow",a);var m=new z.MathNode("annotation",[new z.TextNode(t)]);m.setAttribute("encoding","application/x-tex");var u=new z.MathNode("semantics",[l,m]),f=new z.MathNode("math",[u]);f.setAttribute("xmlns","http://www.w3.org/1998/Math/MathML"),n&&f.setAttribute("display","block");var x=o?"katex":"katex-mathml";return b.makeSpan([x],[f])}var yr=function(e){return new So({style:e.displayMode?D.DISPLAY:D.TEXT,maxSize:e.maxSize,minRuleThickness:e.minRuleThickness})},vr=function(e,t){if(t.displayMode){var r=["katex-display"];t.leqno&&r.push("leqno"),t.fleqn&&r.push("fleqn"),e=b.makeSpan(r,[e])}return e},Uo=function(e,t,r){var n=yr(r),o;if(r.output==="mathml")return br(e,t,n,r.displayMode,!0);if(r.output==="html"){var a=c0(e,n);o=b.makeSpan(["katex"],[a])}else{var l=br(e,t,n,r.displayMode,!1),m=c0(e,n);o=b.makeSpan(["katex"],[l,m])}return vr(o,r)},Yo=function(e,t,r){var n=yr(r),o=c0(e,n),a=b.makeSpan(["katex"],[o]);return vr(a,r)},$o={widehat:"^",widecheck:"\u02C7",widetilde:"~",utilde:"~",overleftarrow:"\u2190",underleftarrow:"\u2190",xleftarrow:"\u2190",overrightarrow:"\u2192",underrightarrow:"\u2192",xrightarrow:"\u2192",underbrace:"\u23DF",overbrace:"\u23DE",overgroup:"\u23E0",undergroup:"\u23E1",overleftrightarrow:"\u2194",underleftrightarrow:"\u2194",xleftrightarrow:"\u2194",Overrightarrow:"\u21D2",xRightarrow:"\u21D2",overleftharpoon:"\u21BC",xleftharpoonup:"\u21BC",overrightharpoon:"\u21C0",xrightharpoonup:"\u21C0",xLeftarrow:"\u21D0",xLeftrightarrow:"\u21D4",xhookleftarrow:"\u21A9",xhookrightarrow:"\u21AA",xmapsto:"\u21A6",xrightharpoondown:"\u21C1",xleftharpoondown:"\u21BD",xrightleftharpoons:"\u21CC",xleftrightharpoons:"\u21CB",xtwoheadleftarrow:"\u219E",xtwoheadrightarrow:"\u21A0",xlongequal:"=",xtofrom:"\u21C4",xrightleftarrows:"\u21C4",xrightequilibrium:"\u21CC",xleftequilibrium:"\u21CB","\\\\cdrightarrow":"\u2192","\\\\cdleftarrow":"\u2190","\\\\cdlongequal":"="},jo=function(e){var t=new z.MathNode("mo",[new z.TextNode($o[e])]);return t.setAttribute("stretchy","true"),t},Xo={overrightarrow:[["rightarrow"],.888,522,"xMaxYMin"],overleftarrow:[["leftarrow"],.888,522,"xMinYMin"],underrightarrow:[["rightarrow"],.888,522,"xMaxYMin"],underleftarrow:[["leftarrow"],.888,522,"xMinYMin"],xrightarrow:[["rightarrow"],1.469,522,"xMaxYMin"],"\\cdrightarrow":[["rightarrow"],3,522,"xMaxYMin"],xleftarrow:[["leftarrow"],1.469,522,"xMinYMin"],"\\cdleftarrow":[["leftarrow"],3,522,"xMinYMin"],Overrightarrow:[["doublerightarrow"],.888,560,"xMaxYMin"],xRightarrow:[["doublerightarrow"],1.526,560,"xMaxYMin"],xLeftarrow:[["doubleleftarrow"],1.526,560,"xMinYMin"],overleftharpoon:[["leftharpoon"],.888,522,"xMinYMin"],xleftharpoonup:[["leftharpoon"],.888,522,"xMinYMin"],xleftharpoondown:[["leftharpoondown"],.888,522,"xMinYMin"],overrightharpoon:[["rightharpoon"],.888,522,"xMaxYMin"],xrightharpoonup:[["rightharpoon"],.888,522,"xMaxYMin"],xrightharpoondown:[["rightharpoondown"],.888,522,"xMaxYMin"],xlongequal:[["longequal"],.888,334,"xMinYMin"],"\\cdlongequal":[["longequal"],3,334,"xMinYMin"],xtwoheadleftarrow:[["twoheadleftarrow"],.888,334,"xMinYMin"],xtwoheadrightarrow:[["twoheadrightarrow"],.888,334,"xMaxYMin"],overleftrightarrow:[["leftarrow","rightarrow"],.888,522],overbrace:[["leftbrace","midbrace","rightbrace"],1.6,548],underbrace:[["leftbraceunder","midbraceunder","rightbraceunder"],1.6,548],underleftrightarrow:[["leftarrow","rightarrow"],.888,522],xleftrightarrow:[["leftarrow","rightarrow"],1.75,522],xLeftrightarrow:[["doubleleftarrow","doublerightarrow"],1.75,560],xrightleftharpoons:[["leftharpoondownplus","rightharpoonplus"],1.75,716],xleftrightharpoons:[["leftharpoonplus","rightharpoondownplus"],1.75,716],xhookleftarrow:[["leftarrow","righthook"],1.08,522],xhookrightarrow:[["lefthook","rightarrow"],1.08,522],overlinesegment:[["leftlinesegment","rightlinesegment"],.888,522],underlinesegment:[["leftlinesegment","rightlinesegment"],.888,522],overgroup:[["leftgroup","rightgroup"],.888,342],undergroup:[["leftgroupunder","rightgroupunder"],.888,342],xmapsto:[["leftmapsto","rightarrow"],1.5,522],xtofrom:[["leftToFrom","rightToFrom"],1.75,528],xrightleftarrows:[["baraboveleftarrow","rightarrowabovebar"],1.75,901],xrightequilibrium:[["baraboveshortleftharpoon","rightharpoonaboveshortbar"],1.75,716],xleftequilibrium:[["shortbaraboveleftharpoon","shortrightharpoonabovebar"],1.75,716]},Zo=function(e){return e.type==="ordgroup"?e.body.length:1},Ko=function(e,t){function r(){var m=4e5,u=e.label.substr(1);if(N.contains(["widehat","widecheck","widetilde","utilde"],u)){var f=e,x=Zo(f.base),y,S,w;if(x>5)u==="widehat"||u==="widecheck"?(y=420,m=2364,w=.42,S=u+"4"):(y=312,m=2340,w=.34,S="tilde4");else{var A=[1,1,2,2,3,3][x];u==="widehat"||u==="widecheck"?(m=[0,1062,2364,2364,2364][A],y=[0,239,300,360,420][A],w=[0,.24,.3,.3,.36,.42][A],S=u+A):(m=[0,600,1033,2339,2340][A],y=[0,260,286,306,312][A],w=[0,.26,.286,.3,.306,.34][A],S="tilde"+A)}var M=new Xe(S),B=new Oe([M],{width:"100%",height:w+"em",viewBox:"0 0 "+m+" "+y,preserveAspectRatio:"none"});return{span:b.makeSvgSpan([],[B],t),minWidth:0,height:w}}else{var q=[],I=Xo[u],W=I[0],Y=I[1],Z=I[2],G=Z/1e3,U=W.length,te,j;if(U===1){var ue=I[3];te=["hide-tail"],j=[ue]}else if(U===2)te=["halfarrow-left","halfarrow-right"],j=["xMinYMin","xMaxYMin"];else if(U===3)te=["brace-left","brace-center","brace-right"],j=["xMinYMin","xMidYMin","xMaxYMin"];else throw new Error(`Correct katexImagesData or update code here to support
                    `+U+" children.");for(var de=0;de<U;de++){var Ge=new Xe(W[de]),Ne=new Oe([Ge],{width:"400em",height:G+"em",viewBox:"0 0 "+m+" "+Z,preserveAspectRatio:j[de]+" slice"}),pe=b.makeSvgSpan([te[de]],[Ne],t);if(U===1)return{span:pe,minWidth:Y,height:G};pe.style.height=G+"em",q.push(pe)}return{span:b.makeSpan(["stretchy"],q,t),minWidth:Y,height:G}}}var n=r(),o=n.span,a=n.minWidth,l=n.height;return o.height=l,o.style.height=l+"em",a>0&&(o.style.minWidth=a+"em"),o},Qo=function(e,t,r,n,o){var a,l=e.height+e.depth+r+n;if(/fbox|color|angl/.test(t)){if(a=b.makeSpan(["stretchy",t],[],o),t==="fbox"){var m=o.color&&o.getColor();m&&(a.style.borderColor=m)}}else{var u=[];/^[bx]cancel$/.test(t)&&u.push(new Qt({x1:"0",y1:"0",x2:"100%",y2:"100%","stroke-width":"0.046em"})),/^x?cancel$/.test(t)&&u.push(new Qt({x1:"0",y1:"100%",x2:"100%",y2:"0","stroke-width":"0.046em"}));var f=new Oe(u,{width:"100%",height:l+"em"});a=b.makeSvgSpan([],[f],o)}return a.height=l,a.style.height=l+"em",a},Ie={encloseSpan:Qo,mathMLnode:jo,svgSpan:Ko};function E(e,t){if(!e||e.type!==t)throw new Error("Expected node of type "+t+", but got "+(e?"node of type "+e.type:String(e)));return e}function u0(e){var t=Ot(e);if(!t)throw new Error("Expected node of symbol group type, but got "+(e?"node of type "+e.type:String(e)));return t}function Ot(e){return e&&(e.type==="atom"||vo.hasOwnProperty(e.type))?e:null}var d0=function(e,t){var r,n,o;e&&e.type==="supsub"?(n=E(e.base,"accent"),r=n.base,e.base=r,o=go(_(e,t)),e.base=n):(n=E(e,"accent"),r=n.base);var a=_(r,t.havingCrampedStyle()),l=n.isShifty&&N.isCharacterBox(r),m=0;if(l){var u=N.getBaseElem(r),f=_(u,t.havingCrampedStyle());m=Q0(f).skew}var x=Math.min(a.height,t.fontMetrics().xHeight),y;if(n.isStretchy)y=Ie.svgSpan(n,t),y=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:a},{type:"elem",elem:y,wrapperClasses:["svg-align"],wrapperStyle:m>0?{width:"calc(100% - "+2*m+"em)",marginLeft:2*m+"em"}:void 0}]},t);else{var S,w;n.label==="\\vec"?(S=b.staticSvg("vec",t),w=b.svgData.vec[1]):(S=b.makeOrd({mode:n.mode,text:n.label},t,"textord"),S=Q0(S),S.italic=0,w=S.width),y=b.makeSpan(["accent-body"],[S]);var A=n.label==="\\textcircled";A&&(y.classes.push("accent-full"),x=a.height);var M=m;A||(M-=w/2),y.style.left=M+"em",n.label==="\\textcircled"&&(y.style.top=".2em"),y=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:a},{type:"kern",size:-x},{type:"elem",elem:y}]},t)}var B=b.makeSpan(["mord","accent"],[y],t);return o?(o.children[0]=B,o.height=Math.max(B.height,o.height),o.classes[0]="mord",o):B},wr=function(e,t){var r=e.isStretchy?Ie.mathMLnode(e.label):new z.MathNode("mo",[be(e.label,e.mode)]),n=new z.MathNode("mover",[$(e.base,t),r]);return n.setAttribute("accent","true"),n},Jo=new RegExp(["\\acute","\\grave","\\ddot","\\tilde","\\bar","\\breve","\\check","\\hat","\\vec","\\dot","\\mathring"].map(function(e){return"\\"+e}).join("|"));T({type:"accent",names:["\\acute","\\grave","\\ddot","\\tilde","\\bar","\\breve","\\check","\\hat","\\vec","\\dot","\\mathring","\\widecheck","\\widehat","\\widetilde","\\overrightarrow","\\overleftarrow","\\Overrightarrow","\\overleftrightarrow","\\overgroup","\\overlinesegment","\\overleftharpoon","\\overrightharpoon"],props:{numArgs:1},handler:function(e,t){var r=Et(t[0]),n=!Jo.test(e.funcName),o=!n||e.funcName==="\\widehat"||e.funcName==="\\widetilde"||e.funcName==="\\widecheck";return{type:"accent",mode:e.parser.mode,label:e.funcName,isStretchy:n,isShifty:o,base:r}},htmlBuilder:d0,mathmlBuilder:wr}),T({type:"accent",names:["\\'","\\`","\\^","\\~","\\=","\\u","\\.",'\\"',"\\r","\\H","\\v","\\textcircled"],props:{numArgs:1,allowedInText:!0,allowedInMath:!1,argTypes:["primitive"]},handler:function(e,t){var r=t[0];return{type:"accent",mode:e.parser.mode,label:e.funcName,isStretchy:!1,isShifty:!0,base:r}},htmlBuilder:d0,mathmlBuilder:wr}),T({type:"accentUnder",names:["\\underleftarrow","\\underrightarrow","\\underleftrightarrow","\\undergroup","\\underlinesegment","\\utilde"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"accentUnder",mode:r.mode,label:n,base:o}},htmlBuilder:function(e,t){var r=_(e.base,t),n=Ie.svgSpan(e,t),o=e.label==="\\utilde"?.12:0,a=b.makeVList({positionType:"top",positionData:r.height,children:[{type:"elem",elem:n,wrapperClasses:["svg-align"]},{type:"kern",size:o},{type:"elem",elem:r}]},t);return b.makeSpan(["mord","accentunder"],[a],t)},mathmlBuilder:function(e,t){var r=Ie.mathMLnode(e.label),n=new z.MathNode("munder",[$(e.base,t),r]);return n.setAttribute("accentunder","true"),n}});var Ft=function(e){var t=new z.MathNode("mpadded",e?[e]:[]);return t.setAttribute("width","+0.6em"),t.setAttribute("lspace","0.3em"),t};T({type:"xArrow",names:["\\xleftarrow","\\xrightarrow","\\xLeftarrow","\\xRightarrow","\\xleftrightarrow","\\xLeftrightarrow","\\xhookleftarrow","\\xhookrightarrow","\\xmapsto","\\xrightharpoondown","\\xrightharpoonup","\\xleftharpoondown","\\xleftharpoonup","\\xrightleftharpoons","\\xleftrightharpoons","\\xlongequal","\\xtwoheadrightarrow","\\xtwoheadleftarrow","\\xtofrom","\\xrightleftarrows","\\xrightequilibrium","\\xleftequilibrium","\\\\cdrightarrow","\\\\cdleftarrow","\\\\cdlongequal"],props:{numArgs:1,numOptionalArgs:1},handler:function(e,t,r){var n=e.parser,o=e.funcName;return{type:"xArrow",mode:n.mode,label:o,body:t[0],below:r[0]}},htmlBuilder:function(e,t){var r=t.style,n=t.havingStyle(r.sup()),o=b.wrapFragment(_(e.body,n,t),t),a=e.label.slice(0,2)==="\\x"?"x":"cd";o.classes.push(a+"-arrow-pad");var l;e.below&&(n=t.havingStyle(r.sub()),l=b.wrapFragment(_(e.below,n,t),t),l.classes.push(a+"-arrow-pad"));var m=Ie.svgSpan(e,t),u=-t.fontMetrics().axisHeight+.5*m.height,f=-t.fontMetrics().axisHeight-.5*m.height-.111;(o.depth>.25||e.label==="\\xleftequilibrium")&&(f-=o.depth);var x;if(l){var y=-t.fontMetrics().axisHeight+l.height+.5*m.height+.111;x=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:o,shift:f},{type:"elem",elem:m,shift:u},{type:"elem",elem:l,shift:y}]},t)}else x=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:o,shift:f},{type:"elem",elem:m,shift:u}]},t);return x.children[0].children[0].children[1].classes.push("svg-align"),b.makeSpan(["mrel","x-arrow"],[x],t)},mathmlBuilder:function(e,t){var r=Ie.mathMLnode(e.label);r.setAttribute("minsize",e.label.charAt(0)==="x"?"1.75em":"3.0em");var n;if(e.body){var o=Ft($(e.body,t));if(e.below){var a=Ft($(e.below,t));n=new z.MathNode("munderover",[r,a,o])}else n=new z.MathNode("mover",[r,o])}else if(e.below){var l=Ft($(e.below,t));n=new z.MathNode("munder",[r,l])}else n=Ft(),n=new z.MathNode("mover",[r,n]);return n}});var ei={">":"\\\\cdrightarrow","<":"\\\\cdleftarrow","=":"\\\\cdlongequal",A:"\\uparrow",V:"\\downarrow","|":"\\Vert",".":"no arrow"},kr=function(){return{type:"styling",body:[],mode:"math",style:"display"}},Sr=function(e){return e.type==="textord"&&e.text==="@"},ti=function(e,t){return(e.type==="mathord"||e.type==="atom")&&e.text===t};function ri(e,t,r){var n=ei[e];switch(n){case"\\\\cdrightarrow":case"\\\\cdleftarrow":return r.callFunction(n,[t[0]],[t[1]]);case"\\uparrow":case"\\downarrow":{var o=r.callFunction("\\\\cdleft",[t[0]],[]),a={type:"atom",text:n,mode:"math",family:"rel"},l=r.callFunction("\\Big",[a],[]),m=r.callFunction("\\\\cdright",[t[1]],[]),u={type:"ordgroup",mode:"math",body:[o,l,m]};return r.callFunction("\\\\cdparent",[u],[])}case"\\\\cdlongequal":return r.callFunction("\\\\cdlongequal",[],[]);case"\\Vert":{var f={type:"textord",text:"\\Vert",mode:"math"};return r.callFunction("\\Big",[f],[])}default:return{type:"textord",text:" ",mode:"math"}}}function ni(e){var t=[];for(e.gullet.beginGroup(),e.gullet.macros.set("\\cr","\\\\\\relax"),e.gullet.beginGroup();;){t.push(e.parseExpression(!1,"\\\\")),e.gullet.endGroup(),e.gullet.beginGroup();var r=e.fetch().text;if(r==="&"||r==="\\\\")e.consume();else if(r==="\\end"){t[t.length-1].length===0&&t.pop();break}else throw new v("Expected \\\\ or \\cr or \\end",e.nextToken)}for(var n=[],o=[n],a=0;a<t.length;a++){for(var l=t[a],m=kr(),u=0;u<l.length;u++)if(!Sr(l[u]))m.body.push(l[u]);else{n.push(m),u+=1;var f=u0(l[u]).text,x=new Array(2);if(x[0]={type:"ordgroup",mode:"math",body:[]},x[1]={type:"ordgroup",mode:"math",body:[]},!("=|.".indexOf(f)>-1))if("<>AV".indexOf(f)>-1)for(var y=0;y<2;y++){for(var S=!0,w=u+1;w<l.length;w++){if(ti(l[w],f)){S=!1,u=w;break}if(Sr(l[w]))throw new v("Missing a "+f+" character to complete a CD arrow.",l[w]);x[y].body.push(l[w])}if(S)throw new v("Missing a "+f+" character to complete a CD arrow.",l[u])}else throw new v('Expected one of "<>AV=|." after @',l[u]);var A=ri(f,x,e),M={type:"styling",body:[A],mode:"math",style:"display"};n.push(M),m=kr()}a%2===0?n.push(m):n.shift(),n=[],o.push(n)}e.gullet.endGroup(),e.gullet.endGroup();var B=new Array(o[0].length).fill({type:"align",align:"c",pregap:.25,postgap:.25});return{type:"array",mode:"math",body:o,arraystretch:1,addJot:!0,rowGaps:[null],cols:B,colSeparationType:"CD",hLinesBeforeRow:new Array(o.length+1).fill([])}}T({type:"cdlabel",names:["\\\\cdleft","\\\\cdright"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=e.funcName;return{type:"cdlabel",mode:r.mode,side:n.slice(4),label:t[0]}},htmlBuilder:function(e,t){var r=t.havingStyle(t.style.sup()),n=b.wrapFragment(_(e.label,r,t),t);return n.classes.push("cd-label-"+e.side),n.style.bottom=.8-n.depth+"em",n.height=0,n.depth=0,n},mathmlBuilder:function(e,t){var r=new z.MathNode("mrow",[$(e.label,t)]);return r=new z.MathNode("mpadded",[r]),r.setAttribute("width","0"),e.side==="left"&&r.setAttribute("lspace","-1width"),r.setAttribute("voffset","0.7em"),r=new z.MathNode("mstyle",[r]),r.setAttribute("displaystyle","false"),r.setAttribute("scriptlevel","1"),r}}),T({type:"cdlabelparent",names:["\\\\cdparent"],props:{numArgs:1},handler:function(e,t){var r=e.parser;return{type:"cdlabelparent",mode:r.mode,fragment:t[0]}},htmlBuilder:function(e,t){var r=b.wrapFragment(_(e.fragment,t),t);return r.classes.push("cd-vert-arrow"),r},mathmlBuilder:function(e,t){return new z.MathNode("mrow",[$(e.fragment,t)])}}),T({type:"textord",names:["\\@char"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){for(var r=e.parser,n=E(t[0],"ordgroup"),o=n.body,a="",l=0;l<o.length;l++){var m=E(o[l],"textord");a+=m.text}var u=parseInt(a);if(isNaN(u))throw new v("\\@char has non-numeric argument "+a);return{type:"textord",mode:r.mode,text:String.fromCharCode(u)}}});var zr=function(e,t){var r=ie(e.body,t.withColor(e.color),!1);return b.makeFragment(r)},Ar=function(e,t){var r=he(e.body,t.withColor(e.color)),n=new z.MathNode("mstyle",r);return n.setAttribute("mathcolor",e.color),n};T({type:"color",names:["\\textcolor"],props:{numArgs:2,allowedInText:!0,argTypes:["color","original"]},handler:function(e,t){var r=e.parser,n=E(t[0],"color-token").color,o=t[1];return{type:"color",mode:r.mode,color:n,body:oe(o)}},htmlBuilder:zr,mathmlBuilder:Ar}),T({type:"color",names:["\\color"],props:{numArgs:1,allowedInText:!0,argTypes:["color"]},handler:function(e,t){var r=e.parser,n=e.breakOnTokenText,o=E(t[0],"color-token").color;r.gullet.macros.set("\\current@color",o);var a=r.parseExpression(!0,n);return{type:"color",mode:r.mode,color:o,body:a}},htmlBuilder:zr,mathmlBuilder:Ar}),T({type:"cr",names:["\\\\"],props:{numArgs:0,numOptionalArgs:1,argTypes:["size"],allowedInText:!0},handler:function(e,t,r){var n=e.parser,o=r[0],a=!n.settings.displayMode||!n.settings.useStrictBehavior("newLineInDisplayMode","In LaTeX, \\\\ or \\newline does nothing in display mode");return{type:"cr",mode:n.mode,newLine:a,size:o&&E(o,"size").value}},htmlBuilder:function(e,t){var r=b.makeSpan(["mspace"],[],t);return e.newLine&&(r.classes.push("newline"),e.size&&(r.style.marginTop=J(e.size,t)+"em")),r},mathmlBuilder:function(e,t){var r=new z.MathNode("mspace");return e.newLine&&(r.setAttribute("linebreak","newline"),e.size&&r.setAttribute("height",J(e.size,t)+"em")),r}});var p0={"\\global":"\\global","\\long":"\\\\globallong","\\\\globallong":"\\\\globallong","\\def":"\\gdef","\\gdef":"\\gdef","\\edef":"\\xdef","\\xdef":"\\xdef","\\let":"\\\\globallet","\\futurelet":"\\\\globalfuture"},Mr=function(e){var t=e.text;if(/^(?:[\\{}$&#^_]|EOF)$/.test(t))throw new v("Expected a control sequence",e);return t},oi=function(e){var t=e.gullet.popToken();return t.text==="="&&(t=e.gullet.popToken(),t.text===" "&&(t=e.gullet.popToken())),t},Tr=function(e,t,r,n){var o=e.gullet.macros.get(r.text);o==null&&(r.noexpand=!0,o={tokens:[r],numArgs:0,unexpandable:!e.gullet.isExpandable(r.text)}),e.gullet.macros.set(t,o,n)};T({type:"internal",names:["\\global","\\long","\\\\globallong"],props:{numArgs:0,allowedInText:!0},handler:function(e){var t=e.parser,r=e.funcName;t.consumeSpaces();var n=t.fetch();if(p0[n.text])return(r==="\\global"||r==="\\\\globallong")&&(n.text=p0[n.text]),E(t.parseFunction(),"internal");throw new v("Invalid token after macro prefix",n)}}),T({type:"internal",names:["\\def","\\gdef","\\edef","\\xdef"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:function(e){var t=e.parser,r=e.funcName,n=t.gullet.popToken(),o=n.text;if(/^(?:[\\{}$&#^_]|EOF)$/.test(o))throw new v("Expected a control sequence",n);for(var a=0,l,m=[[]];t.gullet.future().text!=="{";)if(n=t.gullet.popToken(),n.text==="#"){if(t.gullet.future().text==="{"){l=t.gullet.future(),m[a].push("{");break}if(n=t.gullet.popToken(),!/^[1-9]$/.test(n.text))throw new v('Invalid argument number "'+n.text+'"');if(parseInt(n.text)!==a+1)throw new v('Argument number "'+n.text+'" out of order');a++,m.push([])}else{if(n.text==="EOF")throw new v("Expected a macro definition");m[a].push(n.text)}var u=t.gullet.consumeArg(),f=u.tokens;return l&&f.unshift(l),(r==="\\edef"||r==="\\xdef")&&(f=t.gullet.expandTokens(f),f.reverse()),t.gullet.macros.set(o,{tokens:f,numArgs:a,delimiters:m},r===p0[r]),{type:"internal",mode:t.mode}}}),T({type:"internal",names:["\\let","\\\\globallet"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:function(e){var t=e.parser,r=e.funcName,n=Mr(t.gullet.popToken());t.gullet.consumeSpaces();var o=oi(t);return Tr(t,n,o,r==="\\\\globallet"),{type:"internal",mode:t.mode}}}),T({type:"internal",names:["\\futurelet","\\\\globalfuture"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:function(e){var t=e.parser,r=e.funcName,n=Mr(t.gullet.popToken()),o=t.gullet.popToken(),a=t.gullet.popToken();return Tr(t,n,a,r==="\\\\globalfuture"),t.gullet.pushToken(a),t.gullet.pushToken(o),{type:"internal",mode:t.mode}}});var xt=function(e,t,r){var n=K.math[e]&&K.math[e].replace,o=Jt(n||e,t,r);if(!o)throw new Error("Unsupported symbol "+e+" and font size "+t+".");return o},f0=function(e,t,r,n){var o=r.havingBaseStyle(t),a=b.makeSpan(n.concat(o.sizingClasses(r)),[e],r),l=o.sizeMultiplier/r.sizeMultiplier;return a.height*=l,a.depth*=l,a.maxFontSize=o.sizeMultiplier,a},Cr=function(e,t,r){var n=t.havingBaseStyle(r),o=(1-t.sizeMultiplier/n.sizeMultiplier)*t.fontMetrics().axisHeight;e.classes.push("delimcenter"),e.style.top=o+"em",e.height-=o,e.depth+=o},ii=function(e,t,r,n,o,a){var l=b.makeSymbol(e,"Main-Regular",o,n),m=f0(l,t,n,a);return r&&Cr(m,n,t),m},ai=function(e,t,r,n){return b.makeSymbol(e,"Size"+t+"-Regular",r,n)},Br=function(e,t,r,n,o,a){var l=ai(e,t,o,n),m=f0(b.makeSpan(["delimsizing","size"+t],[l],n),D.TEXT,n,a);return r&&Cr(m,n,D.TEXT),m},g0=function(e,t,r){var n;t==="Size1-Regular"?n="delim-size1":n="delim-size4";var o=b.makeSpan(["delimsizinginner",n],[b.makeSpan([],[b.makeSymbol(e,t,r)])]);return{type:"elem",elem:o}},x0=function(e,t,r){var n=Ae["Size4-Regular"][e.charCodeAt(0)]?Ae["Size4-Regular"][e.charCodeAt(0)][4].toFixed(3):Ae["Size1-Regular"][e.charCodeAt(0)][4].toFixed(3),o=new Xe("inner",uo(e,Math.round(1e3*t))),a=new Oe([o],{width:n+"em",height:t+"em",style:"width:"+n+"em",viewBox:"0 0 "+1e3*n+" "+Math.round(1e3*t),preserveAspectRatio:"xMinYMin"}),l=b.makeSvgSpan([],[a],r);return l.height=t,l.style.height=t+"em",l.style.width=n+"em",{type:"elem",elem:l}},b0=.008,Vt={type:"kern",size:-1*b0},si=["|","\\lvert","\\rvert","\\vert"],li=["\\|","\\lVert","\\rVert","\\Vert"],Nr=function(e,t,r,n,o,a){var l,m,u,f;l=u=f=e,m=null;var x="Size1-Regular";e==="\\uparrow"?u=f="\u23D0":e==="\\Uparrow"?u=f="\u2016":e==="\\downarrow"?l=u="\u23D0":e==="\\Downarrow"?l=u="\u2016":e==="\\updownarrow"?(l="\\uparrow",u="\u23D0",f="\\downarrow"):e==="\\Updownarrow"?(l="\\Uparrow",u="\u2016",f="\\Downarrow"):N.contains(si,e)?u="\u2223":N.contains(li,e)?u="\u2225":e==="["||e==="\\lbrack"?(l="\u23A1",u="\u23A2",f="\u23A3",x="Size4-Regular"):e==="]"||e==="\\rbrack"?(l="\u23A4",u="\u23A5",f="\u23A6",x="Size4-Regular"):e==="\\lfloor"||e==="\u230A"?(u=l="\u23A2",f="\u23A3",x="Size4-Regular"):e==="\\lceil"||e==="\u2308"?(l="\u23A1",u=f="\u23A2",x="Size4-Regular"):e==="\\rfloor"||e==="\u230B"?(u=l="\u23A5",f="\u23A6",x="Size4-Regular"):e==="\\rceil"||e==="\u2309"?(l="\u23A4",u=f="\u23A5",x="Size4-Regular"):e==="("||e==="\\lparen"?(l="\u239B",u="\u239C",f="\u239D",x="Size4-Regular"):e===")"||e==="\\rparen"?(l="\u239E",u="\u239F",f="\u23A0",x="Size4-Regular"):e==="\\{"||e==="\\lbrace"?(l="\u23A7",m="\u23A8",f="\u23A9",u="\u23AA",x="Size4-Regular"):e==="\\}"||e==="\\rbrace"?(l="\u23AB",m="\u23AC",f="\u23AD",u="\u23AA",x="Size4-Regular"):e==="\\lgroup"||e==="\u27EE"?(l="\u23A7",f="\u23A9",u="\u23AA",x="Size4-Regular"):e==="\\rgroup"||e==="\u27EF"?(l="\u23AB",f="\u23AD",u="\u23AA",x="Size4-Regular"):e==="\\lmoustache"||e==="\u23B0"?(l="\u23A7",f="\u23AD",u="\u23AA",x="Size4-Regular"):(e==="\\rmoustache"||e==="\u23B1")&&(l="\u23AB",f="\u23A9",u="\u23AA",x="Size4-Regular");var y=xt(l,x,o),S=y.height+y.depth,w=xt(u,x,o),A=w.height+w.depth,M=xt(f,x,o),B=M.height+M.depth,q=0,I=1;if(m!==null){var W=xt(m,x,o);q=W.height+W.depth,I=2}var Y=S+B+q,Z=Math.max(0,Math.ceil((t-Y)/(I*A))),G=Y+Z*I*A,U=n.fontMetrics().axisHeight;r&&(U*=n.sizeMultiplier);var te=G/2-U,j=[];if(j.push(g0(f,x,o)),j.push(Vt),m===null){var ue=G-S-B+2*b0;j.push(x0(u,ue,n))}else{var de=(G-S-B-q)/2+2*b0;j.push(x0(u,de,n)),j.push(Vt),j.push(g0(m,x,o)),j.push(Vt),j.push(x0(u,de,n))}j.push(Vt),j.push(g0(l,x,o));var Ge=n.havingBaseStyle(D.TEXT),Ne=b.makeVList({positionType:"bottom",positionData:te,children:j},Ge);return f0(b.makeSpan(["delimsizing","mult"],[Ne],Ge),D.TEXT,n,a)},y0=80,v0=.08,w0=function(e,t,r,n,o){var a=ho(e,n,r),l=new Xe(e,a),m=new Oe([l],{width:"400em",height:t+"em",viewBox:"0 0 400000 "+r,preserveAspectRatio:"xMinYMin slice"});return b.makeSvgSpan(["hide-tail"],[m],o)},ci=function(e,t){var r=t.havingBaseSizing(),n=Dr("\\surd",e*r.sizeMultiplier,Rr,r),o=r.sizeMultiplier,a=Math.max(0,t.minRuleThickness-t.fontMetrics().sqrtRuleThickness),l,m=0,u=0,f=0,x;return n.type==="small"?(f=1e3+1e3*a+y0,e<1?o=1:e<1.4&&(o=.7),m=(1+a+v0)/o,u=(1+a)/o,l=w0("sqrtMain",m,f,a,t),l.style.minWidth="0.853em",x=.833/o):n.type==="large"?(f=(1e3+y0)*bt[n.size],u=(bt[n.size]+a)/o,m=(bt[n.size]+a+v0)/o,l=w0("sqrtSize"+n.size,m,f,a,t),l.style.minWidth="1.02em",x=1/o):(m=e+a+v0,u=e+a,f=Math.floor(1e3*e+a)+y0,l=w0("sqrtTall",m,f,a,t),l.style.minWidth="0.742em",x=1.056),l.height=u,l.style.height=m+"em",{span:l,advanceWidth:x,ruleWidth:(t.fontMetrics().sqrtRuleThickness+a)*o}},qr=["(","\\lparen",")","\\rparen","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","\u230A","\u230B","\\lceil","\\rceil","\u2308","\u2309","\\surd"],mi=["\\uparrow","\\downarrow","\\updownarrow","\\Uparrow","\\Downarrow","\\Updownarrow","|","\\|","\\vert","\\Vert","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","\u27EE","\u27EF","\\lmoustache","\\rmoustache","\u23B0","\u23B1"],Lr=["<",">","\\langle","\\rangle","/","\\backslash","\\lt","\\gt"],bt=[0,1.2,1.8,2.4,3],hi=function(e,t,r,n,o){if(e==="<"||e==="\\lt"||e==="\u27E8"?e="\\langle":(e===">"||e==="\\gt"||e==="\u27E9")&&(e="\\rangle"),N.contains(qr,e)||N.contains(Lr,e))return Br(e,t,!1,r,n,o);if(N.contains(mi,e))return Nr(e,bt[t],!1,r,n,o);throw new v("Illegal delimiter: '"+e+"'")},ui=[{type:"small",style:D.SCRIPTSCRIPT},{type:"small",style:D.SCRIPT},{type:"small",style:D.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4}],di=[{type:"small",style:D.SCRIPTSCRIPT},{type:"small",style:D.SCRIPT},{type:"small",style:D.TEXT},{type:"stack"}],Rr=[{type:"small",style:D.SCRIPTSCRIPT},{type:"small",style:D.SCRIPT},{type:"small",style:D.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4},{type:"stack"}],pi=function(e){if(e.type==="small")return"Main-Regular";if(e.type==="large")return"Size"+e.size+"-Regular";if(e.type==="stack")return"Size4-Regular";throw new Error("Add support for delim type '"+e.type+"' here.")},Dr=function(e,t,r,n){for(var o=Math.min(2,3-n.style.size),a=o;a<r.length&&r[a].type!=="stack";a++){var l=xt(e,pi(r[a]),"math"),m=l.height+l.depth;if(r[a].type==="small"){var u=n.havingBaseStyle(r[a].style);m*=u.sizeMultiplier}if(m>t)return r[a]}return r[r.length-1]},Ir=function(e,t,r,n,o,a){e==="<"||e==="\\lt"||e==="\u27E8"?e="\\langle":(e===">"||e==="\\gt"||e==="\u27E9")&&(e="\\rangle");var l;N.contains(Lr,e)?l=ui:N.contains(qr,e)?l=Rr:l=di;var m=Dr(e,t,l,n);return m.type==="small"?ii(e,m.style,r,n,o,a):m.type==="large"?Br(e,m.size,r,n,o,a):Nr(e,t,r,n,o,a)},fi=function(e,t,r,n,o,a){var l=n.fontMetrics().axisHeight*n.sizeMultiplier,m=901,u=5/n.fontMetrics().ptPerEm,f=Math.max(t-l,r+l),x=Math.max(f/500*m,2*f-u);return Ir(e,x,!0,n,o,a)},Me={sqrtImage:ci,sizedDelim:hi,sizeToMaxHeight:bt,customSizedDelim:Ir,leftRightDelim:fi},Hr={"\\bigl":{mclass:"mopen",size:1},"\\Bigl":{mclass:"mopen",size:2},"\\biggl":{mclass:"mopen",size:3},"\\Biggl":{mclass:"mopen",size:4},"\\bigr":{mclass:"mclose",size:1},"\\Bigr":{mclass:"mclose",size:2},"\\biggr":{mclass:"mclose",size:3},"\\Biggr":{mclass:"mclose",size:4},"\\bigm":{mclass:"mrel",size:1},"\\Bigm":{mclass:"mrel",size:2},"\\biggm":{mclass:"mrel",size:3},"\\Biggm":{mclass:"mrel",size:4},"\\big":{mclass:"mord",size:1},"\\Big":{mclass:"mord",size:2},"\\bigg":{mclass:"mord",size:3},"\\Bigg":{mclass:"mord",size:4}},gi=["(","\\lparen",")","\\rparen","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","\u230A","\u230B","\\lceil","\\rceil","\u2308","\u2309","<",">","\\langle","\u27E8","\\rangle","\u27E9","\\lt","\\gt","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","\u27EE","\u27EF","\\lmoustache","\\rmoustache","\u23B0","\u23B1","/","\\backslash","|","\\vert","\\|","\\Vert","\\uparrow","\\Uparrow","\\downarrow","\\Downarrow","\\updownarrow","\\Updownarrow","."];function Wt(e,t){var r=Ot(e);if(r&&N.contains(gi,r.text))return r;throw r?new v("Invalid delimiter '"+r.text+"' after '"+t.funcName+"'",e):new v("Invalid delimiter type '"+e.type+"'",e)}T({type:"delimsizing",names:["\\bigl","\\Bigl","\\biggl","\\Biggl","\\bigr","\\Bigr","\\biggr","\\Biggr","\\bigm","\\Bigm","\\biggm","\\Biggm","\\big","\\Big","\\bigg","\\Bigg"],props:{numArgs:1,argTypes:["primitive"]},handler:function(e,t){var r=Wt(t[0],e);return{type:"delimsizing",mode:e.parser.mode,size:Hr[e.funcName].size,mclass:Hr[e.funcName].mclass,delim:r.text}},htmlBuilder:function(e,t){return e.delim==="."?b.makeSpan([e.mclass]):Me.sizedDelim(e.delim,e.size,t,e.mode,[e.mclass])},mathmlBuilder:function(e){var t=[];e.delim!=="."&&t.push(be(e.delim,e.mode));var r=new z.MathNode("mo",t);return e.mclass==="mopen"||e.mclass==="mclose"?r.setAttribute("fence","true"):r.setAttribute("fence","false"),r.setAttribute("stretchy","true"),r.setAttribute("minsize",Me.sizeToMaxHeight[e.size]+"em"),r.setAttribute("maxsize",Me.sizeToMaxHeight[e.size]+"em"),r}});function Er(e){if(!e.body)throw new Error("Bug: The leftright ParseNode wasn't fully parsed.")}T({type:"leftright-right",names:["\\right"],props:{numArgs:1,primitive:!0},handler:function(e,t){var r=e.parser.gullet.macros.get("\\current@color");if(r&&typeof r!="string")throw new v("\\current@color set to non-string in \\right");return{type:"leftright-right",mode:e.parser.mode,delim:Wt(t[0],e).text,color:r}}}),T({type:"leftright",names:["\\left"],props:{numArgs:1,primitive:!0},handler:function(e,t){var r=Wt(t[0],e),n=e.parser;++n.leftrightDepth;var o=n.parseExpression(!1);--n.leftrightDepth,n.expect("\\right",!1);var a=E(n.parseFunction(),"leftright-right");return{type:"leftright",mode:n.mode,body:o,left:r.text,right:a.delim,rightColor:a.color}},htmlBuilder:function(e,t){Er(e);for(var r=ie(e.body,t,!0,["mopen","mclose"]),n=0,o=0,a=!1,l=0;l<r.length;l++)r[l].isMiddle?a=!0:(n=Math.max(r[l].height,n),o=Math.max(r[l].depth,o));n*=t.sizeMultiplier,o*=t.sizeMultiplier;var m;if(e.left==="."?m=ft(t,["mopen"]):m=Me.leftRightDelim(e.left,n,o,t,e.mode,["mopen"]),r.unshift(m),a)for(var u=1;u<r.length;u++){var f=r[u],x=f.isMiddle;x&&(r[u]=Me.leftRightDelim(x.delim,n,o,x.options,e.mode,[]))}var y;if(e.right===".")y=ft(t,["mclose"]);else{var S=e.rightColor?t.withColor(e.rightColor):t;y=Me.leftRightDelim(e.right,n,o,S,e.mode,["mclose"])}return r.push(y),b.makeSpan(["minner"],r,t)},mathmlBuilder:function(e,t){Er(e);var r=he(e.body,t);if(e.left!=="."){var n=new z.MathNode("mo",[be(e.left,e.mode)]);n.setAttribute("fence","true"),r.unshift(n)}if(e.right!=="."){var o=new z.MathNode("mo",[be(e.right,e.mode)]);o.setAttribute("fence","true"),e.rightColor&&o.setAttribute("mathcolor",e.rightColor),r.push(o)}return m0(r)}}),T({type:"middle",names:["\\middle"],props:{numArgs:1,primitive:!0},handler:function(e,t){var r=Wt(t[0],e);if(!e.parser.leftrightDepth)throw new v("\\middle without preceding \\left",r);return{type:"middle",mode:e.parser.mode,delim:r.text}},htmlBuilder:function(e,t){var r;if(e.delim===".")r=ft(t,[]);else{r=Me.sizedDelim(e.delim,1,t,e.mode,[]);var n={delim:e.delim,options:t};r.isMiddle=n}return r},mathmlBuilder:function(e,t){var r=e.delim==="\\vert"||e.delim==="|"?be("|","text"):be(e.delim,e.mode),n=new z.MathNode("mo",[r]);return n.setAttribute("fence","true"),n.setAttribute("lspace","0.05em"),n.setAttribute("rspace","0.05em"),n}});var k0=function(e,t){var r=b.wrapFragment(_(e.body,t),t),n=e.label.substr(1),o=t.sizeMultiplier,a,l=0,m=N.isCharacterBox(e.body);if(n==="sout")a=b.makeSpan(["stretchy","sout"]),a.height=t.fontMetrics().defaultRuleThickness/o,l=-.5*t.fontMetrics().xHeight;else if(n==="phase"){var u=J({number:.6,unit:"pt"},t),f=J({number:.35,unit:"ex"},t),x=t.havingBaseSizing();o=o/x.sizeMultiplier;var y=r.height+r.depth+u+f;r.style.paddingLeft=y/2+u+"em";var S=Math.floor(1e3*y*o),w=co(S),A=new Oe([new Xe("phase",w)],{width:"400em",height:S/1e3+"em",viewBox:"0 0 400000 "+S,preserveAspectRatio:"xMinYMin slice"});a=b.makeSvgSpan(["hide-tail"],[A],t),a.style.height=y+"em",l=r.depth+u+f}else{/cancel/.test(n)?m||r.classes.push("cancel-pad"):n==="angl"?r.classes.push("anglpad"):r.classes.push("boxpad");var M=0,B=0,q=0;/box/.test(n)?(q=Math.max(t.fontMetrics().fboxrule,t.minRuleThickness),M=t.fontMetrics().fboxsep+(n==="colorbox"?0:q),B=M):n==="angl"?(q=Math.max(t.fontMetrics().defaultRuleThickness,t.minRuleThickness),M=4*q,B=Math.max(0,.25-r.depth)):(M=m?.2:0,B=M),a=Ie.encloseSpan(r,n,M,B,t),/fbox|boxed|fcolorbox/.test(n)?(a.style.borderStyle="solid",a.style.borderWidth=q+"em"):n==="angl"&&q!==.049&&(a.style.borderTopWidth=q+"em",a.style.borderRightWidth=q+"em"),l=r.depth+B,e.backgroundColor&&(a.style.backgroundColor=e.backgroundColor,e.borderColor&&(a.style.borderColor=e.borderColor))}var I;if(e.backgroundColor)I=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:a,shift:l},{type:"elem",elem:r,shift:0}]},t);else{var W=/cancel|phase/.test(n)?["svg-align"]:[];I=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:r,shift:0},{type:"elem",elem:a,shift:l,wrapperClasses:W}]},t)}return/cancel/.test(n)&&(I.height=r.height,I.depth=r.depth),/cancel/.test(n)&&!m?b.makeSpan(["mord","cancel-lap"],[I],t):b.makeSpan(["mord"],[I],t)},S0=function(e,t){var r=0,n=new z.MathNode(e.label.indexOf("colorbox")>-1?"mpadded":"menclose",[$(e.body,t)]);switch(e.label){case"\\cancel":n.setAttribute("notation","updiagonalstrike");break;case"\\bcancel":n.setAttribute("notation","downdiagonalstrike");break;case"\\phase":n.setAttribute("notation","phasorangle");break;case"\\sout":n.setAttribute("notation","horizontalstrike");break;case"\\fbox":n.setAttribute("notation","box");break;case"\\angl":n.setAttribute("notation","actuarial");break;case"\\fcolorbox":case"\\colorbox":if(r=t.fontMetrics().fboxsep*t.fontMetrics().ptPerEm,n.setAttribute("width","+"+2*r+"pt"),n.setAttribute("height","+"+2*r+"pt"),n.setAttribute("lspace",r+"pt"),n.setAttribute("voffset",r+"pt"),e.label==="\\fcolorbox"){var o=Math.max(t.fontMetrics().fboxrule,t.minRuleThickness);n.setAttribute("style","border: "+o+"em solid "+String(e.borderColor))}break;case"\\xcancel":n.setAttribute("notation","updiagonalstrike downdiagonalstrike");break}return e.backgroundColor&&n.setAttribute("mathbackground",e.backgroundColor),n};T({type:"enclose",names:["\\colorbox"],props:{numArgs:2,allowedInText:!0,argTypes:["color","text"]},handler:function(e,t,r){var n=e.parser,o=e.funcName,a=E(t[0],"color-token").color,l=t[1];return{type:"enclose",mode:n.mode,label:o,backgroundColor:a,body:l}},htmlBuilder:k0,mathmlBuilder:S0}),T({type:"enclose",names:["\\fcolorbox"],props:{numArgs:3,allowedInText:!0,argTypes:["color","color","text"]},handler:function(e,t,r){var n=e.parser,o=e.funcName,a=E(t[0],"color-token").color,l=E(t[1],"color-token").color,m=t[2];return{type:"enclose",mode:n.mode,label:o,backgroundColor:l,borderColor:a,body:m}},htmlBuilder:k0,mathmlBuilder:S0}),T({type:"enclose",names:["\\fbox"],props:{numArgs:1,argTypes:["hbox"],allowedInText:!0},handler:function(e,t){var r=e.parser;return{type:"enclose",mode:r.mode,label:"\\fbox",body:t[0]}}}),T({type:"enclose",names:["\\cancel","\\bcancel","\\xcancel","\\sout","\\phase"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"enclose",mode:r.mode,label:n,body:o}},htmlBuilder:k0,mathmlBuilder:S0}),T({type:"enclose",names:["\\angl"],props:{numArgs:1,argTypes:["hbox"],allowedInText:!1},handler:function(e,t){var r=e.parser;return{type:"enclose",mode:r.mode,label:"\\angl",body:t[0]}}});var Pr={};function Te(e){for(var t=e.type,r=e.names,n=e.props,o=e.handler,a=e.htmlBuilder,l=e.mathmlBuilder,m={type:t,numArgs:n.numArgs||0,allowedInText:!1,numOptionalArgs:0,handler:o},u=0;u<r.length;++u)Pr[r[u]]=m;a&&(It[t]=a),l&&(Ht[t]=l)}function Or(e){var t=[];e.consumeSpaces();for(var r=e.fetch().text;r==="\\hline"||r==="\\hdashline";)e.consume(),t.push(r==="\\hdashline"),e.consumeSpaces(),r=e.fetch().text;return t}var _t=function(e){var t=e.parser.settings;if(!t.displayMode)throw new v("{"+e.envName+"} can be used only in display mode.")};function We(e,t,r){var n=t.hskipBeforeAndAfter,o=t.addJot,a=t.cols,l=t.arraystretch,m=t.colSeparationType,u=t.addEqnNum,f=t.singleRow,x=t.maxNumCols,y=t.leqno;if(e.gullet.beginGroup(),f||e.gullet.macros.set("\\cr","\\\\\\relax"),!l){var S=e.gullet.expandMacroAsText("\\arraystretch");if(S==null)l=1;else if(l=parseFloat(S),!l||l<0)throw new v("Invalid \\arraystretch: "+S)}e.gullet.beginGroup();var w=[],A=[w],M=[],B=[];for(B.push(Or(e));;){var q=e.parseExpression(!1,f?"\\end":"\\\\");e.gullet.endGroup(),e.gullet.beginGroup(),q={type:"ordgroup",mode:e.mode,body:q},r&&(q={type:"styling",mode:e.mode,style:r,body:[q]}),w.push(q);var I=e.fetch().text;if(I==="&"){if(x&&w.length===x){if(f||m)throw new v("Too many tab characters: &",e.nextToken);e.settings.reportNonstrict("textEnv","Too few columns specified in the {array} column argument.")}e.consume()}else if(I==="\\end"){w.length===1&&q.type==="styling"&&q.body[0].body.length===0&&A.pop(),B.length<A.length+1&&B.push([]);break}else if(I==="\\\\"){e.consume();var W=void 0;e.gullet.future().text!==" "&&(W=e.parseSizeGroup(!0)),M.push(W?W.value:null),B.push(Or(e)),w=[],A.push(w)}else throw new v("Expected & or \\\\ or \\cr or \\end",e.nextToken)}return e.gullet.endGroup(),e.gullet.endGroup(),{type:"array",mode:e.mode,addJot:o,arraystretch:l,body:A,cols:a,rowGaps:M,hskipBeforeAndAfter:n,hLinesBeforeRow:B,colSeparationType:m,addEqnNum:u,leqno:y}}function z0(e){return e.substr(0,1)==="d"?"display":"text"}var Ce=function(e,t){var r,n,o=e.body.length,a=e.hLinesBeforeRow,l=0,m=new Array(o),u=[],f=Math.max(t.fontMetrics().arrayRuleWidth,t.minRuleThickness),x=1/t.fontMetrics().ptPerEm,y=5*x;if(e.colSeparationType&&e.colSeparationType==="small"){var S=t.havingStyle(D.SCRIPT).sizeMultiplier;y=.2778*(S/t.sizeMultiplier)}var w=e.colSeparationType==="CD"?J({number:3,unit:"ex"},t):12*x,A=3*x,M=e.arraystretch*w,B=.7*M,q=.3*M,I=0;function W(Tn){for(var Yt=0;Yt<Tn.length;++Yt)Yt>0&&(I+=.25),u.push({pos:I,isDashed:Tn[Yt]})}for(W(a[0]),r=0;r<e.body.length;++r){var Y=e.body[r],Z=B,G=q;l<Y.length&&(l=Y.length);var U=new Array(Y.length);for(n=0;n<Y.length;++n){var te=_(Y[n],t);G<te.depth&&(G=te.depth),Z<te.height&&(Z=te.height),U[n]=te}var j=e.rowGaps[r],ue=0;j&&(ue=J(j,t),ue>0&&(ue+=q,G<ue&&(G=ue),ue=0)),e.addJot&&(G+=A),U.height=Z,U.depth=G,I+=Z,U.pos=I,I+=G+ue,m[r]=U,W(a[r+1])}var de=I/2+t.fontMetrics().axisHeight,Ge=e.cols||[],Ne=[],pe,at,Sn=[];if(e.addEqnNum)for(r=0;r<o;++r){var E0=m[r],Wi=E0.pos-de,P0=b.makeSpan(["eqn-num"],[],t);P0.depth=E0.depth,P0.height=E0.height,Sn.push({type:"elem",elem:P0,shift:Wi})}for(n=0,at=0;n<l||at<Ge.length;++n,++at){for(var He=Ge[at]||{},zn=!0;He.type==="separator";){if(zn||(pe=b.makeSpan(["arraycolsep"],[]),pe.style.width=t.fontMetrics().doubleRuleSep+"em",Ne.push(pe)),He.separator==="|"||He.separator===":"){var _i=He.separator==="|"?"solid":"dashed",st=b.makeSpan(["vertical-separator"],[],t);st.style.height=I+"em",st.style.borderRightWidth=f+"em",st.style.borderRightStyle=_i,st.style.margin="0 -"+f/2+"em",st.style.verticalAlign=-(I-de)+"em",Ne.push(st)}else throw new v("Invalid separator type: "+He.separator);at++,He=Ge[at]||{},zn=!1}if(!(n>=l)){var lt=void 0;(n>0||e.hskipBeforeAndAfter)&&(lt=N.deflt(He.pregap,y),lt!==0&&(pe=b.makeSpan(["arraycolsep"],[]),pe.style.width=lt+"em",Ne.push(pe)));var ct=[];for(r=0;r<o;++r){var Gt=m[r],Ut=Gt[n];if(Ut){var Gi=Gt.pos-de;Ut.depth=Gt.depth,Ut.height=Gt.height,ct.push({type:"elem",elem:Ut,shift:Gi})}}ct=b.makeVList({positionType:"individualShift",children:ct},t),ct=b.makeSpan(["col-align-"+(He.align||"c")],[ct]),Ne.push(ct),(n<l-1||e.hskipBeforeAndAfter)&&(lt=N.deflt(He.postgap,y),lt!==0&&(pe=b.makeSpan(["arraycolsep"],[]),pe.style.width=lt+"em",Ne.push(pe)))}}if(m=b.makeSpan(["mtable"],Ne),u.length>0){for(var Ui=b.makeLineSpan("hline",t,f),Yi=b.makeLineSpan("hdashline",t,f),O0=[{type:"elem",elem:m,shift:0}];u.length>0;){var An=u.pop(),Mn=An.pos-de;An.isDashed?O0.push({type:"elem",elem:Yi,shift:Mn}):O0.push({type:"elem",elem:Ui,shift:Mn})}m=b.makeVList({positionType:"individualShift",children:O0},t)}if(e.addEqnNum){var F0=b.makeVList({positionType:"individualShift",children:Sn},t);return F0=b.makeSpan(["tag"],[F0],t),b.makeFragment([m,F0])}else return b.makeSpan(["mord"],[m],t)},xi={c:"center ",l:"left ",r:"right "},Be=function(e,t){for(var r=[],n=new z.MathNode("mtd",[],["mtr-glue"]),o=new z.MathNode("mtd",[],["mml-eqn-num"]),a=0;a<e.body.length;a++){for(var l=e.body[a],m=[],u=0;u<l.length;u++)m.push(new z.MathNode("mtd",[$(l[u],t)]));e.addEqnNum&&(m.unshift(n),m.push(n),e.leqno?m.unshift(o):m.push(o)),r.push(new z.MathNode("mtr",m))}var f=new z.MathNode("mtable",r),x=e.arraystretch===.5?.1:.16+e.arraystretch-1+(e.addJot?.09:0);f.setAttribute("rowspacing",x.toFixed(4)+"em");var y="",S="";if(e.cols&&e.cols.length>0){var w=e.cols,A="",M=!1,B=0,q=w.length;w[0].type==="separator"&&(y+="top ",B=1),w[w.length-1].type==="separator"&&(y+="bottom ",q-=1);for(var I=B;I<q;I++)w[I].type==="align"?(S+=xi[w[I].align],M&&(A+="none "),M=!0):w[I].type==="separator"&&M&&(A+=w[I].separator==="|"?"solid ":"dashed ",M=!1);f.setAttribute("columnalign",S.trim()),/[sd]/.test(A)&&f.setAttribute("columnlines",A.trim())}if(e.colSeparationType==="align"){for(var W=e.cols||[],Y="",Z=1;Z<W.length;Z++)Y+=Z%2?"0em ":"1em ";f.setAttribute("columnspacing",Y.trim())}else e.colSeparationType==="alignat"||e.colSeparationType==="gather"?f.setAttribute("columnspacing","0em"):e.colSeparationType==="small"?f.setAttribute("columnspacing","0.2778em"):e.colSeparationType==="CD"?f.setAttribute("columnspacing","0.5em"):f.setAttribute("columnspacing","1em");var G="",U=e.hLinesBeforeRow;y+=U[0].length>0?"left ":"",y+=U[U.length-1].length>0?"right ":"";for(var te=1;te<U.length-1;te++)G+=U[te].length===0?"none ":U[te][0]?"dashed ":"solid ";return/[sd]/.test(G)&&f.setAttribute("rowlines",G.trim()),y!==""&&(f=new z.MathNode("menclose",[f]),f.setAttribute("notation",y.trim())),e.arraystretch&&e.arraystretch<1&&(f=new z.MathNode("mstyle",[f]),f.setAttribute("scriptlevel","1")),f},Fr=function(e,t){e.envName.indexOf("ed")===-1&&_t(e);var r=[],n=e.envName.indexOf("at")>-1?"alignat":"align",o=We(e.parser,{cols:r,addJot:!0,addEqnNum:e.envName==="align"||e.envName==="alignat",colSeparationType:n,maxNumCols:e.envName==="split"?2:void 0,leqno:e.parser.settings.leqno},"display"),a,l=0,m={type:"ordgroup",mode:e.mode,body:[]};if(t[0]&&t[0].type==="ordgroup"){for(var u="",f=0;f<t[0].body.length;f++){var x=E(t[0].body[f],"textord");u+=x.text}a=Number(u),l=a*2}var y=!l;o.body.forEach(function(M){for(var B=1;B<M.length;B+=2){var q=E(M[B],"styling"),I=E(q.body[0],"ordgroup");I.body.unshift(m)}if(y)l<M.length&&(l=M.length);else{var W=M.length/2;if(a<W)throw new v("Too many math in a row: "+("expected "+a+", but got "+W),M[0])}});for(var S=0;S<l;++S){var w="r",A=0;S%2===1?w="l":S>0&&y&&(A=1),r[S]={type:"align",align:w,pregap:A,postgap:0}}return o.colSeparationType=y?"align":"alignat",o};Te({type:"array",names:["array","darray"],props:{numArgs:1},handler:function(e,t){var r=Ot(t[0]),n=r?[t[0]]:E(t[0],"ordgroup").body,o=n.map(function(l){var m=u0(l),u=m.text;if("lcr".indexOf(u)!==-1)return{type:"align",align:u};if(u==="|")return{type:"separator",separator:"|"};if(u===":")return{type:"separator",separator:":"};throw new v("Unknown column alignment: "+u,l)}),a={cols:o,hskipBeforeAndAfter:!0,maxNumCols:o.length};return We(e.parser,a,z0(e.envName))},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["matrix","pmatrix","bmatrix","Bmatrix","vmatrix","Vmatrix","matrix*","pmatrix*","bmatrix*","Bmatrix*","vmatrix*","Vmatrix*"],props:{numArgs:0},handler:function(e){var t={matrix:null,pmatrix:["(",")"],bmatrix:["[","]"],Bmatrix:["\\{","\\}"],vmatrix:["|","|"],Vmatrix:["\\Vert","\\Vert"]}[e.envName.replace("*","")],r="c",n={hskipBeforeAndAfter:!1,cols:[{type:"align",align:r}]};if(e.envName.charAt(e.envName.length-1)==="*"){var o=e.parser;if(o.consumeSpaces(),o.fetch().text==="["){if(o.consume(),o.consumeSpaces(),r=o.fetch().text,"lcr".indexOf(r)===-1)throw new v("Expected l or c or r",o.nextToken);o.consume(),o.consumeSpaces(),o.expect("]"),o.consume(),n.cols=[{type:"align",align:r}]}}var a=We(e.parser,n,z0(e.envName));return a.cols=new Array(a.body[0].length).fill({type:"align",align:r}),t?{type:"leftright",mode:e.mode,body:[a],left:t[0],right:t[1],rightColor:void 0}:a},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["smallmatrix"],props:{numArgs:0},handler:function(e){var t={arraystretch:.5},r=We(e.parser,t,"script");return r.colSeparationType="small",r},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["subarray"],props:{numArgs:1},handler:function(e,t){var r=Ot(t[0]),n=r?[t[0]]:E(t[0],"ordgroup").body,o=n.map(function(l){var m=u0(l),u=m.text;if("lc".indexOf(u)!==-1)return{type:"align",align:u};throw new v("Unknown column alignment: "+u,l)});if(o.length>1)throw new v("{subarray} can contain only one column");var a={cols:o,hskipBeforeAndAfter:!1,arraystretch:.5};if(a=We(e.parser,a,"script"),a.body.length>0&&a.body[0].length>1)throw new v("{subarray} can contain only one column");return a},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["cases","dcases","rcases","drcases"],props:{numArgs:0},handler:function(e){var t={arraystretch:1.2,cols:[{type:"align",align:"l",pregap:0,postgap:1},{type:"align",align:"l",pregap:0,postgap:0}]},r=We(e.parser,t,z0(e.envName));return{type:"leftright",mode:e.mode,body:[r],left:e.envName.indexOf("r")>-1?".":"\\{",right:e.envName.indexOf("r")>-1?"\\}":".",rightColor:void 0}},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["align","align*","aligned","split"],props:{numArgs:0},handler:Fr,htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["gathered","gather","gather*"],props:{numArgs:0},handler:function(e){N.contains(["gather","gather*"],e.envName)&&_t(e);var t={cols:[{type:"align",align:"c"}],addJot:!0,colSeparationType:"gather",addEqnNum:e.envName==="gather",leqno:e.parser.settings.leqno};return We(e.parser,t,"display")},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["alignat","alignat*","alignedat"],props:{numArgs:1},handler:Fr,htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["equation","equation*"],props:{numArgs:0},handler:function(e){_t(e);var t={addEqnNum:e.envName==="equation",singleRow:!0,maxNumCols:1,leqno:e.parser.settings.leqno};return We(e.parser,t,"display")},htmlBuilder:Ce,mathmlBuilder:Be}),Te({type:"array",names:["CD"],props:{numArgs:0},handler:function(e){return _t(e),ni(e.parser)},htmlBuilder:Ce,mathmlBuilder:Be}),T({type:"text",names:["\\hline","\\hdashline"],props:{numArgs:0,allowedInText:!0,allowedInMath:!0},handler:function(e,t){throw new v(e.funcName+" valid only within array environment")}});var bi=Pr,Vr=bi;T({type:"environment",names:["\\begin","\\end"],props:{numArgs:1,argTypes:["text"]},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];if(o.type!=="ordgroup")throw new v("Invalid environment name",o);for(var a="",l=0;l<o.body.length;++l)a+=E(o.body[l],"textord").text;if(n==="\\begin"){if(!Vr.hasOwnProperty(a))throw new v("No such environment: "+a,o);var m=Vr[a],u=r.parseArguments("\\begin{"+a+"}",m),f=u.args,x=u.optArgs,y={mode:r.mode,envName:a,parser:r},S=m.handler(y,f,x);r.expect("\\end",!1);var w=r.nextToken,A=E(r.parseFunction(),"environment");if(A.name!==a)throw new v("Mismatch: \\begin{"+a+"} matched by \\end{"+A.name+"}",w);return S}return{type:"environment",mode:r.mode,name:a,nameGroup:o}}});var yi=b.makeSpan;function Wr(e,t){var r=ie(e.body,t,!0);return yi([e.mclass],r,t)}function _r(e,t){var r,n=he(e.body,t);return e.mclass==="minner"?z.newDocumentFragment(n):(e.mclass==="mord"?e.isCharacterBox?(r=n[0],r.type="mi"):r=new z.MathNode("mi",n):(e.isCharacterBox?(r=n[0],r.type="mo"):r=new z.MathNode("mo",n),e.mclass==="mbin"?(r.attributes.lspace="0.22em",r.attributes.rspace="0.22em"):e.mclass==="mpunct"?(r.attributes.lspace="0em",r.attributes.rspace="0.17em"):(e.mclass==="mopen"||e.mclass==="mclose")&&(r.attributes.lspace="0em",r.attributes.rspace="0em")),r)}T({type:"mclass",names:["\\mathord","\\mathbin","\\mathrel","\\mathopen","\\mathclose","\\mathpunct","\\mathinner"],props:{numArgs:1,primitive:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"mclass",mode:r.mode,mclass:"m"+n.substr(5),body:oe(o),isCharacterBox:N.isCharacterBox(o)}},htmlBuilder:Wr,mathmlBuilder:_r});var A0=function(e){var t=e.type==="ordgroup"&&e.body.length?e.body[0]:e;return t.type==="atom"&&(t.family==="bin"||t.family==="rel")?"m"+t.family:"mord"};T({type:"mclass",names:["\\@binrel"],props:{numArgs:2},handler:function(e,t){var r=e.parser;return{type:"mclass",mode:r.mode,mclass:A0(t[0]),body:oe(t[1]),isCharacterBox:N.isCharacterBox(t[1])}}}),T({type:"mclass",names:["\\stackrel","\\overset","\\underset"],props:{numArgs:2},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[1],a=t[0],l;n!=="\\stackrel"?l=A0(o):l="mrel";var m={type:"op",mode:o.mode,limits:!0,alwaysHandleSupSub:!0,parentIsSupSub:!1,symbol:!1,suppressBaseShift:n!=="\\stackrel",body:oe(o)},u={type:"supsub",mode:a.mode,base:m,sup:n==="\\underset"?null:a,sub:n==="\\underset"?a:null};return{type:"mclass",mode:r.mode,mclass:l,body:[u],isCharacterBox:N.isCharacterBox(u)}},htmlBuilder:Wr,mathmlBuilder:_r});var Gr=function(e,t){var r=e.font,n=t.withFont(r);return _(e.body,n)},Ur=function(e,t){var r=e.font,n=t.withFont(r);return $(e.body,n)},Yr={"\\Bbb":"\\mathbb","\\bold":"\\mathbf","\\frak":"\\mathfrak","\\bm":"\\boldsymbol"};T({type:"font",names:["\\mathrm","\\mathit","\\mathbf","\\mathnormal","\\mathbb","\\mathcal","\\mathfrak","\\mathscr","\\mathsf","\\mathtt","\\Bbb","\\bold","\\frak"],props:{numArgs:1,allowedInArgument:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=Et(t[0]),a=n;return a in Yr&&(a=Yr[a]),{type:"font",mode:r.mode,font:a.slice(1),body:o}},htmlBuilder:Gr,mathmlBuilder:Ur}),T({type:"mclass",names:["\\boldsymbol","\\bm"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=t[0],o=N.isCharacterBox(n);return{type:"mclass",mode:r.mode,mclass:A0(n),body:[{type:"font",mode:r.mode,font:"boldsymbol",body:n}],isCharacterBox:o}}}),T({type:"font",names:["\\rm","\\sf","\\tt","\\bf","\\it","\\cal"],props:{numArgs:0,allowedInText:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=e.breakOnTokenText,a=r.mode,l=r.parseExpression(!0,o),m="math"+n.slice(1);return{type:"font",mode:a,font:m,body:{type:"ordgroup",mode:r.mode,body:l}}},htmlBuilder:Gr,mathmlBuilder:Ur});var $r=function(e,t){var r=t;return e==="display"?r=r.id>=D.SCRIPT.id?r.text():D.DISPLAY:e==="text"&&r.size===D.DISPLAY.size?r=D.TEXT:e==="script"?r=D.SCRIPT:e==="scriptscript"&&(r=D.SCRIPTSCRIPT),r},M0=function(e,t){var r=$r(e.size,t.style),n=r.fracNum(),o=r.fracDen(),a;a=t.havingStyle(n);var l=_(e.numer,a,t);if(e.continued){var m=8.5/t.fontMetrics().ptPerEm,u=3.5/t.fontMetrics().ptPerEm;l.height=l.height<m?m:l.height,l.depth=l.depth<u?u:l.depth}a=t.havingStyle(o);var f=_(e.denom,a,t),x,y,S;e.hasBarLine?(e.barSize?(y=J(e.barSize,t),x=b.makeLineSpan("frac-line",t,y)):x=b.makeLineSpan("frac-line",t),y=x.height,S=x.height):(x=null,y=0,S=t.fontMetrics().defaultRuleThickness);var w,A,M;r.size===D.DISPLAY.size||e.size==="display"?(w=t.fontMetrics().num1,y>0?A=3*S:A=7*S,M=t.fontMetrics().denom1):(y>0?(w=t.fontMetrics().num2,A=S):(w=t.fontMetrics().num3,A=3*S),M=t.fontMetrics().denom2);var B;if(x){var q=t.fontMetrics().axisHeight;w-l.depth-(q+.5*y)<A&&(w+=A-(w-l.depth-(q+.5*y))),q-.5*y-(f.height-M)<A&&(M+=A-(q-.5*y-(f.height-M)));var I=-(q-.5*y);B=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:f,shift:M},{type:"elem",elem:x,shift:I},{type:"elem",elem:l,shift:-w}]},t)}else{var W=w-l.depth-(f.height-M);W<A&&(w+=.5*(A-W),M+=.5*(A-W)),B=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:f,shift:M},{type:"elem",elem:l,shift:-w}]},t)}a=t.havingStyle(r),B.height*=a.sizeMultiplier/t.sizeMultiplier,B.depth*=a.sizeMultiplier/t.sizeMultiplier;var Y;r.size===D.DISPLAY.size?Y=t.fontMetrics().delim1:Y=t.fontMetrics().delim2;var Z,G;return e.leftDelim==null?Z=ft(t,["mopen"]):Z=Me.customSizedDelim(e.leftDelim,Y,!0,t.havingStyle(r),e.mode,["mopen"]),e.continued?G=b.makeSpan([]):e.rightDelim==null?G=ft(t,["mclose"]):G=Me.customSizedDelim(e.rightDelim,Y,!0,t.havingStyle(r),e.mode,["mclose"]),b.makeSpan(["mord"].concat(a.sizingClasses(t)),[Z,b.makeSpan(["mfrac"],[B]),G],t)},T0=function(e,t){var r=new z.MathNode("mfrac",[$(e.numer,t),$(e.denom,t)]);if(!e.hasBarLine)r.setAttribute("linethickness","0px");else if(e.barSize){var n=J(e.barSize,t);r.setAttribute("linethickness",n+"em")}var o=$r(e.size,t.style);if(o.size!==t.style.size){r=new z.MathNode("mstyle",[r]);var a=o.size===D.DISPLAY.size?"true":"false";r.setAttribute("displaystyle",a),r.setAttribute("scriptlevel","0")}if(e.leftDelim!=null||e.rightDelim!=null){var l=[];if(e.leftDelim!=null){var m=new z.MathNode("mo",[new z.TextNode(e.leftDelim.replace("\\",""))]);m.setAttribute("fence","true"),l.push(m)}if(l.push(r),e.rightDelim!=null){var u=new z.MathNode("mo",[new z.TextNode(e.rightDelim.replace("\\",""))]);u.setAttribute("fence","true"),l.push(u)}return m0(l)}return r};T({type:"genfrac",names:["\\dfrac","\\frac","\\tfrac","\\dbinom","\\binom","\\tbinom","\\\\atopfrac","\\\\bracefrac","\\\\brackfrac"],props:{numArgs:2,allowedInArgument:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0],a=t[1],l,m=null,u=null,f="auto";switch(n){case"\\dfrac":case"\\frac":case"\\tfrac":l=!0;break;case"\\\\atopfrac":l=!1;break;case"\\dbinom":case"\\binom":case"\\tbinom":l=!1,m="(",u=")";break;case"\\\\bracefrac":l=!1,m="\\{",u="\\}";break;case"\\\\brackfrac":l=!1,m="[",u="]";break;default:throw new Error("Unrecognized genfrac command")}switch(n){case"\\dfrac":case"\\dbinom":f="display";break;case"\\tfrac":case"\\tbinom":f="text";break}return{type:"genfrac",mode:r.mode,continued:!1,numer:o,denom:a,hasBarLine:l,leftDelim:m,rightDelim:u,size:f,barSize:null}},htmlBuilder:M0,mathmlBuilder:T0}),T({type:"genfrac",names:["\\cfrac"],props:{numArgs:2},handler:function(e,t){var r=e.parser;e.funcName;var n=t[0],o=t[1];return{type:"genfrac",mode:r.mode,continued:!0,numer:n,denom:o,hasBarLine:!0,leftDelim:null,rightDelim:null,size:"display",barSize:null}}}),T({type:"infix",names:["\\over","\\choose","\\atop","\\brace","\\brack"],props:{numArgs:0,infix:!0},handler:function(e){var t=e.parser,r=e.funcName,n=e.token,o;switch(r){case"\\over":o="\\frac";break;case"\\choose":o="\\binom";break;case"\\atop":o="\\\\atopfrac";break;case"\\brace":o="\\\\bracefrac";break;case"\\brack":o="\\\\brackfrac";break;default:throw new Error("Unrecognized infix genfrac command")}return{type:"infix",mode:t.mode,replaceWith:o,token:n}}});var jr=["display","text","script","scriptscript"],Xr=function(e){var t=null;return e.length>0&&(t=e,t=t==="."?null:t),t};T({type:"genfrac",names:["\\genfrac"],props:{numArgs:6,allowedInArgument:!0,argTypes:["math","math","size","text","math","math"]},handler:function(e,t){var r=e.parser,n=t[4],o=t[5],a=Et(t[0]),l=a.type==="atom"&&a.family==="open"?Xr(a.text):null,m=Et(t[1]),u=m.type==="atom"&&m.family==="close"?Xr(m.text):null,f=E(t[2],"size"),x,y=null;f.isBlank?x=!0:(y=f.value,x=y.number>0);var S="auto",w=t[3];if(w.type==="ordgroup"){if(w.body.length>0){var A=E(w.body[0],"textord");S=jr[Number(A.text)]}}else w=E(w,"textord"),S=jr[Number(w.text)];return{type:"genfrac",mode:r.mode,numer:n,denom:o,continued:!1,hasBarLine:x,barSize:y,leftDelim:l,rightDelim:u,size:S}},htmlBuilder:M0,mathmlBuilder:T0}),T({type:"infix",names:["\\above"],props:{numArgs:1,argTypes:["size"],infix:!0},handler:function(e,t){var r=e.parser;e.funcName;var n=e.token;return{type:"infix",mode:r.mode,replaceWith:"\\\\abovefrac",size:E(t[0],"size").value,token:n}}}),T({type:"genfrac",names:["\\\\abovefrac"],props:{numArgs:3,argTypes:["math","size","math"]},handler:function(e,t){var r=e.parser;e.funcName;var n=t[0],o=Xn(E(t[1],"infix").size),a=t[2],l=o.number>0;return{type:"genfrac",mode:r.mode,numer:n,denom:a,continued:!1,hasBarLine:l,barSize:o,leftDelim:null,rightDelim:null,size:"auto"}},htmlBuilder:M0,mathmlBuilder:T0});var Zr=function(e,t){var r=t.style,n,o;e.type==="supsub"?(n=e.sup?_(e.sup,t.havingStyle(r.sup()),t):_(e.sub,t.havingStyle(r.sub()),t),o=E(e.base,"horizBrace")):o=E(e,"horizBrace");var a=_(o.base,t.havingBaseStyle(D.DISPLAY)),l=Ie.svgSpan(o,t),m;if(o.isOver?(m=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:a},{type:"kern",size:.1},{type:"elem",elem:l}]},t),m.children[0].children[0].children[1].classes.push("svg-align")):(m=b.makeVList({positionType:"bottom",positionData:a.depth+.1+l.height,children:[{type:"elem",elem:l},{type:"kern",size:.1},{type:"elem",elem:a}]},t),m.children[0].children[0].children[0].classes.push("svg-align")),n){var u=b.makeSpan(["mord",o.isOver?"mover":"munder"],[m],t);o.isOver?m=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:u},{type:"kern",size:.2},{type:"elem",elem:n}]},t):m=b.makeVList({positionType:"bottom",positionData:u.depth+.2+n.height+n.depth,children:[{type:"elem",elem:n},{type:"kern",size:.2},{type:"elem",elem:u}]},t)}return b.makeSpan(["mord",o.isOver?"mover":"munder"],[m],t)},vi=function(e,t){var r=Ie.mathMLnode(e.label);return new z.MathNode(e.isOver?"mover":"munder",[$(e.base,t),r])};T({type:"horizBrace",names:["\\overbrace","\\underbrace"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=e.funcName;return{type:"horizBrace",mode:r.mode,label:n,isOver:/^\\over/.test(n),base:t[0]}},htmlBuilder:Zr,mathmlBuilder:vi}),T({type:"href",names:["\\href"],props:{numArgs:2,argTypes:["url","original"],allowedInText:!0},handler:function(e,t){var r=e.parser,n=t[1],o=E(t[0],"url").url;return r.settings.isTrusted({command:"\\href",url:o})?{type:"href",mode:r.mode,href:o,body:oe(n)}:r.formatUnsupportedCmd("\\href")},htmlBuilder:function(e,t){var r=ie(e.body,t,!1);return b.makeAnchor(e.href,[],r,t)},mathmlBuilder:function(e,t){var r=Ve(e.body,t);return r instanceof xe||(r=new xe("mrow",[r])),r.setAttribute("href",e.href),r}}),T({type:"href",names:["\\url"],props:{numArgs:1,argTypes:["url"],allowedInText:!0},handler:function(e,t){var r=e.parser,n=E(t[0],"url").url;if(!r.settings.isTrusted({command:"\\url",url:n}))return r.formatUnsupportedCmd("\\url");for(var o=[],a=0;a<n.length;a++){var l=n[a];l==="~"&&(l="\\textasciitilde"),o.push({type:"textord",mode:"text",text:l})}var m={type:"text",mode:r.mode,font:"\\texttt",body:o};return{type:"href",mode:r.mode,href:n,body:oe(m)}}}),T({type:"hbox",names:["\\hbox"],props:{numArgs:1,argTypes:["text"],allowedInText:!0,primitive:!0},handler:function(e,t){var r=e.parser;return{type:"hbox",mode:r.mode,body:oe(t[0])}},htmlBuilder:function(e,t){var r=ie(e.body,t,!1);return b.makeFragment(r)},mathmlBuilder:function(e,t){return new z.MathNode("mrow",he(e.body,t))}}),T({type:"html",names:["\\htmlClass","\\htmlId","\\htmlStyle","\\htmlData"],props:{numArgs:2,argTypes:["raw","original"],allowedInText:!0},handler:function(e,t){var r=e.parser,n=e.funcName;e.token;var o=E(t[0],"raw").string,a=t[1];r.settings.strict&&r.settings.reportNonstrict("htmlExtension","HTML extension is disabled on strict mode");var l,m={};switch(n){case"\\htmlClass":m.class=o,l={command:"\\htmlClass",class:o};break;case"\\htmlId":m.id=o,l={command:"\\htmlId",id:o};break;case"\\htmlStyle":m.style=o,l={command:"\\htmlStyle",style:o};break;case"\\htmlData":{for(var u=o.split(","),f=0;f<u.length;f++){var x=u[f].split("=");if(x.length!==2)throw new v("Error parsing key-value for \\htmlData");m["data-"+x[0].trim()]=x[1].trim()}l={command:"\\htmlData",attributes:m};break}default:throw new Error("Unrecognized html command")}return r.settings.isTrusted(l)?{type:"html",mode:r.mode,attributes:m,body:oe(a)}:r.formatUnsupportedCmd(n)},htmlBuilder:function(e,t){var r=ie(e.body,t,!1),n=["enclosing"];e.attributes.class&&n.push.apply(n,e.attributes.class.trim().split(/\s+/));var o=b.makeSpan(n,r,t);for(var a in e.attributes)a!=="class"&&e.attributes.hasOwnProperty(a)&&o.setAttribute(a,e.attributes[a]);return o},mathmlBuilder:function(e,t){return Ve(e.body,t)}}),T({type:"htmlmathml",names:["\\html@mathml"],props:{numArgs:2,allowedInText:!0},handler:function(e,t){var r=e.parser;return{type:"htmlmathml",mode:r.mode,html:oe(t[0]),mathml:oe(t[1])}},htmlBuilder:function(e,t){var r=ie(e.html,t,!1);return b.makeFragment(r)},mathmlBuilder:function(e,t){return Ve(e.mathml,t)}});var C0=function(e){if(/^[-+]? *(\d+(\.\d*)?|\.\d+)$/.test(e))return{number:+e,unit:"bp"};var t=/([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(e);if(!t)throw new v("Invalid size: '"+e+"' in \\includegraphics");var r={number:+(t[1]+t[2]),unit:t[3]};if(!cr(r))throw new v("Invalid unit: '"+r.unit+"' in \\includegraphics.");return r};T({type:"includegraphics",names:["\\includegraphics"],props:{numArgs:1,numOptionalArgs:1,argTypes:["raw","url"],allowedInText:!1},handler:function(e,t,r){var n=e.parser,o={number:0,unit:"em"},a={number:.9,unit:"em"},l={number:0,unit:"em"},m="";if(r[0])for(var u=E(r[0],"raw").string,f=u.split(","),x=0;x<f.length;x++){var y=f[x].split("=");if(y.length===2){var S=y[1].trim();switch(y[0].trim()){case"alt":m=S;break;case"width":o=C0(S);break;case"height":a=C0(S);break;case"totalheight":l=C0(S);break;default:throw new v("Invalid key: '"+y[0]+"' in \\includegraphics.")}}}var w=E(t[0],"url").url;return m===""&&(m=w,m=m.replace(/^.*[\\/]/,""),m=m.substring(0,m.lastIndexOf("."))),n.settings.isTrusted({command:"\\includegraphics",url:w})?{type:"includegraphics",mode:n.mode,alt:m,width:o,height:a,totalheight:l,src:w}:n.formatUnsupportedCmd("\\includegraphics")},htmlBuilder:function(e,t){var r=J(e.height,t),n=0;e.totalheight.number>0&&(n=J(e.totalheight,t)-r,n=Number(n.toFixed(2)));var o=0;e.width.number>0&&(o=J(e.width,t));var a={height:r+n+"em"};o>0&&(a.width=o+"em"),n>0&&(a.verticalAlign=-n+"em");var l=new po(e.src,e.alt,a);return l.height=r,l.depth=n,l},mathmlBuilder:function(e,t){var r=new z.MathNode("mglyph",[]);r.setAttribute("alt",e.alt);var n=J(e.height,t),o=0;if(e.totalheight.number>0&&(o=J(e.totalheight,t)-n,o=o.toFixed(2),r.setAttribute("valign","-"+o+"em")),r.setAttribute("height",n+o+"em"),e.width.number>0){var a=J(e.width,t);r.setAttribute("width",a+"em")}return r.setAttribute("src",e.src),r}}),T({type:"kern",names:["\\kern","\\mkern","\\hskip","\\mskip"],props:{numArgs:1,argTypes:["size"],primitive:!0,allowedInText:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=E(t[0],"size");if(r.settings.strict){var a=n[1]==="m",l=o.value.unit==="mu";a?(l||r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" supports only mu units, "+("not "+o.value.unit+" units")),r.mode!=="math"&&r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" works only in math mode")):l&&r.settings.reportNonstrict("mathVsTextUnits","LaTeX's "+n+" doesn't support mu units")}return{type:"kern",mode:r.mode,dimension:o.value}},htmlBuilder:function(e,t){return b.makeGlue(e.dimension,t)},mathmlBuilder:function(e,t){var r=J(e.dimension,t);return new z.SpaceNode(r)}}),T({type:"lap",names:["\\mathllap","\\mathrlap","\\mathclap"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"lap",mode:r.mode,alignment:n.slice(5),body:o}},htmlBuilder:function(e,t){var r;e.alignment==="clap"?(r=b.makeSpan([],[_(e.body,t)]),r=b.makeSpan(["inner"],[r],t)):r=b.makeSpan(["inner"],[_(e.body,t)]);var n=b.makeSpan(["fix"],[]),o=b.makeSpan([e.alignment],[r,n],t),a=b.makeSpan(["strut"]);return a.style.height=o.height+o.depth+"em",a.style.verticalAlign=-o.depth+"em",o.children.unshift(a),o=b.makeSpan(["thinbox"],[o],t),b.makeSpan(["mord","vbox"],[o],t)},mathmlBuilder:function(e,t){var r=new z.MathNode("mpadded",[$(e.body,t)]);if(e.alignment!=="rlap"){var n=e.alignment==="llap"?"-1":"-0.5";r.setAttribute("lspace",n+"width")}return r.setAttribute("width","0px"),r}}),T({type:"styling",names:["\\(","$"],props:{numArgs:0,allowedInText:!0,allowedInMath:!1},handler:function(e,t){var r=e.funcName,n=e.parser,o=n.mode;n.switchMode("math");var a=r==="\\("?"\\)":"$",l=n.parseExpression(!1,a);return n.expect(a),n.switchMode(o),{type:"styling",mode:n.mode,style:"text",body:l}}}),T({type:"text",names:["\\)","\\]"],props:{numArgs:0,allowedInText:!0,allowedInMath:!1},handler:function(e,t){throw new v("Mismatched "+e.funcName)}});var Kr=function(e,t){switch(t.style.size){case D.DISPLAY.size:return e.display;case D.TEXT.size:return e.text;case D.SCRIPT.size:return e.script;case D.SCRIPTSCRIPT.size:return e.scriptscript;default:return e.text}};T({type:"mathchoice",names:["\\mathchoice"],props:{numArgs:4,primitive:!0},handler:function(e,t){var r=e.parser;return{type:"mathchoice",mode:r.mode,display:oe(t[0]),text:oe(t[1]),script:oe(t[2]),scriptscript:oe(t[3])}},htmlBuilder:function(e,t){var r=Kr(e,t),n=ie(r,t,!1);return b.makeFragment(n)},mathmlBuilder:function(e,t){var r=Kr(e,t);return Ve(r,t)}});var Qr=function(e,t,r,n,o,a,l){e=b.makeSpan([],[e]);var m,u;if(t){var f=_(t,n.havingStyle(o.sup()),n);u={elem:f,kern:Math.max(n.fontMetrics().bigOpSpacing1,n.fontMetrics().bigOpSpacing3-f.depth)}}if(r){var x=_(r,n.havingStyle(o.sub()),n);m={elem:x,kern:Math.max(n.fontMetrics().bigOpSpacing2,n.fontMetrics().bigOpSpacing4-x.height)}}var y;if(u&&m){var S=n.fontMetrics().bigOpSpacing5+m.elem.height+m.elem.depth+m.kern+e.depth+l;y=b.makeVList({positionType:"bottom",positionData:S,children:[{type:"kern",size:n.fontMetrics().bigOpSpacing5},{type:"elem",elem:m.elem,marginLeft:-a+"em"},{type:"kern",size:m.kern},{type:"elem",elem:e},{type:"kern",size:u.kern},{type:"elem",elem:u.elem,marginLeft:a+"em"},{type:"kern",size:n.fontMetrics().bigOpSpacing5}]},n)}else if(m){var w=e.height-l;y=b.makeVList({positionType:"top",positionData:w,children:[{type:"kern",size:n.fontMetrics().bigOpSpacing5},{type:"elem",elem:m.elem,marginLeft:-a+"em"},{type:"kern",size:m.kern},{type:"elem",elem:e}]},n)}else if(u){var A=e.depth+l;y=b.makeVList({positionType:"bottom",positionData:A,children:[{type:"elem",elem:e},{type:"kern",size:u.kern},{type:"elem",elem:u.elem,marginLeft:a+"em"},{type:"kern",size:n.fontMetrics().bigOpSpacing5}]},n)}else return e;return b.makeSpan(["mop","op-limits"],[y],n)},Jr=["\\smallint"],it=function(e,t){var r,n,o=!1,a;e.type==="supsub"?(r=e.sup,n=e.sub,a=E(e.base,"op"),o=!0):a=E(e,"op");var l=t.style,m=!1;l.size===D.DISPLAY.size&&a.symbol&&!N.contains(Jr,a.name)&&(m=!0);var u;if(a.symbol){var f=m?"Size2-Regular":"Size1-Regular",x="";if((a.name==="\\oiint"||a.name==="\\oiiint")&&(x=a.name.substr(1),a.name=x==="oiint"?"\\iint":"\\iiint"),u=b.makeSymbol(a.name,f,"math",t,["mop","op-symbol",m?"large-op":"small-op"]),x.length>0){var y=u.italic,S=b.staticSvg(x+"Size"+(m?"2":"1"),t);u=b.makeVList({positionType:"individualShift",children:[{type:"elem",elem:u,shift:0},{type:"elem",elem:S,shift:m?.08:0}]},t),a.name="\\"+x,u.classes.unshift("mop"),u.italic=y}}else if(a.body){var w=ie(a.body,t,!0);w.length===1&&w[0]instanceof ge?(u=w[0],u.classes[0]="mop"):u=b.makeSpan(["mop"],w,t)}else{for(var A=[],M=1;M<a.name.length;M++)A.push(b.mathsym(a.name[M],a.mode,t));u=b.makeSpan(["mop"],A,t)}var B=0,q=0;return(u instanceof ge||a.name==="\\oiint"||a.name==="\\oiiint")&&!a.suppressBaseShift&&(B=(u.height-u.depth)/2-t.fontMetrics().axisHeight,q=u.italic),o?Qr(u,r,n,t,l,q,B):(B&&(u.style.position="relative",u.style.top=B+"em"),u)},yt=function(e,t){var r;if(e.symbol)r=new xe("mo",[be(e.name,e.mode)]),N.contains(Jr,e.name)&&r.setAttribute("largeop","false");else if(e.body)r=new xe("mo",he(e.body,t));else{r=new xe("mi",[new gt(e.name.slice(1))]);var n=new xe("mo",[be("\u2061","text")]);e.parentIsSupSub?r=new xe("mrow",[r,n]):r=xr([r,n])}return r},wi={"\u220F":"\\prod","\u2210":"\\coprod","\u2211":"\\sum","\u22C0":"\\bigwedge","\u22C1":"\\bigvee","\u22C2":"\\bigcap","\u22C3":"\\bigcup","\u2A00":"\\bigodot","\u2A01":"\\bigoplus","\u2A02":"\\bigotimes","\u2A04":"\\biguplus","\u2A06":"\\bigsqcup"};T({type:"op",names:["\\coprod","\\bigvee","\\bigwedge","\\biguplus","\\bigcap","\\bigcup","\\intop","\\prod","\\sum","\\bigotimes","\\bigoplus","\\bigodot","\\bigsqcup","\\smallint","\u220F","\u2210","\u2211","\u22C0","\u22C1","\u22C2","\u22C3","\u2A00","\u2A01","\u2A02","\u2A04","\u2A06"],props:{numArgs:0},handler:function(e,t){var r=e.parser,n=e.funcName,o=n;return o.length===1&&(o=wi[o]),{type:"op",mode:r.mode,limits:!0,parentIsSupSub:!1,symbol:!0,name:o}},htmlBuilder:it,mathmlBuilder:yt}),T({type:"op",names:["\\mathop"],props:{numArgs:1,primitive:!0},handler:function(e,t){var r=e.parser,n=t[0];return{type:"op",mode:r.mode,limits:!1,parentIsSupSub:!1,symbol:!1,body:oe(n)}},htmlBuilder:it,mathmlBuilder:yt});var ki={"\u222B":"\\int","\u222C":"\\iint","\u222D":"\\iiint","\u222E":"\\oint","\u222F":"\\oiint","\u2230":"\\oiiint"};T({type:"op",names:["\\arcsin","\\arccos","\\arctan","\\arctg","\\arcctg","\\arg","\\ch","\\cos","\\cosec","\\cosh","\\cot","\\cotg","\\coth","\\csc","\\ctg","\\cth","\\deg","\\dim","\\exp","\\hom","\\ker","\\lg","\\ln","\\log","\\sec","\\sin","\\sinh","\\sh","\\tan","\\tanh","\\tg","\\th"],props:{numArgs:0},handler:function(e){var t=e.parser,r=e.funcName;return{type:"op",mode:t.mode,limits:!1,parentIsSupSub:!1,symbol:!1,name:r}},htmlBuilder:it,mathmlBuilder:yt}),T({type:"op",names:["\\det","\\gcd","\\inf","\\lim","\\max","\\min","\\Pr","\\sup"],props:{numArgs:0},handler:function(e){var t=e.parser,r=e.funcName;return{type:"op",mode:t.mode,limits:!0,parentIsSupSub:!1,symbol:!1,name:r}},htmlBuilder:it,mathmlBuilder:yt}),T({type:"op",names:["\\int","\\iint","\\iiint","\\oint","\\oiint","\\oiiint","\u222B","\u222C","\u222D","\u222E","\u222F","\u2230"],props:{numArgs:0},handler:function(e){var t=e.parser,r=e.funcName,n=r;return n.length===1&&(n=ki[n]),{type:"op",mode:t.mode,limits:!1,parentIsSupSub:!1,symbol:!0,name:n}},htmlBuilder:it,mathmlBuilder:yt});var en=function(e,t){var r,n,o=!1,a;e.type==="supsub"?(r=e.sup,n=e.sub,a=E(e.base,"operatorname"),o=!0):a=E(e,"operatorname");var l;if(a.body.length>0){for(var m=a.body.map(function(y){var S=y.text;return typeof S=="string"?{type:"textord",mode:y.mode,text:S}:y}),u=ie(m,t.withFont("mathrm"),!0),f=0;f<u.length;f++){var x=u[f];x instanceof ge&&(x.text=x.text.replace(/\u2212/,"-").replace(/\u2217/,"*"))}l=b.makeSpan(["mop"],u,t)}else l=b.makeSpan(["mop"],[],t);return o?Qr(l,r,n,t,t.style,0,0):l},Si=function(e,t){for(var r=he(e.body,t.withFont("mathrm")),n=!0,o=0;o<r.length;o++){var a=r[o];if(!(a instanceof z.SpaceNode))if(a instanceof z.MathNode)switch(a.type){case"mi":case"mn":case"ms":case"mspace":case"mtext":break;case"mo":{var l=a.children[0];a.children.length===1&&l instanceof z.TextNode?l.text=l.text.replace(/\u2212/,"-").replace(/\u2217/,"*"):n=!1;break}default:n=!1}else n=!1}if(n){var m=r.map(function(x){return x.toText()}).join("");r=[new z.TextNode(m)]}var u=new z.MathNode("mi",r);u.setAttribute("mathvariant","normal");var f=new z.MathNode("mo",[be("\u2061","text")]);return e.parentIsSupSub?new z.MathNode("mrow",[u,f]):z.newDocumentFragment([u,f])};T({type:"operatorname",names:["\\operatorname","\\operatorname*"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"operatorname",mode:r.mode,body:oe(o),alwaysHandleSupSub:n==="\\operatorname*",limits:!1,parentIsSupSub:!1}},htmlBuilder:en,mathmlBuilder:Si}),Qe({type:"ordgroup",htmlBuilder:function(e,t){return e.semisimple?b.makeFragment(ie(e.body,t,!1)):b.makeSpan(["mord"],ie(e.body,t,!0),t)},mathmlBuilder:function(e,t){return Ve(e.body,t,!0)}}),T({type:"overline",names:["\\overline"],props:{numArgs:1},handler:function(e,t){var r=e.parser,n=t[0];return{type:"overline",mode:r.mode,body:n}},htmlBuilder:function(e,t){var r=_(e.body,t.havingCrampedStyle()),n=b.makeLineSpan("overline-line",t),o=t.fontMetrics().defaultRuleThickness,a=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r},{type:"kern",size:3*o},{type:"elem",elem:n},{type:"kern",size:o}]},t);return b.makeSpan(["mord","overline"],[a],t)},mathmlBuilder:function(e,t){var r=new z.MathNode("mo",[new z.TextNode("\u203E")]);r.setAttribute("stretchy","true");var n=new z.MathNode("mover",[$(e.body,t),r]);return n.setAttribute("accent","true"),n}}),T({type:"phantom",names:["\\phantom"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){var r=e.parser,n=t[0];return{type:"phantom",mode:r.mode,body:oe(n)}},htmlBuilder:function(e,t){var r=ie(e.body,t.withPhantom(),!1);return b.makeFragment(r)},mathmlBuilder:function(e,t){var r=he(e.body,t);return new z.MathNode("mphantom",r)}}),T({type:"hphantom",names:["\\hphantom"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){var r=e.parser,n=t[0];return{type:"hphantom",mode:r.mode,body:n}},htmlBuilder:function(e,t){var r=b.makeSpan([],[_(e.body,t.withPhantom())]);if(r.height=0,r.depth=0,r.children)for(var n=0;n<r.children.length;n++)r.children[n].height=0,r.children[n].depth=0;return r=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r}]},t),b.makeSpan(["mord"],[r],t)},mathmlBuilder:function(e,t){var r=he(oe(e.body),t),n=new z.MathNode("mphantom",r),o=new z.MathNode("mpadded",[n]);return o.setAttribute("height","0px"),o.setAttribute("depth","0px"),o}}),T({type:"vphantom",names:["\\vphantom"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){var r=e.parser,n=t[0];return{type:"vphantom",mode:r.mode,body:n}},htmlBuilder:function(e,t){var r=b.makeSpan(["inner"],[_(e.body,t.withPhantom())]),n=b.makeSpan(["fix"],[]);return b.makeSpan(["mord","rlap"],[r,n],t)},mathmlBuilder:function(e,t){var r=he(oe(e.body),t),n=new z.MathNode("mphantom",r),o=new z.MathNode("mpadded",[n]);return o.setAttribute("width","0px"),o}}),T({type:"raisebox",names:["\\raisebox"],props:{numArgs:2,argTypes:["size","hbox"],allowedInText:!0},handler:function(e,t){var r=e.parser,n=E(t[0],"size").value,o=t[1];return{type:"raisebox",mode:r.mode,dy:n,body:o}},htmlBuilder:function(e,t){var r=_(e.body,t),n=J(e.dy,t);return b.makeVList({positionType:"shift",positionData:-n,children:[{type:"elem",elem:r}]},t)},mathmlBuilder:function(e,t){var r=new z.MathNode("mpadded",[$(e.body,t)]),n=e.dy.number+e.dy.unit;return r.setAttribute("voffset",n),r}}),T({type:"rule",names:["\\rule"],props:{numArgs:2,numOptionalArgs:1,argTypes:["size","size","size"]},handler:function(e,t,r){var n=e.parser,o=r[0],a=E(t[0],"size"),l=E(t[1],"size");return{type:"rule",mode:n.mode,shift:o&&E(o,"size").value,width:a.value,height:l.value}},htmlBuilder:function(e,t){var r=b.makeSpan(["mord","rule"],[],t),n=J(e.width,t),o=J(e.height,t),a=e.shift?J(e.shift,t):0;return r.style.borderRightWidth=n+"em",r.style.borderTopWidth=o+"em",r.style.bottom=a+"em",r.width=n,r.height=o+a,r.depth=-a,r.maxFontSize=o*1.125*t.sizeMultiplier,r},mathmlBuilder:function(e,t){var r=J(e.width,t),n=J(e.height,t),o=e.shift?J(e.shift,t):0,a=t.color&&t.getColor()||"black",l=new z.MathNode("mspace");l.setAttribute("mathbackground",a),l.setAttribute("width",r+"em"),l.setAttribute("height",n+"em");var m=new z.MathNode("mpadded",[l]);return o>=0?m.setAttribute("height","+"+o+"em"):(m.setAttribute("height",o+"em"),m.setAttribute("depth","+"+-o+"em")),m.setAttribute("voffset",o+"em"),m}});function tn(e,t,r){for(var n=ie(e,t,!1),o=t.sizeMultiplier/r.sizeMultiplier,a=0;a<n.length;a++){var l=n[a].classes.indexOf("sizing");l<0?Array.prototype.push.apply(n[a].classes,t.sizingClasses(r)):n[a].classes[l+1]==="reset-size"+t.size&&(n[a].classes[l+1]="reset-size"+r.size),n[a].height*=o,n[a].depth*=o}return b.makeFragment(n)}var rn=["\\tiny","\\sixptsize","\\scriptsize","\\footnotesize","\\small","\\normalsize","\\large","\\Large","\\LARGE","\\huge","\\Huge"],zi=function(e,t){var r=t.havingSize(e.size);return tn(e.body,r,t)};T({type:"sizing",names:rn,props:{numArgs:0,allowedInText:!0},handler:function(e,t){var r=e.breakOnTokenText,n=e.funcName,o=e.parser,a=o.parseExpression(!1,r);return{type:"sizing",mode:o.mode,size:rn.indexOf(n)+1,body:a}},htmlBuilder:zi,mathmlBuilder:function(e,t){var r=t.havingSize(e.size),n=he(e.body,r),o=new z.MathNode("mstyle",n);return o.setAttribute("mathsize",r.sizeMultiplier+"em"),o}}),T({type:"smash",names:["\\smash"],props:{numArgs:1,numOptionalArgs:1,allowedInText:!0},handler:function(e,t,r){var n=e.parser,o=!1,a=!1,l=r[0]&&E(r[0],"ordgroup");if(l)for(var m="",u=0;u<l.body.length;++u){var f=l.body[u];if(m=f.text,m==="t")o=!0;else if(m==="b")a=!0;else{o=!1,a=!1;break}}else o=!0,a=!0;var x=t[0];return{type:"smash",mode:n.mode,body:x,smashHeight:o,smashDepth:a}},htmlBuilder:function(e,t){var r=b.makeSpan([],[_(e.body,t)]);if(!e.smashHeight&&!e.smashDepth)return r;if(e.smashHeight&&(r.height=0,r.children))for(var n=0;n<r.children.length;n++)r.children[n].height=0;if(e.smashDepth&&(r.depth=0,r.children))for(var o=0;o<r.children.length;o++)r.children[o].depth=0;var a=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r}]},t);return b.makeSpan(["mord"],[a],t)},mathmlBuilder:function(e,t){var r=new z.MathNode("mpadded",[$(e.body,t)]);return e.smashHeight&&r.setAttribute("height","0px"),e.smashDepth&&r.setAttribute("depth","0px"),r}}),T({type:"sqrt",names:["\\sqrt"],props:{numArgs:1,numOptionalArgs:1},handler:function(e,t,r){var n=e.parser,o=r[0],a=t[0];return{type:"sqrt",mode:n.mode,body:a,index:o}},htmlBuilder:function(e,t){var r=_(e.body,t.havingCrampedStyle());r.height===0&&(r.height=t.fontMetrics().xHeight),r=b.wrapFragment(r,t);var n=t.fontMetrics(),o=n.defaultRuleThickness,a=o;t.style.id<D.TEXT.id&&(a=t.fontMetrics().xHeight);var l=o+a/4,m=r.height+r.depth+l+o,u=Me.sqrtImage(m,t),f=u.span,x=u.ruleWidth,y=u.advanceWidth,S=f.height-x;S>r.height+r.depth+l&&(l=(l+S-r.height-r.depth)/2);var w=f.height-r.height-l-x;r.style.paddingLeft=y+"em";var A=b.makeVList({positionType:"firstBaseline",children:[{type:"elem",elem:r,wrapperClasses:["svg-align"]},{type:"kern",size:-(r.height+w)},{type:"elem",elem:f},{type:"kern",size:x}]},t);if(e.index){var M=t.havingStyle(D.SCRIPTSCRIPT),B=_(e.index,M,t),q=.6*(A.height-A.depth),I=b.makeVList({positionType:"shift",positionData:-q,children:[{type:"elem",elem:B}]},t),W=b.makeSpan(["root"],[I]);return b.makeSpan(["mord","sqrt"],[W,A],t)}else return b.makeSpan(["mord","sqrt"],[A],t)},mathmlBuilder:function(e,t){var r=e.body,n=e.index;return n?new z.MathNode("mroot",[$(r,t),$(n,t)]):new z.MathNode("msqrt",[$(r,t)])}});var nn={display:D.DISPLAY,text:D.TEXT,script:D.SCRIPT,scriptscript:D.SCRIPTSCRIPT};T({type:"styling",names:["\\displaystyle","\\textstyle","\\scriptstyle","\\scriptscriptstyle"],props:{numArgs:0,allowedInText:!0,primitive:!0},handler:function(e,t){var r=e.breakOnTokenText,n=e.funcName,o=e.parser,a=o.parseExpression(!0,r),l=n.slice(1,n.length-5);return{type:"styling",mode:o.mode,style:l,body:a}},htmlBuilder:function(e,t){var r=nn[e.style],n=t.havingStyle(r).withFont("");return tn(e.body,n,t)},mathmlBuilder:function(e,t){var r=nn[e.style],n=t.havingStyle(r),o=he(e.body,n),a=new z.MathNode("mstyle",o),l={display:["0","true"],text:["0","false"],script:["1","false"],scriptscript:["2","false"]},m=l[e.style];return a.setAttribute("scriptlevel",m[0]),a.setAttribute("displaystyle",m[1]),a}});var Ai=function(e,t){var r=e.base;if(r)if(r.type==="op"){var n=r.limits&&(t.style.size===D.DISPLAY.size||r.alwaysHandleSupSub);return n?it:null}else if(r.type==="operatorname"){var o=r.alwaysHandleSupSub&&(t.style.size===D.DISPLAY.size||r.limits);return o?en:null}else{if(r.type==="accent")return N.isCharacterBox(r.base)?d0:null;if(r.type==="horizBrace"){var a=!e.sub;return a===r.isOver?Zr:null}else return null}else return null};Qe({type:"supsub",htmlBuilder:function(e,t){var r=Ai(e,t);if(r)return r(e,t);var n=e.base,o=e.sup,a=e.sub,l=_(n,t),m,u,f=t.fontMetrics(),x=0,y=0,S=n&&N.isCharacterBox(n);if(o){var w=t.havingStyle(t.style.sup());m=_(o,w,t),S||(x=l.height-w.fontMetrics().supDrop*w.sizeMultiplier/t.sizeMultiplier)}if(a){var A=t.havingStyle(t.style.sub());u=_(a,A,t),S||(y=l.depth+A.fontMetrics().subDrop*A.sizeMultiplier/t.sizeMultiplier)}var M;t.style===D.DISPLAY?M=f.sup1:t.style.cramped?M=f.sup3:M=f.sup2;var B=t.sizeMultiplier,q=.5/f.ptPerEm/B+"em",I=null;if(u){var W=e.base&&e.base.type==="op"&&e.base.name&&(e.base.name==="\\oiint"||e.base.name==="\\oiiint");(l instanceof ge||W)&&(I=-l.italic+"em")}var Y;if(m&&u){x=Math.max(x,M,m.depth+.25*f.xHeight),y=Math.max(y,f.sub2);var Z=f.defaultRuleThickness,G=4*Z;if(x-m.depth-(u.height-y)<G){y=G-(x-m.depth)+u.height;var U=.8*f.xHeight-(x-m.depth);U>0&&(x+=U,y-=U)}var te=[{type:"elem",elem:u,shift:y,marginRight:q,marginLeft:I},{type:"elem",elem:m,shift:-x,marginRight:q}];Y=b.makeVList({positionType:"individualShift",children:te},t)}else if(u){y=Math.max(y,f.sub1,u.height-.8*f.xHeight);var j=[{type:"elem",elem:u,marginLeft:I,marginRight:q}];Y=b.makeVList({positionType:"shift",positionData:y,children:j},t)}else if(m)x=Math.max(x,M,m.depth+.25*f.xHeight),Y=b.makeVList({positionType:"shift",positionData:-x,children:[{type:"elem",elem:m,marginRight:q}]},t);else throw new Error("supsub must have either sup or sub.");var ue=l0(l,"right")||"mord";return b.makeSpan([ue],[l,b.makeSpan(["msupsub"],[Y])],t)},mathmlBuilder:function(e,t){var r=!1,n,o;e.base&&e.base.type==="horizBrace"&&(o=!!e.sup,o===e.base.isOver&&(r=!0,n=e.base.isOver)),e.base&&(e.base.type==="op"||e.base.type==="operatorname")&&(e.base.parentIsSupSub=!0);var a=[$(e.base,t)];e.sub&&a.push($(e.sub,t)),e.sup&&a.push($(e.sup,t));var l;if(r)l=n?"mover":"munder";else if(e.sub)if(e.sup){var m=e.base;m&&m.type==="op"&&m.limits&&t.style===D.DISPLAY||m&&m.type==="operatorname"&&m.alwaysHandleSupSub&&(t.style===D.DISPLAY||m.limits)?l="munderover":l="msubsup"}else{var u=e.base;u&&u.type==="op"&&u.limits&&(t.style===D.DISPLAY||u.alwaysHandleSupSub)||u&&u.type==="operatorname"&&u.alwaysHandleSupSub&&(u.limits||t.style===D.DISPLAY)?l="munder":l="msub"}else{var f=e.base;f&&f.type==="op"&&f.limits&&(t.style===D.DISPLAY||f.alwaysHandleSupSub)||f&&f.type==="operatorname"&&f.alwaysHandleSupSub&&(f.limits||t.style===D.DISPLAY)?l="mover":l="msup"}return new z.MathNode(l,a)}}),Qe({type:"atom",htmlBuilder:function(e,t){return b.mathsym(e.text,e.mode,t,["m"+e.family])},mathmlBuilder:function(e,t){var r=new z.MathNode("mo",[be(e.text,e.mode)]);if(e.family==="bin"){var n=h0(e,t);n==="bold-italic"&&r.setAttribute("mathvariant",n)}else e.family==="punct"?r.setAttribute("separator","true"):(e.family==="open"||e.family==="close")&&r.setAttribute("stretchy","false");return r}});var on={mi:"italic",mn:"normal",mtext:"normal"};Qe({type:"mathord",htmlBuilder:function(e,t){return b.makeOrd(e,t,"mathord")},mathmlBuilder:function(e,t){var r=new z.MathNode("mi",[be(e.text,e.mode,t)]),n=h0(e,t)||"italic";return n!==on[r.type]&&r.setAttribute("mathvariant",n),r}}),Qe({type:"textord",htmlBuilder:function(e,t){return b.makeOrd(e,t,"textord")},mathmlBuilder:function(e,t){var r=be(e.text,e.mode,t),n=h0(e,t)||"normal",o;return e.mode==="text"?o=new z.MathNode("mtext",[r]):/[0-9]/.test(e.text)?o=new z.MathNode("mn",[r]):e.text==="\\prime"?o=new z.MathNode("mo",[r]):o=new z.MathNode("mi",[r]),n!==on[o.type]&&o.setAttribute("mathvariant",n),o}});var B0={"\\nobreak":"nobreak","\\allowbreak":"allowbreak"},N0={" ":{},"\\ ":{},"~":{className:"nobreak"},"\\space":{},"\\nobreakspace":{className:"nobreak"}};Qe({type:"spacing",htmlBuilder:function(e,t){if(N0.hasOwnProperty(e.text)){var r=N0[e.text].className||"";if(e.mode==="text"){var n=b.makeOrd(e,t,"textord");return n.classes.push(r),n}else return b.makeSpan(["mspace",r],[b.mathsym(e.text,e.mode,t)],t)}else{if(B0.hasOwnProperty(e.text))return b.makeSpan(["mspace",B0[e.text]],[],t);throw new v('Unknown type of space "'+e.text+'"')}},mathmlBuilder:function(e,t){var r;if(N0.hasOwnProperty(e.text))r=new z.MathNode("mtext",[new z.TextNode("\xA0")]);else{if(B0.hasOwnProperty(e.text))return new z.MathNode("mspace");throw new v('Unknown type of space "'+e.text+'"')}return r}});var an=function(){var e=new z.MathNode("mtd",[]);return e.setAttribute("width","50%"),e};Qe({type:"tag",mathmlBuilder:function(e,t){var r=new z.MathNode("mtable",[new z.MathNode("mtr",[an(),new z.MathNode("mtd",[Ve(e.body,t)]),an(),new z.MathNode("mtd",[Ve(e.tag,t)])])]);return r.setAttribute("width","100%"),r}});var sn={"\\text":void 0,"\\textrm":"textrm","\\textsf":"textsf","\\texttt":"texttt","\\textnormal":"textrm"},ln={"\\textbf":"textbf","\\textmd":"textmd"},Mi={"\\textit":"textit","\\textup":"textup"},cn=function(e,t){var r=e.font;return r?sn[r]?t.withTextFontFamily(sn[r]):ln[r]?t.withTextFontWeight(ln[r]):t.withTextFontShape(Mi[r]):t};T({type:"text",names:["\\text","\\textrm","\\textsf","\\texttt","\\textnormal","\\textbf","\\textmd","\\textit","\\textup"],props:{numArgs:1,argTypes:["text"],allowedInArgument:!0,allowedInText:!0},handler:function(e,t){var r=e.parser,n=e.funcName,o=t[0];return{type:"text",mode:r.mode,body:oe(o),font:n}},htmlBuilder:function(e,t){var r=cn(e,t),n=ie(e.body,r,!0);return b.makeSpan(["mord","text"],n,r)},mathmlBuilder:function(e,t){var r=cn(e,t);return Ve(e.body,r)}}),T({type:"underline",names:["\\underline"],props:{numArgs:1,allowedInText:!0},handler:function(e,t){var r=e.parser;return{type:"underline",mode:r.mode,body:t[0]}},htmlBuilder:function(e,t){var r=_(e.body,t),n=b.makeLineSpan("underline-line",t),o=t.fontMetrics().defaultRuleThickness,a=b.makeVList({positionType:"top",positionData:r.height,children:[{type:"kern",size:o},{type:"elem",elem:n},{type:"kern",size:3*o},{type:"elem",elem:r}]},t);return b.makeSpan(["mord","underline"],[a],t)},mathmlBuilder:function(e,t){var r=new z.MathNode("mo",[new z.TextNode("\u203E")]);r.setAttribute("stretchy","true");var n=new z.MathNode("munder",[$(e.body,t),r]);return n.setAttribute("accentunder","true"),n}}),T({type:"vcenter",names:["\\vcenter"],props:{numArgs:1,argTypes:["original"],allowedInText:!1},handler:function(e,t){var r=e.parser;return{type:"vcenter",mode:r.mode,body:t[0]}},htmlBuilder:function(e,t){var r=_(e.body,t),n=t.fontMetrics().axisHeight,o=.5*(r.height-n-(r.depth+n));return b.makeVList({positionType:"shift",positionData:o,children:[{type:"elem",elem:r}]},t)},mathmlBuilder:function(e,t){return new z.MathNode("mpadded",[$(e.body,t)],["vcenter"])}}),T({type:"verb",names:["\\verb"],props:{numArgs:0,allowedInText:!0},handler:function(e,t,r){throw new v("\\verb ended by end of line instead of matching delimiter")},htmlBuilder:function(e,t){for(var r=mn(e),n=[],o=t.havingStyle(t.style.text()),a=0;a<r.length;a++){var l=r[a];l==="~"&&(l="\\textasciitilde"),n.push(b.makeSymbol(l,"Typewriter-Regular",e.mode,o,["mord","texttt"]))}return b.makeSpan(["mord","text"].concat(o.sizingClasses(t)),b.tryCombineChars(n),o)},mathmlBuilder:function(e,t){var r=new z.TextNode(mn(e)),n=new z.MathNode("mtext",[r]);return n.setAttribute("mathvariant","monospace"),n}});var mn=function(e){return e.body.replace(/ /g,e.star?"\u2423":"\xA0")},Ti=pr,_e=Ti,Se=function(){function e(t,r,n){this.lexer=void 0,this.start=void 0,this.end=void 0,this.lexer=t,this.start=r,this.end=n}return e.range=function(t,r){return r?!t||!t.loc||!r.loc||t.loc.lexer!==r.loc.lexer?null:new e(t.loc.lexer,t.loc.start,r.loc.end):t&&t.loc},e}(),Je=function(){function e(r,n){this.text=void 0,this.loc=void 0,this.noexpand=void 0,this.treatAsRelax=void 0,this.text=r,this.loc=n}var t=e.prototype;return t.range=function(r,n){return new e(n,Se.range(this,r))},e}(),q0=`[ \r
	]`,hn="\\\\[a-zA-Z@]+",Ci="\\\\[^\uD800-\uDFFF]",Bi=""+hn+q0+"*",Ni=new RegExp("^("+hn+")"+q0+"*$"),L0="[\u0300-\u036F]",qi=new RegExp(L0+"+$"),Li="("+q0+"+)|([!-\\[\\]-\u2027\u202A-\uD7FF\uF900-\uFFFF]"+(L0+"*")+"|[\uD800-\uDBFF][\uDC00-\uDFFF]"+(L0+"*")+"|\\\\verb\\*([^]).*?\\3|\\\\verb([^*a-zA-Z]).*?\\4|\\\\operatorname\\*"+("|"+Bi)+("|"+Ci+")"),un=function(){function e(r,n){this.input=void 0,this.settings=void 0,this.tokenRegex=void 0,this.catcodes=void 0,this.input=r,this.settings=n,this.tokenRegex=new RegExp(Li,"g"),this.catcodes={"%":14}}var t=e.prototype;return t.setCatcode=function(r,n){this.catcodes[r]=n},t.lex=function(){var r=this.input,n=this.tokenRegex.lastIndex;if(n===r.length)return new Je("EOF",new Se(this,n,n));var o=this.tokenRegex.exec(r);if(o===null||o.index!==n)throw new v("Unexpected character: '"+r[n]+"'",new Je(r[n],new Se(this,n,n+1)));var a=o[2]||" ";if(this.catcodes[a]===14){var l=r.indexOf(`
`,this.tokenRegex.lastIndex);return l===-1?(this.tokenRegex.lastIndex=r.length,this.settings.reportNonstrict("commentAtEnd","% comment has no terminating newline; LaTeX would fail because of commenting the end of math mode (e.g. $)")):this.tokenRegex.lastIndex=l+1,this.lex()}var m=a.match(Ni);return m&&(a=m[1]),new Je(a,new Se(this,n,this.tokenRegex.lastIndex))},e}(),Ri=function(){function e(r,n){r===void 0&&(r={}),n===void 0&&(n={}),this.current=void 0,this.builtins=void 0,this.undefStack=void 0,this.current=n,this.builtins=r,this.undefStack=[]}var t=e.prototype;return t.beginGroup=function(){this.undefStack.push({})},t.endGroup=function(){if(this.undefStack.length===0)throw new v("Unbalanced namespace destruction: attempt to pop global namespace; please report this as a bug");var r=this.undefStack.pop();for(var n in r)r.hasOwnProperty(n)&&(r[n]===void 0?delete this.current[n]:this.current[n]=r[n])},t.has=function(r){return this.current.hasOwnProperty(r)||this.builtins.hasOwnProperty(r)},t.get=function(r){return this.current.hasOwnProperty(r)?this.current[r]:this.builtins[r]},t.set=function(r,n,o){if(o===void 0&&(o=!1),o){for(var a=0;a<this.undefStack.length;a++)delete this.undefStack[a][r];this.undefStack.length>0&&(this.undefStack[this.undefStack.length-1][r]=n)}else{var l=this.undefStack[this.undefStack.length-1];l&&!l.hasOwnProperty(r)&&(l[r]=this.current[r])}this.current[r]=n},e}(),dn={},Di=dn;function h(e,t){dn[e]=t}h("\\noexpand",function(e){var t=e.popToken();return e.isExpandable(t.text)&&(t.noexpand=!0,t.treatAsRelax=!0),{tokens:[t],numArgs:0}}),h("\\expandafter",function(e){var t=e.popToken();return e.expandOnce(!0),{tokens:[t],numArgs:0}}),h("\\@firstoftwo",function(e){var t=e.consumeArgs(2);return{tokens:t[0],numArgs:0}}),h("\\@secondoftwo",function(e){var t=e.consumeArgs(2);return{tokens:t[1],numArgs:0}}),h("\\@ifnextchar",function(e){var t=e.consumeArgs(3);e.consumeSpaces();var r=e.future();return t[0].length===1&&t[0][0].text===r.text?{tokens:t[1],numArgs:0}:{tokens:t[2],numArgs:0}}),h("\\@ifstar","\\@ifnextchar *{\\@firstoftwo{#1}}"),h("\\TextOrMath",function(e){var t=e.consumeArgs(2);return e.mode==="text"?{tokens:t[0],numArgs:0}:{tokens:t[1],numArgs:0}});var pn={0:0,1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9,a:10,A:10,b:11,B:11,c:12,C:12,d:13,D:13,e:14,E:14,f:15,F:15};h("\\char",function(e){var t=e.popToken(),r,n="";if(t.text==="'")r=8,t=e.popToken();else if(t.text==='"')r=16,t=e.popToken();else if(t.text==="`")if(t=e.popToken(),t.text[0]==="\\")n=t.text.charCodeAt(1);else{if(t.text==="EOF")throw new v("\\char` missing argument");n=t.text.charCodeAt(0)}else r=10;if(r){if(n=pn[t.text],n==null||n>=r)throw new v("Invalid base-"+r+" digit "+t.text);for(var o;(o=pn[e.future().text])!=null&&o<r;)n*=r,n+=o,e.popToken()}return"\\@char{"+n+"}"});var R0=function(e,t,r){var n=e.consumeArg().tokens;if(n.length!==1)throw new v("\\newcommand's first argument must be a macro name");var o=n[0].text,a=e.isDefined(o);if(a&&!t)throw new v("\\newcommand{"+o+"} attempting to redefine "+(o+"; use \\renewcommand"));if(!a&&!r)throw new v("\\renewcommand{"+o+"} when command "+o+" does not yet exist; use \\newcommand");var l=0;if(n=e.consumeArg().tokens,n.length===1&&n[0].text==="["){for(var m="",u=e.expandNextToken();u.text!=="]"&&u.text!=="EOF";)m+=u.text,u=e.expandNextToken();if(!m.match(/^\s*[0-9]+\s*$/))throw new v("Invalid number of arguments: "+m);l=parseInt(m),n=e.consumeArg().tokens}return e.macros.set(o,{tokens:n,numArgs:l}),""};h("\\newcommand",function(e){return R0(e,!1,!0)}),h("\\renewcommand",function(e){return R0(e,!0,!1)}),h("\\providecommand",function(e){return R0(e,!0,!0)}),h("\\message",function(e){var t=e.consumeArgs(1)[0];return console.log(t.reverse().map(function(r){return r.text}).join("")),""}),h("\\errmessage",function(e){var t=e.consumeArgs(1)[0];return console.error(t.reverse().map(function(r){return r.text}).join("")),""}),h("\\show",function(e){var t=e.popToken(),r=t.text;return console.log(t,e.macros.get(r),_e[r],K.math[r],K.text[r]),""}),h("\\bgroup","{"),h("\\egroup","}"),h("\\lq","`"),h("\\rq","'"),h("\\aa","\\r a"),h("\\AA","\\r A"),h("\\textcopyright","\\html@mathml{\\textcircled{c}}{\\char`\xA9}"),h("\\copyright","\\TextOrMath{\\textcopyright}{\\text{\\textcopyright}}"),h("\\textregistered","\\html@mathml{\\textcircled{\\scriptsize R}}{\\char`\xAE}"),h("\u212C","\\mathscr{B}"),h("\u2130","\\mathscr{E}"),h("\u2131","\\mathscr{F}"),h("\u210B","\\mathscr{H}"),h("\u2110","\\mathscr{I}"),h("\u2112","\\mathscr{L}"),h("\u2133","\\mathscr{M}"),h("\u211B","\\mathscr{R}"),h("\u212D","\\mathfrak{C}"),h("\u210C","\\mathfrak{H}"),h("\u2128","\\mathfrak{Z}"),h("\\Bbbk","\\Bbb{k}"),h("\xB7","\\cdotp"),h("\\llap","\\mathllap{\\textrm{#1}}"),h("\\rlap","\\mathrlap{\\textrm{#1}}"),h("\\clap","\\mathclap{\\textrm{#1}}"),h("\\mathstrut","\\vphantom{(}"),h("\\underbar","\\underline{\\text{#1}}"),h("\\not",'\\html@mathml{\\mathrel{\\mathrlap\\@not}}{\\char"338}'),h("\\neq","\\html@mathml{\\mathrel{\\not=}}{\\mathrel{\\char`\u2260}}"),h("\\ne","\\neq"),h("\u2260","\\neq"),h("\\notin","\\html@mathml{\\mathrel{{\\in}\\mathllap{/\\mskip1mu}}}{\\mathrel{\\char`\u2209}}"),h("\u2209","\\notin"),h("\u2258","\\html@mathml{\\mathrel{=\\kern{-1em}\\raisebox{0.4em}{$\\scriptsize\\frown$}}}{\\mathrel{\\char`\u2258}}"),h("\u2259","\\html@mathml{\\stackrel{\\tiny\\wedge}{=}}{\\mathrel{\\char`\u2258}}"),h("\u225A","\\html@mathml{\\stackrel{\\tiny\\vee}{=}}{\\mathrel{\\char`\u225A}}"),h("\u225B","\\html@mathml{\\stackrel{\\scriptsize\\star}{=}}{\\mathrel{\\char`\u225B}}"),h("\u225D","\\html@mathml{\\stackrel{\\tiny\\mathrm{def}}{=}}{\\mathrel{\\char`\u225D}}"),h("\u225E","\\html@mathml{\\stackrel{\\tiny\\mathrm{m}}{=}}{\\mathrel{\\char`\u225E}}"),h("\u225F","\\html@mathml{\\stackrel{\\tiny?}{=}}{\\mathrel{\\char`\u225F}}"),h("\u27C2","\\perp"),h("\u203C","\\mathclose{!\\mkern-0.8mu!}"),h("\u220C","\\notni"),h("\u231C","\\ulcorner"),h("\u231D","\\urcorner"),h("\u231E","\\llcorner"),h("\u231F","\\lrcorner"),h("\xA9","\\copyright"),h("\xAE","\\textregistered"),h("\uFE0F","\\textregistered"),h("\\ulcorner",'\\html@mathml{\\@ulcorner}{\\mathop{\\char"231c}}'),h("\\urcorner",'\\html@mathml{\\@urcorner}{\\mathop{\\char"231d}}'),h("\\llcorner",'\\html@mathml{\\@llcorner}{\\mathop{\\char"231e}}'),h("\\lrcorner",'\\html@mathml{\\@lrcorner}{\\mathop{\\char"231f}}'),h("\\vdots","\\mathord{\\varvdots\\rule{0pt}{15pt}}"),h("\u22EE","\\vdots"),h("\\varGamma","\\mathit{\\Gamma}"),h("\\varDelta","\\mathit{\\Delta}"),h("\\varTheta","\\mathit{\\Theta}"),h("\\varLambda","\\mathit{\\Lambda}"),h("\\varXi","\\mathit{\\Xi}"),h("\\varPi","\\mathit{\\Pi}"),h("\\varSigma","\\mathit{\\Sigma}"),h("\\varUpsilon","\\mathit{\\Upsilon}"),h("\\varPhi","\\mathit{\\Phi}"),h("\\varPsi","\\mathit{\\Psi}"),h("\\varOmega","\\mathit{\\Omega}"),h("\\substack","\\begin{subarray}{c}#1\\end{subarray}"),h("\\colon","\\nobreak\\mskip2mu\\mathpunct{}\\mathchoice{\\mkern-3mu}{\\mkern-3mu}{}{}{:}\\mskip6mu"),h("\\boxed","\\fbox{$\\displaystyle{#1}$}"),h("\\iff","\\DOTSB\\;\\Longleftrightarrow\\;"),h("\\implies","\\DOTSB\\;\\Longrightarrow\\;"),h("\\impliedby","\\DOTSB\\;\\Longleftarrow\\;");var fn={",":"\\dotsc","\\not":"\\dotsb","+":"\\dotsb","=":"\\dotsb","<":"\\dotsb",">":"\\dotsb","-":"\\dotsb","*":"\\dotsb",":":"\\dotsb","\\DOTSB":"\\dotsb","\\coprod":"\\dotsb","\\bigvee":"\\dotsb","\\bigwedge":"\\dotsb","\\biguplus":"\\dotsb","\\bigcap":"\\dotsb","\\bigcup":"\\dotsb","\\prod":"\\dotsb","\\sum":"\\dotsb","\\bigotimes":"\\dotsb","\\bigoplus":"\\dotsb","\\bigodot":"\\dotsb","\\bigsqcup":"\\dotsb","\\And":"\\dotsb","\\longrightarrow":"\\dotsb","\\Longrightarrow":"\\dotsb","\\longleftarrow":"\\dotsb","\\Longleftarrow":"\\dotsb","\\longleftrightarrow":"\\dotsb","\\Longleftrightarrow":"\\dotsb","\\mapsto":"\\dotsb","\\longmapsto":"\\dotsb","\\hookrightarrow":"\\dotsb","\\doteq":"\\dotsb","\\mathbin":"\\dotsb","\\mathrel":"\\dotsb","\\relbar":"\\dotsb","\\Relbar":"\\dotsb","\\xrightarrow":"\\dotsb","\\xleftarrow":"\\dotsb","\\DOTSI":"\\dotsi","\\int":"\\dotsi","\\oint":"\\dotsi","\\iint":"\\dotsi","\\iiint":"\\dotsi","\\iiiint":"\\dotsi","\\idotsint":"\\dotsi","\\DOTSX":"\\dotsx"};h("\\dots",function(e){var t="\\dotso",r=e.expandAfterFuture().text;return r in fn?t=fn[r]:(r.substr(0,4)==="\\not"||r in K.math&&N.contains(["bin","rel"],K.math[r].group))&&(t="\\dotsb"),t});var D0={")":!0,"]":!0,"\\rbrack":!0,"\\}":!0,"\\rbrace":!0,"\\rangle":!0,"\\rceil":!0,"\\rfloor":!0,"\\rgroup":!0,"\\rmoustache":!0,"\\right":!0,"\\bigr":!0,"\\biggr":!0,"\\Bigr":!0,"\\Biggr":!0,$:!0,";":!0,".":!0,",":!0};h("\\dotso",function(e){var t=e.future().text;return t in D0?"\\ldots\\,":"\\ldots"}),h("\\dotsc",function(e){var t=e.future().text;return t in D0&&t!==","?"\\ldots\\,":"\\ldots"}),h("\\cdots",function(e){var t=e.future().text;return t in D0?"\\@cdots\\,":"\\@cdots"}),h("\\dotsb","\\cdots"),h("\\dotsm","\\cdots"),h("\\dotsi","\\!\\cdots"),h("\\dotsx","\\ldots\\,"),h("\\DOTSI","\\relax"),h("\\DOTSB","\\relax"),h("\\DOTSX","\\relax"),h("\\tmspace","\\TextOrMath{\\kern#1#3}{\\mskip#1#2}\\relax"),h("\\,","\\tmspace+{3mu}{.1667em}"),h("\\thinspace","\\,"),h("\\>","\\mskip{4mu}"),h("\\:","\\tmspace+{4mu}{.2222em}"),h("\\medspace","\\:"),h("\\;","\\tmspace+{5mu}{.2777em}"),h("\\thickspace","\\;"),h("\\!","\\tmspace-{3mu}{.1667em}"),h("\\negthinspace","\\!"),h("\\negmedspace","\\tmspace-{4mu}{.2222em}"),h("\\negthickspace","\\tmspace-{5mu}{.277em}"),h("\\enspace","\\kern.5em "),h("\\enskip","\\hskip.5em\\relax"),h("\\quad","\\hskip1em\\relax"),h("\\qquad","\\hskip2em\\relax"),h("\\tag","\\@ifstar\\tag@literal\\tag@paren"),h("\\tag@paren","\\tag@literal{({#1})}"),h("\\tag@literal",function(e){if(e.macros.get("\\df@tag"))throw new v("Multiple \\tag");return"\\gdef\\df@tag{\\text{#1}}"}),h("\\bmod","\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}\\mathbin{\\rm mod}\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}"),h("\\pod","\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern8mu}{\\mkern8mu}{\\mkern8mu}(#1)"),h("\\pmod","\\pod{{\\rm mod}\\mkern6mu#1}"),h("\\mod","\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern12mu}{\\mkern12mu}{\\mkern12mu}{\\rm mod}\\,\\,#1"),h("\\pmb","\\html@mathml{\\@binrel{#1}{\\mathrlap{#1}\\kern0.5px#1}}{\\mathbf{#1}}"),h("\\newline","\\\\\\relax"),h("\\TeX","\\textrm{\\html@mathml{T\\kern-.1667em\\raisebox{-.5ex}{E}\\kern-.125emX}{TeX}}");var gn=Ae["Main-Regular"]["T".charCodeAt(0)][1]-.7*Ae["Main-Regular"]["A".charCodeAt(0)][1]+"em";h("\\LaTeX","\\textrm{\\html@mathml{"+("L\\kern-.36em\\raisebox{"+gn+"}{\\scriptstyle A}")+"\\kern-.15em\\TeX}{LaTeX}}"),h("\\KaTeX","\\textrm{\\html@mathml{"+("K\\kern-.17em\\raisebox{"+gn+"}{\\scriptstyle A}")+"\\kern-.15em\\TeX}{KaTeX}}"),h("\\hspace","\\@ifstar\\@hspacer\\@hspace"),h("\\@hspace","\\hskip #1\\relax"),h("\\@hspacer","\\rule{0pt}{0pt}\\hskip #1\\relax"),h("\\ordinarycolon",":"),h("\\vcentcolon","\\mathrel{\\mathop\\ordinarycolon}"),h("\\dblcolon",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-.9mu}\\vcentcolon}}{\\mathop{\\char"2237}}'),h("\\coloneqq",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2254}}'),h("\\Coloneqq",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2237\\char"3d}}'),h("\\coloneq",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"3a\\char"2212}}'),h("\\Coloneq",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"2237\\char"2212}}'),h("\\eqqcolon",'\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2255}}'),h("\\Eqqcolon",'\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"3d\\char"2237}}'),h("\\eqcolon",'\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2239}}'),h("\\Eqcolon",'\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"2212\\char"2237}}'),h("\\colonapprox",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"3a\\char"2248}}'),h("\\Colonapprox",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"2237\\char"2248}}'),h("\\colonsim",'\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"3a\\char"223c}}'),h("\\Colonsim",'\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"2237\\char"223c}}'),h("\u2237","\\dblcolon"),h("\u2239","\\eqcolon"),h("\u2254","\\coloneqq"),h("\u2255","\\eqqcolon"),h("\u2A74","\\Coloneqq"),h("\\ratio","\\vcentcolon"),h("\\coloncolon","\\dblcolon"),h("\\colonequals","\\coloneqq"),h("\\coloncolonequals","\\Coloneqq"),h("\\equalscolon","\\eqqcolon"),h("\\equalscoloncolon","\\Eqqcolon"),h("\\colonminus","\\coloneq"),h("\\coloncolonminus","\\Coloneq"),h("\\minuscolon","\\eqcolon"),h("\\minuscoloncolon","\\Eqcolon"),h("\\coloncolonapprox","\\Colonapprox"),h("\\coloncolonsim","\\Colonsim"),h("\\simcolon","\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\vcentcolon}"),h("\\simcoloncolon","\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\dblcolon}"),h("\\approxcolon","\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\vcentcolon}"),h("\\approxcoloncolon","\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\dblcolon}"),h("\\notni","\\html@mathml{\\not\\ni}{\\mathrel{\\char`\u220C}}"),h("\\limsup","\\DOTSB\\operatorname*{lim\\,sup}"),h("\\liminf","\\DOTSB\\operatorname*{lim\\,inf}"),h("\\injlim","\\DOTSB\\operatorname*{inj\\,lim}"),h("\\projlim","\\DOTSB\\operatorname*{proj\\,lim}"),h("\\varlimsup","\\DOTSB\\operatorname*{\\overline{lim}}"),h("\\varliminf","\\DOTSB\\operatorname*{\\underline{lim}}"),h("\\varinjlim","\\DOTSB\\operatorname*{\\underrightarrow{lim}}"),h("\\varprojlim","\\DOTSB\\operatorname*{\\underleftarrow{lim}}"),h("\\gvertneqq","\\html@mathml{\\@gvertneqq}{\u2269}"),h("\\lvertneqq","\\html@mathml{\\@lvertneqq}{\u2268}"),h("\\ngeqq","\\html@mathml{\\@ngeqq}{\u2271}"),h("\\ngeqslant","\\html@mathml{\\@ngeqslant}{\u2271}"),h("\\nleqq","\\html@mathml{\\@nleqq}{\u2270}"),h("\\nleqslant","\\html@mathml{\\@nleqslant}{\u2270}"),h("\\nshortmid","\\html@mathml{\\@nshortmid}{\u2224}"),h("\\nshortparallel","\\html@mathml{\\@nshortparallel}{\u2226}"),h("\\nsubseteqq","\\html@mathml{\\@nsubseteqq}{\u2288}"),h("\\nsupseteqq","\\html@mathml{\\@nsupseteqq}{\u2289}"),h("\\varsubsetneq","\\html@mathml{\\@varsubsetneq}{\u228A}"),h("\\varsubsetneqq","\\html@mathml{\\@varsubsetneqq}{\u2ACB}"),h("\\varsupsetneq","\\html@mathml{\\@varsupsetneq}{\u228B}"),h("\\varsupsetneqq","\\html@mathml{\\@varsupsetneqq}{\u2ACC}"),h("\\imath","\\html@mathml{\\@imath}{\u0131}"),h("\\jmath","\\html@mathml{\\@jmath}{\u0237}"),h("\\llbracket","\\html@mathml{\\mathopen{[\\mkern-3.2mu[}}{\\mathopen{\\char`\u27E6}}"),h("\\rrbracket","\\html@mathml{\\mathclose{]\\mkern-3.2mu]}}{\\mathclose{\\char`\u27E7}}"),h("\u27E6","\\llbracket"),h("\u27E7","\\rrbracket"),h("\\lBrace","\\html@mathml{\\mathopen{\\{\\mkern-3.2mu[}}{\\mathopen{\\char`\u2983}}"),h("\\rBrace","\\html@mathml{\\mathclose{]\\mkern-3.2mu\\}}}{\\mathclose{\\char`\u2984}}"),h("\u2983","\\lBrace"),h("\u2984","\\rBrace"),h("\\minuso","\\mathbin{\\html@mathml{{\\mathrlap{\\mathchoice{\\kern{0.145em}}{\\kern{0.145em}}{\\kern{0.1015em}}{\\kern{0.0725em}}\\circ}{-}}}{\\char`\u29B5}}"),h("\u29B5","\\minuso"),h("\\darr","\\downarrow"),h("\\dArr","\\Downarrow"),h("\\Darr","\\Downarrow"),h("\\lang","\\langle"),h("\\rang","\\rangle"),h("\\uarr","\\uparrow"),h("\\uArr","\\Uparrow"),h("\\Uarr","\\Uparrow"),h("\\N","\\mathbb{N}"),h("\\R","\\mathbb{R}"),h("\\Z","\\mathbb{Z}"),h("\\alef","\\aleph"),h("\\alefsym","\\aleph"),h("\\Alpha","\\mathrm{A}"),h("\\Beta","\\mathrm{B}"),h("\\bull","\\bullet"),h("\\Chi","\\mathrm{X}"),h("\\clubs","\\clubsuit"),h("\\cnums","\\mathbb{C}"),h("\\Complex","\\mathbb{C}"),h("\\Dagger","\\ddagger"),h("\\diamonds","\\diamondsuit"),h("\\empty","\\emptyset"),h("\\Epsilon","\\mathrm{E}"),h("\\Eta","\\mathrm{H}"),h("\\exist","\\exists"),h("\\harr","\\leftrightarrow"),h("\\hArr","\\Leftrightarrow"),h("\\Harr","\\Leftrightarrow"),h("\\hearts","\\heartsuit"),h("\\image","\\Im"),h("\\infin","\\infty"),h("\\Iota","\\mathrm{I}"),h("\\isin","\\in"),h("\\Kappa","\\mathrm{K}"),h("\\larr","\\leftarrow"),h("\\lArr","\\Leftarrow"),h("\\Larr","\\Leftarrow"),h("\\lrarr","\\leftrightarrow"),h("\\lrArr","\\Leftrightarrow"),h("\\Lrarr","\\Leftrightarrow"),h("\\Mu","\\mathrm{M}"),h("\\natnums","\\mathbb{N}"),h("\\Nu","\\mathrm{N}"),h("\\Omicron","\\mathrm{O}"),h("\\plusmn","\\pm"),h("\\rarr","\\rightarrow"),h("\\rArr","\\Rightarrow"),h("\\Rarr","\\Rightarrow"),h("\\real","\\Re"),h("\\reals","\\mathbb{R}"),h("\\Reals","\\mathbb{R}"),h("\\Rho","\\mathrm{P}"),h("\\sdot","\\cdot"),h("\\sect","\\S"),h("\\spades","\\spadesuit"),h("\\sub","\\subset"),h("\\sube","\\subseteq"),h("\\supe","\\supseteq"),h("\\Tau","\\mathrm{T}"),h("\\thetasym","\\vartheta"),h("\\weierp","\\wp"),h("\\Zeta","\\mathrm{Z}"),h("\\argmin","\\DOTSB\\operatorname*{arg\\,min}"),h("\\argmax","\\DOTSB\\operatorname*{arg\\,max}"),h("\\plim","\\DOTSB\\mathop{\\operatorname{plim}}\\limits"),h("\\bra","\\mathinner{\\langle{#1}|}"),h("\\ket","\\mathinner{|{#1}\\rangle}"),h("\\braket","\\mathinner{\\langle{#1}\\rangle}"),h("\\Bra","\\left\\langle#1\\right|"),h("\\Ket","\\left|#1\\right\\rangle"),h("\\angln","{\\angl n}"),h("\\blue","\\textcolor{##6495ed}{#1}"),h("\\orange","\\textcolor{##ffa500}{#1}"),h("\\pink","\\textcolor{##ff00af}{#1}"),h("\\red","\\textcolor{##df0030}{#1}"),h("\\green","\\textcolor{##28ae7b}{#1}"),h("\\gray","\\textcolor{gray}{#1}"),h("\\purple","\\textcolor{##9d38bd}{#1}"),h("\\blueA","\\textcolor{##ccfaff}{#1}"),h("\\blueB","\\textcolor{##80f6ff}{#1}"),h("\\blueC","\\textcolor{##63d9ea}{#1}"),h("\\blueD","\\textcolor{##11accd}{#1}"),h("\\blueE","\\textcolor{##0c7f99}{#1}"),h("\\tealA","\\textcolor{##94fff5}{#1}"),h("\\tealB","\\textcolor{##26edd5}{#1}"),h("\\tealC","\\textcolor{##01d1c1}{#1}"),h("\\tealD","\\textcolor{##01a995}{#1}"),h("\\tealE","\\textcolor{##208170}{#1}"),h("\\greenA","\\textcolor{##b6ffb0}{#1}"),h("\\greenB","\\textcolor{##8af281}{#1}"),h("\\greenC","\\textcolor{##74cf70}{#1}"),h("\\greenD","\\textcolor{##1fab54}{#1}"),h("\\greenE","\\textcolor{##0d923f}{#1}"),h("\\goldA","\\textcolor{##ffd0a9}{#1}"),h("\\goldB","\\textcolor{##ffbb71}{#1}"),h("\\goldC","\\textcolor{##ff9c39}{#1}"),h("\\goldD","\\textcolor{##e07d10}{#1}"),h("\\goldE","\\textcolor{##a75a05}{#1}"),h("\\redA","\\textcolor{##fca9a9}{#1}"),h("\\redB","\\textcolor{##ff8482}{#1}"),h("\\redC","\\textcolor{##f9685d}{#1}"),h("\\redD","\\textcolor{##e84d39}{#1}"),h("\\redE","\\textcolor{##bc2612}{#1}"),h("\\maroonA","\\textcolor{##ffbde0}{#1}"),h("\\maroonB","\\textcolor{##ff92c6}{#1}"),h("\\maroonC","\\textcolor{##ed5fa6}{#1}"),h("\\maroonD","\\textcolor{##ca337c}{#1}"),h("\\maroonE","\\textcolor{##9e034e}{#1}"),h("\\purpleA","\\textcolor{##ddd7ff}{#1}"),h("\\purpleB","\\textcolor{##c6b9fc}{#1}"),h("\\purpleC","\\textcolor{##aa87ff}{#1}"),h("\\purpleD","\\textcolor{##7854ab}{#1}"),h("\\purpleE","\\textcolor{##543b78}{#1}"),h("\\mintA","\\textcolor{##f5f9e8}{#1}"),h("\\mintB","\\textcolor{##edf2df}{#1}"),h("\\mintC","\\textcolor{##e0e5cc}{#1}"),h("\\grayA","\\textcolor{##f6f7f7}{#1}"),h("\\grayB","\\textcolor{##f0f1f2}{#1}"),h("\\grayC","\\textcolor{##e3e5e6}{#1}"),h("\\grayD","\\textcolor{##d6d8da}{#1}"),h("\\grayE","\\textcolor{##babec2}{#1}"),h("\\grayF","\\textcolor{##888d93}{#1}"),h("\\grayG","\\textcolor{##626569}{#1}"),h("\\grayH","\\textcolor{##3b3e40}{#1}"),h("\\grayI","\\textcolor{##21242c}{#1}"),h("\\kaBlue","\\textcolor{##314453}{#1}"),h("\\kaGreen","\\textcolor{##71B307}{#1}");var xn={"\\relax":!0,"^":!0,_:!0,"\\limits":!0,"\\nolimits":!0},Ii=function(){function e(r,n,o){this.settings=void 0,this.expansionCount=void 0,this.lexer=void 0,this.macros=void 0,this.stack=void 0,this.mode=void 0,this.settings=n,this.expansionCount=0,this.feed(r),this.macros=new Ri(Di,n.macros),this.mode=o,this.stack=[]}var t=e.prototype;return t.feed=function(r){this.lexer=new un(r,this.settings)},t.switchMode=function(r){this.mode=r},t.beginGroup=function(){this.macros.beginGroup()},t.endGroup=function(){this.macros.endGroup()},t.future=function(){return this.stack.length===0&&this.pushToken(this.lexer.lex()),this.stack[this.stack.length-1]},t.popToken=function(){return this.future(),this.stack.pop()},t.pushToken=function(r){this.stack.push(r)},t.pushTokens=function(r){var n;(n=this.stack).push.apply(n,r)},t.scanArgument=function(r){var n,o,a;if(r){if(this.consumeSpaces(),this.future().text!=="[")return null;n=this.popToken();var l=this.consumeArg(["]"]);a=l.tokens,o=l.end}else{var m=this.consumeArg();a=m.tokens,n=m.start,o=m.end}return this.pushToken(new Je("EOF",o.loc)),this.pushTokens(a),n.range(o,"")},t.consumeSpaces=function(){for(;;){var r=this.future();if(r.text===" ")this.stack.pop();else break}},t.consumeArg=function(r){var n=[],o=r&&r.length>0;o||this.consumeSpaces();var a=this.future(),l,m=0,u=0;do{if(l=this.popToken(),n.push(l),l.text==="{")++m;else if(l.text==="}"){if(--m,m===-1)throw new v("Extra }",l)}else if(l.text==="EOF")throw new v("Unexpected end of input in a macro argument, expected '"+(r&&o?r[u]:"}")+"'",l);if(r&&o)if((m===0||m===1&&r[u]==="{")&&l.text===r[u]){if(++u,u===r.length){n.splice(-u,u);break}}else u=0}while(m!==0||o);return a.text==="{"&&n[n.length-1].text==="}"&&(n.pop(),n.shift()),n.reverse(),{tokens:n,start:a,end:l}},t.consumeArgs=function(r,n){if(n){if(n.length!==r+1)throw new v("The length of delimiters doesn't match the number of args!");for(var o=n[0],a=0;a<o.length;a++){var l=this.popToken();if(o[a]!==l.text)throw new v("Use of the macro doesn't match its definition",l)}}for(var m=[],u=0;u<r;u++)m.push(this.consumeArg(n&&n[u+1]).tokens);return m},t.expandOnce=function(r){var n=this.popToken(),o=n.text,a=n.noexpand?null:this._getExpansion(o);if(a==null||r&&a.unexpandable){if(r&&a==null&&o[0]==="\\"&&!this.isDefined(o))throw new v("Undefined control sequence: "+o);return this.pushToken(n),n}if(this.expansionCount++,this.expansionCount>this.settings.maxExpand)throw new v("Too many expansions: infinite loop or need to increase maxExpand setting");var l=a.tokens,m=this.consumeArgs(a.numArgs,a.delimiters);if(a.numArgs){l=l.slice();for(var u=l.length-1;u>=0;--u){var f=l[u];if(f.text==="#"){if(u===0)throw new v("Incomplete placeholder at end of macro body",f);if(f=l[--u],f.text==="#")l.splice(u+1,1);else if(/^[1-9]$/.test(f.text)){var x;(x=l).splice.apply(x,[u,2].concat(m[+f.text-1]))}else throw new v("Not a valid argument number",f)}}}return this.pushTokens(l),l},t.expandAfterFuture=function(){return this.expandOnce(),this.future()},t.expandNextToken=function(){for(;;){var r=this.expandOnce();if(r instanceof Je)if(r.text==="\\relax"||r.treatAsRelax)this.stack.pop();else return this.stack.pop()}throw new Error},t.expandMacro=function(r){return this.macros.has(r)?this.expandTokens([new Je(r)]):void 0},t.expandTokens=function(r){var n=[],o=this.stack.length;for(this.pushTokens(r);this.stack.length>o;){var a=this.expandOnce(!0);a instanceof Je&&(a.treatAsRelax&&(a.noexpand=!1,a.treatAsRelax=!1),n.push(this.stack.pop()))}return n},t.expandMacroAsText=function(r){var n=this.expandMacro(r);return n&&n.map(function(o){return o.text}).join("")},t._getExpansion=function(r){var n=this.macros.get(r);if(n==null)return n;var o=typeof n=="function"?n(this):n;if(typeof o=="string"){var a=0;if(o.indexOf("#")!==-1)for(var l=o.replace(/##/g,"");l.indexOf("#"+(a+1))!==-1;)++a;for(var m=new un(o,this.settings),u=[],f=m.lex();f.text!=="EOF";)u.push(f),f=m.lex();u.reverse();var x={tokens:u,numArgs:a};return x}return o},t.isDefined=function(r){return this.macros.has(r)||_e.hasOwnProperty(r)||K.math.hasOwnProperty(r)||K.text.hasOwnProperty(r)||xn.hasOwnProperty(r)},t.isExpandable=function(r){var n=this.macros.get(r);return n!=null?typeof n=="string"||typeof n=="function"||!n.unexpandable:_e.hasOwnProperty(r)&&!_e[r].primitive},e}(),bn={"\u0301":{text:"\\'",math:"\\acute"},"\u0300":{text:"\\`",math:"\\grave"},"\u0308":{text:'\\"',math:"\\ddot"},"\u0303":{text:"\\~",math:"\\tilde"},"\u0304":{text:"\\=",math:"\\bar"},"\u0306":{text:"\\u",math:"\\breve"},"\u030C":{text:"\\v",math:"\\check"},"\u0302":{text:"\\^",math:"\\hat"},"\u0307":{text:"\\.",math:"\\dot"},"\u030A":{text:"\\r",math:"\\mathring"},"\u030B":{text:"\\H"}},yn={\u00E1:"a\u0301",\u00E0:"a\u0300",\u00E4:"a\u0308",\u01DF:"a\u0308\u0304",\u00E3:"a\u0303",\u0101:"a\u0304",\u0103:"a\u0306",\u1EAF:"a\u0306\u0301",\u1EB1:"a\u0306\u0300",\u1EB5:"a\u0306\u0303",\u01CE:"a\u030C",\u00E2:"a\u0302",\u1EA5:"a\u0302\u0301",\u1EA7:"a\u0302\u0300",\u1EAB:"a\u0302\u0303",\u0227:"a\u0307",\u01E1:"a\u0307\u0304",\u00E5:"a\u030A",\u01FB:"a\u030A\u0301",\u1E03:"b\u0307",\u0107:"c\u0301",\u010D:"c\u030C",\u0109:"c\u0302",\u010B:"c\u0307",\u010F:"d\u030C",\u1E0B:"d\u0307",\u00E9:"e\u0301",\u00E8:"e\u0300",\u00EB:"e\u0308",\u1EBD:"e\u0303",\u0113:"e\u0304",\u1E17:"e\u0304\u0301",\u1E15:"e\u0304\u0300",\u0115:"e\u0306",\u011B:"e\u030C",\u00EA:"e\u0302",\u1EBF:"e\u0302\u0301",\u1EC1:"e\u0302\u0300",\u1EC5:"e\u0302\u0303",\u0117:"e\u0307",\u1E1F:"f\u0307",\u01F5:"g\u0301",\u1E21:"g\u0304",\u011F:"g\u0306",\u01E7:"g\u030C",\u011D:"g\u0302",\u0121:"g\u0307",\u1E27:"h\u0308",\u021F:"h\u030C",\u0125:"h\u0302",\u1E23:"h\u0307",\u00ED:"i\u0301",\u00EC:"i\u0300",\u00EF:"i\u0308",\u1E2F:"i\u0308\u0301",\u0129:"i\u0303",\u012B:"i\u0304",\u012D:"i\u0306",\u01D0:"i\u030C",\u00EE:"i\u0302",\u01F0:"j\u030C",\u0135:"j\u0302",\u1E31:"k\u0301",\u01E9:"k\u030C",\u013A:"l\u0301",\u013E:"l\u030C",\u1E3F:"m\u0301",\u1E41:"m\u0307",\u0144:"n\u0301",\u01F9:"n\u0300",\u00F1:"n\u0303",\u0148:"n\u030C",\u1E45:"n\u0307",\u00F3:"o\u0301",\u00F2:"o\u0300",\u00F6:"o\u0308",\u022B:"o\u0308\u0304",\u00F5:"o\u0303",\u1E4D:"o\u0303\u0301",\u1E4F:"o\u0303\u0308",\u022D:"o\u0303\u0304",\u014D:"o\u0304",\u1E53:"o\u0304\u0301",\u1E51:"o\u0304\u0300",\u014F:"o\u0306",\u01D2:"o\u030C",\u00F4:"o\u0302",\u1ED1:"o\u0302\u0301",\u1ED3:"o\u0302\u0300",\u1ED7:"o\u0302\u0303",\u022F:"o\u0307",\u0231:"o\u0307\u0304",\u0151:"o\u030B",\u1E55:"p\u0301",\u1E57:"p\u0307",\u0155:"r\u0301",\u0159:"r\u030C",\u1E59:"r\u0307",\u015B:"s\u0301",\u1E65:"s\u0301\u0307",\u0161:"s\u030C",\u1E67:"s\u030C\u0307",\u015D:"s\u0302",\u1E61:"s\u0307",\u1E97:"t\u0308",\u0165:"t\u030C",\u1E6B:"t\u0307",\u00FA:"u\u0301",\u00F9:"u\u0300",\u00FC:"u\u0308",\u01D8:"u\u0308\u0301",\u01DC:"u\u0308\u0300",\u01D6:"u\u0308\u0304",\u01DA:"u\u0308\u030C",\u0169:"u\u0303",\u1E79:"u\u0303\u0301",\u016B:"u\u0304",\u1E7B:"u\u0304\u0308",\u016D:"u\u0306",\u01D4:"u\u030C",\u00FB:"u\u0302",\u016F:"u\u030A",\u0171:"u\u030B",\u1E7D:"v\u0303",\u1E83:"w\u0301",\u1E81:"w\u0300",\u1E85:"w\u0308",\u0175:"w\u0302",\u1E87:"w\u0307",\u1E98:"w\u030A",\u1E8D:"x\u0308",\u1E8B:"x\u0307",\u00FD:"y\u0301",\u1EF3:"y\u0300",\u00FF:"y\u0308",\u1EF9:"y\u0303",\u0233:"y\u0304",\u0177:"y\u0302",\u1E8F:"y\u0307",\u1E99:"y\u030A",\u017A:"z\u0301",\u017E:"z\u030C",\u1E91:"z\u0302",\u017C:"z\u0307",\u00C1:"A\u0301",\u00C0:"A\u0300",\u00C4:"A\u0308",\u01DE:"A\u0308\u0304",\u00C3:"A\u0303",\u0100:"A\u0304",\u0102:"A\u0306",\u1EAE:"A\u0306\u0301",\u1EB0:"A\u0306\u0300",\u1EB4:"A\u0306\u0303",\u01CD:"A\u030C",\u00C2:"A\u0302",\u1EA4:"A\u0302\u0301",\u1EA6:"A\u0302\u0300",\u1EAA:"A\u0302\u0303",\u0226:"A\u0307",\u01E0:"A\u0307\u0304",\u00C5:"A\u030A",\u01FA:"A\u030A\u0301",\u1E02:"B\u0307",\u0106:"C\u0301",\u010C:"C\u030C",\u0108:"C\u0302",\u010A:"C\u0307",\u010E:"D\u030C",\u1E0A:"D\u0307",\u00C9:"E\u0301",\u00C8:"E\u0300",\u00CB:"E\u0308",\u1EBC:"E\u0303",\u0112:"E\u0304",\u1E16:"E\u0304\u0301",\u1E14:"E\u0304\u0300",\u0114:"E\u0306",\u011A:"E\u030C",\u00CA:"E\u0302",\u1EBE:"E\u0302\u0301",\u1EC0:"E\u0302\u0300",\u1EC4:"E\u0302\u0303",\u0116:"E\u0307",\u1E1E:"F\u0307",\u01F4:"G\u0301",\u1E20:"G\u0304",\u011E:"G\u0306",\u01E6:"G\u030C",\u011C:"G\u0302",\u0120:"G\u0307",\u1E26:"H\u0308",\u021E:"H\u030C",\u0124:"H\u0302",\u1E22:"H\u0307",\u00CD:"I\u0301",\u00CC:"I\u0300",\u00CF:"I\u0308",\u1E2E:"I\u0308\u0301",\u0128:"I\u0303",\u012A:"I\u0304",\u012C:"I\u0306",\u01CF:"I\u030C",\u00CE:"I\u0302",\u0130:"I\u0307",\u0134:"J\u0302",\u1E30:"K\u0301",\u01E8:"K\u030C",\u0139:"L\u0301",\u013D:"L\u030C",\u1E3E:"M\u0301",\u1E40:"M\u0307",\u0143:"N\u0301",\u01F8:"N\u0300",\u00D1:"N\u0303",\u0147:"N\u030C",\u1E44:"N\u0307",\u00D3:"O\u0301",\u00D2:"O\u0300",\u00D6:"O\u0308",\u022A:"O\u0308\u0304",\u00D5:"O\u0303",\u1E4C:"O\u0303\u0301",\u1E4E:"O\u0303\u0308",\u022C:"O\u0303\u0304",\u014C:"O\u0304",\u1E52:"O\u0304\u0301",\u1E50:"O\u0304\u0300",\u014E:"O\u0306",\u01D1:"O\u030C",\u00D4:"O\u0302",\u1ED0:"O\u0302\u0301",\u1ED2:"O\u0302\u0300",\u1ED6:"O\u0302\u0303",\u022E:"O\u0307",\u0230:"O\u0307\u0304",\u0150:"O\u030B",\u1E54:"P\u0301",\u1E56:"P\u0307",\u0154:"R\u0301",\u0158:"R\u030C",\u1E58:"R\u0307",\u015A:"S\u0301",\u1E64:"S\u0301\u0307",\u0160:"S\u030C",\u1E66:"S\u030C\u0307",\u015C:"S\u0302",\u1E60:"S\u0307",\u0164:"T\u030C",\u1E6A:"T\u0307",\u00DA:"U\u0301",\u00D9:"U\u0300",\u00DC:"U\u0308",\u01D7:"U\u0308\u0301",\u01DB:"U\u0308\u0300",\u01D5:"U\u0308\u0304",\u01D9:"U\u0308\u030C",\u0168:"U\u0303",\u1E78:"U\u0303\u0301",\u016A:"U\u0304",\u1E7A:"U\u0304\u0308",\u016C:"U\u0306",\u01D3:"U\u030C",\u00DB:"U\u0302",\u016E:"U\u030A",\u0170:"U\u030B",\u1E7C:"V\u0303",\u1E82:"W\u0301",\u1E80:"W\u0300",\u1E84:"W\u0308",\u0174:"W\u0302",\u1E86:"W\u0307",\u1E8C:"X\u0308",\u1E8A:"X\u0307",\u00DD:"Y\u0301",\u1EF2:"Y\u0300",\u0178:"Y\u0308",\u1EF8:"Y\u0303",\u0232:"Y\u0304",\u0176:"Y\u0302",\u1E8E:"Y\u0307",\u0179:"Z\u0301",\u017D:"Z\u030C",\u1E90:"Z\u0302",\u017B:"Z\u0307",\u03AC:"\u03B1\u0301",\u1F70:"\u03B1\u0300",\u1FB1:"\u03B1\u0304",\u1FB0:"\u03B1\u0306",\u03AD:"\u03B5\u0301",\u1F72:"\u03B5\u0300",\u03AE:"\u03B7\u0301",\u1F74:"\u03B7\u0300",\u03AF:"\u03B9\u0301",\u1F76:"\u03B9\u0300",\u03CA:"\u03B9\u0308",\u0390:"\u03B9\u0308\u0301",\u1FD2:"\u03B9\u0308\u0300",\u1FD1:"\u03B9\u0304",\u1FD0:"\u03B9\u0306",\u03CC:"\u03BF\u0301",\u1F78:"\u03BF\u0300",\u03CD:"\u03C5\u0301",\u1F7A:"\u03C5\u0300",\u03CB:"\u03C5\u0308",\u03B0:"\u03C5\u0308\u0301",\u1FE2:"\u03C5\u0308\u0300",\u1FE1:"\u03C5\u0304",\u1FE0:"\u03C5\u0306",\u03CE:"\u03C9\u0301",\u1F7C:"\u03C9\u0300",\u038E:"\u03A5\u0301",\u1FEA:"\u03A5\u0300",\u03AB:"\u03A5\u0308",\u1FE9:"\u03A5\u0304",\u1FE8:"\u03A5\u0306",\u038F:"\u03A9\u0301",\u1FFA:"\u03A9\u0300"},vn=function(){function e(r,n){this.mode=void 0,this.gullet=void 0,this.settings=void 0,this.leftrightDepth=void 0,this.nextToken=void 0,this.mode="math",this.gullet=new Ii(r,n,this.mode),this.settings=n,this.leftrightDepth=0}var t=e.prototype;return t.expect=function(r,n){if(n===void 0&&(n=!0),this.fetch().text!==r)throw new v("Expected '"+r+"', got '"+this.fetch().text+"'",this.fetch());n&&this.consume()},t.consume=function(){this.nextToken=null},t.fetch=function(){return this.nextToken==null&&(this.nextToken=this.gullet.expandNextToken()),this.nextToken},t.switchMode=function(r){this.mode=r,this.gullet.switchMode(r)},t.parse=function(){this.settings.globalGroup||this.gullet.beginGroup(),this.settings.colorIsTextColor&&this.gullet.macros.set("\\color","\\textcolor");var r=this.parseExpression(!1);return this.expect("EOF"),this.settings.globalGroup||this.gullet.endGroup(),r},t.parseExpression=function(r,n){for(var o=[];;){this.mode==="math"&&this.consumeSpaces();var a=this.fetch();if(e.endOfExpression.indexOf(a.text)!==-1||n&&a.text===n||r&&_e[a.text]&&_e[a.text].infix)break;var l=this.parseAtom(n);if(l){if(l.type==="internal")continue}else break;o.push(l)}return this.mode==="text"&&this.formLigatures(o),this.handleInfixNodes(o)},t.handleInfixNodes=function(r){for(var n=-1,o,a=0;a<r.length;a++)if(r[a].type==="infix"){if(n!==-1)throw new v("only one infix operator per group",r[a].token);n=a,o=r[a].replaceWith}if(n!==-1&&o){var l,m,u=r.slice(0,n),f=r.slice(n+1);u.length===1&&u[0].type==="ordgroup"?l=u[0]:l={type:"ordgroup",mode:this.mode,body:u},f.length===1&&f[0].type==="ordgroup"?m=f[0]:m={type:"ordgroup",mode:this.mode,body:f};var x;return o==="\\\\abovefrac"?x=this.callFunction(o,[l,r[n],m],[]):x=this.callFunction(o,[l,m],[]),[x]}else return r},t.handleSupSubscript=function(r){var n=this.fetch(),o=n.text;this.consume(),this.consumeSpaces();var a=this.parseGroup(r);if(!a)throw new v("Expected group after '"+o+"'",n);return a},t.formatUnsupportedCmd=function(r){for(var n=[],o=0;o<r.length;o++)n.push({type:"textord",mode:"text",text:r[o]});var a={type:"text",mode:this.mode,body:n},l={type:"color",mode:this.mode,color:this.settings.errorColor,body:[a]};return l},t.parseAtom=function(r){var n=this.parseGroup("atom",r);if(this.mode==="text")return n;for(var o,a;;){this.consumeSpaces();var l=this.fetch();if(l.text==="\\limits"||l.text==="\\nolimits"){if(n&&n.type==="op"){var m=l.text==="\\limits";n.limits=m,n.alwaysHandleSupSub=!0}else if(n&&n.type==="operatorname"&&n.alwaysHandleSupSub){var u=l.text==="\\limits";n.limits=u}else throw new v("Limit controls must follow a math operator",l);this.consume()}else if(l.text==="^"){if(o)throw new v("Double superscript",l);o=this.handleSupSubscript("superscript")}else if(l.text==="_"){if(a)throw new v("Double subscript",l);a=this.handleSupSubscript("subscript")}else if(l.text==="'"){if(o)throw new v("Double superscript",l);var f={type:"textord",mode:this.mode,text:"\\prime"},x=[f];for(this.consume();this.fetch().text==="'";)x.push(f),this.consume();this.fetch().text==="^"&&x.push(this.handleSupSubscript("superscript")),o={type:"ordgroup",mode:this.mode,body:x}}else break}return o||a?{type:"supsub",mode:this.mode,base:n,sup:o,sub:a}:n},t.parseFunction=function(r,n){var o=this.fetch(),a=o.text,l=_e[a];if(!l)return null;if(this.consume(),n&&n!=="atom"&&!l.allowedInArgument)throw new v("Got function '"+a+"' with no arguments"+(n?" as "+n:""),o);if(this.mode==="text"&&!l.allowedInText)throw new v("Can't use function '"+a+"' in text mode",o);if(this.mode==="math"&&l.allowedInMath===!1)throw new v("Can't use function '"+a+"' in math mode",o);var m=this.parseArguments(a,l),u=m.args,f=m.optArgs;return this.callFunction(a,u,f,o,r)},t.callFunction=function(r,n,o,a,l){var m={funcName:r,parser:this,token:a,breakOnTokenText:l},u=_e[r];if(u&&u.handler)return u.handler(m,n,o);throw new v("No function handler for "+r)},t.parseArguments=function(r,n){var o=n.numArgs+n.numOptionalArgs;if(o===0)return{args:[],optArgs:[]};for(var a=[],l=[],m=0;m<o;m++){var u=n.argTypes&&n.argTypes[m],f=m<n.numOptionalArgs;(n.primitive&&u==null||n.type==="sqrt"&&m===1&&l[0]==null)&&(u="primitive");var x=this.parseGroupOfType("argument to '"+r+"'",u,f);if(f)l.push(x);else if(x!=null)a.push(x);else throw new v("Null argument, please report this as a bug")}return{args:a,optArgs:l}},t.parseGroupOfType=function(r,n,o){switch(n){case"color":return this.parseColorGroup(o);case"size":return this.parseSizeGroup(o);case"url":return this.parseUrlGroup(o);case"math":case"text":return this.parseArgumentGroup(o,n);case"hbox":{var a=this.parseArgumentGroup(o,"text");return a!=null?{type:"styling",mode:a.mode,body:[a],style:"text"}:null}case"raw":{var l=this.parseStringGroup("raw",o);return l!=null?{type:"raw",mode:"text",string:l.text}:null}case"primitive":{if(o)throw new v("A primitive argument cannot be optional");var m=this.parseGroup(r);if(m==null)throw new v("Expected group as "+r,this.fetch());return m}case"original":case null:case void 0:return this.parseArgumentGroup(o);default:throw new v("Unknown group type as "+r,this.fetch())}},t.consumeSpaces=function(){for(;this.fetch().text===" ";)this.consume()},t.parseStringGroup=function(r,n){var o=this.gullet.scanArgument(n);if(o==null)return null;for(var a="",l;(l=this.fetch()).text!=="EOF";)a+=l.text,this.consume();return this.consume(),o.text=a,o},t.parseRegexGroup=function(r,n){for(var o=this.fetch(),a=o,l="",m;(m=this.fetch()).text!=="EOF"&&r.test(l+m.text);)a=m,l+=a.text,this.consume();if(l==="")throw new v("Invalid "+n+": '"+o.text+"'",o);return o.range(a,l)},t.parseColorGroup=function(r){var n=this.parseStringGroup("color",r);if(n==null)return null;var o=/^(#[a-f0-9]{3}|#?[a-f0-9]{6}|[a-z]+)$/i.exec(n.text);if(!o)throw new v("Invalid color: '"+n.text+"'",n);var a=o[0];return/^[0-9a-f]{6}$/i.test(a)&&(a="#"+a),{type:"color-token",mode:this.mode,color:a}},t.parseSizeGroup=function(r){var n,o=!1;if(this.gullet.consumeSpaces(),!r&&this.gullet.future().text!=="{"?n=this.parseRegexGroup(/^[-+]? *(?:$|\d+|\d+\.\d*|\.\d*) *[a-z]{0,2} *$/,"size"):n=this.parseStringGroup("size",r),!n)return null;!r&&n.text.length===0&&(n.text="0pt",o=!0);var a=/([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(n.text);if(!a)throw new v("Invalid size: '"+n.text+"'",n);var l={number:+(a[1]+a[2]),unit:a[3]};if(!cr(l))throw new v("Invalid unit: '"+l.unit+"'",n);return{type:"size",mode:this.mode,value:l,isBlank:o}},t.parseUrlGroup=function(r){this.gullet.lexer.setCatcode("%",13);var n=this.parseStringGroup("url",r);if(this.gullet.lexer.setCatcode("%",14),n==null)return null;var o=n.text.replace(/\\([#$%&~_^{}])/g,"$1");return{type:"url",mode:this.mode,url:o}},t.parseArgumentGroup=function(r,n){var o=this.gullet.scanArgument(r);if(o==null)return null;var a=this.mode;n&&this.switchMode(n),this.gullet.beginGroup();var l=this.parseExpression(!1,"EOF");this.expect("EOF"),this.gullet.endGroup();var m={type:"ordgroup",mode:this.mode,loc:o.loc,body:l};return n&&this.switchMode(a),m},t.parseGroup=function(r,n){var o=this.fetch(),a=o.text,l;if(a==="{"||a==="\\begingroup"){this.consume();var m=a==="{"?"}":"\\endgroup";this.gullet.beginGroup();var u=this.parseExpression(!1,m),f=this.fetch();this.expect(m),this.gullet.endGroup(),l={type:"ordgroup",mode:this.mode,loc:Se.range(o,f),body:u,semisimple:a==="\\begingroup"||void 0}}else if(l=this.parseFunction(n,r)||this.parseSymbol(),l==null&&a[0]==="\\"&&!xn.hasOwnProperty(a)){if(this.settings.throwOnError)throw new v("Undefined control sequence: "+a,o);l=this.formatUnsupportedCmd(a),this.consume()}return l},t.formLigatures=function(r){for(var n=r.length-1,o=0;o<n;++o){var a=r[o],l=a.text;l==="-"&&r[o+1].text==="-"&&(o+1<n&&r[o+2].text==="-"?(r.splice(o,3,{type:"textord",mode:"text",loc:Se.range(a,r[o+2]),text:"---"}),n-=2):(r.splice(o,2,{type:"textord",mode:"text",loc:Se.range(a,r[o+1]),text:"--"}),n-=1)),(l==="'"||l==="`")&&r[o+1].text===l&&(r.splice(o,2,{type:"textord",mode:"text",loc:Se.range(a,r[o+1]),text:l+l}),n-=1)}},t.parseSymbol=function(){var r=this.fetch(),n=r.text;if(/^\\verb[^a-zA-Z]/.test(n)){this.consume();var o=n.slice(5),a=o.charAt(0)==="*";if(a&&(o=o.slice(1)),o.length<2||o.charAt(0)!==o.slice(-1))throw new v(`\\verb assertion failed --
                    please report what input caused this bug`);return o=o.slice(1,-1),{type:"verb",mode:"text",body:o,star:a}}yn.hasOwnProperty(n[0])&&!K[this.mode][n[0]]&&(this.settings.strict&&this.mode==="math"&&this.settings.reportNonstrict("unicodeTextInMathMode",'Accented Unicode text character "'+n[0]+'" used in math mode',r),n=yn[n[0]]+n.substr(1));var l=qi.exec(n);l&&(n=n.substring(0,l.index),n==="i"?n="\u0131":n==="j"&&(n="\u0237"));var m;if(K[this.mode][n]){this.settings.strict&&this.mode==="math"&&o0.indexOf(n)>=0&&this.settings.reportNonstrict("unicodeTextInMathMode",'Latin-1/Unicode text character "'+n[0]+'" used in math mode',r);var u=K[this.mode][n].group,f=Se.range(r),x;if(yo.hasOwnProperty(u)){var y=u;x={type:"atom",mode:this.mode,family:y,loc:f,text:n}}else x={type:u,mode:this.mode,loc:f,text:n};m=x}else if(n.charCodeAt(0)>=128)this.settings.strict&&($0(n.charCodeAt(0))?this.mode==="math"&&this.settings.reportNonstrict("unicodeTextInMathMode",'Unicode text character "'+n[0]+'" used in math mode',r):this.settings.reportNonstrict("unknownSymbol",'Unrecognized Unicode character "'+n[0]+'"'+(" ("+n.charCodeAt(0)+")"),r)),m={type:"textord",mode:"text",loc:Se.range(r),text:n};else return null;if(this.consume(),l)for(var S=0;S<l[0].length;S++){var w=l[0][S];if(!bn[w])throw new v("Unknown accent ' "+w+"'",r);var A=bn[w][this.mode];if(!A)throw new v("Accent "+w+" unsupported in "+this.mode+" mode",r);m={type:"accent",mode:this.mode,loc:Se.range(r),label:A,isStretchy:!1,isShifty:!0,base:m}}return m},e}();vn.endOfExpression=["}","\\endgroup","\\end","\\right","&"];var Hi=function(e,t){if(!(typeof e=="string"||e instanceof String))throw new TypeError("KaTeX can only parse string typed expression");var r=new vn(e,t);delete r.gullet.macros.current["\\df@tag"];var n=r.parse();if(delete r.gullet.macros.current["\\current@color"],delete r.gullet.macros.current["\\color"],r.gullet.macros.get("\\df@tag")){if(!t.displayMode)throw new v("\\tag works only in display equations");r.gullet.feed("\\df@tag"),n=[{type:"tag",mode:"text",body:n,tag:r.parse()}]}return n},I0=Hi,wn=function(e,t,r){t.textContent="";var n=H0(e,r).toNode();t.appendChild(n)};typeof document<"u"&&document.compatMode!=="CSS1Compat"&&(typeof console<"u"&&console.warn("Warning: KaTeX doesn't work in quirks mode. Make sure your website has a suitable doctype."),wn=function(){throw new v("KaTeX doesn't work in quirks mode.")});var Ei=function(e,t){var r=H0(e,t).toMarkup();return r},Pi=function(e,t){var r=new jt(t);return I0(e,r)},kn=function(e,t,r){if(r.throwOnError||!(e instanceof v))throw e;var n=b.makeSpan(["katex-error"],[new ge(t)]);return n.setAttribute("title",e.toString()),n.setAttribute("style","color:"+r.errorColor),n},H0=function(e,t){var r=new jt(t);try{var n=I0(e,r);return Uo(n,e,r)}catch(o){return kn(o,e,r)}},Oi=function(e,t){var r=new jt(t);try{var n=I0(e,r);return Yo(n,e,r)}catch(o){return kn(o,e,r)}},Fi={version:"0.13.0",render:wn,renderToString:Ei,ParseError:v,__parse:Pi,__renderToDomTree:H0,__renderToHTMLTree:Oi,__setFontMetrics:xo,__defineSymbol:i,__defineMacro:h,__domTree:{Span:pt,Anchor:Kt,SymbolNode:ge,SvgNode:Oe,PathNode:Xe,LineNode:Qt}},Vi=Fi;return F=F.default,F}()})});V0.ParseError;var Dn=V0.renderToString;function In(L,O,P){for(var F=P,V=0,v=L.length;F<O.length;){var X=O[F];if(V<=0&&O.slice(F,F+v)===L)return F;X==="\\"?F++:X==="{"?V++:X==="}"&&V--,F++}return-1}function Hn(L){return L.replace(/[-/\\^$*+?.()|[\]{}]/g,"\\$&")}var En=/^\\begin{/;function Pn(L,O){for(var P,F=[],V=new RegExp("("+O.map(function(ve){return Hn(ve.left)}).join("|")+")");P=L.search(V),P!==-1;){P>0&&(F.push({type:"text",data:L.slice(0,P)}),L=L.slice(P));var v=O.findIndex(function(ve){return L.startsWith(ve.left)});if(P=In(O[v].right,L,O[v].left.length),P===-1)break;var X=L.slice(0,P+O[v].right.length),ye=En.test(X)?X:L.slice(O[v].left.length,P);F.push({type:"math",data:ye,rawData:X,display:O[v].display}),L=L.slice(P+O[v].right.length)}return L!==""&&F.push({type:"text",data:L}),F}function On(L,O,P,F){for(var V=Pn(L,O),v=[],X=0;X<V.length;X++)if(V[X].type==="text")v.push(V[X].data);else{var ye=V[X].data,ve=V[X].display;try{var Ue=Dn(ye,{displayMode:ve,macros:F});v.push(Ue)}catch(Ye){if(P)throw Ye;v.push(V[X].data)}}return v.join("")}Nn(`.___Latex___1nfc2_1 ._latex_1nfc2_1 {
  font: inherit
}
`,{});var Fn=function(L){qn(O,L);function O(){return L!==null&&L.apply(this,arguments)||this}return O.prototype.render=function(){var P=this.props,F=P.children,V=P.delimiters,v=P.strict,X=P.macros,ye=On(F,V,v,X);return wt.createElement("span",{className:"__Latex__",dangerouslySetInnerHTML:{__html:ye}})},O.defaultProps={delimiters:[{left:"$$",right:"$$",display:!0},{left:"\\(",right:"\\)",display:!1},{left:"$",right:"$",display:!1},{left:"\\[",right:"\\]",display:!0}],strict:!1},O}(wt.Component);const Vn=Fn,Wn=vt.div`
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
`,_n=vt.div`
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
`,Gn=vt.div`
  font-family: "Roboto Mono";
  padding: 0.5rem;
`,Un=vt.div`
  font-family: "Open Sans";
  width: 100%;
  text-align: center;
  padding: 1rem;
`,Yn=vt.button`
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
`,$n=({domain:L,style:O,substance:P,variation:F,index:V,answer:v,showAnswer:X,onSelect:ye,onDeselect:ve})=>{const[Ue,Ye]=wt.useState(!1);return qe.jsxs(_n,{checked:Ue,answer:v,showAnswer:X,onClick:()=>{Ye($e=>($e?ve(V):ye(V),!$e))},children:[qe.jsx(Gn,{children:V+1}),qe.jsx(ji,{name:`choice-${V}`,domain:L,style:O,substance:P,variation:F,interactive:!1,excludeWarnings:[]},`choice-${V}`)]})};function W0(L){const[O,P]=wt.useState(new Set),[F,V]=wt.useState(!1),v=L.diagrams.map(({domain:X,style:ye,substance:ve,variation:Ue,answer:Ye},$e)=>qe.jsx($n,{domain:X,style:ye,substance:ve,variation:Ue,index:$e,answer:Ye,showAnswer:F,onSelect:mt=>{P(je=>je.add(mt)),V(!1)},onDeselect:mt=>P(je=>(je.delete(mt),V(!1),je))},`problem-choice-${$e}`));return qe.jsxs(Wn,{onClick:X=>X.stopPropagation(),children:[qe.jsx(Un,{children:qe.jsx(Vn,{delimiters:[{right:"$",left:"$",display:!1}],children:L.prompt})}),v,qe.jsx(Yn,{onClick:()=>V(!0),children:"Check Answer"})]})}let _0;Bn={title:"Example/Multiple Choice Problem Component",component:W0},_0=L=>qe.jsx($i,{theme:{default:"#fff"},children:qe.jsx(W0,{...L})}),et=_0.bind({}),et.args={prompt:"Choose the correct Lewis structure for $\\mathrm{HCN}$.",diagrams:[{substance:kt,domain:zt,style:St,variation:"1",answer:!0},{substance:kt,domain:zt,style:St,variation:"2",answer:!0},{substance:kt,domain:zt,style:St,variation:"3",answer:!1},{substance:kt,domain:zt,style:St,variation:"4",answer:!1}]},et.parameters={...et.parameters,docs:{...(G0=et.parameters)==null?void 0:G0.docs,source:{originalSource:`args => <ThemeProvider theme={{
  default: "#fff"
}}>
    <MultipleChoiceProblem {...args} />
  </ThemeProvider>`,...(Y0=(U0=et.parameters)==null?void 0:U0.docs)==null?void 0:Y0.source}}},Cn=["Props"]});export{et as Props,Cn as __namedExportsOrder,Ki as __tla,Bn as default};
