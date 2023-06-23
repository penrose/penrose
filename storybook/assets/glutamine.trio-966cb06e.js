import{m as n}from"./resolver-e57fa3ea.js";import{d as o}from"./molecules.domain-55131764.js";import"./iframe-5aba9dd1.js";const e=`-- Atoms

Carbon C1, C2, C3, C4, C5
Hydrogen H1, H2, H3, H4, H5, H6, H7, H8, H9
Oxygen O1, O2, O3
Nitrogen N1, N2

-- C-C Bonds

Bond C1C2 := MakeSingleBond(C1, C2)
Bond C2C3 := MakeSingleBond(C2, C3)
Bond C3C4 := MakeSingleBond(C3, C4)
Bond C4C5 := MakeSingleBond(C4, C5)

-- C-H Bonds

Bond C2H3 := MakeSingleBond(C2, H3)
Bond C2H4 := MakeSingleBond(C2, H4)
Bond C3H5 := MakeSingleBond(C3, H5)
Bond C3H6 := MakeSingleBond(C3, H6)

-- C-O Bonds

Bond C1O1 := MakeDoubleBond(C1, O1)
Bond C5O2 := MakeDoubleBond(C5, O2)
Bond C5O3 := MakeSingleBond(C5, O3)

-- C-N Bonds

Bond C1N1 := MakeSingleBond(C1, N1)
Bond C4N2 := MakeSingleBond(C4, N2)

-- O-H Bonds

Bond O3H9 := MakeSingleBond(O3, H9)

-- N-H Bonds
Bond N1H1 := MakeSingleBond(N1, H1)
Bond N1H2 := MakeSingleBond(N1, H2)
Bond N2H7 := MakeSingleBond(N2, H7)
Bond N2H8 := MakeSingleBond(N2, H8)`,r=n("molecules"),i=`canvas {
    width = 800
    height = 700
}

-- Atoms

forall Atom x {
    string x.symbol_string = ""
    shape x.icon = Circle {
        strokeWidth : 0.0
        r : 15.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    shape x.symbol = Equation {
        string : x.symbol_string
        rotation : 0.0
        center : x.icon.center
    }
    x.layering = x.symbol above x.icon
}

forall Carbon x {
    override x.symbol_string = "C"
}

forall Hydrogen x {
    override x.symbol_string = "H"
}

forall Oxygen x {
    override x.symbol_string = "O"
}

forall Nitrogen x {
    override x.symbol_string = "N"
}

-- Bonds

forall Bond b
where b := MakeSingleBond(x, y)
with Atom x; Atom y {
    shape b.icon = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 2.0
    }
    encourage equal(vdist(x.icon.center, y.icon.center), 60.0)
    b.icon below x.icon, y.icon
}

forall Bond b
where b := MakeDoubleBond(x, y)
with Atom x; Atom y {
    shape b.icon = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 6.0
    }
    shape b.line2 = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth: 2.0
    }
    b.icon below x.icon, y.icon
    encourage equal(vdist(x.icon.center, y.icon.center), 60.0)
}

forall Bond b
where b := MakeTripleBond(x, y)
with Atom x; Atom y {
    shape b.icon = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth: 10.0
    }
    shape b.line2 = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 6.0
    }
    shape b.line3 = Line {
        start : x.icon.center
        end : y.icon.center
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth: 2.0
    }
    b.line3 above b.line2
    b.line2 above b.icon
    b.icon below x.icon, y.icon
    ensure equal(vdist(x.icon.center, y.icon.center), 60.0)
}

-- Repulsion

forall Atom x; Atom y {
    encourage notTooClose(x.icon, y.icon)
}

`,s={substance:e,style:[{contents:i,resolver:r}],domain:o,variation:"MemoriesDeer750"};export{s as default};
//# sourceMappingURL=glutamine.trio-966cb06e.js.map
