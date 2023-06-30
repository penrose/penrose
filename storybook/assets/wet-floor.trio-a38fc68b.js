import{s as n,r as o,d as e}from"./atoms-and-bonds.domain-3a953dd8.js";import{m as r}from"./resolver-028225a2.js";import"./iframe-2bac3a0c.js";const l=`-- center
Hydrogen H1, H2
Oxygen O1
SBond(H1, O1)
SBond(H2, O1)

Hydrogen H3, H4
Oxygen O2
SBond(H3, O2)
SBond(H4, O2)

Hydrogen H5, H6
Oxygen O3
SBond(H5, O3)
SBond(H6, O3)

Hydrogen H7, H8
Oxygen O4
SBond(H7, O4)
SBond(H8, O4)

WBond(O1, H3)
WBond(H1, O3)
WBond(H2, O4)`,s=r("atoms-and-bonds"),d=`color {
  pink = #F4ABAA
  blue = #2A3166
}


forall Atom a  {
    override a.symbol.fillColor = color.blue
    override a.background.fillColor = color.pink
}

forall Atom a, b where SBond(a, b) as e {
    override e.symbol.strokeColor = color.blue
    override e.symbol.strokeWidth = 3
}

forall Atom a, b where WBond(a, b) as e {
    override e.symbol.strokeColor = color.blue
}`,O={substance:l,style:[{contents:n,resolver:o},{contents:d,resolver:s}],domain:e,variation:"MosslandsFerret98651",excludeWarnings:[]};export{O as default};
//# sourceMappingURL=wet-floor.trio-a38fc68b.js.map
