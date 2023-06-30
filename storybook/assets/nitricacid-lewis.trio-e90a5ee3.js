import{s as n,r as o}from"./lewis.style-fd1674ec.js";import{d as e}from"./molecules.domain-55131764.js";import"./resolver-9b16ebb5.js";import"./iframe-7b8a5a1d.js";const r=`Hydrogen h
Nitrogen n
Oxygen o1, o2, o3
-- bonds
Bond b1 := MakeSingleBond(h, o1)
Bond b2 := MakeSingleBond(o1, n)
Bond b3 := MakeDoubleBond(n, o2)
Bond b4 := MakeSingleBond(o3, n)
-- electrons
ZeroValenceElectrons(h)
ZeroValenceElectrons(n)
FourValenceElectrons(o1)
FourValenceElectrons(o2)
SixValenceElectrons(o3)
-- layout
-- Collinear(n, o1, h)
-- VerticalAlign(o2, n)
-- HorizontalAlign(o1, o3)
`,s={substance:r,style:[{contents:n,resolver:o}],domain:e,variation:"DeadlockWren0150",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=nitricacid-lewis.trio-e90a5ee3.js.map
