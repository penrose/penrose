import{s as e,r as n}from"./euclidean.style-64788f49.js";import{d as t}from"./geometry.domain-952a3e66.js";import"./resolver-691c4a84.js";import"./iframe-f4259546.js";const r=`-- TODO full example can sometimes throw error "2147482504"
Point A, B, C, D, E, F, d, b
Circle c := CircleR(F, C)
Segment AC := Chord(c, A, C)
Segment EC := Chord(c, E, C)
Segment FB := Radius(c, B)
Segment FD := Radius(c, D)
Segment FC := Radius(c, C)
Segment Fd := PerpendicularBisectorLabelPts(EC, F, d)
Segment Fb := PerpendicularBisectorLabelPts(AC, F, b)
On(b, FB)
On(d, FD)
Collinear(F, b, B)
Collinear(F, d, D)
Segment EA := Diameter(c, E, A)
AutoLabel A, B, C, D, E, F, d, b`,l={substance:r,style:[{contents:e,resolver:n}],domain:t,variation:"CosmopolitanBeaver4985"};export{l as default};
//# sourceMappingURL=c11p12.trio-edb7e447.js.map
