import{s as e,r as n}from"./euclidean.style-71fbf884.js";import{d as t}from"./geometry.domain-952a3e66.js";import"./resolver-9b16ebb5.js";import"./iframe-7b8a5a1d.js";const r=`-- name in registry: midsegment-triangles
Point D, E, F, G, H, J, K
Let DEF := Triangle(D, E, F)
Let GJF := Triangle(G, J, F)
Segment GJ := MidSegment(DEF, G, J)  -- TODO add markers when attribute changes are implemented
Segment HK := MidSegment(GJF, H, K)
Angle aGFJ := InteriorAngle(G, F, J)
AutoLabel D, E, F, G, H, J, K`,m={substance:r,style:[{contents:e,resolver:n}],domain:t,variation:"FollyCamel2864",excludeWarnings:[]};export{m as default};
//# sourceMappingURL=c05p01.trio-cd6a7978.js.map
