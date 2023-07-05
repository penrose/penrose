import{s as e,r as n}from"./euclidean.style-b4b43ffc.js";import{d as t}from"./geometry.domain-952a3e66.js";import"./resolver-1df37e9f.js";import"./iframe-71d88643.js";const r=`-- name in registry: midsegment-triangles
Point D, E, F, G, H, J, K
Let DEF := Triangle(D, E, F)
Let GJF := Triangle(G, J, F)
Segment GJ := MidSegment(DEF, G, J)  -- TODO add markers when attribute changes are implemented
Segment HK := MidSegment(GJF, H, K)
Angle aGFJ := InteriorAngle(G, F, J)
AutoLabel D, E, F, G, H, J, K`,m={substance:r,style:[{contents:e,resolver:n}],domain:t,variation:"FollyCamel2864",excludeWarnings:[]};export{m as default};
//# sourceMappingURL=c05p01.trio-cc1d45cf.js.map
