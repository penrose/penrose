import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-eaec9ca2.js";import"./resolver-4b330e38.js";import"./iframe-893c3c1f.js";const a=`-- relative-orientation.substance
-- 
-- This example re-diagrams a figure from
-- Crane, "Discrete Differential Geometry: An
-- Applied Introduction", Section 2.2

Vertex i, j, k, l

Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Edge jl := MakeEdge(j,l)
Edge li := MakeEdge(l,i)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)

IsPositivelyOriented(ijk)
IsNegativelyOriented(jil)
Label i "i"
Label j "j"
Label k "k"
Label l "l"

`,o={substance:a,style:[{contents:e,resolver:n}],domain:i,variation:"SylvanDolphin4902"};export{o as default};
//# sourceMappingURL=relative-orientation.trio-ca5d0203.js.map
