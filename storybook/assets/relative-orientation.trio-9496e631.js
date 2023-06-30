import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-e48fd28d.js";import"./resolver-028225a2.js";import"./iframe-2bac3a0c.js";const a=`-- relative-orientation.substance
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

`,o={substance:a,style:[{contents:e,resolver:n}],domain:i,variation:"SylvanDolphin4902",excludeWarnings:[]};export{o as default};
//# sourceMappingURL=relative-orientation.trio-9496e631.js.map
