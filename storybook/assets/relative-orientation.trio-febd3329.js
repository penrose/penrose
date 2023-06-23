import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-97c9a206.js";import"./resolver-7258f632.js";import"./iframe-7ac534ec.js";const a=`-- relative-orientation.substance
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
//# sourceMappingURL=relative-orientation.trio-febd3329.js.map
