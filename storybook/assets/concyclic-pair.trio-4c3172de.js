import{s as e,r as n,d as a}from"./triangle-mesh-2d.domain-e48fd28d.js";import"./resolver-028225a2.js";import"./iframe-2bac3a0c.js";const r=`-- concyclic-pair.substance
-- 
-- This example re-diagrams Figure 9 from
-- Gillespie et al, "Discrete Conformal Equivalence
-- of Polyhedral Surfaces"

Vertex i, j, k, l

Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Edge il := MakeEdge(i,l)
Edge lj := MakeEdge(l,j)
Edge kl := MakeEdge(k,l)
IsFlipped(kl)
IsBoundaryEdge(jk)
IsBoundaryEdge(ki)
IsBoundaryEdge(il)
IsBoundaryEdge(lj)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)
Concyclic( ijk, jil )

Corner cK := MakeCorner(k,i,j)
Label cK "α"
Corner cL := MakeCorner(l,j,i)
Label cL "β"
Corner cI := MakeCorner(i,l,k)
Label cI "β’"
Corner cJ := MakeCorner(j,k,l)
Label cJ "α’"

`,k={substance:r,style:[{contents:e,resolver:n}],domain:a,variation:"DamaskWeasel66717",excludeWarnings:[]};export{k as default};
//# sourceMappingURL=concyclic-pair.trio-4c3172de.js.map
