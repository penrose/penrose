import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-8ca2bfde.js";import"./index-6887b9a4.js";const a=`-- relative-orientation.substance
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

`,t={substance:a,style:[{contents:e,resolver:n}],domain:i,variation:"SylvanDolphin4902",excludeWarnings:[]};export{t as default};
