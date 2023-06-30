import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-0a60d5c5.js";import"./index-d0dd7cba.js";const a=`-- create a triangle with vertices i, j, k
Vertex i, j, k
Edge ij := MakeEdge(i,j)
Edge jk := MakeEdge(j,k)
Edge ki := MakeEdge(k,i)
Triangle ijk := MakeTriangle(i,j,k)

-- draw two different triangle centers
Point a, b
a := Circumcenter(ijk)
b := Incenter(ijk)

Circle Ca := Circumcircle(ijk)
Circle Cb := Incircle(ijk)

AutoLabel All
NoLabel ij
NoLabel jk
NoLabel ki
Label i "i"
Label j "j"
Label k "k"
`,l={substance:a,style:[{contents:e,resolver:n}],domain:i,variation:"ClamshellKookabura228",excludeWarnings:[]};export{l as default};
