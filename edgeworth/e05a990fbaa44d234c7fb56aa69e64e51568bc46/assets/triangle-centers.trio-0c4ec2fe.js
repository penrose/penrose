import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-2d6b8dc2.js";import"./index-0dcc9a9a.js";const a=`-- create a triangle with vertices i, j, k
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
`,l={substance:a,style:[{contents:e,resolver:n}],domain:i,variation:"ClamshellKookabura228"};export{l as default};
