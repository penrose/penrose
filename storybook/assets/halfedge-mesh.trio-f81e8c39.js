import{s as e,r as a,d as n}from"./triangle-mesh-2d.domain-56e507a9.js";import"./resolver-1ba7f18b.js";import"./iframe-196f87c1.js";const i=`Vertex i, j, k, l, m

Edge eij := MakeEdge(i,j)
Edge ejk := MakeEdge(j,k)
Edge eki := MakeEdge(k,i)
Edge eil := MakeEdge(i,l)
Edge elj := MakeEdge(l,j)
Edge eim := MakeEdge(i,m)
Edge eml := MakeEdge(m,l)

Triangle ijk := MakeTriangle(i,j,k)
Triangle jil := MakeTriangle(j,i,l)
Triangle iml := MakeTriangle(i,m,l)

Halfedge hij := MakeHalfedge(i,j)
Halfedge hji := MakeHalfedge(j,i)
Halfedge hjk := MakeHalfedge(j,k)
Halfedge hki := MakeHalfedge(k,i)
Halfedge hil := MakeHalfedge(i,l)
Halfedge hli := MakeHalfedge(l,i)

Label hij "h"
Label hji "h->twin"
Label hjk "next"
Label hki "next->next"
Label hil "twin->next"
Label hli "twin->next->twin"
`,k={substance:i,style:[{contents:e,resolver:a}],domain:n,variation:"FrenzyHawk3762"};export{k as default};
//# sourceMappingURL=halfedge-mesh.trio-f81e8c39.js.map
