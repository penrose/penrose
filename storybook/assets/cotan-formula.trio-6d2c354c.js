import{s as e,r as n,d as i}from"./triangle-mesh-2d.domain-13cf9482.js";import"./resolver-16fd6907.js";import"./iframe-c84affa1.js";const l=`Vertex i, j, k, l

Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge jl := MakeEdge( j, l )

Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )

Corner ckij := MakeCorner( k, i, j )
Corner clji := MakeCorner( l, j, i )

DualEdge starij := MakeDualEdge( ijk, jil )

Point x := Circumcenter(ijk)
Point y := Circumcenter(jil)

Circle Cijk := Circumcircle(ijk)
Circle Cjil := Circumcircle(jil)

-- Length L := DualEdgeLength(starij)
-- Length L := EdgeLength(ki)

AutoLabel All
NoLabel ij
NoLabel jk
NoLabel ki
NoLabel il
NoLabel jl
Label ckij "α"
Label clji "β"
Label i "i"
Label j "j"
Label k "k"
Label l "l"
`,k={substance:l,style:[{contents:e,resolver:n}],domain:i,variation:"LumberjackHornet927"};export{k as default};
//# sourceMappingURL=cotan-formula.trio-6d2c354c.js.map
