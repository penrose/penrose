import{s as e,r as n,d as a}from"./triangle-mesh-2d.domain-917a0a29.js";import"./index-5c0b39e3.js";const r=`-- angle-equivalence.substance
-- 
-- This example re-diagrams Figure 1 from
-- Crane, "Conformal Geometry of Simplicial Surfaces"

-- Initial mesh

Vertex i, j, k, l -- for the two "inner" triangles
Vertex a, b, c -- for the three "ears"

Edge ij := MakeEdge( i, j )
Edge jk := MakeEdge( j, k )
Edge ki := MakeEdge( k, i )
Edge il := MakeEdge( i, l )
Edge lj := MakeEdge( l, j )
Edge ka := MakeEdge( k, a )
Edge ja := MakeEdge( j, a )
Edge ib := MakeEdge( i, b )
Edge kb := MakeEdge( k, b )
Edge lc := MakeEdge( l, c )
Edge ic := MakeEdge( i, c )

IsBoundaryEdge( lj )
IsBoundaryEdge( ka )
IsBoundaryEdge( ja )
IsBoundaryEdge( ib )
IsBoundaryEdge( kb )
IsBoundaryEdge( lc )
IsBoundaryEdge( ic )

Triangle ijk := MakeTriangle( i, j, k )
Triangle jil := MakeTriangle( j, i, l )
Triangle kja := MakeTriangle( k, j, a )
Triangle ikb := MakeTriangle( i, k, b )
Triangle lic := MakeTriangle( l, i, c )

Corner cjil := MakeCorner( j, i, l )
Corner cilj := MakeCorner( i, l, j )
Corner clji := MakeCorner( l, j, i )

-- Same mesh, under a similarity transformation

Vertex I := similarity(i)
Vertex J := similarity(j)
Vertex K := similarity(k)
Vertex L := similarity(l)
Vertex A := similarity(a)
Vertex B := similarity(b)
Vertex C := similarity(c)

Edge IJ := MakeEdge( I, J )
Edge JK := MakeEdge( J, K )
Edge KI := MakeEdge( K, I )
Edge IL := MakeEdge( I, L )
Edge LJ := MakeEdge( L, J )
Edge KA := MakeEdge( K, A )
Edge JA := MakeEdge( J, A )
Edge IB := MakeEdge( I, B )
Edge KB := MakeEdge( K, B )
Edge LC := MakeEdge( L, C )
Edge IC := MakeEdge( I, C )

IsBoundaryEdge( LJ )
IsBoundaryEdge( KA )
IsBoundaryEdge( JA )
IsBoundaryEdge( IB )
IsBoundaryEdge( KB )
IsBoundaryEdge( LC )
IsBoundaryEdge( IC )

Triangle IJK := MakeTriangle( I, J, K )
Triangle JIL := MakeTriangle( J, I, L )
Triangle KJA := MakeTriangle( K, J, A )
Triangle IKB := MakeTriangle( I, K, B )
Triangle LIC := MakeTriangle( L, I, C )

Corner cJIL := MakeCorner( J, I, L )
Corner cILJ := MakeCorner( I, L, J )
Corner cLJI := MakeCorner( L, J, I )


Label cjil "α"
Label cilj "β"
Label clji "γ"
Label cJIL "α"
Label cILJ "β"
Label cLJI "γ"
-- HasLabel( i )
-- HasLabel( j )
-- HasLabel( k )



`,l={substance:r,style:[{contents:e,resolver:n}],domain:a,variation:"ClamshellKookabura228"};export{l as default};
