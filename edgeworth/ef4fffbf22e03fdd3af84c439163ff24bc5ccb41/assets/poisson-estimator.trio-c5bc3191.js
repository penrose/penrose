import{s as n,r as a,d as o}from"./walk-on-spheres.domain-16485ad5.js";import"./index-941eab72.js";const e=`-- Describe a short walk used to estimate the solution to
-- a Poisson equation Δu = f, which involves both boundary
-- samples x and source samples y.

Domain U

Point x0
Ball B0 := ballAround( x0 )
Point y0 := sampleInterior( B0 )
isSourceSample( y0 )
Point x1 := sampleBoundary( B0 )
Ball B1 := ballAround( x1 )
Point y1 := sampleInterior( B1 )
isSourceSample( y1 )
Point x2 := sampleBoundary( B1 )
Ball B2 := ballAround( x2 )
Point y2 := sampleInterior( B2 )
isSourceSample( y2 )
Point x3 := sampleBoundary( B2 )
Ball B3 := ballAround( x3 )
Point y3 := sampleInterior( B3 )
isSourceSample( y3 )

Label U $\\Omega$
Label x0 $x_0$
Label x1 $x_1$
Label x2 $\\ldots$
Label x3 $x_k$
Label y0 $y_0$
Label y1 $y_1$

`,t={substance:e,style:[{contents:n,resolver:a}],domain:o,variation:"CoraletteStingray87535",excludeWarnings:[]};export{t as default};
