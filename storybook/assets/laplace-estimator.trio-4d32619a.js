import{s as n,r as a,d as l}from"./walk-on-spheres.domain-739d4913.js";import"./resolver-1df37e9f.js";import"./iframe-71d88643.js";const o=`-- A short walk used to estimate the solution to
-- a Laplace equation Δu = 0 with pure Dirichlet
-- boundary conditions, via WoS.

Domain U

Point x0
Ball B0 := ballAround( x0 )
Point x1 := sampleBoundary( B0 )
Ball B1 := ballAround( x1 )
Point x2 := sampleBoundary( B1 )
Ball B2 := ballAround( x2 )
Point x3 := sampleBoundary( B2 )
Ball B3 := ballAround( x3 )
Point x4 := sampleBoundary( B3 )
Ball B4 := ballAround( x4 )

Label U  $\\Omega$
Label x0 $x_0$
Label x1 $x_1$
Label x2 $x_2$
Label x3 $\\ldots$
Label x4 $x_k$

`,s={substance:o,style:[{contents:n,resolver:a}],domain:l,variation:"LilyDunlin3394",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=laplace-estimator.trio-4d32619a.js.map
