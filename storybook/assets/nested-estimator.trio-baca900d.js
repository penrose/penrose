import{s as n,r as e,d as a}from"./walk-on-spheres.domain-9cc578b4.js";import"./resolver-e7510c39.js";import"./iframe-bc7664f5.js";const l=`-- Describe a nested walk used to estimate the solution to,
-- e.g., a biharmonic equation Δ²u = 0, which starts new
-- walks at interior points y sampled in the ball around
-- each point x from the primary walk.

Domain U

-- main walk
Point x0
Ball B0 := ballAround( x0 )
Point x1 := sampleBoundary( B0 )
Ball B1 := ballAround( x1 )
Point x2 := sampleBoundary( B1 )
Ball B2 := ballAround( x2 )
Point x3 := sampleBoundary( B2 )
Ball B3 := ballAround( x3 )

-- nested walk from step 0 of main walk
NestedPoint y00 := startWalkFrom( B0 )
Ball B00 := nestedBallAround( y00 )
NestedPoint y01 := sampleBoundaryNested( B00 )
Ball B01 := nestedBallAround( y01 )
NestedPoint y02 := sampleBoundaryNested( B01 )
Ball B02 := nestedBallAround( y02 )

-- nested walk from step 1 of main walk
NestedPoint y10 := startWalkFrom( B1 )
Ball B10 := nestedBallAround( y10 )
NestedPoint y11 := sampleBoundaryNested( B10 )
Ball B11 := nestedBallAround( y11 )
NestedPoint y12 := sampleBoundaryNested( B11 )
Ball B12 := nestedBallAround( y12 )

Label U $\\Omega$
Label x0 $x_0$
Label x1 $x_1$
Label x2 $\\ldots$
Label x3 $x_k$

`,r={substance:l,style:[{contents:n,resolver:e}],domain:a,variation:"CosmosGull147",excludeWarnings:[]};export{r as default};
//# sourceMappingURL=nested-estimator.trio-baca900d.js.map
