import{s as n,r as e,d as t}from"./walk-on-spheres.domain-51e6b24b.js";import"./resolver-0be50027.js";import"./iframe-dde75e39.js";const a=`-- Describe a short walk used to estimate the solution to
-- a basic Laplace equation Δu = 0, using off-centered
-- rather than on-centered steps.

Domain U

Step x0, x1, x2, x3, x4

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )
x4 := sampleBoundary( x3 )

offCenter( x0 )
offCenter( x1 )
offCenter( x2 )
offCenter( x3 )
offCenter( x4 )

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "x₂"
Label x3 "…"
Label x4 "xₖ"


`,x={substance:a,style:[{contents:n,resolver:e}],domain:t,variation:"PlumburnPartridge8506"};export{x as default};
//# sourceMappingURL=offcenter-estimator.trio-bce9bc28.js.map
