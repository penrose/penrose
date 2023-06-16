import{s as a,r as n,d as e}from"./walk-on-spheres.domain-5518b314.js";import"./resolver-8abb9d85.js";import"./iframe-05d022d4.js";const s=`-- Describe a short walk used to estimate the solution to
-- a basic Laplace equation Δu = 0, which involves both
-- boundary samples x and source samples y.

Domain U
Step x0, x1, x2, x3, x4

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )
x4 := sampleBoundary( x3 )

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "x₂"
Label x3 "…"
Label x4 "xₖ"


`,x={substance:s,style:[{contents:a,resolver:n}],domain:e,variation:"LilyDunlin3394"};export{x as default};
//# sourceMappingURL=laplace-estimator.trio-ca345fb6.js.map
