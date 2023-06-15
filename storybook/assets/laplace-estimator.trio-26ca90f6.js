import{s as a,r as n,d as e}from"./walk-on-spheres.domain-efa0e91e.js";import"./resolver-4b330e38.js";import"./iframe-893c3c1f.js";const s=`-- Describe a short walk used to estimate the solution to
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
//# sourceMappingURL=laplace-estimator.trio-26ca90f6.js.map