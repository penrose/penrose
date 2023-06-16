import{s as a,r as n,d as e}from"./walk-on-spheres.domain-ca6c8912.js";import"./index-5c0b39e3.js";const s=`-- Describe a short walk used to estimate the solution to
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


`,t={substance:s,style:[{contents:a,resolver:n}],domain:e,variation:"LilyDunlin3394"};export{t as default};
