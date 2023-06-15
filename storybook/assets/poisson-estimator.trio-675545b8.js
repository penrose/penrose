import{s as n,r as e,d as a}from"./walk-on-spheres.domain-2d6c3d5d.js";import"./resolver-8e6b3de6.js";import"./iframe-916e8a48.js";const o=`-- Describe a short walk used to estimate the solution to
-- a Poisson equation Δu = f, which involves both boundary
-- samples x and source samples y.

Domain U
Step x0, x1, x2, x3
Sample y0, y1, y2, y3

x1 := sampleBoundary(x0)
x2 := sampleBoundary(x1)
x3 := sampleBoundary(x2)
y0 := sampleSource(x0)
y1 := sampleSource(x1)
y2 := sampleSource(x2)
y3 := sampleSource(x3)

AutoLabel All
Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "…"
Label x3 "xₖ"
Label y0 "y₀"
Label y1 "y₁"
NoLabel y2
NoLabel y3


`,r={substance:o,style:[{contents:n,resolver:e}],domain:a,variation:"CoraletteStingray87535"};export{r as default};
//# sourceMappingURL=poisson-estimator.trio-675545b8.js.map
