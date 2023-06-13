import{s as n,r as e,d as a}from"./walk-on-spheres.domain-a301487f.js";import"./index-7ab1eacb.js";const o=`-- Describe a short walk used to estimate the solution to
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


`,t={substance:o,style:[{contents:n,resolver:e}],domain:a,variation:"CoraletteStingray87534"};export{t as default};
