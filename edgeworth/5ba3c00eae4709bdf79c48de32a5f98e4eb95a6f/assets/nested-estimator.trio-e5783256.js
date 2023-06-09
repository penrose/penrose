import{s as n,r as e,d as a}from"./walk-on-spheres.domain-b300c149.js";import"./index-0914b29d.js";const t=`-- Describe a nested walk used to estimate the solution to,
-- e.g., a biharmonic equation Δ²u = 0, which starts new
-- walks at interior points y sampled in the ball around
-- each point x from the primary walk.

Domain U

Step x0, x1, x2, x3
Step y00, y01, y02
Step y10, y11, y12

x1 := sampleBoundary( x0 )
x2 := sampleBoundary( x1 )
x3 := sampleBoundary( x2 )

y00 := sampleInterior( x0 )
y01 := sampleBoundary( y00 )
y02 := sampleBoundary( y01 )
nested( y00 )
nested( y01 )
nested( y02 )

y10 := sampleInterior( x1 )
y11 := sampleBoundary( y10 )
y12 := sampleBoundary( y11 )
nested( y10 )
nested( y11 )
nested( y12 )

Label U "Ω"
Label x0 "x₀"
Label x1 "x₁"
Label x2 "…"
Label x3 "xₖ"
Label y00 "y₀"
Label y10 "y₁"

`,o={substance:t,style:[{contents:n,resolver:e}],domain:a,variation:"ElmCrane34650"};export{o as default};
