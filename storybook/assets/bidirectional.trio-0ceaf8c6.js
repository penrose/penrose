import{s as n,r as t,d as e}from"./ray-tracing.domain-326e487e.js";import"./resolver-691c4a84.js";import"./iframe-f4259546.js";const r=`-- variation: SunflowerDolphin187

Scene S
Light L
Camera C

-- eye subpath
Point x0
onCamera(x0,C)
Ray r0 := rayFrom(x0)
Point x1 := intersect(r0,S)
Ray r1 := rayFrom(x1)
Point x2 := intersect(r1,S)
Label x1 $x_1$
Label x2 $x_2$

-- light subpath
Point y0
onLight(y0,L)
Ray s0 := rayFrom(y0)
Point y1 := intersect(s0,S)
Ray s1 := rayFrom(y1)
Point y2 := intersect(s1,S)
Label y1 $y_1$
Label y2 $y_2$

-- connection between subpaths
Ray rxy := rayBetween(x2,y2)

`,s={substance:r,style:[{contents:n,resolver:t}],domain:e,variation:"SunflowerDolphin187"};export{s as default};
//# sourceMappingURL=bidirectional.trio-0ceaf8c6.js.map
