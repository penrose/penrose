import{s as n,r as t,d as e}from"./ray-tracing.domain-f8246fa0.js";import"./resolver-8a310668.js";import"./iframe-92384b25.js";const r=`-- variation: SunflowerDolphin187

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
//# sourceMappingURL=bidirectional.trio-6b5db103.js.map
