import{s as n,r as e,d as t}from"./ray-tracing.domain-446e5386.js";import"./index-0672ecd7.js";const r=`-- variation: SunflowerDolphin187

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

`,i={substance:r,style:[{contents:n,resolver:e}],domain:t,variation:"SunflowerDolphin187"};export{i as default};
