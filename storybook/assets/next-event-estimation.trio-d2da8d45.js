import{s as n,r,d as t}from"./ray-tracing.domain-47819d65.js";import"./resolver-9b16ebb5.js";import"./iframe-7b8a5a1d.js";const e=`-- variation: MauvewoodTapir6620

Scene S
Light L
Camera C

Point x0
onCamera(x0,C)
Ray r0 := rayFrom(x0)
Point x1 := intersect(r0,S)
Ray r1 := rayFrom(x1)
Point x2 := intersect(r1,S)
Ray r2 := rayFrom(x2)
Point x3 := intersect(r2,S)
Ray r3 := rayFrom(x3)
hitsLight(r3,L)

Label x1 $x_1$
Label x2 $x_2$
Label x3 $x_3$

-- direct connection to light at each bounce
Ray rL1 := rayBetween(x1,L)
Ray rL2 := rayBetween(x2,L)
`,s={substance:e,style:[{contents:n,resolver:r}],domain:t,variation:"TransfusionElephant49974",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=next-event-estimation.trio-d2da8d45.js.map
