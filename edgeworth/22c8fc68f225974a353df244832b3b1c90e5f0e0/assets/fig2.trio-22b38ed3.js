import{s as e,r as n,d as i}from"./pseudograph.domain-03bf3e8d.js";import"./index-3083d8fe.js";const o=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

Link e1m1 := Edge(Chicago, Denver)
Link e1m2 := Edge(Chicago, Denver)
Link e1m3 := Edge(Chicago, Denver)
Link e2 := Edge(Chicago, Detroit)
Link e3m1 := Edge(Chicago, NYC)
Link e3m2 := Edge(Chicago, NYC)
Link e4 := Edge(Chicago, Washington)
Link e5m1 := Edge(Denver, LA)
Link e5m2 := Edge(Denver, LA)
Link e6 := Edge(Denver, SF)
Link e7 := Edge(Detroit, NYC)
Link e8 := Edge(LA, SF)
Link e9m1 := Edge(NYC, Washington)
Link e9m2 := Edge(NYC, Washington)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,t={substance:o,style:[{contents:e,resolver:n}],domain:i,variation:"",excludeWarnings:[]};export{t as default};
