import{s as e,r as n,d as i}from"./pseudograph.domain-e9f59f96.js";import"./index-b40072f4.js";const g=`Vertex Chicago, Denver, Detroit, LA, NYC, SF, Washington

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

Link e10 := Edge(Chicago, Chicago)
Link e11 := Edge(Denver, Denver)
Link e12 := Edge(Detroit, Detroit)
Link e13 := Edge(LA, LA)
Link e14 := Edge(NYC, NYC)
Link e15 := Edge(SF, SF)
Link e16 := Edge(Washington, Washington)

AutoLabel Chicago, Denver, Detroit, Washington
Label LA "Los Angeles"
Label NYC "New York"
Label SF "San Francisco"
`,a={substance:g,style:[{contents:e,resolver:n}],domain:i,variation:""};export{a as default};
