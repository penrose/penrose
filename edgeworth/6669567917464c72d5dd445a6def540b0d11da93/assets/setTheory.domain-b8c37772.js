const t=`Set A, B, C, D, E, F, G

IsSubset(B, A)
IsSubset(C, A)
IsSubset(D, B)
IsSubset(E, B)
IsSubset(F, C)
IsSubset(G, C)

Not(Intersecting(E, D))
Not(Intersecting(F, G))
Not(Intersecting(B, C))

AutoLabel All
`,e=`type Set

predicate Not(Prop p1)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`;export{e as d,t as s};
