const setDomain = `
type Set

predicate Not(Prop p1)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`;

const setStyle = `
canvas {
  width = 400
  height = 400
}

forall Set x {
  shape x.icon = Circle { }
  shape x.text = Equation {
    string : x.label
    fontSize : "32px"
  }
  ensure contains(x.icon, x.text)
  encourage norm(x.text.center - x.icon.center) == 0
  layer x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  layer x.icon above y.icon
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
  ensure disjoint(x.icon, y.icon)
}

forall Set x; Set y
where Intersecting(x, y) {
  ensure overlapping(x.icon, y.icon)
  ensure disjoint(y.text, x.icon)
  ensure disjoint(x.text, y.icon)
}
`;

const set1substance = `
Set A, B, C, D

IsSubset(B, A)
IsSubset(C, A)
Not(Intersecting(B, C))
Not(Intersecting(D, A))

AutoLabel All
`;

const set2substance = `
Set A, B, C, D

IsSubset(B, A)
IsSubset(C, A)

Not(Intersecting(B, C))
IsSubset(A, D)

AutoLabel All`;

export const trio1 = {
  substance: set1substance,
  domain: setDomain,
  style: setStyle,
  variation: "set1",
};
export const trio2 = {
  substance: set2substance,
  domain: setDomain,
  style: setStyle,
  variation: "set1",
};
