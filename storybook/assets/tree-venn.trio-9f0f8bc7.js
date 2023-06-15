import{s as n,d as e}from"./setTheory.domain-b8c37772.js";import{m as t}from"./resolver-4b330e38.js";import"./iframe-893c3c1f.js";const o=t("set-theory-domain"),i=`canvas {
  width = 800
  height = 700
}

forall Set x {
  x.icon = Circle {
    strokeWidth : 0
  }

  x.text = Equation {
    string : x.label
    fontSize : "32px"
  }

  ensure contains(x.icon, x.text)
  encourage sameCenter(x.text, x.icon)
  x.textLayering = x.text above x.icon
}

forall Set x; Set y
where IsSubset(x, y) {
  ensure smallerThan(x.icon, y.icon)
  ensure disjoint(y.text, x.icon, 10)
  ensure contains(y.icon, x.icon, 5)
  x.icon above y.icon
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
`,a={substance:n,style:[{contents:i,resolver:o}],domain:e,variation:"PlumvilleCapybara104"};export{a as default};
//# sourceMappingURL=tree-venn.trio-9f0f8bc7.js.map