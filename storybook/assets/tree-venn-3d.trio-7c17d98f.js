import{s as e}from"./tree.substance-d6cc2fdb.js";import{m as n}from"./resolver-11ab920e.js";import{d as s}from"./setTheory.domain-681bfd26.js";import"./iframe-f5449c82.js";const t=n("set-theory-domain"),a=`canvas {
  width = 800
  height = 800
}

forall Set x {
    x.shape = Circle {
        strokeWidth : 0.
    }

    x.shading = Image {
        center : x.shape.center 
        width : x.shape.r * 2.0
        height : x.shape.r * 2.0
        href : "shading.svg"
    }

    x.shadow = Image {
        href : "set-theory-domain-shadow.svg"
        width : x.shape.r * 2.15
        height : x.shape.r * 2.22
        center : (x.shape.center[0] + 0.03 * x.shading.width, x.shape.center[1] - 0.051 * x.shading.height)
    }

    x.text = Equation {
        string : x.label
        fillColor: rgba(1.0, 1.0, 1.0, 1.0)
        width: 0.4 * x.shape.r
        height: 0.4 * x.shape.r
    }

    ensure contains(x.shape, x.text)
    ensure lessThan(20, x.shape.r)
    encourage sameCenter(x.text, x.shape)

    x.shape below x.text
    x.shading below x.shape
    x.shadow below x.shading
}

forall Set x; Set y
where IsSubset(x, y) {
    ensure disjoint(y.text, x.shape, 5.0)
    ensure contains(y.shape, x.shape, 5.0)
    x.shape above y.shape
    y.text below x.shape
    x.shadow above y.shape
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.shape, y.shape)
}

forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.shape, y.shape)
    ensure disjoint(y.text, x.shape)
    ensure disjoint(x.text, y.shape)
}
`,i={substance:e,style:[{contents:a,resolver:t}],domain:s,variation:"",excludeWarnings:[]};export{i as default};
//# sourceMappingURL=tree-venn-3d.trio-7c17d98f.js.map
