import{m as n}from"./resolver-978398b2.js";import"./iframe-1692de9e.js";const e=`Object o 
AutoLabel All`,t=n("animation"),r=`canvas {
    width = 400
    height = 400
}

layout = [centerX, centerY, shrink]

forall Object o {
    x = ? 
    y = ?
    r = ?
    o.shape = Circle {
      center: (x, y)
      r: r
    }
    o.text = Equation {
        string: o.label
    }
    ensure contains(o.shape, o.text)
    ensure equal(x, 0)
    ensure equal(y, 0) except centerX
    ensure equal(r, 30) in shrink
}`,o="type Object",c={substance:e,style:[{contents:r,resolver:t}],domain:o,variation:"",excludeWarnings:[]};export{c as default};
//# sourceMappingURL=center-shrink-circle.trio-59b7050e.js.map
