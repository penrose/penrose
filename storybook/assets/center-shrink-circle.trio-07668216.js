import{m as n}from"./resolver-1df37e9f.js";import"./iframe-71d88643.js";const e=`Object o 
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
//# sourceMappingURL=center-shrink-circle.trio-07668216.js.map
