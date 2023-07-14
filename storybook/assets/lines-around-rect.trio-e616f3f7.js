import{m as n}from"./resolver-84c3bc8f.js";import{d as e}from"./shapes.domain-43822a77.js";import"./iframe-38af56f5.js";const l=`Text g
Line l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15
`,t=n("shape-distance"),o=`canvas {
  width = 800
  height = 700
}

forall Text \`g\` {
  \`g\`.shape = Equation {
    string : "e^{i \\pi} = -1"
    fontSize : "50px"
  }
  \`g\`.box = Rectangle {
    center: \`g\`.shape.center
    width : \`g\`.shape.width
    height : \`g\`.shape.height
    fillColor: none()
    strokeWidth: 1
    strokeColor: #000
  }
}

forall Line l {
  l.shape = Line {
    strokeColor: #000
  }

  ensure touching(\`g\`.shape, l.shape)
}
`,a={substance:l,style:[{contents:o,resolver:t}],domain:e,variation:"",excludeWarnings:[]};export{a as default};
//# sourceMappingURL=lines-around-rect.trio-e616f3f7.js.map
