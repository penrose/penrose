import{m as n}from"./resolver-6b1c30e6.js";import{d as t}from"./all-shapes.domain-8745a2d9.js";import"./iframe-fa2344d7.js";const e=`-- https://tikz.dev/tikz-arrows
Line none, concave, straight, line, doubleLine, loopup, loopdown, loop
AutoLabel All`,o=n("shape-spec"),r=`canvas {
  width = 400
  height = 600
}
const {
  ch2 = canvas.height/2
  cw2 = canvas.width/2
  bg = Rectangle {
    center: (0, 0)
    width: canvas.width
    height: canvas.height
    fillColor: #fff
  }
  vguide1 = Line {
    start: (0, ch2)
    end: (0, -ch2)
    strokeColor: #f00
    strokeWidth: 1
  }
  vguide2 = Line {
    start: (-cw2 + 10, ch2)
    end: (-cw2 + 10, -ch2)
    strokeColor: #f00
    strokeWidth: 1
  }

}

forall Line l {
  rowPadding = 70
  rowY = -match_id * rowPadding + canvas.height/2
  l.icon = Line {
    start: (-const.cw2 + 10, rowY)
    end: (0, rowY)
    startArrowhead: l.label
    endArrowhead: l.label
    strokeColor: #000
    strokeWidth: 3.0
  }
  l.text = Equation {
    string: "\\textbf{" + l.label + "}"
    center: (?, rowY)
    fillColor: #000
  }
  ensure equal(l.text.center[0], l.text.height/2 + rowPadding)
}

`,s={substance:e,style:[{contents:r,resolver:o}],domain:t,variation:""};export{s as default};
//# sourceMappingURL=arrowheads.trio-bdbfc6c9.js.map
