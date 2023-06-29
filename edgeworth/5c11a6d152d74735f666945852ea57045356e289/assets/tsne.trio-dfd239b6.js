import{a as n}from"./index-6d8b3b93.js";const o=`Point p00
Point p01
Point p02
Point p03
Point p04
Point p05
Point p06
Point p07
Point p08
Point p09
Point p10
Point p11
Point p12
Point p13
Point p14
Point p15
Point p16
Point p17
Point p18
Point p19`,t=n("tsne"),i=`canvas {
    width = 600
    height = 600
}

layout = [first, second]

global {
    -- Settings
    scalar minSize = 5
    scalar maxSize = 30
    scalar positionPerplexity = 5
    scalar colorPerplexity = 5

    -- Background
    shape background = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: rgba(0.9, 1, 0.9, 1)
    }
}

forall Point p {
    p.saturation = random(0, 1)
    p.angle = random(0, 6.3)
    p.size = (abs(cos(p.angle)), abs(sin(p.angle)))
    p.center = (?, ?)

    shape p.rect = Rectangle{
	    center: p.center
	    width: global.minSize + p.size[0] *(global.maxSize - global.minSize)
	    height: global.minSize + p.size[1] *(global.maxSize - global.minSize)
	    fillColor: hsva(360 * p.saturation, 100, 80, 1)
    }

    p.position = (global.positionPerplexity * p.size[0], global.positionPerplexity * p.size[1], global.colorPerplexity * p.saturation)
    p.rect above global.background
}

forall Point p1; Point p2 {
    ensure disjoint(p1.rect, p2.rect) in second
}

collect Point p into points {
    positions = listof position from points
    projections = listof center from points
    ensure tsneEnergy(positions, projections) < 0
}
`,e="type Point",p={substance:o,style:[{contents:i,resolver:t}],domain:e,variation:"BilbaoNewt5645"};export{p as default};
