import{m as n}from"./resolver-e7510c39.js";import"./iframe-bc7664f5.js";const e=`Bus BUS

Component MAR
Signal s_BUS_MAR := mkSignal(BUS, MAR)

Component RAM
Signal s_BUS_RAM := mkSignal(BUS, RAM)
Signal s_RAM_BUS := mkSignal(RAM, BUS)
Signal s_MAR_RAM := mkSignal(MAR, RAM)

Component IR
Signal s_BUS_IR := mkSignal(BUS, IR)
Signal s_IR_BUS := mkSignal(IR, BUS)

Component CS
Signal s_IR_CS := mkSignal(IR, CS)


Component PC
Signal s_BUS_PC := mkSignal(BUS, PC)
Signal s_PC_BUS := mkSignal(PC, BUS)


Component RA
Signal s_BUS_RA := mkSignal(BUS, RA)
Signal s_RA_BUS := mkSignal(RA, BUS)

Component RB
Signal s_BUS_RB := mkSignal(BUS, RB)


Component ALU
Signal s_RB_ALU := mkSignal(RB, ALU)
Signal s_RA_ALU := mkSignal(RA, ALU)
Signal s_ALU_BUS := mkSignal(ALU, BUS)


Component RF
Signal s_ALU_RF := mkSignal(ALU, RF)


Component RO
Signal s_BUS_RO := mkSignal(BUS, RO)


Component D
Signal s_RO_D := mkSignal(RO, D)


Component LCD
Signal s_BUS_LCD := mkSignal(BUS, LCD)


Component RSP
Signal s_BUS_RSP := mkSignal(BUS, RSP)
Signal s_RSP_BUS := mkSignal(RSP, BUS)

Component IN
Signal s_IN_BUS := mkSignal(IN, BUS)

Component C

Label BUS "Data Bus"
Label MAR "Memory Address Register"
Label RAM "Random Access Memory"
Label IR "Instruction Register"
Label CS "Control Signals"
Label PC "Program Counter"
Label RA "Register A"
Label RB "Register B"
Label ALU "Arithmetic Logic Unit"
Label RF "Flags Register"
Label RO "Output Register"
Label D "7-Segment Display"
Label LCD "LCD Display"
Label RSP "Stack Pointer Register"
Label IN "Input Module"
Label C "Clock Module"`,t=n("box-arrow-diagram"),o=`canvas {
    height = 1000
    width = 1000
}

layout = [dots, rects, arrow]

debug {
    -- this is the color used for the debugging dots and lines
    color = rgba(0, 0, 0, 0.0)
}


forall Component c {
    c.center = (? in [dots, rects], ? in [dots, rects])

    -- c.dot is for debugging purposes and can be removed
    c.dot = Circle{
        center: c.center
        r: 5
        fillColor: debug.color
    }

    c.icon = Rectangle {
        center: c.center
        fillColor: rgba(1, 1, 1, 1)
        strokeColor: rgba(0, 0, 0, 1)
        strokeWidth: 2
        width: c.text.width + 30
        height: c.text.height + 30
    }

    c.text = Text {
        string: c.label
        center: c.center
    }

    layer c.dot above c.icon
    layer c.dot above c.text
}

forall Component a; Component b; Signal s
where s := mkSignal(a, b) {

    s.start_center = a.center
    s.end_center = b.center

    -- debug_line can be removed
    debug_line = Line {
        start: s.start_center
        end: s.end_center
        strokeStyle: "dashed"
        strokeColor: debug.color
    }
    layer debug_line below a.dot
    layer debug_line below b.dot

    ensure norm(s.start_center - s.end_center) < 50 in dots
}

-- mkSignal is not symmetric and hence the repetition here
forall Component a; Component b; Component c; Signal s1; Signal s2
where s1 := mkSignal(a, b); s2 := mkSignal(b, c) {
    encourage norm(a.center - c.center) == 10000 in [dots, rects]
}
forall Component a; Component b; Component c; Signal s1; Signal s2
where s1 := mkSignal(b, a); s2 := mkSignal(b, c) {
    encourage norm(a.center - c.center) == 10000 in [dots, rects]
}
forall Component a; Component b; Component c; Signal s1; Signal s2
where s1 := mkSignal(a, b); s2 := mkSignal(c, b) {
    encourage norm(a.center - c.center) == 10000 in [dots, rects]
}
forall Component a; Component b; Component c; Signal s1; Signal s2
where s1 := mkSignal(b, a); s2 := mkSignal(c, b) {
    encourage norm(a.center - c.center) == 10000 in [dots, rects]
}

forall Signal s1; Signal s2 {
    ensure shapeDistanceLines(s1.start_center, s1.end_center, s2.start_center, s2.end_center) > 0 in [dots]
}

forall Bus b {
    override b.icon.height = 400
    override b.icon.width = b.text.width + 40
}


forall Component a; Component b {
    ensure disjoint(a.icon, b.icon, 50) in rects
}

forall Component a; Component b; Signal s
where s := mkSignal(a, b) {
    ensure shapeDistance(a.icon, b.icon) < 60 in rects

    s.start = (? in [arrow], ? in [arrow])
    s.end = (? in [arrow], ? in [arrow])
    s.icon = Line {
        start: s.start
        end: s.end
        endArrowhead: "straight"
        strokeColor: #000000ff
    }
    ensure signedDistance(a.icon, s.start) == 5 in arrow
    ensure signedDistance(b.icon, s.end) == 5 in arrow
    strength = 100
    encourage strength * norm(s.end - s.start) == 0 in arrow
    
    s.near_center_start = encourage .5 * strength * norm(s.start - a.icon.center) == 0 in arrow
    s.near_center_end = encourage .5 * strength * norm(s.end - b.icon.center) == 0 in arrow
}

-- for arrows with the Bus, it does not point towards the center of the bus
forall Component a; Bus b; Signal s
where s := mkSignal(a, b) {
    delete s.near_center_end
}
forall Bus a; Component b; Signal s
where s := mkSignal(a, b) {
    delete s.near_center_start
}


-- split the apparent double-headed arrows into two arrows.
forall Component a; Component b; Signal s1; Signal s2
where s1 := mkSignal(a, b); s2 := mkSignal(b, a) {

    start = (s1.start + s2.end) / 2
    end = (s1.end + s2.start) / 2

    t = unit(end - start)
    n = rot90(t)
    h = 4

    override s1.icon = Line {
        start: start + h * n
        end: end + h * n
        endArrowhead: "straight"
        strokeColor: #000000ff
    }

    override s2.icon = Line {
        start: end - h * n
        end: start - h * n
        endArrowhead: "straight"
        strokeColor: #000000ff
    }
}`,a=`type Component
type Bus <: Component
type Signal

constructor mkSignal(Component from, Component to) -> Signal`,i={substance:e,style:[{contents:o,resolver:t}],domain:a,variation:"ButtermelonPenguin8290",excludeWarnings:[]};export{i as default};
//# sourceMappingURL=computer-architecture.trio-a9ec21ff.js.map
