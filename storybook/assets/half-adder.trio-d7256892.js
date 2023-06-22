import{m as n}from"./resolver-5520b3c7.js";import"./iframe-e9261e69.js";const e=`AutoLabel All

InputNode IN1, IN2
OutputNode SUM, CAR

Node XORIN1, XORIN2, XOROUT
XORGate XOR := MakeXORGate(XORIN1, XORIN2, XOROUT)

Node ANDIN1, ANDIN2, ANDOUT 
ANDGate AND := MakeANDGate(ANDIN1, ANDIN2, ANDOUT)

Node S1IN, S1OUT1, S1OUT2
SplitComponent S1 := MakeSplitComponent(S1IN, S1OUT1, S1OUT2)

Node S2IN, S2OUT1, S2OUT2
SplitComponent S2 := MakeSplitComponent(S2IN, S2OUT1, S2OUT2)

Connection C1 := MakeConnection(IN1, S1IN)
Connection C2 := MakeConnection(IN2, S2IN)
Connection C3 := MakeConnection(S1OUT1, XORIN1)
Connection C4 := MakeConnection(S2OUT1, XORIN2)
Connection C5 := MakeConnection(S1OUT2, ANDIN1)
Connection C6 := MakeConnection(S2OUT2, ANDIN2)
Connection C7 := MakeConnection(XOROUT, SUM)
Connection C8 := MakeConnection(ANDOUT, CAR)`,t=n("logic-circuit-domain"),o=`canvas {
    width = 800
    height = 700
}

background {
    shape icon = Rectangle {
        center: (0, 0)
        width: canvas.width
        height: canvas.height
        fillColor: rgba(1.0, 1.0, 1.0, 1.0)
    }
}

-- Nodes
forall Node x {
    vec2 x.center = (?, ?)
}

forall InputNode x {
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 1.0
        r : 6.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }

    shape x.symbol = Equation {
       string : x.label
       rotation : 0.0
       center : (x.icon.center[0] - 30.0, x.icon.center[1])
    }

    x.icon above background.icon
    x.symbol above background.icon
}

forall OutputNode x {
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 1.0
        r : 6.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape x.symbol = Equation {
       string : x.label
       rotation : 0.0
       center : (x.icon.center[0] + 35.0, x.icon.center[1])
    }

    x.icon above background.icon
    x.symbol above background.icon
}

forall InputNode A; InputNode B {
    ensure equal(A.center[0], B.center[0])
}

forall OutputNode A; OutputNode B {
    ensure equal(A.center[0], B.center[0])
}

-- Gates

forall XORGate G
where G := MakeXORGate(IN1, IN2, OUT)
with Node IN1; Node IN2; Node OUT {
    vec2 G.center = (?, ?)
    scalar G.width = 100.0
    scalar G.height = 60.0

    shape G.part1 = Rectangle {
        center : G.center
        width : 40.0
        height : 50.0
        strokeWidth: 1.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part2 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 25.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part3 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 24.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
    }

    scalar G.GG = 50.0
    shape G.part4 = Circle {
        center : (G.part1.center[0] - G.GG + 3.0, G.part1.center[1])
        r : sqrt(25.0 * 25.0 + (G.GG - 20.0) * (G.GG  - 20.0))
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part5 = Rectangle {
        center : (G.part1.center[0] - G.GG - 30.0+ 3.0, G.part1.center[1])
        width : 2 * (G.GG - 20.0) + 60.0
        height : 3 * (G.GG - 20.0)
        strokeWidth: 0.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part5 above G.part4

    shape G.part6 = Circle {
        center : (G.part1.center[0] - G.GG  - 6.0, G.part1.center[1])
        r : sqrt(25.0 * 25.0 + (G.GG - 20.0) * (G.GG  - 20.0))
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part7 = Rectangle {
        center : (G.part1.center[0] - G.GG - 30.0 - 6.0, G.part1.center[1])
        width : 2 * (G.GG - 20.0) + 60.0
        height : 3 * (G.GG - 20.0)
        strokeWidth: 0.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part4 above G.part3
    G.part6 above G.part5
    G.part7 above G.part6

    G.part1 above G.part2
    G.part3 above G.part1

    G.part1 above background.icon
    G.part2 above background.icon
    G.part3 above background.icon
    G.part4 above background.icon
    G.part5 above background.icon
    G.part6 above background.icon
    G.part7 above background.icon

    ensure equal(vdist(IN1.center, (G.part1.center[0] - 20.0, G.part1.center[1] + 17.0)), 0.0)
    ensure equal(vdist(IN2.center, (G.part1.center[0] - 20.0, G.part1.center[1] - 17.0)), 0.0)
    ensure equal(vdist(OUT.center, (G.part1.center[0] + 45.0, G.part1.center[1])), 0.0)
}

forall ANDGate G
where G := MakeANDGate(IN1, IN2, OUT)
with Node IN1; Node IN2; Node OUT {
    vec2 G.center = (?, ?)
    scalar G.width = 100.0
    scalar G.height = 60.0

    shape G.part1 = Rectangle {
        center : G.center
        width : 40.0
        height : 50.0
        strokeWidth: 1.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part2 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 25.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape G.part3 = Circle {
        center : (G.part1.center[0] + 20.0, G.part1.center[1])
        r : 24.0
        fillColor : rgba(1.0, 1.0, 1.0, 1.0)
        strokeWidth : 1.0
        strokeColor : rgba(1.0, 1.0, 1.0, 1.0)
    }
    G.part1 above G.part2
    G.part3 above G.part1

    G.part1 above background.icon
    G.part2 above background.icon
    G.part3 above background.icon

    ensure equal(vdist(IN1.center, (G.part1.center[0] - 20.0, G.part1.center[1] + 17.0)), 0.0)
    ensure equal(vdist(IN2.center, (G.part1.center[0] - 20.0, G.part1.center[1] - 17.0)), 0.0)
    ensure equal(vdist(OUT.center, (G.part1.center[0] + 45.0, G.part1.center[1])), 0.0)
}

forall SplitComponent x
where x := MakeSplitComponent(IN, OUT1, OUT2)
with Node IN; Node OUT1; Node OUT2 {
    vec2 x.center = IN.center
    shape x.icon = Circle {
        center: x.center
        strokeWidth : 0.0
        r : 6.0
        fillColor : rgba(0.0, 0.0, 0.0, 1.0)
    }
    shape x.line = Line {
        start : OUT1.center
        end : OUT2.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    x.icon above x.line

    x.icon above background.icon
    x.line above background.icon

    ensure equal(IN.center[0], OUT1.center[0])
    ensure equal(IN.center[0], OUT2.center[0])
    ensure lessThan(0.0, -(OUT1.center[1] - IN.center[1]) * (-IN.center[1] + OUT2.center[1]))
}

forall Gate G; Gate H {
    ensure lessThan(0.6 * (G.height + H.height), abs(G.part1.center[1] - H.part1.center[1]))
    ensure lessThan(0.6 * (G.width + H.width), abs(G.part1.center[0] - H.part1.center[0]))
}

forall Connection x; InputNode y {
    x.line1 below y.icon
    x.line2 below y.icon
    x.line3 below y.icon
}

forall SplitComponent x; XORGate y {
    x.line above y.part7
}

forall Connection x; XORGate y {
    x.line1 above y.part7
    x.line2 above y.part7
    x.line3 above y.part7
}

-- Connections

forall Connection C
where C := MakeConnection(A, B)
with Node A; Node B {
    scalar C.pivot = ?
    shape C.line1 = Line {
        start : A.center
        end : (C.pivot, A.center[1])
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    shape C.line2 = Line {
        start : (C.pivot, A.center[1])
        end : (C.pivot, B.center[1])
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }
    shape C.line3 = Line {
        start : (C.pivot, B.center[1])
        end : B.center
        strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
        strokeWidth: 1.0
    }

    C.line1 above background.icon
    C.line2 above background.icon
    C.line3 above background.icon

    encourage equal(A.center[1], B.center[1])
    ensure lessThan(A.center[0], B.center[0])
    ensure lessThan(A.center[0], C.pivot)
    ensure lessThan(C.pivot, B.center[0])
}

forall SplitComponent x; SplitComponent y {
    ensure lessThan(10.0, abs(x.center[0] - y.center[0]))
}`,r=`-- Nodes

type Node

type InputNode <: Node
type OutputNode <: Node

-- Gates

type Component

type SplitComponent <: Component
type Gate <: Component

type OneInputGate <: Gate
type TwoInputGate <: Gate

constructor MakeSplitComponent(Node IN, Node OUT1, Node OUT2) -> SplitComponent

type Buffer <: OneInputGate
type NOTGate <: OneInputGate

constructor MakeBuffer(Node IN, Node OUT) -> Buffer
constructor MakeNOTGate(Node IN, Node OUT) -> NOTGate

type ORGate <: TwoInputGate
type NORGate <: TwoInputGate
type ANDGate <: TwoInputGate
type NANDGate <: TwoInputGate
type XORGate <: TwoInputGate
type XNORGate <: TwoInputGate

constructor MakeORGate(Node IN1, Node IN2, Node OUT) -> ORGate
constructor MakeNORGate(Node IN1, Node IN2, Node OUT) -> NORGate
constructor MakeANDGate(Node IN1, Node IN2, Node OUT) -> ANDGate
constructor MakeNANDGate(Node IN1, Node IN2, Node OUT) -> NANDGate
constructor MakeXORGate(Node IN1, Node IN2, Node OUT) -> XORGate
constructor MakeXNORGate(Node IN1, Node IN2, Node OUT) -> XNORGate

-- Connections

type Connection

constructor MakeConnection(Node A, Node B) -> Connection`,i={substance:e,style:[{contents:o,resolver:t}],domain:r,variation:"CauliflowerDeer97138"};export{i as default};
//# sourceMappingURL=half-adder.trio-d7256892.js.map
