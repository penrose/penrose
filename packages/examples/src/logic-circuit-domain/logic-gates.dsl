-- Nodes

type Node

type InputNode <: Node
type OutputNode <: Node

-- Gates

type Component

type SplitComponent <: Component
type Gate <: Component

type OneInputGate <: Gate
type TwoInputGate <: Gate

constructor MakeSplitComponent : Node IN * Node OUT1 * Node OUT2 -> SplitComponent

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

constructor MakeConnection(Node A, Node B) -> Connection