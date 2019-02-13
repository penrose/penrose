-- Actual DSLL
tconstructor Diagram : type
tconstructor VertexType : type
tconstructor BounceType : type
tconstructor DiffuseBounce : type
tconstructor SpecularBounce : type
tconstructor GlossyBounce : type
tconstructor LightSource : type
tconstructor Camera : type
tconstructor Path : type -- a sampled path

BounceType <: VertexType
Camera <: VertexType
LightSource <: VertexType
DiffuseBounce <: BounceType
SpecularBounce <: BounceType
GlossyBounce <: BounceType

tsynonym PathType := List(VertexType)

operator sample (T : PathType) : Path

value diagram : Diagram
value L : LightSource
value E : Camera
value D : DiffuseBounce
value S : SpecularBounce
value G : GlossyBounce
