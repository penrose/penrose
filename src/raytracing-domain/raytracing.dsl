-- Actual DSLL
tconstructor Scene : type
tconstructor PathVertex : type
tconstructor BounceType : type
tconstructor DiffuseBounce : type
tconstructor SpecularBounce : type
tconstructor GlossyBounce : type
tconstructor LightSource : type
tconstructor Camera : type
tconstructor Path : type
tconstructor PathSample : type -- a sampled path

BounceType <: PathVertex
Camera <: PathVertex
LightSource <: PathVertex
DiffuseBounce <: BounceType
SpecularBounce <: BounceType
GlossyBounce <: BounceType

operator Sample (T : Path) : PathSample

predicate HasForm (p : Path, s : String) : Prop
predicate In (p : Path, s : Scene) : Prop

-- value L : LightSource
-- value E : Camera
-- value D : DiffuseBounce
-- value S : SpecularBounce
-- value G : GlossyBounce
