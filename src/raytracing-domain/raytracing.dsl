-- Actual DSLL
tconstructor Scene : type
tconstructor PathVertex : type
tconstructor PathEdge : type
tconstructor PathSample : type -- a sampled path
tconstructor SceneGeometry : type
tconstructor Camera : type
tconstructor Path : type
tconstructor LightSource : type
tconstructor SpecularObject : type
tconstructor DiffuseObject : type


-- BounceType <: PathVertex
Camera <: SceneGeometry
LightSource <: SceneGeometry
DiffuseObject <: SceneGeometry
SpecularObject <: SceneGeometry
-- GlossyBounce <: SceneGeometry

operator Sample (T : Path) : PathSample

predicate HasForm (p : Path, s : String) : Prop
predicate In (p : Path, s : Scene) : Prop
predicate InVE (v : PathVertex, e : PathEdge) : Prop
predicate InVP (v : PathVertex, p : PathSample) : Prop
predicate InOS (p : SceneGeometry, s : Scene) : Prop
predicate OnLight(v : PathVertex) : Prop
predicate OnEye(v : PathVertex) : Prop
predicate IsDiffuse(v : PathVertex) : Prop
predicate IsSpecular(v : PathVertex) : Prop
predicate SceneSatisfies(s : Scene, p : Path) : Prop
predicate Hits(v : PathVertex, o : SceneGeometry) : Prop

-- value L : LightSource
-- value E : Camera
-- value D : DiffuseBounce
-- value S : SpecularBounce
-- value G : GlossyBounce
