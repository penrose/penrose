-- Actual DSLL
type Scene
type PathVertex
type PathEdge
type PathSample -- a sampled path
type SceneGeometry
type Camera
type Path
type LightSource
type SpecularObject
type DiffuseObject


-- BounceType <: PathVertex
Camera <: SceneGeometry
LightSource <: SceneGeometry
DiffuseObject <: SceneGeometry
SpecularObject <: SceneGeometry
-- GlossyBounce <: SceneGeometry

function Sample : Path p -> PathSample

predicate HasForm : Path p * String s 
predicate In  :  Path p * Scene s 
predicate InVP  :  PathVertex v * Path p 
predicate InOS  :  SceneGeometry p * Scene s 
predicate OnLight :  PathVertex v
predicate OnEye :  PathVertex v
predicate IsDiffuse :  PathVertex v
predicate IsSpecular :  PathVertex v
predicate SceneSatisfies :  Scene s * Path p
predicate Hits :  PathVertex v * SceneGeometry o

constructor CreateEdge : PathVertex v0 * PathVertex v1 -> PathEdge

-- value L : LightSource
-- value E : Camera
-- value D : DiffuseBounce
-- value S : SpecularBounce
-- value G : GlossyBounce
