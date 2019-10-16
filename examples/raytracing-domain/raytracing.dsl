type PathVertex
type PathEdge
type Path
type SceneGeometry
type Camera
type PathType
type LightSource
type SpecularObject
type DiffuseObject


-- BounceType <: PathVertex
Camera <: SceneGeometry
LightSource <: SceneGeometry
DiffuseObject <: SceneGeometry
SpecularObject <: SceneGeometry
-- GlossyBounce <: SceneGeometry

function Sample : PathType p -> Path

predicate HasForm : PathType p * String s 
predicate In  :  PathType p * Scene s 
predicate InVP  :  PathVertex v * PathType p 
predicate OnLight :  PathVertex v
predicate OnEye :  PathVertex v
predicate IsDiffuse :  PathVertex v
predicate IsSpecular :  PathVertex v
predicate Hits :  PathVertex v * SceneGeometry o

constructor CreateEdge : PathVertex v0 * PathVertex v1 -> PathEdge

-- value L : LightSource
-- value E : Camera
-- value D : DiffuseBounce
-- value S : SpecularBounce
-- value G : GlossyBounce
