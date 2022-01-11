type Scalar

type kVector
type Vector <: kVector
type Bivector <: kVector

function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector
