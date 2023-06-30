import{s as n,r as e}from"./euclidean.style-2ca9287e.js";import{d as a}from"./geometry.domain-952a3e66.js";import"./resolver-0c99df2e.js";import"./iframe-b95d2f91.js";const t=`-- TODO simplify after attributes are implemented
-- name in registry: congruent-triangles
Point U, S, T,  X, Y, Z

Let UTS := Triangle(U, T, S)
Let US := Segment(U, S)
Let UT := Segment(U, T)
Let ST := Segment(S, T)
Angle aTUS := InteriorAngle(T, U, S)
Angle aUST := InteriorAngle(U, S, T)
Angle aUTS := InteriorAngle(U, T, S)

Let XYZ := Triangle(X, Y, Z)
Let XZ := Segment(X, Z)
Let XY := Segment(X, Y)
Let ZY := Segment(Z, Y)
Angle aXYZ := InteriorAngle(X, Y, Z)
Angle aYZX := InteriorAngle(Y, Z, X)
Angle aZXY := InteriorAngle(Z, X, Y)

EqualLengthMarker(ST, XY)
EqualLength(ST, XY)

EqualLengthMarker(XZ, US)
EqualLength(XZ, US)

EqualLengthMarker(ZY, UT)
EqualLength(ZY, UT)

EqualAngleMarker(aTUS, aXYZ)
EqualAngleMarker(aUST, aYZX)
EqualAngleMarker(aUTS, aZXY)
EqualAngle(aTUS, aXYZ)
EqualAngle(aUST, aYZX)
EqualAngle(aUTS, aZXY)

AutoLabel U, S, T, X, Y, Z`,T={substance:t,style:[{contents:n,resolver:e}],domain:a,variation:"WaterspoutFish94072"};export{T as default};
//# sourceMappingURL=ex.trio-113d8a11.js.map
