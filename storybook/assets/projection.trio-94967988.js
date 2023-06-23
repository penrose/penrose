import{m as e}from"./resolver-52f68c7c.js";import"./iframe-b9a20e36.js";const n=`AmbientSpace S

Hyperplane P

HyperplaneInAmbientSpace(P, S)

Vector v

VectorInAmbientSpace(v, S)
VectorInHyperplane(v, P, S)

Vector u

VectorInAmbientSpace(u, S)
VectorOrthogonalToHyperplane(u, P, S)
VectorOrthogonalToVector(u, v, S)

Vector w := addV(v, u)
VectorInAmbientSpace(w, S)


AutoLabel All
`,r=e("fake-3d-linear-algebra"),o=`canvas {
    width = 5
    height = 5
}

forall AmbientSpace S {
    S.origin = (0., 0., 0.)
    S.vp = (0.5, 1, 1) -- viewpoint
    S.origin_proj_vp = S.origin - (dot(S.origin, S.vp) / normsq(S.vp)) * S.vp
    S.origin_proj_2d = (S.origin_proj_vp[0], S.origin_proj_vp[1])
    S.shape = Circle {
      center : S.origin_proj_2d
      r : .02
      fillColor : rgba(0, 0, 0, 255)
    }
}

forall Hyperplane P; AmbientSpace S
where HyperplaneInAmbientSpace(P, S) {
    P.normal_vector = (0, 0, 1)

    vec3 vertex1 = (-2, -2, 0)
    vec3 vertex2 = (-2, 2, 0)
    vec3 vertex3 = (2, 2, 0)
    vec3 vertex4 = (2, -2, 0)

    vec3 vertex1_o = vertex1 - (dot(vertex1, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex2_o = vertex2 - (dot(vertex2, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex3_o = vertex3 - (dot(vertex3, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector
    vec3 vertex4_o = vertex4 - (dot(vertex4, P.normal_vector) / normsq(P.normal_vector)) * P.normal_vector

    vec3 vertex1_proj_vp = vertex1_o - (dot(vertex1_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex2_proj_vp = vertex2_o - (dot(vertex2_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex3_proj_vp = vertex3_o - (dot(vertex3_o, S.vp) / normsq(S.vp)) * S.vp
    vec3 vertex4_proj_vp = vertex4_o - (dot(vertex4_o, S.vp) / normsq(S.vp)) * S.vp

  P.shape = Polygon {
    strokeWidth : .01
    strokeColor : rgba(0., 0., 0., 255.)
    points : [(vertex1_proj_vp[0], vertex1_proj_vp[1]), (vertex2_proj_vp[0], vertex2_proj_vp[1]), (vertex3_proj_vp[0], vertex3_proj_vp[1]), (vertex4_proj_vp[0], vertex4_proj_vp[1])]
  }
}

forall Vector v; AmbientSpace S
where VectorInAmbientSpace(v, S) {
  v.vec = (?, ?, ?)

  vec3 vec_endpoint_proj_vp = v.vec - (dot(v.vec, S.vp) / normsq(S.vp)) * S.vp

  v.shape = Line {
    start: S.origin_proj_2d
    end: (vec_endpoint_proj_vp[0], vec_endpoint_proj_vp[1])
    strokeWidth : 0.01
    endArrowhead: "straight"
    strokeColor : rgba(0, 0, 0, 255.)
  }
}

forall Vector v; Hyperplane P; AmbientSpace S
where VectorInHyperplane(v, P, S) {
  ensure inRange(v.vec[0], -2, 2)
  ensure inRange(v.vec[1], -2, 2)
  ensure equal(v.vec[2], 0)
  ensure equal(dot(v.vec, P.normal_vector), 0)
  v.shape above P.shape
}

forall Vector v; Hyperplane P; AmbientSpace S
where VectorOrthogonalToHyperplane(v, P, S) {
  scalar multiplier = ?
  override v.vec = multiplier * P.normal_vector
  encourage notTooClose(v.shape, S.shape, 0.01)
}

forall Vector v; Vector u; AmbientSpace S
where VectorOrthogonalToVector(v, u, S) {
  encourage nonDegenerateAngle(v.shape, S.shape, u.shape, 500)
}

forall Vector u; Vector v; Vector w; AmbientSpace S
where w := addV(u, v); VectorInAmbientSpace(u, S); VectorInAmbientSpace(v, S); VectorInAmbientSpace(w, S) {
  override w.vec = u.vec + v.vec

  w.dashed_u = Line {
    start: v.shape.end
    end: w.shape.end
    strokeStyle : "dashed"
    strokeColor: #70707080
    strokeWidth : .01
    strokeDasharray: "0.07,0.05"
  }
  w.dashed_v = Line {
    start: u.shape.end
    end: w.shape.end
    strokeStyle : "dashed"
    strokeColor: #70707080
    strokeWidth : .01
    strokeDasharray: "0.07,0.05"
  }
}
`,t=`type AmbientSpace
type Hyperplane
type Vector

predicate HyperplaneInAmbientSpace(Hyperplane, AmbientSpace)
predicate VectorInAmbientSpace(Vector, AmbientSpace)
predicate VectorInHyperplane(Vector, Hyperplane, AmbientSpace)
predicate VectorOrthogonalToHyperplane(Vector, Hyperplane, AmbientSpace)
predicate VectorOrthogonalToVector(Vector, Vector, AmbientSpace)

function addV(Vector, Vector) -> Vector
`,a={substance:n,style:[{contents:o,resolver:r}],domain:t,variation:"SucculentsReindeer397"};export{a as default};
//# sourceMappingURL=projection.trio-94967988.js.map
