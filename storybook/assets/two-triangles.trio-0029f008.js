import{m as n}from"./resolver-f1d01b85.js";import"./iframe-02305a86.js";const e=`Triangle s, t

Label s $\\mathbf{f}$
Label t $\\widetilde{\\mathbf{f}}$
`,a=n("triangle-mesh-3d"),t=`canvas {
   width = 200
   height = 200
}

global {

   -- Ground plane coordinates in 3D
   scalar planeSize = 50 -- plane size
   scalar planeHeight = -40 -- plane height

   -- Use a simple pinhole camera model, where the
   -- only camera parameter is the distance along Z
   scalar cZ = -160 -- camera z coordinate

   -- Corner coordinates of the global ground plane
   vec3 q00 = ( -planeSize, planeHeight, -planeSize )
   vec3 q10 = (  planeSize, planeHeight, -planeSize )
   vec3 q01 = ( -planeSize, planeHeight,  planeSize )
   vec3 q11 = (  planeSize, planeHeight,  planeSize )

   -- Apply a random rotation to the ground plane
   -- (Note that we could also apply this rotation to the triangle
   -- vertices, but since they're sampled randomly, it wouldn't
   -- really change the appearance of the kinds of diagrams we sample).
   scalar θ = ?
   vec3 Q00 = ( q00[0]*cos(θ) + q00[2]*sin(θ), q00[1], q00[2]*cos(θ) - q00[0]*sin(θ) )
   vec3 Q10 = ( q10[0]*cos(θ) + q10[2]*sin(θ), q10[1], q10[2]*cos(θ) - q10[0]*sin(θ) )
   vec3 Q01 = ( q01[0]*cos(θ) + q01[2]*sin(θ), q01[1], q01[2]*cos(θ) - q01[0]*sin(θ) )
   vec3 Q11 = ( q11[0]*cos(θ) + q11[2]*sin(θ), q11[1], q11[2]*cos(θ) - q11[0]*sin(θ) )

   -- Perform perspective projection on 3D coordinates to get 2D coordinates p
   vec2 p00 = canvas.width * (Q00[0],Q00[1])/(Q00[2] - global.cZ)
   vec2 p10 = canvas.width * (Q10[0],Q10[1])/(Q10[2] - global.cZ)
   vec2 p01 = canvas.width * (Q01[0],Q01[1])/(Q01[2] - global.cZ)
   vec2 p11 = canvas.width * (Q11[0],Q11[1])/(Q11[2] - global.cZ)

   -- Draw polygon using projected 2D coordinates p
   shape groundPlane = Polygon {
      points: (p00,p10,p11,p01)
      width: canvas.width
      height: canvas.height
      fillColor: rgba(0,0,0,0.1)
      strokeColor: rgba(.7,.7,.7,1)
      strokeWidth: .5
      ensureOnCanvas: false
   }
}

forall Triangle t
{
   -- We'll sample the triangle vertices from a bounding box of size c
   scalar c = .9*min( global.planeSize, abs(global.planeHeight) )

   -- triangle vertex coordinates in 3D
   vec3 qi = (?,?,?)
   vec3 qj = (?,?,?)
   vec3 qk = (?,?,?)

   ensure -c < qi[0]
   ensure qi[0] < c
   ensure -c < qi[1]
   ensure qi[1] < c
   ensure -c < qi[2]
   ensure qi[2] < c

   ensure -c < qj[0]
   ensure qj[0] < c
   ensure -c < qj[1]
   ensure qj[1] < c
   ensure -c < qj[2]
   ensure qj[2] < c

   ensure -c < qk[0]
   ensure qk[0] < c
   ensure -c < qk[1]
   ensure qk[1] < c
   ensure -c < qk[2]
   ensure qk[2] < c

   -- Perform perspective projection on 3D coordinates to get 2D coordinates p
   vec2 t.pi = canvas.width * (qi[0],qi[1])/(qi[2]-global.cZ)
   vec2 t.pj = canvas.width * (qj[0],qj[1])/(qj[2]-global.cZ)
   vec2 t.pk = canvas.width * (qk[0],qk[1])/(qk[2]-global.cZ)

   -- Draw polygon using projected 2D coordinates p
   shape t.icon = Polygon {
      points: (t.pi,t.pj,t.pk)
      width: canvas.width
      height: canvas.height
      fillColor: #34379aaa
      strokeColor: #1b1f8a
      strokeWidth: .5
      ensureOnCanvas: false
   }

   -- Make sure the triangle is positively oriented in the
   -- image plane, and has some- "fat" angles so that it
   -- doesn't degenerate
   vec2 eij = t.pj - t.pi
   vec2 ejk = t.pk - t.pj
   vec2 eki = t.pi - t.pk
   ensure cross2D( eij, -ejk ) < 0
   ensure angleFrom( -ejk, eij ) > toRadians( 45 )
   ensure angleFrom( -eki, ejk ) > toRadians( 45 )
   ensure angleFrom( -eij, eki ) > toRadians( 45 )

   -- Draw triangle vertices and labels as dots and equations,
   -- using again the projected 2D coordinates p
   scalar dotSize = 1.0
   color dotColor = rgba(0,0,0,1)
   string dotFontSize = "10px"
   scalar offset = 6 -- offset of labels from vertices
   shape t.vertexI = Circle {
      center: t.pi
      r: dotSize
      fillColor: dotColor
   }
   shape t.vertexJ = Circle {
      center: t.pj
      r: dotSize
      fillColor: dotColor
   }
   shape t.vertexK = Circle {
      center: t.pk
      r: dotSize
      fillColor: dotColor
   }
   shape t.labelI = Equation {
       string: t.label + "_i"
       center: t.pi - offset*unit(eij-eki)
       fontSize: dotFontSize
   }
   shape t.labelJ = Equation {
       string: t.label + "_j"
       center: t.pj - offset*unit(ejk-eij)
       fontSize: dotFontSize
   }
   shape t.labelK = Equation {
       string: t.label + "_k"
       center: t.pk - offset*unit(eki-ejk)
       fontSize: dotFontSize
   }

   -- Finally, draw a shadow of the triangle on the global ground plane
   -- by just replacing the y-coordinate with the height of the ground plane
   scalar h = global.planeHeight
   vec2 ri = (qi[0],h)
   vec2 rj = (qj[0],h)
   vec2 rk = (qk[0],h)

   -- Perform perspective projection on 3D coordinates r to get 2D coordinates s
   vec2 si = canvas.width * ri/(qi[2]-global.cZ)
   vec2 sj = canvas.width * rj/(qj[2]-global.cZ)
   vec2 sk = canvas.width * rk/(qk[2]-global.cZ)

   -- Draw shadow polygon using projected 2D coordinates s
   shape t.shadow = Polygon {
      points: (si,sj,sk)
      width: canvas.width
      height: canvas.height
      fillColor: rgba(0,0,0,0.1)
      strokeColor: none()
      ensureOnCanvas: false
   }

   -- Make sure the triangle shadow lands on the ground plane
   --ensure contains( global.groundPlane, t.shadow )
}

-- For any pair of triangles, make sure that triangles
-- don't overlap, and moreover the vertices of one triangle are
-- far from being contained in the other triangle (which helps
-- to avoid overlapping labels).
forall Triangle s; Triangle t
{
   scalar padding = 10.0

   -- make sure triangles don't overlap
   ensure disjoint( t.icon, s.icon )

   -- make sure vertices of t are far from s
   ensure disjoint( t.vertexI, s.icon, padding )
   ensure disjoint( t.vertexJ, s.icon, padding )
   ensure disjoint( t.vertexK, s.icon, padding )

   -- make sure vertices of s are far from t
   ensure disjoint( s.vertexI, t.icon, padding )
   ensure disjoint( s.vertexJ, t.icon, padding )
   ensure disjoint( s.vertexK, t.icon, padding )
}

`,o=`type Triangle
`,s={substance:e,style:[{contents:t,resolver:a}],domain:o,variation:"LightheartedCormorant8083",excludeWarnings:["BBoxApproximationWarning"]};export{s as default};
//# sourceMappingURL=two-triangles.trio-0029f008.js.map
