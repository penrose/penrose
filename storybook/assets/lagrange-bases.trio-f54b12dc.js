import{m as e}from"./resolver-7e2b0c77.js";import"./iframe-f81e2c74.js";const n=`Node a, b, c, d
Node u, v, x, y, z
Element t2 := QuadraticTriangle( a, b, c, x, y, z )
Element e := QuadraticTriangle( b, a, d, x, u, v )

Label a $p_i$
Label b $p_j$
Label c $p_k$
Label d $p_l$
Label x $m_{ij}$
Label y $m_{jk}$
Label z $m_{ki}$
Label u $m_{il}$
Label v $m_{jl}$

`,o=e("lagrange-bases"),r=`canvas {
   width = 240
   height = 180
}

Global {
   -- background color
   shape box = Rectangle {
      fillColor: rgba( .9, 1, .9, 1 )
      center: (0,0)
      width: canvas.width
      height: canvas.height
      ensureOnCanvas: false
   }

   scalar dotSize = 2.5
   scalar smallDotSize = 2.
   scalar edgeWidth = 1.5
   string labelSize = "10px"
}

Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color lightGray = rgba(0,0,0,.1)
}

forall Node x {
   shape x.icon = Circle {
      center: (?,?)
      r: Global.dotSize
      fillColor: Colors.black
   }

   layer x.icon above Global.box
}

-- put text near a node if it has a label
forall Node x
where x has math label {

   -- direction of label relative to the node location
   -- (as an angle in radians)
   scalar theta = ?

   x.labelText = Equation {
      center: x.icon.center + 10.*unitVector(theta)
      string: x.label
      fontSize: Global.labelSize
      fillColor: Colors.black
   }
}

forall Element e; Node vi; Node vj; Node vk
where e := LinearTriangle( vi, vj, vk ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center

   -- draw edges as straight lines
   shape e.curveIJ = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveJK = Line {
      start: pj
      end: pk
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveKI = Line {
      start: pk
      end: pi
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }

   -- make sure triangles are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any triangle sum to π
   scalar minAngle = toRadians( 30. )
   ensure greaterThan( angleFrom(pj-pi,pk-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
}

forall Element e; Node vi; Node vj; Node vk; Node vl
where e := LinearQuad( vi, vj, vk, vl ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center
   vec2 pl = vl.icon.center

   -- draw edges as straight lines
   shape e.curveIJ = Line {
      start: pi
      end: pj
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveJK = Line {
      start: pj
      end: pk
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveKL = Line {
      start: pk
      end: pl
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }
   shape e.curveLI = Line {
      start: pl
      end: pi
      strokeColor: Colors.black
      strokeWidth: Global.edgeWidth
      ensureOnCanvas: false
   }

   -- make sure quads are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any quad sum to 2π
   scalar minAngle = toRadians( 30. )
   ensure greaterThan( angleFrom(pj-pi,pl-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
   ensure greaterThan( angleFrom(pl-pk,pj-pk), minAngle )
}

forall Element e; Node vi; Node vj; Node vk; Node mij; Node mjk; Node mki
where e := QuadraticTriangle( vi, vj, vk, mij, mjk, mki ) {

   -- grab node locations (for convenience)
   vec2 pi = vi.icon.center
   vec2 pj = vj.icon.center
   vec2 pk = vk.icon.center
   vec2 pij = mij.icon.center
   vec2 pjk = mjk.icon.center
   vec2 pki = mki.icon.center

   -- offset the middle nodes in some random direction from the
   -- geometric midpoints between the triangle vertices
   scalar offsetSize = 20.
   scalar thetaIJ = ?
   scalar thetaJK = ?
   scalar thetaKI = ?
   override mij.icon.center = (pi+pj)/2. + offsetSize * unitVector(thetaIJ)
   override mjk.icon.center = (pj+pk)/2. + offsetSize * unitVector(thetaJK)
   override mki.icon.center = (pk+pi)/2. + offsetSize * unitVector(thetaKI)

   -- offset the midpoint labels so they don't cross the edges
   -- (since we don't yet support disjoint constraints for Bézier
   -- curves, just push the vertex away from both segments to the midpoint)
   vec2 cij = mij.icon.center
   vec2 nij = unit(unit(cij-pi) + unit(cij-pj))
   override mij.labelText.center = cij + 12.*nij

   vec2 cjk = mjk.icon.center
   vec2 njk = unit(unit(cjk-pj) + unit(cjk-pk))
   override mjk.labelText.center = cjk + 12.*njk

   vec2 cki = mki.icon.center
   vec2 nki = unit(unit(cki-pk) + unit(cki-pi))
   override mki.labelText.center = cki + 12.*nki

   -- similarly, offset the vertex labels so they don't cross the edges
   vec2 ni = unit(unit(pi-pij) + unit(pi-pki))
   override vi.labelText.center = pi + 10.*ni

   vec2 nj = unit(unit(pj-pjk) + unit(pj-pij))
   override vj.labelText.center = pj + 10.*nj

   vec2 nk = unit(unit(pk-pki) + unit(pk-pjk))
   override vk.labelText.center = pk + 10.*nk

   -- draw the midpoints in a different style
   override mij.icon.r = Global.smallDotSize
   override mij.icon.fillColor = Colors.white
   override mij.icon.strokeColor = Colors.black
   override mij.icon.strokeWidth = 1.

   override mjk.icon.r = Global.smallDotSize
   override mjk.icon.fillColor = Colors.white
   override mjk.icon.strokeColor = Colors.black
   override mjk.icon.strokeWidth = 1.

   override mki.icon.r = Global.smallDotSize
   override mki.icon.fillColor = Colors.white
   override mki.icon.strokeColor = Colors.black
   override mki.icon.strokeWidth = 1.

   -- draw edges as quadratic Bézier curves
   shape e.curveIJ = Path {
      d: interpolateQuadraticFromPoints("open", pi, pij, pj)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }
   shape e.curveJK = Path {
      d: interpolateQuadraticFromPoints("open", pj, pjk, pk)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }
   shape e.curveKI = Path {
      d: interpolateQuadraticFromPoints("open", pk, pki, pi)
      strokeColor: Colors.black
      strokeWidth: 1.5
      ensureOnCanvas: false
   }

   layer e.curveIJ above Global.box
   layer e.curveJK above Global.box
   layer e.curveKI above Global.box

   -- make sure triangles are reasonably nice by ensuring
   -- the signed corner angles aren't too small (we don't
   -- need to worry about the final angle since the
   -- angles of any triangle sum to π
   scalar minAngle = toRadians( 50. )
   ensure greaterThan( angleFrom(pj-pi,pk-pi), minAngle )
   ensure greaterThan( angleFrom(pk-pj,pi-pj), minAngle )
}

`,a=`type Node
type Element

constructor LinearTriangle( Node vi, Node vj, Node vk ) -> Element
constructor LinearQuad( Node vi, Node vj, Node vk, Node vl ) -> Element
constructor QuadraticTriangle( Node vi, Node vj, Node vk, Node mij, Node mjk, Node mki ) -> Element
`,l={substance:n,style:[{contents:r,resolver:o}],domain:a,variation:"CadaverousJaguar540"};export{l as default};
//# sourceMappingURL=lagrange-bases.trio-f54b12dc.js.map
