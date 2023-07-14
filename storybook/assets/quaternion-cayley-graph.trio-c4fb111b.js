import{s as e,d as n}from"./Group.domain-2c102861.js";import{m as r}from"./resolver-b9429209.js";import"./iframe-8d1c39a4.js";const t=r("group-theory"),a=`-- Draws a Cayley graph for a group G, assuming that
--   (i) all elements g of G have been declared as an Element,
--   (ii) all generator s of G have been tagged, via IsGenerator(), and
--   (iii) the group multiplication table has been specified, via IsProduct.
-- For drawing the Cayley graph, it is not strictly necessary to specify the
-- entire multiplication table: one can instead just specify the subset of
-- the table corresponding to right-multiplication by any generator.  However,
-- for other Styles in the domain Group, it may be helpful to generate Substance
-- files that specify the full table.

canvas {
    width = 240
    height = 200
}

colors {
   white = rgba( 1., 1., 1., 1. )
   lightGray = rgba( .8, .8, .8, 1. )
   mediumGray = rgba( .6, .6, .6, 1. )
   darkGray = rgba( .4, .4, .4, 1. )
}

global {
   scalar targetEdgeLength = 40.
   scalar pathWidth = 1.
   scalar pathOutlineWidth = 3.
}

-- draw each group element as a dot
forall Element g
{
   shape g.icon = Circle {
      r: 5.0 -- radius
      fillColor: colors.lightGray
      strokeColor: colors.mediumGray
      strokeWidth: .55
      ensureOnCanvas: true
   }

   shape g.labelText = Equation {
       string: g.label
       center: g.icon.center
       fontSize: "5px"
       fillColor: colors.darkGray
   }
}

-- draw a circle around the identity element
forall Element e
where IsIdentity(e)
{
   shape identityMarker = Circle {
      center: e.icon.center
      r: 1.5*e.icon.r
      fillColor: rgba(0,0,0,.1)
   }
}

 -- highlight any generating element by changing its color
 forall Element s
 where IsGenerator(s)
 {
    scalar r = ?
    scalar g = ?
    scalar b = ?
    ensure inRange(r,.25,1.0)
    ensure inRange(g,.25,1.0)
    ensure inRange(b,.25,1.0)

    override s.icon.fillColor = rgba(r,g,b,1.0)
    override s.icon.strokeColor = rgba(.7*r,.7*g,.7*b,1.0)
    override s.labelText.fillColor = s.icon.strokeColor
 }

-- encourage all nodes to avoid each other
forall Element g1; Element g2
{
   vec2 x1 = g1.icon.center
   vec2 x2 = g2.icon.center
   scalar d = norm( x1 - x2 )

   -- minimize a Coulomb potential
   encourage equal( 0., 2.*sqr(1000./d) )

   -- minimize IPC potential
   -- scalar dhat = 10.*g1.icon.r
   --encourage equal( 0., max(0, -sqr(d - dhat)*log(d/dhat))) 
}

-- rule for any two elements g1, g2 related by a generator s
forall Element g1; Element g2; Element s
where IsGenerator(s); IsProduct( g2, g1, s )
{
   -- draw an arrow from g1 to g2
   vec2 x0 = g1.icon.center
   vec2 x2 = g2.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = g1.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}

-- same rule as above, but catches the special case where g1 is the identity
-- (since we don't currently support matching on non-distinct tuples)
forall Element s; Element e
where IsGenerator(s); IsProduct( s, e, s )
{
   -- draw an arrow from e to s
   vec2 x0 = e.icon.center
   vec2 x2 = s.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = e.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}

-- same rule as above, but catches the special case where g1 = s
-- (since we don't currently support matching on non-distinct tuples)
forall Element g; Element s
where IsGenerator(s); IsProduct( g, s, s )
{
   -- draw an arrow from s to g
   vec2 x0 = s.icon.center
   vec2 x2 = g.icon.center
   vec2 u = (x2-x0)/norm(x2-x0)
   vec2 n = rot90(u)
   vec2 p0 = x0
   vec2 p2 = x2 - 10*u + 3*n
   vec2 m = (p0+p2)/2
   vec2 p1 = m + 3.*n
   shape orientedPath = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: s.icon.fillColor
      endArrowhead: "straight"
      endArrowheadSize: 0.75
      strokeWidth: global.pathWidth
   }
   shape pathOutline = Path {
      d: interpolateQuadraticFromPoints( "open", p0, p1, p2 )
      strokeColor: colors.white
      endArrowhead: "none"
      strokeWidth: global.pathOutlineWidth
   }
   layer pathOutline below orientedPath

   -- encourage these nodes to be close together, by minimizing a spring energy
   vec2 x1 = s.icon.center
   scalar d = norm( x1 - x2 )
   scalar k = 1. -- spring stiffness
   scalar L = global.targetEdgeLength -- rest length
   encourage equal( 0., k*(d-L)*(d-L)/2. ) -- minimize ½ k(d-L)²
}
`,s={substance:e,style:[{contents:a,resolver:t}],domain:n,variation:"MeadowbrookChimpanzee02726",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=quaternion-cayley-graph.trio-c4fb111b.js.map
