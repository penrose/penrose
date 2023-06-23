import{s as n,d as a}from"./Group.domain-2c102861.js";import{m as e}from"./resolver-52f68c7c.js";import"./iframe-b9a20e36.js";const l=e("group-theory"),t=`canvas {
    width = 240
    height = 200
}

colors {
   lightGray = rgba( .8, .8, .8, 1. )
   mediumGray = rgba( .6, .6, .6, 1. )
   darkGray = rgba( .4, .4, .4, 1. )
}

global {
   scalar tableWidth = 180
   scalar tableHeight = 180

   scalar boxPadding = 2.0
}

-- draw each group element as a label for a row and column
forall Element g
{
   scalar m = match_id
   scalar n = match_total

   -- associate each group element with a value t ∈ [0,1]
   scalar g.t = m / n

   -- compute horizontal/vertical coordinates u/v for each element
   scalar g.u = ( g.t - 0.5 ) * global.tableHeight
   scalar g.v = ( (1.-g.t) - 0.5 ) * global.tableWidth

   -- compute box widths/heights (for later use by cascading rules)
   scalar g.width = global.tableWidth / n
   scalar g.height= global.tableHeight / n

   -- define a color associated with this element

   -- -- Random colors
   -- scalar g.R = ?
   -- scalar g.G = ?
   -- scalar g.B = ?
   -- ensure inRange( g.R, 0, 1 )
   -- ensure inRange( g.G, 0, 1 )
   -- ensure inRange( g.B, 0, 1 )

   -- -- Rainbow colors
   -- scalar π = MathPI()
   -- scalar θ = 2.*π * g.t
   -- scalar φ = 2.*π/3.
   -- scalar g.R = .5 + .5*cos(θ + 0.*φ)
   -- scalar g.G = .5 + .5*cos(θ + 2.*φ)
   -- scalar g.B = .5 + .5*cos(θ + 4.*φ)
   
     -- "Hot" colors
     scalar T = .1 + .8*(1.-g.t)
     scalar s = max(0,min(1,T)) -- clamp to [0,1]
     scalar g.R = 1-sqr(1-max(0,min(1,3*T)))
     scalar g.G = 3.*s*s - 2.*s*s*s
     scalar g.B = max(0,min(1,3*T - 2))

   color g.boxColor = rgba( g.R, g.G, g.B, .75 )
   color g.labelColor= rgba( .75*g.R, .75*g.G, .75*g.B, 1. )

   shape g.rowLabel = Equation {
       string: g.label
       center: (-global.tableWidth/2, g.v )
       fontSize: "8px"
       fillColor: colors.darkGray
   }

   shape g.colLabel = Equation {
       string: g.label
       center: (g.u, global.tableHeight/2)
       fontSize: "8px"
       fillColor: colors.darkGray
   }
}

-- -- if colors are chosen randomly, encourage them to be far apart in RGB space
-- forall Element a; Element b
-- {
--    encourage equal( 0, 1./abs(a.R-b.R) )
--    encourage equal( 0, 1./abs(a.G-b.G) )
--    encourage equal( 0, 1./abs(a.B-b.B) )
-- }

-- draw a box for each product in the multiplication table
forall Element a; Element b; Element c
where IsProduct( a, b, c )
{
   shape productShape = Rectangle {
      center: ( b.u, c.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "8px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element e
where IsProduct( a, e, a )
{
   shape productShape = Rectangle {
      center: ( e.u, a.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "8px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element e
where IsProduct( a, a, e )
{
   shape productShape = Rectangle {
      center: ( a.u, e.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "8px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element a; Element b
where IsProduct( a, b, b )
{
   shape productShape = Rectangle {
      center: ( b.u, b.v )
      width: a.width - global.boxPadding
      height: a.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: a.boxColor
   }

   shape productText = Equation {
      string: a.label
      center: productShape.center
      fontSize: "8px"
      fillColor: a.labelColor
   }
}

-- draw a box for each product in the multiplication table
-- (since we can't match on non-distinct tuples, we must duplicate and specialize the rule for distinct tuples)
forall Element e
where IsProduct( e, e, e )
{
   shape productShape = Rectangle {
      center: ( e.u, e.v )
      width: e.width - global.boxPadding
      height: e.height - global.boxPadding
      cornerRadius: 2.0
      fillColor: e.boxColor
   }

   shape productText = Equation {
      string: e.label
      center: productShape.center
      fontSize: "8px"
      fillColor: e.labelColor
   }
}
`,c={substance:n,style:[{contents:t,resolver:l}],domain:a,variation:"MeadowbrookChimpanzee02726"};export{c as default};
//# sourceMappingURL=quaternion-multiplication-table.trio-98f7e8ec.js.map
