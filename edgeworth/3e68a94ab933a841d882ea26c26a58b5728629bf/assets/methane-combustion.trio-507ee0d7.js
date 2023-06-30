import{a as n}from"./index-d0dd7cba.js";import{d as e}from"./structural-formula.domain-614d22a0.js";const o=`AutoLabel All

-- combustion reaction, expressed via structural-formula DSL

Reaction combustion
Label combustion "Methane Combustion Reaction"
IsNetForward( combustion )

-- reactants: CH4 + 2O2 ====================================
Carbon C1
Hydrogen H1, H2, H3, H4
Oxygen O1, O2, O3, O4

Molecule methane
Label methane $\\text{methane}\\ (\\mathrm{CH}_4)$
Contains( methane, C1 )
Contains( methane, H1 )
Contains( methane, H2 )
Contains( methane, H3 )
Contains( methane, H4 )
SingleBond( C1, H1 )
SingleBond( C1, H2 )
SingleBond( C1, H3 )
SingleBond( C1, H4 )

Molecule oxygen1, oxygen2
Label oxygen1 $\\text{oxygen}\\ (\\mathrm{O}_2)$
Label oxygen2 $\\text{oxygen}\\ (\\mathrm{O}_2)$
Contains( oxygen1, O1 )
Contains( oxygen1, O2 )
DoubleBond( O1, O2 )
Contains( oxygen2, O3 )
Contains( oxygen2, O4 )
DoubleBond( O3, O4 )

IsReactant( methane )
IsReactant( oxygen1 )
IsReactant( oxygen2 )

-- products: CO2 + 2H20 ====================================
Carbon C2
Hydrogen H5, H6, H7, H8
Oxygen O5, O6, O7, O8

Molecule carbonDioxide
Label carbonDioxide $\\text{carbon dioxide}\\ (\\mathrm{CO}_2)$
Contains( carbonDioxide, C2 )
Contains( carbonDioxide, O5 )
Contains( carbonDioxide, O6 )
DoubleBond( C2, O5 )
DoubleBond( C2, O6 )

Molecule water1, water2
Label water1 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Label water2 $\\text{water}\\ (\\mathrm{H}_2\\mathrm{O})$
Contains( water1, O7 )
Contains( water1, H5 )
Contains( water1, H6 )
SingleBond( O7, H5 )
SingleBond( O7, H6 )
Contains( water2, O8 )
Contains( water2, H7 )
Contains( water2, H8 )
SingleBond( O8, H7 )
SingleBond( O8, H8 )

IsProduct( carbonDioxide )
IsProduct( water1 )
IsProduct( water2 )

`,r=n("structural-formula"),a=`-- TODO would be really helpful to be able to join multiple
-- .style files---currently we are maintaining parallel edits
-- across both pseudo-3d.style and pseudo-3d-reaction.style

canvas {
   scalar width  = 1248.
   scalar height = 702.
}

Colors {
   vec4 clear     = rgba( 0., 0., 0., 0. )
   vec4 black     = rgba( 0., 0., 0., 1. )
   vec4 gray      = rgba( .5, .5, .5, 1. )
   vec4 lightGray = rgba( .9, .9, .9, 1. )
   vec4 white     = rgba( 1., 1., 1., 1. )
   vec4 red       = rgba( 1., 0., 0., 1. )
   vec4 green     = rgba( 0., .7, 0., 1. )
   vec4 blue      = rgba( 0., 0., 1., 1. )
   vec4 darkRed   = rgba( .7, 0., 0., 1. )
   vec4 darkBlue  = rgba( 0., 0., .7, 1. )
   vec4 purple    = rgba( .66, .36, .95, 1. )
   vec4 turquoise  = rgba( .1, .7, .6, 1. )
}

Global {
   scalar atomRadius = 25.
   scalar bondLength = 60.

   scalar padding = 50.

   -- specify the typeface(s) that should
   -- be used, in order of availability
   string fontFamily = "Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif"

   -- box around the whole canvas
   shape bbox = Rectangle {
      width : canvas.width
      height : canvas.height
      center : (0.,0.)
      fillColor : Colors.clear
      strokeColor : Colors.gray
      strokeWidth : 8
   }

   scalar reactionBoxSize = .75 * canvas.width/2.
   scalar reactionBoxTop = reactionBoxSize/2.
   scalar reactionBoxBottom = -reactionBoxSize/2.

   -- box around reactants
   scalar reactantCenter = -canvas.width/4.
   shape reactantBox = Rectangle {
      width : reactionBoxSize
      height : reactionBoxSize
      center : (reactantCenter,0.)
      fillColor : rgba( 0., 0., 0., .1 )
      strokeColor : Colors.clear
      strokeWidth : 12
      cornerRadius : 20
   }
   shape reactantText = Text {
      string : "reactants"
      center : (reactantCenter,-24.+reactionBoxBottom) -- TODO make issue about non-commutativity of + here
      fillColor : Colors.black
      fontSize : "18px"
      fontFamily: Global.fontFamily
      fontWeight: "bold"
   }

   -- box around products
   scalar productCenter = canvas.width/4.
   scalar productTop = .75*canvas.width/4.
   shape productBox = Rectangle {
      width : reactionBoxSize
      height : reactionBoxSize
      center : (productCenter,0.)
      fillColor : rgba( 0., 0., 0., .1 )
      strokeColor : Colors.clear
      strokeWidth : 12
      cornerRadius : 20
   }
   shape productText = Text {
      string : "products"
      center : (productCenter,-24.+reactionBoxBottom)
      fillColor : Colors.black
      fontSize : "18px"
      fontFamily: Global.fontFamily
      fontWeight: "bold"
   }
}

forall Node n {

   scalar cx = ?
   scalar cy = ?
   vec2 n.center = (cx,cy)

   scalar R = Global.atomRadius

   shape n.icon = Circle {
      r : R
      center : n.center
      fillColor : Colors.white
      strokeColor : Colors.black
      strokeWidth : 3.
   }

   shape n.shadow = Ellipse {
      rx : 2.*Global.atomRadius
      ry : Global.atomRadius
      center : (cx,cy) + (0.,-2.*R)
      fillColor : rgba( .95, .95, .95, 1. )
      strokeColor : Colors.clear
      strokeWidth : 3.
   }

   shape n.text = Text {
      string : n.label
      center : n.center
      fillColor : Colors.black
      fontSize : "10.5px"
   }

   ensure contains( Global.bbox, n.icon )

   layer n.icon below n.text
}

-- make sure shadows are drawn below all molecules
forall Node n1; Node n2 {
   layer n1.shadow below n2.icon
   layer n2.shadow below n1.icon
}

-- draw functional groups as boxes
forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.text.fillColor = Colors.black
   override g.shadow.fillColor = Colors.clear

   g.box = Rectangle {
      center : g.center
      width : 3.*Global.atomRadius
      height : 1.5*Global.atomRadius
      fillColor : Colors.lightGray
      strokeColor : Colors.gray
      strokeWidth : 3.
      cornerRadius : 10.
   }

   layer g.shadow below g.box
}

forall Oxygen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.red
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Carbon a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.black
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.blue
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.black
   override a.text.string = ""

   -- make hydrogen atoms (and their shadows) smaller
   override a.icon.r = .75*Global.atomRadius
   override a.shadow.rx = .75*2.*Global.atomRadius
   override a.shadow.ry = .75*Global.atomRadius
}

forall Chlorine a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.green
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}

forall Sodium a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.purple
   override a.text.fillColor = Colors.white
   override a.text.string = ""
}


forall Node n1; Node n2
where SingleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line above n2.icon
   layer innerLine above line
   layer innerLine below n1.icon
}

forall Node n1; Node n2
where DoubleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   vec2 v = ( -u[1], u[0] )
   scalar r = Global.atomRadius

   shape line1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   shape line2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.turquoise
      strokeLinecap: "round"
   }

   shape innerLine2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.white
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 above n2.icon
   layer innerLine1 above line1
   layer innerLine1 below n1.icon

   layer line2 above n2.icon
   layer innerLine2 above line2
   layer innerLine2 below n1.icon

   layer line2 above innerLine1
}

forall Node n1; Node n2
where IonicBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 2.5
      strokeColor : Colors.turquoise
      strokeLinecap: "butt"
      style: "dashed"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line below n1.icon
   layer line below n2.icon
}


-- make bonds with hydrogen shorter
forall Node n; Hydrogen h
where SingleBond(n,h) {
   vec2 x1 = n.center
   vec2 x2 = h.center
   encourage equal( 2.*norm(x1-x2), .5*Global.bondLength )
}

-- give common molecules a physical bond angle
forall Oxygen o; Hydrogen h1; Hydrogen h2 -- water (H2O)
where SingleBond(o,h1); SingleBond(o,h2) { -- TODO make an issue about symmetry of predicate matches
   vec2 a = o.center
   vec2 b = h1.center
   vec2 c = h2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(104.5) )
}
forall Carbon c0; Oxygen o1; Oxygen o2 -- carbon dioxide (CO2)
where DoubleBond(c0,o1); DoubleBond(c0,o2) {
   vec2 a = c0.center
   vec2 b = o1.center
   vec2 c = o2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(180.) )
}

-- use a Coulomb-like force to prevent nodes from overlapping
-- (but only worry about nodes in the same reactant/product box)
forall Molecule m; Node n1; Node n2
where IsReactant(m); Contains(m,n1); Contains(m,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m1; Molecule m2; Node n1; Node n2
where IsReactant(m1); IsReactant(m2); Contains(m1,n1); Contains(m2,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m; Node n1; Node n2
where IsProduct(m); Contains(m,n1); Contains(m,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}
forall Molecule m1; Molecule m2; Node n1; Node n2
where IsProduct(m1); IsProduct(m2); Contains(m1,n1); Contains(m2,n2) {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
}


-- place a label near the Nodes in each Molecule
forall Molecule m {

   -- m.box = Rectangle {
   --    center : (?,?)
   --    fillColor : Colors.clear
   -- }

   m.labelCenter = (?,?)

   m.text = Equation {
      string : m.label
      fillColor : Colors.black
      fontSize : "13.5px"
      center : m.labelCenter
      --center : m.box.center - (0.,3.*Global.atomRadius)
      -- TODO add stroke (have to expose SVG text stroke in Penrose renderer)
   }

   -- used to prevent overlap with molecules
   -- (since "ensure disjoint" isn't currently supported for Equation-Circle pairs)
   scalar R = 20.
   m.textPhantom1 = Circle {
      center : m.labelCenter
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
   m.textPhantom2 = Circle {
      center : m.labelCenter + (2.*R,0.)
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
   m.textPhantom3 = Circle {
      center : m.labelCenter - (2.*R,0.)
      r : R
      fillColor : Colors.clear
      strokeColor : Colors.clear
      strokeWidth : 3.
   }
}
forall Molecule m; Node n
where Contains(m,n) {
   --override m.box.center = n.icon.center
   encourage near( m.text, n.icon )
   ensure disjoint( m.textPhantom1, n.icon )
   ensure disjoint( m.textPhantom2, n.icon )
   ensure disjoint( m.textPhantom3, n.icon )
   layer m.text above n.icon
}

forall Reaction r {

   scalar h = (Global.reactionBoxTop + canvas.height/2.)/2.

   r.text = Text {
      string : r.label
      center : (0.,h)
      fontSize : "30px"
      fontFamily: Global.fontFamily
      fontVariant: "small-caps"
      fillColor : Colors.black
   }
}

-- put all reactants on the left
forall Molecule m; Node n
where IsReactant(m); Contains(m,n) {
   ensure contains( Global.reactantBox, n.icon, Global.padding )
   ensure contains( Global.reactantBox, m.text, Global.padding )
   layer n.icon above Global.reactantBox
   layer n.shadow below Global.reactantBox
}

-- put all products on the right
forall Molecule m; Node n
where IsProduct(m); Contains(m,n) {
   ensure contains( Global.productBox, n.icon, Global.padding )
   ensure contains( Global.productBox, m.text, Global.padding )
   layer n.icon above Global.productBox
   layer n.shadow below Global.productBox
}

forall Reaction r
where IsNetForward(r) {

   vec2 Rc = Global.reactantCenter
   vec2 Pc = Global.productCenter
   scalar s = Global.reactionBoxSize
   scalar p = 10.

   shape reactionArrow = Line {
      start : (Rc+(0.5*s)+p,0.)
      end : (Pc-(0.5*s)-p,0.)
      strokeColor : Colors.black
      strokeWidth : 5.
      endArrowhead : "straight"
      endArrowheadSize : .5
   }
}

`,i={substance:o,style:[{contents:a,resolver:r}],domain:e,variation:"",excludeWarnings:[]};export{i as default};
