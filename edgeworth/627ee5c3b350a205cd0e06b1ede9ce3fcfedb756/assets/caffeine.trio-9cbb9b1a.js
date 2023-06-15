import{a as n}from"./index-0672ecd7.js";const e=`-- caffeine molecule, expressed via structural-formula DSL
Carbon C1, C2, C3, C4, C5, C6, C7, C8
Nitrogen N1, N2, N3, N4
Oxygen O1, O2
Hydrogen H1, H2, H3, H4, H5, H6, H7, H8, H9, H10

SingleBond(N2, C4)
SingleBond(C4, C5)
DoubleBond(C5, C6)
SingleBond(C6, N1)
SingleBond(N1, C2)
SingleBond(N2, C2)
DoubleBond(C2, O2)
SingleBond(N2, C1)
SingleBond(C1, H7)
SingleBond(C1, H6)
SingleBond(C1, H5)
DoubleBond(C4, O1)
SingleBond(C7, H4)
SingleBond(C7, H3)
SingleBond(H2, C7)
SingleBond(N3, C7)
SingleBond(N3, C5)
SingleBond(C8, N3)
DoubleBond(C8, N4)
SingleBond(N4, C6)
SingleBond(C8, H1)
SingleBond(C3, H10)
SingleBond(C3, H8)
SingleBond(C3, H9)
SingleBond(N1, C3)

AutoLabel All
`,o=n("structural-formula"),r=`canvas {
   scalar width  = 1066.5
   scalar height = 600.0
}

Colors {
   vec4 clear     = rgba( 0., 0., 0., 0. )
   vec4 black     = rgba( 0., 0., 0., 1. )
   vec4 gray      = rgba( .5, .5, .5, 1. )
   vec4 white     = rgba( 1., 1., 1., 1. )
   vec4 red       = rgba( 1., 0., 0., 1. )
   vec4 blue      = rgba( 0., 0., 1., 1. )
   vec4 darkRed   = rgba( .7, 0., 0., 1. )
   vec4 darkBlue  = rgba( 0., 0., .7, 1. )
}

Global {
   scalar atomRadius = 20.
   scalar bondLength = 60.

   shape bbox = Rectangle {
      width : canvas.width
      height : canvas.height
      center : (0.,0.)
      fillColor : Colors.clear
      strokeColor : Colors.gray
   }
}

forall Node n {

   vec2 n.center = (?,?)

   shape n.icon = Circle {
      r : Global.atomRadius
      center : n.center
      fillColor : Colors.white
      strokeColor : Colors.black
      strokeWidth : 2.
   }

   shape n.background = Image {
      href : "structural-formula-atom.svg"
      center : n.center
      width : 4.*Global.atomRadius
      height : 4.*Global.atomRadius
   }

   shape n.text = Text {
      string: n.label
      center: n.center
      fillColor: Colors.black
      fontSize: "12px"
      fontFamily: "HelveticaNeue-CondensedBold, Helvetica Neue, Helvetica, Arial, sans-serif"
      fontWeight: "Bold"
   }

   ensure contains( Global.bbox, n.icon, 0. )

   layer n.icon below n.text
   layer n.background below n.icon
   layer n.icon above Global.bbox
   layer n.background above Global.bbox
   layer n.text above Global.bbox
}

forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.text.fillColor = Colors.black
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.red
   override a.icon.strokeColor = Colors.darkRed
   override a.text.fillColor = Colors.white
   override a.text.string = "H"
}

forall Carbon a {
   override a.icon.fillColor = Colors.black
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.white
   override a.text.string = "C"
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.blue
   override a.icon.strokeColor = Colors.darkBlue
   override a.text.fillColor = Colors.white
   override a.text.string = "N"
}

forall Oxygen a {
   override a.icon.fillColor = Colors.white
   override a.icon.strokeColor = Colors.gray
   override a.text.fillColor = Colors.black
   override a.text.string = "O"
}

forall Node n1; Node n2
where SingleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   scalar r = Global.atomRadius

   shape line = Line {
      start : x1 - 1.3*r*u
        end : x2 + 1.3*r*u
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line below n1.icon
   layer line below n2.icon
   layer n1.background below line
   layer n2.background below line
}

forall Node n1; Node n2
where DoubleBond( n1, n2 ) {

   vec2 x1 = n1.center
   vec2 x2 = n2.center
   vec2 u = unit(x1-x2)
   vec2 v = ( -u[1], u[0] )
   scalar r = Global.atomRadius

   shape line1 = Line {
      start : x1 - 1.3*r*u - .25*r*v
        end : x2 + 1.3*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   shape line2 = Line {
      start : x1 - 1.3*r*u + .25*r*v
        end : x2 + 1.3*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.black
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 below n1.icon
   layer line2 below n1.icon
   layer line1 below n2.icon
   layer line2 below n2.icon
   layer n1.background below line1
   layer n2.background below line1
   layer n1.background below line2
   layer n2.background below line2
}

forall Node n1; Node n2 {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )
   layer n1.background below n2.icon
   layer n2.background below n1.icon
}

`,l=`-- structural-formula.domain
--
-- This Penrose Domain schema is used to encode molecular
-- structures in a format suitable for drawing a variety
-- of different kinds of molecular structure diagrams.

-- a Node is any collection of atoms that is treated
-- as a single logical unit, such as a functional
-- group (or just a single atom)
type Node

-- a FunctionalGroup represents a collection of atoms
-- in the same molecule (such as an alcohol or ester)
type FunctionalGroup <: Node

-- an Atom represents a single atom within a larger
-- molecule (or as an isolated ion)
type Atom <: Node

-- specific types of atoms (more could be added here)
type Hydrogen <: Atom
type   Carbon <: Atom
type Nitrogen <: Atom
type   Oxygen <: Atom
type   Sodium <: Atom
type Chlorine <: Atom

-- predicates used to specify bonds between Nodes
predicate SingleBond(Node n1, Node n2)
predicate DoubleBond(Node n1, Node n2)
predicate  IonicBond(Node n1, Node n2)

-- a Molecule is a collection of Atoms, or more generally,
-- Nodes, held together by bonds.  It is not essential to
-- annotate which collections of Nodes are connected, but
-- grouping Nodes is helpful for, e.g., labeling molecules
-- and/or grouping reactants/products.
type Molecule

predicate Contains(Molecule m, Node n)

-- these predicates are used to delineate reactants and
-- produces in a chemical equation
predicate IsReactant(Molecule m)
predicate IsProduct(Molecule m)

-- a reaction involving all reactants and products
type Reaction

-- predicates to mark the type of reaction
predicate     IsNetForward(Reaction r)
predicate IsStoichiometric(Reaction r)
predicate    IsEquilibrium(Reaction r)
predicate  IsBidirectional(Reaction r)

`,t={substance:e,style:[{contents:r,resolver:o}],domain:l,variation:""};export{t as default};
