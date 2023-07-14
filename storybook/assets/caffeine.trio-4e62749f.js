import{m as n}from"./resolver-7739be03.js";import{d as e}from"./structural-formula.domain-614d22a0.js";import"./iframe-f92d5283.js";const o=`-- caffeine molecule, expressed via structural-formula DSL
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

Title title
Label title "caffeine"
`,r=n("structural-formula"),l=`canvas {
   scalar width  = 800
   scalar height = 800
}

global {
   shape background = Image {
      href: "background.svg"
      center: (0,0)
      width: canvas.width
      height: canvas.height
      preserveAspectRatio: "none"
      ensureOnCanvas: false
   }
}

Colors {
   scalar a = .4
   vec4 clear           = rgba( 0., 0., 0., 0. )
   vec4 black           = rgba( 0., 0., 0., 1. )
   vec4 clearblack      = rgba( a, a, a, 1. )
   vec4 gray            = rgba( .5, .5, .5, 1. )
   vec4 cleargray       = rgba( .8, .8, .8, 1. )
   vec4 lightGray       = rgba( .9, .9, .9, 1. )
   vec4 white           = rgba( 1., 1., 1., 1. )
   vec4 red             = rgba( 1., 0., 0., 1. )
   vec4 clearred        = rgba( 1., a, a, 1. )
   vec4 green           = rgba( 0., .7, 0., 1. )
   vec4 cleargreen      = rgba( a, .7, a, 1. )
   vec4 blue            = rgba( 0., 0., 1., 1. )
   vec4 clearblue       = rgba( a, a, 1., 1. )
   vec4 darkRed         = rgba( .7, 0., 0., 1. )
   vec4 darkBlue        = rgba( 0., 0., .7, 1. )
   vec4 purple          = rgba( .66, .36, .95, 1. )
   vec4 clearpurple     = rgba( .86, .56, 1., 1. )
   vec4 turquoise       = rgba( .1, .7, .6, 1. )
}

Global {
   scalar atomRadius = 25.
   scalar bondLength = 60.

   scalar padding = 100.
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

   shape n.shading = Image {
      href: "node-shading.svg"
      center: n.icon.center
      width: 2*n.icon.r - n.icon.strokeWidth/2
      height: 2*n.icon.r - n.icon.strokeWidth/2
      ensureOnCanvas: false
   }

   shape n.shadow = Ellipse {
      rx : 2.*Global.atomRadius
      ry : Global.atomRadius
      center : (cx,cy) + (0.,-2.*R)
      fillColor : rgba( 0, 0, 0, 1 )
      strokeColor : Colors.clear
      strokeWidth : 3.
   }

   layer n.shading above n.icon
   layer n.icon above n.shadow
   layer n.shadow above global.background
}

collect Node n into nodes {

   shadows = listof shadow from nodes
   
   shape nodeShadows = Group {
      shapes: shadows
      opacity: .1
      style: "filter:blur(5px);"
   }
}

forall Node n
where n has label {
   shape n.labelText = Equation {
      string : n.label
      center : n.center
      fillColor : Colors.black
      fontSize : "18px"
      ensureOnCanvas: false
   }

   layer n.icon below n.labelText
}

forall FunctionalGroup g {
   override g.icon.fillColor = Colors.clear
   override g.icon.strokeColor = Colors.clear
   override g.labelText.fillColor = Colors.black
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


forall FunctionalGroup g
where g has label {
   layer g.labelText above g.box
}

forall Oxygen a {
   override a.icon.fillColor = Colors.clearred
   override a.icon.strokeColor = Colors.red
   override a.labelText.fillColor = none()
}

forall Carbon a {
   override a.icon.fillColor = Colors.clearblack
   override a.icon.strokeColor = Colors.black
   override a.labelText.fillColor = none()
}

forall Nitrogen a {
   override a.icon.fillColor = Colors.clearblue
   override a.icon.strokeColor = Colors.blue
   override a.labelText.fillColor = none()
}

forall Hydrogen a {
   override a.icon.fillColor = Colors.cleargray
   override a.icon.strokeColor = Colors.gray
   override a.labelText.fillColor = none()

   -- make hydrogen atoms (and their shadows) smaller
   override a.icon.r = .75*Global.atomRadius
   override a.shadow.rx = .75*2.*Global.atomRadius
   override a.shadow.ry = .75*Global.atomRadius
}

forall Chlorine a {
   override a.icon.fillColor = Colors.cleargreen
   override a.icon.strokeColor = Colors.green
   override a.labelText.fillColor = none()
}

forall Sodium a {
   override a.icon.fillColor = Colors.clearpurple
   override a.icon.strokeColor = Colors.purple
   override a.labelText.fillColor = none()
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
      strokeColor : Colors.gray
      strokeLinecap: "round"
   }

   shape innerLine = Line {
      start : x1 - .5*r*u
        end : x2 + .5*r*u
      strokeWidth : 4.
      strokeColor : Colors.lightGray
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line above n2.shading
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
      strokeColor : Colors.gray
      strokeLinecap: "round"
   }

   shape innerLine1 = Line {
      start : x1 - .5*r*u - .25*r*v
        end : x2 + .5*r*u - .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.lightGray
      strokeLinecap: "round"
   }

   shape line2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 10.
      strokeColor : Colors.gray
      strokeLinecap: "round"
   }

   shape innerLine2 = Line {
      start : x1 - .5*r*u + .25*r*v
        end : x2 + .5*r*u + .25*r*v
      strokeWidth : 4.
      strokeColor : Colors.lightGray
      strokeLinecap: "round"
   }

   encourage equal( norm(x1-x2), Global.bondLength )

   layer line1 above n2.shading
   layer innerLine1 above line1
   layer innerLine1 below n1.icon

   layer line2 above n2.shading
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

-- give water molecules a physical bond angle
forall Oxygen o; Hydrogen h1; Hydrogen h2
where SingleBond(o,h1); SingleBond(o,h2) {
   vec2 a = o.center
   vec2 b = h1.center
   vec2 c = h2.center
   encourage equal( angleBetween(b-a,c-a), toRadians(104.5) )
}

-- use a Coulomb-like force to prevent nodes from overlapping
forall Node n1; Node n2 {
   vec2 x1 = n1.center
   vec2 x2 = n2.center
   encourage equal( 200000./normsq(x1-x2), 0. )

   -- make sure shadows are drawn below all molecules
   layer n1.shadow below n2.icon
   layer n2.shadow below n1.icon
}

forall Title t
where t has label
{
   shape t.labelText = Text {
      string : t.label
      fillColor : Colors.black
      fontSize : "40px"
      fontFamily: "HelveticaNeue-CondensedBold, Helvetica Neue, Helvetica, Arial, sans-serif"
      fontWeight: "bold"
      center : (?,?)
   }
   layer t.labelText above global.background
}

forall Title t; Node n
where t has label {
   ensure disjoint( t.labelText, n.icon )
}

forall Molecule m; Node n
where Contains(m,n) {
   encourage near( m.box, n.icon )
   layer m.labelText above n.icon
}

`,s={substance:o,style:[{contents:l,resolver:r}],domain:e,variation:"GreybeardChinchilla991",excludeWarnings:[]};export{s as default};
//# sourceMappingURL=caffeine.trio-4e62749f.js.map
