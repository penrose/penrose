import{s as n,d as e}from"./setTheory.domain-b8c37772.js";import{a as o}from"./index-e5d181b5.js";const t=o("set-theory-domain"),r=`canvas {
  width = 800
  height = 700
}


Colors {
   color black = rgba(0.,0.,0.,1.)
   color red = rgba(1.,0.,0.,1.)
   color green = rgba(0.,.7,0.,1.)
   color blue = rgba(0.,0,1.,1.)
   color white = rgba(1.,1.,1.,1.)
   color lightGray = rgba(.8,.8,.8,1.)
}

Global {
   shape box = Rectangle {
      center: (0.,0.)
      fillColor: none()
      strokeColor: Colors.lightGray
      strokeWidth: 2.
      width: canvas.width
      height: canvas.height
   }

   scalar setRadius = 18.
}

forall Set x {

   vec2 x.center = (?,?)

   x.icon = Text {
      center: x.center
      string: x.label
      fontFamily: "Courier"
      fontSize: "20px"
      fontWeight: "bold"
      fillColor: Colors.black
   }

   x.bounds = Circle {
      center: x.center
      r: Global.setRadius
      fillColor: none()
   }
}

forall Set x; Set y {
   -- Try to make sure no labels overlap
   encourage notTooClose(x.bounds, y.bounds, 5.0)
}


forall Set x; Set y
where IsSubset(x, y) {

   vec2 p = x.center
   vec2 q = y.center
   vec2 u = unit(q-p)
   scalar r = Global.setRadius

   arrow = Line {
      start: p + r*u
      end: q - r*u
      strokeWidth : 4.0
      strokeColor : rgba(0.0, 0.0, 0.0, 1.0)
      endArrowhead: "straight"
      endArrowheadSize: .5
   }

   -- Position y above x
   encourage above(y.bounds, x.bounds)

   -- Have sets 'fight' to be aligned with the superset's x-position
   encourage x.bounds.center[0] == y.bounds.center[0]
}

/*
-- TODO: This one currently causes convergence to become very slow but works eventually

Set x1; Set x2
where IsSubset(x1, S); IsSubset(x2, S)
with Set S {
   -- If two sets are direct subsets of the same set, give them the same y-position
   heightFn = ensure sameHeight(x1.bounds, x2.bounds)
}
*/
`,l={substance:n,style:[{contents:r,resolver:t}],domain:e,variation:""};export{l as default};
