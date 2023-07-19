var s=Object.defineProperty;var n=(a,t)=>s(a,"name",{value:t,configurable:!0});import{o as e}from"./iframe-57848583.js";const m=`type Scalar

-- define vectors and bivectors as subtypes of a base k-vector type
type kVector
type Vector <: kVector
type Bivector <: kVector

-- some basic operations on k-vectors
function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector

-- (more can be added here ) --
`,i=n((a,t)=>{if(t.startsWith("/"))return t;const r=a.split("/");for(const o of t.split("/"))o===".."?r.pop():o!=="."&&r.push(o);return r.join("/")},"join"),l=n(a=>async t=>{switch(i(a,t)){case"curve-examples/catmull-rom/catmull-rom.svg":return(await e(()=>import("./catmull-rom.svg-dec68f87.js"),[],import.meta.url)).default;case"geometric-queries/closest-point/closest-point-examples.svg":return(await e(()=>import("./closest-point-examples.svg-ba672853.js"),[],import.meta.url)).default;case"geometric-queries/closest-silhouette-point/closest-silhouette-point-example.svg":return(await e(()=>import("./closest-silhouette-point-example.svg-139d0076.js"),[],import.meta.url)).default;case"geometric-queries/geometric-queries-example.svg":return(await e(()=>import("./geometric-queries-example.svg-593d0132.js"),[],import.meta.url)).default;case"geometric-queries/ray-intersect/ray-intersect-examples.svg":return(await e(()=>import("./ray-intersect-examples.svg-11de9b11.js"),[],import.meta.url)).default;case"hypergraph/example.svg":return(await e(()=>import("./example.svg-b469d428.js"),[],import.meta.url)).default;case"lagrange-bases/example.svg":return(await e(()=>import("./example.svg-83d50d93.js"),[],import.meta.url)).default;case"mobius/background.svg":return(await e(()=>import("./background.svg-b3688b01.js"),[],import.meta.url)).default;case"mobius/sphere-shading.svg":return(await e(()=>import("./sphere-shading.svg-c4cf2b2e.js"),[],import.meta.url)).default;case"penrose-sound/penrose-sound-example.svg":return(await e(()=>import("./penrose-sound-example.svg-c76d28a5.js"),[],import.meta.url)).default;case"persistent-homology/example.svg":return(await e(()=>import("./example.svg-7e5521c0.js"),[],import.meta.url)).default;case"persistent-homology/persistent-homology-shading.svg":return(await e(()=>import("./persistent-homology-shading.svg-004be5f1.js"),[],import.meta.url)).default;case"ray-tracing/lightbulb.svg":return(await e(()=>import("./lightbulb.svg-369ad0eb.js"),[],import.meta.url)).default;case"ray-tracing/ray-tracing-examples.svg":return(await e(()=>import("./ray-tracing-examples.svg-0fce5f9c.js"),[],import.meta.url)).default;case"set-theory-domain/set-theory-domain-shadow.svg":return(await e(()=>import("./set-theory-domain-shadow.svg-b7a20a17.js"),[],import.meta.url)).default;case"set-theory-domain/shading.svg":return(await e(()=>import("./shading.svg-806ab889.js"),[],import.meta.url)).default;case"stochastic-process/ball-shading.svg":return(await e(()=>import("./ball-shading.svg-bf3a9326.js"),[],import.meta.url)).default;case"structural-formula/background.svg":return(await e(()=>import("./background.svg-2adafeec.js"),[],import.meta.url)).default;case"structural-formula/node-shading.svg":return(await e(()=>import("./node-shading.svg-a7761123.js"),[],import.meta.url)).default;case"structural-formula/structural-formula-atom.svg":return(await e(()=>import("./structural-formula-atom.svg-fc054676.js"),[],import.meta.url)).default;case"walk-on-spheres/ball-shading.svg":return(await e(()=>import("./ball-shading.svg-5fec5b5c.js"),[],import.meta.url)).default;case"walk-on-spheres/images/wos-laplace-estimator-walk-on-spheres.svg":return(await e(()=>import("./wos-laplace-estimator-walk-on-spheres.svg-5922152b.js"),[],import.meta.url)).default;case"walk-on-spheres/images/wos-nested-estimator-walk-on-spheres.svg":return(await e(()=>import("./wos-nested-estimator-walk-on-spheres.svg-42d8d9be.js"),[],import.meta.url)).default;case"walk-on-spheres/images/wos-offcenter-estimator-walk-on-spheres.svg":return(await e(()=>import("./wos-offcenter-estimator-walk-on-spheres.svg-8d1c1e77.js"),[],import.meta.url)).default;case"walk-on-spheres/images/wos-poisson-estimator-walk-on-spheres.svg":return(await e(()=>import("./wos-poisson-estimator-walk-on-spheres.svg-da763e66.js"),[],import.meta.url)).default;default:return}},"makeResolver"),d=l("exterior-algebra"),p=`-- define the size of the drawing
canvas {
   width = 240
   height = 180
}

-- define some colors re-used throughout
Colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color clearGray = rgba(0,0,0,.2)
}

Global {

   -- draw a box around the canvas (this box will 
   -- also be used to constrain shapes to the canvas)
   shape box = Rectangle {
      center: (0,0)
      width: canvas.width
      height: canvas.height
      fillColor: none()
      strokeColor: Colors.clearGray
      strokeWidth: 1
   }

   -- some additional parameters to get consistent styling throughout
   scalar lineThickness = 1.5
   scalar fontSize = "9px"
   string fontFamily = "Linux Libertine"
}

-- for each Vector declared in the .substance program, this match will
-- get executed once to draw a little widget for that vector, and
-- set up any associated constraints
forall Vector v {

   -- declare a point whose location will be
   -- determined via optimization
   v.p = (?,?)

   -- draw an arrow to p
   v.icon = Line {
      start: (0,0)
      end: v.p
      strokeColor: Colors.black
      strokeWidth: Global.lineThickness
      strokeLinecap: "round"
      endArrowhead: "straight"
      endArrowheadSize: .5
   }
   -- keep the arrow on the canvas
   ensure contains( Global.box, v.icon)
}

-- draw a label for the vector if it has one
forall Vector v
where v has label {
   v.labelText = Equation {
      string: v.label
      center: v.p + 4.*unit(v.p)
      fillColor: Colors.black
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
   }
   -- keep the label on the canvas
   ensure contains( Global.box, v.labelText )
}

-- draw a bivector that has been defined as the wedge of two
-- vectors as a little parallelogram
forall Bivector w; Vector u; Vector v
where w := Wedge(u,v) {

   -- pick a random brightly-saturated color for this bivector
   -- (these colors will be re-used in subsequent rules, which
   -- why we define them up-front and associate them with w)
   scalar w.hue = ?
   color w.solidColor = hsva( w.hue, 100, 80, 1 )
   color w.clearColor = hsva( w.hue, 100, 80, .2 )

   -- draw a parallelogram with sides u,v
   shape w.icon = Path {
      d: pathFromPoints("closed", [ (0,0), u.p, u.p+v.p, v.p ])
      fillColor: w.clearColor
      strokeColor: none()
   }
   -- keep the parallelogram on the canvas
   ensure contains( Global.box, w.icon )

   -- try to make sure the parallelogram is a reasonable size
   scalar area = abs( cross2D( u.p, v.p ))
   scalar canvasArea = canvas.width * canvas.height
   encourage greaterThan( area, canvasArea/10. )

   -- compute the minimum width of the parallelogram
   -- by projecting each vector onto the unit normal
   -- of the other
   vec2 nu = unit( rot90(u.p) )
   vec2 nv = unit( rot90(v.p) )
   scalar wu = abs( dot( nu, v.p ))
   scalar wv = abs( dot( nv, u.p ))
   scalar minWidth = min( wu, wv )

   -- draw an orientation marker
   scalar w.c = (u.p+v.p)/2. -- center
   scalar R = .75 * minWidth/2. -- radius
   vec2 x0 = w.c + R*unit(u.p-v.p) -- arc start
   vec2 x1 = w.c + R*unit(v.p-u.p) -- arc end
   scalar cw = .5*sign( cross2D(u.p,v.p) ) + .5 -- clockwise (1) or not (0)
   shape w.marker = Path {
      d: arc( "open", x0, x1, (R,R), 0., 0, cw )
      fillColor: none()
      strokeColor: w.solidColor
      strokeWidth: .75*Global.lineThickness
      startArrowhead: "straight"
      startArrowheadSize: .5
   }
}

-- draw a label for the bivector if it has one
forall Bivector w
where w has label {
   w.labelText = Equation {
      string: w.label
      center: w.c -- marker center
      fillColor: w.solidColor
      fontSize: Global.fontSize
      fontFamily: Global.fontFamily
   }
}

`,_=`Vector a, b, c
Bivector u := Wedge(a,b)
Bivector v := Wedge(b,c)

AutoLabel All
Label u $a \\wedge b$
Label v $b \\wedge c$
`;export{p as a,m as d,l as m,d as r,_ as s};
//# sourceMappingURL=vector-wedge.substance-91c5fb5e.js.map
