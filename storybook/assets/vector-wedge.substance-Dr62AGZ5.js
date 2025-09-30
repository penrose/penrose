import{_ as a,__tla as d}from"./iframe-BHpLOgMN.js";let i,n,s,u,c,m=Promise.all([(()=>{try{return d}catch{}})()]).then(async()=>{let l;n=`type Scalar

-- define vectors and bivectors as subtypes of a base k-vector type
type kVector
type Vector <: kVector
type Bivector <: kVector

-- some basic operations on k-vectors
function Scale( Scalar, Vector ) -> Vector
function Add( Vector, Vector ) -> Vector
function Wedge( Vector, Vector ) -> Bivector

-- (more can be added here ) --
`,l=(r,e)=>{if(e.startsWith("/"))return e;const t=r.split("/");for(const o of e.split("/"))o===".."?t.pop():o!=="."&&t.push(o);return t.join("/")},s=r=>async e=>{switch(l(r,e)){case"alloy-models/river-crossing/character-svgs/chicken.svg":return(await a(async()=>{const{default:t}=await import("./chicken.svg-56B-zJC9.js");return{default:t}},[],import.meta.url)).default;case"alloy-models/river-crossing/character-svgs/farmer.svg":return(await a(async()=>{const{default:t}=await import("./farmer.svg-ChBrqwtd.js");return{default:t}},[],import.meta.url)).default;case"alloy-models/river-crossing/character-svgs/fox.svg":return(await a(async()=>{const{default:t}=await import("./fox.svg-DTUibzpv.js");return{default:t}},[],import.meta.url)).default;case"alloy-models/river-crossing/character-svgs/grain.svg":return(await a(async()=>{const{default:t}=await import("./grain.svg-IQxHB8cD.js");return{default:t}},[],import.meta.url)).default;case"curve-examples/catmull-rom/catmull-rom.svg":return(await a(async()=>{const{default:t}=await import("./catmull-rom.svg-C4ycy2Hk.js");return{default:t}},[],import.meta.url)).default;case"dinoshade/dinoshade-vectorized.svg":return(await a(async()=>{const{default:t}=await import("./dinoshade-vectorized.svg-CwZ8MkzE.js");return{default:t}},[],import.meta.url)).default;case"geometric-queries/closest-point/closest-point-examples.svg":return(await a(async()=>{const{default:t}=await import("./closest-point-examples.svg-BHxDql41.js");return{default:t}},[],import.meta.url)).default;case"geometric-queries/closest-silhouette-point/closest-silhouette-point-example.svg":return(await a(async()=>{const{default:t}=await import("./closest-silhouette-point-example.svg-THSzyXP8.js");return{default:t}},[],import.meta.url)).default;case"geometric-queries/geometric-queries-example.svg":return(await a(async()=>{const{default:t}=await import("./geometric-queries-example.svg-COVH8sVq.js");return{default:t}},[],import.meta.url)).default;case"geometric-queries/ray-intersect/ray-intersect-examples.svg":return(await a(async()=>{const{default:t}=await import("./ray-intersect-examples.svg-BDCpUsNb.js");return{default:t}},[],import.meta.url)).default;case"hypergraph/example.svg":return(await a(async()=>{const{default:t}=await import("./example.svg-CVEkLGxv.js");return{default:t}},[],import.meta.url)).default;case"impossible-ngon/nsides-chirality.svg":return(await a(async()=>{const{default:t}=await import("./nsides-chirality.svg-Ch_APDsM.js");return{default:t}},[],import.meta.url)).default;case"impossible-ngon/parameters.svg":return(await a(async()=>{const{default:t}=await import("./parameters.svg-CEUVvfSD.js");return{default:t}},[],import.meta.url)).default;case"impossible-ngon/radial-gradient.svg":return(await a(async()=>{const{default:t}=await import("./radial-gradient.svg-BtjMVzU7.js");return{default:t}},[],import.meta.url)).default;case"lagrange-bases/example.svg":return(await a(async()=>{const{default:t}=await import("./example.svg-BYqRYtbg.js");return{default:t}},[],import.meta.url)).default;case"matrix-library/test-example.svg":return(await a(async()=>{const{default:t}=await import("./test-example.svg-pR2yO6FG.js");return{default:t}},[],import.meta.url)).default;case"mobius/background.svg":return(await a(async()=>{const{default:t}=await import("./background.svg-D-qyyBIg.js");return{default:t}},[],import.meta.url)).default;case"mobius/sphere-shading.svg":return(await a(async()=>{const{default:t}=await import("./sphere-shading.svg-0CTN8eB6.js");return{default:t}},[],import.meta.url)).default;case"penrose-sound/penrose-sound-example.svg":return(await a(async()=>{const{default:t}=await import("./penrose-sound-example.svg-DUTclhOu.js");return{default:t}},[],import.meta.url)).default;case"persistent-homology/example.svg":return(await a(async()=>{const{default:t}=await import("./example.svg-y03oCSFZ.js");return{default:t}},[],import.meta.url)).default;case"persistent-homology/persistent-homology-shading.svg":return(await a(async()=>{const{default:t}=await import("./persistent-homology-shading.svg-Bj2tamfj.js");return{default:t}},[],import.meta.url)).default;case"ray-tracing/lightbulb.svg":return(await a(async()=>{const{default:t}=await import("./lightbulb.svg-CUUTgwu4.js");return{default:t}},[],import.meta.url)).default;case"ray-tracing/ray-tracing-examples.svg":return(await a(async()=>{const{default:t}=await import("./ray-tracing-examples.svg-tC1Eyss1.js");return{default:t}},[],import.meta.url)).default;case"set-theory-domain/set-theory-domain-shadow.svg":return(await a(async()=>{const{default:t}=await import("./set-theory-domain-shadow.svg-zYleTdTV.js");return{default:t}},[],import.meta.url)).default;case"set-theory-domain/shading.svg":return(await a(async()=>{const{default:t}=await import("./shading.svg-JSeLmSOe.js");return{default:t}},[],import.meta.url)).default;case"stochastic-process/ball-shading.svg":return(await a(async()=>{const{default:t}=await import("./ball-shading.svg-CZk8Zx21.js");return{default:t}},[],import.meta.url)).default;case"structural-formula/background.svg":return(await a(async()=>{const{default:t}=await import("./background.svg-Dq-RvFqv.js");return{default:t}},[],import.meta.url)).default;case"structural-formula/node-shading.svg":return(await a(async()=>{const{default:t}=await import("./node-shading.svg-BVC0KIZV.js");return{default:t}},[],import.meta.url)).default;case"structural-formula/structural-formula-atom.svg":return(await a(async()=>{const{default:t}=await import("./structural-formula-atom.svg-vQQbnv9g.js");return{default:t}},[],import.meta.url)).default;case"walk-on-spheres/ball-shading.svg":return(await a(async()=>{const{default:t}=await import("./ball-shading.svg-C4J024Pg.js");return{default:t}},[],import.meta.url)).default;case"walk-on-spheres/images/wos-laplace-estimator-walk-on-spheres.svg":return(await a(async()=>{const{default:t}=await import("./wos-laplace-estimator-walk-on-spheres.svg-DH59Hio6.js");return{default:t}},[],import.meta.url)).default;case"walk-on-spheres/images/wos-nested-estimator-walk-on-spheres.svg":return(await a(async()=>{const{default:t}=await import("./wos-nested-estimator-walk-on-spheres.svg-mtJhRwcO.js");return{default:t}},[],import.meta.url)).default;case"walk-on-spheres/images/wos-offcenter-estimator-walk-on-spheres.svg":return(await a(async()=>{const{default:t}=await import("./wos-offcenter-estimator-walk-on-spheres.svg-0jzfe7p4.js");return{default:t}},[],import.meta.url)).default;case"walk-on-spheres/images/wos-poisson-estimator-walk-on-spheres.svg":return(await a(async()=>{const{default:t}=await import("./wos-poisson-estimator-walk-on-spheres.svg-CHkOhcvh.js");return{default:t}},[],import.meta.url)).default;default:return}},u=s("exterior-algebra"),i=`-- define the size of the drawing
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

`,c=`Vector a, b, c
Bivector u := Wedge(a,b)
Bivector v := Wedge(b,c)

AutoLabel All
Label u $a \\wedge b$
Label v $b \\wedge c$
`});export{m as __tla,i as a,n as d,s as m,u as r,c as s};
