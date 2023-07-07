import{a as n}from"./index-3083d8fe.js";const o=n("ray-tracing"),r=`-- first sample/compute the path, then place labels
layout = [ tracingStage, labelingStage ]

canvas {
   width = 240
   height = 200
}

global {
   scalar pointSize = 2

   scalar arrowScale = 15.0
   scalar arrowThickness = 1.0
   string arrowShape = "straight"
   string arrowSize = .5

   string rayLineDashing = "4,3"
   color rayLineColor = #000
   scalar rayLineWidth = 0.65

   scalar eps = 0.1 -- offset distance (to prevent ray from intersecting origin)

   color sceneColor = #ddd
   color sceneStrokeColor = #aaa
}

colors {
   color white = #ffffff
}

-- Define the scene geometry.  You can uncomment one of the blocks
-- below, or design your own!  Currently Penrose does not support direct
-- import of SVG data for geometry that will be used in computation/optimization,
-- but if you want to extract a shape from a particular SVG polygon you can
-- open the .svg file and copy/paste the "points" attribute into the \`points:\`
-- field of a polygon (paying attention to formatting).
forall Scene S {

   --------- Rectangular scene ---------
   -- shape S.geometry = Rectangle {
   --    center: (0,0)
   --    width: canvas.width - 30
   --    height: canvas.height - 30
   --    fillColor: global.sceneColor
   --    strokeColor: global.sceneStrokeColor
   --    strokeWidth: 1
   --    ensureOnCanvas: false
   -- }

   --------- Polygonal scene ------------
   shape S.geometry = Polygon {
      points: [(-97,75.3), (-110.5,28.8), (-89.2,-93.1), (104.7,-84.1), (104.7,75.3), (43.8,95.7), (2.9,18.6), (28.5,-18.4), (-4.9,-44.8), (-41.2,6.9), (4.4,42.3), (-36.1,90.9)]
      fillColor: global.sceneColor
      strokeColor: global.sceneStrokeColor
      strokeWidth: 1
      ensureOnCanvas: false
   }

   --------- Randomized polygonal scene ------------
   -- scalar maxPerturbation = 15
   -- vec2 p0 = maxPerturbation*diskRandom()
   -- vec2 p1 = maxPerturbation*diskRandom()
   -- vec2 p2 = maxPerturbation*diskRandom()
   -- vec2 p3 = maxPerturbation*diskRandom()
   -- vec2 p4 = maxPerturbation*diskRandom()
   -- vec2 p5 = maxPerturbation*diskRandom()
   -- scalar w = .8*canvas.width/2
   -- scalar h = .8*canvas.height/2
   -- shape S.geometry = Polygon {
   --    points: ( (w,-h)+p0, (0,-h)+p1, (-w,-h)+p2, (-w,h)+p3, (0,h)+p4, (w,h)+p5)
   --    fillColor: global.sceneColor
   --    strokeColor: global.sceneStrokeColor
   --    strokeWidth: 1
   --    ensureOnCanvas: false
   -- }

   --------- Elliptical scene with holes ---------
   -- shape S.g1 = Ellipse {
   --    center: (0,0)
   --    rx: canvas.width/2.
   --    ry: canvas.height/2.
   --    fillColor: global.sceneColor
   --    strokeColor: global.sceneStrokeColor
   --    strokeWidth: 1
   --    ensureOnCanvas: false
   -- }
   -- shape S.g2 = Circle {
   --    center: (canvas.width/4,0)
   --    r: canvas.width/8.
   --    fillColor: #fff
   --    strokeColor: global.sceneStrokeColor
   --    strokeWidth: 1
   --    ensureOnCanvas: false
   -- }
   -- shape S.g3 = Circle {
   --    center: (-canvas.width/4,0)
   --    r: canvas.width/8.
   --    fillColor: #fff
   --    strokeColor: global.sceneStrokeColor
   --    strokeWidth: 1
   --    ensureOnCanvas: false
   -- }
   -- shape S.geometry = Group {
   --    shapes: [S.g1,S.g2,S.g3]
   -- }
}

-- Draw each point as a black dot, and make sure that
-- it's drawn on top of the scene geometry.  Each point
-- will also keep track of a normal, which will be zero
-- if the point is on the scene interior (rather than its boundary).
forall Point p; Scene S {
   vec2 p.x = (?,?) -- location
   vec2 p.n = (0,0) -- normal

   shape p.icon = Circle {
      center: p.x
      r: global.pointSize
      fillColor: #000
      ensureOnCanvas: false
   }

   layer p.icon above S.geometry
}

-- If a point has a label, render this label as TeX
forall Point p
where p has math label {
   shape p.labelText = Equation {
      string: p.label
      ensureOnCanvas: false
      fontSize: "14px"
      fillColor: #000
      strokeColor: "#ffffff"
      strokeWidth: 2.5
      paintOrder: "stroke"
      strokeLinejoin: "round"
   }

   -- in the labeling stage of layout, try to put the label near the point
   encourage norm( p.labelText.center - p.x ) == 10 in labelingStage

   -- draw the label in front of the dot, which is in turn in front
   -- of other things like the scene geometry
   layer p.labelText above p.icon
}

-- Draw each ray as a little arrow.  Each ray keeps track of its
-- origin and direction.  Later, if rays are used to compute intersections,
-- we will also draw a line along the ray of intersection.
forall Ray r {
   vec2 r.o = (?,?) -- origin
   vec2 r.d = circleRandom() -- direction

   shape r.icon = Line {
      start: r.o
      end: r.o + global.arrowScale*r.d
      strokeColor: #000
      strokeWidth: global.arrowThickness
      endArrowhead: global.arrowShape
      endArrowheadSize: global.arrowSize
      ensureOnCanvas: false
   }
}

-- Draw each light as a copy of the file \`lightbulb.svg\`.
-- (Feel free to use your own custom icon here!)
forall Light L {
   vec2 L.x = (?,?) -- location

   shape L.icon = Image {
      center: L.x
      width: 27
      height: 41
      href: "lightbulb.svg"
      ensureOnCanvas: false
   }
}

-- Make sure the light is drawn in front of the scene geometry
forall Light L; Scene S {
   layer L.icon above S.geometry
}

-- Draw a little icon for each camera.  This time, we'll draw the icon
-- using Style directly, just to show how it's done.  But in practice
-- you may find it easier to just import an SVG (as done for lights).
forall Camera C {

   vec2 C.x = (?,?) -- center location

   shape body = Rectangle {
      center: C.x
      width: 1.5*16
      height: 1.5*11
      fillColor: #aaa
      strokeColor: #666
      strokeWidth: 1
      cornerRadius: 2
      ensureOnCanvas: false
   }
   shape viewfinder = Rectangle {
      center: C.x + (0,body.height/2)
      width: .4*body.width
      height: .3*body.height
      fillColor: body.strokeColor
      cornerRadius: 2
      ensureOnCanvas: false
   }
   shape outerLens = Circle {
      center: C.x
      r: .38*body.height
      fillColor: #ccc
      strokeColor: body.strokeColor
      strokeWidth: body.strokeWidth
      ensureOnCanvas: false
   }
   shape C.innerLens = Circle {
      center: C.x
      r: .25*body.height
      fillColor: #227788
      strokeColor: body.strokeColor
      strokeWidth: body.strokeWidth
      ensureOnCanvas: false
   }
   shape flash = Circle {
      center: C.x + (body.width,body.height)/2 - (3,3)
      r: .07*body.height
      fillColor: #fff
      strokeColor: body.strokeColor
      strokeWidth: .8*body.strokeWidth
      ensureOnCanvas: false
   }
   shape shutterRelease = Rectangle {
      center: C.x + (-body.width/3,body.height/2)
      width: .1*body.width
      height: .15*body.height
      fillColor: body.strokeColor
      ensureOnCanvas: false
   }

   layer viewfinder below body
   layer shutterRelease below body

   shape C.icon = Group {
      shapes: [viewfinder,body,outerLens,C.innerLens,flash,shutterRelease]
      ensureOnCanvas: false
   }
}

-- Make sure the camera is placed somewhere in the scene, with some padding
forall Scene S; Camera C {
   layer C.icon above S.geometry
   ensure signedDistance( S.geometry, C.x ) < -20
}

-- Make sure the light is placed somewhere in the scene, with some padding
forall Scene S; Light L {
   layer L.icon above S.geometry
   constraint L.inScene = ensure signedDistance( S.geometry, L.x ) < -20
}

-- If a ray r comes from a point p, put the ray's origin at that point
forall Ray r; Point p
where r := rayFrom(p) {
   override r.o = p.x
}

-- If we're shooting a ray from a point on the boundary,
-- sample the ray direction from the hemisphere around the
-- surface normal.
forall Ray r0; Ray r1; Point p; Scene S
where p := intersect(r0,S); r1 := rayFrom(p) {
   scalar thetaMax = .9*MathPI()/2
   scalar theta = thetaMax*random(-1,1)
   override r1.d = rotateBy( p.n, theta )
}

-- If a ray hits a specular point, set the ray direction to
-- the normal direction, and draw a mirror icon.
-- a mirror icon.
forall Ray r0; Ray r1; Point p; Scene S
where p := intersect(r0,S); r1 := rayFrom(p); isSpecular(p) {
   override r1.d = p.n

   scalar w = 15
   scalar h = 2
   vec2 t = rot90(p.n)
   vec2 m00 = p.x - w*t - h*p.n
   vec2 m01 = p.x - w*t + h*p.n
   vec2 m10 = p.x + w*t - h*p.n
   vec2 m11 = p.x + w*t + h*p.n

   shape mirror = Polygon {
      points: [m00,m01,m11,m10]
      fillColor: #ccccff
      strokeColor: #bbbbff
      strokeWidth: 1
      ensureOnCanvas: false
   }
}

-- If a point p is defined by a ray-scene intersection, shoot
-- a ray and update p's position and normal.  Also draw a line
-- along the ray, and the normal at the hit point.
forall Point p; Ray r; Scene S
where p := intersect(r,S) {
   vec2 o = r.o + global.eps*r.d
   override p.x = rayIntersect(S.geometry, o, r.d)
   override p.n = rayIntersectNormal(S.geometry, o, r.d)

   shape r.rayLine = Line {
      start: r.o
      end: p.x
      fill: "none"
      strokeColor: global.rayLineColor
      strokeWidth: global.rayLineWidth
      ensureOnCanvas: false
   }

   shape normalArrow = Line {
      start: p.x
      end: p.x + global.arrowScale*p.n
      strokeColor: #5a5
      strokeWidth: global.arrowThickness
      endArrowhead: global.arrowShape
      endArrowheadSize: global.arrowSize
      ensureOnCanvas: false
   }

   layer r.rayLine above S.geometry
   layer normalArrow above S.geometry
   layer p.icon above normalArrow
}

-- If a ray is supposed to hit a light, we cheat: rather
-- than try to sample the whole path so that it hits the
-- light at this moment, just move the light to some point
-- along the final ray!  (In principle the same effect could
-- also be achieved via optimization, e.g., if the light
-- needs to be placed in one particular location in the diagram.)
forall Ray r; Light L; Scene S
where hitsLight(r,L) {

   -- find the first hit point y along ray r
   vec2 o = r.o + global.eps*r.d -- offset by epsilon to avoid hitting the origin
   vec2 y = rayIntersect(S.geometry, o, r.d)

   -- place the light halfway between the hit point and the ray origin
   -- (just so it's not too close to the scene boundary on either end)
   vec2 z = (y+o)/2
   override L.x = z

   -- Since the light is now inside the scene by construction,
   -- we no longer need the constraint that ensures it's inside
   -- the scene geometry.  (Keeping this constraint can cause
   -- some funny behavior from the optimizer, since it's trying
   -- to optimize a point that's now fixed to a definite location.)
   delete L.inScene

   shape r.rayLine = Line {
      start: r.o
      end: z
      fill: "none"
      strokeColor: global.rayLineColor
      strokeWidth: global.rayLineWidth
      ensureOnCanvas: false
   }

   layer r.rayLine above S.geometry
}

-- Draw direct connections between points, lights, and/or cameras
-- as a dashed line (e.g., for connections between eye and light
-- subpaths in bidirectional path tracing).  If this connection
-- passes through the scene geometry, draw a red "X" at the first
-- intersection point.
forall Ray r; Entity p; Entity q; Scene S
where r := rayBetween(p,q) {
   override r.o = p.x
   override r.d = unit(q.x-p.x)
   shape r.rayLine = Line {
      start: p.x
      end: q.x
      fill: "none"
      strokeColor: global.rayLineColor
      strokeWidth: global.rayLineWidth
      ensureOnCanvas: false
      style: "dashed"
      strokeDasharray: global.rayLineDashing
   }
   layer r.rayLine above S.geometry

   -- don't draw an outgoing ray direction for a direct connection
   override r.icon.end = r.icon.start
   override r.icon.endArrowhead = "none"

   -- check if there are any intersections along this ray,
   -- and draw a red "X" at the first intersection if so
   vec2 v = unit(q.x-p.x)
   vec2 y = rayIntersect(S.geometry, p.x + global.eps*v, v)
   scalar dy = norm(y-p.x)
   scalar dq = norm(q.x-p.x)
   scalar alpha = min( max(100*(dq-dy),0), 1 ) -- TODO should just expose ifCond in Style
   shape notVisible = Text {
      string: "×"
      center: y
      fillColor: rgba(1,0,0,alpha)
      fontSize: "18px"
      strokeColor: rgba(1,1,1,alpha)
      strokeWidth: 2
      paintOrder: "stroke"
      ensureOnCanvas: false
   }
   layer notVisible above S.geometry
}

-- Make sure a point's label is not obscured by the camera
forall Point p; Camera C
where p has math label {
   layer p.labelText above C.icon
}

-- If a point is on a camera, replace its location with the
-- camera's location, and hide its icon below the camera's icon.
-- Also set its normal to zero, since it's now an interior point.
forall Point p; Camera C
where onCamera(p,C) {
   override p.x = C.x
   override p.n = (0,0)
   layer p.icon below C.icon
}

-- If a point is on a light, replace its location with the
-- camera's location, and hide its icon below the camera's icon.
-- Also set its normal to zero, since it's now an interior point.
forall Point p; Light L
where onLight(p,L) {
   override p.x = L.x
   override p.n = (0,0)
   layer p.icon below L.icon
}

-- Draw ray arrows behind any camera
forall Ray r; Camera C {
   layer r.icon below C.icon
   layer r.rayLine below C.icon
}

-- Draw ray arrows behind any light
forall Ray r; Light L {
   layer r.icon below L.icon
   layer r.rayLine below L.icon
}

-- If a point is on the scene boundary, a good place to put its
-- label is outside the scene, offset in the normal direction away
-- from the point.
forall Scene S; Point p; Ray r
where p := intersect(r,S); p has label {
   override p.labelText.center = p.x - 10*p.n
}

`,a=`type Entity
type Scene
type Light <: Entity
type Camera <: Entity
type Point <: Entity
type Ray

constructor rayFrom(Point p) -> Ray
constructor rayBetween(Entity p, Entity q) -> Ray
constructor intersect(Ray r, Scene S) -> Point

predicate onCamera(Point p, Camera C)
predicate onLight(Point p, Light L)
predicate hitsLight(Ray r, Light L)
predicate isSpecular(Point p)

`;export{a as d,o as r,r as s};
