# dinoshade

![Shadowy Leapin' Lizards!](dinoshade.png)

This example shows how rich 3D graphics including lighting, shadows, reflections, and perspective projection can be turned into 2D vector diagrams using Penrose, using familiar concepts from standard 3D graphics APIs (such as OpenGL and Direct3D).  The example reimagines the [`dinoshade` OpenGL/GLUT C++ example](http://sgifiles.irixnet.org/sgi/opengl/contrib/mjk/tips/dinoshade.c), originally written by Mark Kilgard at SGI.  If you've never written 3D graphics code before, you may want to start with one of the [many terrific OpenGL tutorials](https://www.reddit.com/r/opengl/comments/5dbzp0/how_to_best_learn_opengl_in_20162017/) available online—while Penrose does a lot of things automatically, you do need to still know how to "think in 3D!"  If you do have prior experience with 3D graphics APIs, you should feel quite at home with this example.

## Overview - The Vectorization Pipeline

![Rasterization vs. Vectorization Pipeline](pipeline.png)

The output for this example is shown above: a dinosaur standing on a plane, with soft shadows and a blurry reflection; lighting and shadows are determined by a point light source (which from this view is off screen).  Construction of this example follows a pattern similar to the [real time graphics pipeline](https://en.wikipedia.org/wiki/Graphics_pipeline): the camera is defined via modelview and projection matrices, a list of 3D points is used to describe the scene geometry, and the camera matrices are used to transform these 3D points into 2D coordinates on the canvas.  The key difference, as illustrated in the diagram above, is that in the final stage projected primitives (such as triangles and line segments) are translated into vector graphics primitives, rather than rasterized to final pixel values.  In particular, they are translated into a [standard vector graphics (SVG) file](https://en.wikipedia.org/wiki/SVG), which is a human-readable file format for describing 2D graphics.  An SVG file can then be displayed by a variety of applications such as web browsers (Chrome, Safari, etc.), or specialized vector graphics editors (Inkscape, Adobe Illustrator, etc.).  These programs in effect complete the final stage of image generation, by rasterizing 2D shapes in the SVG file to pixel values that can be displayed by a hardware device.  However, deferring this final rasterization stage has a variety of benefits:

- **Retargetability.**  Since the output of the 3D vectorization pipeline does not assume any definite size for the final rasterized image, graphics can be re-used on a variety of different devices, or in different contexts (e.g., full-screen vs. embedded in a document) without losing fidelity.  In other words, the output representation is "resolution-independent."
- **File Size.** In many cases, a vectorized image is much smaller than a rasterized version, since the former stores a more minimal specification of the image (e.g., just the three corner locations of a triangles, rather than the many thousands of pixel values needed to fill in the same triangle).  E.g., the SVG and PNG versions of the `dinoshade` image above differ in file size by about 5x.
- **Editability.** Unlike raster images, which can be edited only at the level of individual pixels, vectorized 3D graphics easily admit higher-level semantic edits such as "change the color of the dinosaur from green to blue" or "make the wireframe thinner" (or remove it altogether).  Moreover, such alterations can be made programmatically and automatically without needing access to the original runtime environment (e.g., OpenGL or Direct3D).

## The Dinoshade Example

Let's walk through the `dinoshade` example step by step, which is largely contained in the Style program `dinoshade.style`.  Since Style is a [specification language](https://en.wikipedia.org/wiki/Specification_language) rather than a [programming language](https://en.wikipedia.org/wiki/Programming_language), many pieces of this example are much shorter than in the [original C++ example](http://sgifiles.irixnet.org/sgi/opengl/contrib/mjk/tips/dinoshade.c).  In other words, we can focus more on _"what gets drawn"_ rather than _"how to draw it"_.

### Scene Setup

We first need to define some global values that describe our scene, such as the image size, and the configuration of the camera and the lights.  The very first statement in any Style program (which is required) is the output size of the generated image:

```haskell
canvas {
   width = 1920
   height = 1080
}
```

Next, we define a single global camera used by all objects in the scene.  The camera determines how points get projected from 3D to 2D.  In Penrose, a camera is specified using the exact same commands as in OpenGL---here we use the equivalents of [`gluLookAt()`](https://registry.khronos.org/OpenGL-Refpages/gl2.1/xhtml/gluLookAt.xml), [`gluPerspective()`](https://registry.khronos.org/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml), and [`glViewport()`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/glViewport.xhtml):

```haskell
camera {
  -- Camera viewpoint
  -- (same parameters as the gluLookAt() command from OpenGL)
  vec3 eye = (30,30,30) -- where is the camera sitting?
  vec3 center = (0,0,0) -- what is the camera looking at?
  vec3 up = (0,-1,0) -- which direction is "up"?
  mat4x4 model = lookAt( eye, center, up )

  -- Camera projection
  -- (same parameters as the gluPerspective() command from OpenGL)
  scalar fovy = 40 -- field of view in degrees
  scalar aspect = canvas.width / canvas.height -- aspect ratio
  scalar near = 1.0 -- Z near
  scalar far = 40.0 -- Z far
  mat4x4 proj = perspective( fovy, aspect, near, far )

  -- Viewport
  -- (same parameters as the glViewport() command from OpenGL)
  list view = [ -canvas.width/2, -canvas.height/2, canvas.width, canvas.height ]
}
```

For more elaborate diagrams, one could of course define multiple cameras (e.g., declared as Substance variables, each with their own local parameters).

We also have a single global light used by all objects in the scene.  The light influences how polygons are shaded, and determines how our geometry gets projected onto the ground plane to draw a shadow:

```haskell
light {
  -- Light position
  scalar pointLight = 1 -- set to 0 for a directional light "at infinity"
  vec4 pos = ( -10, 30, 10, 1 )

  -- Draw a circle at the light location
  -- (Note that this widget won't be visible from all camera views)
  vec2 q = project( (pos[0],pos[1],pos[2]), camera.model, camera.proj, camera.view )
  shape lightBall = Circle {
     center: q
     r: 10
     fillColor: #ffcc00
     strokeColor: #ff6600
     strokeWidth: 2
  }
}
```

You'll notice here that the light has _four_ coordinates rather than three, following the standard practice in computer graphics and computer vision of using [homogeneous coordinates](https://en.wikipedia.org/wiki/Homogeneous_coordinates#Use_in_computer_graphics_and_computer_vision).  Here for instance, the final coordinate provides the ability to render with a point light (final coordinate 1) or a directional light source "at infinity" (final coordinate 0).  This value is not merely a binary "flag" used to toggle between these options—rather, it directly and naturally factors into the shadow calculations themselves.  More generally, many of the 3D transformation functions in Penrose offer both homogeneous and non-homogeneous versions, as well as conversions to/from homogeneous coordinates, to make standard calculations straightforward to implement.

Finally, we have some global data that we need only define once, such as the title of the diagram, the geometry and shape/shading of the ground plane, and the vertex coordinates of the dinosaur model:

```haskell
global {

   shape title = Text {
      center: (canvas.width/2 - 460,canvas.height/2 - 80)
      string: "Shadowy Leaping Lizards!"
      fontSize: "60pt"
      fontFamily: "Arial Narrow,Arial,sans-serif;"
      fontStyle: "italic"
   }

   -- draw one floor for the whole scene
   color floorColor = rgba( 0.3, 0.3, 0.3, 0.2 )
   list floorPoints = [ ( -20.0, 0.0, 20.0 ), ( 20.0, 0.0, 20.0 ), ( 20.0, 0.0, -20.0 ), ( -20.0, 0.0, -20.0 ) ]
   list p = projectList( floorPoints, camera.model, camera.proj, camera.view )
   shape floor = Polygon {
      points: [ (p[0][0],p[0][1]), (p[1][0],p[1][1]), (p[2][0],p[2][1]), (p[3][0],p[3][1]) ]
      fillColor: global.floorColor
      strokeColor: #bbb
      strokeWidth: 4.0
      ensureOnCanvas: false
   }

   -- geometry used to define the dinosaur mesh
   list dinoPoints = [ (-8, 3, -1.5), (-7, 1, -1.5), (-7, 1, 1.5), (-8, 3, 1.5), (-3, 1, -1.5), (-3, 1, 1.5), (0, 4, -1.5), (0, 4, 1.5), (2, 4, -1.5), (2, 4, 1.5), (3, 5, -1.5), (3, 5, 1.5), (3, 11.5, -1.5), (3, 11.5, 1.5), (5, 12, -1.5), (5, 12, 1.5), (5, 13, -1.5), (5, 13, 1.5), (2, 13.5, -1.5), (2, 13.5, 1.5), (5, 14, -1.5), (5, 14, 1.5), (5, 15, -1.5), (5, 15, 1.5), (3, 16, -1.5), (3, 16, 1.5), (0, 16, -1.5), (0, 16, 1.5), (-1, 15, -1.5), (-1, 15, 1.5), (-1, 13, -1.5), (-1, 13, 1.5), (0, 12, -1.5), (0, 12, 1.5), (-1, 11, -1.5), (-1, 11, 1.5), (-2, 6, -1.5), (-2, 6, 1.5), (-4, 3, -1.5), (-4, 3, 1.5), (-5, 2, -1.5), (-5, 2, 1.5), (-7, 2, -1.5), (-7, 2, 1.5), (0, 10, 1.5), (1, 9, 1.5), (1, 9, 2.25), (0, 10, 2.25), (2, 9, 1.5), (2, 9, 2.25), (5, 8, 1.5), (5, 8, 2.25), (6, 9, 1.5), (6, 9, 2.25), (8, 9, 1.5), (8, 9, 2.25), (7, 9.5, 1.5), (7, 9.5, 2.25), (8, 10, 1.5), (8, 10, 2.25), (7, 10, 1.5), (7, 10, 2.25), (7.5, 11, 1.5), (7.5, 11, 2.25), (6.5, 10, 1.5), (6.5, 10, 2.25), (6, 11, 1.5), (6, 11, 2.25), (6, 10, 1.5), (6, 10, 2.25), (5, 9, 1.5), (5, 9, 2.25), (3, 11, 1.5), (3, 11, 2.25), (1, 11, 1.5), (1, 11, 2.25), (0, 6, 1.5), (0, 4, 3), (0, 6, 3), (1, 3, 1.5), (1, 3, 3), (1, 2, 1.5), (1, 2, 3), (0, 1, 1.5), (0, 1, 3), (0, 0.5, 1.5), (0, 0.5, 3), (1, 0, 1.5), (1, 0, 3), (4, 0, 1.5), (4, 0, 3), (2, 1, 1.5), (2, 1, 3), (2, 2, 1.5), (2, 2, 3), (4, 4, 1.5), (4, 4, 3), (3, 6, 1.5), (3, 6, 3), (2, 7, 1.5), (2, 7, 3), (1, 7, 1.5), (1, 7, 3), (0, 10, -2.25), (1, 9, -2.25), (1, 9, -1.5), (0, 10, -1.5), (2, 9, -2.25), (2, 9, -1.5), (5, 8, -2.25), (5, 8, -1.5), (6, 9, -2.25), (6, 9, -1.5), (8, 9, -2.25), (8, 9, -1.5), (7, 9.5, -2.25), (7, 9.5, -1.5), (8, 10, -2.25), (8, 10, -1.5), (7, 10, -2.25), (7, 10, -1.5), (7.5, 11, -2.25), (7.5, 11, -1.5), (6.5, 10, -2.25), (6.5, 10, -1.5), (6, 11, -2.25), (6, 11, -1.5), (6, 10, -2.25), (6, 10, -1.5), (5, 9, -2.25), (5, 9, -1.5), (3, 11, -2.25), (3, 11, -1.5), (1, 11, -2.25), (1, 11, -1.5), (0, 6, -3), (0, 4, -3), (0, 6, -1.5), (1, 3, -3), (1, 3, -1.5), (1, 2, -3), (1, 2, -1.5), (0, 1, -3), (0, 1, -1.5), (0, 0.5, -3), (0, 0.5, -1.5), (1, 0, -3), (1, 0, -1.5), (4, 0, -3), (4, 0, -1.5), (2, 1, -3), (2, 1, -1.5), (2, 2, -3), (2, 2, -1.5), (4, 4, -3), (4, 4, -1.5), (3, 6, -3), (3, 6, -1.5), (2, 7, -3), (2, 7, -1.5), (1, 7, -3), (1, 7, -1.5), (0.75, 15, -1.6), (1, 14.7, -1.6), (1, 14.7, 1.6), (0.75, 15, 1.6), (1.6, 14.7, -1.6), (1.6, 14.7, 1.6), (2.0999999, 15, -1.6), (2.0999999, 15, 1.6), (1.6, 15.25, -1.6), (1.6, 15.25, 1.6), (1, 15.25, -1.6), (1, 15.25, 1.6) ]
   list dinoNormals = [ (-0.894427, -0.447214, 0.), (0., -1., 0.), (0.707107, -0.707107, 0.), (0., -1., 0.), (0.707107, -0.707107, 0.), (1., 0., 0.), (0.242536, -0.970143, 0.), (1., 0., 0.), (0.164399, 0.986394, 0.), (0.164399, -0.986394, 0.), (1., 0., 0.), (0.447214, 0.894427, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (-1., 0., 0.), (-0.707107, -0.707107, 0.), (-0.707107, 0.707107, 0.), (-0.980581, 0.196116, 0.), (-0.83205, 0.5547, 0.), (-0.707107, 0.707107, 0.), (0., 1., 0.), (0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.), (-0.707107, -0.707107, 0.), (0., -1., 0.), (-0.316228, -0.948683, 0.), (0.707107, -0.707107, 0.), (0., -1., 0.), (0.447214, 0.894427, 0.), (0.447214, -0.894427, 0.), (0., 1., 0.), (0.894427, -0.447214, 0.), (-0.707107, 0.707107, 0.), (0.894427, 0.447214, 0.), (-1., 0., 0.), (-0.707107, 0.707107, 0.), (0.707107, 0.707107, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.), (-1., 0., 0.), (-0.707107, -0.707107, 0.), (-1., 0., 0.), (-0.707107, 0.707107, 0.), (-1., 0., 0.), (-0.447214, -0.894427, 0.), (0., -1., 0.), (0.447214, 0.894427, 0.), (1., 0., 0.), (0.707107, -0.707107, 0.), (0.894427, 0.447214, 0.), (0.707107, 0.707107, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.), (-0.707107, -0.707107, 0.), (0., -1., 0.), (-0.316228, -0.948683, 0.), (0.707107, -0.707107, 0.), (0., -1., 0.), (0.447214, 0.894427, 0.), (0.447214, -0.894427, 0.), (0., 1., 0.), (0.894427, -0.447214, 0.), (-0.707107, 0.707107, 0.), (0.894427, 0.447214, 0.), (-1., 0., 0.), (-0.707107, 0.707107, 0.), (0.707107, 0.707107, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.), (-1., 0., 0.), (-0.707107, -0.707107, 0.), (-1., 0., 0.), (-0.707107, 0.707107, 0.), (-1., 0., 0.), (-0.447214, -0.894427, 0.), (0., -1., 0.), (0.447214, 0.894427, 0.), (1., 0., 0.), (0.707107, -0.707107, 0.), (0.894427, 0.447214, 0.), (0.707107, 0.707107, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.), (-0.768221, -0.640184, 0.), (0., -1., 0.), (0.514496, -0.857493, 0.), (0.447214, 0.894427, 0.), (0., 1., 0.), (-0.707107, 0.707107, 0.), (0., 0., 1.), (0., 0., 1.) ]
   scalar dinoAngle = random(0,2) * MathPI()
   scalar dinoX = 15*random(-1,1)
   scalar dinoY = 15*random(-1,1)
}
```

Notice here that we often only need to declare _what needs to be drawn_ rather than _how to draw it_.  For instance, to draw the text we need only say what text we want, what font/color we want, and where on the canvas it should go.  We don't need to say _when_ the text gets drawn (i.e., we don't need to think about execution order, as in the usual [imperative programming](https://en.wikipedia.org/wiki/Imperative_programming) model of OpenGL/Direct3D), and generally avoid a lot of the crufty function calls needed to [draw text](https://users.cs.jmu.edu/bernstdh/web/common/lectures/summary_opengl-text.php) in a traditional 3D graphics API.  We just need to give the bare minimum specification.

The most important line in the snippet above is the one that transforms a list of 3D points into a list of 2D points that can be used to draw 2D shapes.  In general, the pattern we'll use is

```haskell
-- define a list of coordinates in 3D
list points3D = [ (x0,y0,z0), (x1,y1,z1), ..., (xN,yN,zN) ]

-- use the camera data to project 3D points to 2D
list points2D = projectList( points3D, camera.model, camera.proj, camera.view )
```

The function `projectList()` takes each of the points in the input list, applies the given modelview and projection transformations, and maps the final coordinates to the given viewport.  In fact, this function is equivalent to applying `gluProject()` to each point in the list.  A projection for a single point can also be computed, using just `project()` (or `projectDepth()`, which also provides the z-depth relative to the viewer).

### Drawing the Dinosaur

Next we get to the main event: drawing our dinosaur!

TODO: continue here

```haskell
-- Rule for drawing the main dinosaur geometry
forall Dinosaur D {
   -- Shading / appearance parameters
   scalar D.alpha = .5
   vec3 D.skinColor = ( 0.1, 1.0, 0.1 )
   vec3 D.eyeColor = ( 1.0, 0.2, 0.2 )
   color D.wireColor = #0309
   scalar wireWidth = 2.0
   scalar ambient = .5

   -- Lighting information
   vec3 L = unit(( light.pos[0], light.pos[1], light.pos[2] ))
   list N = global.dinoNormals

   -- Define a transformation that positions the dinosaur in space.
   -- (The base transformation will be elaborated on to draw shadows and reflections.)
   mat4x4 D.baseTransform = rotate3dh( global.dinoAngle, (0,1,0) ) then translate3dh( global.dinoX, 0, global.dinoY )
   mat4x4 D.model = D.baseTransform

   -- Transform all the 3D vertex coordinates defining the dino geometry,
   -- and project them to 2D window coordinates.
   list p = projectList( global.dinoPoints, camera.model * D.model, camera.proj, camera.view )
```

