# triangle-mesh-3d

This example is an experiment with using Penrose to generate 3D diagrams. _Note that Penrose does not yet natively support many 3D vector operations!_ However, nothing prevents us from performing all the necessary calculations (3D rotations, perspective projection, etc.) inline---it just requires a whole lot of typing...

The example works by defining points as 3D coordinates, then using a simplistic camera model to project these 3D coordinates to 2D coordinates, which are drawn as usual.

What's particularly cool (and ultimately, useful) about this setup is that we can very easily use objectives and constraints on the 2D shapes to "push back" on the arrangement of objects in 3D. This kind of optimization is especially important for diagrams, where one wishes elements to be legible in the final 2D projection. For instance, in this example we ensure that the 3D coordinates are such that

- 2D labels of the 3D vertices do not overlap
- the 2D projections of the triangles do not overlap in screen space
- 2D projections of triangles have a reasonable area in screen space
- 2D projections of triangle have reasonable angles in screen space
- their shadows project onto the ground plane (enforced via a 2D polygon inclusion constraint)

Future development on Penrose should make such examples easier, by better support for `vec3` (and `vec4`) types, akin to [GLSL](https://en.wikipedia.org/wiki/OpenGL_Shading_Language), as well as associated matrix types and convenience functions (e.g., building a nice modelview and/or projection matrix from intuitive parameters, a la standard libraries in the OpenGL ecosystem, like [glm](https://github.com/g-truc/glm)).
