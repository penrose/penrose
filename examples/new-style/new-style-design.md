# Style 1.1 design

# Motivation

After writing some sample Style programs, it became clear that certain "convenience features" could greatly improve the quality-of-life of Style writers, particularly with respect to graphics programming and concision. This RFC describes the new features we plan to add to Style, as well as how they will interact.

## Example programs

See [vector-revised.sty](https://github.com/penrose/penrose/blob/new-style/examples/new-style/vector-revised.sty).

TODO: Revise re: functions?

# Features

## Variables have types

Example:

```
Set A {
    scalar c = 0.0
    vec2 v = A.shape.start
    shape A.shape = Circle { ... }
    ...
}
```

The full list of types is:

(Existing)

* scalar
* int
* bool
* string
* path (in the SVG path sense)
* color
* file (synonym for string path, like `"/assets/vec.svg"`)
* style (synonym for string category, like `"dotted"`)

(new)

* vec2
* vec3
* mat2x2
* mat3x3

(polymorphic)

* list of elements (heterogeneous, untyped)
* typed lists of only vec2, vec3, or color (palette)

(Deprecated/unused)

* homogeneous matrix
* polygon

(Polymorphic types like tuples and lists are left out of the Style 1.1 spec.)

### Type parsing, conversions

* Scalars should support more syntax (e.g. `1e-10`, `0.`)
* Values of type int should be promoted to type scalar when used in an operation with another scalar

### There are new "graphics types"

2D and 3D vector and matrix types are modeled off GLSL:

* https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)##Vectors
* https://en.wikibooks.org/wiki/GLSL_Programming/Vector_and_Matrix_Operations

They only contain scalars. A vector is constructed as in the following example: `vec2 U.origin = (?, 0.0)`

Vectors should support various convenience functions automatically:

* setters and getters for the relevant coordinates (`v[0]` or `v.x`, `v[1]` or `v.y`, `v[2]` or `v.z`)

(Swizzling is left out of the Style 1.1 spec, as it can get a little complicated WRT checking that the same coordinate isn't used, etc.)

The system can support use of homogeneous coordinates (for 2D) if the user uses 3D vectors and does all the conversions themselves.

The system will also provide convenience constructors for matrices, such as `mat2x2 m = translate(1.0, 2.0)`.

2D vectors will also be used natively as the representation for shapes' coordinates (`A.shape.start` and `A.shape.end`), with the appropriate getters and setters (e.g. `A.shape.end.x`). Example:

```
forall VectorSpace U {               -- LinearAlgebra.sty
   scalar axisSize = 1.0 -- This is a local var
   vec2 U.origin = (?, ?) -- This is a globally-accessible var
   vec3 U.blah = (?, ?, ?)
   shorthand o = U.origin
   shape U.xAxis = Arrow { -- draw an arrow along the x-axis 
          start : (o.x - axisSize, o.y)
            end : (o.x + axisSize, o.y)
    }
}

```

Colors will also have the appropriate getters and setters, e.g. `x.r` (and so on). The policy for colors is that they are all gotten and set as RGBA, though internally they may be represented as HSVA.

## Changes in statements (in blocks of rules)

### Variables must be declared with their types

Variables continue to only be declarable in blocks, and when declared, their type must be given (i.e. we don't do type inference). Otherwise, a variable in a statement can be overridden as before.

```
Set X {
    scalar c = 1.0
}

Set `A` {
    override `A`.c = 2.0
}
```

### Allow local variables

TODO

They have types too

### Shorthand can be used in blocks

TODO

Block scope
File scope

## Changes in namespaces

### Namespaces

```
myNamespace = {
   color red = rgba( 1, 0, 0, 0 )
   function myF = ``...your typescript here...``
   objective myO = ``...your typescript here...``
   constraint myC = ``...your typescript here...`` -- Still has to be written as an energy
}
```

### Functions can be declared in Typescript in any namespace

TODO

Where are these typed, where are these declared?

(hard to do any one feature in isolation)

No Style variables

### Style modules

TODO

syntax: "import..."

Overall proposal for Style imports/functions/naming:
* continue to enforce 1 layer of namespaces (i.e. every statement must be in a namespace)
* only namespaces can contain functions/objectives/constraints
* the programmer is responsible for avoiding naming conflicts between namespaces
* namespaces with the same name will have their contents combined if they have different names (as in @keenan's myColors example) or overridden if any of the contents have the same name (with the appropriate compiler warning if the override keyword isn't used)

The compiler looks for...

# Design questions

- Initialization
- Casting
- Getting and setting
- Pending variables -- where do they appear?
- *Setting shape properties together*
- *Getting shape properties together*
- Operators
- Built-in functions
- Example code
  - SIGGRAPH paper, Fig. 12
  - Maybe also euclidean.sty?
- Do we also want matrices? 
- How to keep this backward-compatible? (If at all?)
- What is the priority of this?

# Formalization

## Syntax

TODO

## Grammar

TODO

## Semantics

TODO

# Implementation questions

- TODO

## Parsing/lexing

## Checking

## Compiling

## Debugging

# Related languages

* GLSL
* CSS

# Related issues

* https://github.com/penrose/penrose/issues/372
* https://github.com/penrose/penrose/issues/375
* Anything tagged `dealbreaker` or `style-1.1`
* Discussion in #language-design
