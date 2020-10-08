# Style 1.1 design

# Motivation

After writing some sample Style programs, it became clear that certain "convenience features" could greatly improve the quality-of-life of Style writers, particularly with respect to graphics programming and concision. This RFC describes the new features we plan to add to Style, as well as how they will interact. These are the features we plan to add before the codesign study. We are not aiming for backward compatibility -- features are mostly a superset of Style 1.0, so they will break existing programs (e.g. if they are missing type annotations).

## Example programs

See [vector-revised.sty](https://github.com/penrose/penrose/blob/new-style/examples/new-style/vector-revised.sty).

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

(polymorphic)

* list of elements (heterogeneous, untyped)
* typed lists of only vec2, vec3, or color (palette)

(new)

* shape (any GPI)
* vec2
* vec3
* mat2x2
* mat3x3

(Deprecated/unused)

* homogeneous matrix
* polygon

(Polymorphic types like tuples and lists are left out of the Style 1.1 spec.)

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

### Type parsing, conversions

* Scalars should support more syntax (e.g. `1e-10`, `0.`)
* Types can't be cast manually
* Values of type `int` should be promoted to type `scalar` when used in an operation with another `scalar`

### There are new "graphics types"

2D and 3D vector and matrix types are modeled off GLSL:

* https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)##Vectors
* https://en.wikibooks.org/wiki/GLSL_Programming/Vector_and_Matrix_Operations

#### Initialization

They only contain scalars. A vector is constructed as in the following example: `vec2 U.origin = (?, 0.0)`

Vector coordinates are initialized from however `?` (varying) variables are initialized, currently from the canvas size.

#### Functions

Vectors should support various convenience functions automatically:

* setters and getters for the relevant coordinates (`v[0]` or `v.x`, `v[1]` or `v.y`, `v[2]` or `v.z`)

Matrices should also support indexing to get and set elements, e.g. `m[1][2] = n[0][1]`.

(Swizzling is left out of the Style 1.1 spec, as it can get a little complicated WRT checking that the same coordinate isn't used, etc.)

The system can support use of homogeneous coordinates (for 2D) if the user uses 3D vectors and does all the conversions themselves.

The system will also provide convenience functions for vectors, such as `dot`, and convenience constructors for matrices, such as `mat2x2 m = translate(1.0, 2.0)`.

The following convenience functions for vectors will have syntactic sugar:

* `+`, `-`, `*.` (vectors of same size; the last one is `dot`)
* `*`, `/` (scalar-vector ops)

Other functions, like matrix-vector multiplication, will have functions provided, but not special syntax.

2d vectors will also be used natively as the representation for shapes' coordinates (`A.shape.start` and `A.shape.end`), with the appropriate getters and setters (e.g. `A.shape.end.x`). Example:

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

### Allow local variables

Now, variables can be declared locally, as in `pi` below, i.e. they only live in the scope of the block, must be declared with a type, and can have names reused in following blocks.

A local variable is not attached to a Substance object, and so cannot be referred to by any other block that brings a Substance object in scope. 

Within the scope of a block, local variables are immutable references (like `const` in JS) and can't be redefined.

Local variables can refer to anything in scope, e.g. things in other namespaces.


```
Colors {
    color red = rgba(1, 0, 0, 1)
}

Set X {
    scalar pi = 3.14
    // pi = 2 * pi // <-- this is a compiler error

    color myRed = lighten(Colors.red) // this is fine

    scalar X.x = 2
    X.x = 3 // <-- This is fine, as fields and properties can be redefined

    shape X.shape = Circle {
        x : X.x
        r : pi * 100
        color : myRed
    }
}

Set `A` {
    // `A`.pi does not exist, so it doesn't make sense to write "override `A`.pi = ..."
    scalar pi = 6.28; // can define its own
    ...

    override `A`.shape.r = pi * 100 // Yields a different value
}

```

### Shorthand can be used in blocks

`Shorthand` is a convenience keyword, in block scope, for defining an immutable reference to anything in namespace or block scope (including through a selector). `Shorthand` behaves just like `local`.


```
myFunctions {
   function f = {{ x => cos(x)/x*x }}
}

Set A {
   shorthand f = myFunctions.f
   A.x = (f(1) + f(2))/f(3)
}
```

In the future, we might give `shorthand` file scope (so shorthands can be reused across blocks in the same Style module), but for now, it only has block scope.

## Changes in namespaces and modules

### Namespaces

Namespaces are extended with three special behaviors:

1. Variables defined in namespaces are accessible in the namespace's scope from anywhere in the current Style module or any module that imports it. However, when declared, they don't need to be prepended with the namespace's name.
2. Namespaces can contain objective/function/constraint definitions.

Example for 1-2:

```
myNamespace = {
   color red = rgba( 1, 0, 0, 1 ) // No namespace needed, i.e. not `color myNamespace.red`
   function <<...your typescript here...>>
   objective myO = <<...your typescript here...>>
   constraint myC = <<...your typescript here...>> -- Still has to be written as an energy
}

Set A {
    shape A.shape = Circle {
        color : myNamespace.red
    }
}
```

3. Namespaces with the same name will have their contents combined if they have different names or overridden if any of the contents have the same name (with the appropriate compiler warning if the override keyword isn't used). Example:

```
namespace myColors {
   color red = rgba(1,0,0,1)
}

// some code here, maybe a module import

namespace myColors {
   color green = rgba(0,1,0,1) // now `myColors` contains `red` and `green`
}
```

### Functions can be declared in Typescript in any namespace

They have namespace scope only, and can be referred to in the same way as anything else in a namespace, including by shorthand. Functions/objectives/constraints need to live in `keyword << ... >>` syntax (where `keyword` is the kind of definition).

Each must have a Typescript definition that uses valid Style types (as defined in the Penrose system's Typescript `types.d.ts`). They must be written in a differentiable manner. Objectives/constraints should return an energy, and functions should return the appropriate Style type.

Example:

```
defs {

  function << 
      orientedSquare: (arr1: any, arr2: any, pt: any, len: VarAD): IPathDataV<VarAD> => {
        // TODO: Write the full function; this is just a fixed path for testing
        checkFloat(len);

        const elems: Elem<VarAD>[] =
          [{ tag: "Pt", contents: mapTup2(constOf, [100, 100]) },
          { tag: "Pt", contents: mapTup2(constOf, [200, 200]) },
          { tag: "Pt", contents: mapTup2(constOf, [300, 150]) }];
        const path: SubPath<VarAD> = { tag: "Open", contents: elems };

        return { tag: "PathDataV", contents: [path] };
      }
  >>

  // Just for demonstration; not actually used in program
  objective <<
      sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
        return distsq(center(s1), center(s2));
      }
  >>

}

Vector v
where In(v,V)
with VectorSpace V {

    shorthand square = defs.orientedSquare

    shape v.shape = Curve {
        pathData: square(V.axis1, V.axis2, intersection(V.axis1, V.axis2), 100.0)
    }

    encourage defs.sameCenter(...) // some args here
}

```

Currently we don't allow function definitions in blocks of rules, nor can they refer to Style variables, so neither of the below examples will be valid.

```
Set A { // Not OK
  function f = ``x => x + 1``
  A.z = f(1)
}}

Set A { // Not OK
  A.r = 0.0
  function f = ``x => x + A.r``
  A.z = f(1)
}
```

### Style modules

Styles can now be imported as modules as such: 

```
import "Colors.sty'
import "Keenan.sty"
```

One Style may import multiple modules. Multiple modules are brought into scope sequentially, with the current Style module coming last. The compiler looks for modules in the folder specified in the `runpenrose` command:

`runpenrose dir=/absolute/path/here my.sub my.sty my.dsl`

The policy for potential name clashes between imported modules is:

* the Style compiler continues to enforce 1 layer of namespaces (i.e. every statement must be in a namespace)
* the programmer is responsible for avoiding naming conflicts between namespaces

Currently, we do not check for circular dependencies in the import graph.

# Formalization

Coming soon, after discussion of RFC is done.

## Syntax

TODO

## Grammar

TODO

## Semantics

TODO

# Implementation

Coming soon, after discussion of RFC is done.

## Parsing/lexing

TODO: Likely to be tricky: JS lexing

## Checking

TODO: How will typechecking work?

Circular references

## Compiling

TODO

# Usability

TODO

## Warnings, errors, and gotchas

TODO

## Debugging

TODO

# Related languages

* GLSL
* CSS
* Various CSS extensions, like SASS

# Related issues

* https://github.com/penrose/penrose/issues/372
* https://github.com/penrose/penrose/issues/375
* Anything tagged `dealbreaker` or `style-1.1`
* Discussion in #language-design
