# Syntax

A _domain_ schema describes the types of objects, as well as relations between these objects, that Penrose diagrams work with. For example, say we want to use Penrose to draw a Venn diagram that represents the relationship between mathematical sets. We can declare the following domain schema:

```
type Set

predicate Not (Prop p1)
predicate Intersecting (Set s1, Set s2)
predicate IsSubset (Set s1, Set s2)
```

This schema tells Penrose that the diagrams works with and illustrates _Set_ objects, relations such as _Intersecting_ and _IsSubset_, and logical relation _Not_ applied upon other statements.

Notably, the _domain_ schema are not _instructions_ of how to draw the _Set_ objects and relations between them. Such instructions of the specific diagram elements are provided by the _style_ schema, not the _domain_ schema. This allows multiple visual representations to be applied to objects from the same domain.

There are four types of statements that can appear in the _domain_ schema.

## Type Declarations

A _type_ is a class of objects that a diagram works with. In the set-theory example, _Set_ is a type. The syntax for a type declaration is as follows:

```
type typename
```

This statement declares a type with name `typename`. After declaration, a type can be referred to in other parts of the _domain_ schema, as well as the _style_ and _substance_ schemas.

For example, statement `type Set` declares a type with name `Set`. We can then use the name `Set` to later refer to this type.

### Subtypes

Conceptually, a type might be a _subtype_ of another type. As an example, in a chemistry domain, types _Hydrogen_ and _Oxygen_ might both be subtypes of the type _Atom_. Penrose allows us to represent subtyping relations as follows:

```
type subtype_name <: supertype_name
```

Take the example of types _Hydrogen_, _Oxygen_, and _Atom_:

```
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
```

Subtyping describes "is-a" relationships between types: a Hydrogen "is-a[n]" Atom; an Oxygen "is-a[n]" Atom. Using the "is-a" interpretation, since an Atom "is-a[n]" Atom, inherently, for all type `A`, we have that `A` is a subtype of `A`. This interpretation of subtyping is consistent with "inheritance" in many object-oriented programming languages.

Penrose adopts this interpretation: if Penrose expects to see an object with type `A`, then it will also accept an object of type `B`, as long as `B` is a subtype of `A`. We say that "`B` **matches** `A`."

## Predicate Declarations

A _predicate_ represents mathematical or logical statements regarding zero or more objects or other statements. To declare a predicate, we write

```
predicate predicate_name (argument_list)
```

where

- `predicate_name` is the name of the predicate, which can be referred to in the _substance_ and _style_ schemas; and
- `argument_list` describes the types of objects that this predicate works with. We will go more into its details below.

The arguments in `argument_list` contains the types of objects that this predicate accepts, separated by commas. Here is a _domain_ schema that contains a couple of type declarations and four different, valid, predicate declarations:

```
type FirstType, SecondType

predicate P1 (FirstType, SecondType)
predicate P2 (SecondType a1, SecondType a2)
predicate P3 ()
predicate P4 (FirstType, SecondType, SecondType)
```

Notice how some declarations have names associated with the argument types (e.g. `P2` with argument names `a1` and `a2`), and some don't. These names to argument types are allowed and encouraged to enhance readability, but are not required.

### The `Prop` Type

A special type, _Prop_, is built into Penrose, and is available to be used as predicate arguments. The _Prop_ type represents predicates themselves. For example, the predicate _Not_ is declared in the set-theory example:

```
predicate Not (Prop p1)
```

When we use the _Not_ predicate in the _substance_ or _style_ schemas, then, we can pass in any predicates as _Prop_. For example, we can write `Not(Intersecting(s1, s2))` in the _substance_ or _style_ schemas to conceptually denote that `s1` and `s2` are disjoint.

### Symmetric Predicates

Some relations between objects are symmetric: for example, in the set-theory domain, the relations "_a_ intersects _b_" and "_b_ intersects _a_" are equivalent. As a recent new feature, Penrose supports symmetric predicates:

```
symmetric predicate predicate_name (argument_type, argument_type)
```

For now, Penrose only supports symmetry of binary predicates (taking exactly two arguments), and, the two arguments must be of the exact same type in the _domain_ schema (not necessarily when the predicate is used in the _substance_ and _style_ schemas).

As an example, consider the following valid _domain_ schema in the chemistry domain:

```
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond (Atom, Atom)
```

The predicate `Bond` is declared symmetric. Then, if `H` is a `Hydrogen` and `O` is an `Oxygen`, then `Bond(H, O)` and `Bond(O, H)` are considered equivalent by the Penrose engine. In other words, if Penrose expects to see `Bond(H, O)` in the _substance_ or _style_ schemas, then it will also accept `Bond(O, H)` even though the order of arguments are different.

## Function and Constructor Declarations

In mathematics, we sometimes define functions that takes some inputs and returns some output. Penrose allows us to define _functions_ that behave similarly:

```
function function_name (argument_list) -> output_type
```

where

- `function_name` declares the name of the function, which can be referred to by the _substance_ and _style_ schemas;
- `argument_list` is a list of the types of inputs that this function accepts, similar to the argument list in predicate declarations; and
- `output_type` represents the type of the output of the function. When we _assign_ the result of the function to an object in the _substance_ and _style_ schema, the output of the function must match the type of the assigned object (either the types are exactly the same, or the output type is a subtype of the assigned object type).

For example, in the linear-algebra domain, we can define

```
type Vector
function addVector (Vector v1, Vector v2) -> Vector
```

_Constructors_ in Penrose are functionally equivalent to functions, with the same declaration syntax, except for the first word:

```
constructor constructor_name (argument_list) -> output_type
```

## Comments

_Comments_ are lines in the schema that are ignored by the Penrose parser. They are for documentation purposes, and are not involved in the generation of the diagrams. In the _domain_ schema, they start with two dashes:

```
-- this is a comment
```
