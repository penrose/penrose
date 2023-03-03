# Usage

The _substance_ program tells Penrose _what_ objects and relations to draw. In the set-theory example, for example, we can have the following _substance_ program:

```
Set A, B, C

IsSubset (A, C)
IsSubset (B, C)
Not (Intersecting (A, B))

AutoLabel All
```

The first line declares the objects that are to be drawn, the last line tells Penrose to automatically label the objects based on their names. All other lines invoke the predicates defined in the _domain_ schema to declare relations between objects.

Notably, like the _domain_ schema, the _substance_ program does not contain any instructions about how, say, a `Set` must be rendered, or how the relation `IsSubset` should be reflected in the diagram. The _substance_ program only declares the existence of these objects and relations, whereas the _style_ schema shows _how_ these objects and relations can be drawn.

Formally, a _substance_ program can contain five types of statements.

## Object Declarations

An _object declaration_ declares the existence of an object, and specify its type:

```
type_name object_name
```

where

- `type_name` is a type that is declared in the _domain_ schema; and
- `object_name` is the name given to this object, which can be referred to in other parts of the _substance_ program.

Once an object is declared, we can refer to it using `object_name`, which becomes a _substance_ variable.

Penrose also allows users to declare multiple objects of the same type at the same time:

```
type_name object_name_1, object_name_2, ...
```

This is equivalent to declaring the objects sequentially and separately.

## Predicate Applications

We can _apply_ the predicates (first defined in the _domain_ schema) in our _substance_ program simply by invoking it. The syntax is

```
predicate_name (argument_list)
```

where

- `predicate_name` is the name of the predicate, as declared in the _domain_ schema; and
- `argument_list` is a comma-separated list of objects (defined earlier in the _substance_ program) or other predicate applications.

The types of the _substance_ objects in `argument_list` must match the types of the arguments as declared in the _domain_ schema, allowing for subtypes to match their supertypes. Furthermore, if the _domain_ argument type is `Prop`, then the _substance_ argument should be a predicate application.

We illustrate these rules with some examples. Suppose we have _domain_ schema

```
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
type NotAnAtom

predicate Bond (Atom, Atom)
predicate Not (Prop)
```

and _substance_ object declarations

```
Hydrogen H
Oxygen O
Atom A
NotAnAtom NA
```

Then,

| Predicate Application | Valid? | Notes                                                                      |
| :-------------------- | :----- | -------------------------------------------------------------------------- |
| `Bond (H, O)`         | Yes    |
| `Bond (H, H)`         | Yes    |
| `Bond (O, A)`         | Yes    |
| `Bond (NA, H)`        | No     | `NA` has type `NotAnAtom` which does not match the required `Atom`         |
| `Not (H)`             | No     | `H` has type `Hydrogen` which does not match the required `Prop`           |
| `Not (Bond (H, O))`   | Yes    | `Bond (H, O)` is a predicate application which matches the required `Prop` |

## Function and Constructor Applications

In Penrose, functions and constructors behave equivalently. There are two ways of invoking a function or constructor. The first way is

```
object_name := function_constructor_name (argument_list)
```

which requires object with `object_name` to be declared beforehand in the _substance_ program. The second way combines the declaration of the object and the invocation of the function into one statement:

```
type_name object_name := function_constructor_name (argument_list)
```

The rules for `argument_list` remain the same as in predicate applications. We further require that the output type of the function or constructor must match the type of `object_name`, up to subtyping. That is, if the function outputs type `A` and `object_name` has type `B`, then if `A` is a subtype of `B`, then the assignment is valid.

## Labeling Statements

Each declared object has a label, which can be accessed in the _style_ schema. In the _substance_ program, the _labeling statements_ specify the value of these labels.

There are three types of labeling statements:

- `AutoLabel All`: this statement assigns the label of each object in the _substance_ program to be its name. For instance, if we declare object `Atom A`, then `AutoLabel All` will automatically assign `A` as its label.
- `Label object_name label_value`: this statement manually assign object `object_name`'s label to be `label_value`. There are two types of `label_value`s:
  - A math label is a TeX string delimited by dollar signs, e.g., `Label p $p_0$`
  - A text label is a plain-text string delimited by double quotes, e.g., `Label p "a point"`.
- `NoLabel object_list`: this statement ensures that objects in `object_list` do not have a label.

If an object has an assigned label, then in the _style_ schema, we can access the object's `label` property.

## Comments

Comments are ignored by the Penrose engine. In the _substance_ program, comments are declared using double dashes:

```
-- this is a comment
```
