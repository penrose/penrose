# Single Substance Statements

## Object Declarations

An _object declaration_ declares the existence of an object, and specifies its type:

```substance
type_name object_name
```

where

- `type_name` is a type that is declared in the [Domain] schema; and
- `object_name` is the name given to this object, which can be referred to in other parts of the Substance program.

Once an object is declared, we can refer to it using `object_name`, which becomes a Substance variable.

Penrose also allows users to declare multiple objects of the same type at the same time:

```substance
type_name object_name_1, object_name_2, ...
```

This is equivalent to declaring the objects sequentially and separately.

## Predicate Applications

We can _apply_ the predicates (first defined in the [Domain] schema) in our Substance program simply by invoking it. The syntax is

```substance
predicate_name (argument_list)
```

where

- `predicate_name` is the name of the predicate, as declared in the [Domain] schema; and
- `argument_list` is a comma-separated list of objects (defined earlier in the Substance program) or other predicate applications.

The types of the Substance objects in `argument_list` must match the types of the arguments as declared in the [Domain] schema, allowing for subtypes to match their supertypes.

We illustrate these rules with some examples. Suppose we have [Domain] schema

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
type NotAnAtom

predicate Bond (Atom, Atom)
```

and Substance object declarations

```substance
Hydrogen H
Oxygen O
Atom A
NotAnAtom NA
```

Then,

| Predicate Application | Valid? | Notes                                                              |
| :-------------------- | :----- | ------------------------------------------------------------------ |
| `Bond (H, O)`         | Yes    |
| `Bond (H, H)`         | Yes    |
| `Bond (O, A)`         | Yes    |
| `Bond (NA, H)`        | No     | `NA` has type `NotAnAtom` which does not match the required `Atom` |

## Function and Constructor Applications

In Penrose, functions and constructors behave almost equivalently (the only difference being that constructors can additionally be invoked with the `Let` keyword explained below). Both functions and constructors can be invoked in the following two ways. The first way is

```substance
object_name := function_constructor_name (argument_list)
```

which requires object with `object_name` to be declared beforehand in the Substance program. The second way combines the declaration of the object and the invocation of the function into one statement:

```substance
type_name object_name := function_constructor_name (argument_list)
```

Finally, constructors alone can be invoked using the `Let` keyword if the constructor name is the same as the output type, as follows:

```substance
Let object_name := function_constructor_name (argument_list)
```

The rules for `argument_list` remain the same as in predicate applications. We further require that the output type of the function or constructor must match the type of `object_name`, up to subtyping. That is, if the function outputs type `A` and `object_name` has type `B`, then if `A` is a subtype of `B`, then the assignment is valid.

## Labeling Statements

Each declared object has a label, which can be accessed in the [Style] language. In the Substance program, the _labeling statements_ specify the value of these labels.

There are three types of labeling statements:

- `AutoLabel All`: this statement assigns the label of each object in the Substance program to be its name. For instance, if we declare object `Atom A`, then `AutoLabel All` will automatically assign `A` as its label.
- `Label object_name label_value`: this statement manually assign object `object_name`'s label to be `label_value`. There are two types of `label_value`s:
  - A math label is a TeX string delimited by dollar signs, e.g., `Label p $p_0$`
  - A text label is a plain-text string delimited by double quotes, e.g., `Label p "a point"`.
- `NoLabel object_list`: this statement ensures that objects in `object_list` do not have a label.

If an object has an assigned label, then in the [Style] language, we can access the object's `label` property.

[Domain]: ../domain/overview.md
[Style]: ../style/overview.md
