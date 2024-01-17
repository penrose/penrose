# Predicate Declarations

A _predicate_ represents mathematical or logical statements regarding zero or more objects or other statements. To declare a predicate, we write

```domain
predicate predicate_name (argument_list)
```

where

- `predicate_name` is the name of the predicate, which can be referred to in the _substance_ and _style_ schemas; and
- `argument_list` describes the types of objects that this predicate works with. We will go more into its details below.

The arguments in `argument_list` contains the types of objects that this predicate accepts, separated by commas. Here is a _domain_ schema that contains a couple of type declarations and four different, valid, predicate declarations:

```domain
type FirstType, SecondType

predicate P1 (FirstType, SecondType)
predicate P2 (SecondType a1, SecondType a2)
predicate P3 ()
predicate P4 (FirstType, SecondType, SecondType)
```

Notice how some declarations have names associated with the argument types (e.g. `P2` with argument names `a1` and `a2`), and some don't. These names to argument types are allowed and encouraged to enhance readability, but are not required.

## Symmetric Predicates

Some relations between objects are symmetric: for example, in the set-theory domain, the relations "_a_ intersects _b_" and "_b_ intersects _a_" are equivalent. As a recent new feature, Penrose supports symmetric predicates:

```domain
symmetric predicate predicate_name (argument_type, argument_type)
```

For now, Penrose only supports symmetry of binary predicates (taking exactly two arguments), and, the two arguments must be of the exact same type in the _domain_ schema (not necessarily when the predicate is used in the _substance_ and _style_ schemas).

As an example, consider the following valid _domain_ schema in the chemistry domain:

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond (Atom, Atom)
```

The predicate `Bond` is declared symmetric. Then, if `H` is a `Hydrogen` and `O` is an `Oxygen`, then `Bond(H, O)` and `Bond(O, H)` are considered equivalent by the Penrose engine. In other words, if Penrose expects to see `Bond(H, O)` in the _substance_ or _style_ schemas, then it will also accept `Bond(O, H)` even though the order of arguments are different.
