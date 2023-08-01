# Substance Usage

The _substance_ program tells Penrose _what_ objects and relations to draw. In the set-theory example, for example, we can have the following _substance_ program:

```substance
Set A, B, C

IsSubset (A, C)
IsSubset (B, C)
Not (Intersecting (A, B))

AutoLabel All
```

The first line declares the objects that are to be drawn, the last line tells Penrose to automatically label the objects based on their names. All other lines invoke the predicates defined in the _domain_ schema to declare relations between objects.

Notably, like the _domain_ schema, the _substance_ program does not contain any instructions about how, say, a `Set` must be rendered, or how the relation `IsSubset` should be reflected in the diagram. The _substance_ program only declares the existence of these objects and relations, whereas the _style_ schema shows _how_ these objects and relations can be drawn.

A _substance_ program may contain two types of statements: single statements or sequenced statements.

## Single Substance Statements

### Object Declarations

An _object declaration_ declares the existence of an object, and specifies its type:

```substance
type_name object_name
```

where

- `type_name` is a type that is declared in the _domain_ schema; and
- `object_name` is the name given to this object, which can be referred to in other parts of the _substance_ program.

Once an object is declared, we can refer to it using `object_name`, which becomes a _substance_ variable.

Penrose also allows users to declare multiple objects of the same type at the same time:

```substance
type_name object_name_1, object_name_2, ...
```

This is equivalent to declaring the objects sequentially and separately.

### Predicate Applications

We can _apply_ the predicates (first defined in the _domain_ schema) in our _substance_ program simply by invoking it. The syntax is

```substance
predicate_name (argument_list)
```

where

- `predicate_name` is the name of the predicate, as declared in the _domain_ schema; and
- `argument_list` is a comma-separated list of objects (defined earlier in the _substance_ program) or other predicate applications.

The types of the _substance_ objects in `argument_list` must match the types of the arguments as declared in the _domain_ schema, allowing for subtypes to match their supertypes. Furthermore, if the _domain_ argument type is `Prop`, then the _substance_ argument should be a predicate application.

We illustrate these rules with some examples. Suppose we have _domain_ schema

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
type NotAnAtom

predicate Bond (Atom, Atom)
predicate Not (Prop)
```

and _substance_ object declarations

```substance
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

### Function and Constructor Applications

In Penrose, functions and constructors behave almost equivalently (the only difference being that constructors can additionally be invoked with the `Let` keyword explained below). Both functions and constructors can be invoked in the following two ways. The first way is

```substance
object_name := function_constructor_name (argument_list)
```

which requires object with `object_name` to be declared beforehand in the _substance_ program. The second way combines the declaration of the object and the invocation of the function into one statement:

```substance
type_name object_name := function_constructor_name (argument_list)
```

Finally, constructors alone can be invoked using the `Let` keyword if the constructor name is the same as the output type, as follows:

```substance
Let object_name := function_constructor_name (argument_list)
```

The rules for `argument_list` remain the same as in predicate applications. We further require that the output type of the function or constructor must match the type of `object_name`, up to subtyping. That is, if the function outputs type `A` and `object_name` has type `B`, then if `A` is a subtype of `B`, then the assignment is valid.

### Labeling Statements

Each declared object has a label, which can be accessed in the _style_ schema. In the _substance_ program, the _labeling statements_ specify the value of these labels.

There are three types of labeling statements:

- `AutoLabel All`: this statement assigns the label of each object in the _substance_ program to be its name. For instance, if we declare object `Atom A`, then `AutoLabel All` will automatically assign `A` as its label.
- `Label object_name label_value`: this statement manually assign object `object_name`'s label to be `label_value`. There are two types of `label_value`s:
  - A math label is a TeX string delimited by dollar signs, e.g., `Label p $p_0$`
  - A text label is a plain-text string delimited by double quotes, e.g., `Label p "a point"`.
- `NoLabel object_list`: this statement ensures that objects in `object_list` do not have a label.

If an object has an assigned label, then in the _style_ schema, we can access the object's `label` property.

## Substance Statement Sequences

The _substance_ schema allows users to define sequences that expand into multiple statements. A sequence of statements is a single substance statement (as described above) with templated identifiers and an indexing clause, like

```substance
Vector v_i for i in [0, 2]
-- is equivalent to Vector v_0, v_1, v_2
```

Templated identifiers (like `v_i` in the example) are regular _substance_ identifiers consisting alphanumeric characters and underscores, except that the last underscore and the substring following it denote an indexed variable. In the example, `_i` in `v_i` denotes an index variable `i` taking on the values in the range of `[0, 2]`, i.e. `0, 1, 2`. This line of code then gets expanded into three statements `Vector v_0`, `Vector v_1`, and `Vector v_2`.

When templated identifiers collide with regular identifiers, the latter is shadowed in indexing statements:

```substance
Vector v_i, v_j, vec1

Vector v_i for i in [0, 10]
  -- ok: expands into v_0, v_1, v_2, ..., v_10

Vector v_j for i in [0, 1]
  -- error: the range of `j` is not defined by `i in [0, 1]`
  -- notice that `v_j` here does not refer to the `v_j` defined above
  -- since `v_j` occurs in a sequence statement.

Orthogonal(v_i, vec1) for i in [0, 3]
  -- ok: expands into Orthogonal(v_0, vec1); Orthogonal(v_1, vec1);
  --     Orthogonal(v_2, vec1); Orthogonal(v_3, vec1)
  -- notice that `vec1` does not have an underscore, so it is treated
  -- as a regular substance variable

Orthogonal(v_i, v_j) for i in [0, 5]
  -- error: the range of `j` is not defined by `i in [0, 5]`
```

When multiple template variables and ranges are present, _substance_ takes all combinations (cartisian product) of the indices in the ranges, like

```substance
Orthogonal(v_i, v_j) for i in [0, 1], j in [1, 2]
  -- expands into Orthogonal(v_0, v_1); Orthogonal(v_0, v_2);
  -- Orthogonal(v_1, v_1); Orthogonal(v_1, v_2)
```

### Conditional Filtering

Sometimes, we don't want to iterate through all possible combinations, since some combinations are undesirable. _Substance_ allows users to filter the combinations using a Boolean expression in the `where` clause. _Substance_ would discard all combinations that make the Boolean expression false.

```substance
Vector v_i for i in [0, 10] where i % 2 == 0
  -- even number: 0, 2, 4, 6, 8, 10

Orthogonal(v_i, v_j) for i in [0, 2], j in [0, 2] where i <= j
  -- triangular range: [0, 0], [0, 1], [0, 2], [1, 1], [1, 2], [2, 2]

Orthogonal(v_i, v_j) for i in [0, 3], j in [0, 3] where i + 1 == j
  -- consecutive pairs: [0, 1], [1, 2], [2, 3]

Orthogonal(v_i, v_j) for i in [0, 3], j in [0, 3] where i % 2 == 0 && j == i + 1
  -- disjoint pairs of 2: [0, 1], [2, 3]
```

Specifically, the Boolean expressions may contain:

- Boolean constants (`true` and `false`),
- Unary logical operator (`!` for logical-not) followed by a Boolean expression,
- Binary logical operators (`&&` for logical-and and `||` for logical-or) between Boolean expressions, and
- Numerical comparisons (`==` for equality, `!=` for non-equality, `<` for less-than, `>` for greater-than, `<=` for less-than-or-equal-to, and `>=` for greater-than-or-equal-to) between numerical expressions.

Numerical expressions may contain:

- Floating-point constants,
- Index variables defined in the ranges, like `i` in `for i in [0, 2]`,
- Unary numerical operators (`-`) followed by a numerical expression, and
- Binary numerical operators (`+` for plus, `-` for minus, `*` for multiplication, `/` for division, `%` for modulo, and `^` for power) between two numerical expressions.

The order of operations are the typical ones, and parentheses can be used to override the default order of operations, just like other programming languages.

### Duplications

A sequence generates a list of _substance_ statements. The existing semantics on duplicates apply to them as well.

```substance
Vector v_0

Vector v_i for i in [0, 2]
  -- error: `v_0` is declared and cannot be re-declared

Vector v_i, v_j for i in [0, 2], j in [0, 2]
  -- error: `v_0` is declared and cannot be re-declared

Orthogonal(v_0, v_1)

Orthogonal(v_i, v_j) for i in [0, 2], j in [0, 2] where i != j
  -- ok, because duplicated predicates are ok
```

### Accessing Individual Elements of Sequence

An indexing statement generates identifiers by replacing index variables with strings of integer values. Therefore, generated identifiers can be used just like regular identifiers as long as they exist:

```substance
Vector v_i for i in [0, 2]
LinearlyDependent(v_0, v_2) -- ok
```

## Comments

Comments are ignored by the Penrose engine. In the _substance_ program, comments are declared using double dashes:

```substance
-- this is a comment
```
