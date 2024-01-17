# Selectors

Selectors are the most important component in a Style program, since they actually describe _how_ to draw elements of a diagram. The syntax for Selectors is as follows:

```style
forall list_object_declarations
where list_relations
with list_object_declarations {
    ... (Selector Body)
}
```

where

- `list_object_declarations` is a **semicolon**-separated list of object declarations, similar to the object declarations in the Substance program. Each object declaration has syntax `type_name object_name`. The names declared in `list_object_declarations` are referred to as _style variables_.
- `list_relations` is a **semicolon**-separated list of constraints (about objects in `list_object_declaration`) that must be satisfied in order for this style block to be triggered.

The `forall`, `where`, and `with` clauses form the Selector Header. One might observe that both the `forall` clause and the `with` clause take in list of object declarations. The two clauses are treated equivalently in the header. Variables declared in these clauses can be accessed within the Selector Body.

The first clause of a Selector Header must be `forall`; the other clauses `where` and `with` can be written in any order. The header does not allow for empty lists - that is, `list_object_declarations` and `list_relations` in the `where` and `with` clauses must not be empty. If it is desirable to not have any elements in a clause, the entire clause must be omitted.

In the set-theory example, a style block may look like

```style
forall Set x {
}
```

or

```style
forall Set x; Set y
where IsSubset (x, y) {
}
```

or

```style
forall Set x
where IsSubst(x, y)
with Set y {
}
```

## Matching style block against substance program in general

Penrose functions by matching a Selector Header against a Substance program. In a nutshell, given a style block

```style
forall Set x; Set y
where IsSubset (x, y)
```

the Penrose compiler searches through the Substance program to find sets of objects consistent with `Set x; Set y` such that `IsSubset(x, y)` is satisfied. This is done through generating mappings from _style variables_ to _substance variables_, which are the objects in the Substance program.

For instance, consider a simple set-theory Substance program that works with the previous style block:

```substance
Set A, B, C
IsSubset (A, B)
IsSubset (B, C)
```

By matching the style block against the Substance program, we essentially consider six possible mappings (note that repeated elements are, by default, disallowed; see next section), some of which are valid and some are invalid:

| Mapping          | `IsSubset(x, y)` becomes | Satisfied by Substance |
| :--------------- | :----------------------- | ---------------------- |
| `x -> A; y -> B` | `IsSubset(A, B)`         | Yes                    |
| `x -> A; y -> C` | `IsSubset(A, C)`         | No                     |
| `x -> B; y -> A` | `IsSubset(B, A)`         | No                     |
| `x -> B; y -> C` | `IsSubset(B, C)`         | Yes                    |
| `x -> C; y -> A` | `IsSubset(C, A)`         | No                     |
| `x -> C; y -> B` | `IsSubset(C, B)`         | No                     |

Here, Penrose filters out mappings which do not satisfy the constraints listed in the Style block, and keeps a list of _good_ mappings (in this example, two mappings are kept). For each _good_ mapping, the body of the Style block (`list_body_expressions`) is executed, where each instance of the Style variables (`x` and `y`) is substituted with the corresponding Substance variables (once with `A` and `B`, once with `B` and `C`).

## Repeatable vs Non-Repeatable Matching

Penrose, by default, performs "non-repeatable" matching. That is, if there are two style variables declared in a Selector Header (in `with` or `where` clause), then the two style variables must correspond to different substance variables.

As an example, the header

```style
forall Node a; Node b
where Edge(a, b)
```

does not generate a valid matching against the substance

```substance
Node X
Edge(X, X)
```

This is a design feature because many times, we would want different visualization for self-edges like this. To override this behavior, one can add the `repeatable` keyword:

```style
forall repeatable Node a; Node b
where Edge(a, b)
```

and this header will allow `a` and `b` to both map to `X`.

## Object Declarations

In the list of object declarations in a Style block, we can declare two types of objects, which are matched differently by the Penrose compiler.

### Substance objects

We can declare a Substance object, whose object name is surrounded by backticks. For instance,

```style
forall Set `A` {
}
```

can only be mapped to the Substance object with the exact same name (`A`) provided that the types match (subtyping allowed). In other words, given Substance program

```substance
Set A, B, C
```

matching the Style block against the Substance block yields only one valid mapping: `` `A` -> A ``.

### Style objects

If an object name is not surrounded by backticks, then this object is a Style object with a Style variable. As seen before, the Penrose compiler will try to map Style variables to any Substance objects, provided that their types match (subtyping allowed).

## Relations

A Style block supports three types of relations, two of which can also be seen in the Substance program.

### Predicate Applications

Just like in the Substance program, each predicate application has syntax

```substance
predicate_name (argument_list)
```

where elements of `argument_list` can refer to objects declared in `list_object_declarations`, or be other predicate applications. The types must still match, allowing subtyping.

Optionally, one can give an alias to a predicate application:

```substance
predicate_name (argument_list) as alias_name
```

If such an alias is set, then `alias_name` will be accessible in the style block body, and it will always refer to the version of the predicate application within the Substance program.

#### Symmetry

If a predicate is declared as symmetric, then it gets special treatment.
Suppose we have the following Domain schema:

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond (Atom, Atom)
```

and the following style block:

```style
forall Hydrogen h; Oxygen o
where Bond (h, o) {
    ...
}
```

The style block will successfully match the following substance schema:

```substance
Hydrogen H
Oxygen O
Bond (O, H)
```

where `Bond (h, o)` in the style block matches against `Bond (O, H)` in the substance schema. Because Bond is declared symmetric, when Penrose looks for `Bond (h, o)`, it also looks for `Bond (o, h)` and finds a match. In other words, the matching algorithm handles the equivalence between `Bond (h, o)` and `Bond (o, h)` correctly.

### Function and Constructor Applications

Each function or constructor application has syntax

```substance
object_name := function_name (argument_list)
```

We do not allow aliasing for function and constructor applications. Arguments in `argument_list` must have types that match the Domain argument types, similar to the substance schema.

### Object Property Relations

Aside from predicate applications and function (constructor) applications, Penrose also supports a predicate-like relation that checks whether an object has a certain property, say `label`. For instance, we may write

```style
forall Set s
where s has label {
    ... some code that uses s.label
}
```

If a certain `Set A` in the Substance program does not have a label (perhaps due to `NoLabel` declarations), then `s` will not be mapped to `A`, thus preventing an access of nonexistent properties.

We can further distinguish between math labels and text labels (see [substance labeling](../substance/statements.md#labeling-statements)): `where p has math label` matches math labels, whereas `where p has text label` matches text labels.

## Matching Deduplication

The matching algorithm is designed to avoid duplicated mappings. If two mappings give us the same set of matched objects (in the Substance program) and the equivalent set of matched substance relations (predicate applications and function or constructor applications), then the algorithm only triggers on one of them.

For instance, say Penrose tries to match the Style block

```style
forall Set x; Set y {
}
```

against Substance program

```substance
Set A, B
```

Then, only one of mappings `x -> A; y -> B` and `x -> B; y -> A` triggers the Style block.

## Reserved Variables

Within a Style block body, some variable names are reserved for metadata purposes:

- `match_total` is an integer that refers to the number of times that this Style blocks will be triggered (or matched) in total; and
- `match_id` is the 1-indexed ordinal of this current matching.

These values can directly be read or overwritten within the style block body if needed.
