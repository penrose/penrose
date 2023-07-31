---
title: "What Have We Done to the Languages?"
date: 2023-06-30
authors:
  - author: Yiliang Liang
    twitter: "@liangyilinag"
    github: liangyiliang
blog: true
---

<BlogMeta  />

Exciting new language features in Penrose!

---

I joined the Penrose team in July 2022, and worked on of a few language-related improvements to Penrose that make Penrose more natural, flexible and expressive, from allowing Style programs to leverage symmetric properties of relations to letting users write more natural, mathematical expressions in constraints and objectives.

## Binary Symmetric Predicates

Mathematical relations can be symmetric. For example, if set `A` is equal to set `B`, then by symmetry, set `B` is equal to set `A`. Penrose didn't have the notion of symmetry and one would need to duplicate code to simulate this.

In July 2022, we implemented support for binary symmetric predicates which allowed for more flexible style-substance matching. By marking a binary predicate with the `symmetric` keyword, the Domain author basically tells Penrose that it does not care about the order of input to the predicate, just like a symmetric mathematical relation.

### Why did we need this?

The necessity of this feature was pointed out in [Keenan Crane](https://www.cs.cmu.edu/~kmcrane/)'s [GitHub issue](https://github.com/penrose/penrose/issues/744) using an interesting example. Consider the Domain program

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom

predicate Bond(Atom, Atom)
```

Under this Domain, one can construct a water molecule in multiple different ways, two examples being

```substance
-- version 1
Hydrogen H1, H2
Oxygen O
Bond(O, H1)
Bond(O, H2)
```

and

```substance
-- version 2
Hydrogen H1, H2
Oxygen O
Bond(H1, O)
Bond(O, H2)
```

Now suppose the Style author wants to enforce that the angle between the two bonds in a water molecule is 104.5 degrees. They can write,

```style
forall Oxygen o; Hydrogen h1; Hydrogen h2
where Bond(o, h1); Bond(o, h2) {
  -- enforce that the angle between the two bonds are 104.5 degrees
}
```

But this pattern will only match `version 1` of the water molecule. It does not match `version 2`, because according to the present matcher logic,

- `Bond(o, h1)` does not match `Bond(H1, O)`, because
- `o` does not match `H1` and `h1` does not match `O`, because
- `Oxygen` (declared type of `o`) does not match `Hydrogen` (declared type of `H1`) and `Hydrogen` (declared type of `h1`) does not match `Oxygen` (declared type of `O`).

But, intuitively, `Bond(o, h1)` should match `Bond(H1, O)`. Even though we intuitively understand `Bond` as a symmetric relation, Penrose doesn't know that. Without this knowledge, in order to match `version 2` of the water molecule, we would need to duplicate the code.

### What have we done?

We added (in a [pull request](https://github.com/penrose/penrose/pull/1061)) a keyword, `symmetric`, to the Domain language that applies to binary predicates. The domain author can now write,

```domain
symmetric predicate Bond(Atom, Atom)
```

Then, when the style-substance matcher tries to match the Style predicate `Bond(o, h1)` against the Substance predicate `Bond(H1, O)`, it will exploit the symmetric property of the predicate to consider `Bond(O, H1)` which will successfully match.

Predicates annotated with the `symmetric` keyword must meet two requirements, or Penrose will throw an error:

- The predicates must be binary (as of now).
- The two declared argument types of a symmetric predicate must be exactly equal. This is because, in a symmetric predicate, the two arguments are understood as exchangeable. So, one can write `symmetric predicate Bond(Atom, Atom)`, but not `symmetric predicate Bond(Atom, Oxygen)`. Of course, when applying a predicate in Substance or Style, subtypes can be used.

### How have we done it?

The first implementation of symmetry basically runs the following algorithm to match Style predicate `StyName(...StyArgs)` against Substance predicate `SubName(...SubArgs)`:

```
if (StyName !== SubName)
  return fail

// `match_raw(StyArgs, SubArgs)` requires an exact match and cares about ordering
mapping_raw = match_raw(StyArgs, SubArgs)

// if the match is successful, return the mapping resulted from the match.
if success:
  return mapping_raw

// flip the arguments of `SubArgs`
SubArgsSym = [SubArgs[1], SubArgs[0]]

// try matching again with the flipped arguments
mapping_sym = match_raw(StyArgs, SubArgsSym)

if success:
  return mapping_sym

return fail
```

In other words, the algorithm would attempt to match the original substance predicate (`Bond(H1, O)`). Only if the matching fails does it attempt to match the predicate with flipped arguments (`Bond(O, H1)`).

But sometimes, even if matching on the original substance predicate succeeds, we still actively seek for the alternative version with flipped arguments.
As an example, if `Equal` is a symmetric predicate between two `Set` objects, one can write the Substance program

```substance
Set A, B, C
Equal(B, A)
Equal(B, C)
```

and the Style program

```style
forall Set x, y, z
where Equal(x, y); Equal(y, z) {
  -- some code
}
```

However, using the aforementioned algorithm, this Style block does not match against the Substance program. Because `Equal(x, y)` already matches against `Equal(B, A)`, the matcher takes as truth the variable mapping `{x -> B, y -> A}`, and does not even consider the symmetric version of `Equal(B, A)`. The mapping requires that `y -> A`, which would prohibit the matcher from matching `Equal(y, z)` against `Equal(B, C)`, symmetric or not.

If the symmetric version of `Equal(B, A)` is considered, however, the matcher can also produce `{x -> A, y -> B}`, which would be compatible with the mapping `{y -> B, z -> C}` produced from matching `Equal(y, z)` against `Equal(B, C)`.

This bug was first discovered by [Nimo](https://www.cs.cmu.edu/~woden/) and written up by me in [this issue](https://github.com/penrose/penrose/issues/1126) in October 2022. The [fix](https://github.com/penrose/penrose/pull/1127) involves requiring the predicate matcher to always attempt _both_ versions of the symmetric predicate:

```
if (StyName !== SubName)
  return fail

toReturn = []

mapping_raw = match_raw(StyArgs, SubArgs)

if success:
  toReturn.push(mapping_raw)

// flip the arguments of `SubArgs`
SubArgsSym = [SubArgs[1], SubArgs[0]]

mapping_sym = match_raw(StyArgs, SubArgsSym)

if success:
  toReturn.push(mapping_sym)

if toReturn.length === 0:
  return fail

return toReturn

```

## Predicate Aliasing

We changed the Style language to allow the Style writer to give an alias to a matched predicate so that values and shapes can be associated to that matched predicate.

### Why did we need this?

As an example, say we would like to draw a line between two `Atom`s whenever a `Bond` predicate exists between them. We could write,

```style
forall Atom a; Atom b
where Bond(a, b) {
  ???.bondLine = Line {
    -- ...
  }
}
```

The question is, what does `bondLine` belong to? In other words, that would `???` be? Under this circumstance, what _can_ we fill in the blank `???`?

- We can either use `a` or `b`. But that would imply `bondLine` belongs to either `a` or `b`, which does not make sense - the line of a `Bond` does not belong to either of its arguments - it is shared between `a` and `b`.
- We can leave it blank, making it `bondLine = Line { ... }`. The issue is that we can no longer refer to `bondLine` outside of the block, and so cannot override its properties elsewhere.

Naturally, `bondLine` should belong to the predicate `Bond(a, b)`. It would be nice to say something to the effect of `Bond(a, b).bondLine`.

### What have we done?

The idea of predicate aliasing was first envisioned in July 2021 by [Helena Yang](https://heleaf.me/)'s [PR](https://github.com/penrose/penrose/pull/623) but was never merged. In July 2022, I picked up the issue in a new [PR](https://github.com/penrose/penrose/pull/1066), made it compatible with the codebase at that time, and merged it.

Predicate aliasing allows the Style author to write:

```style
forall Atom a; Atom b
where Bond(a, b) as bond {
  --                ^^^^
  -- Now, `bond` refers to `Bond(a, b)` in this Style block

  bond.bondLine = Line {
    -- ...
  }
}
```

Now, if the Style program matches Substance predicate `Bond(X, Y)`, then in this matching, `bondLine` would belong to `bond = Bond(X, Y)`. If another Style block matches the same `Bond(X, Y)`, then the same `bondLine` can be accessed using the alias that was assigned to it by that Style block.

### How have we done it?

Recall that matching a Style block against the Substance program yields a list of "mappings" that map each Style variable to the corresponding Substance variable. For example, if we match `Bond(a, b)` against `Bond(X, Y)` (not considering potential symmetry), we would get the mapping `{a -> X, b -> Y}`.

When a predicate alias exists, we augment the generate mapping to include an additional entry, mapping the alias name to a special "predicate instance name" of the matched predicate. In the example it would generate,

```
{
  a -> X,
  b -> Y,
  bond -> Bond_X_Y
}
```

where `Bond_X_Y` is the "predicate instance name" of the matched `Bond(X, Y)`.

Since `bond -> Bond_X_Y` is present in the mapping, we are then allowed to refer to its children, such as assigning `bond.bondLine` to a shape.

## Match Metadata

A Style block can be matched multiple times. We add two "reserved variables" for each Style block that exposes to the Style writer how many times this Style block is matched, and the index of the current matching.

### Why did we need this?

In January 2022, [Professor Keenan Crane](https://www.cs.cmu.edu/~kmcrane/) pointed out the need for each Style block to have access to:

- The total number of times the Style block is matched (the _cardinal_ of the Style block). This can be useful in, for example, using the total number of tick marks to determine the spacing needed between tick marks.
- The index (or the _ordinal_) of the _current_ match of this Style block. This can be useful in, for example, determining the number of tick marks to draw on a marked angle.

(For concrete examples see Professor Keenan Crane's [GitHub issue](https://github.com/penrose/penrose/issues/785)).

### What have we done?

We [added](https://github.com/penrose/penrose/pull/1074) two reserved variables in each Style block:

- `match_total` records how many times, in total, the Style block is matched, and
- `match_id` records the `1`-indexed ordinal of the current matching.

As an example,

::: code-group

```domain
type MyType
```

```substance
MyType T1, T2, T3, T4
```

```style
-- canvas specifications omitted
forall MyType t {
  // can use `match_id` and `match_total`
}
```

:::

Since the Style block is matched four times, `match_total` equals to 4 within the Style block. Furthermore, `match_id` is one of 1, 2, 3, or 4, depending on which matching it represents currently.

### How have we done it?

For each Style block, we _augment_ the Style block body with two artificial AST nodes, as if they were parts of the original Style program:

- One node visualizes to `match_total = <the total number of matches>`, and
- The other visualizes to `match_id = <the current ordinal of the matching>`.

Then we process the Style block as usual. Notice that now, `match_total` and `match_id` are assigned to the correct values and can be used later in the Style block as local variables.

## Inline Comparison Operators

We add syntactic sugars of `<`, `==`, and `>` to correspond to function calls `lessThan`, `equal`, and `greaterThan`. This makes Style-writing more convenient as it allows the users to write, for example,

```style
ensure circle1.r < circle2.r
```

and have it be treated as equivalent to

```style
ensure lessThan(circle1.r, circle2.r)
```

### How have we done it?

We modified the grammar for constraint and objective declarations with

```
<constr> ::= ensure <body> <staged layout>+
   <obj> ::= encourage <body> <staged layout>+

  <body> ::= <identifier> ( <expr_list> )  // Function Call
          |  <expr> <op> <expr>            // Inline Comparison

    <op> ::= ==                            // Equal
          |  <                             // Less Than
          |  >                             // Greater Than
```

In other words, the `<body>` of a constraint or objective now can either be a "Function Call" or an "Inline Comparison."

In the Style compiler where we handle constraints and objectives, we proceed as before when we encounter a "Function Call." When we encounter an "Inline Comparison," on the other hand, we handle it as if it is a "Function Call," with the corresponding function name (`lessThan`, `greaterThan`, or `equal`) and the two operands as parameters to the function.

### Caveats

Originally we wanted to implement operators `<=` and `>=`. However, there are no corresponding functions to these operators in the Penrose system. From an optimizer's perspective, in fact, `<=` and `<` are treated the same, and `>=` and `>` are treated the same.

## Conclusion

It has been an extremely joyful journey working on these features. These features, albeit basic, allow users to write clean, readable, and flexible Penrose programs that can draw more complex diagrams more easily. I'd love to continue this exploration of ways to improve programming language user experiences like in Penrose -- indeed, there are other exciting features that I worked on throughout these months which I can't wait to share in future blogposts!
