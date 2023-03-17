<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Binary Symmetric Predicates

<BlogMeta github="liangyiliang" date="2023-03-17" />

In July 2022, we implemented support for binary symmetric predicates which allowed for more flexible style-substance matching. By marking a binary predicate with the `symmetric` keyword, the Domain author basically tells Penrose that it does not care about the order of input to the predicate, just like a symmetric mathematical relation. The style-substance matcher can exploit this symmetric property to match more objects in a more natural way.

## Problem

In January 2022, Professor Keenan Crane pointed out, in [an issue](https://github.com/penrose/penrose/issues/744), an interesting example where symmetric predicates can help. Consider the Domain program

```
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom

predicate Bond(Atom, Atom)
```

Under this Domain, one can construct a water molecule in multiple different ways, two examples being

```
-- version 1
Hydrogen H1, H2
Oxygen O
Bond(O, H1)
Bond(O, H2)
```

and

```
-- version 2
Hydrogen H1, H2
Oxygen O
Bond(H1, O)
Bond(O, H2)
```

Now suppose the Style write wants to enforce that the angle between the two bonds in a water molecule is 104.5 degrees. They can write,

```
forall Oxygen o; Hydrogen h1; Hydrogen h2
where Bond(o, h1); Bond(o, h2) {
  -- enforce that the
}
```

But this pattern will only match `version 1` of the water molecule. It does not match `version 2`, because according to the present matcher logic,

- `Bond(o, h1)` does not match `Bond(H1, O)`, because
- `o` does not match `H1` and `h1` does not match `O`, because
- `Oxygen` (declared type of `o`) does not match `Hydrogen` (declared type of `H1`) and `Hydrogen` (declared type of `h1`) does not match `Oxygen` (declared type of `O`).

But, intuitively, `Bond(o, h1)` should match `Bond(H1, O)`, since `Bond` is understood as a symmetric relation. That is, `Bond(H1, O)` should be understood as mathematically equivalent to `Bond(O, H1)`, which would match `Bond(o, h1)`. But since Penrose has no knowledge of the Domain except those that the user has provided, it does not know about the `Bond` predicate being symmetric, so it cannot make such an inference.

Without symmetric predicates, the solution would be to duplicate the code.

## Solution

We [implement](https://github.com/penrose/penrose/pull/1061) support for "binary symmetric predicates." For the previous example, the Domain author can write,

```
symmetric predicate Bond(Atom, Atom)
```

Then, when the style-substance matcher tries to match the Style predicate `Bond(o, h1)` against the Substance predicate `Bond(H1, O)`, it will exploit the symmetric property of the predicate to consider `Bond(O, H1)` which will successfully match, and generate the mapping `{o -> O, h1 -> H1}`.

## Implementation Caveats and Bugfix

The first implementation of symmetry relies on the following algorithm:

```
match_predicates(style_predicate, substance_predicate) {
  mapping_raw = match_raw(style_predicate, substance_predicate)
  if success:
    return mapping_raw
  else:
    substance_predicate_sym = // flip the arguments of substance_predicate
    // For example, if substance_predicate is Bond(H1, O)
    // then substance_predicate_sym is Bond(O, H1)

    mapping_sym = match_raw(style_predicate, substance_predicate_sym)
    if success:
      return mapping_sym
    else:
      return fail
}
```

In other words, the algorithm would attempt to match the original substance predicate (`Bond(H1, O)`). Only if the matching fails does it attempt to match the predicate with flipped arguments (`Bond(O, H1)`).

But sometimes, even if matching on the original substance predicate succeeds, we still actively seek for the alternative version with flipped arguments. As an example, if `Equal` is a symmetric predicate between two `Set` objects, one can write the Substance program

```
Set A, B, C
Equal(B, A)
Equal(B, C)
```

and the Style program

```
forall Set x, y, z
where Equal(x, y); Equal(y, z) {
  -- some code
}
```

In this example, `Equal(x, y)` already successfully matches against `Equal(B, A)`, generating `{x -> B, y -> A}`, so the matcher does not even consider the alternative `Equal(A, B)`. However, this matching requires `y -> A` which would not allow the other predicate, `Equal(y, z)`, to match against `Equal(B, C)`. If the alternative was considered, one can realize that matching `Equal(x, y)` against `Equal(A, B)` allows the matcher to match `Equal(y, z)` against `Equal(B, C)`. In this case, even though `Equal(B, A)` is present in the Substance program, we actively seek for `Equal(A, B)` instead.

This bug was first realized by Nimo and written up by me in [this issue](https://github.com/penrose/penrose/issues/1126) in October 2022. The [fix](https://github.com/penrose/penrose/pull/1127) involves requiring the predicate matcher to always attempt _both_ versions of the symmetric predicate:

```
match_predicates(style_predicate, substance_predicate) {
  mapping_raw = match_raw(style_predicate, substance_predicate)

  substance_predicate_sym = // flip the arguments of substance_predicate
  mapping_sym = match_raw(style_predicate, substance_predicate_sym)

  result = []
  if mapping_raw succeeds:
    result.push(mapping_raw)
  if mapping_sym succeeds:
    result.push(mapping_sym)

  return result
}
```
