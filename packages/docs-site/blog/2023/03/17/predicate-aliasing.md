<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Predicate Aliasing

<BlogMeta github="liangyiliang" date="2023-03-17" />

Sometimes it makes more sense for a value (or shape) to belong to a predicate than to belong to a specific object.

As an example, say we would like to draw a line between two `Atom`s whenever a `Bond` predicate exists between them. We could write,

```
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

Naturally, `bondLine` should belong to the predicate `Bond(a, b)`. Predicate aliasing, first implemented in July 2021 ([PR](https://github.com/penrose/penrose/pull/623)) and merged in July 2022 ([PR](https://github.com/penrose/penrose/pull/1066)), allows a Style block body to access the relevant predicates themselves using an alias. We can write,

```
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
