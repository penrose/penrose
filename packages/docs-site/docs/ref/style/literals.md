# Literals

## Literals in Selector Header

### Declarations of Literals

Unlike in the _substance_ program, within a Style selector block, we can declare objects of literal types. Here is an example:

::: code-group

```style
forall Number n {}
forall String s {}
```

```substance
P(1)
P(-1)
Q("Hello")
Q("World")
```

```domain
predicate P(Number)
predicate Q(String)
```

:::

Here, the first Style block (`forall Number n {}`) will be invoked twice with `n -> 1` and `n -> -1`, and the second Style block (`forall String s {}`) will be invoked twice with `s -> "Hello"` and `s -> "World`.

### Uses of Literals

If an argument of the predicate expects an object of [literal type][LitType], then for convenience, in the selector, the style variable for that argument does not need to be explicitly declared (though it can). In this concrete example,

::: code-group

```style
forall Set s
where Has(s, n) {}
```

```domain
type Set
predicate Has(Set, Number)
```

:::

the Style variable `n` is automatically inferred to have type `Number` since per Domain, the second parameter of predicate `Has` expects a `Number` type. Hence, explicitly declaring `Number n` is not necessary in the Style header.

Furthermore, literal expressions can also appear directly in Style selectors. A literal expression in style selector will only match the same literal expression in Substance. For example,

::: code-group

```style
forall Set s
where Has(s, 1) {}
```

```substance
Set s1, s2
Has(s1, 1)
Has(s2, 2)
Has(s3, 1)
```

```domain
type Set
predicate Has(Set, Number)
```

The style selector block will only match twice, since only `s = s1` and `s = s3` satisfy the relation `Has(s, 1)`.

:::

## Literals in Selector Body

If a style variable `x` refers to some Substance literal, then within the Style block, accessing `x` directly gives the value of the Substance literal.

For example, consider the following set of programs:

:::code-group

```style
forall Number n
where P(n) {
    Circle {
        r: n
    }
}
forall String s
where Q(s) {
    Text {
        string: s
    }
}
```

```substance
P(123)
P(456)
Q("Hello")
Q("World")
```

```domain
predicate P(Number)
predicate Q(String)
```

:::

the above program will render the following shapes:

- a circle of radius 123 (for mapping `n -> 123`) and a circle of radius 2 (for mapping `n -> 456`)
- a text of string "Hello" (for mapping `s -> "Hello"`) and a text of string "World" (for mapping `s -> "World"`).

If a variable `xs` is a collection of substance literals (for now, only literal numbers), then accessing `xs` gives a vector of the corresponding values, without requiring a Collection Access expression.

If `x` refers to a Substance literal, then `nameof x` just returns a string representation of that literal.

[LitType]: ../domain/types.md#literal-types
