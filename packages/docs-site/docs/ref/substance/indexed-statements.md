# Indexed Substance Statements

The _substance_ language allows users to define indexed expressions that expand into multiple statements. An indexed statement is a single substance statement (as described above) with templated identifiers and an indexing clause. A common example is declaring an indexed set of variables:

```substance
Vector v_i for i in [0, 2]
-- is equivalent to Vector v_0, v_1, v_2
```

Templated identifiers (like `v_i` in the example) are regular _substance_ identifiers consisting alphanumeric characters and underscores, except that the last underscore and the substring following it denote an indexed variable. In the example, `_i` in `v_i` denotes an index variable `i` taking on the values in the range of `[0, 2]`, i.e. `0, 1, 2`. This line of code then gets expanded into three statements `Vector v_0`, `Vector v_1`, and `Vector v_2`.

:::info
The phrase `i in [x, y]` for some `x` and `y` requires that `i` is an integer, `i` $\geq$ `x`, and `i` $\leq$ `y`. As such, expressions like `Vector v_i for i in [3, 0]` has no effect, since there is no integer `i` that is at least `3` and at most `0`.
:::

When templated identifiers collide with regular identifiers, the latter is shadowed in indexing statements:

```substance
Vector v_i, v_j, vec1

Vector v_i for i in [0, 10]
  -- ok: expands into v_0, v_1, v_2, ..., v_10

Vector v_j for i in [0, 1]
  -- error: the range of `j` is not defined by `i in [0, 1]`
  -- notice that `v_j` here does not refer to the `v_j` defined above
  -- since `v_j` occurs in an indexed statement.

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

## Conditional Filtering

Sometimes, we don't want to iterate through all possible combinations, since some combinations are undesirable. _Substance_ allows users to filter the combinations using a Boolean expression in the `where` clause. _Substance_ would discard all combinations that make the Boolean expression false.

```substance
Vector v_i for i in [0, 10] where i % 2 == 0
  -- even indices: 0, 2, 4, 6, 8, 10

Orthogonal(v_i, v_j) for i in [0, 2], j in [0, 2] where i <= j
  -- triangular range: [0, 0], [0, 1], [0, 2], [1, 1], [1, 2], [2, 2]

Orthogonal(v_i, v_j) for i in [0, 3], j in [0, 3] where i + 1 == j
  -- consecutive pairs: [0, 1], [1, 2], [2, 3]

Edge(v_i, v_j) for i in [0, 4], j in [0, 4] where j == (i + 1) mod 5
  -- cyclic pairs: [0, 1], [1, 2], [2, 3], [3, 4], [4, 0]

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
- Binary numerical operators (`+` for plus, `-` for minus, `*` for multiplication, `/` for division, either `%` or `mod` for modulo, and `^` for power) between two numerical expressions.

:::warning

Because of an internal tokenizer bug, expressions like `+1` are always parsed as one single token `+1` denoting the integer "positive one", instead of two tokens `+` and `1`, regardless of their locations in the program. Similarly, `-1` is always parsed as a single `-1` instead of `-` and `1`.

As such, expressions like `2+1` are always interpreted as `2` and `+1` instead of the expected `2`, `+`, and `1`; the same occurs for `2-1` which is interpreted as `2` and `-1` instead of the expected `2`, `-`, and `1`. In other words, they are interpreted as two numbers side-by-side instead of a number, an operator, and another number. This bug causes errors like

```error
Error: Syntax error at line 1 col 39:

  Node n0_i for i in [0,15] where i == 2+1
                                        ^
Unexpected int_literal token: "+1".
```

since the `+` operator is absorbed into the token `+1` so the parser can no longer find the `+` operator.

This bug has been documented [here](https://github.com/penrose/penrose/issues/1516).

The workaround to this bug is to always put spaces around the `+` and `-` operators, writing expressions like `2 + 1`, `n - 1`, etc., unless signed numbers like `-1` and `+3` are specifically required.
:::

The default order of operations is the same as other programming languages, and parentheses can be used to override the default order of operations.

## Duplications

An indexed statement generates a list of _substance_ statements. The existing semantics on duplicates apply to them as well.

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

## Accessing Individual Elements of an Indexed Set

An indexing statement generates identifiers by replacing index variables with strings of integer values. Therefore, generated identifiers can be used just like regular identifiers as long as they exist:

```substance
Vector v_i for i in [0, 2]
LinearlyDependent(v_0, v_2) -- ok
```
