# Literal Expressions

[Domain] declares two built-in literal types, `Number` and `String`. Objects of these literal types can appear in Substance only as literal expressions:

- If a number literal (e.g., `1.234`, `5`, `-3.14159`) appears in Substance, it will be interpreted as an object of type `Number`.
- If a string literal (e.g., `"hello world"`) appears in Substance, it will be interpreted as an object of type `String`.

Explicit declarations like these of objects with these literal types are disallowed:

```substance
String s
Number n
```

## Using Literal Expressions as Arguments

If a predicate / function / constructor argument expects a literal type, then we need to provide it a literal expression. We illustate this using an example:

Suppose the _domain_ schema is as follows:

```domain
type Set
predicate HasNum(Set set, Number num)
predicate HasStr(Set set, String str)
```

Then, this is a valid _substance_ program

```substance
Set s1, s2
HasNum(s1, 1.234)
HasNum(s1, 2)
HasStr(s1, "Hello")
HasNum(s2, 5.678)
HasNum(s2, -5.678)
HasStr(s2, "world")
```

since numerical literals (`1.234`, `2`, `5.678`, and `-5.678`) have type `Number` and string literals (`"Hello"` and `"world"`) have type `String`.

## Literal Expressions in Indexed Statements

Within an [indexed statement][IStmt], if an identifier name coincides with the template variable name, Substance treats it as a numerical literal expression, like

```substance
NumberSet s
Contains(s, i) for i in [1, 10]
  -- ok: expands into Contains(s, 1), Contains(s, 2), ..., Contains(s, 10)
```

[Domain]: ../domain/overview.md
[IStmt]: ./indexed-statements.md
