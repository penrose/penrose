# Function and Constructor Declarations

In mathematics, we sometimes define functions that takes some inputs and returns some output. Penrose allows us to define _functions_ that behave similarly:

```domain
function function_name (argument_list) -> output_type
```

where

- `function_name` declares the name of the function, which can be referred to by the [Substance] and [Style] schemas;
- `argument_list` is a list of the types of inputs that this function accepts, similar to the argument list in predicate declarations; and
- `output_type` represents the type of the output of the function. When we _assign_ the result of the function to an object in the [Substance] and [Style] schema, the output of the function must match the type of the assigned object (either the types are exactly the same, or the output type is a subtype of the assigned object type).

For example, in the linear-algebra domain, we can define

```domain
type Vector
function addVector (Vector v1, Vector v2) -> Vector
```

_Constructors_ in Penrose are functionally equivalent to functions, with the same declaration syntax, except for the first word:

```domain
constructor constructor_name (argument_list) -> output_type
```

Optionally, where the constructor name is the same as the output type, constructors may omit the output type:

```domain
constructor constructor_name (argument_list)
```

[Style]: ../style/overview.md
[Substance]: ../substance/overview.md
