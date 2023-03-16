# Value Types

A _value_ in Penrose have the following categories:

- Numbers (type `FloatV`), constructed as either a numerical value or `?` signifying the unknown value to be determined during optimization (see [here](usage#unknown-scalar)).
- Booleans (type `BoolV`), constructed as either `true` or `false`.
- Strings (type `StrV`), constructed as text written in between double-quotes (like `"hello world"`), and concatenations (like `"\text{" + x.label + "}"`)
- Path data (type `PathDataV`), constructed as in [here](shapes/path#defining-an-svg-path-using-d).
- List of points (type `PtListV`), constructed using implicit casting from matrices or nested lists.
- Colors (type `ColorV`), constructed as in [here](usage#colors).
- List of numbers (type `ListV`), constructed as comma-separated numbers surrounded between square brackets, like `[1, 2, ?, 4, 5]`.
- Vectors (type `VectorV`), constructed as comma-separated numbers surrounded by parentheses, like `(1, 2, 3, ?, 5)`.
- Matrices (type `MatrixV`), constructed as vectors of vectors, like `((1, 2, 3), (4, 5, 6), (7, 8, 9))`.
- Tuples (type `TupV`), constructed as a pair of two numbers surrounded by brackets, like `{1, ?}`.
- List of lists (type `LListV`), constructed like `[[1, 2, 3], [4, 5, 6], [7, 8, 9]]`.
- List of shapes (type `ShapeListV`), constructed as a list of paths to previously-defined shapes. For example, `[t.shape1, t.shape2, t.shape3]` where `t.shape1`, `t.shape2`, and `t.shape3` are all previously-defined.

Many of the value types can also be results of computations among other types and shapes (see [details](usage#expressions-and-their-types)).

As of now, Penrose's types are not strictly enforced, except when they act as shape parameters (see [details](shapes-overview#strict-typing-on-shape-parameters)).
