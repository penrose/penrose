# Type Declarations

A _type_ is a class of objects that a diagram works with. In the set-theory example, _Set_ is a type. The syntax for a type declaration is as follows:

```domain
type typename
```

This statement declares a type with name `typename`. After declaration, a type can be referred to in other parts of the Domain schema, as well as the [Style] and [Substance] schemas.

For example, statement `type Set` declares a type with name `Set`. We can then use the name `Set` to later refer to this type.

## Subtypes

Conceptually, a type might be a _subtype_ of another type. As an example, in a chemistry domain, types _Hydrogen_ and _Oxygen_ might both be subtypes of the type _Atom_. Penrose allows us to simultaneously declare a type and declare subtyping relations as follows:

```domain
type subtype_name <: supertype_name
```

Take the example of types _Hydrogen_, _Oxygen_, and _Atom_:

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
```

If the types are already declared beforehand, just omit the `type` keyword:

```domain
Hydrogen <: Atom
Oxygen <: Atom
```

Subtyping describes "is-a" relationships between types: a Hydrogen "is-a[n]" Atom; an Oxygen "is-a[n]" Atom. Using the "is-a" interpretation, since an Atom "is-a[n]" Atom, inherently, for all type `A`, we have that `A` is a subtype of `A`. This interpretation of subtyping is consistent with "inheritance" in many object-oriented programming languages.

Penrose adopts this interpretation: if Penrose expects to see an object with type `A`, then it will also accept an object of type `B`, as long as `B` is a subtype of `A`. We say that "`B` **matches** `A`."

[Style]: ../style/overview.md
[Substance]: ../substance/overview.md

## Literal Types

The _domain_ schema provides two built-in _literal types_, `String` and `Number`, which represent string and numerical literals. These types can be inferred from literal expressions in _substance_ and _style_.

These literal types can be referred to in other parts of the _domain_ schema without declaration as arguments to predicates, functions, and constructors. For example, a _domain_ schema about sets containing numbers can be described with

```domain
type Set
predicate Has(Set s, Number n)
```

Notice that `Number` is built-in so does not need to be declared.

### Restrictions on Literal Types

Since these types are built-in, other type names cannot conflict with these literal types. That is, type declarations like

```domain
type Number
type String
```

are disallowed.

Since these literal types have special properties, they cannot be supertyped or subtyped. That is, we disallow declarations like

```domain
type Set
Set <: Number
String <: Set
```

Finally, literal types cannot be outputs of functions or constructors since they cannot be explicitly constructed, only inferred from literal expressions in _substance_ and _style_. Hence we disallow declarations like

```domain
function addOne (Number n) -> Number
```
