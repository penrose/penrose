# Selector Blocks

The body of a block contains declarations of variables, shapes, and the relationship between objects.

## Assignments

We can assign an expression to a field:

```style
type_annotation field = expression
```

where

- `type_annotation` is an optional field denoting the type of the variable,
- `field` is a path to the variable being assigned, and
- `expression` is the expression to be assigned to `field`.

`field` can either be

- A single identifier, which denotes a local assignment, not accessible outside of this matching; or
- An object name (defined in `list_object_declarations`) or predicate application alias, followed by a dot operator and an identifier, which denotes an assignment bound to a Substance instance of object or predicate application after we substitute in the mapping. These assignments are accessible if the same Substance object or predicate application is matched again.

For example, consider the following Style block:

```style
forall MyType t1; MyType t2
where MyPredicate (t1, t2) as r1 {
	x = -- this is a local assignment not accessible outside of this substitution or this block
	t1.a = -- this is bound to the substance instance of `MyType t1`
	r1.c =  -- this is bound to the substance instance of `MyPredicate (t1, t2)`
}
```

Refer to [this section](./expressions.md) for a detailed explanation of the available expressions and their associated types.

## Override and Deletion

The Style language allows users to modify fields that are previously declared. The `override` keyword changes the value of the field. As an example,

```style
forall Set X {
    shape X.shape = Circle {
        x: X.x
        r: 100
    }
}

forall Set `A` {
    override `A`.shape.r = 200
}
```

the radius of the circle for every `Set` is `100`, except if the `Set` has name `A`, then the radius is `200`.

Deletion of fields works similarly, with the `delete` keyword. This feature can be helpful for, e.g., removing visual elements for a subtype. For instance,

```style
-- by default, draw a circle for all instances of type T
forall T x {
    x.widget = Circle { }
}

-- but don't draw this circle for instances of a subtype S <: T
forall S x {
    delete x.widget
}
```

Note that one must be careful not to reference deleted attributes in a later generic block. For instance, the following block will produce an error if invoked for an instance of `S`:

```style
forall T x {
    shape x.newWidget = Circle {
        center : x.widget.center -- not defined for instances of S
    }
}
```

## Constraints and Objectives

A good diagram must satisfy some basic constraints, while trying to optimize upon some objectives (specifying diagram beauty). We declare these constraints and objectives within the style blocks. A constraint declaration has syntax

```style
ensure constraint_name (argument_list)
```

and an objective declaration has syntax

```style
encourage objective_name (argument_list)
```

where `argument_list` may refer to constant values, global / local variables, and other variables bound to _substnace_ instances of objects and predicate applications. A full list of available constraints and objectives can be found [here](./functions).

We also provide syntax sugar expressions for some commonly-used objectives and constraints. In particular,

- `a > b` is the syntax sugar for the constraint / objective `greaterThan(a, b)`,
- `a == b` is the syntax sugar for the constraint / objective `equal(a, b)`, and
- `a < b` is the syntax sugar for the constraint / objective `lessThan(a, b)`.

## Layering

We can specify the layering between two shapes (particularly useful when two shapes overlap) using layering statements: either

```style
layer shape_1 above shape_2
```

or

```style
layer shape_1 below shape_2
```

where `shape_1` and `shape_2` can be variables assigned to shapes.

We have special handling of layering statements for `Group` shapes, found [here](./shapes/group.md).
