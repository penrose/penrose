# Namespaces

The syntax for a namespace is as follows:

```style
namespace_name {
    -- ... (the namespace body)
}
```

Refer to [this section](./block-body) for a detailed explanation of what may appear in the body of a namespace.

Values declared within a namespace can be read outside of the namespace using the "dot" operator:

```style
namespace_name.field_name
```

Hence they are also called _global_ variables. Overwriting these values is not allowed.

## Canvas Preamble Block

Each Style program _must_ contain a _canvas preamble block_, a special type of namespace which describes the width and height of the canvas. For example, preamble block

```style
canvas {
    width = 800
    height = 700
}
```

tells Penrose that the drawing canvas should have a width of 800 pixels and a height of 700 pixels.
