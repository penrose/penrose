<script setup>
import ShapeProps from "../../../../src/components/ShapeProps.vue";
</script>

# Group

A group is a shape that contains multiple shapes, rendered under `<g>` tag. For example, one can write,

```
t.s1 = Circle {}
t.s2 = Rectangle {}
t.s3 = Text {
    string: "Hello"
}
t.g = Group {
    shapes : [t.s1, t.s2, t.s3]
}
t.s4 = Circle {}
t.g2 = Group {
    shapes : [t.s4, t.g]
}
```

and get something like,

```
<svg ...>
  <g>
    <circle ... />
    <g>
      <circle ... />
      <rect ... />
      <text .../>
    </g>
  </g>
</svg>
```

## Properties

<ShapeProps shape-name="Group" />

## Constructing a Group

A `Group` shape only contains one parameter, `shapes`, which is an _unordered_ list of paths to previously-defined shapes. In the example above, shape `t.g2` is a group that contains shape `t.s4` (a `Circle`) and shape `t.g` (another `Group`). As illustrated, `Group` shapes can be nested.

We prohibit inline shape declarations within groups. In other words,

```
-- this is bad
bad_group = Group {
    shapes: [ Circle {}, Rectangle {} ]
}
```

does not compile. All shapes within a group must be declared previously and referred to by their paths.

## Layering Semantics

Inherently, shapes within a group are to stay together and be drawn together (meaning rendered on consecutive layers). In other words, if Group `g` only contains `s1` and `s2`, then they must be drawn in the order of either `s1, s2` or `s2, s1`, consecutively.

Hence, the following layering should not be allowed (and will hence generate a warning),

```
s1 = // some shape
s2 = // some shape
g = Group {
  shapes: [s1, s2]
}
s3 = // some shape

layer s1 above s2
layer s3 above s2
layer s3 below s1
```

because the layer directives enforce the layer ordering of `bottom, s2, s3, s1, top` which breaks apart group `g` containing `s1, s2`.

To achieve this goal, `Group` shapes have the following layering semantics:

- Layering directives on a group applies also to all members of the group.
- Layering directives on a group member applies to the entire group too.

## Constraints and Objectives

The `Group` shape has a bounding box, taken as the smallest axis-aligned rectangle that contains the bounding boxes of all its member shapes. With this bounding box defined, some constraints and objectives (such as `contains`, `disjoint`, and `overlapping`) can work as well.

For example,

```
s1 = // some shape
s2 = // some shape

g = Group {
    shapes: [s1, s2]
}

s3 = // some shape

ensure contains(g, s3)
```

will ensure that group `g` (and hence both members `s1` and `s2`) is contained within `s3`.
