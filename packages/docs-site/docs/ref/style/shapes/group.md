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

A `Group` shape has a property, `shapes`, which is an _unordered_ list of paths to previously-defined shapes. In the example above, shape `t.g2` is a group that contains shape `t.s4` (a `Circle`) and shape `t.g` (another `Group`). As illustrated, `Group` shapes can be nested.

We prohibit inline shape declarations within groups. In other words,

```
-- this is bad
bad_group = Group {
    shapes: [ Circle {}, Rectangle {} ]
}
```

does not compile. All shapes within a group must be declared previously and referred to by their paths.

We follow the convention that a shape cannot be contained within multiple groups. If Penrose detects that multiple groups contain the same shape, it will fire a warning and attempt to find the "best" grouping that does not have this issue.

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

## Clipping

A `Group` shape also has an optional `clipPath` property that allows shapes within the group to be clipped by another shape, analogous to the `clipPath` property in SVG (see [link](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/clipPath)). Parts of a `Group` shape that lie outside of the region bounded by the clipping shape are not drawn.

In Penrose, the `clipPath` property can take in two types of values:

- `noClip()` denotes that this `Group` shape is not clipped by any other shape.
- `clip(someShape)` denotes that this `Group` shape is clipped by `someShape`, which, just like other group members, must be declared previously and referred to by its path.

As an example, to define a `Group` shape with members `s1` and `s2`, clipped by `s3`, one can write,

```
g = Group {
  shapes: [s1, s2]
  clipPath: clip(s3)
}
```

A shape that is used to clip a `Group` is conceptually a member of that group. So the prohibition of multiple groups having the same shape as their member still stands.

## Bounding Box

The bounding box of a `Group` shape that is not clipped by any other shape is computed as the smallest axis-aligned rectangle that contains the bounding boxes of all its member shapes.

If this group is clipped by another shape, then the bounding box of the clipped `Group` shape is computed as the intersection of the following two bounding boxes:

- The bounding box of the `Group` shape, excluding the clipped shape, and
- The bounding box of the clipping shape.
