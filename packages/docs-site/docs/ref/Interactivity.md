# Interactivity (experimental)

The editor supports interactivity in two forms:

- **Edit Mode**: translation and scaling of most shapes for last-minute adjustments
- **Play Mode**: constrained dragging for prototyping interactivity

## Edit Mode

When Edit Mode is enabled in `Settings -> Interactive Mode -> Edit Mode`, certain shapes will become selectable:

![](/img/docs/interactivity/selecting.gif)

Once a shape is selected, you can translate it by dragging:

![](/img/docs/interactivity/translating.gif)

You can also scale the shape by dragging the resize handles:

![](/img/docs/interactivity/scaling.gif)

Once translated or scaled, the shape will be "pinned", indicated by a red bounding box, and the optimizer will
not attempt to optimize its position or size. To unpin or pin a shape manually, right-click:

![](/img/docs/interactivity/pinning.gif)

Not all shapes are translatable or scalable. In order to be translatable, both of the center coordinates must be sampled values.
Otherwise, there is no input to the optimization problem that dragging could affect. For example the following three shapes would be translatable:

```style
c1 = Circle {}  // `center` is by default sampled
c2 = Circle {
    center: (?, ?)
}

center3 = (?, ?)
c3 = Circle {
    center: center3
}
```

while the following two would not:

```style
c4 = Circle {
    center: (?, 0)
}

center5 = (?, ?)
c5 = Circle {
    center: center + (0, 10)
}
```

For `Line` shapes, all coordinate of `start` and `end` must be sampled,
and for `Polygon` and `Polyline` shapes, all coordinates for all points must be sampled.
Similarly, a shape is only scalable if `width`/`height`, `rx`/`ry`, or `r` are sampled values, depending on the shape type. Paths and groups are not currently interactive.

## Play Mode

Play Mode is enabled in `Settings -> Interactive Mode -> Play Mode`. In Play Mode, you can drag shapes around the canvas
(subject to the same conditions as Edit Mode), but without a bounding-box widget. However, only shapes that are given a `draggingConstraint` attribute will
become draggable:

```style
c1 = Circle {
    -- draggable anywhere on canvas
    draggingConstraint: "return [x, y]";
}

c2 = Circle {
    -- draggable only in the top-right quadrant
    draggingConstraint: "return [Math.max(0, x), Math.max(0, y)]";
}
```

`draggingConstraint` is the body of a javascript function that takes in the mouse `x` and `y` positions, and returns
the desired position of the object. If you would like to constrain the dragging to certain region, this function should
compute a projection onto that region. The above constraints could be used to generate a diagram like the following:

![](/img/docs/interactivity/play-mode.gif)
