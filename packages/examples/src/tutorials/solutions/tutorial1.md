# Challenge 1:

Add another `Set` to the diagram. So you should have 3 circles on your screen.

`.dsl`

```
type Set
```

`.sty`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
}
```

_With changes:_
`.sub`

```
Set A
Set B
Set C
```

# Challenge 2:

Keep 3 sets. Represent `Set` as squares with side length equal to `50.0`.

`.sub`

```
Set A
Set B
Set C
```

`.dsl`

```
type Set
```

_With changes:_
`.sty`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Rectangle {
        width : 50.0
        height : 50.0
    }
}
```

# Challenge 3:

Keep 3 sets. Represent `Set` as rectangles with `strokeWidth` equal to 15.

`.sub`

```
Set A
Set B
Set C
```

`.dsl`

```
type Set
```

_With changes:_
`.sty`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Rectangle {
        strokeColor : sampleColor(0.5, "rgb")
        strokeWidth : 15.0
    }
}
```

# Challenge 4

Keep 3 sets. For each set, represent `Set` as both a `Circle` and a square.

`.sub`

```
Set A
Set B
Set C
```

`.dsl`

```
type Set
```

_With changes:_
`.sty`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.side = ?
    x.circle = Circle { }
    x.square = Rectangle {
      width : x.side
      height : x.side
    }
}
```
