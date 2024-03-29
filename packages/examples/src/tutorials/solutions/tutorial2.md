# Challenge 1:

Define a predicate `Intersecting` that takes in two sets and outputs 2 circles that overlap.

`.domain`

```
type Set
predicate Intersecting(Set s1, Set s2)
```

`.substance`

```
Set A
Set B
Intersecting(A, B)
```

`.style`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure x.icon.r > 25
    ensure x.icon.r < 150
}

forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.icon, y.icon, -15)
}
```

# Challenge 2:

Define a predicate that takes in two sets and outputs 2 circles that are disjoint.

`.domain`

```
type Set
predicate Disjoint(Set s1, Set s2)
```

`.substance`

```
Set A
Set B
Disjoint(A, B)
```

`.style`

```
canvas {
  width = 800
  height = 700
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure x.icon.r > 25
    ensure x.icon.r < 200
}

forall Set x; Set y
where Disjoint(x, y) {
    ensure disjoint(x.icon, y.icon, 15)
}
```
