# Challenge 1: 
Define a predicate `Intersecting` that takes in two sets, and outputs 2 circles that overlap.

`.dsl`
```typescript
type Set
predicate Intersecting : Set s1 * Set s2
```

`.sub`
```
Set A
Set B
Intersecting(A, B)
```

`.sty`
```typescript
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
}

forall Set x; Set y
where Intersecting(x, y) {
    ensure overlapping(x.icon, y.icon)
}
```

# Challenge 2:
Define a predicate that is the opposite of Intersecting that takes in two sets, and outputs 2 circles that are disjoint.

`.dsl`
```typescript
type Set
predicate Intersecting : Set s1 * Set s2
```

`.sub`
```
Set A
Set B
NotIntersecting(A, B)
```

`.sty`
```typescript
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
}

forall Set x; Set y
where Not(Intersecting(x, y)) {
    ensure disjoint(x.icon, y.icon)
}

```
