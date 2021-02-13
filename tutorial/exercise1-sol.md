# Challenge 1: 
Add another `Set` to the diagram. So you should have 3 circles on your screen.
`.dsl`
```typescript
type Set
```

`.sty`
```typescript
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
Keep 3 sets. Represent `Set` as squares with `side` equal to `50.0`.
`.sub`
```
Set A
Set B
Set C
```

`.dsl`
```typescript
type Set
```

_With changes:_
`.sty`
```typescript
forall Set x {
    x.icon = Square {
        side : 50.0
    }
}
```

# Challenge 3:
Keep 3 sets. Represent `Set` as rectangles with `rotation` equal to 45 degrees. 
`.sub`
```
Set A
Set B
Set C
```

`.dsl`
```typescript
type Set
```

_With changes:_
`.sty`
```typescript
forall Set x {
    x.icon = Rectangle {
        rotation : 45.0
    }
}
```

