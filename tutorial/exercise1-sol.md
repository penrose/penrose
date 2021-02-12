# Challenge 1: 
Add another `Set` to the diagram. So you should have 3 circles on your screen.
`.dsl`
```typescript
type Set
```
_With changes:_

`.sub`
```
Set A
Set B
Set C
```

`.sty`
```typescript
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
}
```

# Challenge 2:
Represent `Set` as squares with `width` equal to `3.0`.

# Challenge 3:
Represent `Set` as rectangels with `rotation` equal to 45 degrees. 

