# Part 3: Functions 

## Starter Code

`.dsl`
```typescript
type VectorSpace
type LinearMap
predicate In: Vector * VectorSpace V
predicate From: LinearMap V * VectorSpace domain * VectorSpace codomain
```

`.sub`
```typescript
VectorSpace U, V
LinearMap f
From(f, U, V)
```
