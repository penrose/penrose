# Example (2)

## Insert goal diagram
In the second example, we will work to diagram the relationship of a set is a **subset** of the other. The common way to visually represent A is a subset of B is by drawing A as a smaller circle that is fully contained in the circle that represents B. For those who doesn't know what a subset is, a short explanation would be A is a subset of B if and only if all the elements in A are in B. For example, the set of erasers would be a subset of the set of stationeries, since every single eraser is a stationary, thus being contained in the set of stationeries. 

### :page_facing_up: :green_book: DOMAIN
To illustrate the subset relationship, we have to expand our domain file to let Penrose know that an arbitrary set can be a subset of another arbitrary set. Remember in last example, we defined objects of type `Set` using `type Set`, and now we want to define a _**relationship**_ between two sets. 

To define a relationship between objects in the domain, there are a few things we need to decide on:
* Name of the relationship
* Numbers of arguments, i.e. how many objects are involves in this relationship
* The type of arguments, i.e. what are the type of objects that are involes in this relationship 

The syntax for declaring a relationship is through the use of keyword `predicate`, and then inputting the neccessary information to construct the predicate,

![predicate syntax](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/predicateSyntax.png)

For our relationship, it follows naturally that the name should be `IsSubset`, and we have 2 arguments of type `Set`. 

`setTheory.dsl`
```typescript
type Set
predicate IsSubset : Set s1 * Set s2
```

Now we are free to use the predicate `IsSubset` in our `.sub` and define what it means visually in our `.sty`. 

### :page_facing_up: :black_square_button: SUBSTANCE
In our goal diagram, we have 3 sets, therefore we will declare 3 different sets in our `.sub`. Note that how we declared two sets in separate lines in the previous example, and another way to declare multiple objects with the same type would be `Set A, B, C`, where we first declare the type of objects follows by a list of variable names separated by commas. 
`Set A, B, C` is the equivalent of 
```typescript
Set A
Set B
Set C
```
Then we want to declare the relationship between the sets. 

`.sub`
```typescript
Set A, B, C
IsSubset(B, A)
IsSubset(C, B)
```

### :page_facing_up: :tophat: STYLE
The style program will be the most complex part, and you will see that it is normally this way developing with Penrose. 

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
where IsSubset(x, y) {
    ensure smallerThan(x.icon, y.icon)
    ensure contains(y.icon, x.icon, 5.0)
    x.icon above y.icon
}
```
