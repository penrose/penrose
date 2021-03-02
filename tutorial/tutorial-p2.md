# Part 2: Predicates & Constraints
In Penrose, we are not only given the power to represent mathematical objects with shapes, but we are also able to represent complicated relationships between the objects. In this tutorial, we will learn about defining `predicate` and visually represent `predicate` with the constraint keyword `ensure`. After this tutorial, you should be equipped to diagrams relationships between objects with Penrose. 

## Table of Contents
* [Tutorial Homepage](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial.md)
* [Goal](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#goal)
* [Domain File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#page_facing_up-domain)
* [Substance File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#page_facing_up-substance)
* [Style File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#page_facing_up-style)
* [Exercises](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#exercises)
* [Take-aways](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md#take-aways)
* [Next Tutorial: Functions](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md)

## Goal

![Goal Diagram for Part 2](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part2/goal.svg)

In the second example, we will work to diagram the relationship of a set is a **subset** of the other. The common way to visually represent A is a subset of B is by drawing A as a smaller circle that is fully contained in the circle that represents B. For those who doesn't know what a subset is, a short explanation would be A is a subset of B if and only if all the elements in A are in B. For example, the set of erasers would be a subset of the set of stationeries, since every single eraser is a stationary, thus being contained in the set of stationeries. 

## :page_facing_up: Domain
To illustrate the subset relationship, we have to expand our domain file to let Penrose know that an arbitrary set can be a subset of another arbitrary set. Remember in last example, we defined objects of type `Set` using `type Set`, and now we want to define a _**relationship**_ between two sets. 

To define a relationship between objects in the domain, there are a few things we need to decide on:
* Name of the relationship
* Numbers of arguments, i.e. how many objects are involves in this relationship
* The type of arguments, i.e. what are the type of objects that are involes in this relationship 

The syntax for declaring a relationship is through the use of keyword `predicate`, and then inputting the neccessary information to construct the predicate,

![predicate syntax](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part2/predicateSyntax.png)

For our relationship, it follows naturally that the name should be `IsSubset`, and we have 2 arguments of type `Set`. 

`setTheory.dsl`
```typescript
type Set
predicate IsSubset : Set s1 * Set s2
```

Now we are free to use the predicate `IsSubset` in our `.sub` and define what it means visually in our `.sty`. 

## :page_facing_up: Substance
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

## :page_facing_up: Style
The style program will be the most complex part, and you will see that it is normally this way developing with Penrose. In this example, we introduce a new keyword `ensure`, which allows you to constrain certain aspects of certain shapes. Essentially, we use `ensure` to let Penrose know that these are rules the diagram **must** satisfy. Hence, we also call `ensure` statements _constraints_. 

Recall that we learned about predicates that are defined in `.dsl` and used in `.sub`, and now we need to define the visual definition of the predicate. To show that A is a subset of B, we need the following visual characterstics: 
* A's circle needs to be smaller than B's circle
* A's circle needs to be contained within B'c circle (their borders should not be intersecting in any way) 
* A's circle is on top of B's circle

Therefore we call the corresponding `ensure` functions on the `.icon` fields (that we used to define the shape of objects in tutorial part 1) of two arbitray sets that have the relationship of `IsSubset`. 

Now our selector is not just `forall Set A` since we only want to apply these styling rules to the sets that have the relationship `isSubset`, therefore we need to condition on the arbitrary sets we are looping through in the program with the keyword `where HasRelationshipR(A, B)` where the `HasRelationshipR` is `IsSubset` for this particular case. 

`.sty`
```
forall Set A; Set B
where IsSubset(A, B) {
    ensure smallerThan(A.icon, B.icon)
    ensure contains(A.icon, B.icon, 5.0)
    A.icon above B.icon
}
```

Notice that in our first example, we did not care about the size of our shapes, but now we do care about the size since we want specific hiearchy of sizes, and in the process of forcing containments of the circles, the circles can get **_TOO BIG_** or **_TOO SMALL_**. So we need to specify a range of acceptable sizes for our circles so nothing goes crazy. 

![no ensures leads to trouble](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part2/no_ensures.png)

This is what might happen when you don't constrain the sizes. :imp:

Since we care about the sizes of **all** the sets, and need to **ensure** all of their sizes are within a reasonable range, we will again make use of our newly introduced keyword `ensure`. We call `ensure` on the fields of the object we want to make sure that are within reasonable range. Since we want to constrain the size of the shapes, we call `ensure MinSize(x.icon)` and `ensure maxSize(x.icon)`. 

`.sty`
```typescript
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
}
```
So putting it together, we have

`.sty`
```typescript
forall Set A; Set B
where IsSubset(A, B) {
    ensure smallerThan(A.icon, B.icon)
    ensure contains(A.icon, B.icon, 5.0)
    A.icon above B.icon
    
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure minSize(x.icon)
    ensure maxSize(x.icon)
}
```


## Exercises
* Define a predicate `Intersecting` that takes in two sets, and outputs 2 circles that overlap. 
![exercise 1 goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part2/e1c1.png)
* Define a predicate that is the opposite of `Intersecting` that takes in two sets, and outputs 2 circles that are disjoint. 
![exercise 2 goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part2/e1c2.png)
[Reference solutions](https://github.com/penrose/penrose/blob/docs-edit/tutorial/exercise2-sol.md)

## Take-aways
* We use the keyword `predicate` to define relationship between objects.
* We use the keyword `ensure` to define constraints on the diagram. 
