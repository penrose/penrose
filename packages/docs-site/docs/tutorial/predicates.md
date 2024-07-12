# Predicates & Constraints

In Penrose, we are not only given the power to represent mathematical objects with shapes, but we are also able to represent complicated relationships between the objects. In this tutorial, we will learn about defining **predicates**, and visually representing them with the constraint keyword `ensure`. After this tutorial, you should be equipped to create diagrams with relationships between objects in Penrose.

👉 <a href="/try/index.html?examples=tutorials%2Ftutorial2" target="_blank">**Open this online workspace**</a> in a separate tab to follow along!

## Goal

![Goal Diagram for Part 2](/img/tutorial/goal.svg)

In the second example, we will work to diagram a set that is a **subset** of another, i.e. "A is a subset of B". Recall that A is a subset of B if and only if all the elements in A are in B. For those who are not familiar with subsets, you can think of apples as a subset of fruits, because every apple is a fruit. In diagrams, the subset relationship is commonly represented by drawing A as a smaller circle that is fully contained in the circle that represents B.

## 📄 Domain

To illustrate the subset relationship, we have to expand our domain file to let Penrose know that an arbitrary set can be a subset of another arbitrary set. Recall that in Tutorial 1 we defined `Set` objects using `type Set`, and now we want to define a _**relationship**_ between two sets.

To define a relationship between objects in the domain, there are a few things we need to decide on:

- The name of the relationship
- The number of arguments, i.e. how many objects are involved in this relationship
- The type of arguments, i.e. what are the type of objects that are involved in this relationship

The syntax for declaring a relationship is through the use of keyword `predicate`, the name of the predicate, and the objects that are involved in the predicate:

```domain
predicate IsR(t1 var1, t2 var2)
```

(Here, `t1` is the type of `var1`, and `t2` is the type of `var2`.)

This pattern informs Penrose that there is a relationship that we care about called `IsR` which takes arguments of type `t1` and `t2`. Penrose still doesn't know how to visually represent this relationship, so we will have to define that logic in our `.style` file a little later.

In the case of our current example, we can name our relationship `Subset`, and we have 2 arguments of type `Set`.

```domain
type Set
predicate Subset(Set s1, Set s2)
```

Now we are free to use the predicate `Subset` in our `.substance` file and define what it means visually in our `.style` file.

## 📄 Substance

In our goal diagram, we have 3 sets, therefore we will declare 3 different sets in our `.substance` file. Note that in the previous example we declared each of our sets on separate lines, but we could also declare multiple objects of the same type in a single line. For instance, `Set A, B, C` is equivalent to writing:

```substance
Set A
Set B
Set C
```

So, we declare the set objects that will appear in our diagram, then we declare the relationships between the sets. In this case, let's make "B a subset of A" and "C a subset of B".

```substance
Set A, B, C
Subset(B, A)
Subset(C, B)
```

## 📄 Style

The style program will be the most complex part, and you find that this is expected when developing with Penrose. In this example, we introduce a new keyword `ensure`, which allows you to constrain certain aspects of certain shapes. Essentially, we use `ensure` to let Penrose know that these are rules the diagram **must** satisfy. Hence, we also call `ensure` statements _constraints_.

Recall that we learned about predicates that are defined in `.domain` and used in `.substance`, and now we need to define the visual definition of the predicate. To show that "A is a subset of B", we need the following visual characteristics:

- A's circle needs to be smaller than B's circle
- A's circle needs to be contained within B's circle (their borders should not be intersecting in any way)
- A's circle is on top of B's circle

Therefore we call the corresponding `ensure` functions on the `.icon` fields (that we used to assign shape objects to in tutorial 1) of two arbitrary sets that have the relationship of `Subset`.

Now our selector is not just `forall Set A` since we only want to apply these styling rules to the sets that have the relationship `Subset`. Therefore, we need to add a condition to the arbitrary sets we are looping through in the program. We can do this with the pattern `where HasRelationshipR(A, B)` where the `HasRelationshipR` is `Subset` in this particular case. Now our `.style` file looks like this:

```style
forall Set A; Set B
where Subset(A, B) {
    ensure contains(A.icon, B.icon, 5.0)
    A.icon above B.icon
}
```

`contains()` is a couple of the built-in constraints pre-defined by Penrose. A full list of available constraints is available in the function library documentation [here](/docs/ref/style/functions#constraint-functions).

Notice that in our first example, we did not care about the size of our shapes, but now we want to maintain a specific hierarchy between objects. While Penrose tries to satisfy all constraints, it is possible for the circles to become too big or too small. So we need to specify a range of acceptable sizes for our circles so nothing goes crazy.

![This is what might happen when you don't constrain the sizes. 👿](/img/tutorial/no_ensures.png)

Since we care about the sizes of **all** the sets and we need to **ensure** all of their sizes are within a reasonable range, we will again make use of our newly introduced keyword `ensure`. We call `ensure` on any fields of the object that we want to limit to within a certain range. In this case, we want to constrain the size of the shapes, so we add `ensure x.icon.r > 25`. This implicitly invokes `greaterThan`, which is another pre-defined constraint that Penrose provides. If you're curious, the documentation for this constraint can be found [here](/docs/ref/style/functions#constraint-functions) (scroll down to `greaterThan`). You can also see how Penrose implemented this constraint by clicking on the "Defined in" link.

```style
forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure x.icon.r > 25
}
```

So putting it all together, we have:

```style
forall Set A; Set B
where Subset(A, B) {
    ensure contains(A.icon, B.icon, 5.0)
    A.icon above B.icon
}

forall Set x {
    x.icon = Circle {
        strokeWidth : 0.0
    }
    ensure x.icon.r > 25
}
```

You can now compile our new code!

## Exercises

Complete the following exercises to practice implementing predicates in Penrose! As a reminder, you can find the documentation for all pre-defined constraints [here](/docs/ref/style/functions#constraint-functions).

- Define a predicate `Intersecting` that takes in two sets and outputs 2 circles that overlap.

![](/img/tutorial/e1c1.png)

- Define a predicate that takes in two sets and outputs 2 circles that are disjoint.

![](/img/tutorial/e1c2.png)

[Reference solutions](https://github.com/penrose/penrose/blob/main/packages/examples/src/tutorials/solutions/tutorial2.md)

## Take-aways

- We use the keyword `predicate` to define relationships between objects.
- We use the keyword `ensure` to define constraints on the diagram.
