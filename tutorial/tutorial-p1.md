# Part 1: Penrose Basics
This is the first diagram we will make together. This is the equivalent of the ```print("Hello World")``` program for Penrose.

## Table of Contents
* [Goal]()
* [Domain File]()
* [Substance File]()
* [Style File]()
* [Take-aways]()
* [Next Tutorial]()

![exercise 1 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/2sets_nolabel.png)

This is what you will achieve at the end of Example (1). 

### :speaking_head: WHAT IS THIS?
Some of you who have experiences with set theory may recognize that ellipses are common for representing sets, and that's exactly what we have here. We have 2 sets without names (we will get to labeling later :grimacing:).

### :page_facing_up: DOMAIN
It follows naΩturally that our mathematical **domain** is Set Theory. Therefore, we can rename our `.dsl` file to `setTheory.dsl`.  

Recall that a `.dsl` file defines the possible types of objects in our domain. Essentially, we are _teaching_ Penrose the neccessary vocabulary that we use to communicate our concept. For example, Penrose has no idea that there are objects of type plant or furniture in a house, but no worries, we can let Penrose know with several lines of code :speaker:. We declare a new type following the syntax of `type TYPE_NAME`. For example, if we want Penrose to know that there are objects of type plant, we would do `type Plant` or `type plant`. We normally capitalize type names. 

#### :question: POP QUIZ: What's the most fundamental type of element in Set Theory? (hint: the name gives it away.)

The answer is a **Set**! A set is a **type** of element in set theory. Therefore in our `setTheory.dsl`, we write the following line,

`setTheory.dsl`
```typescript
type Set
```
And that is all we need for this exercise in `.dsl`! :tada: Now Penrose knows that there are objects of type Set. 

If you look closely at the repository, we have a [domain file](https://github.com/penrose/penrose/blob/main/examples/set-theory-domain/setTheory.dsl) that contains more extensive operations common in Set Theory such as `Intersection`, `Union`, `Subset`, and more. 

### :page_facing_up: SUBSTANCE
Since we are visualizing 2 sets, they are our **substances** for this diagram. 

We declare a substance by first declaring its *type* followed by its *name*. The name will not appear in the diagram unless you choose to label your substances, therefore in this exercise, it doesn't matter how you name your sets. 

`twoSets.sub`
```typescript
Set A 
Set B 
```

Now, Penrose will know that you want two substances of type `Set` in your diagram. :tada: 

Here we have capitalized `Set` because recall in our `setTheory.dsl` file, we wrote `type Set`, and if we did `type set` instead, we would declare our set with `set A` here. There is no magic here, you define your Penrose world completely. :earth_americas: :sparkles:

### :page_facing_up: STYLE
For style, we have a little more work to do. A `.sty` file is essentially a `.css` file for your `html` (which wouold be our `.sub` file). We will rename our `.sty` file to `twosets.sty`. 

Now, Penrose does _not_ know a set is commonly represented as a circle. **We need to style our elements from scratch.** This might seem strange, but this way you are given absolute freedom in how you want to represent your substances in the diagram. Your set doesn't have to be a circle, it can be square, a rectangle, etc. But for this example, we will be representing sets as circles. 

The syntax for declaring styles goes like this,

![style syntax](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/syntax.png)

Note that the syntax is _very_ similar to the mathematical way of talking about objects, so it should be pretty natural for people with some backgrounds in math. If you don't, that's completely fine too! You can interpret the syntax as:  we go through the substances in the diagram, and _for all_ the substances of type `t` that we see, we apply the same styling as defined in the `{ }`. 

Here we have our `type t` as `Set`, and we want to all of our sets to be a circle. We can make that happen by setting the `.icon` field to a shape object. 

`twosets.sty`
```typescript
forall Set x {
    x.icon = (* some shape object *)
}
```

So, what are the shapes we can use? Currently, the system supports 12 different shapes, and you can find the specs for every shape [here](https://github.com/penrose/penrose/wiki/Shape-library). It is a page that you will visit frequently as you work in Penrose. 

![Circle Spec](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/circle_spec.png)

This is the specification for the shape **Circle**, and all the other shapes we have available are documented in the same way. You can see a sample diagram of how the shape will look like, and a table that lists out the different properties you can manipulate. 

When we construct the `Circle` object for our Set, we need to look back at our goal diagram. Our desired circles are strokeless, therefore we will set `strokeWidth: 0.0`.  For all the other properties that we did not specify, Penrose will choose the best value for them based on optimization, or in other words, Penrose will choose the best for you, so you do not have to worry.  

`twosets.sty`
```typescript
forall Set x {
    x.icon =  Circle {
        strokeWidth : 0.0
    }
}
```
And that's a wrap! :tada: 

### :building_construction: COMPILE
Now it's time to see all of our hardwork (drumroll please :drum:)!  To compile your Penrose programs (more detailed description [here](https://github.com/penrose/penrose/wiki/Getting-started)), you need: 
* two terminals opened both at the penrose root directory
* run `yarn start` in one to get the browser window to pop out
* run `roger watch twosets.sub twosets.sty setTheory.dsl` to send the files over to the server
* Refresh! 

# Exercise 
Now, you understand the differences between and usage of the `.dsl`, `.sub` and `sty` files. We have 3 challanges for you that will not require you to create new files, but only work within the existing files. **Hint:** Make use of the shape specs [here](https://github.com/penrose/penrose/wiki/Shape-library).
* **Challenge 1:** Add another `Set` to the diagram. 
![challenge 1 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/e1c1.png)

* **Challenge 2:** Keep 3 sets. Represent `Set` as squares with `side` equal to `50.0`. 
![challenge 2 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/e1c2.png)

* **Challenge 3:** Keep 3 sets. Represent `Set` as rectangles with `strokeWidth` equal to 15. 
![challenge 3 result](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part1/e1c3.jpeg)

After you are done, you can double check your work with [solutions](https://github.com/penrose/penrose/blob/docs-edit/tutorial/exercise1-sol.md).

## Congratulations on finishing your first Penrose diagram! Along the way, you learned about:
* For each Penrose diagram, we need three files. 
    * `.dsl` file defines the domain.
    * `.sub` file defines the substances of the particular diagram.
    * `.sty` file defines how we want to visually represent the mathematical objects. 
* We define a type of object in our domain with `type TYPE_NAME` in `.dsl`.
* We define the substances in our diagram by declaring its type and its variable name in `.sub`.
* We define the styles using the syntax `forall TYPE_NAME x { /* declarations */ }` in `.sty`. 

Now we are going to start our [second tutorial](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p2.md) that teaches a set of new skills, allowing you to create more intricate and complex diagrams with Penrose. Take a stretch or a little walk, and get ready to be one step closer to being a Penrose expert! :smile:
