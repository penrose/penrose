# Part 3: Functions 
So far, we've directly declared substances in our diagram, which can then have some relationship with other substances in the diagram by the use of predicates. 

Now we will introduce functions in Penrose, which allow us to compose atomic substances in the diagram and define __new__ substances based on __existing__ ones. It's a very powerful feature and you will find it super convenient in your journey as a Penrose developer :airplane: :rose:.

## Table of Contents
* [Tutorial Homepage](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial.md)
* [Goal](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#goal)
* [Domain File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-domain)
* [Substance File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-substance)
* [Style File](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-style)
* [Exercises](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#exercises)
* [Take-aways](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#take-aways)
* Next Tutorial: Coming Up! 

## Goal
We've had a good time with Set Theory, now we will move on to visualizing :sparkles: **vectors** :sparkles: that you have mostly likely encountered in your highschool physics or math class. 

In particular, we are visualizing vector addtion. Below is the goal diagram for this tutorial, which shows vector addition of vectors `v` and `w`, that results in a vector `u = v + w`.
![no dash addition](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/using/sum.jpeg)
At the end of the tutorial, you will be able to work with functions in Penrose to create objects from existing objects, enjoying the convenience of compositionality. 

## Starter Code
This example is a bit more involved than the previous tutorials but no worries! We are providing you some starter code to get things running :runner:. The starter code has some helpful constants to draw a vector space with its x and y axis. 
Download the [starter code here](https://github.com/penrose/penrose/tree/docs-edit/tutorial/code/tutorial3/starter-code).
![starter code image](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/vectorspace_wg.png)
You should compile the starter code and see a vector space labeled `U` with its x-axis and y-axis that looks like the above image. 

There are comments in the starter code that documents each block briefly. In this exercise we are mainly working on the `.sty` file, thus the given `.dsl` and `.sub` file contains almost everything we need. This will be a common occurence for you if you are a domain expert, crafting different visualizations for a domain. 

### :point_right: For more in-depth explanations on the starter code, we prepared a separate document [here](https://github.com/penrose/penrose/blob/docs-edit/tutorial/supplementary/tutorial3/starter-code-walk-through.md). 

## :page_facing_up: Domain
We will write our first function in Penrose together :dizzy:!

In Penrose, functions are declarative, just like everything else. Functions allow us to compose already defined elements into new elements. They only have inputs and outputs. We will add a new line to our `.dsl` file that defines `addV`, which adds two vectors. 

```typescript
/* new line in .dsl file */
function addV: Vector * Vector -> Vector
```
The syntax for function is similar to defining elementary objects and predicates. As usual we have the type keyword, which is `function` in this case at the front. Then we write the function name followed by a colon `:` that signifies what comes after is the input type. The input type can be multiple elements connected by `*`. Lastly, we write `->` before the output type. 

## :page_facing_up: Substance
There are no imperative functions in Penrose, only functional functions (And more broadly, Penrose is purely functional). When we have `y = f(x)`, it's read like `y` is defined as `f(x)`. 

The syntax for composing a new object using a function involves a new operator `:=`, which is the assignment operator. 

We write `y := f(x)` to define `y` as `f(x)`. Therefore we define a new vector by `Vector u := addV(v, w)`. Furthermore, we want `u` to be in our original vector space along with our existing vectors `v` and `w`, therefore we use the already defined predicate `In` on `u` and `U` by writing `In(u, U)`. 

```typescript
/* new lines in .sub file ******/
Vector u := addV(v, w)
In(u, U)
/* autolabel on the last line */
AutoLabel All
```
## :page_facing_up: Style
Now the real fun starts! 🖌️ But before that, we will get a bit theoretical and understand why Penrose is the way it is. 

> The goal of Penrose is to visualize mathematical relationships.

These relationships can be instantiated in **many** concrete ways. Therefore, Penrose does not include any built in numeric types. 

For example, when we define a Vector, we don't assign it concrete values (e.g. we don't write `Vector v := (1,2)`). Instead we simply declare the Vector and associate it with a `VectorSpace` using the `In` predicate. It's a good rule of thumb that we **DO NOT** touch concrete numbers until we get into the `.sty` file. 

There are two tasks we need to do in our style program on top of the starter code:
* [Task 1](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#task-1-vectors-in-vector-space): Draw vectors in a vector space
* [Task 2](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#task-2-vector-as-sum-of-two-existing-vectors): Draw vectors that are the sum of two existing vectors 
Task 1 involves more lines of code but it should be pretty easy to follow (if you followed through the previous tutorials), and Task 2 teaches how to do simple computations for objects that are composed by existing objects. 

### Task 1: Vectors In Vector Space
Every vector exists in a vector space, and we draw them at the origin of the specific vector space it belongs to. In the given `.dsl` file, you will find a defined predicate called `In` that takes in a vector and a vector space. The way we work with `In` is very similar to the `isSubset` predicate we have worked with in tutorial 2. 

We start with writing the selector, selecting vectors that are in a vector space.
```typescript
forall Vector u; VectorSpace U
where In(u, U) {
  /* draw a vector in vector space */
}
```

Similar to what we did in the previous tutorials with sets. We use Penrose to visualize abstract relationships between objects. The same idea of abstraction applies to our vectors. We do not have specific values in mind for the vectors, and we want Penrose to decide for us. To do that, we use the `?` syntax to let Penrose know that we will be happy with an optimized value. Therefore we write,

```typescript
u.vector = (?, ?)
```

Next, vectors are commonly visually represented by single-head arrows ➡️, where the dull end is anchored at the origin, and the arrow head points at the vector position in space. Therefore we will need to assign some field of `u` to an arrow shape object to draw an arrow on the screen. 

```typescript
u.shape = Arrow {
  start: U.origin 
  end : U.origin + u.vector 
  thickness : 3.0 
  color : const.lightBlue /* or any color you want */
  arrowheadSize : const.arrowheadSize /* feel free to play with other values */
}
```
Note that the field name `shape` can be replaced by anything you want, we just need to assign the penrose shape object to some field (remember in tutorial 1 we used `.icon`). Here we are simply defining some properties of the `Arrow` shape object. 

🗒️ Side note: **3.0 is not the same as 3**. This is important when you need a **float** versus an **int**. We often write **3.**, thus ignoring the zero after the decimal point for convenience. 

Lastly, we need a field to write the variable name of our vector in the diagram.
```typescript
u.text = Text {
  string : u.label
  color : u.shape.color /* this way it changes accordingly when we change the arrow's color */
}
```

Just one more step for this task! We will need to place some constraints on how we draw the diagram. Think about drawing a diagram like our goal diagram by hand and check the following to see if you've catched everything we need to watch out for:
* Vector is indeed inside the vector space
* The name of our vector is beside our vector, and is inside the vector space
* The name of our vector does not get covered by the 2 axes

So we write the following lines to let Penrose know the above:
```typescript
ensure contains(U.background, u.shape)
ensure contains(U.background, u.text)
ensure atDist(u.shape, u.text, 15.0)
ensure minSize(u.shape)

layer u.text above U.xAxis
layer u.text above U.yAxis
```
🔥 Yes! We are halfway there. Now you should see something similar to the following diagram. 
![2 vectors](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/2vectors_wg.png)
[Complete code for drawing vector in vector space](https://github.com/penrose/penrose/blob/docs-edit/tutorial/code/tutorial3/complete-code/vector.sty#L69)

### Task 2: Vector As Sum of Two Existing Vectors
Again, we start with writing a selector. Here we have a bit more selection to do, since we have 3 vectors and 1 vector space involved. Furthermore, we want make sure that both `u,v,w` are indeed in the same vector space. Therefore, our selector will be the following,
```
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U)
```
Now we will move onto actually visualizing the vector addition. 

When we manually add 2 vectors, we add their x values and y values to get the new vector's x, y values. The new vector already has some x, y values chosen by Penrose for optimization, therefore we will need to override the existing values using the `override` keyword. (Every vector in a vector space is anchored at the origin, therefore we are only changing the end point of any vector. )

Since Penrose supports vector in the style program (read about more features [here](https://github.com/penrose/penrose/wiki/Style-language-spec#vectors-and-matrices)), to get the sum of two vectors we simply need to add them using the `+` operator. But don't forget that all the vectors are offsetted by the origin vector, therefore we also need to subtract the origin from our sum. 

```typescript
/* new lines in .sty file */
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U) {
  override u.shape.end = v.shape.end + w.shape.end - U.origin
}
```
This is easier than you expected, right? 😄 You're all done with this tutorial! Take a sip of water and come back to solifidy your knowledge with the following exercises. 🌴

[Complete code for the example.](https://github.com/penrose/penrose/blob/docs-edit/tutorial/code/tutorial3/complete-code/vector.sty#L69)

## Exercises
_FYI: All the sample goal diagrams are only a single sampling of many possible variations._

We follow the convention of `u` being the resultant vector, and use `v, w` for input vectors. 
* **Exercise 1:** Create a new function that computes vector subtraction, and draws the difference vector, i.e. `u = v - w`.
![Exercise 1 Goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/using/difference.jpeg)
* **Exercise 2:** Create a new function that computes scalar multiplication, and draws the scaled vector, i.e. `u = a * v` where `a` is a scalar. Start with defining a new type `Scalar`, and define the function accordingly. Wtih styles, first try having a fixed valued scalar, eg. let the scalar be `5.0`. Then make use of `?` and constraints to have arbitrary scalars. Hint: `inRange` is a helpful function, and `(2, 5)` is a nice bound. 
![Exercise 2 Goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/using/scaled.jpeg)
* **Exercise 3:** Add more lines of code to the `addV` block in style file to visualize the Parallelogram method of vector addition.

📏 *Parallelogram Method Steps*: 
  * Anchor the two vectors `v, w` at the same origin. This is already been done by how we draw our vectors at the origin. 
  * Draw a line (often dashed) from the tail of `v` to the tail of `w`, now you have a parallelogram. 
  * The diagonal from the origin point to the new connected point of `v, w` will be the sum vector's tail position. 
  * Read more on parallelogram method [here](https://www.varsitytutors.com/hotmath/hotmath_help/topics/adding-and-subtracting-vectors).
![Exercise 3 Goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/using/parallelogram.jpeg)

[Sample Solutions](https://github.com/penrose/penrose/tree/docs-edit/tutorial/code/tutorial3/complete-code)

## Take-aways
#### 🧑‍🎨 Principle of Styling:
* Define base objects.
  * In our Exercise 2, we first defined `Scalar`, and defined their values in the style program. Then we could use `Scalar` in a function with `Vector`, and we can access the scalar's value to actually scale a vector! Here `Scalar` and `Vector` are the atomic elements that we can play with and build more complex objects. 
* Casecase styles.
  * Very similar to CSS, we use selectors and inherit stylings from the atomic elements. When we defined new vectors from `addV`, the vector with it's new tail value still has the same styling as other vectors. 
* Override if needed. 
  * Penrose is *purely* functional, i.e. every value that is set is supposedly fixed. Therefore if you want to reassign a value, you *need* to use the `override` keyword. 
#### 💭 Substance is abstract! No numbers!
* The visual meaning is in the style program, including all the concrete numerical things. We have built-in types such as `scalar, color, ...` in our style program for readability. Currently we don't support type checking. 
* Substance program is **infinitely** flexible. ***Nothing is built-in.***
* Define your world in the domain file. Domain program is **infinitely** flexible. ***Nothing is built-in.***
#### 🖋️ New Syntax:
* `.dsl`
  * `function funcName : inputType -> outputType` is the syntax for defining functions in `.dsl`.
* `.sub`
  * `:=` is the assignment operator for composing objects. 
* `.sty`
  * `?` means undetermined value that will be decided by Penrose for optimization.
  * We can do computation inside `.sty` to draw objects in relation to some data. 
