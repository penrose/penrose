# Part 3: Functions 
So far, we've directly declared substances in our diagram, which can then have some relationship with other substances in the diagram by the use of predicates. 

Now we will introduce functions in Penrose, which allow us to compose atomic substances in the diagram and define _new_ substances based on _existing_ ones. It's a very powerful feature and you will find it super convenient in your journey as a Penrose developer :airplane: :rose:.

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
We've had a good time with Set Theory, now we will move on to visualizing vectors in Linear Algebra :sparkles:. 

In this tutorial, you do not need any advanced Linear Algebra knowledge. We are simply adding 2 vectors together to get a new vector. 
![no dash addition](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/without_dash.png)
At the end of the tutorial, you will be able to work with functions in Penrose to create objects from existing objects, enjoying the convenience of compositionality. 

## Starter Code
This example is a bit more involved than the previous tutorials but no worries! We are providing you some starter code to get things running :runner:. The starter code has some helpful constants to draw a vector space with its x and y axis. 
Download the [starter code here](https://github.com/penrose/penrose/tree/docs-edit/tutorial/starter-code/tutorial-p3).
![starter code image](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/vectorspace.png)
You should compile the starter code and see a vector space labeled `U` with its x-axis and y-axis. 

Now we are going to spend some time going over the starter code. There are comments in the starter code that documents each block briefly. In this exercise we are mainly working on the `.sty` file, thus the given `.dsl` and `.sub` file contains almost everything we need. This will be a common occurence for you if you are a domain expert, crafting different visualizations for a domain. 

#### :point_right: Read along for more in-depth explanation on the starter code, or [skip to the example](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-domain).

### :runner: Domain
```typescript
type VectorSpace
type Vector
predicate In: Vector * VectorSpace V
```
Here we have 2 types, 1 predicate. Together we will define a new function. Furthermore, we have implemented drawing a vector space for you and we will together draw a vector in a vector space and vector addition. 

### :runner: Substance
```typescript
VectorSpace U
Vector v 
Vector w
In(v, U)
In(w, U)
```
The substance file contains the lines for putting two vectors in the same vector space `U`. Later we will compose a new vector as the sum of the two existing vectors. 

### :runner: Style 

The style file is a bit more involved, therefore we provide a very detailed step through of the code. __Again, feel free to [skip to the example](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-domain) if you feel confident reading and comprehending the code by yourself__.

On the very top of the file, you see this block of code. 
```typescript
const { 
  scalar vectorSpaceSize = 350.0
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  scalar arrowThickness = 1.5
  gray = rgba(0.6, 0.6, 0.6, 1.)
  none = rgba(0., 0., 0., 0.)
}
```
This is helpful way and a good practice to organize your constants in an object. Here we have some constants that helps with drawing out the vector space. Feel free to add more useful constants in here for your explorations.

The real fun starts! 🥁 To draw a vector space, we have a background, an origin, x-axis and y-aixs. 

* __Background__
```typescript
U.background = Square {
    center : U.origin
    side : const.vectorSpaceSize
    color : const.lightGray
    strokeColor : const.none
}
 ```
The background is a simple square :white_large_square:. Feel free to add a new constant to the `const` object and assign a new color to the background. 

* __Origin__
```typescript
scalar axisSize = const.vectorSpaceSize / 2.0
vec2 U.origin = (0., 0.)
vec2 o = U.origin /* just so we don't need to type U.origin everytime */
U.axisColor = const.gray
```
For any vector space, we need an origin, and everything else will be centered on the origin. 

* __Axis__
The x-axis and y-axis have each of their center on the origin of the vector space, and extends out to opposing directions. It follows that their length is half of the length of vector space (which is a square). 

An axis is a line with arrowheads on both ends. As defined by the code above, `axisSize = vectorSpaceSize / 2.0`, and the center of the axis line is at the origin. Therefore we have the start `x` value as origin's `x` subtracted by the axis size, and the end `x` value as origin's `x` added by the axis size. 

```typescript
U.xAxis = Line {
    start : (o[0] - axisSize, o[1]) 
    end : (o[0] + axisSize, o[1])
    thickness : const.lineThickness
    style : "solid"
    color : U.axisColor
    leftArrowhead: true
    rightArrowhead: true
    arrowheadSize : const.arrowheadSize * 2.
}
```

Refer to [here](https://github.com/penrose/penrose/wiki/Shape-library#line) for more information on the line shape. 

```typescript
U.text = Text {
    string : U.label
    center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
    color : U.axisColor
}
```
Lastly we just have some styling on the label. That's it! :cartwheeling: 

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
Now, things are going to be a bit tricky, but rest assured, we will get through it together! 

There are two tasks we need to do in our style program on top of the starter code:
* Task 1: Draw vectors in a vector space
* Task 2: Draw vectors that are the sum of two existing vectors 


### Task 1: Vectors In Vector Space
Every vector exists in a vector space, and we draw them at the origin of the specific vector space it belongs to. In the given `.dsl` file, you will find a defined predicate called `In` that takes in a vector and a vector space. The way we work with `In` is very similar to the `isSubset` predicate we have worked with in tutorial 2. 

We start with writing the selector, selecting vectors that are in a vector space.
```typescript
forall Vector u; VectorSpace U
where In(u,U) {
  /* draw a vector in vector space */
}
```
Next, vectors are commonly visually represented by single-head arrows, where the dull end is anchored at the origin, and the arrow head points at the vector position in space. Therefore we will draw an arrow on the screen. 

```typescript
u.shape = Arrow {
  start: U.origin
  end : (?, ?)
  thickness : 3.0
  color : const.lightBlue
  arrowheadSize : const.arrowheadSize
}
```
Note that the field name `shape` can be replaced by anything you want, we just need to assign the penrose shape object to some field (remember in tutorial 1 we used `.icon`). Here we are simply defining some properties of the `Arrow` shape object. One thing that might be confusing is the `(?, ?)` vector for `end`. It simply means that it is undetermined at the moment, and Penrose will decide for us. It is the equivalent of not defining it and Penrose will figure it out for us, but we are putting it here to explicitly show you how arbitray vectors will get arbitrary values. 

The value of the vector we draw in the diagram may be shifted by the vector space origin, therefore we also need to store the actual vector value in another field for future computations. 
```typescript
u.vector = u.shape.end - u.shape.start
```

Lastly, we need a field to write the variable name of our vector in the diagram.
```typescript
u.text = Text {
  string : u.label
  color : u.shape.color
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
🔥 Yes! You made it! We are halfway there. Now you should see something similar to the following diagram. 
# INSERT DIAGRAM
[Complete code for drawing vector in vector space](https://github.com/penrose/penrose/blob/docs-edit/tutorial/complete-code/tutorial-p3/vectorAddition.sty#L71)

Now you are ready to tackle the most challenging and rewarding part of this tutorial. 🎯

### Task 2: Vector As Sum of Two Existing Vectors
Again, we start with writing a selector. Here we have a bit more selection to do, since we have 3 vectors and 1 vector space involved. Furthermore, we want make sure that both `u,v,w` are indeed in the same vector space. Therefore, our selector will be the following,
```
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U)
```
Now we will move onto actually visualizing the vector addition. 
![no dash addition](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/without_dash.png) 

When we manually add 2 vectors, we add their x values and y values to get the new vector's x, y values. The new vector already has some x, y values chosen by Penrose for optimization, therefore we will need to override the existing values using the `override` keyword. (Every vector in a vector space is anchored at the origin, therefore we are only changing the end point of any vector. )

The x and y values of any vector in our current environment can be accessed using the array indexing syntax of `[0]` for `x` and `[1]` for `y`. 

Putting our thoughts together, we need to change the end values of `u`, which consists of `end[0]`(x) and `end[1]`(y). The value for our new x is the sum of `v.shape.end[0] + w.shape.end[0]` and furthermore, we need to subtract `U.origin[0]` from the sum since we are anchored at the origin. The logic for the y value is symmetric. 

Now putting everything together, we have the following code added to our `.sty` file. 
```typescript
/* new lines in .sty file */
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U) {
  override u.shape.end[0] = v.shape.end[0] + w.shape.end[0] - U.origin[0]
  override u.shape.end[1] = v.shape.end[1] + w.shape.end[1] - U.origin[1]
}
```
## Exercises

## Take-aways


