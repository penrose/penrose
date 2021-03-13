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

Now we are going to spend some time going over the starter code. There are comments in the starter code that documents each block briefly. 

#### Read along for more in-depth explanation on the starter code, or [skip to the example](https://github.com/penrose/penrose/blob/docs-edit/tutorial/tutorial-p3.md#page_facing_up-domain).

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
This is helpful way and a good practice to organize your constants in an object. Here we have some constants that helps with drawing out the vector space. Feel free to add more useful constants in here. 

To draw a vector space, we have an origin, x-axis and y-aixs. The x-axis and y-axis have each of their center on the origin of the vector space, and extends out to opposing directions. It follows that their length is half of the length of vector space (which is a square). 

```typescript
scalar axisSize = const.vectorSpaceSize / 2.0
vec2 U.origin = (0., 0.)
vec2 o = U.origin /* just so we don't need to type U.origin everytime */
U.axisColor = const.gray
```

Next, we go on to drawing the axis. The code for the `x` and `y` axis are symmetric. 

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

:point_right: Refer to [here](https://github.com/penrose/penrose/wiki/Shape-library#line) for more information on the line shape. 

```typescript
U.text = Text {
    string : U.label
    center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
    color : U.axisColor
}
```
Lastly we just have some styling on the label. That's it! :cartwheeling: 

## :page_facing_up: Domain
We will write our first function in Penrose together. In Penrose, functions are declarative, just like everything else. Functions allow us to compose already defined elements into new elements. They only have inputs and outputs. We will add a new line to our `.dsl` file that defines `addV`, which takes in 2 vectors and outputs a single vector. 
```typescript
/* new line in .dsl file */
function addV: Vector * Vector -> Vector
```
We use the keyword `function` and `->` to denote the output type.

## :page_facing_up: Substance
There are no imperative functions in Penrose, only functional functions. When we have `y = f(x)`, it's read like `y` is defined as `f(x)`. 

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
First we write our selector in similar fashion we've done in the earlier examples. Here we have a bit more selection to do, since we have 3 vectors and 1 vector space involved. Furthermore, we want make sure that both `u,v,w` are indeed in the same vector space. Therefore, our selector will be the following,
```
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U)
```
Now we will move onto actually visualizing the vector addition. 
![no dash addition](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/without_dash.png) 

When we manually add 2 vectors, we add their x values and y values to get the new vector's x,y values. The new vector already has some x, y values chosen by Penrose for optimization, therefore we will need to override the existing values using the `override` keyword. (Every vector in a vector space is anchored at the origin, therefore we are only changing the end point of any vector. )

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


