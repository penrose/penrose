# Part 3: Functions 
So far, we've directly declared substances in our diagram, which can then have some relationship with other substances in the diagram by the use of predicates. Now we will introduce functions in Penrose, which allow us to compose atomic substances in the diagram and define new substances based on existing ones. 

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
![Tutorial 3 Goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/goal_withlabel.png)
We will be defining a function `addV` that takes in 2 two dimensional vectors `v`, `w` and outputs a new vector `u`. In addition, we will be drawing two dashed lines that's commonly present in the parallelogram method for vector addition. 

## Starter Code
This example is a bit more involved than the previous tutorials, therefore we will be providing you some starter code to get things running. 
Download the [starter code here](https://github.com/penrose/penrose/tree/docs-edit/tutorial/starter-code/tutorial-p3), and you should see this in your browser. 
![starter code image](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/starter_code.png)
You should compile the starter code right now, and you should have something similar to this. Here we have 2 vectors `u`, `w` in a vector space labeled `U`. 

## :page_facing_up: Domain
Now we will write our first function in Penrose together. In Penrose, functions are declarative, just like everything else. Functions allow us to compose already defined elements into new elements. They only have inputs and outputs. We will add a new line to our `.dsl` file that defines `addV`, which takes in 2 vectors and outputs a single vector. 
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
Now we will move onto actually visualizing the vector addition. There are two steps we need to do to achieve our goal diagram. 
![Tutorial 3 Goal](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/goal_withlabel.png)
* __Step 1:__
  * Draw the new vector `u` that is the sum of the two vectors `v, w`. 
  * When we manually add 2 vectors, we add their x values and y values to get the new vector's x,y values. The new vector already has some x, y values chosen by Penrose for optimization, therefore we will need to override the existing values using the `override` keyword. (Every vector in a vector space is anchored at the origin, therefore we are only changing the end point of any vector. )
  * The x and y values of any vector in our current environment can be accessed using the array indexing syntax of `[0]` for `x` and `[1]` for `y`. 
  * Putting our thoughts together, we need to change the end values of `u`, which consists of `end[0]`(x) and `end[1]`(y). The value for our new x is the sum of `v.shape.end[0] + w.shape.end[0]` and furthermore, we need to subtract `U.origin[0]` from the sum since we are anchored at the origin. The logic for the y value is symmetric. 
  * New lines in `.sty` inside the selector block: 
  ```typescript
  override u.shape.end[0] = v.shape.end[0] + w.shape.end[0] - U.origin[0]
  override u.shape.end[1] = v.shape.end[1] + w.shape.end[1] - U.origin[1]
  ```
* __Step 2:__
  * Draw the two dashed lines that connects the tips of the three vectors. 


# INSERT GOAL DIAGRAM WITH NOTATIONS

![no dash addition](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/without_dash.png)

Add this to both `dashed_w` and `dashed_v` styles, or feel free to choose your own stylings. 
```typescript
thickness : const.arrowThickness2
style : "dashed"
arrowheadSize : const.arrowheadSize
```

Now putting everything together, we have the following code added to our `.sty` file. 
```typescript
/* new lines in .sty file */
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U) {
  override u.shape.end[0] = v.shape.end[0] + w.shape.end[0] - U.origin[0]
  override u.shape.end[1] = v.shape.end[1] + w.shape.end[1] - U.origin[1]

  u.dashed_v = Arrow {
    start: (w.shape.end[0], w.shape.end[1])
    end: (u.shape.end[0], u.shape.end[1])
    thickness : const.arrowThickness2
    style : "dashed"
    arrowheadSize : const.arrowheadSize
  }

  u.dashed_w = Arrow {
    start: (v.shape.end[0], v.shape.end[1])
    end: (u.shape.end[0], u.shape.end[1])
    thickness : const.arrowThickness2
    style : "dashed"
    arrowheadSize : const.arrowheadSize
  }

  u.sw_layering = u.dashed_w below u.shape
  u.sv_layering = u.dashed_v below u.shape
}
```
## Exercises

## Take-aways


