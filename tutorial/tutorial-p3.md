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
# INSERT PHOTO
Here we have 2 vectors in a vector space labeled U. 

## :page_facing_up: Domain
Now we will write our first function in Penrose together. In Penrose, functions are declarative, just like everything else. Functions allow us to compose already defined elements into new elements. They only have inputs and outputs. We will add a new line to our `.dsl` file that defines `addV`, which takes in 2 vectors and outputs a single vector. 
```typescript
/* new line in .dsl file */
function addV: Vector * Vector -> Vector
```
We use the keyword `function` and `->` to denote the output type.

## :page_facing_up: Substance
There are no imperative functions in Penrose, only functional functions. When we have `y=f(x)`, it's read like `y` is defined as `f(x)`. 

```typescript
/* new line in .sub file ******/
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
Now we will move onto actually visualizing the vector addition. Let's come back and look at our goal diagram. 
# INSERT GOAL DIAGRAM WITH NOTATIONS

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


