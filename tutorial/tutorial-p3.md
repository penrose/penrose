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

## Starter Code
This example is a bit more involved than the previous tutorials, therefore we will be providing you some starter code to get things running. 
Download the [starter code here](https://github.com/penrose/penrose/tree/docs-edit/tutorial/starter-code/tutorial-p3), and you should see this in your browser. 

## Goal
![Tutorial 3 Goal]()
We will be defining a function `addV` that takes in 2 two dimensional vectors `u`, `v` and outputs a new vector `w` in the normal vector addition way, i.e. `u =<a, b>, v = <c, d>, w = u + v = <a + c, b + d>`. 

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

```typescript
/* new lines in .sty file */
forall Vector u; Vector v; Vector w; VectorSpace U
where u := addV(v,w); In(u, U); In(v, U); In(w, U) {
  override u.shape.end[0] = v.shape.end[0] + w.shape.end[0] - U.origin[0]
  override u.shape.end[1] = v.shape.end[1] + w.shape.end[1] - U.origin[1]
  --override u.shape.color = blendColor(v.shape.color, w.shape.color

  u.slider_v = Arrow {
    start: (w.shape.end[0], w.shape.end[1])
    end: (u.shape.end[0], u.shape.end[1])
    thickness : const.arrowThickness2
    style : "dashed"
    arrowheadSize : const.arrowheadSize
  }

  u.slider_w = Arrow {
    start: (v.shape.end[0], v.shape.end[1])
    end: (u.shape.end[0], u.shape.end[1])
    thickness : const.arrowThickness2
    style : "dashed"
    arrowheadSize : const.arrowheadSize
  }

  u.sw_layering = u.slider_w below u.shape
  u.sv_layering = u.slider_v below u.shape
}
```
## Exercises

## Take-aways


