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

## :page_facing_up: Style

## Exercises

## Take-aways


