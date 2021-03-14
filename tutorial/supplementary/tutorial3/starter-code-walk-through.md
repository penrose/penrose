# Starter Code Walk Through
This is an supplementary document to tutorial 3, which teaches functions in Penrose by using function in penrose. This document goes through the given starter code for the tutorial in detail. 

## :runner: Starter Code Diagram
![starter code image](https://github.com/penrose/penrose/blob/docs-edit/assets/tutorial/part3/vectorspace_wg.png)
Download the [starter code here](https://github.com/penrose/penrose/tree/docs-edit/tutorial/code/tutorial3/starter-code).


## :runner: Domain
```typescript
type VectorSpace
type Vector
predicate In: Vector * VectorSpace V
```
Here we have 2 types, 1 predicate. Together we will define a new function. Furthermore, we have implemented drawing a vector space for you and we will together draw a vector in a vector space and vector addition. 

## :runner: Substance
```typescript
VectorSpace U
Vector v 
Vector w
In(v, U)
In(w, U)
```
The substance file contains the lines for putting two vectors in the same vector space `U`. Later we will compose a new vector as the sum of the two existing vectors. 

## :runner: Style 

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
