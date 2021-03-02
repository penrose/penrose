# Part 3: Functions 
*Insert intro blurb*

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

## :page_facing_up: Domain
### Starter Code
`.dsl`
```typescript
type VectorSpace
type LinearMap
predicate In: Vector * VectorSpace V
predicate From: LinearMap V * VectorSpace domain * VectorSpace codomain
```

## :page_facing_up: Substance
### Starter Code
`.sub`
```typescript
VectorSpace U, V
LinearMap f
From(f, U, V)
```

## :page_facing_up: Style

### Starter Code
`.sty`
```typescript

```

## Exercises

## Take-aways

```
  
const { -- 0
  scalar perpLen = 20.0
  -- For unit mark
  scalar markerPadding = 15.0
  scalar barSize = 5.0
  scalar vectorSpaceSize = 350.0
  scalar repelWeight = 0.7
  scalar arrowheadSize = 0.7
  scalar lineThickness = 1.
  int intForTesting = 1
  bool boolForTesting = true
}

C { -- 1
    -- black = #000000
    color black = rgba(0.,0.,0.,1.)
    white = rgba(1., 1., 1., 1.)
    lightBlue = rgba(1e-1, 0.1, 0.9, 1.0)
    -- Note: we don't currently support color accessors r,g,b
    -- darkBlue = rgba(lightBlue.r / 2., lightBlue.g / 2., lightBlue.b / 2., 0.5)
    darkGray = rgba(0.4, 0.4, 0.4, 1.)
    gray = rgba(0.6, 0.6, 0.6, 1.)
    green = rgba(0., 0.8, 0., 1.)
    -- blue = #0000ff
    none = rgba(0., 0., 0., 0.)
}


forall VectorSpace U { -- 3
    scalar axisSize = const.vectorSpaceSize / 2.0 -- This should get promoted to float
    vec2 U.origin = (0., 0.)
    vec2 o = U.origin
    U.axisColor = C.gray

    shape U.background = Square {
        center : U.origin
        side : const.vectorSpaceSize
        color : C.none
        strokeColor : C.none
        -- strokeWidth : 2.0
    }

    shape U.xAxis = Line {
        start : (o[0] - axisSize, o[1]) -- TODO This gets mis-parsed as a matrix access
        end : (o[0] + axisSize, o[1])
        thickness : const.lineThickness
        style : "solid"
        color : U.axisColor
        leftArrowhead: true
        rightArrowhead: true
        arrowheadSize : const.arrowheadSize * 2.
    }

    U.yAxis = Line {
           start : (o[0], o[1] - axisSize)
             end : (o[0], o[1] + axisSize)
       thickness : const.lineThickness
           style : "solid"
           color : U.axisColor
           leftArrowhead: true
           rightArrowhead: true
        arrowheadSize : const.arrowheadSize * 2.
    }

    U.text = Text {
        string : U.label
        center : (U.origin[0] - axisSize, U.origin[1] + axisSize)
        color : U.axisColor
    }
}

forall Vector u; VectorSpace U -- 4
where In(u,U) {
  u.text = Text {
    -- center : (?, ?) -- This should be done automatically
    string : u.label
    color : u.arrow.color
  }

  u.arrow = Arrow {
    start : U.origin
    end : (?, ?)
    thickness : 3.0
    color : C.lightBlue
    arrowheadSize : const.arrowheadSize
  }

   u.vector = u.arrow.end - u.arrow.start -- Vector sugar for subtraction

   ensure contains(U.background, u.arrow)
   ensure contains(U.background, u.text)
   ensure atDist(u.arrow, u.text, 15.0)
   ensure minSize(u.arrow)

  layer u.text above U.xAxis
  layer u.text above U.yAxis
}
```

